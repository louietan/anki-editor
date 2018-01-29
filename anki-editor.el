;;; anki-editor.el --- Make Anki Cards in Org-mode
;;
;; Copyright (C) 2018 Louie Tan <louietanlei@gmail.com>
;;
;; Filename: anki-editor.el
;; Description: Make Anki Cards in Org-mode
;; Author: Louie Tan
;; Version: 0.1.2
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/louietan/anki-editor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This package is for people who use Anki as SRS but would like to
;;  make cards in Org-mode.
;;
;;  With this package, you can make cards from something like:
;;  (which is inspired by `org-dirll')
;;
;;  * Computing                    :deck:
;;  ** Item                        :note:
;;     :PROPERTIES:
;;     :ANKI_NOTE_TYPE: Basic
;;     :END:
;;  *** Front
;;      How to hello world in elisp ?
;;  *** Back
;;      #+BEGIN_SRC emacs-lisp
;;      (message "Hello, world!")
;;      #+END_SRC
;;
;;  This package leverages Org-mode's built-in HTML backend to
;;  generate HTML for contents of note fields with specific syntax
;;  (e.g. latex) translated to Anki style, then save the note to Anki.
;;
;;  For this package to work, you have to setup these external dependencies:
;;  - curl
;;  - AnkiConnect, an Anki addon that runs an HTTP server to expose
;;                 Anki functions as RESTful APIs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'json)
(require 'org-element)


(defconst anki-editor-prop-note-type :ANKI_NOTE_TYPE)
(defconst anki-editor-prop-note-tags :ANKI_TAGS)
(defconst anki-editor-prop-note-id :ANKI_NOTE_ID)
(defconst anki-editor-prop-failure-reason :ANKI_FAILURE_REASON)
(defconst anki-editor-buffer-html-output "*anki-editor HTML Output*")

(defgroup anki-editor nil
  "Customizations for anki-editor."
  :group 'org)

(defcustom anki-editor-note-tag
  "note"
  "Headings with this tag will be considered as notes.")

(defcustom anki-editor-deck-tag
  "deck"
  "Headings with this tag will be considered as decks.")

(defcustom anki-editor-anki-connect-listening-address
  "127.0.0.1"
  "The network address anki-connect is listening.")

(defcustom anki-editor-anki-connect-listening-port
  "8765"
  "The port number anki-connect is listening.")

(defcustom anki-editor-latex-clean-for-cloze
  nil
  "If non-nil, successive `}' will be automatically separated by spaces to prevent early-closing of cloze.")


;;; anki-connect

(defun anki-editor--anki-connect-invoke (action version &optional params)
  "Invoke anki-connect with ACTION, VERSION and PARAMS."
  (let* ((data `(("action" . ,action)
                 ("version" . ,version)))
         (request-body (json-encode
                        (if params
                            (push `("params" . ,params) data)
                          data)))
         (request-tempfile (make-temp-file "emacs-anki-editor")))

    (with-temp-file request-tempfile
      (setq buffer-file-coding-system 'utf-8)
      (set-buffer-multibyte t)
      (insert request-body))

    (let* ((raw-resp (shell-command-to-string
                      (format "curl %s:%s --silent -X POST --data-binary @%s"
                              (shell-quote-argument anki-editor-anki-connect-listening-address)
                              (shell-quote-argument anki-editor-anki-connect-listening-port)
                              (shell-quote-argument request-tempfile))))
           resp error)

      (when (file-exists-p request-tempfile) (delete-file request-tempfile))
      (condition-case err
          (let ((json-array-type 'list))
            (setq resp (json-read-from-string raw-resp)
                  error (alist-get 'error resp)))
        (error (setq error
                     (format "Unexpected error communicating with anki-connect: %s, the response was %s"
                             (error-message-string err)
                             (prin1-to-string raw-resp)))))
      `((result . ,(alist-get 'result resp))
        (error . ,error)))))

(defmacro anki-editor--anki-connect-invoke-result (&rest args)
  "Invoke anki-connect with ARGS, return the result from response or raise an error."
  `(let* ((resp (anki-editor--anki-connect-invoke ,@args))
          (rslt (alist-get 'result resp))
          (err (alist-get 'error resp)))
     (when err (error err))
     rslt))

(defun anki-editor--anki-connect-map-note (note)
  "Convert NOTE to the form that anki-connect accepts."
  (let-alist note
    (list (cons "id" .note-id)
          (cons "deckName" .deck)
          (cons "modelName" .note-type)
          (cons "fields" .fields)
          ;; Convert tags to a vector since empty list is identical to nil
          ;; which will become None in Python, but anki-connect requires it
          ;; to be type of list.
          (cons "tags" (vconcat .tags)))))

(defun anki-editor--anki-connect-heading-to-note (heading)
  "Convert HEADING to a note in the form that anki-connect accepts."
  (anki-editor--anki-connect-map-note
   (anki-editor--heading-to-note heading)))

;;; Commands

;;;###autoload
(defun anki-editor-submit ()
  "Send notes in current buffer to Anki.

For each note heading, if there's no note id in property drawer,
create a note, otherwise, update fields and tags of the existing
note.

If one fails, the failure reason will be set in property drawer
of that heading."
  (interactive)
  (let ((total 0)
        (failed 0)
        current-deck)
    (org-map-entries
     (lambda ()
       (let ((current-tags (org-get-tags)))
         (cond
          ((member anki-editor-deck-tag current-tags)
           (setq current-deck (nth 4 (org-heading-components))))
          ((member anki-editor-note-tag current-tags)
           (progn
             (setq total (1+ total))
             (anki-editor--clear-failure-reason)
             (condition-case err
                 (anki-editor--process-note-heading current-deck)
               (error (progn
                        (setq failed (1+ failed))
                        (anki-editor--set-failure-reason (error-message-string err))))))))))
     (mapconcat 'identity `(,anki-editor-deck-tag ,anki-editor-note-tag) "|"))
    (message (with-output-to-string
               (princ (format "Submitted %d notes, with %d failed." total failed))
               (when (> failed 0)
                 (princ " Check property drawers for failure reasons."))))))

;;;###autoload
(defun anki-editor-insert-deck (&optional prefix)
  "Insert a deck heading.
With PREFIX, only insert the deck name at point."
  (interactive "P")
  (message "Fetching decks...")
  (let ((decks (sort (anki-editor--anki-connect-invoke-result "deckNames" 5) #'string-lessp))
        deckname)
    (setq deckname (completing-read "Choose a deck: " decks))
    (if prefix
        (insert deckname)
      (let (inserted)
        (anki-editor--visit-superior-headings
         (lambda ()
           (when (member anki-editor-deck-tag (org-get-tags))
             (anki-editor--insert-deck-heading deckname)
             (setq inserted t))))

        (unless inserted
          (anki-editor--insert-deck-heading deckname))))))

;;;###autoload
(defun anki-editor-insert-note ()
  "Insert the skeleton of a note.

The contents to be insrted are structured with a note heading
along with subheadings that correspond to fields.

Where the note is inserted depends on where the point is.

When the point is somewhere inside a note heading, the new note
is inserted below this note with same heading level.

Or when the point is outside any note heading but inside a
heading that isn't tagged with 'deck' but under a deck heading,
the new note is one level lower to and is inserted at the bottom
of this heading.

Or when the point is inside a deck heading, the behavior is the
same as above.

Otherwise, it's inserted at point."
  (interactive)
  (message "Fetching note types...")
  (let ((note-types (sort (anki-editor--anki-connect-invoke-result "modelNames" 5) #'string-lessp))
        note-type note-heading fields)
    (setq note-type (completing-read "Choose a note type: " note-types))
    (message "Fetching note fields...")
    (setq fields (anki-editor--anki-connect-invoke-result "modelFieldNames" 5 `((modelName . ,note-type)))
          note-heading (read-from-minibuffer "Enter the heading: " "Item"))

    ;; find and go to the best position, then insert the note
    (let ((cur-point (point))
          pt-of-grp
          inserted)
      (anki-editor--visit-superior-headings
       (lambda ()
         (let ((tags (org-get-tags)))
           (cond
            ;; if runs into a note heading, inserts the note heading with
            ;; the same level
            ((member anki-editor-note-tag tags)
             (progn
               (anki-editor--insert-note-skeleton note-heading note-type fields)
               (setq inserted t)
               t))
            ;; if runs into a deck heading, inserts the note heading one
            ;; level lower to current deck heading or to the group
            ;; heading that was visited before
            ((member anki-editor-deck-tag tags)
             (progn
               (when pt-of-grp (goto-char pt-of-grp))
               (anki-editor--insert-note-skeleton note-heading note-type fields t)
               (setq inserted t)
               t))
            ;; otherwise, consider it as a group heading and save its
            ;; point for further consideration, then continue
            (t (progn
                 (unless pt-of-grp (setq pt-of-grp (point)))
                 nil))))))
      (unless inserted
        (goto-char cur-point)
        (anki-editor--insert-note-skeleton note-heading note-type fields)))))

;;;###autoload
(defun anki-editor-insert-tags ()
  "Insert a tag at point with autocompletion."
  (interactive)
  (let ((tags (sort (anki-editor--anki-connect-invoke-result "getTags" 5) #'string-lessp)))
    (while t (insert (format " %s" (completing-read "Choose a tag: " tags))))))

;;;###autoload
(defun anki-editor-export-heading-contents-to-html ()
  "Export the contents of the heading at point to HTML."
  (interactive)
  (let ((tree (org-element-at-point))
        contents)
    (if (or (null tree)
            (not (eq (org-element-type tree) 'headline)))
        (error "No element at point or it's not a heading")

      (setq contents (buffer-substring-no-properties (org-element-property :contents-begin tree)
                                                     (org-element-property :contents-end tree)))
      (when (buffer-live-p (get-buffer anki-editor-buffer-html-output))
        (kill-buffer anki-editor-buffer-html-output))
      (switch-to-buffer-other-window (get-buffer-create anki-editor-buffer-html-output))
      (insert (anki-editor--generate-html contents)))))

;;;###autoload
(defun anki-editor-convert-region-to-html ()
  "Convert and replace region to HTML."
  (interactive)
  (unless (region-active-p) (error "No active region"))
  (insert (anki-editor--generate-html
           (delete-and-extract-region (region-beginning) (region-end)))))

;;;###autoload
(defun anki-editor-anki-connect-upgrade ()
  "Upgrade anki-connect to the latest version.

This will display a confirmation dialog box in Anki asking if you
want to continue.  The upgrading is done by downloading the latest
code in the master branch of its Github repo.

This is useful when new version of this package depends on the
bugfixes or new features of anki-connect."
  (interactive)
  (when (yes-or-no-p "NOTE: This will download the latest codebase of anki-connect to your system, which is not guaranteed to be safe or stable. Generally, you don't need this command, this is useful only when new version of this package requires the updates of anki-connect that are not released yet. Do you still want to continue?")
    (let ((result (anki-editor--anki-connect-invoke-result "upgrade" 5)))
      (when (and (booleanp result) result)
        (message "anki-connect has been upgraded, you might have to restart Anki to make it in effect.")))))

;;; Core Functions

(defun anki-editor--insert-deck-heading (deckname)
  "Insert a deck heading with DECKNAME."
  (org-insert-heading-respect-content)
  (insert deckname)
  (anki-editor--set-tags-fix anki-editor-deck-tag))

(defun anki-editor--process-note-heading (deck)
  "Process note heading at point.
DECK is used when the action is note creation."
  (unless deck (error "No deck specified"))

  (let (note-elem note)
    (setq note-elem (org-element-at-point)
          note-elem (let ((content (buffer-substring
                                    (org-element-property :begin note-elem)
                                    (org-element-property :end note-elem))))
                      (with-temp-buffer
                        (insert content)
                        (car (org-element-contents (org-element-parse-buffer)))))
          note (anki-editor--heading-to-note note-elem))
    (push `(deck . ,deck) note)
    (anki-editor--save-note note)))

(defun anki-editor--insert-note-skeleton (heading note-type fields &optional demote)
  "Insert a note skeleton with HEADING, NOTE-TYPE and FIELDS.
If DEMOTE is t, demote the inserted note heading."
  (org-insert-heading-respect-content)
  (when demote (org-do-demote))
  (insert heading)
  (anki-editor--set-tags-fix anki-editor-note-tag)
  (org-set-property (substring (symbol-name anki-editor-prop-note-type) 1) note-type)
  (dolist (field fields)
    (save-excursion
      (org-insert-heading-respect-content)
      (org-do-demote)
      (insert field)))

  ;; TODO: Is it a good idea to automatically move to the first field
  ;; heading and open a new line ?

  ;; (org-next-visible-heading 1)
  ;; (end-of-line)
  ;; (newline-and-indent)
  )

(defun anki-editor--save-note (note)
  "Request anki-connect for updating or creating NOTE."
  (if (= (alist-get 'note-id note) -1)
      (anki-editor--create-note note)
    (anki-editor--update-note note)))

(defun anki-editor--create-note (note)
  "Request anki-connect for creating NOTE."
  (let* ((response (anki-editor--anki-connect-invoke
                    "addNote" 5 `((note . ,(anki-editor--anki-connect-map-note note)))))
         (result (alist-get 'result response))
         (err (alist-get 'error response)))
    (if result
        (org-set-property (substring (symbol-name anki-editor-prop-note-id) 1)
                          (format "%d" (alist-get 'result response)))
      (error (or err "Sorry, the operation was unsuccessful and detailed information is unavailable.")))))

(defun anki-editor--update-note (note)
  "Request anki-connect for updating fields and tags of NOTE."
  (anki-editor--anki-connect-invoke-result
   "updateNoteFields" 5 `((note . ,(anki-editor--anki-connect-map-note note))))

  ;; update tags
  (let (existing-note added-tags removed-tags)
    (setq existing-note (car (anki-editor--anki-connect-invoke-result
                              "notesInfo" 5 `(("notes" . (,(alist-get 'note-id note))))))
          added-tags (cl-set-difference (alist-get 'tags note) (alist-get 'tags existing-note) :test #'string-equal)
          removed-tags (cl-set-difference (alist-get 'tags existing-note) (alist-get 'tags note) :test #'string-equal))

    (when added-tags
      (anki-editor--anki-connect-invoke-result
       "addTags" 5 `(("notes" . (,(alist-get 'note-id note)))
                     ("tags" . ,(mapconcat #'identity added-tags " ")))))
    (when removed-tags
      (anki-editor--anki-connect-invoke-result
       "removeTags" 5 `(("notes" . (,(alist-get 'note-id note)))
                        ("tags" . ,(mapconcat #'identity removed-tags " ")))))))

(defun anki-editor--set-failure-reason (reason)
  "Set failure reason to REASON in property drawer at point."
  (org-set-property (substring (symbol-name anki-editor-prop-failure-reason) 1) reason))

(defun anki-editor--clear-failure-reason ()
  "Clear failure reason in property drawer at point."
  (org-delete-property (substring (symbol-name anki-editor-prop-failure-reason) 1)))

(defun anki-editor--heading-to-note (heading)
  "Construct an alist representing a note for HEADING."
  (let (note-id note-type tags fields)
    (setq note-id (org-element-property anki-editor-prop-note-id heading)
          note-type (org-element-property anki-editor-prop-note-type heading)
          tags (org-element-property anki-editor-prop-note-tags heading)
          fields (mapcar #'anki-editor--heading-to-note-field (anki-editor--get-subheadings heading)))

    (unless note-type (error "Missing note type"))
    (unless fields (error "Missing fields"))

    `((note-id . ,(string-to-number (or note-id "-1")))
      (note-type . ,note-type)
      (tags . ,(and tags (split-string tags " ")))
      (fields . ,fields))))

(defun anki-editor--get-subheadings (heading)
  "Get all the subheadings of HEADING."
  (org-element-map (org-element-contents heading)
      'headline 'identity nil nil 'headline))

(defun anki-editor--heading-to-note-field (heading)
  "Convert HEADING to field data, a cons cell, the car of which is the field name, the cdr of which is contens represented in HTML."
  (let ((field-name (substring-no-properties
                     (org-element-property
                      :raw-value
                      heading)))
        (contents (org-element-contents heading)))
    `(,field-name . ,(anki-editor--generate-html
                      (org-element-interpret-data contents)))))

(defun anki-editor--generate-html (contents)
  "Convert CONTENTS to HTML."
  (with-temp-buffer
    (org-mode)
    (insert contents)
    (setq anki-editor--replacement-records nil)
    (anki-editor--replace-latex)
    (anki-editor--buffer-to-html)
    (anki-editor--translate-latex)
    (buffer-substring-no-properties (point-min) (point-max))))

;; Transformers

(defun anki-editor--buffer-to-html ()
  "Transform contents of buffer to HTML."
  (when (> (buffer-size) 0)
    (insert
     (org-export-string-as
      (delete-and-extract-region (point-min) (point-max)) 'html t))))

(defun anki-editor--replace-latex ()
  "Replace latex objects with the hash of it's content."
  (let (object type memo)
    (while (setq object (org-element-map
                            (org-element-parse-buffer)
                            '(latex-fragment latex-environment) 'identity nil t))

      (setq type (org-element-type object)
            memo (anki-editor--replace-node object
                                            (lambda (original)
                                              (anki-editor--hash type
                                                                 original))))
      (push `(,(cdr memo) . ((type . ,type)
                             (original . ,(car memo))))
            anki-editor--replacement-records))))

(defvar anki-editor--anki-latex-syntax-map
  `((,(format "^%s" (regexp-quote "$$")) . "[$$]")
    (,(format "%s$" (regexp-quote "$$")) . "[/$$]")
    (,(format "^%s" (regexp-quote "$")) . "[$]")
    (,(format "%s$" (regexp-quote "$")) . "[/$]")
    (,(format "^%s" (regexp-quote "\\(")) . "[$]")
    (,(format "%s$" (regexp-quote "\\)")) . "[/$]")
    (,(format "^%s" (regexp-quote "\\[")) . "[$$]")
    (,(format "%s$" (regexp-quote "\\]")) . "[/$$]")))

(defun anki-editor--clean-latex (content)
  "Add whitespace between curly braces in CONTENT for compatiblity with cloze regions."
  ;; `save-match-data' prevent issues for callers performing their own matching
  (save-match-data
    (let ((result content)
          (match (string-match "}}" content)))
      (while match
        (setq result (replace-match "} } " nil nil result))
        ;; step 1 back in case we have more than two }
        (setq match (string-match "}}" result (- match 1))))
      result)))

(defun anki-editor--wrap-latex (content)
  "Wrap CONTENT with Anki-style latex markers."
  (format "[latex]%s[/latex]" (if anki-editor-latex-clean-for-cloze
                                  (anki-editor--clean-latex content)
                                content)))

(defun anki-editor--convert-latex-fragment (frag)
  "Convert latex fragment FRAG to Anki-style."
  (let ((copy frag))
    (dolist (map anki-editor--anki-latex-syntax-map)
      (setq frag (replace-regexp-in-string (car map) (cdr map) frag t t)))
    (if (equal copy frag)
        (anki-editor--wrap-latex frag)
      frag)))

(defun anki-editor--translate-latex ()
  "Transform latex objects that were previously replaced with hashes to Anki-style."
  (let (ele-data translated)
    (dolist (record anki-editor--replacement-records)
      (setq ele-data (cdr record))
      (goto-char (point-min))
      (when (search-forward (car record) nil t)
        (pcase (alist-get 'type ele-data)
          ('latex-fragment (replace-match (anki-editor--convert-latex-fragment (alist-get 'original ele-data)) t t))
          ('latex-environment (replace-match (anki-editor--wrap-latex (alist-get 'original ele-data)) t t)))
        (push record translated)))
    (setq anki-editor--replacement-records (cl-set-difference anki-editor--replacement-records translated))))

;;; Utilities

(defun anki-editor--hash (type text)
  "Compute hash of object, whose type and contens is TYPE and TEXT respectively."
  (sha1 (format "%s %s" (symbol-name type) text)))

(defun anki-editor--set-tags-fix (tags)
  "Set tags to TAGS and fix tags on the fly."
  (org-set-tags-to tags)
  (org-fix-tags-on-the-fly))

(defun anki-editor--effective-end (node)
  "Get the effective end of NODE.

org-element considers whitespaces or newlines after an element or
object still belong to it, which is to say :end property of an
element matches :begin property of the following one at the same
level, if any.  This will make it unable to separate elements with
their following ones after replacing.  This function 'fixes' this
by resetting the end to the point after the last character that's
not blank.  I'm not sure if this works for all cases though :)"
  (let ((end (org-element-property :end node)))
    (while (and (>= end (point-min))
                ;; check if character before END is blank
                (string-match-p "[[:blank:]\r\n]" (buffer-substring (1- end) end)))
      (setq end (1- end)))
    end))

(defun anki-editor--replace-node (node replacer)
  "Replace contents of NODE with the result from applying REPLACER to the contents of NODE."
  (let* ((begin (org-element-property :begin node))
         (end (anki-editor--effective-end node))
         (original (delete-and-extract-region begin end))
         (replacement (funcall replacer original)))
    (goto-char begin)
    (insert replacement)
    (cons original replacement)))

(defun anki-editor--visit-superior-headings (visitor &optional level)
  "Move point to and call VISITOR at each superior heading from point.
Don't pass LEVEL, it's only used in recursion.
Stops when VISITOR returns t or point reaches the beginning of buffer."
  (let (stop)
    (when (org-at-heading-p)
      (let ((cur-level (car (org-heading-components))))
        (when (or (null level) (< cur-level level))
          (setq level cur-level
                stop (funcall visitor)))))
    (when (and (not stop) (/= (point) (point-min)))
      (org-previous-visible-heading 1)
      (anki-editor--visit-superior-headings visitor level))))


(provide 'anki-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor.el ends here
