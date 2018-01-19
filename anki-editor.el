;;; anki-editor.el --- Create Anki Cards in Org-mode
;;
;; Copyright (C) 2018 Louie Tan <louietanlei@gmail.com>
;;
;; Filename: anki-editor.el
;; Description: Create Anki Cards in Org-mode
;; Author: Louie Tan
;; Version: 0.1.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/louietan/anki-editor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This package is for people who use Anki as SRS but would like to
;;  create cards in Org-mode.  It does so by using Org-mode's built-in
;;  HTML backend to generate HTML with specific syntax (e.g. latex)
;;  translated to the Anki style, then sends requests to anki-connect
;;  (an Anki addon that runs an HTTP server to expose Anki functions
;;  as APIs).
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

(defcustom anki-editor-note-tag
  "note"
  "Headings with this tag will be considered as notes."
  :group 'anki-editor)
(defcustom anki-editor-deck-tag
  "deck"
  "Headings with this tag will be considered as decks."
  :group 'anki-editor)

(defcustom anki-editor-anki-connect-listening-address
  "127.0.0.1"
  "The network address anki-connect is listening."
  :group 'anki-editor)

(defcustom anki-editor-anki-connect-listening-port
  "8765"
  "The port number anki-connect is listening."
  :group 'anki-editor)


;;; anki-connect

(defun anki-editor--anki-connect-invoke (action version &optional params)
  "Invoke anki-connect with ACTION, VERSION and PARAMS."
  (let* ((data `(("action" . ,action)
                 ("version" . ,version)))
         (request-body (json-encode
                        (if params
                            (add-to-list 'data `("params" . ,params))
                          data)))
         (request-tempfile (make-temp-file "emacs-anki-editor")))

    (with-temp-file request-tempfile
      (setq buffer-file-coding-system 'utf-8)
      (set-buffer-multibyte t)
      (insert request-body))

    (let* ((raw-resp (shell-command-to-string
                      (format "curl %s:%s --silent -X POST --data-binary @%s"
                              anki-editor-anki-connect-listening-address
                              anki-editor-anki-connect-listening-port
                              request-tempfile)))
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
  `(("id" . ,(alist-get 'note-id note))
    ("deckName" . ,(alist-get 'deck note))
    ("modelName" . ,(alist-get 'note-type note))
    ("fields" . ,(alist-get 'fields note))
    ;; Convert tags to a vector since empty list is identical to nil
    ;; which will become None in Python, but anki-connect requires it
    ;; to be type of list.
    ("tags" . ,(vconcat (alist-get 'tags note)))))

(defun anki-editor--anki-connect-heading-to-note (heading)
  "Convert HEADING to a note in the form that anki-connect accepts."
  (anki-editor--anki-connect-map-note
   (anki-editor--heading-to-note heading)))


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
        (failed 0))
    (save-excursion
      (goto-char (point-min))
      (let (current-tags current-deck)
        (while (not (= (point) (point-max)))
          (when (org-at-heading-p)
            (setq current-tags (org-get-tags))
            (cond
             ((member anki-editor-deck-tag current-tags) (setq current-deck (nth 4 (org-heading-components))))
             ((member anki-editor-note-tag current-tags) (progn
                                                           (setq total (1+ total))
                                                           (anki-editor--clear-failure-reason)
                                                           (condition-case err
                                                               (anki-editor--process-note-heading current-deck)
                                                             (error (progn
                                                                      (setq failed (1+ failed))
                                                                      (anki-editor--set-failure-reason (error-message-string err)))))))))
          (org-next-visible-heading 1))))
    (message (with-output-to-string
               (princ (format "Submitted %d notes, with %d failed." total failed))
               (when (> failed 0)
                 (princ " Check property drawers for failure reasons."))))))

;;;###autoload
(defun anki-editor-insert-deck (&optional prefix)
  "Insert a deck heading with the same level as current heading.
With PREFIX, only insert the deck name."
  (interactive "P")
  (message "Fetching decks...")
  (let ((decks (sort (anki-editor--anki-connect-invoke-result "deckNames" 5) #'string-lessp))
        deckname)
    (setq deckname (completing-read "Choose a deck: " decks))
    (unless prefix (org-insert-heading-respect-content))
    (insert deckname)
    (unless prefix (anki-editor--set-tags-fix anki-editor-deck-tag))))

;;;###autoload
(defun anki-editor-insert-note ()
  "Insert the skeleton of a note.
The contents to be insrted are structured with a note heading
that's one level lower to the current one as well as subheadings
that correspond to fields."
  (interactive)
  (message "Fetching note types...")
  (let ((note-types (sort (anki-editor--anki-connect-invoke-result "modelNames" 5) #'string-lessp))
        note-type note-heading fields)
    (setq note-type (completing-read "Choose a note type: " note-types))
    (message "Fetching note fields...")
    (setq fields (anki-editor--anki-connect-invoke-result "modelFieldNames" 5 `((modelName . ,note-type)))
          note-heading (read-from-minibuffer "Enter the heading: " "Item"))
    (org-insert-heading-respect-content)
    (org-do-demote)
    (insert note-heading)
    (anki-editor--set-tags-fix anki-editor-note-tag)
    (org-set-property (substring (symbol-name anki-editor-prop-note-type) 1) note-type)
    (seq-each (lambda (field)
                (save-excursion
                  (org-insert-heading-respect-content)
                  (org-do-demote)
                  (insert field)))
              fields)
    (org-next-visible-heading 1)
    (end-of-line)
    (newline-and-indent)))

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

(setq anki-editor--key-map `((,(kbd "C-c a s") . ,#'anki-editor-submit)
                             (,(kbd "C-c a i d") . ,#'anki-editor-insert-deck)
                             (,(kbd "C-c a i n") . ,#'anki-editor-insert-note)
                             (,(kbd "C-c a i t") . ,#'anki-editor-insert-tags)
                             (,(kbd "C-c a e") . ,#'anki-editor-export-heading-contents-to-html)))

;;;###autoload
(defun anki-editor-setup-default-keybindings ()
  "Set up the default keybindings."
  (interactive)
  (dolist (map anki-editor--key-map)
    (local-set-key (car map) (cdr map)))
  (message "anki-editor default keybindings have been set"))

;;;###autoload
(defun anki-editor-anki-connect-upgrade ()
  "Upgrade anki-connect to the latest version.

This will display a confirmation dialog box in Anki asking if you
want to continue.  The upgrading is done by downloading the latest
code in the master branch of its Github repo.

This is useful when new version of this package depends on the
bugfixes or new features of anki-connect."
  (interactive)
  (let ((result (anki-editor--anki-connect-invoke-result "upgrade" 5)))
    (when (and (booleanp result) result)
      (message "anki-connect has upgraded, you may have to restart Anki to make it in effect."))))

;;; Core Functions

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
    (add-to-list 'note `(deck . ,deck))
    (anki-editor--save-note note)))

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
      (add-to-list 'anki-editor--replacement-records
                   `(,(cdr memo) . ((type . ,type)
                                    (original . ,(car memo))))))))

(setq anki-editor--anki-latex-syntax-map
      `((,(format "^%s" (regexp-quote "$$")) . "[$$]")
        (,(format "%s$" (regexp-quote "$$")) . "[/$$]")
        (,(format "^%s" (regexp-quote "$")) . "[$]")
        (,(format "%s$" (regexp-quote "$")) . "[/$]")
        (,(format "^%s" (regexp-quote "\\(")) . "[$]")
        (,(format "%s$" (regexp-quote "\\)")) . "[/$]")
        (,(format "^%s" (regexp-quote "\\[")) . "[$$]")
        (,(format "%s$" (regexp-quote "\\]")) . "[/$$]")))

(defun anki-editor--wrap-latex (content)
  "Wrap CONTENT with Anki-style latex markers."
  (format "[latex]%s[/latex]" content))

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
        (add-to-list 'translated record)))
    (setq anki-editor--replacement-records (cl-set-difference anki-editor--replacement-records translated))))

;;; Utilities

(defun anki-editor--hash (type text)
  "Compute hash of object, whose type and contens is TYPE and TEXT respectively."
  (sha1 (format "%s %s" (symbol-name type) text)))

(defun anki-editor--set-tags-fix (tags)
  "Set tags to TAGS and fix tags on the fly."
  (org-set-tags-to tags)
  (org-fix-tags-on-the-fly))

(defun anki-editor--replace-node (node replacer)
  "Replace contents of NODE with the result from applying REPLACER to the contents of NODE."
  (let* ((begin (org-element-property :begin node))
         (end (- (org-element-property :end node) (org-element-property :post-blank node)))
         (original (delete-and-extract-region begin end))
         (replacement (funcall replacer original)))
    (goto-char begin)
    (insert replacement)
    (cons original replacement)))

(provide 'anki-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor.el ends here
