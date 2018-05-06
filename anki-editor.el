;;; anki-editor.el --- Make Anki Cards in Org-mode
;;
;; Copyright (C) 2018 Louie Tan <louietanlei@gmail.com>
;;
;; Filename: anki-editor.el
;; Description: Make Anki Cards in Org-mode
;; Author: Louie Tan
;; Version: 0.2.1
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
(require 'ox)
(require 'seq)

(defconst anki-editor-prop-note-type :ANKI_NOTE_TYPE)
(defconst anki-editor-prop-note-id :ANKI_NOTE_ID)
(defconst anki-editor-prop-failure-reason :ANKI_FAILURE_REASON)
(defconst anki-editor-buffer-html-output "*AnkiEditor HTML Output*")
(defconst anki-editor-org-tag-regexp "^\\([[:alnum:]_@#%]+\\)+$")

(defgroup anki-editor nil
  "Customizations for anki-editor."
  :group 'org)

(defcustom anki-editor-deck-tag
  "deck"
  "Headings with this tag will be considered as decks.")

(defcustom anki-editor-break-consecutive-braces-in-latex
  nil
  "If non-nil, consecutive `}' will be automatically separated by spaces to prevent early-closing of cloze.
See https://apps.ankiweb.net/docs/manual.html#latex-conflicts.")

(defcustom anki-editor-create-decks
  nil
  "If non-nil, creates deck before creating a note.")

(defcustom anki-editor-anki-connect-listening-address
  "127.0.0.1"
  "The network address AnkiConnect is listening.")

(defcustom anki-editor-anki-connect-listening-port
  "8765"
  "The port number AnkiConnect is listening.")


;;; AnkiConnect

(defun anki-editor--anki-connect-invoke (action version &optional params)
  "Invoke AnkiConnect with ACTION, VERSION and PARAMS."
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
      (condition-case err
          (let ((json-array-type 'list))
            (setq resp (json-read-from-string raw-resp)
                  error (alist-get 'error resp)))
        (error (setq error
                     (format "Unexpected error communicating with AnkiConnect: %s, the response was %s"
                             (error-message-string err)
                             (prin1-to-string raw-resp)))))
      `((result . ,(alist-get 'result resp))
        (error . ,error)))))

(defmacro anki-editor--anki-connect-invoke-result (&rest args)
  "Invoke AnkiConnect with ARGS, return the result from response or raise an error."
  `(let* ((resp (anki-editor--anki-connect-invoke ,@args))
          (rslt (alist-get 'result resp))
          (err (alist-get 'error resp)))
     (when err (error err))
     rslt))

(defun anki-editor--anki-connect-map-note (note)
  "Convert NOTE to the form that AnkiConnect accepts."
  (let-alist note
    (list (cons "id" .note-id)
          (cons "deckName" .deck)
          (cons "modelName" .note-type)
          (cons "fields" .fields)
          ;; Convert tags to a vector since empty list is identical to nil
          ;; which will become None in Python, but AnkiConnect requires it
          ;; to be type of list.
          (cons "tags" (vconcat .tags)))))

(defun anki-editor--anki-connect-heading-to-note (heading)
  "Convert HEADING to a note in the form that AnkiConnect accepts."
  (anki-editor--anki-connect-map-note
   (anki-editor--heading-to-note heading)))

(defun anki-editor--anki-connect-store-media-file (path)
  "Store media file for PATH, which is an absolute file name.
The result is the path to the newly stored media file."
  (unless (and (executable-find "base64")
               (executable-find "sha1sum"))
    (error "Please make sure `base64' and `sha1sum' are available from your shell, which are required for storing media files"))

  (let* ((content (string-trim
                   (shell-command-to-string
                    (format "base64 --wrap=0 %s"
                            (shell-quote-argument path)))))
         (hash (string-trim
                (shell-command-to-string
                 (format "sha1sum %s | awk '{print $1}'"
                         (shell-quote-argument path)))))
         (media-file-name (format "%s-%s%s"
                                  (file-name-base path)
                                  hash
                                  (file-name-extension path t))))
    (anki-editor--anki-connect-invoke-result
     "storeMediaFile" 5
     `((filename . ,media-file-name)
       (data . ,content)))
    media-file-name))

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
       (cond
        ;; FIXME: decks won't get iterated any more, consider remove
        ;; the `deck' tags and use properties instead ?
        ((member anki-editor-deck-tag (org-get-tags))
         (setq current-deck (nth 4 (org-heading-components))))
        ((org-entry-get (point) (anki-editor--keyword-name anki-editor-prop-note-type))
         (progn
           (setq total (1+ total))
           (message "Processing note at %d..." (point))
           (anki-editor--clear-failure-reason)
           (condition-case err
               (anki-editor--process-note-heading current-deck)
             (error (progn
                      (setq failed (1+ failed))
                      (anki-editor--set-failure-reason (error-message-string err)))))))))
     (concat (anki-editor--keyword-name anki-editor-prop-note-type) "<>\"\""))

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
        (anki-editor--visit-ancestor-headings
         (lambda ()
           (when (member anki-editor-deck-tag (org-get-tags))
             (anki-editor--insert-deck-heading deckname)
             (setq inserted t))))

        (unless inserted
          (anki-editor--insert-deck-heading deckname))))))

;;;###autoload
(defun anki-editor-insert-note (&optional prefix)
  "Insert a note interactively.

Where the note subtree is placed depends on PREFIX, which is the
same as how it is used by `M-RET'(org-insert-heading)."
  (interactive "P")
  (message "Fetching note types...")
  (let ((note-types (sort (anki-editor--anki-connect-invoke-result "modelNames" 5) #'string-lessp))
        note-type note-heading fields)
    (setq note-type (completing-read "Choose a note type: " note-types))
    (message "Fetching note fields...")
    (setq fields (anki-editor--anki-connect-invoke-result "modelFieldNames" 5 `((modelName . ,note-type)))
          note-heading (read-from-minibuffer "Enter the heading: " "Item"))
    (anki-editor--insert-note-skeleton prefix note-heading note-type fields)))

;;;###autoload
(defun anki-editor-cloze-region (&optional arg)
  "Cloze region with number ARG."
  (interactive "p")
  (unless (region-active-p) (error "No active region"))
  (let ((region (buffer-substring (region-beginning) (region-end)))
        (hint (read-from-minibuffer "Hint (optional): ")))
    (save-excursion
      (delete-region (region-beginning) (region-end))
      (insert (with-output-to-string
                (princ (format "{{c%d::%s" (or arg 1) region))
                (unless (string-empty-p (string-trim hint)) (princ (format "::%s" hint)))
                (princ "}}"))))))

;;;###autoload
(defun anki-editor-export-subtree-to-html ()
  "Export subtree of the element at point to HTML."
  (interactive)
  (org-export-to-buffer
      anki-editor--ox-anki-html-backend
      anki-editor-buffer-html-output nil t nil t nil
      (lambda () (html-mode))))

;;;###autoload
(defun anki-editor-convert-region-to-html ()
  "Convert and replace region to HTML."
  (interactive)
  (org-export-replace-region-by anki-editor--ox-anki-html-backend))

;;;###autoload
(defun anki-editor-anki-connect-upgrade ()
  "Upgrade AnkiConnect to the latest version.

This will display a confirmation dialog box in Anki asking if you
want to continue.  The upgrading is done by downloading the latest
code in the master branch of its Github repo.

This is useful when new version of this package depends on the
bugfixes or new features of AnkiConnect."
  (interactive)
  (when (yes-or-no-p "NOTE: This will download the latest codebase of AnkiConnect to your system, which is not guaranteed to be safe or stable. Generally, you don't need this command, this is useful only when new version of this package requires the updates of AnkiConnect that are not released yet. Do you still want to continue?")
    (let ((result (anki-editor--anki-connect-invoke-result "upgrade" 5)))
      (when (and (booleanp result) result)
        (message "AnkiConnect has been upgraded, you might have to restart Anki to make it in effect.")))))

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
                        (org-mode)
                        (insert content)
                        (car (org-element-contents (org-element-parse-buffer)))))
          note (anki-editor--heading-to-note note-elem))
    (push `(deck . ,deck) note)
    (anki-editor--save-note note)))

(defun anki-editor--insert-note-skeleton (prefix heading note-type fields)
  "Insert a note subtree (skeleton) with HEADING, NOTE-TYPE and FIELDS.
Where the subtree is created depends on PREFIX."
  (org-insert-heading prefix)
  (insert heading)
  (org-set-property (anki-editor--keyword-name anki-editor-prop-note-type) note-type)
  (dolist (field fields)
    (save-excursion
      (org-insert-heading-respect-content)
      (org-do-demote)
      (insert field))))

(defun anki-editor--save-note (note)
  "Request AnkiConnect for updating or creating NOTE."
  (if (= (alist-get 'note-id note) -1)
      (anki-editor--create-note note)
    (anki-editor--update-note note)))

(defun anki-editor--create-note (note)
  "Request AnkiConnect for creating NOTE."
  (when anki-editor-create-decks
    (anki-editor--create-deck (alist-get 'deck note)))

  (let* ((response (anki-editor--anki-connect-invoke
                    "addNote" 5 `((note . ,(anki-editor--anki-connect-map-note note)))))
         (result (alist-get 'result response))
         (err (alist-get 'error response)))
    (if result
        ;; put ID of newly created note in property drawer
        (org-set-property (anki-editor--keyword-name anki-editor-prop-note-id)
                          (format "%d" (alist-get 'result response)))
      (error (or err "Sorry, the operation was unsuccessful and detailed information is unavailable.")))))

(defun anki-editor--update-note (note)
  "Request AnkiConnect for updating fields and tags of NOTE."
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

(defun anki-editor--create-deck (deck-name)
  "Request AnkiConnect for creating a deck named DECK-NAME."
  (anki-editor--anki-connect-invoke-result "createDeck" 5 `((deck . ,deck-name))))

(defun anki-editor--set-failure-reason (reason)
  "Set failure reason to REASON in property drawer at point."
  (org-entry-put nil (anki-editor--keyword-name anki-editor-prop-failure-reason) reason))

(defun anki-editor--clear-failure-reason ()
  "Clear failure reason in property drawer at point."
  (org-entry-delete nil (anki-editor--keyword-name anki-editor-prop-failure-reason)))

(defun anki-editor-is-valid-org-tag (tag)
  "Check if string TAG can be used as an Org tag."
  (string-match-p anki-editor-org-tag-regexp tag))

(defun anki-editor-all-tags ()
  "Get all tags from Anki."
  (let (anki-tags)
    (prog1
        (setq anki-tags (anki-editor--anki-connect-invoke-result "getTags" 5))
      (unless (seq-every-p #'anki-editor-is-valid-org-tag anki-tags)
        (warn "Some tags from Anki contain characters that are not valid in Org tags.")))))

(defun anki-editor--before-set-tags (&optional _ just-align)
  "Build tag list for completion including tags from Anki.

When the value of `org-current-tag-alist' is non-nil, just append
to it.

Otherwise, advise function `org-get-buffer-tags' to append tags
from Anki to the result.

Do nothing when JUST-ALIGN is non-nil."
  (unless just-align
    (if org-current-tag-alist
        (setq org-current-tag-alist
              (org-tag-add-to-alist
               (mapcar #'list (anki-editor-all-tags))
               org-current-tag-alist))
      (unless (advice-member-p 'anki-editor--get-buffer-tags #'org-get-buffer-tags)
        (advice-add 'org-get-buffer-tags :around #'anki-editor--get-buffer-tags)))))

(defun anki-editor--get-buffer-tags (oldfun)
  "Append tags from Anki to the result of applying OLDFUN."
  (append (funcall oldfun) (mapcar #'list (anki-editor-all-tags))))

;; TODO: consider turn this package into a minor mode to enable it to stop advising ?
(advice-add 'org-set-tags :before #'anki-editor--before-set-tags)

(defun anki-editor--heading-to-note (heading)
  "Construct an alist representing a note for HEADING."
  (let (note-id note-type tags fields)
    (setq note-id (org-element-property anki-editor-prop-note-id heading)
          note-type (org-element-property anki-editor-prop-note-type heading)
          tags (org-get-tags-at)
          fields (mapcar #'anki-editor--heading-to-note-field (anki-editor--get-subheadings heading)))

    (unless note-type (error "Missing note type"))
    (unless fields (error "Missing fields"))

    `((note-id . ,(string-to-number (or note-id "-1")))
      (note-type . ,note-type)
      (tags . ,tags)
      (fields . ,fields))))

(defun anki-editor--heading-to-note-field (heading)
  "Convert HEADING to field data, a cons cell, the car of which is the field name, the cdr of which is contens represented in HTML."
  (let ((field-name (substring-no-properties
                     (org-element-property
                      :raw-value
                      heading)))
        (contents (org-element-contents heading)))
    `(,field-name . ,(org-export-string-as
                      (org-element-interpret-data contents)
                      anki-editor--ox-anki-html-backend t))))

;;; Org Export Backend

(defconst anki-editor--ox-anki-html-backend
  (org-export-create-backend
   :parent 'html
   :transcoders '((latex-fragment . anki-editor--ox-latex)
                  (latex-environment . anki-editor--ox-latex)
                  (link . anki-editor--ox-link))))

(defconst anki-editor--anki-latex-syntax-map
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

(defun anki-editor--ox-latex (latex contents info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((code (org-element-property :value latex))
         (copy code))
    ;; translate delimiters
    (dolist (map anki-editor--anki-latex-syntax-map)
      (setq code (replace-regexp-in-string (car map) (cdr map) code t t)))

    (when (equal copy code)
      (setq code (anki-editor--wrap-latex
                  (if (eq (org-element-type latex) 'latex-fragment)
                      code
                    (format "\n<pre>\n%s</pre>\n"
                            (org-remove-indentation code))))))

    (if anki-editor-break-consecutive-braces-in-latex
        (replace-regexp-in-string "}}" "} } " code)
      code)))

(defun anki-editor--ox-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  THE
IMPLEMENTATION IS BASICALLY COPIED AND SIMPLIFIED FROM
ox-html.el :)"
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (org-string-nw-p desc))
         (path
          (cond
           ((member type '("http" "https" "ftp" "mailto" "news"))
            (url-encode-url (org-link-unescape (concat type ":" raw-path))))
           ((string= type "file")
            ;; Possibly append `:html-link-home' to relative file
            ;; name.
            (let ((home (and (plist-get info :html-link-home)
                             (org-trim (plist-get info :html-link-home)))))
              (when (and home
                         (plist-get info :html-link-use-abs-url)
                         (file-name-absolute-p raw-path))
                (setq raw-path (concat (file-name-as-directory home) raw-path)))
              (message "Storing media file to Anki for %s..." raw-path)
              (anki-editor--anki-connect-store-media-file (expand-file-name (url-unhex-string raw-path)))))
           (t raw-path)))
          ;; Extract attributes from parent's paragraph.  HACK: Only do
          ;; this for the first link in parent (inner image link for
          ;; inline images).  This is needed as long as attributes
          ;; cannot be set on a per link basis.
          (attributes-plist
           (let* ((parent (org-export-get-parent-element link))
                  (link (let ((container (org-export-get-parent link)))
                          (if (and (eq (org-element-type container) 'link)
                                   (org-html-inline-image-p link info))
                              container
                            link))))
             (and (eq (org-element-map parent 'link 'identity info t) link)
                  (org-export-read-attribute :attr_html parent))))
          (attributes
           (let ((attr (org-html--make-attribute-string attributes-plist)))
             (if (org-string-nw-p attr) (concat " " attr) ""))))
         (cond
          ;; Image file.
          ((and (plist-get info :html-inline-images)
                (org-export-inline-image-p
                 link (plist-get info :html-inline-image-rules)))
           (org-html--format-image path attributes-plist info))
          ;; Radio target: Transcode target's contents and use them as
          ;; link's description.
          ((string= type "radio")
           (let ((destination (org-export-resolve-radio-link link info)))
             (if (not destination) desc
               (format "<a href=\"#%s\"%s>%s</a>"
                       (org-export-get-reference destination info)
                       attributes
                       desc))))
          ;; Links pointing to a headline: Find destination and build
          ;; appropriate referencing command.
          ((member type '("custom-id" "fuzzy" "id"))
           (let ((destination (if (string= type "fuzzy")
                                  (org-export-resolve-fuzzy-link link info)
                                (org-export-resolve-id-link link info))))
             (pcase (org-element-type destination)
               ;; ID link points to an external file.
               (`plain-text
                (let ((fragment (concat "ID-" path)))
                  (format "<a href=\"%s#%s\"%s>%s</a>"
                          destination fragment attributes (or desc destination))))
               ;; Fuzzy link points nowhere.
               (`nil
                (format "<i>%s</i>"
                        (or desc
                            (org-export-data
                             (org-element-property :raw-link link) info))))
               ;; Link points to a headline.
               (`headline
                (let ((href (or (org-element-property :CUSTOM_ID destination)
                                (org-export-get-reference destination info)))
                      ;; What description to use?
                      (desc
                       ;; Case 1: Headline is numbered and LINK has no
                       ;; description.  Display section number.
                       (if (and (org-export-numbered-headline-p destination info)
                                (not desc))
                           (mapconcat #'number-to-string
                                      (org-export-get-headline-number
                                       destination info) ".")
                         ;; Case 2: Either the headline is un-numbered or
                         ;; LINK has a custom description.  Display LINK's
                         ;; description or headline's title.
                         (or desc
                             (org-export-data
                              (org-element-property :title destination) info)))))
                  (format "<a href=\"#%s\"%s>%s</a>" href attributes desc)))
               ;; Fuzzy link points to a target or an element.
               (_
                (let* ((ref (org-export-get-reference destination info))
                       (org-html-standalone-image-predicate
                        #'org-html--has-caption-p)
                       (number (cond
                                (desc nil)
                                ((org-html-standalone-image-p destination info)
                                 (org-export-get-ordinal
                                  (org-element-map destination 'link
                                    #'identity info t)
                                  info 'link 'org-html-standalone-image-p))
                                (t (org-export-get-ordinal
                                    destination info nil 'org-html--has-caption-p))))
                       (desc (cond (desc)
                                   ((not number) "No description for this link")
                                   ((numberp number) (number-to-string number))
                                   (t (mapconcat #'number-to-string number ".")))))
                  (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc))))))
          ;; Coderef: replace link with the reference name or the
          ;; equivalent line number.
          ((string= type "coderef")
           (let ((fragment (concat "coderef-" (org-html-encode-plain-text path))))
             (format "<a href=\"#%s\" %s%s>%s</a>"
                     fragment
                     (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
                             fragment fragment)
                     attributes
                     (format (org-export-get-coderef-format path desc)
                             (org-export-resolve-coderef path info)))))
          ;; External link with a description part.
          ((and path desc) (format "<a href=\"%s\"%s>%s</a>"
                                   (org-html-encode-plain-text path)
                                   attributes
                                   desc))
          ;; External link without a description part.
          (path (let ((path (org-html-encode-plain-text path)))
                  (format "<a href=\"%s\"%s>%s</a>"
                          path
                          attributes
                          (org-link-unescape path))))
          ;; No path, only description.  Try to do something useful.
          (t (format "<i>%s</i>" desc)))))

;;; Utilities

(defun anki-editor--keyword-name (keyword)
  "Get name of a keyword symbol KEYWORD without leading `:'."
  (substring (symbol-name keyword) 1))

(defun anki-editor--set-tags-fix (tags)
  "Set tags to TAGS and fix tags on the fly."
  (org-set-tags-to tags)
  (org-fix-tags-on-the-fly))

(defun anki-editor--get-subheadings (heading)
  "Get all the subheadings of HEADING."
  (org-element-map (org-element-contents heading)
      'headline 'identity nil nil 'headline))

(defun anki-editor--visit-ancestor-headings (visitor &optional level)
  "Move point to and call VISITOR at each ancestor heading from point.
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
      (anki-editor--visit-ancestor-headings visitor level))))


(provide 'anki-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor.el ends here
