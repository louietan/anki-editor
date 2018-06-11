;;; anki-editor.el --- Minor mode for making Anki cards with Org  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 Lei Tan <louietanlei@gmail.com>
;;
;; Description: Make Anki Cards in Org-mode
;; Author: Lei Tan
;; Version: 0.3.0
;; Package-Requires: ((emacs "25") (request "0.3.0") (dash "2.12.0"))
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
;;  * Item                     :emacs:lisp:programming:
;;    :PROPERTIES:
;;    :ANKI_DECK: Computing
;;    :ANKI_NOTE_TYPE: Basic
;;    :END:
;;  ** Front
;;     How to hello world in elisp ?
;;  ** Back
;;     #+BEGIN_SRC emacs-lisp
;;       (message "Hello, world!")
;;     #+END_SRC
;;
;;  This package extends Org-mode's built-in HTML backend to generate
;;  HTML for contents of note fields with specific syntax (e.g. latex)
;;  translated to Anki style, then save the note to Anki.
;;
;;  For this package to work, you have to setup these external dependencies:
;;  - curl
;;  - AnkiConnect, an Anki addon that runs an HTTP server to expose
;;                 Anki functions as RESTful APIs, see
;;                 https://github.com/FooSoft/anki-connect#installation
;;                 for installation instructions
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

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 'org-element)
(require 'ox)
(require 'ox-html)
(require 'request)

(defconst anki-editor-prop-note-type "ANKI_NOTE_TYPE")
(defconst anki-editor-prop-note-id "ANKI_NOTE_ID")
(defconst anki-editor-prop-deck "ANKI_DECK")
(defconst anki-editor-prop-failure-reason "ANKI_FAILURE_REASON")
(defconst anki-editor-buffer-html-output "*AnkiEditor HTML Output*")
(defconst anki-editor-org-tag-regexp "^\\([[:alnum:]_@#%]+\\)+$")

(defgroup anki-editor nil
  "Customizations for anki-editor."
  :group 'org)

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

(defun anki-editor--anki-connect-action (action &optional params version)
  (let (a)
    (when version
      (push `(version . ,version) a))
    (when params
      (push `(params . ,params) a))
    (push `(action . ,action) a)))

(defun anki-editor--anki-connect-invoke-queue ()
  (let (action-queue)
    (lambda (&optional action params handler)
      (if action
          (push (cons (anki-editor--anki-connect-action action params) handler) action-queue)
        (when action-queue
          (apply #'anki-editor--anki-connect-invoke-multi (nreverse action-queue))
          (setq action-queue nil))))))

(defun anki-editor--anki-connect-invoke (action &optional params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((request-body (json-encode (anki-editor--anki-connect-action action params 5)))
        (request-backend 'curl)
        (json-array-type 'list)
        reply err)

    (let ((response (request (format "http://%s:%s"
                                     anki-editor-anki-connect-listening-address
                                     anki-editor-anki-connect-listening-port)
                             :type "POST"
                             :parser 'json-read
                             :data (encode-coding-string request-body 'utf-8)
                             :success (cl-function (lambda (&key data &allow-other-keys)
                                                     (setq reply data)))
                             :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
                                                   (setq err (string-trim (cdr error-thrown)))))
                             :sync t)))

      ;; HACK: I expect the behavior of the sync mode to be that
      ;; callbacks get called before the invocation to `request' ends,
      ;; but it seems not to be the case (or I get it wrong ?) that
      ;; sometimes when the curl process finishes, the
      ;; `request--curl-callback' (the sentinel of the curl process,
      ;; which calls `request--callback', which subsequently calls the
      ;; callbacks) get called after `request--curl-sync' ends. Here I
      ;; check if the `done-p' is nil (which will be set to `t' after
      ;; callbacks have been called) and call `request--curl-callback'
      ;; manually.
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))

    (when err (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defmacro anki-editor--anki-connect-invoke-result (&rest args)
  "Invoke AnkiConnect with ARGS, return the result from response or raise an error."
  `(let-alist (anki-editor--anki-connect-invoke ,@args)
     (when .error (error .error))
     .result))

(defun anki-editor--anki-connect-invoke-multi (&rest actions)
  (-zip-with (lambda (result handler) (and handler (funcall handler result)))
             (anki-editor--anki-connect-invoke-result
              "multi" `((actions . ,(mapcar #'car actions))))
             (mapcar #'cdr actions)))

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

(defun anki-editor--anki-connect-store-media-file (path)
  "Store media file for PATH, which is an absolute file name.
The result is the path to the newly stored media file."
  (unless (-all? #'executable-find '("base64" "sha1sum"))
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
     "storeMediaFile"
     `((filename . ,media-file-name)
       (data . ,content)))
    media-file-name))


;;; Org Export Backend

(defconst anki-editor--ox-anki-html-backend
  (org-export-create-backend
   :parent 'html
   :transcoders '((latex-fragment . anki-editor--ox-latex)
                  (latex-environment . anki-editor--ox-latex))))

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

(defun anki-editor--ox-latex (latex _contents _info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((code (org-element-property :value latex))
         (copy code))
    ;; translate delimiters
    (dolist (map anki-editor--anki-latex-syntax-map)
      (setq code (replace-regexp-in-string (car map) (cdr map) code t t)))

    (when (string= copy code)
      (setq code (anki-editor--wrap-latex
                  (if (eq (org-element-type latex) 'latex-fragment)
                      code
                    (format "\n<pre>\n%s</pre>\n"
                            (org-remove-indentation code))))))

    (if anki-editor-break-consecutive-braces-in-latex
        (replace-regexp-in-string "}}" "} } " code)
      code)))

(defun anki-editor--ox-html-link (oldfun link desc info)
  "When LINK is a link to local file, transcodes it to html and stores the target file to Anki, otherwise calls OLDFUN for help.
The implementation is borrowed and simplified from ox-html."
  (or (catch 'giveup
        (let* ((type (org-element-property :type link))
               (raw-path (org-element-property :path link))
               (desc (org-string-nw-p desc))
               (path
                (cond
                 ((string= type "file")
                  ;; Possibly append `:html-link-home' to relative file
                  ;; name.
                  (let ((inhibit-message nil)
                        (home (and (plist-get info :html-link-home)
                                   (org-trim (plist-get info :html-link-home)))))
                    (when (and home
                               (plist-get info :html-link-use-abs-url)
                               (file-name-absolute-p raw-path))
                      (setq raw-path (concat (file-name-as-directory home) raw-path)))
                    (message "Storing media file to Anki for %s..." raw-path)
                    ;; storing file to Anki and return the modified path
                    (anki-editor--anki-connect-store-media-file (expand-file-name (url-unhex-string raw-path)))))
                 (t (throw 'giveup nil))))
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

           (t (throw 'giveup nil)))))
      (funcall oldfun link desc info)))


;;; Utilities

(defun anki-editor--get-subheadings (heading)
  "Get all the subheadings of HEADING."
  (org-element-map (org-element-contents heading)
      'headline 'identity nil nil 'headline))


;;; Core Functions

(defun anki-editor--process-note-heading ()
  "Process note heading at point."
  (-->
   (org-element-at-point)
   (let ((content (buffer-substring
                   (org-element-property :begin it)
                   ;; in case the buffer is narrowed,
                   ;; e.g. by `org-map-entries' when
                   ;; scope is `tree'
                   (min (point-max) (org-element-property :end it)))))
     (with-temp-buffer
       (org-mode)
       (insert content)
       (car (org-element-contents (org-element-parse-buffer)))))
   (anki-editor--heading-to-note it)
   (anki-editor--save-note it)))

(defun anki-editor--insert-note-skeleton (prefix deck heading note-type fields)
  "Insert a note subtree (skeleton) with HEADING, NOTE-TYPE and FIELDS.
Where the subtree is created depends on PREFIX."
  (org-insert-heading prefix)
  (insert heading)

  (unless (save-excursion
            (org-up-heading-safe)
            ;; don't insert `ANKI_DECK' if some ancestor already has
            ;; the same value
            (and (not (string-blank-p deck))
                 (string= deck (org-entry-get-with-inheritance anki-editor-prop-deck))))
    (org-set-property anki-editor-prop-deck deck))

  (org-set-property anki-editor-prop-note-type note-type)

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

(defun anki-editor--set-note-id (id)
  (unless id
    (error "Note creation failed for unknown reason"))
  (org-set-property anki-editor-prop-note-id (number-to-string id)))

(defun anki-editor--create-note (note)
  "Request AnkiConnect for creating NOTE."
  (let ((queue (anki-editor--anki-connect-invoke-queue)))
    (when anki-editor-create-decks
      (funcall queue
               'createDeck
               `((deck . ,(alist-get 'deck note)))))

    (funcall queue
             'addNote
             `((note . ,(anki-editor--anki-connect-map-note note)))
             #'anki-editor--set-note-id)

    (funcall queue)))

(defun anki-editor--update-note (note)
  "Request AnkiConnect for updating fields and tags of NOTE."

  (let ((queue (anki-editor--anki-connect-invoke-queue)))
    (funcall queue
             'updateNoteFields
             `((note . ,(anki-editor--anki-connect-map-note note))))

    (funcall queue
             'notesInfo
             `((notes . (,(alist-get 'note-id note))))
             (lambda (result)
               ;; update tags
               (let* ((existing-note (car result))
                      (tags-to-add (-difference (alist-get 'tags note) (alist-get 'tags existing-note)))
                      (tags-to-remove (-difference (alist-get 'tags existing-note) (alist-get 'tags note)))
                      (tag-queue (anki-editor--anki-connect-invoke-queue)))

                 (when tags-to-add
                   (funcall tag-queue
                            'addTags `((notes . (,(alist-get 'note-id note)))
                                       (tags . ,(mapconcat #'identity tags-to-add " ")))))

                 (when tags-to-remove
                   (funcall tag-queue
                            'removeTags `((notes . (,(alist-get 'note-id note)))
                                          (tags . ,(mapconcat #'identity tags-to-remove " ")))))

                 (funcall tag-queue))))

    (funcall queue)))

(defun anki-editor--set-failure-reason (reason)
  "Set failure reason to REASON in property drawer at point."
  (org-entry-put nil anki-editor-prop-failure-reason reason))

(defun anki-editor--clear-failure-reason ()
  "Clear failure reason in property drawer at point."
  (org-entry-delete nil anki-editor-prop-failure-reason))

(defun anki-editor--get-allowed-values-for-property (property)
  "Get allowed values for PROPERTY."
  (pcase property
    ((pred (string= anki-editor-prop-deck)) (anki-editor-deck-names))
    ((pred (string= anki-editor-prop-note-type)) (anki-editor-note-types))
    (_ nil)))

(defun anki-editor-is-valid-org-tag (tag)
  "Check if string TAG can be used as an Org tag."
  (string-match-p anki-editor-org-tag-regexp tag))

(defun anki-editor-all-tags ()
  "Get all tags from Anki."
  (let (anki-tags)
    (prog1
        (setq anki-tags (anki-editor--anki-connect-invoke-result "getTags"))
      (unless (-all? #'anki-editor-is-valid-org-tag anki-tags)
        (warn "Some tags from Anki contain characters that are not valid in Org tags.")))))

(defun anki-editor-deck-names ()
  "Get all decks names from Anki."
  (anki-editor--anki-connect-invoke-result "deckNames"))

(defun anki-editor--before-set-tags (&optional _ just-align)
  "Build tag list for completion including tags from Anki.

When the value of `org-current-tag-alist' is non-nil, just append
to it.

Otherwise, advise function `org-get-buffer-tags' to append tags
from Anki to the result.

Do nothing when JUST-ALIGN is non-nil."
  (unless (or just-align
              (advice-member-p 'anki-editor--get-buffer-tags #'org-get-buffer-tags))
    (advice-add 'org-get-buffer-tags :around #'anki-editor--get-buffer-tags)))

(defun anki-editor--get-buffer-tags (oldfun)
  "Append tags from Anki to the result of applying OLDFUN."
  (append (funcall oldfun) (mapcar #'list (anki-editor-all-tags))))

(defun anki-editor-note-types ()
  "Get note types from Anki."
  (anki-editor--anki-connect-invoke-result "modelNames"))

(defun anki-editor--heading-to-note (heading)
  "Construct an alist representing a note for HEADING."
  (let ((org-trust-scanner-tags t)
        (deck (org-entry-get-with-inheritance anki-editor-prop-deck))
        (note-id (org-entry-get nil anki-editor-prop-note-id))
        (note-type (org-entry-get nil anki-editor-prop-note-type))
        (tags (org-get-tags-at))
        (fields (mapcar #'anki-editor--heading-to-note-field (anki-editor--get-subheadings heading))))

    (unless deck (error "No deck specified"))
    (unless note-type (error "Missing note type"))
    (unless fields (error "Missing fields"))

    `((deck . ,deck)
      (note-id . ,(string-to-number (or note-id "-1")))
      (note-type . ,note-type)
      (tags . ,tags)
      (fields . ,fields))))

(defun anki-editor--heading-to-note-field (heading)
  "Convert HEADING to field data, a cons cell, the car of which is the field name, the cdr of which is contens represented in HTML."
  (let ((inhibit-message t)  ;; suppress echo message from `org-babel-exp-src-block'
        (field-name (substring-no-properties
                     (org-element-property
                      :raw-value
                      heading)))
        (contents (org-element-contents heading)))

    `(,field-name . ,(or (org-export-string-as
                          (org-element-interpret-data contents)
                          anki-editor--ox-anki-html-backend t)
                         ;; 8.2.10 version of
                         ;; `org-export-filter-apply-functions'
                         ;; returns nil for an input of empty string,
                         ;; which will cause AnkiConnect to fail
                         ""))))


;;; Minor mode

;;;###autoload
(define-minor-mode anki-editor-mode
  "anki-eidtor-mode"
  :lighter " anki-editor"
  (if anki-editor-mode (anki-editor-setup-minor-mode)
    (anki-editor-teardown-minor-mode)))

(defun anki-editor-setup-minor-mode ()
  "Set up this minor mode."
  (add-hook 'org-property-allowed-value-functions #'anki-editor--get-allowed-values-for-property)
  (advice-add 'org-set-tags :before #'anki-editor--before-set-tags)
  (advice-add 'org-html-link :around #'anki-editor--ox-html-link))

(defun anki-editor-teardown-minor-mode ()
  "Tear down this minor mode."
  (remove-hook 'org-property-allowed-value-functions #'anki-editor--get-allowed-values-for-property)
  (advice-remove 'org-set-tags #'anki-editor--before-set-tags)
  (when (advice-member-p 'anki-editor--get-buffer-tags #'org-get-buffer-tags)
    (advice-remove 'org-get-buffer-tags #'anki-editor--get-buffer-tags))
  (advice-remove 'org-html-link #'anki-editor--ox-html-link))


;;; Commands

(defun anki-editor-push-notes (&optional arg match scope)
  "Build notes from headings that can be matched by MATCH within SCOPE and push them to Anki.

The default search condition `&ANKI_NOTE_TYPE<>\"\"' will always
be appended to MATCH.

For notes that already exist in Anki (i.e. has `ANKI_NOTE_ID'
property), only their fields and tags will be updated, change of
deck or note type are currently not supported.

If SCOPE is not specified, the following rules are applied to
determine the scope:

- If there's an active region, it will be set to `region'
- If called with prefix `C-u', it will be set to `tree'
- If called with prefix double `C-u', it will be set to `file'
- If called with prefix triple `C-u', will be set to `agenda'

See doc string of `org-map-entries' for what these different options mean.

If one fails, the failure reason will be set in property drawer
of that heading."
  (interactive "P")

  (unless scope
    (setq scope (cond
                 ((region-active-p) 'region)
                 ((equal arg '(4)) 'tree)
                 ((equal arg '(16)) 'file)
                 ((equal arg '(64)) 'agenda)
                 (t nil))))
  (setq match (concat match "&" anki-editor-prop-note-type "<>\"\""))

  (let ((total (progn
                 (message "Counting notes...")
                 (length (org-map-entries t match scope))))
        (acc 0)
        (failed 0))
    (org-map-entries (lambda ()
                       (message "[%d/%d] Processing notes in buffer \"%s\", wait a moment..."
                                (cl-incf acc) total (buffer-name))
                       (anki-editor--clear-failure-reason)
                       (condition-case err
                           (anki-editor--process-note-heading)
                         (error (cl-incf failed)
                                (anki-editor--set-failure-reason (error-message-string err)))))
                     match
                     scope)

    (message (if (= 0 failed)
                 (format "Successfully pushed %d notes to Anki." acc)
               (format "Pushed %d notes, %d of which are failed. Check property drawers for failure reasons. Once you've fixed the issues, you could use `anki-editor-retry-failure-notes' to re-push the failed notes."
                       acc failed)))))

(defun anki-editor-retry-failure-notes (&optional arg scope)
  "Retry pushing notes that were failed.
This command just calls `anki-editor-submit' with match string
matching non-empty `ANKI_FAILURE_REASON' properties."
  (interactive "P")
  (anki-editor-push-notes arg (concat anki-editor-prop-failure-reason "<>\"\"") scope))

(defun anki-editor-insert-note (&optional prefix)
  "Insert a note interactively.

Where the note subtree is placed depends on PREFIX, which is the
same as how it is used by `M-RET'(org-insert-heading)."
  (interactive "P")
  (message "Fetching note types...")
  (let* ((deck (or (org-entry-get-with-inheritance anki-editor-prop-deck)
                   (progn
                     (message "Fetching decks...")
                     (completing-read "Choose a deck: "
                                      (sort (anki-editor-deck-names) #'string-lessp)))))
         (note-type (completing-read "Choose a note type: "
                                     (sort (anki-editor-note-types) #'string-lessp)))
         (fields (progn
                   (message "Fetching note fields...")
                   (anki-editor--anki-connect-invoke-result "modelFieldNames" `((modelName . ,note-type)))))
         (note-heading (read-from-minibuffer "Enter the note heading (optional): ")))

    (anki-editor--insert-note-skeleton prefix
                                       deck
                                       (if (string-blank-p note-heading)
                                           "Item"
                                         note-heading)
                                       note-type
                                       fields)))

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
                (unless (string-blank-p hint) (princ (format "::%s" hint)))
                (princ "}}"))))))

(defun anki-editor-export-subtree-to-html ()
  "Export subtree of the element at point to HTML."
  (interactive)
  (org-export-to-buffer
      anki-editor--ox-anki-html-backend
      anki-editor-buffer-html-output nil t nil t nil
      #'html-mode))

(defun anki-editor-convert-region-to-html ()
  "Convert and replace region to HTML."
  (interactive)
  (org-export-replace-region-by anki-editor--ox-anki-html-backend))

(defun anki-editor-anki-connect-upgrade ()
  "Upgrade AnkiConnect to the latest version.

This will display a confirmation dialog box in Anki asking if you
want to continue.  The upgrading is done by downloading the latest
code in the master branch of its Github repo.

This is useful when new version of this package depends on the
bugfixes or new features of AnkiConnect."
  (interactive)
  (when (yes-or-no-p "NOTE: This will download the latest codebase of AnkiConnect to your system, which is not guaranteed to be safe or stable. Generally, you don't need this command, this is useful only when new version of this package requires the updates of AnkiConnect that are not released yet. Do you still want to continue?")
    (let ((result (anki-editor--anki-connect-invoke-result "upgrade")))
      (when (and (booleanp result) result)
        (message "AnkiConnect has been upgraded, you might have to restart Anki to make it in effect.")))))


(provide 'anki-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor.el ends here
