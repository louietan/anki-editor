;;; anki-editor.el --- Minor mode for making Anki cards with Org  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018-2019 Lei Tan <louietanlei[at]gmail[dot]com>
;;
;; Description: Make Anki Cards in Org-mode
;; Author: Lei Tan
;; Version: 0.3.3
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/louietan/anki-editor
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This package is for users of both Emacs and Anki, who'd like to
;;  make Anki cards in Org mode.  With this package, Anki cards can be
;;  made from an Org buffer like below: (inspired by org-drill)
;;
;;  * Sample                  :emacs:lisp:programming:
;;    :PROPERTIES:
;;    :ANKI_DECK: Computing
;;    :ANKI_NOTE_TYPE: Basic
;;    :END:
;;  ** Front
;;     How to say "hello world" in elisp?
;;  ** Back
;;     #+BEGIN_SRC emacs-lisp
;;       (message "Hello, world!")
;;     #+END_SRC
;;
;;  This package extends Org-mode's built-in HTML backend to generate
;;  HTML for contents of note fields with specific syntax (e.g. latex)
;;  translated to Anki style.
;;
;;  For this package to work, you have to setup these external dependencies:
;;  - curl
;;  - AnkiConnect, an Anki addon that runs an RPC server over HTTP to expose
;;                 Anki functions as APIs,
;;                 see https://github.com/FooSoft/anki-connect#installation
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
(require 'json)
(require 'org-element)
(require 'ox)
(require 'ox-html)

(defgroup anki-editor nil
  "Customizations for anki-editor."
  :group 'org)

(defcustom anki-editor-break-consecutive-braces-in-latex
  nil
  "If non-nil, consecutive `}' will be automatically separated by spaces to prevent early-closing of cloze.
See https://apps.ankiweb.net/docs/manual.html#latex-conflicts."
  :type 'boolean)

(defcustom anki-editor-org-tags-as-anki-tags
  t
  "If nil, tags of entries won't be counted as Anki tags."
  :type 'boolean)

(defcustom anki-editor-protected-tags
  '("marked" "leech")
  "A list of tags that won't be deleted from Anki even though
they're absent in Org entries, such as special tags `marked',
`leech'."
  :type '(repeat string))

(defcustom anki-editor-ignored-org-tags
  (append org-export-select-tags org-export-exclude-tags)
  "A list of Org tags that are ignored when constructing notes
form entries."
  :type '(repeat string))

(defcustom anki-editor-api-host
  "127.0.0.1"
  "The network address AnkiConnect is listening."
  :type 'string)

(defcustom anki-editor-api-port
  "8765"
  "The port number AnkiConnect is listening."
  :type 'string)

(defcustom anki-editor-latex-style 'builtin
  "The style of latex to translate into."
  :type '(radio (const :tag "Built-in" builtin)
                (const :tag "MathJax" mathjax)))

(defcustom anki-editor-include-default-style t
  "Wheter or not to include `org-html-style-default' when using `anki-editor-copy-styles'.
For example, you might want to turn this off when you are going to
provide your custom styles in `anki-editor-html-head'."
  :type 'boolean)

(defcustom anki-editor-html-head nil
  "Additional html tags to append to card stylings when using `anki-editor-copy-styles'.
For example, you can put custom styles or scripts in this variable."
  :type 'string)


;;; AnkiConnect

(defconst anki-editor-api-version 5)

(cl-defun anki-editor--fetch (url
                              &rest settings
                              &key
                              (type "GET")
                              data success _error
                              (parser 'buffer-string)
                              &allow-other-keys)
  "This is a simplistic little function to make http requests using cURL.
The api is borrowed from request.el.  It exists because
request.el's sync mode calls cURL asynchronously under the hood,
which doesn't work on some machines (like mine) where the process
sentinel never gets called.  After some debugging of Emacs, it
seems that in 'process.c' the pselect syscall to the file
descriptor of inotify used by 'autorevert' always returns a
nonzero value and causes 'status_notify' never being called.  To
determine whether it's a bug in Emacs and make a patch requires
more digging."
  (let ((tempfile (make-temp-file "emacs-anki-editor"))
        (responsebuf (generate-new-buffer " *anki-editor-curl*")))
    (when data
      (with-temp-file tempfile
        (setq buffer-file-coding-system 'utf-8)
        (set-buffer-multibyte t)
        (insert data)))
    (unwind-protect
        (with-current-buffer responsebuf
          (apply #'call-process "curl" nil t nil (list
                                                  url
                                                  "--silent"
                                                  "-X" type
                                                  "--data-binary"
                                                  (concat "@" tempfile)))

          (goto-char (point-min))
          (when success
            (apply success (list :data (funcall parser)))))
      (kill-buffer responsebuf)
      (delete-file tempfile))))

(defun anki-editor-api-call (action &rest params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((payload (list :action action :version anki-editor-api-version))
        (_request-backend 'curl)
        (json-array-type 'list)
        reply err)

    (when params
      (plist-put payload :params params))

    (anki-editor--fetch (format "http://%s:%s"
                                anki-editor-api-host
                                anki-editor-api-port)
                        :type "POST"
                        :parser 'json-read
                        :data (json-encode payload)
                        :success (cl-function (lambda (&key data &allow-other-keys)
                                                (setq reply data)))
                        :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                                              (setq err (string-trim (cdr error-thrown)))))
                        :sync t)
    (when err (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defun anki-editor-api-call-result (&rest args)
  "Invoke AnkiConnect with ARGS, return the result from response
or raise an error."
  (let-alist (apply #'anki-editor-api-call args)
    (when .error (error .error))
    .result))

(defmacro anki-editor-api-with-multi (&rest body)
  "Use in combination with `anki-editor-api-enqueue' to combine
multiple api calls into a single 'multi' call, return the results
of these calls in the same order."
  `(let (--anki-editor-var-multi-actions--
         --anki-editor-var-multi-results--)
     ,@body
     (setq --anki-editor-var-multi-results--
           (anki-editor-api-call-result
            'multi
            :actions (nreverse
                      ;; Here we make a vector from the action list,
                      ;; or `json-encode' will consider it as an association list.
                      (vconcat
                       --anki-editor-var-multi-actions--))))
     (cl-loop for result in --anki-editor-var-multi-results--
              do (when-let ((pred (listp result))
                            (err (alist-get 'error result)))
                   (error err))
              collect result)))

(defmacro anki-editor-api-enqueue (action &rest params)
  "Like `anki-editor-api-call', but is only used in combination
with `anki-editor-api-with-multi'.  Instead of sending the
request directly, it simply queues the request."
  `(let ((action (list :action ,action))
         (params (list ,@params)))
     (when params
       (plist-put action :params params))
     (push action --anki-editor-var-multi-actions--)))

(defun anki-editor-api--note (note)
  "Convert NOTE to the form that AnkiConnect accepts."
  (list
   :id (string-to-number (or (anki-editor-note-id note) "0"))
   :deckName (anki-editor-note-deck note)
   :modelName (anki-editor-note-model note)
   :fields (anki-editor-note-fields note)
   ;; Convert tags to a vector since empty list is identical to nil
   ;; which will become None in Python, but AnkiConnect requires it
   ;; to be type of list.
   :tags (vconcat (anki-editor-note-tags note))))

(defun anki-editor-api--store-media-file (path)
  "Store media file for PATH, which is an absolute file name.
The result is the path to the newly stored media file."
  (let* ((bytes (with-temp-buffer
                  (insert-file-contents-literally path)
                  (buffer-string)))
         (hash (secure-hash 'sha1 bytes))
         (media-file-name (format "%s-%s%s"
                                  (file-name-base path)
                                  hash
                                  (file-name-extension path t))))
    (when (eq :json-false
              (anki-editor-api-call-result 'retrieveMediaFile
                                           :filename media-file-name))
      (message "Storing media file %s to Anki, this might take a while" path)
      (anki-editor-api-call-result 'storeMediaFile
                                   :filename media-file-name
                                   :data (base64-encode-string bytes)))
    media-file-name))


;;; Org export backend

(defconst anki-editor--ox-anki-html-backend
  (org-export-create-backend
   :parent 'html
   :transcoders '((latex-fragment . anki-editor--ox-latex)
                  (latex-environment . anki-editor--ox-latex))))

(defconst anki-editor--ox-export-ext-plist
  '(:with-toc nil :with-properties nil :with-planning nil :anki-editor-mode t))

(cl-macrolet ((with-table (table)
                          `(cl-loop for delims in ,table
                                    collect
                                    (list (concat "^" (regexp-quote (cl-first delims)))
                                          (cl-second delims)
                                          (concat (regexp-quote (cl-third delims)) "$")
                                          (cl-fourth delims)))))

  (defconst anki-editor--native-latex-delimiters
    (with-table '(("$$" "[$$]"
                   "$$" "[/$$]")
                  ("$" "[$]"
                   "$" "[/$]")
                  ("\\(" "[$]"
                   "\\)" "[/$]")
                  ("\\[" "[$$]"
                   "\\]" "[/$$]"))))

  (defconst anki-editor--mathjax-delimiters
    (with-table '(("$$" "\\["
                   "$$" "\\]")
                  ("$" "\\("
                   "$" "\\)")))))

(defun anki-editor--translate-latex-fragment (latex-code)
  (cl-loop for delims in (cl-ecase anki-editor-latex-style
                           (builtin anki-editor--native-latex-delimiters)
                           (mathjax anki-editor--mathjax-delimiters))
           for matches = (string-match (cl-first delims) latex-code)
           when matches
           do
           (setq latex-code (replace-match (cl-second delims) t t latex-code))
           (string-match (cl-third delims) latex-code)
           (setq latex-code (replace-match (cl-fourth delims) t t latex-code))
           until matches
           finally return latex-code))

(defun anki-editor--translate-latex-env (latex-code)
  (setq latex-code (replace-regexp-in-string "\n" "<br>" (org-html-encode-plain-text latex-code)))
  (cl-ecase anki-editor-latex-style
    (builtin (concat "[latex]<br>" latex-code "[/latex]"))
    (mathjax (concat "\\[<br>" latex-code "\\]"))))

(defun anki-editor--ox-latex (latex _contents _info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((code (org-remove-indentation (org-element-property :value latex))))
    (setq code (cl-ecase (org-element-type latex)
                 (latex-fragment (anki-editor--translate-latex-fragment code))
                 (latex-environment (anki-editor--translate-latex-env code))))
    (if anki-editor-break-consecutive-braces-in-latex
        (replace-regexp-in-string "}}" "} } " code)
      code)))

(defun anki-editor--ox-html-link (oldfun link desc info)
  "When LINK is a link to local file, transcodes it to html and stores the target file to Anki, otherwise calls OLDFUN for help.
The implementation is borrowed and simplified from ox-html."
  (or (catch 'giveup
        (unless (plist-get info :anki-editor-mode)
          (throw 'giveup nil))

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
                    ;; storing file to Anki and return the modified path
                    (anki-editor-api--store-media-file (expand-file-name (url-unhex-string raw-path)))))
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

           ;; Audio file.
           ((string-suffix-p ".mp3" path t)
            (format "[sound:%s]" path))

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

(defun anki-editor--export-string (src fmt)
  (cl-ecase fmt
    ('nil src)
    ('t (or (org-export-string-as src
                                  anki-editor--ox-anki-html-backend
                                  t
                                  anki-editor--ox-export-ext-plist)
            ;; 8.2.10 version of
            ;; `org-export-filter-apply-functions'
            ;; returns nil for an input of empty string,
            ;; which will cause AnkiConnect to fail
            ""))))


;;; Core primitives

(defconst anki-editor-prop-note-type "ANKI_NOTE_TYPE")
(defconst anki-editor-prop-note-id "ANKI_NOTE_ID")
(defconst anki-editor-prop-format "ANKI_FORMAT")
(defconst anki-editor-prop-deck "ANKI_DECK")
(defconst anki-editor-prop-tags "ANKI_TAGS")
(defconst anki-editor-prop-tags-plus (concat anki-editor-prop-tags "+"))
(defconst anki-editor-prop-failure-reason "ANKI_FAILURE_REASON")
(defconst anki-editor-org-tag-regexp "^\\([[:alnum:]_@#%]+\\)+$")

(cl-defstruct anki-editor-note
  id model deck fields tags)

(defvar anki-editor--collection-data-updated nil
  "Whether or not collection data is updated from Anki. Used by `anki-editor--with-collection-data-updated' to avoid unnecessary updates.")

;; The following variables should only be used inside `anki-editor--with-collection-data-updated'.

(defvar anki-editor--model-names nil
  "Note types from Anki.")

(defvar anki-editor--model-fields nil
  "Alist of (NOTE-TYPE . FIELDS).")

(defmacro anki-editor--with-collection-data-updated (&rest body)
  "Execute BODY with collection data updated from Anki.

Note that since we have no idea of whether BODY will update collection
data, BODY might read out-dated data.  This doesn't matter right now
as note types won't change in BODY."
  (declare (indent defun) (debug t))
  `(if anki-editor--collection-data-updated
       (progn ,@body)
     (cl-destructuring-bind (models)
         (anki-editor-api-with-multi
          (anki-editor-api-enqueue 'modelNames))
       (unwind-protect
           (progn
             (setq anki-editor--collection-data-updated t
                   anki-editor--model-names models
                   anki-editor--model-fields
                   (cl-loop for flds in (eval `(anki-editor-api-with-multi
                                                ,@(cl-loop for mod in models
                                                           collect `(anki-editor-api-enqueue 'modelFieldNames :modelName ,mod))))
                            for mod in models
                            collect (cons mod flds)))
             ,@body)
         (setq anki-editor--collection-data-updated nil)))))

(defun anki-editor-map-note-entries (func &optional match scope &rest skip)
  "Simple wrapper that calls `org-map-entries' with
  `&ANKI_NOTE_TYPE<>\"\"' appended to MATCH."
  ;; disable property inheritance temporarily, or all subheadings of a
  ;; note heading will be counted as note headings as well
  (let ((org-use-property-inheritance nil))
    (org-map-entries func (concat match "&" anki-editor-prop-note-type "<>\"\"") scope skip)))

(defun anki-editor--insert-note-skeleton (prefix deck heading type fields)
  "Insert a note subtree (skeleton) with HEADING, TYPE and FIELDS.
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
  (org-set-property anki-editor-prop-note-type type)
  (dolist (field fields)
    (save-excursion
      (org-insert-heading-respect-content)
      (org-do-demote)
      (insert field))))

(defun anki-editor--push-note (note)
  "Request AnkiConnect for updating or creating NOTE."
  (cond
   ((null (anki-editor-note-id note))
    (anki-editor--create-note note))
   (t
    (anki-editor--update-note note))))

(defun anki-editor--set-note-id (id)
  (unless id
    (error "Note creation failed for unknown reason"))
  (org-set-property anki-editor-prop-note-id (number-to-string id)))

(defun anki-editor--create-note (note)
  "Request AnkiConnect for creating NOTE."
  (thread-last
      (anki-editor-api-with-multi
       (anki-editor-api-enqueue 'createDeck
                                :deck (anki-editor-note-deck note))
       (anki-editor-api-enqueue 'addNote
                                :note (anki-editor-api--note note)))
    (nth 1)
    (anki-editor--set-note-id)))

(defun anki-editor--update-note (note)
  "Request AnkiConnect for updating fields and tags of NOTE."
  (let* ((oldnote (caar (anki-editor-api-with-multi
                         (anki-editor-api-enqueue 'notesInfo
                                                  :notes (list (anki-editor-note-id note)))
                         (anki-editor-api-enqueue 'updateNoteFields
                                                  :note (anki-editor-api--note note)))))
         (tagsadd (cl-set-difference (anki-editor-note-tags note)
                                     (alist-get 'tags oldnote)
                                     :test 'string=))
         (tagsdel (thread-first (alist-get 'tags oldnote)
                    (cl-set-difference (anki-editor-note-tags note) :test 'string=)
                    (cl-set-difference anki-editor-protected-tags :test 'string=))))
    (anki-editor-api-with-multi
     (when tagsadd
       (anki-editor-api-enqueue 'addTags
                                :notes (list (anki-editor-note-id note))
                                :tags (mapconcat #'identity tagsadd " ")))
     (when tagsdel
       (anki-editor-api-enqueue 'removeTags
                                :notes (list (anki-editor-note-id note))
                                :tags (mapconcat #'identity tagsdel " "))))))

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
    ((pred (string= anki-editor-prop-format)) (list "t" "nil"))
    ((pred (string-match-p (format "%s\\+?" anki-editor-prop-tags))) (anki-editor-all-tags))
    (_ nil)))

(defun anki-editor-is-valid-org-tag (tag)
  "Check if string TAG can be used as an Org tag."
  (string-match-p anki-editor-org-tag-regexp tag))

(defun anki-editor-all-tags ()
  "Get all tags from Anki."
  (anki-editor-api-call-result 'getTags))

(defun anki-editor-deck-names ()
  "Get all decks names from Anki."
  (anki-editor-api-call-result 'deckNames))

(defun anki-editor--enable-tag-completion ()
  (and anki-editor-mode anki-editor-org-tags-as-anki-tags))

(defun anki-editor--before-set-tags (&optional _ just-align)
  "Fetch and cache tags from Anki."
  (when (and (anki-editor--enable-tag-completion)
             (not just-align))
    (setq anki-editor--anki-tags-cache (anki-editor-all-tags))
    (when (cl-notevery #'anki-editor-is-valid-org-tag anki-editor--anki-tags-cache)
      (warn "Some tags from Anki contain characters that are not valid in Org tags."))))

(defun anki-editor--get-buffer-tags (oldfun)
  "Append tags from Anki to the result of applying OLDFUN."
  (append (funcall oldfun)
          (when (anki-editor--enable-tag-completion)
            (mapcar #'list anki-editor--anki-tags-cache))))

(defun anki-editor-note-types ()
  "Get note types from Anki."
  (anki-editor-api-call-result 'modelNames))

(defun anki-editor-entry-format ()
  (read (or (org-entry-get-with-inheritance anki-editor-prop-format t) "t")))

(defun anki-editor-toggle-format ()
  "Cycle ANKI_FROMAT through \"nil\" and \"t\"."
  (interactive)
  (let ((val (pcase (org-entry-get nil anki-editor-prop-format nil t)
               ('nil "nil")
               ("nil" "t")
               ("t" nil)
               (_ "nil"))))
    (if val
        (org-entry-put nil anki-editor-prop-format val)
      (org-entry-delete nil anki-editor-prop-format))))

(defun anki-editor-note-at-point ()
  "Make a note struct from current entry."
  (let ((org-trust-scanner-tags t)
        (deck (org-entry-get-with-inheritance anki-editor-prop-deck))
        (format (anki-editor-entry-format))
        (note-id (org-entry-get nil anki-editor-prop-note-id))
        (note-type (org-entry-get nil anki-editor-prop-note-type))
        (tags (cl-set-difference (anki-editor--get-tags)
                                 anki-editor-ignored-org-tags
                                 :test #'string=))
        (fields (anki-editor--build-fields)))

    (anki-editor--with-collection-data-updated
      (when-let ((missing (cl-set-difference
                           (alist-get note-type anki-editor--model-fields nil nil #'string=)
                           (mapcar #'car fields)
                           :test #'string=)))
        ;; use heading as the missing field
        (push (cons (car missing)
                    (anki-editor--export-string
                     (substring-no-properties (org-get-heading t t t))
                     format))
              fields)))

    (unless deck (error "Missing deck"))
    (unless note-type (error "Missing note type"))

    (make-anki-editor-note :id note-id
                           :model note-type
                           :deck deck
                           :tags tags
                           :fields fields)))

(defun anki-editor--get-tags ()
  (let ((tags (anki-editor--entry-get-multivalued-property-with-inheritance
               nil
               anki-editor-prop-tags)))
    (if anki-editor-org-tags-as-anki-tags
        (append tags (org-get-tags))
      tags)))

(defun anki-editor--entry-get-multivalued-property-with-inheritance (pom property)
  "Return a list of values in a multivalued property with inheritance."
  (let* ((value (org-entry-get pom property t))
	     (values (and value (split-string value))))
    (mapcar #'org-entry-restore-space values)))

(defun anki-editor--build-fields ()
  "Build a list of fields from subheadings of current heading.

Return a list of cons of (FIELD-NAME . FIELD-CONTENT)."
  (save-excursion
    (cl-loop with inhibit-message = t ; suppress echo message from `org-babel-exp-src-block'
             initially (unless (org-goto-first-child)
                         (cl-return))
             for last-pt = (point)
             for element = (org-element-at-point)
             for heading = (substring-no-properties
                            (org-element-property :raw-value element))
             for format = (anki-editor-entry-format)
             ;; contents-begin includes drawers and scheduling data,
             ;; which we'd like to ignore, here we skip these
             ;; elements and reset contents-begin.
             for begin = (cl-loop for eoh = (org-element-property :contents-begin element)
                                  then (org-element-property :end subelem)
                                  for subelem = (progn
                                                  (goto-char eoh)
                                                  (org-element-context))
                                  while (memq (org-element-type subelem)
                                              '(drawer planning property-drawer))
                                  finally return (org-element-property :begin subelem))
             for end = (org-element-property :contents-end element)
             for raw = (or (and begin
                                end
                                (buffer-substring-no-properties
                                 begin
                                 ;; in case the buffer is narrowed,
                                 ;; e.g. by `org-map-entries' when
                                 ;; scope is `tree'
                                 (min (point-max) end)))
                           "")
             for content = (anki-editor--export-string raw format)
             collect (cons heading content)
             ;; proceed to next field entry and check last-pt to
             ;; see if it's already the last entry
             do (org-forward-heading-same-level nil t)
             until (= last-pt (point)))))


;;; Minor mode

(defvar-local anki-editor--anki-tags-cache nil)

(defun anki-editor--concat-multivalued-property-value (prop value)
  (let ((old-values (org-entry-get-multivalued-property nil prop)))
    (unless (string-suffix-p prop "+")
      (setq old-values (cl-set-difference old-values
                                          (org-entry-get-multivalued-property
                                           nil (concat prop "+"))
                                          :test 'string=)))
    (mapconcat #'org-entry-protect-space
               (append old-values (list value))
               " ")))

(setq org-properties-postprocess-alist
      (append org-properties-postprocess-alist
              (list (cons anki-editor-prop-tags
                          (lambda (value)
                            (anki-editor--concat-multivalued-property-value anki-editor-prop-tags value)))
                    (cons anki-editor-prop-tags-plus
                          (lambda (value)
                            (anki-editor--concat-multivalued-property-value anki-editor-prop-tags-plus value))))))

;;;###autoload
(define-minor-mode anki-editor-mode
  "anki-editor-mode"
  :lighter " anki-editor"
  (if anki-editor-mode (anki-editor-setup-minor-mode)
    (anki-editor-teardown-minor-mode)))

(defun anki-editor-setup-minor-mode ()
  "Set up this minor mode."
  (anki-editor-api-check)
  (add-hook 'org-property-allowed-value-functions #'anki-editor--get-allowed-values-for-property nil t)
  (advice-add 'org-set-tags :before #'anki-editor--before-set-tags)
  (advice-add 'org-get-buffer-tags :around #'anki-editor--get-buffer-tags)
  (advice-add 'org-html-link :around #'anki-editor--ox-html-link))

(defun anki-editor-teardown-minor-mode ()
  "Tear down this minor mode."
  (remove-hook 'org-property-allowed-value-functions #'anki-editor--get-allowed-values-for-property t))


;;; Commands

(defvar anki-editor--note-markers nil)

(defun anki-editor--collect-note-marker ()
  (message "Scanning notes %d (%s@%d), wait a moment..."
           (length anki-editor--note-markers) (buffer-name) (point))
  (push (point-marker) anki-editor--note-markers))

(defun anki-editor-push-notes (&optional scope match)
  "Build notes from headings that match MATCH within SCOPE and push them to Anki.

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
  (interactive (list (cond
                      ((region-active-p) 'region)
                      ((equal current-prefix-arg '(4)) 'tree)
                      ((equal current-prefix-arg '(16)) 'file)
                      ((equal current-prefix-arg '(64)) 'agenda)
                      (t nil))))
  (unwind-protect
      (progn
        (anki-editor-map-note-entries #'anki-editor--collect-note-marker match scope)
        (setq anki-editor--note-markers (reverse anki-editor--note-markers))
        (let ((count 0)
              (failed 0))
          (save-excursion
            (anki-editor--with-collection-data-updated
              (cl-loop with bar-width = 30
                       for marker in anki-editor--note-markers
                       for progress = (/ (float (cl-incf count)) (length anki-editor--note-markers))
                       do
                       (goto-char marker)
                       (message "Uploading notes in buffer %s%s [%s%s] %d/%d (%.2f%%)"
                                (marker-buffer marker)
                                (if (zerop failed)
                                    ""
                                  (propertize (format " %d failed" failed)
                                              'face `(:foreground "red")))
                                (make-string (truncate (* bar-width progress)) ?#)
                                (make-string (- bar-width (truncate (* bar-width progress))) ?.)
                                count
                                (length anki-editor--note-markers)
                                (* 100 progress))
                       (anki-editor--clear-failure-reason)
                       (condition-case-unless-debug err
                           (anki-editor--push-note (anki-editor-note-at-point))
                         (error (cl-incf failed)
                                (anki-editor--set-failure-reason (error-message-string err))))
                       ;; free marker
                       (set-marker marker nil))))
          (message
           (cond
            ((zerop (length anki-editor--note-markers)) "Nothing to push")
            ((zerop failed) (format "Successfully pushed %d notes to Anki" count))
            (t (format "Pushed %d notes to Anki, with %d failed.  Check property drawers for details.
When you have fixed those issues, try re-push the failed ones with `anki-editor-retry-failed-notes'."
                       count failed))))))
    ;; clean up markers
    (cl-loop for m in anki-editor--note-markers
             do (set-marker m nil)
             finally do (setq anki-editor--note-markers nil))))

(defun anki-editor-push-new-notes (&optional scope)
  "Push note entries without ANKI_NOTE_ID in SCOPE to Anki."
  (interactive)
  (anki-editor-push-notes scope (concat anki-editor-prop-note-id "=\"\"")))

(defun anki-editor-retry-failed-notes (&optional scope)
  "Retry pushing notes marked as failed.
This command just calls `anki-editor-submit' with match string
matching non-empty `ANKI_FAILURE_REASON' properties."
  (interactive)
  (anki-editor-push-notes scope (concat anki-editor-prop-failure-reason "<>\"\"")))

(defun anki-editor-delete-notes (noteids)
  "Delete notes in NOTEIDS or the note at point."
  (interactive (list (list (org-entry-get nil anki-editor-prop-note-id))))
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p (format "Do you really want to delete note %s? The deletion can't be undone. " (nth 0 noteids))))
    (anki-editor-api-call-result 'deleteNotes
                                 :notes noteids)
    (org-entry-delete nil anki-editor-prop-note-id)
    (when (called-interactively-p 'interactive)
      (message "Deleted note %s" (nth 0 noteids)))))

(defun anki-editor-insert-note (&optional prefix)
  "Insert a note interactively.

Where the note subtree is placed depends on PREFIX, which is the
same as how it is used by `M-RET'(org-insert-heading).

When note heading is not provided, it is used as the first field."
  (interactive "P")
  (let* ((deck (or (org-entry-get-with-inheritance anki-editor-prop-deck)
                   (completing-read "Deck: " (sort (anki-editor-deck-names) #'string-lessp))))
         (type (completing-read "Note type: " (sort (anki-editor-note-types) #'string-lessp)))
         (fields (anki-editor-api-call-result 'modelFieldNames :modelName type))
         (heading (read-from-minibuffer "Note heading (optional): ")))
    (anki-editor--insert-note-skeleton prefix
                                       deck
                                       heading
                                       type
                                       (if (string-blank-p heading)
                                           (cdr fields)
                                         fields))))

(defun anki-editor-cloze-region (&optional arg hint)
  "Cloze region with number ARG."
  (interactive "p\nsHint (optional): ")
  (unless (region-active-p) (error "No active region"))
  (anki-editor-cloze (region-beginning) (region-end) arg hint))

(defun anki-editor-cloze-dwim (&optional arg hint)
  "Cloze current active region or a word the under the cursor"
  (interactive "p\nsHint (optional): ")
  (cond
   ((region-active-p) (anki-editor-cloze (region-beginning) (region-end) arg hint))
   ((thing-at-point 'word) (let ((bounds (bounds-of-thing-at-point 'word)))
                             (anki-editor-cloze (car bounds) (cdr bounds) arg hint)))
   (t (error "Nothing to create cloze from"))))

(defun anki-editor-cloze (begin end arg hint)
  "Cloze region from BEGIN to END with number ARG."
  (let ((region (buffer-substring begin end)))
    (save-excursion
      (delete-region begin end)
      (insert (with-output-to-string
                (princ (format "{{c%d::%s" (or arg 1) region))
                (unless (string-blank-p hint) (princ (format "::%s" hint)))
                (princ "}}"))))))

(defun anki-editor-export-subtree-to-html ()
  "Export subtree of the element at point to HTML."
  (interactive)
  (org-export-to-buffer
      anki-editor--ox-anki-html-backend
      "*AnkiEditor HTML Output*" nil t nil t anki-editor--ox-export-ext-plist #'html-mode))

(defun anki-editor-convert-region-to-html ()
  "Convert and replace region to HTML."
  (interactive)
  (org-export-replace-region-by anki-editor--ox-anki-html-backend))


;;; More utilities

(defun anki-editor-api-check ()
  "Check if correct version of AnkiConnect is serving."
  (interactive)
  (let ((ver (condition-case err
                 (anki-editor-api-call-result 'version)
               (error (error "Failed to connect to Anki: %s" (error-message-string err))))))
    (if (<= anki-editor-api-version ver)
        (when (called-interactively-p 'interactive)
          (message "AnkiConnect v.%d is running" ver))
      (error "anki-editor requires minimal version %d of AnkiConnect installed"
             anki-editor-api-version))))

(defun anki-editor-api-upgrade ()
  "Upgrade AnkiConnect to the latest version.

This will display a confirmation dialog box in Anki asking if you
want to continue.  The upgrading is done by downloading the latest
code in the master branch of its Github repo.

This is useful when new version of this package depends on the
bugfixes or new features of AnkiConnect."
  (interactive)
  (when (yes-or-no-p "This is going to download the latest AnkiConnect from the Internet to your computer, do you want to continue? ")
    (let ((result (anki-editor-api-call-result 'upgrade)))
      (when (and (booleanp result) result)
        (message "AnkiConnect has been upgraded, you might have to restart Anki for the changes to take effect.")))))

(defun anki-editor-sync-collections ()
  "Synchronizes the local anki collections with ankiweb."
  (interactive)
  (anki-editor-api-call-result 'sync))

(defun anki-editor-gui-browse (&optional query)
  "Open Anki Browser with QUERY.
When called interactively, it will try to set QUERY to current
note or deck."
  (interactive (list (pcase (org-entry-get-with-inheritance anki-editor-prop-note-id)
                       ((and (pred stringp) nid) (format "nid:%s" nid))
                       (_ (format "deck:%s"
                                  (or (org-entry-get-with-inheritance anki-editor-prop-deck)
                                      "current"))))))
  (anki-editor-api-call 'guiBrowse :query (or query "")))

(defun anki-editor-gui-add-cards ()
  "Open Anki Add Cards dialog with presets from current note
entry."
  (interactive)
  (anki-editor-api-call-result 'guiAddCards
                               :note (append
                                      (anki-editor-api--note
                                       (anki-editor-note-at-point))
                                      (list :options '(:closeAfterAdding t)))))

(defun anki-editor-find-notes (&optional query)
  "Find notes with QUERY."
  (interactive "sQuery: ")
  (let ((nids (anki-editor-api-call-result 'findNotes
                                           :query (or query ""))))
    (if (called-interactively-p 'interactive)
        (message "%S" nids)
      nids)))

(defvar anki-editor--style-start "</style>\n<!-- {{ Emacs Org-mode -->")
(defvar anki-editor--style-end "<!-- Emacs Org-mode }} -->\n<style>")

(defun anki-editor-copy-styles ()
  "Copy `org-html-style-default' and `anki-editor-html-head' to Anki card stylings."
  (interactive)
  (let ((head (concat (org-element-normalize-string anki-editor--style-start)
                      (org-element-normalize-string (format "<!-- Updated: %s -->" (current-time-string)))
                      (when anki-editor-include-default-style
                        (org-element-normalize-string org-html-style-default))
                      (org-element-normalize-string anki-editor-html-head)
                      anki-editor--style-end)))
    (cl-loop for model in (anki-editor-note-types)
             for style = (let* ((css (alist-get 'css (anki-editor-api-call-result 'modelStyling :modelName model)))
                                (start (string-match
                                        (regexp-quote anki-editor--style-start)
                                        css))
                                (end (string-match
                                      (regexp-quote anki-editor--style-end)
                                      css)))
                           (if (and start end)
                               (progn
                                 (cl-incf end (length anki-editor--style-end))
                                 ;; skip whitespaces
                                 (when-let ((newend (string-match "[[:graph:]]" css end)))
                                   (setq end newend))
                                 (concat
                                  (substring css 0 start)
                                  (substring css end)))
                             css))
             do
             (message "Updating styles for \"%s\"..." model)
             (anki-editor-api-call-result 'updateModelStyling
                                          :model (list :name model
                                                       :css (concat (concat head "\n\n") style)))
             finally do (message "Updating styles...Done"))))

(defun anki-editor-remove-styles ()
  "Remove from card stylings html tags generated by this mode."
  (interactive)
  (cl-loop for model in (anki-editor-note-types)
           for css = (alist-get 'css (anki-editor-api-call-result 'modelStyling :modelName model))
           for start = (string-match
                        (regexp-quote anki-editor--style-start)
                        css)
           for end = (string-match
                      (regexp-quote anki-editor--style-end)
                      css)
           if (and start end)
           do
           (cl-incf end (length anki-editor--style-end))
           ;; also remove whitespaces
           (when-let ((newend (string-match "[[:graph:]]" css end)))
             (setq end newend))
           (message "Resetting styles for \"%s\"..." model)
           (anki-editor-api-call-result
            'updateModelStyling
            :model (list :name model
                         :css (concat
                               (substring css 0 start)
                               (substring css end))))
           finally do (message "Resetting styles...Done")))


(provide 'anki-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor.el ends here
