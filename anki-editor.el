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
(require 'seq)

(defgroup anki-editor nil
  "Customizations for anki-editor."
  :group 'org)

(defcustom anki-editor-break-consecutive-braces-in-latex
  nil
  "If non-nil, consecutive `}' will be automatically separated by spaces to prevent early-closing of cloze.
See https://apps.ankiweb.net/docs/manual.html#latex-conflicts.")

(defcustom anki-editor-org-tags-as-anki-tags
  t
  "If nil, tags of entries wont't be counted as Anki tags.")

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
  "The network address AnkiConnect is listening.")

(defcustom anki-editor-api-port
  "8765"
  "The port number AnkiConnect is listening.")

(defcustom anki-editor-use-math-jax nil
  "Use Anki's built in MathJax support instead of LaTeX.")


(defcustom anki-editor-note-match nil
  "Additional matching string for mapping through anki note headings.")

;;; AnkiConnect

(defconst anki-editor-api-version 5)

(cl-defun anki-editor--http-send (url
                                  &rest settings
                                  &key type data success error parser
                                  &allow-other-keys)
  "This is a simplistic little function to make http requests using cURL.
The api is borrowed from request.el.  It exists because
request.el's sync mode calls cURL asynchronously under the hood,
which doesn't work on some machiens (like mine) where the process
sentinel never gets called.  After some debugging of Emacs, it
seems that in 'process.c' the pselect syscall to the file
descriptor of inotify used by 'autorevert' always returns a
nonzero value and causes 'status_notify' never being called.  To
determine whether it's a bug in Emacs and make a patch requires
more digging."
  (let ((tempfile (make-temp-file "emacs-anki-editor"))
        (responsebuf (generate-new-buffer " *anki-editor-curl*")))
    (with-temp-file tempfile
      (setq buffer-file-coding-system 'utf-8)
      (set-buffer-multibyte t)
      (insert data))
    (unwind-protect
        (with-current-buffer responsebuf
          (apply #'call-process "curl" nil t nil (list
                                                  url
                                                  "--silent"
                                                  "-X" type
                                                  "--data-binary"
                                                  (concat "@" tempfile)))

          (goto-char (point-min))
          (apply success (list :data (funcall parser))))
      (kill-buffer responsebuf)
      (delete-file tempfile))))

(defun anki-editor-api-call (action &rest params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((payload (list :action action :version anki-editor-api-version))
        (request-backend 'curl)
        (json-array-type 'list)
        reply err)
    (when params (setq payload (plist-put payload :params params)))
    (anki-editor--http-send (format "http://%s:%s"
                                    anki-editor-api-host
                                    anki-editor-api-port)
                            :type "POST"
                            :parser 'json-read
                            :data (json-encode payload)
                            :success (cl-function (lambda (&key data &allow-other-keys)
                                                    (setq reply data)))
                            :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
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
  "Used in combination with `anki-editor-api-enqueue' to queue
multiple api calls and combine them into one 'multi' call at the
end, return the results of these calls in the same order."
  `(let (--anki-editor-multi-var-actions--
         --anki-editor-multi-var-results--)
     ,@body
     (setq --anki-editor-multi-var-results--
           (anki-editor-api-call-result
            'multi
            :actions (nreverse
                      ;; Here we make a vector from the action list,
                      ;; or `json-encode' will consider it as an association list.
                      (vconcat
                       --anki-editor-multi-var-actions--))))
     (cl-loop for result in --anki-editor-multi-var-results--
              do (when-let ((_ (listp result))
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
     (push action --anki-editor-multi-var-actions--)))

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
  '(:with-toc nil :anki-editor-mode t))

(macrolet ((with-table (table)
                       `(cl-loop for delims in ,table
                                 collect
                                 (list (concat "^" (regexp-quote (car delims)))
                                       (cadr delims)
                                       (concat (regexp-quote (caddr delims)) "$")
                                       (cadddr delims)))))

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

(defmacro anki-editor--enclose-with (tag &rest children)
  (declare (indent defun))
  `(format "<%s>%s</%s>"
           ,(symbol-name tag)
           (concat ,@children)
           ,(symbol-name tag)))

(defun anki-editor--wrap-latex (content)
  "Wrap CONTENT with Anki-style latex markers."
  (anki-editor--enclose-with p
    (anki-editor--enclose-with div "[latex]")
    content
    (anki-editor--enclose-with div "[/latex]")))

(defun anki-editor--wrap-div (content)
  (anki-editor--enclose-with div content))

(defun anki-editor--translate-latex-fragment (latex-code)
  (let ((table (if anki-editor-use-math-jax
                   anki-editor--mathjax-delimiters
                 anki-editor--native-latex-delimiters)))
    (cl-loop for delims in table
             with matched = nil
             when (setq matched (string-match (car delims) latex-code))
             do
             (setq latex-code (replace-match (cadr delims) t t latex-code))
             (string-match (caddr delims) latex-code)
             (setq latex-code (replace-match (cadddr delims) t t latex-code))
             until matched
             finally return latex-code)))

(defun anki-editor--translate-latex-env (latex-code)
  (let ((latex-block (mapconcat
                      #'anki-editor--wrap-div
                      (split-string (org-html-encode-plain-text latex-code)
                                    "\n")
                      "")))
    (if anki-editor-use-math-jax
        (anki-editor--enclose-with p
          latex-block)
      (anki-editor--wrap-latex latex-block))))

(defun anki-editor--ox-latex (latex _contents _info)
  "Transcode LATEX from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((code (org-remove-indentation (org-element-property :value latex))))
    (setq code
          (pcase (org-element-type latex)
            ('latex-fragment (anki-editor--translate-latex-fragment code))
            ('latex-environment (anki-editor--translate-latex-env code))))
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


;;; Core primitives

(defconst anki-editor-prop-note-type "ANKI_NOTE_TYPE")
(defconst anki-editor-prop-note-id "ANKI_NOTE_ID")
(defconst anki-editor-prop-exporter "ANKI_EXPORTER")
(defconst anki-editor-prop-deck "ANKI_DECK")
(defconst anki-editor-prop-tags "ANKI_TAGS")
(defconst anki-editor-prop-tags-plus (concat anki-editor-prop-tags "+"))
(defconst anki-editor-prop-failure-reason "ANKI_FAILURE_REASON")
(defconst anki-editor-org-tag-regexp "^\\([[:alnum:]_@#%]+\\)+$")
(defconst anki-editor-exporter-raw "raw")
(defconst anki-editor-exporter-default "default")

(cl-defstruct anki-editor-note
  id model deck fields tags)

(defun anki-editor-map-note-entries (func &optional match scope &rest skip)
  "Simple wrapper that calls `org-map-entries' with
  `&ANKI_NOTE_TYPE<>\"\"' appended to MATCH."
  ;; disable property inheritance temporarily, or all subheadings of a
  ;; note heading will be counted as note headings as well
  (let ((org-use-property-inheritance nil)
	(match (if anki-editor-note-match
		   (concat match "&" anki-editor-note-match "&" anki-editor-prop-note-type "<>\"\"")
		 (concat match "&" anki-editor-prop-note-type "<>\"\""))))
    (org-map-entries func match scope skip)))

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
  (let* ((oldnote (thread-first
                      (anki-editor-api-with-multi
                       (anki-editor-api-enqueue 'notesInfo
                                                :notes (list (anki-editor-note-id note)))
                       (anki-editor-api-enqueue 'updateNoteFields
                                                :note (anki-editor-api--note note)))
                    (caar)))
         (tags-add (cl-set-difference (anki-editor-note-tags note)
                                      (alist-get 'tags oldnote)
                                      :test 'string=))
         (tags-del (thread-first (alist-get 'tags oldnote)
                     (cl-set-difference (anki-editor-note-tags note) :test 'string=)
                     (cl-set-difference anki-editor-protected-tags :test 'string=))))
    (anki-editor-api-with-multi
     (when tags-add
       (anki-editor-api-enqueue 'addTags
                                :notes (list (anki-editor-note-id note))
                                :tags (mapconcat #'identity tags-add " ")))
     (when tags-del
       (anki-editor-api-enqueue 'removeTags
                                :notes (list (anki-editor-note-id note))
                                :tags (mapconcat #'identity tags-del " "))))))

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
    ((pred (string= anki-editor-prop-exporter)) (list anki-editor-exporter-raw anki-editor-exporter-default))
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
    (unless (seq-every-p #'anki-editor-is-valid-org-tag anki-editor--anki-tags-cache)
      (warn "Some tags from Anki contain characters that are not valid in Org tags."))))

(defun anki-editor--get-buffer-tags (oldfun)
  "Append tags from Anki to the result of applying OLDFUN."
  (append (funcall oldfun)
          (when (anki-editor--enable-tag-completion)
            (mapcar #'list anki-editor--anki-tags-cache))))

(defun anki-editor-note-types ()
  "Get note types from Anki."
  (anki-editor-api-call-result 'modelNames))

(defun anki-editor-note-at-point ()
  "Make a note struct from current entry."
  (let ((org-trust-scanner-tags t)
        (deck (org-entry-get-with-inheritance anki-editor-prop-deck))
        (note-id (org-entry-get nil anki-editor-prop-note-id))
        (note-type (org-entry-get nil anki-editor-prop-note-type))
        (tags (cl-set-difference (anki-editor--get-tags)
                                 anki-editor-ignored-org-tags
                                 :test 'string=))
        (fields (anki-editor--build-fields)))

    (unless deck (error "No deck specified"))
    (unless note-type (error "Missing note type"))
    (unless fields (error "Missing fields"))

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
  "Build a list of fields from subheadings of current heading,
each element of which is a cons cell, the car of which is field
name and the cdr of which is field content."
  (save-excursion
    (let (fields
          (point-of-last-child (point)))
      (when (org-goto-first-child)
        (while (/= point-of-last-child (point))
          (setq point-of-last-child (point))
          (let* ((inhibit-message t)  ;; suppress echo message from `org-babel-exp-src-block'
                 (field-heading (org-element-at-point))
                 (field-name (substring-no-properties
                              (org-element-property
                               :raw-value
                               field-heading)))
                 (contents-begin (org-element-property :contents-begin field-heading))
                 (contents-end (org-element-property :contents-end field-heading))
                 (exporter (or (org-entry-get-with-inheritance anki-editor-prop-exporter)
                               anki-editor-exporter-default))
                 (end-of-header (org-element-property :contents-begin field-heading))
                 raw-content
                 content-elem)
            (when (string= exporter anki-editor-exporter-raw)
              ;; contents-begin includes drawers and scheduling data,
              ;; which we'd like to ignore, here we skip these
              ;; elements and reset contents-begin.
              (while (progn
                       (goto-char end-of-header)
                       (setq content-elem (org-element-context))
                       (memq (car content-elem) '(drawer planning property-drawer)))
                (setq end-of-header (org-element-property :end content-elem)))
              (setq contents-begin (org-element-property :begin content-elem)))
            (setq raw-content (or (and contents-begin
                                       contents-end
                                       (buffer-substring
                                        contents-begin
                                        ;; in case the buffer is narrowed,
                                        ;; e.g. by `org-map-entries' when
                                        ;; scope is `tree'
                                        (min (point-max) contents-end)))
                                  ""))
            (push (cons field-name
                        (pcase exporter
                          ((pred (string= anki-editor-exporter-raw))
                           raw-content)
                          ((pred (string= anki-editor-exporter-default))
                           (or (org-export-string-as
                                raw-content
                                anki-editor--ox-anki-html-backend
                                t
                                anki-editor--ox-export-ext-plist)
                               ;; 8.2.10 version of
                               ;; `org-export-filter-apply-functions'
                               ;; returns nil for an input of empty string,
                               ;; which will cause AnkiConnect to fail
                               ""))
                          (_ (error "Invalid exporter: %s" exporter))))
                  fields)
            (org-forward-heading-same-level nil t))))
      (reverse fields))))


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

  (let ((total (progn
                 (message "Counting notes...")
                 (length (anki-editor-map-note-entries t match scope))))
        (acc 0)
        (failed 0))
    (anki-editor-map-note-entries
     (lambda ()
       (message "[%d/%d] Processing notes in buffer \"%s\", wait a moment..."
                (cl-incf acc) total (buffer-name))
       (anki-editor--clear-failure-reason)
       (condition-case-unless-debug err
           (anki-editor--push-note (anki-editor-note-at-point))
         (error (cl-incf failed)
                (anki-editor--set-failure-reason (error-message-string err)))))
     match scope)

    (message
     (cond
      ((zerop total) "Nothing to push")
      ((zerop failed) (format "Pushed %d notes to Anki successfully" acc))
      (t (format "Pushed %d notes in total, among which %d were failed.  Check property drawers for failure reasons.
When the issues are resolved, you could repush the failed ones with `anki-editor-retry-failed-notes'."
                 acc failed))))))

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
                   (anki-editor-api-call-result 'modelFieldNames
                                                :modelName note-type)))
         (note-heading (read-from-minibuffer "Enter the note heading (optional): ")))

    (anki-editor--insert-note-skeleton prefix
                                       deck
                                       (if (string-blank-p note-heading)
                                           "Item"
                                         note-heading)
                                       note-type
                                       fields)))

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
  (let ((ver (anki-editor-api-call-result 'version)))
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

(provide 'anki-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anki-editor.el ends here
