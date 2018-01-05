;;; anki-editor.el --- Create Anki cards in Org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Louie Tan

;; Author: Louie Tan <louietanlei@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distaributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'json)
(require 'org-element)


(defconst anki-editor-note-tag "note")
(defconst anki-editor-deck-tag "deck")
(defconst anki-editor-note-type-prop :ANKI_NOTE_TYPE)
(defconst anki-editor-note-tags-prop :ANKI_TAGS)
(defconst anki-editor-anki-connect-listening-address "127.0.0.1")
(defconst anki-editor-anki-connect-listening-port "8765")

;; Commands

(defun anki-editor-submit ()
  (interactive)
  (let* ((tree (org-element-parse-buffer))
         (note-headings (anki-editor--get-note-headings tree))
         (total (length note-headings)))

    (if (null note-headings)
        (message "No notes found in current buffer")

      (message "Submitting %d notes to Anki..." total)
      (anki-editor--anki-connect-invoke
       "addNotes" 5
       `(("notes" . ,(mapcar #'anki-editor--anki-connect-heading-to-note
                             note-headings)))
       (lambda (result)
         (let ((failed (seq-count #'null result)))
           (message (format "Submitted %d notes, %d successful, %d failed." total (- total failed) failed))))))))

(defun anki-editor-insert-deck ()
  (interactive)
  (message "Fetching decks...")
  (anki-editor--anki-connect-invoke
   "deckNames" 5 nil
   (lambda (result)
     (setq result (append (sort result #'string-lessp) nil))
     (insert (completing-read "Choose a deck: " result))
     (anki-editor--set-tags-fix anki-editor-deck-tag))))

(defun anki-editor-insert-note ()
  (interactive)
  (message "Fetching note types...")
  (anki-editor--anki-connect-invoke
   "modelNames" 5 nil
   (lambda (note-types)
     (let (note-type note-heading)
       (setq note-types (append (sort note-types #'string-lessp) nil)
             note-type (completing-read "Choose a note type: " note-types))
       (message "Fetching note fields...")
       (anki-editor--anki-connect-invoke
        "modelFieldNames" 5 `((modelName . ,note-type))
        (lambda (fields)
          (setq fields (append (reverse fields) nil)
                note-heading (read-from-minibuffer "Enter the heading: " "Item"))
          (org-insert-subheading nil)
          (insert note-heading)
          (anki-editor--set-tags-fix anki-editor-note-tag)
          (org-set-property (substring (symbol-name anki-editor-note-type-prop) 1) note-type)
          (org-next-visible-heading 1)
          (end-of-line 0)
          (dolist (field fields)
            (save-excursion
              (org-insert-subheading nil)
              (insert field)))
          (org-next-visible-heading 1)
          (end-of-line)
          (newline-and-indent)))))))

(setq anki-editor--key-map `((,(kbd "C-c a s") . ,#'anki-editor-submit)
                             (,(kbd "C-c a d") . ,#'anki-editor-insert-deck)
                             (,(kbd "C-c a n") . ,#'anki-editor-insert-note)))

(defun anki-editor-setup-default-keybindings ()
  (interactive)
  (dolist (map anki-editor--key-map)
    (local-set-key (car map) (cdr map)))
  (message "anki-editor default keybindings have been set"))


;; Core Functions

(defun anki-editor--get-note-headings (data &optional test)
  (unless test (setq test 'identity))
  (org-element-map data 'headline
    (lambda (element)
      (let ((tags (org-element-property :tags element)))
        (when (and (member anki-editor-note-tag tags) (funcall test element))
          element)))))

(defun anki-editor--heading-to-note (heading)
  (let (deck note-type tags fields)
    (setq deck (anki-editor--get-deck-name heading)
          note-type (org-element-property anki-editor-note-type-prop heading)
          tags (org-element-property anki-editor-note-tags-prop heading)
          fields (mapcar #'anki-editor--heading-to-note-field (anki-editor--get-subheadings heading)))

    (unless deck (error "Please specify a deck !"))
    (unless note-type (error "Please specify a note type !"))
    (unless fields (error "Please specify fields !"))

    `((deck . ,deck)
      (note-type . ,note-type)
      (tags . ,(and tags (split-string tags " ")))
      (fields . ,fields))))

(defun anki-editor--get-deck-name (element)
  (let ((ancestor (anki-editor--find-ancestor
                   element
                   (lambda (it)
                     (member anki-editor-deck-tag (org-element-property :tags it))))))
    (and ancestor
         (substring-no-properties (org-element-property :raw-value ancestor)))))

(defun anki-editor--get-subheadings (heading)
  (org-element-map (org-element-contents heading)
      'headline 'identity nil nil 'headline))

(defun anki-editor--heading-to-note-field (heading)
  (let ((field-name (substring-no-properties
                     (org-element-property
                      :raw-value
                      heading)))
        (contents (org-element-contents heading)))
    `(,field-name . ,(anki-editor--generate-html
                      (org-element-interpret-data contents)))))

(defun anki-editor--generate-html (org-content)
  (with-temp-buffer
    (insert org-content)
    (setq anki-editor--replacement-records nil)
    (anki-editor--replace-latex)
    (anki-editor--buffer-to-html)
    (anki-editor--translate-latex)
    (buffer-substring-no-properties (point-min) (point-max))))

;; Transformers

(defun anki-editor--buffer-to-html ()
  (when (> (buffer-size) 0)
    (save-mark-and-excursion
     (mark-whole-buffer)
     (org-html-convert-region-to-html))))

(defun anki-editor--replace-latex ()
  (let (object)
    (while (setq object (org-element-map
                            (org-element-parse-buffer)
                            'latex-fragment 'identity nil t))
      (let (begin end latex hash)
        (setq begin (org-element-property :begin object)
              end (- (org-element-property :end object) (org-element-property :post-blank object))
              latex (delete-and-extract-region begin end))
        (goto-char begin)
        (insert (setq hash (anki-editor--hash 'latex-fragment latex)))
        (add-to-list 'anki-editor--replacement-records
                     `(,hash . ((type . latex-fragment)
                                (original . ,latex))))))))

(setq anki-editor--anki-latex-syntax-map
      `((,(format "^%s" (regexp-quote "$$")) . "[$$]")
        (,(format "%s$" (regexp-quote "$$")) . "[/$$]")
        (,(format "^%s" (regexp-quote "$")) . "[$]")
        (,(format "%s$" (regexp-quote "$")) . "[/$]")
        (,(format "^%s" (regexp-quote "\\(")) . "[$]")
        (,(format "%s$" (regexp-quote "\\)")) . "[/$]")
        (,(format "^%s" (regexp-quote "\\[")) . "[$$]")
        (,(format "%s$" (regexp-quote "\\]")) . "[/$$]")))

(defun anki-editor--translate-latex-to-anki-syntax (latex)
  (dolist (map anki-editor--anki-latex-syntax-map)
    (setq latex (replace-regexp-in-string (car map) (cdr map) latex t t)))
  latex)

(defun anki-editor--translate-latex ()
  (dolist (stash anki-editor--replacement-records)
    (goto-char (point-min))
    (let ((hash (car stash))
          (value (cdr stash)))
      (when (eq 'latex-fragment (alist-get 'type value))
        (when (search-forward hash nil t)
          (replace-match (anki-editor--translate-latex-to-anki-syntax
                          (alist-get 'original value))
                         t t))))))

;; Utilities

(defun anki-editor--hash (type text)
  (format "%s-%s" (symbol-name type) (sha1 text)))

(defun anki-editor--find-ancestor (element test)
  (let ((parent (org-element-property :parent element)))
    (and parent
         (if (funcall test parent)
             parent
           (anki-editor--find-ancestor parent test)))))

(defun anki-editor--set-tags-fix (tags)
  (org-set-tags-to tags)
  (org-fix-tags-on-the-fly))

;; anki-connect

(defun anki-editor--anki-connect-invoke (action version &optional params success)
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

    (let* ((response (shell-command-to-string
                      (format "curl %s:%s --silent -X POST --data-binary @%s"
                              anki-editor-anki-connect-listening-address
                              anki-editor-anki-connect-listening-port
                              request-tempfile)))
           anki-error)
      (when (file-exists-p request-tempfile) (delete-file request-tempfile))
      (condition-case err
          (progn
            (setq response (json-read-from-string response)
                  anki-error (alist-get 'error response))
            (when anki-error (error "anki-connect responded with error: %s" anki-error))
            (when success (funcall success (alist-get 'result response))))
        (error (message "%s" (error-message-string err)))))))

(defun anki-editor--anki-connect-map-note (note)
  `(("deckName" . ,(alist-get 'deck note))
    ("modelName" . ,(alist-get 'note-type note))
    ("fields" . ,(alist-get 'fields note))
    ;; Convert tags to a vector since empty list is identical to nil
    ;; which will become None in Python, but anki-connect requires it
    ;; to be type of list.
    ("tags" . ,(vconcat (alist-get 'tags note)))))

(defun anki-editor--anki-connect-heading-to-note (heading)
  (anki-editor--anki-connect-map-note
   (anki-editor--heading-to-note heading)))


(provide 'anki-editor)

;;; anki-editor.el ends here
