(require 'seq)
(require 'anki-editor)

(defconst anki-editor-fix-prop-tags "ANKI_TAGS")
(defconst anki-editor-fix-prop-deck "ANKI_DECK")
(defconst anki-editor-fix-tag-note "note")
(defconst anki-editor-fix-tag-deck "deck")


(defun anki-editor-fix-notes (&optional list-of-files)
  "Fix notes in LIST-OF-FILES, or in current buffer if it's nil.
`anki-editor' has be to upgraded to `0.3' before using this
command.  It should be noted that only letters, numbers, `_' and
`@' are allowed in Org tags, and existing tags of headings will
be considered as Anki tags now."

  (interactive)
  (when (yes-or-no-p "It's strongly recommended that you backup or version-control your old note files before running this command. Are you sure to continue ?")
    (org-map-entries
     (lambda ()
       (message "Fixing in buffer \"%s\" at %d..." (buffer-name) (point))
       (let* ((local-tags (org-get-local-tags))
              (old-anki-tags (org-entry-get-multivalued-property nil anki-editor-fix-prop-tags))
              (new-tags (cl-set-difference (append old-anki-tags (org-get-local-tags))
                                           (list anki-editor-fix-tag-deck anki-editor-fix-tag-note)
                                           :test #'string=)))

         (unless (seq-every-p #'anki-editor-is-valid-org-tag new-tags)
           (error "Fixing failed in buffer \"%s\" at point %d: only letters, numbers, `_', `@', `#' and `%%' are allowed in an Org tag"
                  (buffer-name)
                  (point)))
         (org-set-tags-to new-tags)
         (beginning-of-line)
         (org-fix-tags-on-the-fly)
         (org-entry-delete nil anki-editor-fix-prop-tags)
         (cond
          ((member anki-editor-fix-tag-deck local-tags)
           (anki-editor-fix-deck)))))
     nil
     list-of-files)
    (message "Fixing... DONE!")))

(defun anki-editor-fix-deck ()
  (interactive)
  (org-set-property anki-editor-fix-prop-deck (nth 4 (org-heading-components))))
