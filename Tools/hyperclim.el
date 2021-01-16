;;; hyperclim.el --- Browse CLIM2 specification inside Emacs.

;;; Commentary:
;;;
;;; This code is in the public domain.
;;;
;;; Originally written by Andy Hefner (andy.hefner@verizon.net)

;;; Code:

(require 'cl-lib nil t)
(require 'cl-lib "lib/cl-lib")
(require 'browse-url)                   ;you need the Emacs 20 version
(require 'thingatpt)

(defvar hyperclim-base-url
  "http://bauhh.dyndns.org:8000/clim-spec/")

(defvar specification-symbols
  (cl-flet ((read-file (name)
              (car (read-from-string
                    (with-current-buffer (find-file-noselect name)
                      (buffer-substring-no-properties (point-min) (point-max)))))))
    (append (read-file "../data/clim-symbols.sexp")
            (read-file "../data/clim-sys-symbols.sexp"))))

;;; Internal bits

(defvar hyperclim--history nil)

(defun hyperclim--reference-url (reference)
  (cl-destructuring-bind (section sub-section id) reference
    (concat section
            (when sub-section
              (format "-%s" sub-section))
            (format ".html#_%d" id))))

(defun hyperclim--entry-url (entry)
  (cl-destructuring-bind (entry . index) entry
    (concat hyperclim-base-url
            (hyperclim--reference-url (nth index (third entry))))))

(defun hyperclim--entry-pretty-names (entry)
  (let ((result '()))
    (cl-destructuring-bind (name kind references) entry
      (dolist (reference references)
        (cl-destructuring-bind (section sub-section id) reference
          (push (format "%s [%s] in %s.%s[%d]"
                        name kind section sub-section id)
                result))))
    (nreverse result)))

(cl-defun hyperclim--matching-entry (pretty-name collection)
  (dolist (entry collection)
    (let ((i 0))
      (dolist (pretty-name* (hyperclim--entry-pretty-names entry))
        (when (string= pretty-name pretty-name*)
          (cl-return-from hyperclim--matching-entry
            (cons entry i)))
        (cl-incf i)))))

(defun hyperclim--matching-entries (name)
  (cl-remove name specification-symbols :test-not #'string= :key #'first))

(defun hyperclim--completion-collection (collection)
  (let ((result '()))
    (dolist (entry collection)
      (dolist (name (hyperclim--entry-pretty-names entry))
        (push name result)))
    (nreverse result)))

(defun clim-lookup (p)
  "Look up the symbol P or symbol under point.

By default it looks up the symbol under the point, but if it
isn't over something resembling a symbol, it will prompt you.

Also, you can use a prefix arg to force prompting."
  (interactive "p")
  (let ((collection (hyperclim--completion-collection specification-symbols))
        (symbol-name (thing-at-point 'symbol)))
    (unless (and (= 1 p) (stringp symbol-name))
      (setq symbol-name (completing-read
                         "Symbol name: " collection nil t symbol-name 'hyperclim--history)))
    (let ((entry (or (hyperclim--matching-entry symbol-name specification-symbols)
                     (let* ((entries (hyperclim--matching-entries symbol-name))
                            (entry   (completing-read "Entry: " (hyperclim--completion-collection entries) nil t)))
                       (hyperclim--matching-entry entry specification-symbols)))))
      (if entry
          (browse-url (hyperclim--entry-url entry))
        (message "Symbol %s not found." symbol-name)))))

;;; Font lock highlighting for CLIM-specified symbols

(defvar hyperclim-specification-keyword-exceptions
  '("t" "nil"
    "boolean"
    "integer" "ratio" "rational" "float" "real" "complex" "number"
    "character" "string" "symbol" "keyword" "pathname" "sequence"
    "null" "not" "and" "or" "member")
  "Entries that should not be highlighted.
Mostly because they coincide with Common Lisp symbols that are
frequently used for other purposes.")

(defface font-lock-clim-specified-face
  '((t . (:underline "gray40")))
  "Face for symbols specified in the CLIM specification."
  :group 'slime)

(defun hyperclim-add-specification-keywords ()
  "Add highlighting for CLIM-specified symbols."
  (let ((keywords '()))
    (dolist (entry specification-symbols)
      (cl-destructuring-bind (name kind references) entry
        (when (and (not (cl-member name hyperclim-specification-keyword-exceptions
                                   :test #'string=))
                   (not (eq kind :concept))
                   references)
          (let ((regex (format "\\_<\\(%s\\)\\_>" (regexp-quote name))))
            (push `(,regex 1 'font-lock-clim-specified-face)
                  keywords)))))
    (font-lock-add-keywords nil keywords)))

(add-hook 'lisp-mode-hook
          (lambda ()
            (when (string-match-p "/mcclim/" (buffer-file-name))
              (hyperclim-add-specification-keywords))))

(provide 'hyperclim)
;;; hyperclim.el ends here
