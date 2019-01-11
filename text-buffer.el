;;; text-buffer.el --- Creating named buffer -*- coding: utf-8; lexical-binding: t -*-

;; Author: DKrivets
;; Created: 29 Dec 2018
;; Version: 0.0.3
;; Keywords: text-buffer, languages, programming
;; Homepage: https://github.com/dkrivets/text-buffer
;; Package-Require: ((emacs "24"))

;;; Commentary:
;;  Simple way to create new buffer does not think about it name.
;;  Have 2 parameters:
;;  1. TEXT-PREFIX-NAME: Prefix of buffer name.
;;  2. TEXT-SPLITTER: Splitter of buffer name.
;;  By default, it looks like "TEMP-".
;;  When buffer will be created it name will be "TEMP-1".
;;  Package has an one key-binding to create a buffer: \\[C-x n]
;; TODO add internationalization
;;
;; Change log:
;; 11 Jan 2019 Simplify algorithm: idea from alphapapa
;; 10 Jan 2019 Added hook \\[text-buffer-load-hook] it may help to change key binding
;;; Code:

;;;; Requirements


;;;; Customization

(defgroup text-buffer nil
  "Simple way to create new buffer does not think about it name."
  :group 'applications)


(defcustom text-buffer-prefix-name "TEMP"
  "Prefix of buffer name."
  :type  'string
  :group 'text-buffer)


(defcustom text-buffer-load-hook nil
  "A hook run once when the package has been loaded."
  :type 'hook
  :group 'text-buffer)


;;;; Variables

;;;;; Keymaps

(defvar text-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x n") #'text-buffer-create-buffer)
    map)
  "Keymap for text-buffer: to create a buffer: `\\[text-buffer-create-buffer]'.")


;;;; Functions

(defun text-buffer--base-create-buffer (name)
  "Create buffer with NAME and switch to it."
  (switch-to-buffer (get-buffer-create name)))


(defun text-buffer--get-name (name)
  "Get NAME of buffer.
If NAME is null generate new name base on `text-buffer-prefix-name'."
  (if (> 0 (length name))
      name
    (generate-new-buffer-name text-buffer-prefix-name)))

;;;;; Commands

;;;###autoload
(defun text-buffer-create-buffer ()
  "Create buffer with NAME interactivly.
Main function which creates buffer with name you can input or default
which count from exist buffer."
  (interactive)
  ;; Create user helper with buffer-name
  (let* ((desc (format "New buffer name:[%s] " (generate-new-buffer-name text-buffer-prefix-name)))
         (name (read-string desc))  ;; Read user data from mini-buffer
         (buf-name (text-buffer--get-name name)))
	;; Run process
	(text-buffer--base-create-buffer buf-name)))


;;;###autoload
(define-minor-mode text-buffer
  "TEXT-BUFFER mode.
Simple way to create new buffer does not think about it name.
Have 2 customize parameters:
1. TEXT-PREFIX-NAME: Prefix of buffer name.
By default, it looks like \"TEMP-\".
When the buffer will be created it name will be \"TEMP-1\".

Key bindings:
`\\{text-buffer-map}'

To change key-binding there is a hook \\[text-buffer-load-hook] which can be used.
(add-hook 'text-buffer-load-hook
  (lambda ()
    (define-key text-buffer-map (kbd \"<f9>\") \\=#'text-buffer-create-buffer)))"
  :group 'text-buffer
  :require 'text-buffer
  :lighter " TB"
  :keymap text-buffer-map
  :global t
  (make-local-variable 'text-buffer-map)
)

;; Hook on load
(run-hooks 'text-buffer-load-hook)

;;;; Footer

(provide 'text-buffer)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; text-buffer.el ends here
