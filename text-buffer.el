;;; text-buffer.el --- Creating named buffer -*- coding: utf-8; lexical-binding: t -*-

;; Author: DKrivets
;; Created: 29 Dec 2018
;; Version: 0.0.4
;; Keywords: text-buffer, languages, programming
;; Homepage: https://github.com/dkrivets/text-buffer
;; Package-Require: ((emacs "24"))

;;; Commentary:
;;  Simple way to create new buffer does not think about it name.
;;  Have 1 parameter1:
;;  1. TEXT-PREFIX-NAME: Prefix of buffer name.
;;  By default, it looks like "TEMP".
;;  When buffer will be created it name will be "TEMP".
;;  And the next buffer will have a name like "TEMP<2>".
;; TODO add internationalization
;;
;; Change log:
;; 09 Feb 2019 Deleted text-buffer-load-hook and default key binding
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
(defun text-buffer-create ()
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
Have 1 customize parameters:
1. TEXT-PREFIX-NAME: Prefix of buffer name.
By default, it looks like \"TEMP\".
When the buffer will be created it name will be \"TEMP\" and next \"TEMP<2>\".

Example:
(require 'text-buffer)
(text-buffer)
(global-set-key (kbd \"C-x n\") #'text-buffer-create)
"
  :group 'text-buffer
  :require 'text-buffer
  :lighter " TB"
  :global t
  (make-local-variable 'text-buffer-map)
)

;;;; Footer

(provide 'text-buffer)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; text-buffer.el ends here
