;;; named-buffer.el --- Creating named buffer -*- coding: utf-8; lexical-binding: t -*-

;; Author: DKrivets
;; Created: 29 Dec 2018
;; Version: 0.0.5
;; Keywords: named-buffer, languages, programming
;; Homepage: https://github.com/dkrivets/named-buffer
;; Package-Require: ((emacs "24"))

;;; Commentary:
;;  Simple way to create new buffer does not think about it name.
;;  Have 1 parameter1:
;;  1. NAMED-BUFFER-PREFIX-NAME: Prefix of buffer name.
;;  By default, it looks like "TEMP".
;;  When buffer will be created it name will be "TEMP".
;;  And the next buffer will have a name like "TEMP<2>".
;; TODO add internationalization
;;
;; Change log:
;; 09 Feb 2019 Deleted text-buffer-load-hook and default key binding
;; 11 Jan 2019 Simplify algorithm: idea from alphapapa
;; 10 Jan 2019 Added hook \\[named-buffer-load-hook] it may help to change key binding
;; 07 Apr 2019 Rename mode and file
;;; Code:

;;;; Requirements


;;;; Customization

(defgroup named-buffer nil
  "Simple way to create new buffer does not think about it name."
  :group 'applications)


(defcustom named-buffer-prefix-name "TEMP"
  "Prefix of buffer name."
  :type  'string
  :group 'named-buffer)


;;;; Functions

(defun named-buffer--base-create-buffer (name)
  "Create buffer with NAME and switch to it."
  (switch-to-buffer (get-buffer-create name)))


(defun named-buffer--get-name (name)
  "Get NAME of buffer.
If NAME is null generate new name base on `named-buffer-prefix-name'."
  (if (> 0 (length name))
      name
    (generate-new-buffer-name named-buffer-prefix-name)))

;;;;; Commands

;;;###autoload
(defun named-buffer-create ()
  "Create buffer with NAME interactivly.
Main function which creates buffer with name you can input or default
which count from exist buffer."
  (interactive)
  ;; Create user helper with buffer-name
  (let* ((desc (format "New buffer name:[%s] " (generate-new-buffer-name named-buffer-prefix-name)))
         (name (read-string desc))  ;; Read user data from mini-buffer
         (buf-name (named-buffer--get-name name)))
	;; Run process
	(named-buffer--base-create-buffer buf-name)))


;;;###autoload
(define-minor-mode named-buffer
  "NAMED-BUFFER mode.
Simple way to create new buffer does not think about it name.
Have 1 customize parameters:
1. NAMED-BUFFER-PREFIX-NAME: Prefix of buffer name.
By default, it looks like \"TEMP\".
When the buffer will be created it name will be \"TEMP\" and next \"TEMP<2>\".

Example:
(require 'named-buffer)
(named-buffer)
(global-set-key (kbd \"C-x n n\") #'named-buffer-create)
"
  :group 'named-buffer
  :require 'named-buffer
  :lighter " NB"
  :global t
  (make-local-variable 'named-buffer-map)
)

;;;; Footer

(provide 'named-buffer)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; named-buffer.el ends here
