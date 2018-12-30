;;; text-buffer.el --- Options for window -*- coding: utf-8; lexical-binding: t -*-

;; Author: DKrivets
;; Created: 29 Dec 2018
;; Version: 0.0.1
;; Keywords: text-buffer, languages, programming
;; Homepage: https://github.com/dkrivets/text-buffer
;; Package-Require: ((dash "2.14.1"))

;;; Commentary:
;;  Simply work with new buffers

;;; Code:

(defgroup text-buffer nil "Simply work with new buffers." :group 'applications)


(defcustom text-prefix-name "TEMP"
  "Prefix of buffer name."
  :type 'string
  :group 'text-buffer)


(defcustom text-splitter "-"
  "Splitter of buffer name."
  :type 'string
  :group 'text-buffer)


(defvar text-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x n") 'tb-create-buffer))
  "Keymap for text-buffer.")


(defun tb--get-template-name ()
  "Get template for buffer name."
  (concat text-prefix-name text-splitter))


(defun tb--base-create-buffer (name)
  "Create buffer with NAME and switch to it."
  (switch-to-buffer (get-buffer-create name)))


(defun tb--get-buffer-list ()
  "Get list of buffers with template name."
  (delq nil
	(mapcar
	 (lambda (i)
	   (let ((buf          (buffer-name i))
		 (template     (tb-get-template-name))
		 (template-len (length (tb-get-template-name))))
	     (if (< template-len (length buf))
		 (if (string= template (substring buf 0 template-len))
		     i
		   nil))))
	   (append (buffer-list) ()))))


(defun tb--get-max-buf-num (buf-list)
  "Get max exists bufer num with template name in BUF-LIST."
  ;; Check size of list
  ;; Return 0 or work with buffer list
  (if (= 0 (length buf-list))
      0
    (-max
     (-map
      (lambda (i)
	;; Get exists postfix of buffer
	(let ((num (substring (buffer-name i) (length (tb--get-template-name)))))
	  ;; Convert postfix to number
	  (string-to-number num)))
      buf-list))))


(defun tb--make-default-name ()
  "Make default name.
Uses format %s%d where %s - is a concatination of values
of text-prefix-name text-splitter and %d count + 1 of same buffers."
  (format "%s%d"
	  ;; Get template name
	  (tb--get-template-name)
	  ;; Count of same buffers with apply 1
	  (1+ (tb--get-max-buf-num (tb--get-buffer-list)))))


(defun tb-create-buffer ()
  "Create buffer with NAME interactivly.
Main function which creates buffer with name you can input or default
which count from exist buffer."
  (interactive)
  ;; Create user helper with buffer-name
  (let ((desc (format "New buffer name:[%s] " (tb--make-default-name))))
    ;; Read user data from mini-buffer
    (let ((name (read-string desc)))
      ;; Check which data we will be use: users or default
      (let ((buf-name
	     (if (> 0 (length name))
		 name
	       (tb--make-default-name))))
	;; Run process
	(tb--base-create-buffer buf-name)))))


(define-minor-mode text-buffer
  "TEXT-BUFFER mode."
  :group 'text-buffer
  :require 'text-buffer
  :lighter " TB"
  :keymap text-buffer-map
  :global t
  )

(provide 'text-buffer)
;;; text-buffer.el ends here
