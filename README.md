## Named Buffer for Emacs

NAMED-BUFFER mode is a small extension which has a
simple way to create new buffer does not think about it name.
By default, name of a new buffer looks like TEMP<2>, but you can set your name.
When the buffer will be created it name will be \"TEMP\". The next 
buffer name increment number \"TEMP<2>\" and etc.
![Emacs ui: named-buffer in use.](https://github.com/dkrivets/named-buffer/blob/assets/emacs_textbufer.png?raw=true)

## Customization
Have 1 customize parameters:
1. NAMED-BUFFER-PREFIX-NAME: Prefix of buffer name - "TEMP".

## Installation
```emacs-lisp
(require 'named-buffer)
(named-buffer)
(global-set-key (kbd "C-x n n") #'named-buffer-create)
```
