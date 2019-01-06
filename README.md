### Text Buffer for Emacs

TEXT-BUFFER mode is a small extension which has a
simple way to create new buffer does not think about it name.
By default, name of a new buffer looks like TEMP-1, but you can set your name.
When the buffer will be created it name will be \"TEMP-1\". The next 
buffer name increment number \"TEMP-2\" and etc.
![Emacs ui: text-buffer in use.](https://github.com/dkrivets/text-buffer/blob/assets/emacs_textbufer.png?raw=true)

#### Customization
Have 2 customize parameters:
1. TEXT-PREFIX-NAME: Prefix of buffer name - "TEMP".
2. TEXT-SPLITTER: Splitter of buffer name - "-".

#### Key bindings
By default, for creating new buffer uses C-x n .

#### Installation
```emacs-lisp
(require 'text-buffer)
(text-buffer)
```
