;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'rcodetools)
(setq rct-find-tag-if-avaiable nil)
(defun ruby-mode-hook-rcodertools ()
  (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
  (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
  (define-key ruby-mode-map "\C-c\C-d" 'xmp)
  (define-key ruby-mode-map "\C-c\C-f" 'rct-ri))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)

(require 'anything-rcodetools)
(setq rct-get-all-methods-command "PAGER=cat fri -l")
;; See docs
(define-key anything-map [(control ?;)] 'anything-execute-persistent-action)
