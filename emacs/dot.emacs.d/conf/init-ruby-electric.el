;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

