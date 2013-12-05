;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile" . ruby-mode))
