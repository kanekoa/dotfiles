;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; http://code.google.com/p/yasnippet/
(setq yas/trigger-key "\C-c TAB")
(require 'yasnippet-config)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;;(require 'dropdown-list)
;;(setq yas/prompt-functions '(yas/dropdown-prompt))

