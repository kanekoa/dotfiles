;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'sgml-match-mode)
(add-hook 'sgml-mode-hook
          '(lambda ()
             (sgml-match-mode t)))
