;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'smart-compile)

(setq smart-compile-alist
      (append '(("\\.pl\\'" . "/opt/local/bin/perl %f")
                ("\\.py\\'" . "/opt/local/bin/python %f"))
              smart-compile-alist))

(global-set-key "\C-cr" 'smart-compile)
