;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(load "elscreen" "ElScreen" t)

(global-set-key [?\C-,] (lambda () (interactive) (elscreen-previous)))
(global-set-key [?\C-.] (lambda () (interactive) (elscreen-next)))
