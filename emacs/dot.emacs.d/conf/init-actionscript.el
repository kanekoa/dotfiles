;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(defvar running-on-x (eq window-system 'x))
(eval-after-load "actionscript-mode" '(load "as-config"))
(autoload 'actionscript-mode "actionscript-mode" "ActionScript" t)
(setq auto-mode-alist (append '(("\\.as$" . actionscript-mode)) auto-mode-alist))
