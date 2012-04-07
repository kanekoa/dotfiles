;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ref: http://d.hatena.ne.jp/rubikitch/20091221/autoinstall

(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/site-lisp/")
;;(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)             ; 互換性確保
(setq ediff-window-setup-function 'ediff-setup-window-plain)