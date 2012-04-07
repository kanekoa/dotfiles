;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'auto-complete-config)
(global-auto-complete-mode 1)
;;(ac-config-default)

(setq ac-sources
  '(ac-source-abbrev
	ac-source-words-in-same-mode-buffers
	ac-source-yasnippet
	))

(add-hook 'cperl-mode-hook
		 (lambda ()
		   (require 'perl-completion)
		   (add-to-list 'ac-sources 'ac-source-perl-completion)))
