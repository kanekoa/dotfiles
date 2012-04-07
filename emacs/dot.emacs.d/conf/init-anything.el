;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'anything)
;;(require 'anything-startup)
(require 'anything-config)
(require 'anything-match-plugin)
(and (equal current-language-environment "Japanese")
     (require 'anything-migemo nil t))
(require 'anything-complete nil t)
(require 'anything-show-completion)

(setq anything-sources
	  (list anything-c-source-buffers+
			anything-c-source-recentf
			anything-c-source-files-in-current-dir+
			anything-c-source-complex-command-history
			anything-c-source-emacs-commands
			;;anything-c-source-locate
			anything-c-source-mac-spotlight
			))

(setq anything-enable-shortcuts 'alphabet)
(global-set-key [?\C-\o] 'anything-at-point)

(substitute-key-definition 'kill-summary 'anything-show-kill-ring global-map)
(substitute-key-definition 'execute-extended-command 'anything-execute-extended-command global-map)

(defun anything-split-window (buf)
  (split-window)
  (other-window 1)
  (switch-to-buffer buf))
(setq anything-display-function 'anything-split-window)
