;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'color-moccur)

;; .svn以下は探さない
(setq dmoccur-exclusion-mask
      (append '("\\~$" "\\.svn\\/\*") dmoccur-exclusion-mask))

(setq moccur-split-word t)

;; for anything interface
(require 'anything-c-moccur)
(setq anything-c-moccur-enable-initial-pattern t)

(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur)
(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur)
(add-hook 'dired-mode-hook
          '(lambda ()
             (local-set-key (kbd "0") 'anything-c-moccur-dired-do-moccur-by-moccur)))
