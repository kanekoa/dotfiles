;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-


;;-----------------------------------------------------------------
;; Window size and position
;;-----------------------------------------------------------------
(setq initial-frame-alist '((width . 100) (height . 50)))
;;-----------------------------------------------------------------
;; 半透明
;;-----------------------------------------------------------------
(add-to-list 'default-frame-alist '(alpha . (0.97 0.9)))
;;-----------------------------------------------------------------
;; Color
;;-----------------------------------------------------------------
(set-foreground-color "#ffffff")
(set-background-color "#000")
(set-cursor-color "#ffffff")

;;-----------------------------------------------------------------
;; anti-aliasing by Quartz 2D
;;-----------------------------------------------------------------
(setq mac-allow-anti-aliasing t)

;; no beep, no flush
(setq ring-bell-function 'ignore)

;; Command-Key and Option-Key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))
