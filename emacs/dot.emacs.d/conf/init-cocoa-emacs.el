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
(define-key global-map [?¥] [?\\])

;; Font
(set-face-attribute 'default nil
            :family "Bitstream Vera Sans Mono"
            ;:family "Monaco"
            :height 125)
            ;:height 120)
		    ;:height 90)
(set-fontset-font "fontset-default"
		  'japanese-jisx0208
		  '("Osaka" . "iso10646-1"))
(set-fontset-font "fontset-default"
		  'katakana-jisx0201
		  '("Osaka" . "iso10646-1"))
(setq face-font-rescale-alist
      '((".*Monaco-bold.*" . 1.0)
        (".*Monaco-medium.*" . 1.0)
        (".*Osaka-bold.*" . 1.2)
        (".*Osaka-medium.*" . 1.2)
        ("-cdac$" . 1.4)))


