;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-


;;-----------------------------------------------------------------
;; ファイル名の濁点・半濁点を化けないようにする
;;-----------------------------------------------------------------
(autoload 'utf-8m "utf-8m" t)
(set-file-name-coding-system 'utf-8m)
;;-----------------------------------------------------------------
;; Window size and position
;;-----------------------------------------------------------------
(setq initial-frame-alist '((width . 100) (height . 50)))
;;-----------------------------------------------------------------
;; 半透明
;;-----------------------------------------------------------------
(add-to-list 'default-frame-alist '(alpha . (0.97 0.9)))
;;-----------------------------------------------------------------
;; Font
;;-----------------------------------------------------------------
(require 'carbon-font)  
;; profont + hiragino maru gothic
;;(setq carbon-font-encode-family-list-profont
;;      '((ascii . "profontwindows")
;;	  (japanese-jisx0208 . "ヒラギノ丸ゴ*")
;;          (katakana-jisx0201 . "ヒラギノ丸ゴ*")
;;          (korean-ksc5601 . "applegothic*")))
;;  (carbon-font-create-fontset "profontmaru"
			      ;; carbon-font-defined-sizes
;;                              10
;;			      carbon-font-encode-family-list-profont)
  
;; verdana + hiragino maru gothic
;;(setq carbon-font-encode-family-list-verdana
;;	'((ascii . "verdana")
;;        (japanese-jisx0208 . "ヒラギノ丸ゴ*")
;;        (katakana-jisx0201 . "ヒラギノ丸ゴ*")
;;        (korean-ksc5601 . "applegothic*")))
;;(carbon-font-create-fontset "verdanamaru"
			      ;; carbon-font-defined-sizes
;;                            11
;;			      carbon-font-encode-family-list-verdana)
  
;; vera + hiragino maru gothic
(setq carbon-font-encode-family-list-vera
      '((ascii . "Bitstream Vera Sans Mono*")
	(japanese-jisx0208 . "ヒラギノ丸ゴ*")
	(katakana-jisx0201 . "ヒラギノ丸ゴ*")
	(korean-ksc5601 . "applegothic*")))
(carbon-font-create-fontset "veramaru"
			    ;; carbon-font-defined-sizes
			    13
			    carbon-font-encode-family-list-vera)

;; ;; monaco + hiragino maru gothic
;; (setq carbon-font-encode-family-list-monaco
;;       '((ascii . "monaco")
;; 	(japanese-jisx0208 . "ヒラギノ丸ゴ*")
;; 	(katakana-jisx0201 . "ヒラギノ丸ゴ*")
;; 	(korean-ksc5601 . "applegothic*")))
;; (carbon-font-create-fontset "monamaru"
;; 			    ;; carbon-font-defined-sizes
;; 			    11
;; 			    carbon-font-encode-family-list-monaco)



(setq default-frame-alist (append (list
				   ;;'(font . "fontset-profontmaru")
				   ;;'(font . "fontset-verdanamaru")
				   ;;'(font . "fontset-monamaru")
				   '(font . "fontset-veramaru")
				   )default-frame-alist))
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
