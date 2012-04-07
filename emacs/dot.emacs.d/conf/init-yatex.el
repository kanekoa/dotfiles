;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ref: http://macwiki.sourceforge.jp/wiki/index.php/CarbonEmacsAndYatex


;;;; YaTeX (野鳥)
;; yatex-mode を起動させる設定
(setq auto-mode-alist 
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)


;; 自動改行時の文字数
(defvar YaTeX-fill-column 90)


;; 文章作成時の漢字コードの設定
;; 1 = Shift_JIS, 2 = ISO-2022-JP, 3 = EUC-JP
;; default は 2
(setq YaTeX-kanji-code 3) ; euc-jp


;; TeXShopのplatexとプレビュアを利用
;; Macでしか動かないので注意
(setq tex-command "~/Library/TeXShop/bin/platex2pdf-euc")
(setq dvi2-command "open -a TeXShop")


;; section型の補完でミニバッファから入力しない
(setq YaTeX-skip-default-reader t)


;; PATH
;(setq exec-path (cons "/opt/local/bin" exec-path))
;(setenv "PATH"
;	(concat '"/opt/local/bin:" (getenv "PATH")))

