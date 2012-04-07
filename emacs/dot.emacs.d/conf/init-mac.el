;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;-----------------------------------------------------------------
;; Mac用設定
;; http://www.pqrs.org/~tekezo/macosx/doc/unix/
;;-----------------------------------------------------------------
;; コマンドキーをMETAとして使う
;;(setq mac-command-key-is-meta nil)
;;(setq mac-command-modifier-meta nil)
;; optionキーをMETAとして使う
;;(setq mac-option-modifier 'meta)
;;(setq grep-find-use-xargs 'bsd)
;;(setq browse-url-generic-program "open")
;;-----------------------------------------------------------------
;; Ctrl/Cmd/Optionがシステムに渡されるのを防ぐ
;;-----------------------------------------------------------------
;;(setq mac-pass-control-to-system nil)
;;(setq mac-pass-command-to-system nil)
;;(setq mac-pass-option-to-system nil)
;;-----------------------------------------------------------------
;; Mac OS X のインプットメソッドをオフにする
;; http://hiro2.jp/2007-07-13-1.html
;;-----------------------------------------------------------------
;;(mac-input-method-mode 0)

;;-----------------------------------------------------------------
;; Cmdキーがシステムに渡されるのを防ぐ (サービスメニューが呼ばれない様に)
;;-----------------------------------------------------------------
(setq mac-pass-command-to-system nil)

;;-----------------------------------------------------------------
;; optionキーをMETAとして使う
;;-----------------------------------------------------------------
(setq mac-option-modifier 'meta)

