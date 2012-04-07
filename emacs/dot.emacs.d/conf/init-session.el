;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ref http://www.bookshelf.jp/soft/meadow_27.html#SEC346

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; 保存時でなく閉じた際の状態を保存
(setq session-undo-check -1)
