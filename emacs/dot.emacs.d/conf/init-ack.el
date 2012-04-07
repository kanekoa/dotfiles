;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ref http://d.hatena.ne.jp/antipop/20080810/1218369556

(defun ack ()
  (interactive)
  (let ((grep-find-command "ack --nogroup"))
	(call-interactively 'grep-find)))


