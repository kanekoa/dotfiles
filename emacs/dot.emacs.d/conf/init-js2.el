;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 2)
             (setq tab-width 2)
             (setq indent-tabs-mode nil)            ;; タブを無効に
             (setq js2-cleanup-whitespace nil)      ;; 行末空白の自動削除無効
             (setq js2-enter-indents-newline nil)   ;; C-mでインデントしないように
             (setq js2-mirror-mode nil)             ;; (, { とかを入力した際，対応する括弧とかを自動的に挿入しない。
             (setq js2-bounce-indent-flag nil)      ;; インデント固定

             ;; ref http://8-p.info/emacs-javascript.html            
             (defun indent-and-back-to-indentation ()
               (interactive)
               (indent-for-tab-command)
               (let ((point-of-indentation
                      (save-excursion
                        (back-to-indentation)
                        (point))))
                 (skip-chars-forward "\s " point-of-indentation)))
             (define-key js2-mode-map "\C-i" 'indent-and-back-to-indentation)
))
