;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ref http://d.hatena.ne.jp/higepon/20080731/1217491155

(require `autoinsert)

(setq auto-insert-directory "~/.emacs.d/autoinsert-tmpl/")

;; 各ファイルによってテンプレートを切り替える
(setq auto-insert-alist
      (nconc '(
               ("\\.cpp$" . ["template.cpp" my-template])
               ("\\.h$"   . ["template.h" my-template])
			   ("\\.pl$"   . ["template.pl" my-template])
			   ("\\.pm$"   . ["template.pm" my-template])
			   ("\\.html$" . ["template.html" my-template])
			   ("\\.el$" . ["template.el" my-template])
			   ("\\.py$" . ["template.py" my-template])
               ) auto-insert-alist))
(require 'cl)

;; ここが腕の見せ所
(defvar template-replacements-alists
  '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%"    . (lambda () (format "__SCHEME_%s__" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))

(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
        (progn
          (goto-char (point-min))
          (replace-string (car c) (funcall (cdr c)) nil)))
    template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

