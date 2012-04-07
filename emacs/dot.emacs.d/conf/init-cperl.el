;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ref: http://diary.lampetty.net/20051112.html

(defalias 'perl-mode 'cperl-mode)
(autoload 'cperl-mode
  "cperl-mode"
  "alternate mode for editing Perl programs" t)
(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-close-paren-offset -4
      cperl-comment-column 40
      cperl-highlight-variables-indiscriminately t
      cperl-indent-parens-as-block t
      cperl-label-offset -4
      cperl-tab-always-indent nil
      cperl-font-lock t)
(add-hook 'cperl-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width nil)
             (local-set-key "\C-cp" 'perl-eval)))
(setq auto-mode-alist
      (append (list (cons "\\.\\(pl\\|pm\\|t\\)$" 'cperl-mode))
              auto-mode-alist))

(defun perl-eval (beg end)
  "Run selected region as Perl code"
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "perl")))
