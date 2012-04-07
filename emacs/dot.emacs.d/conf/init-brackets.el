;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(load "brackets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for C
(add-hook 'c-mode-hook
          '(lambda()
             (progn
               ;; { で{}を書く
               (define-key c-mode-map "{" 'insert-braces)
               ;; ( で()を書く
               (define-key c-mode-map "(" 'insert-parens)
               ;; " で""を書く
               (define-key c-mode-map "\"" 'insert-double-quotation)
               ;; [ で[]を書く
               (define-key c-mode-map "[" 'insert-brackets)
               ;; Ctrl+c }でregionを{}で囲む
               (define-key c-mode-map "\C-c}" 'insert-braces-region)
               ;; Ctrl+c )でregionを()で囲む
               (define-key c-mode-map "\C-c)" 'insert-parens-region)
               ;; Ctrl+c ]でregionを[]で囲む
               (define-key c-mode-map "\C-c]" 'insert-brackets-region)
               ;; Ctrl+c "でregionを""で囲む
               (define-key c-mode-map "\C-c\"" 'insert-double-quotation-region)
               )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for cperl-mode
(add-hook 'cperl-mode-hook
          '(lambda()
             (progn
               (define-key cperl-mode-map "{" 'insert-braces)
               (define-key cperl-mode-map "(" 'insert-parens)
               (define-key cperl-mode-map "\"" 'insert-double-quotation)
               (define-key cperl-mode-map "[" 'insert-brackets)
               (define-key cperl-mode-map "\C-c}" 'insert-braces-region)
               (define-key cperl-mode-map "\C-c)" 'insert-parens-region)
               (define-key cperl-mode-map "\C-c]" 'insert-brackets-region)
               (define-key cperl-mode-map "\C-c\"" 'insert-double-quotation-region)
               )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for lisp-mode
;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda()
;;              (progn
;;                (define-key emacs-lisp-mode-map "(" 'insert-parens)
;;                (define-key emacs-lisp-mode-map "\"" 'insert-double-quotation)
;;                (define-key emacs-lisp-mode-map "\C-c)" 'insert-parens-region)
;;                )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for emacs-lisp-mode
;; (add-hook 'lisp-mode-hook
;;           '(lambda()
;;              (progn
;;                (define-key lisp-mode-map "(" 'insert-parens)
;;                (define-key lisp-mode-map "\"" 'insert-double-quotation)
;;                (define-key lisp-mode-map "\C-c)" 'insert-parens-region)
;;                )))