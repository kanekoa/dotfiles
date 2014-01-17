;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ref http://svn.codecheck.in/dotfiles/emacs/sugmak/

;;-----------------------------------------------------------------
;; OSを判別
;;-----------------------------------------------------------------
(defvar run-unix  (or (equal system-type 'gnu/linux)
                      (or (equal system-type 'usg-unix-v)
                          (or  (equal system-type 'berkeley-unix)
                               (equal system-type 'cygwin)))))

(defvar run-linux
  (equal system-type 'gnu/linux))
(defvar run-system-v
  (equal system-type 'usg-unix-v))
(defvar run-bsd
  (equal system-type 'berkeley-unix))
(defvar run-cygwin ;; cygwinもunixグループにしておく
  (equal system-type 'cygwin))

(defvar run-w32
  (and (null run-unix)
       (or (equal system-type 'windows-nt)
           (equal system-type 'ms-dos))))

(defvar run-darwin (equal system-type 'darwin))

;;-----------------------------------------------------------------
;; Emacsenの種類とヴァージョンを判別
;;-----------------------------------------------------------------
(defvar run-emacs20
  (and (equal emacs-major-version 20)
       (null (featurep 'xemacs))))
(defvar run-emacs21
  (and (equal emacs-major-version 21)
       (null (featurep 'xemacs))))
(defvar run-emacs22
  (and (equal emacs-major-version 22)
       (null (featurep 'xemacs))))
(defvar run-emacs23
  (and (equal emacs-major-version 23)
       (null (featurep 'xemacs))))
(defvar run-meadow (featurep 'meadow))
(defvar run-meadow1 (and run-meadow run-emacs20))
(defvar run-meadow2 (and run-meadow run-emacs21))
(defvar run-meadow3 (and run-meadow run-emacs22))
(defvar run-xemacs (featurep 'xemacs))
(defvar run-xemacs-no-mule
  (and run-xemacs (not (featurep 'mule))))
(defvar run-carbon-emacs (featurep 'carbon-emacs-package))
(defvar run-cocoa-emacs (and run-darwin window-system))

;;-----------------------------------------------------------------
;; elispと設定ファイルのディレクトリをload-pathに追加
;;-----------------------------------------------------------------
(setq load-path
      (append (list
               (expand-file-name "~/.emacs.d/site-lisp/apel/")
               (expand-file-name "~/.emacs.d/site-lisp/")
               (expand-file-name "~/.emacs.d/conf/")
               )
              load-path))
;;-----------------------------------------------------------------
;; Load every single settings
;;-----------------------------------------------------------------
;; Global Settings
(load "init-global")
;;---------------------------------
;; Japanese
;;---------------------------------
;;(load "init-skk")
;;(load "init-migemo")
;;---------------------------------
;; Utilities
;;---------------------------------
;; multi window
;; (load "init-windows")
(load "init-elscreen")
;;(load "init-color")
;;(load "init-ibuffer")
;;(load "init-mcomplete")
;;(load "init-mic-paren")
;;(load "init-highlight-current-line")
;;(load "init-highlight-completion")
;;(load "init-ce-scroll")
;;(load "init-ls-lisp")
;;(load "init-killring")
(load "init-kill-summary")
(load "init-session")
;;(load "init-revive")
;;(load "init-install")
(load "init-linum")
;;(load "init-minibuf-isearch")
;;(load "init-calendar")
;;(load "init-url")
;;(load "init-recentf")
(load "init-screen-lines")
;; (load "init-autosave-buffers")
;; (load "init-tempo-snippets")
(load "init-swbuf")
(load "init-dmacro")
;;(load "init-widen-window")
(load "init-goto-chg")
;;(load "init-iswitchb")
(load "init-auto-install")
(load "init-sense-region")
(load "init-smartchr")
(load "init-sequential-command")
(load "init-minor-mode-hack")
(load "init-key-chord")
(load "init-summarye")
(load "init-fold-dwim")
(load "init-surround-string")
;;---------------------------------
;; Dired
;;---------------------------------
(load "init-dired")
;;(load "init-dired-sorter")
(load "init-wdired")
;;---------------------------------
;; Search
;;---------------------------------
;;(load "init-grep")
(load "init-color-moccur")
(load "init-moccur-edit")
;;(load "init-isearch-all")
(load "init-ack")
;;---------------------------------
;; Shell, Terminal
;;---------------------------------
;;(load "init-shell")
;; (load "init-eshell")
;; (load "init-term")
;;---------------------------------
;; Abbreviation
;;---------------------------------
(load "init-auto-complete")
;; (load "init-abbrev-complete")
;; (load "init-dabbrev-ja")
;; (load "init-dabbrev-highlight")
;;---------------------------------
;; Coding Assistance, Snippets, etc
;;---------------------------------
;;(load "init-template")
(load "init-auto-insert")
;; (load "init-tags")
;;(load "init-brackets")
(load "init-yasnippet")
;;(load "init-yasnippet-bundle")
;; (load "init-ac-mode") ;; conflicts with auto-complete
;; (load "init-abbrves")
;;(load "init-regex-tool")
(load "init-flymake")
;;(load "init-woman")
;;---------------------------------
;; Anything.el
;;---------------------------------
(load "init-anything")
;;(load "init-anything-c-moccur")
;;(load "init-anything-c-yasnippet")
;; (load "init-anything-dabbrev-expand")
(load "init-anything-project")
;;---------------------------------
;; Version Control
;;---------------------------------
;;(load "init-psvn")
;;(load "init-psvk")
;; (load "init-svk")
(load "init-dsvn")
(load "init-mvc")
;;---------------------------------
;; Development Environment
;;---------------------------------
;;(load "init-cedet")
;;(load "init-ecb")
(load "init-smart-compile")
;;==========;;==========;;==========;;
;; Programing Language
;;==========;;==========;;==========;;
(load "init-cc")
;;-------------------------
;; Common Lisp
;;-------------------------
;;(load "init-lisp")
;;(load "init-slime")
;;-------------------------
;; Perl
;;-------------------------
(load "init-cperl")
(load "init-perl-setperl5lib")
(load "init-perl-flymake")
;;(load "init-perl-completion")
;;(load "init-pod")
;;(load "init-perltidy")
;;(load "init-perlcritic")
;; (load "init-perlnow")
;; (load "init-html-tt")
;; (load "init-pde")
;;-------------------------
;; JS + AS
;;-------------------------
(load "init-actionscript")
(load "init-js2")
;;-------------------------
;; Python
;;-------------------------
(load "init-python")
;;-------------------------
;; Ruby
;;-------------------------
(load "init-ruby")
(load "init-ruby-electric")
(load "init-ruby-block")
(load "init-rcodetools")
;;-------------------------
;; PHP
;;-------------------------
;;(load "init-php")
;;(load "init-php-flymake")
;;-------------------------
;; SQL
;;-------------------------
;;(load "init-sql")
;;-------------------------
;; Java
;;-------------------------
;;(load "init-java")
;;-------------------------
;; Erlang
;;-------------------------
;;(load "init-erlang")
;;-------------------------
;; Haskell
;;-------------------------
;;(load "init-haskell")
;;-------------------------
;; scheme
;;-------------------------
;;(load "init-scheme")
(load "init-scala")
;;-------------------------
;; HTML + CSS
;;-------------------------
;;(load "init-yahtml")
;;(load "init-nxml")
;;(load "init-html")
;;(load "init-html-helper")
;;(load "init-color-selection")
;;(load "init-sgml")
;;(load "init-css")
(load "init-zencoding")
(load "init-sgml-match-mode")
;;-------------------------
;; TEX (yet another tex)
;;-------------------------
;;(load "init-yatex")
;;-------------------------
;; YAML
;;-------------------------
;;(load "init-yaml")
;;---------------------------------
;; Networking
;;---------------------------------
;; (load "init-mew")
;;(load "init-tramp")
;;---------------------------------
;; GTD, Memo
;;---------------------------------
;; (load "init-howm")
;; (load "init-remember")
;;---------------------------------
;; Document Creation
;;---------------------------------
;;(load "init-doc-view")
;;(load "init-sdic")
;;(load "init-goby")
;;(load "init-org")
;;(load "init-muse")
;;---------------------------------
;; Wiki
;;---------------------------------
;;(load "init-moinmoin")
;;---------------------------------
;; Application
;;---------------------------------
;;(load "init-erc")
;;(load "init-navi2ch")
;;(load "init-twitter")
;;(load "init-w3m")
;; (load "init-g-client")
;;-----------------------------------------------------------------
;; Settings for Mac
;;-----------------------------------------------------------------
(when
    (and run-darwin
         (load "init-mac")
         )
  )
;;-----------------------------------------------------------------
;; Settings for Carbon/Cocoa Emacs
;;-----------------------------------------------------------------
(when
    (and run-carbon-emacs
         (load "init-carbon-emacs")
         ))
(when
    (and run-cocoa-emacs
         (load "init-cocoa-emacs")
         ))
;;-----------------------------------------------------------------
;; 以下、自動生成されたもの
;;-----------------------------------------------------------------
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-battery-mode nil)
 '(display-time-mode nil)
;; '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


