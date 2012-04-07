;;; mvc.el -- M(enu|ulti) Version Control Interface

;; Copyright (C) 2007,2008,2009 Tadashi Watanabe <wac@umiushi.org>

;; Author: Tadashi Watanabe <wac@umiushi.org>
;; Maintainer: Tadashi Watanabe <wac@umiushi.org>
;; Version: 
;; Keywords: tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary

;; http://umiushi.org/~wac/mvc/

;; ログを拾って、特定のリビジョンに対してコマンドを発行させたい。
;; changeset: などクリック可能な個所に face を設定してやって、
;; return やその他のキーでコマンドを発行させる?

;;; TODO

;; - Subversion/CVS の dir-list って使ってなくない?
;; - bazaar, git 対応。
;; - CVS 対応。

;;; Code:

(defgroup mvc nil
  "Version control interface for Emacs"
  :group 'tools)
(defgroup mvc-variables nil
  "mvc variables"
  :group 'mvc)
(defgroup mvc-faces nil
  "mvc faces"
  :group 'mvc)

(defcustom mvc-default-status-display-unknown t
  "mvc-default-status-display-unknown"
  :type 'boolean
  :group 'mvc-variables)
(defcustom mvc-default-status-display-unmodified t
  "mvc-default-status-display-unmodified"
  :type 'boolean
  :group 'mvc-variables)
(defcustom mvc-default-status-display-backup nil
  "mvc-default-status-display-backup"
  :type 'boolean
  :group 'mvc-variables)
(defcustom mvc-default-status-display-ignore nil
  "mvc-default-status-display-ignore"
  :type 'boolean
  :group 'mvc-variables)

(defcustom mvc-default-use-animation-timer t
  "mvc-default-use-animation-timer"
  :type 'boolean
  :group 'mvc-variables)

(defcustom mvc-default-use-color t
  "mvc-default-use-color"
  :type 'boolean
  :group 'mvc-variables)

(defcustom mvc-default-read-directory t
  "mvc-default-read-directory"
  :type 'boolean
  :group 'mvc-variables)

(defcustom mvc-default-process-connection-type nil
  "mvc-default-process-connection-type"
  :type 'boolean
  :group 'mvc-variables)

(defcustom mvc-default-tmp-directory "/tmp"
  "mvc-default-tmp-directory"
  :type 'string
  :group 'mvc-variables)

(defcustom mvc-default-program-search-concurrent t
  "mvc-default-program-search-concurrent"
  :type 'boolean
  :group 'mvc-variables)

(defcustom mvc-default-program-search-order-list '(mercurial git bazaar subversion cvs)
  "mvc-default-program-search-order-list"
  :type '(list symbol symbol symbol symbol symbol)
  :group 'mvc-variables)

;; mvc-default-option-list-system と mvc-default-option-list-user の構
;; 造は似ていますが異なります。
;; mvc-default-option-list-system は
;; ('command-key . ('vcskey . ("表示用文字列"
;;                             "コマンド引数1"
;;                             "コマンド引数2"
;;                             "...")))
;; のような構造で、 mvc-default-option-list-user は
;; ('command-key . ('vcskey . ("コマンド引数1"
;;                             "コマンド引数2"
;;                             "...")))
;; のような構造になります。つまり、 mvc-default-option-list-user には
;; "表示用文字列" が存在しないことに注意が必要です。
;;
;; この mvc-default-option-list-system はシステム向けを意識した引数で、
;; 通常はユーザが触れることはありません。ユーザがカスタマイズするのは
;; mvc-default-option-list-user となります。
(defcustom mvc-default-option-list-system '((diff . ((mercurial . ("diff"
								   "diff"))
						     (git . ("diff"
							     "diff"))
						     (bazaar . ("diff"
								"diff"))
						     (subversion . ("diff"
								    "diff"))
						     (cvs . ("diff"
							     "diff"))))
					    (add . ((mercurial . ("add"
								  "add"))
						    (git . ("add"
							    "add"))
						    (bazaar . ("add"
							       "add"))
						    (subversion . ("add"
								   "add"))
						    (cvs . ("add"
							    "add"))))
					    (annotate . ((mercurial . ("annotate"
								       "annotate"))
							 (git . ("annotate"
								 "annotate"))
							 (bazaar . ("annotate"
								    "annotate"))
							 (subversion . ("annotate"
									"annotate"))
							 (cvs . ("annotate"
								 "annotate"))))
					    (revert . ((mercurial . ("revert"
								     "revert"))
						       (git . ("revert"
							       ""))
						       (bazaar . ("revert"
								  "revert"))
						       (subversion . ("revert"
								      "revert"))
						       (cvs . ("revert"
							       "revert"))))
					    (remove . ((mercurial . ("remove"
								     "remove"))
						       (git . ("remove"
							       "rm"))
						       (bazaar . ("remove"
								  "remove"))
						       (subversion . ("remove"
								      "remove"))
						       (cvs . ("remove"
							       "remove"))))
					    (git-rm-cached . ((git . ("remove cached"
								      "rm"))))
					    (git-checkout . ((git . ("checkout"
								     "checkout"))))
					    (git-reset . ((git . ("reset"
								  "reset"))))
					    (rename . ((mercurial . ("rename"
								     "rename"))
						       (git . ("rename"
							       "mv"))
						       (bazaar . ("rename"
								  "mv"))
						       (subversion . ("rename"
								      "rename"))
						       (cvs . ("rename"
							       "rename"))))
					    (commit . ((mercurial . ("commit"
								     "commit"))
						       (git . ("commit"
							       "commit"))
						       (bazaar . ("commit"
								  "commit"))
						       (subversion . ("commit"
								      "commit"))
						       (cvs . ("commit"
							       "commit"))))
					    (status . ((mercurial . ("status"
								     "status"))
						       (git . ("status"
							       "status"))
						       (bazaar . ("status"
								  "status"))
						       (subversion . ("status"
								      "status"))
						       (cvs . ("-n update"
							       "-n"
							       "update"))))
					    (pull . ((mercurial . ("pull"
								   "pull"))
						     (git . ("pull"
							     "pull"))
						     (bazaar . ("pull"
								"pull"))
						     (subversion . ("unsupported"
								    ""))
						     (cvs . ("unsupported"
							     ""))))
					    (push . ((mercurial . ("push"
								   "push"))
						     (git . ("push"
							     "push"))
						     (bazaar . ("push"
								"push"))
						     (subversion . ("unsupported"
								    ""))
						     (cvs . ("unsupported"
							     ""))))
					    (update . ((mercurial . ("update"
								     "update"))
						       (git . ("update"
							       "pull"))
						       (bazaar . ("update"
								  "update"))
						       (subversion . ("update"
								      "update"
								      "--accept"
								      "postpone"
								      "--force"))
						       (cvs . ("update"
							       "update"))))
					    (log . ((mercurial . ("log"
								  "log"))
						    (git . ("log"
							    "log"))
						    (bazaar . ("log"
							       "log"))
						    (subversion . ("log"
								   "log"))
						    (cvs . ("log"
							    "log")))))
  "mvc-default-option-list-system"
  :type '(list (cons (const diff) (list
				   (cons (const mercurial) (repeat string))
				   (cons (const git) (repeat string))
				   (cons (const bazaar) (repeat string))
				   (cons (const subversion) (repeat string))
				   (cons (const cvs) (repeat string))))
	       (cons (const add) (list
				  (cons (const mercurial) (repeat string))
				  (cons (const git) (repeat string))
				  (cons (const bazaar) (repeat string))
				  (cons (const subversion) (repeat string))
				  (cons (const cvs) (repeat string))))
	       (cons (const annotate) (list
				       (cons (const mercurial) (repeat string))
				       (cons (const git) (repeat string))
				       (cons (const bazaar) (repeat string))
				       (cons (const subversion) (repeat string))
				       (cons (const cvs) (repeat string))))
	       (cons (const revert) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const remove) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const rename) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const commit) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const status) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const log) (list
				  (cons (const mercurial) (repeat string))
				  (cons (const git) (repeat string))
				  (cons (const bazaar) (repeat string))
				  (cons (const subversion) (repeat string))
				  (cons (const cvs) (repeat string)))))
  :group 'mvc-variables)

(defcustom mvc-default-option-list-user '((diff . ((mercurial . (""))
						   (git . (""))
						   (bazaar . (""))
						   (subversion . (""))
						   (cvs . (""))))
					  (add . ((mercurial . (""))
						  (git . (""))
						  (bazaar . (""))
						  (subversion . (""))
						  (cvs . (""))))
					  (annotate . ((mercurial . ("--user"
								     "--number"
								     "--changeset"
								     "--date"))
						       (git . (""))
						       (bazaar . (""))
						       (subversion . (""))
						       (cvs . (""))))
					  (revert . ((mercurial . ("--no-backup"))
						     (git . (""))
						     (bazaar . (""))
						     (subversion . (""))
						     (cvs . (""))))
					  (remove . ((mercurial . (""))
						     (git . (""))
						     (bazaar . (""))
						     (subversion . (""))
						     (cvs . (""))))
					  (git-rm-cached . ((git . ("--cache"))))
					  (git-checkout . ((git . (""))))
					  (git-reset . ((git . (""))))
					  (rename . ((mercurial . (""))
						     (git . (""))
						     (bazaar . (""))
						     (subversion . (""))
						     (cvs . (""))))
					  (commit . ((mercurial . (""))
						     (git . (""))
						     (bazaar . (""))
						     (subversion . (""))
						     (cvs . (""))))
					  (status . ((mercurial . ("--all"
								   "."))
						     (git . (""))
						     (bazaar . ("--short"))
						     (subversion . ("--verbose"))
						     (cvs . (""))))
					  (log . ((mercurial . ("--limit=32"
								"--verbose"))
						  (git . ("-32"))
						  (bazaar . ("--limit=32"
							     "--verbose"))
						  (subversion . ("--limit=32"
								 "--verbose"))
						  (cvs . ("")))))
  "mvc-default-option-list-user"
  :type '(list (cons (const diff) (list
				   (cons (const mercurial) (repeat string))
				   (cons (const git) (repeat string))
				   (cons (const bazaar) (repeat string))
				   (cons (const subversion) (repeat string))
				   (cons (const cvs) (repeat string))))
	       (cons (const add) (list
				  (cons (const mercurial) (repeat string))
				  (cons (const git) (repeat string))
				  (cons (const bazaar) (repeat string))
				  (cons (const subversion) (repeat string))
				  (cons (const cvs) (repeat string))))
	       (cons (const annotate) (list
				       (cons (const mercurial) (repeat string))
				       (cons (const git) (repeat string))
				       (cons (const bazaar) (repeat string))
				       (cons (const subversion) (repeat string))
				       (cons (const cvs) (repeat string))))
	       (cons (const revert) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const remove) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const rename) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const commit) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const status) (list
				     (cons (const mercurial) (repeat string))
				     (cons (const git) (repeat string))
				     (cons (const bazaar) (repeat string))
				     (cons (const subversion) (repeat string))
				     (cons (const cvs) (repeat string))))
	       (cons (const log) (list
				  (cons (const mercurial) (repeat string))
				  (cons (const git) (repeat string))
				  (cons (const bazaar) (repeat string))
				  (cons (const subversion) (repeat string))
				  (cons (const cvs) (repeat string)))))
  :group 'mvc-variables)

(defcustom mvc-default-commit-message '((mercurial . "no comment\n")
					(git . "no comment\n")
					(bazaar . "no comment\n")
					(subversion . "")
					(cvs . ""))
  "mvc-default-commit-message"
  :type '(list (cons (const mercurial) string)
	       (cons (const git) string)
	       (cons (const bazaar) string)
	       (cons (const subversion) string)
	       (cons (const cvs) string))
  :group 'mvc-variables)

;; '((vcskey . (("表示用文字列"
;;               "説明文字列"
;;               function symbol))))
(defcustom mvc-default-especial-list
  '((mercurial . nil)
    (git . nil)
    (bazaar . nil)
    (subversion . (("   add svn:ignore"
		    "add mark file(s) to svn:ignore for current directory"
		    mvc-especial-mode-svn-add-ignore)
		   ("remove svn:ignore"
		    "remove mark file(s) from svn:ignore for current directory"
		    mvc-especial-mode-svn-remove-ignore)))
    (cvs . nil))
  "mvc-default-especial-list"
  :type '(list (cons (const mercurial) (repeat (list string string symbol)))
	       (cons (const git) (repeat (list string string symbol)))
	       (cons (const bazaar) (repeat (list string string symbol)))
	       (cons (const subversion) (repeat (list string string symbol)))
	       (cons (const cvs) (repeat (list string string symbol))))
  :group 'mvc-variables)

;; '((vcskey . ("path regexp 0"
;;              "path regexp 1"
;;              "path regexp N")))
;;
;; path regexp で指定された path では recursive flag を nil として
;; status バッファを起動します。ファイル数が多いワーキングコピーを指定
;; しておくと status の待ち時間が短かくなります。
;;
;; default-directory と比較するので、完全一致させる場合は末尾に "/" が
;; 必要なことに注意が必要です。
;;
;; なお recursive flag は以下の vcs とコマンドに対応しています。
;;
;;    - Subversion status
;;    - Subversion revert
;;    - Subversion commit
;;
;; 他の vcs に対しては表示制御用として使う?
(defcustom mvc-default-ignore-recursive-regexp-list
  '((mercurial . nil)
    (git . nil)
    (bazaar . nil)
    (subversion . ("^/tmp/recursive-ignore-test-0/$"
		   "^~/tmp/svntest/$"))
    (cvs . nil))
  "mvc-default-ignore-recursive-regexp-list"
  :type '(list (cons (const mercurial) (repeat string))
	       (cons (const git) (repeat string))
	       (cons (const bazaar) (repeat string))
	       (cons (const subversion) (repeat string))
	       (cons (const cvs) (repeat string)))
  :group 'mvc-variables)




(defface mvc-face-tab-active
  '((((type x w32 mac) (class color)) (:foreground "black" :background "bisque1" :bold t :box (:line-width 2 :style released-button))))
  "tab active"
  :group 'mvc-faces)
(defface mvc-face-tab-inactive
  '((((type x w32 mac) (class color)) (:foreground "black" :background "bisque4" :box (:line-width 2 :style released-button))))
  "tab inactive"
  :group 'mvc-faces)

(defface mvc-face-button-active
  '((((type x w32 mac) (class color)) (:foreground "black" :background "bisque1" :bold t :box (:line-width 2 :style released-button))))
  "button active"
  :group 'mvc-faces)
(defface mvc-face-button-inactive
  '((((type x w32 mac) (class color)) (:foreground "black" :background "bisque4" :box (:line-width 2 :style released-button))))
  "button inactive"
  :group 'mvc-faces)

(defface mvc-face-toggle-button-active
  '((((type x w32 mac) (class color)) (:foreground "black" :background "bisque3" :bold t :box (:line-width 3 :style pressed-button))))
  "toggle button active"
  :group 'mvc-faces)
(defface mvc-face-toggle-button-inactive
  '((((type x w32 mac) (class color)) (:foreground "black" :background "bisque2" :bold t :box (:line-width 3 :style released-button))))
  "toggle button inactive"
  :group 'mvc-faces)

(defface mvc-face-default-directory
  '((((type x w32 mac) (class color)) (:foreground "black" :background "bisque1" :bold t)))
  "default-directory"
  :group 'mvc-faces)

(defface mvc-face-status-footer
  '((((type x w32 mac) (class color)) (:foreground "black" :background "bisque1")))
  "status footer"
  :group 'mvc-faces)
(defface mvc-face-status-directory
  '((((type x w32 mac) (class color) (background light)) (:foreground "darkblue" :bold t))
    (((type x w32 mac) (class color) (background dark)) (:foreground "blue" :bold t)))
  "status directory"
  :group 'mvc-faces)
(defface mvc-face-status-modified
  '((((type x w32 mac) (class color) (background light)) (:foreground "darkgreen" :bold t))
    (((type x w32 mac) (class color) (background dark)) (:foreground "green" :bold t)))
  "status modified"
  :group 'mvc-faces)

(defface mvc-face-commit-headline
  '((((type x w32 mac) (class color) (background light)) (:foreground "darkblue" :bold t :underline t))
    (((type x w32 mac) (class color) (background dark)) (:foreground "blue" :bold t :underline t)))
  "commit headline"
  :group 'mvc-faces)
(defface mvc-face-commit-fatal
  '((((type x w32 mac) (class color)) (:foreground "red" :background "dimgray" :bold t :underline t)))
  "commit fatal"
  :group 'mvc-faces)
(defface mvc-face-commit-warning
  '((((type x w32 mac) (class color) (background light)) (:foreground "darkred" :bold t :underline t))
    (((type x w32 mac) (class color) (background dark)) (:foreground "red" :bold t :underline t)))
  "commit warning"
  :group 'mvc-faces)
(defface mvc-face-commit-information
  '((((type x w32 mac) (class color) (background light)) (:foreground "black" :bold t))
    (((type x w32 mac) (class color) (background dark)) (:foreground "white" :bold t)))
  "commit information"
  :group 'mvc-faces)

(defface mvc-face-animation-timer-0
  '((((type x w32 mac) (class color)) (:foreground "#000f0f" :background "lightgray" :bold t)))
  "animation timer 0"
  :group 'mvc-faces)
(defface mvc-face-animation-timer-1
  '((((type x w32 mac) (class color)) (:foreground "#003f00" :background "lightgray" :bold t)))
  "animation timer 1"
  :group 'mvc-faces)
(defface mvc-face-animation-timer-2
  '((((type x w32 mac) (class color)) (:foreground "#007f00" :background "lightgray" :bold t)))
  "animation timer 2"
  :group 'mvc-faces)
(defface mvc-face-animation-timer-3
  '((((type x w32 mac) (class color)) (:foreground "#003f00" :background "lightgray" :bold t)))
  "animation timer 3"
  :group 'mvc-faces)
(defface mvc-face-animation-timer-4
  '((((type x w32 mac) (class color)) (:foreground "#000f0f" :background "lightgray" :bold t)))
  "animation timer 4"
  :group 'mvc-faces)
(defface mvc-face-animation-timer-5
  '((((type x w32 mac) (class color)) (:foreground "#00007f" :background "lightgray" :bold t)))
  "animation timer 5"
  :group 'mvc-faces)
(defface mvc-face-animation-timer-6
  '((((type x w32 mac) (class color)) (:foreground "#0000ff" :background "lightgray" :bold t)))
  "animation timer 6"
  :group 'mvc-faces)
(defface mvc-face-animation-timer-7
  '((((type x w32 mac) (class color)) (:foreground "#00007f" :background "lightgray" :bold t)))
  "animation timer 7"
  :group 'mvc-faces)

(defface mvc-face-log-revision
  '((((type x w32 mac) (class color)) (:foreground "#00007f" :background "lightgray" :bold t :underline t)))
  "log revision"
  :group 'mvc-faces)

(defface mvc-face-especial-path
  '((((type x w32 mac) (class color) (background light)) (:foreground "darkblue" :bold t))
    (((type x w32 mac) (class color) (background dark)) (:foreground "blue" :bold t)))
  "especial path"
  :group 'mvc-faces)




(defconst mvc-program-display-name '((mercurial . "hg")
				     (git . "git")
				     (bazaar . "bazaar")
				     (subversion . "svn")
				     (cvs . "cvs")
				     "display name"))

(defconst mvc-program-name '((mercurial . "hg")
			     (git . "git")
			     (bazaar . "bzr")
			     (subversion . "svn")
			     (cvs . "cvs")
			     "program name"))

(defconst mvc-control-directory-name '((mercurial . ".hg")
				       (git . ".git")
				       (bazaar . ".bzr")
				       (subversion . ".svn")
				       (cvs . "CVS")
				       "control directory name"))

(defconst mvc-mode-name-status "mvc status")
(defconst mvc-mode-name-commitlog "mvc commitlog")
(defconst mvc-mode-name-log "mvc log")
(defconst mvc-mode-name-especial "mvc especial")
(defconst mvc-mode-name-especial-commit "mvc especial-commit")

(defconst mvc-running-header-decoration-0 "**")
(defconst mvc-running-header-decoration-1 "--")
(defconst mvc-running-header-running "running")
(defconst mvc-running-header-head (concat mvc-running-header-decoration-0 " "))
(defconst mvc-running-header-tail (concat " " mvc-running-header-running))
(defconst mvc-message-process-already-running "process already running!")
(defconst mvc-header-string-time "--- --- -- --:--:-- ----")




(defvar mvc-status-buffer-name-base-hash (make-hash-table :test 'equal))
(defvar mvc-status-buffer-list nil)

(defvar mvc-program-list-cache-hash (make-hash-table :test 'equal))




;;; mvc-status-mode-map

(setq mvc-status-mode-map (make-sparse-keymap))

(define-key mvc-status-mode-map "f" 'mvc-status-mode-find-file)
(define-key mvc-status-mode-map "n" 'mvc-status-mode-next)
(define-key mvc-status-mode-map "p" 'mvc-status-mode-previous)
(define-key mvc-status-mode-map "\C-n" 'mvc-status-mode-next)
(define-key mvc-status-mode-map "\C-p" 'mvc-status-mode-previous)
(define-key mvc-status-mode-map "\C-i" 'mvc-status-mode-next-button)
(define-key mvc-status-mode-map "\M-\C-i" 'mvc-status-mode-next-status)
(define-key mvc-status-mode-map (kbd "M-C-S-i") 'mvc-status-mode-previous-status)

(define-key mvc-status-mode-map "\C-c?" 'mvc-status-mode-toggle-display-unknown)
(define-key mvc-status-mode-map "\C-c_" 'mvc-status-mode-toggle-display-unmodified)
(define-key mvc-status-mode-map "\C-c~" 'mvc-status-mode-toggle-display-backup)
(define-key mvc-status-mode-map "\C-ci" 'mvc-status-mode-toggle-display-ignore)
(define-key mvc-status-mode-map "m" 'mvc-status-mode-mark)
(define-key mvc-status-mode-map "u" 'mvc-status-mode-unmark)
(define-key mvc-status-mode-map "*!" 'mvc-status-mode-unmark-all)
(define-key mvc-status-mode-map "*?" 'mvc-status-mode-mark-unknown)
(define-key mvc-status-mode-map "*A" 'mvc-status-mode-mark-add)
(define-key mvc-status-mode-map "*D" 'mvc-status-mode-mark-remove)
(define-key mvc-status-mode-map "*M" 'mvc-status-mode-mark-modified)
(define-key mvc-status-mode-map "*R" 'mvc-status-mode-mark-path-regexp)

(define-key mvc-status-mode-map "<" 'mvc-status-mode-pull)
(define-key mvc-status-mode-map ">" 'mvc-status-mode-push)
(define-key mvc-status-mode-map "\M-<" 'mvc-status-mode-beginning-of-list)
(define-key mvc-status-mode-map "\M->" 'mvc-status-mode-end-of-list)
(define-key mvc-status-mode-map "a" 'mvc-status-mode-add)
(define-key mvc-status-mode-map "b" 'mvc-status-mode-annotate)
(define-key mvc-status-mode-map "c" 'mvc-status-mode-commit)
(define-key mvc-status-mode-map "g" 'mvc-status-mode-status)
(define-key mvc-status-mode-map "l" 'mvc-status-mode-log)
(define-key mvc-status-mode-map "r" 'mvc-status-mode-revert)
(define-key mvc-status-mode-map "s" 'mvc-status-mode-status)
(define-key mvc-status-mode-map "D" 'mvc-status-mode-remove)
(define-key mvc-status-mode-map "Q" 'mvc-status-mode-quit)
(define-key mvc-status-mode-map "R" 'mvc-status-mode-rename)
(define-key mvc-status-mode-map "U" 'mvc-status-mode-update)
(define-key mvc-status-mode-map "\M-u" 'mvc-status-mode-update)
(define-key mvc-status-mode-map "=" 'mvc-status-mode-diff-only-current)
(define-key mvc-status-mode-map (kbd "C-=") 'mvc-status-mode-diff-current-or-mark)
(define-key mvc-status-mode-map "!" 'mvc-status-mode-especial)




;;; mvc-commitlog-mode-map

(setq mvc-commitlog-mode-map (make-sparse-keymap))

(define-key mvc-commitlog-mode-map "\C-c\C-c" 'mvc-commitlog-mode-done)




;;; mvc-log-mode-map

(setq mvc-log-mode-map (make-sparse-keymap))

(define-key mvc-log-mode-map "\C-i" 'mvc-log-mode-next)
(define-key mvc-log-mode-map "n" 'mvc-log-mode-next)
(define-key mvc-log-mode-map "p" 'mvc-log-mode-previous)
(define-key mvc-log-mode-map " " 'scroll-up)
(define-key mvc-log-mode-map "\C-m" 'mvc-log-mode-return)




;;; mvc-especial-mode-map

(setq mvc-especial-mode-map (make-sparse-keymap))

(define-key mvc-especial-mode-map "\C-i" 'mvc-especial-mode-next)
(define-key mvc-especial-mode-map "g" 'mvc-especial-mode-draw)
(define-key mvc-especial-mode-map "n" 'mvc-especial-mode-next)
(define-key mvc-especial-mode-map "p" 'mvc-especial-mode-previous)
(define-key mvc-especial-mode-map "s" 'mvc-especial-mode-draw)




;;; mvc-especial-commit-mode-map

(setq mvc-especial-commit-mode-map (make-sparse-keymap))

(define-key mvc-especial-commit-mode-map "\C-c\C-c" 'mvc-especial-commit-mode-done)




;;; mvc utilities

(defun mvc-search-control-directory (program)
  (catch 'outer
    (let ((current-dir (expand-file-name "."))
	  (control-dir (cdr (assq program mvc-control-directory-name)))
	  full-path-dir)
      (while t
	(when (file-symlink-p current-dir)
	  (setq current-dir (file-symlink-p current-dir)))
	(setq full-path-dir (concat current-dir "/" control-dir))
	(if (file-directory-p full-path-dir)
	    (progn
	      (throw 'outer full-path-dir))
	  (if (string-match "\\(.+\\)/.+" current-dir)
	      (setq current-dir (match-string 1 current-dir))
	    (throw 'outer nil)))))))


(defun mvc-clear-program-list-cache (clear-all-p)
  "バージョンコントロールプログラムに関するキャッシュをクリアします。
clear-all が nil ならばカレントディレクトリのキャッシュをクリアし、
Non-nil ならば全てのキャッシュをクリアします。
mvc-is-exist-program-hash は常にクリアされます。"
  (if clear-all-p
      (clrhash mvc-program-list-cache-hash)
    (puthash (expand-file-name default-directory) nil mvc-program-list-cache-hash)))


(defun mvc-get-current-program-list ()
  "カレントディレクトリのバージョンコントロールプログラムに対応す
るシンボルのリストを返します。プログラムは
mvc-default-program-search-order-list の順に探索されますが、
mvc-default-program-search-concurrent が nil ならば最初の 1 つが見
付かった時点で探索を打ち切ります。プログラムが不明な場合は nil を
返します。以下のプログラムを認識します。

- 'mercurial
- 'git
- 'bazaar
- 'subversion
- 'cvs

初回の呼び出しはディレクトリをたどるため、少しだけ時間がかかること
がありますが、この結果はキャッシュされるため、次以降の呼び出しは高
速に実行されます。キャッシュは関数 mvc-clear-program-list-cache で
クリアすることができます。
"
  (let ((name-list (gethash (expand-file-name default-directory) mvc-program-list-cache-hash)))
    (unless name-list
      (catch 'found
	(mapcar #'(lambda (program)
		    (cond ((eq program 'mercurial)
			   (when (mvc-search-control-directory 'mercurial)
			     (setq name-list (append name-list (list 'mercurial)))
			     (unless mvc-default-program-search-concurrent
			       (throw 'found nil))))
			  ((eq program 'git)
			   (when (mvc-search-control-directory 'git)
			     (setq name-list (append name-list (list 'git)))
			     (unless mvc-default-program-search-concurrent
			       (throw 'found nil))))
			  ((eq program 'bazaar)
			   (when (mvc-search-control-directory 'bazaar)
			     (setq name-list (append name-list (list 'bazaar)))
			     (unless mvc-default-program-search-concurrent
			       (throw 'found nil))))
			  ((eq program 'subversion)
			   (when (file-exists-p ".svn")
			     (setq name-list (append name-list (list 'subversion)))
			     (unless mvc-default-program-search-concurrent
			       (throw 'found nil))))
			  ((eq program 'cvs)
			   (when (file-exists-p "CVS")
			     (setq name-list (append name-list (list 'cvs)))
			     (unless mvc-default-program-search-concurrent
			       (throw 'found nil))))
			  (t
			   (error "Illegal mvc-default-program-search-order-list"))))
		mvc-default-program-search-order-list))
      (puthash (expand-file-name default-directory) name-list mvc-program-list-cache-hash))
    name-list))


(defun mvc-create-buffer-name (prefix initial-directory)
  (let* ((key (expand-file-name initial-directory))
	 (base (gethash key mvc-status-buffer-name-base-hash)))
    (unless base
      (string-match "/\\([^/]+\\)/$" key)
      (setq base (match-string 1 key))
      (setq base (substring (generate-new-buffer-name (concat prefix base)) (length prefix)))
      (puthash key base mvc-status-buffer-name-base-hash))
    (concat prefix base)))


(defun mvc-call-process-temporary (status-buffer
				   program
				   option-list
				   directory)
  (let (temporary-process-buffer-name)
    (with-current-buffer status-buffer
      (setq temporary-process-buffer-name (cdr (assq 'process-temporary mvc-local-buffer-name-list))))
    (set-buffer (get-buffer-create temporary-process-buffer-name))
    (setq default-directory directory)
    (setq buffer-undo-list t)
    (setq buffer-read-only nil)
    (erase-buffer)
    (apply 'call-process program nil t nil option-list)))


(defun mvc-show-call-process-temporary-result (status-buffer &optional buffer-active-p)
  (let (result-buffer
	temporary-process-buffer-name)
    (with-current-buffer status-buffer
      (setq result-buffer (get-buffer-create (cdr (assq 'result mvc-local-buffer-name-list))))
      (setq temporary-process-buffer-name (cdr (assq 'process-temporary mvc-local-buffer-name-list))))
    (unless (get-buffer-window result-buffer)
      (split-window))
    (if buffer-active-p
	(pop-to-buffer result-buffer)
      (display-buffer result-buffer))
    (with-current-buffer result-buffer
      (setq buffer-undo-list t)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-buffer-substring temporary-process-buffer-name)
      (setq buffer-read-only t))))


(defun mvc-get-ls-current-list (status-buffer)
  (let (current-list)
    (save-current-buffer
      (mvc-call-process-temporary status-buffer
				  "ls"
				  (list "-a" "-l") ; POSIX options
				  default-directory)
      (goto-char (point-min))
      (while (not (eobp))
	;; mode,dirent,user,group,size,mon,day,time,name
	(let ((line (buffer-substring (point) (progn
						(end-of-line)
						(point)))))
	  (when (string-match "\\([^ ]+\\) +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +\\(.+\\)" line)
	    (let ((mode (match-string 1 line))
		  (name (match-string 2 line)))
	      (unless (and (eq (compare-strings mode nil 1 "d" nil 1)t )
			   (or (string= name ".svn")
			       (string= name "CVS")
			       (string= name ".hg")
			       (string= name ".git")
			       (string= name ".bzr")
			       (string= name ".")
			       (string= name "..")))
		(setq current-list (append current-list (list line))))))
	  (forward-line)))
      current-list)))

(defun mvc-get-ls-parameter-file-list-core (list current-dir dir-hash)
  (mapcar #'(lambda (line)
	      (string-match "\\([^ ]+\\) +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +\\(.+\\)" line)
	      (let ((line (match-string 0 line))
		    (mode (match-string 1 line))
		    (name (match-string 2 line))
		    (prefix-path current-dir))
		(when (> (length prefix-path) 0)
		  (setq prefix-path (concat prefix-path "/")))
		(if (eq (compare-strings mode nil 1 "d" nil 1) t)
		    (progn
		      (let ((new-current-dir name))
			(when (> (length current-dir) 0)
			  (setq new-current-dir (concat current-dir "/" new-current-dir)))
			(setq parameter-file-list (append parameter-file-list (list (concat prefix-path name "/"))))
			(mvc-get-ls-parameter-file-list-core (gethash new-current-dir dir-hash)
							     new-current-dir
							     dir-hash)))
		  (setq parameter-file-list (append parameter-file-list (list (concat prefix-path name)))))))
	  list)
  nil)

;; dynamic scope な変数 "parameter-file-list" を更新することに注意
(defun mvc-get-ls-parameter-file-list (status-buffer)
  (let ((current-list (mvc-get-ls-current-list status-buffer)))
    (save-current-buffer
      (let (current-name-list)
	(mapcar #'(lambda (line)
		    (string-match "\\([^ ]+\\) +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +\\(.+\\)" line)
		    (setq current-name-list (append current-name-list (list (match-string 2 line)))))
		current-list)
	(mvc-call-process-temporary status-buffer
				    "ls"
				    (append (list "-R" "-a" "-l") ; POSIX options
					    current-name-list)
				    default-directory))

      (goto-char (point-min))
      (let (state
	    dir
	    (dir-hash (make-hash-table :test 'equal)))
	(while (not (eobp))
	  (when (looking-at "^$")
	    (setq state 'before-space))
	  (cond ((eq state 'before-space)
		 (when (looking-at "\\(.+\\):$")
		   (setq state 'dir-total)
		   (setq dir (match-string 1))))
		((eq state 'dir-total)
		 (setq state 'dir))	; skip "total \d+"
		((eq state 'dir)
		 (when (looking-at "\\([^ ]+\\) +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +\\(.+\\)")
		   (let ((line (match-string 0))
			 (mode (match-string 1))
			 (name (match-string 2)))
		     (unless (and (eq (compare-strings mode nil 1 "d" nil 1) t)
				  (or (string= name ".")
				      (string= name "..")))
		       (puthash dir (append (gethash dir dir-hash) (list line)) dir-hash))))))
	  (forward-line))
	
	(mvc-get-ls-parameter-file-list-core current-list
					     ""
					     dir-hash))))
  nil)


(defun mvc-insert-with-face (string face)
  (if mvc-default-use-color
      (let ((start (point)))
	(insert string)
	(set-text-properties start
			     (+ start (length string))
			     (list 'face face)))
    (insert string)))




;;; mvc-status

(defun mvc-status ()
  (interactive)

  (let (initial-directory
	initial-program-list)
    (if mvc-default-read-directory
	(progn
	  (catch 'loop
	    (while t
	      (setq initial-directory (read-file-name "directory: " nil default-directory))
	      (if (file-directory-p initial-directory)
		  (throw 'loop nil)
		(message (concat "\"" initial-directory "\" is not directory"))
		(sit-for 3))))
	  (when (not (string-match "/$" initial-directory))
	    (setq initial-directory (concat initial-directory "/"))))
      (setq initial-directory default-directory))

    (let ((default-directory initial-directory))
      (setq initial-program-list (mvc-get-current-program-list)))

    (if initial-program-list
	(let (first-buffer)
	  (mapcar #'(lambda (program)
		      (let ((buffer (mvc-create-buffer-name (concat "*mvc-"
								    (cdr (assq program mvc-program-display-name))
								    "-status*")
							    initial-directory)))
			(if (get-buffer buffer)
			    (progn
			      (set-buffer buffer)
			      (mvc-async-status))
			  (set-buffer (get-buffer-create buffer))
			  (setq default-directory initial-directory)
			  (setq mvc-status-buffer-list (cons (current-buffer) mvc-status-buffer-list))
			  (mvc-async-status program)))
		      (unless first-buffer
			(setq first-buffer (current-buffer))))
		  initial-program-list)
	  (switch-to-buffer first-buffer))
      (message "mvc.el : UNKNOWN VCS!"))))


(defun mvc-status-get-current-program-option-list-base (command)
  (let (result)
    (mapcar #'(lambda (a)
		(when (not (and (stringp a)
				(string= a "")))
		  (setq result (append result (list a)))))
	    (append (cdr (assq mvc-local-program
			       (cdr (assq command mvc-default-option-list-system))))
		    (cdr (assq mvc-local-program
			       (cdr (assq command mvc-default-option-list-user))))))
    (when (eq mvc-local-program 'git)
      (setq result (append result (list "--"))))
    result))

(defun mvc-status-get-current-program-option-list-revert (option-list)
  (cond ((or (eq mvc-local-program 'mercurial)
	     (eq mvc-local-program 'git)
	     (eq mvc-local-program 'bazaar))
	 option-list)
	((eq mvc-local-program 'subversion)
	 (when mvc-local-recursive-p
	   (setq option-list (append option-list (list "--recursive"))))
	 option-list)
	((eq mvc-local-program 'cvs)
	 (message "UNSUPPORTED")
	 nil)
	(t
	 (message "UNKNOWN PROGRAM!")
	 nil)))

(defun mvc-status-get-current-program-option-list-log (option-list prefix-argument)
  (cond ((or (eq mvc-local-program 'mercurial)
	     (eq mvc-local-program 'subversion)
	     (eq mvc-local-program 'bazaar))
	 (let (result)
	   (if prefix-argument
	       (let ((limit (prefix-numeric-value prefix-argument)))
		 (mapcar #'(lambda (a)
			     (setq result (append result (if (string-match "^--limit=[0-9]+$" a)
							     (list (format "--limit=%d" limit))
							   (list a)))))
			 option-list))
	     (setq result option-list))
	   result))
	((eq mvc-local-program 'git)
	 (let (result)
	   (if prefix-argument
	       (let ((limit (prefix-numeric-value prefix-argument)))
		 (mapcar #'(lambda (a)
			     (setq result (append result (if (string-match "^-[0-9]+$" a)
							     (list (format "-=%d" limit))
							   (list a)))))
			 option-list))
	     (setq result option-list))
	   result))
	((eq mvc-local-program 'cvs)
	 (message "UNSUPPORTED")
	 nil)
	(t
	 (message "UNKNOWN PROGRAM!")
	 nil)))

(defun mvc-status-get-current-program-option-list-diff (option-list prefix-argument)
  (if prefix-argument
      (cond ((eq mvc-local-program 'mercurial)
	     (append option-list (list (concat "--rev="
					       (completing-read
						"--rev: "
						'("PREV" "HEAD")
						nil
						nil
						"")))))
	    ((eq mvc-local-program 'git)
	     option-list)
	    ((eq mvc-local-program 'bazaar)
	     (append option-list (list (concat "--revision="
					       (completing-read
						"--revision: "
						'("PREV" "HEAD")
						nil
						nil
						"PREV")))))
	    ((eq mvc-local-program 'subversion)
	     (append option-list (list (concat "--revision="
					       (completing-read
						"--revision: "
						'("PREV" "HEAD")
						nil
						nil
						"PREV")))))
	    ((eq mvc-local-program 'cvs)
	     (append option-list (list (concat "--revision="
					       (completing-read
						"--revision: "
						'("PREV" "HEAD")
						nil
						nil
						"PREV")))))
	    (t
	     (message "UNKNOWN PROGRAM!")
	     nil))
    option-list))

(defun mvc-status-get-current-program-option-list-commit (option-list temporary-file-name)
  (cond ((eq mvc-local-program 'mercurial)
	 (append option-list (list (concat "--logfile=" temporary-file-name))))
	((eq mvc-local-program 'git)
	 (let (result)
	   (mapcar #'(lambda (a)
		       (if (string= a "--")
			   (setq result (append result (list (concat "--file=" temporary-file-name) a)))
			 (setq result (append result (list a)))))
		   option-list)
	   result))
	((eq mvc-local-program 'bazaar)
	 (append option-list (list (concat "--file=" temporary-file-name))))
	((eq mvc-local-program 'subversion)
	 (setq option-list (append option-list (list (concat "--file=" temporary-file-name))))
	 (unless mvc-local-recursive-p
	   (setq option-list (append option-list (list "--non-recursive"))))
	 option-list)
	((eq mvc-local-program 'cvs)
	 (message "UNSUPPORTED")
	 nil)
	(t
	 (message "UNKNOWN PROGRAM!")
	 nil)))

(defun mvc-status-get-current-program-option-list-status (option-list)
  (cond ((eq mvc-local-program 'subversion)
	 (if mvc-local-recursive-p
	     option-list
	   ;; (append option-list (list "--depth=immediates"))
	   (append option-list (list "--non-recursive"))))
	((or (eq mvc-local-program 'mercurial)
	     (eq mvc-local-program 'git)
	     (eq mvc-local-program 'bazaar)
	     (eq mvc-local-program 'cvs))
	 option-list)
	(t
	 (message "UNKNOWN PROGRAM!")
	 nil)))

(defun mvc-status-get-current-program-option-list (command &optional argument)
  "command に対応する option-list を必要ならば argument に従い書き換えて返します。
対応しない引数や組み合わせの場合は nil を返します。

認識する command は以下のとおりです。

* argument を共なわないもの
'add
'annotate
'revert
'remove
'git-rm-cached
'git-checkout
'git-reset
'rename
'status

* argument を共なうもの
'log (argument(prefix-argument):表示数)
'diff (argument(prefix-argument):Non-nil の場合内部で completing-read することに注意)
'commit (argument(string):テンポラリログファイル名)"
  (let ((option-list (mvc-status-get-current-program-option-list-base command)))
    (cond ((eq command 'revert)
	   (mvc-status-get-current-program-option-list-revert option-list))
	  ((eq command 'log)
	   (mvc-status-get-current-program-option-list-log option-list argument))
	  ((eq command 'diff)
	   (mvc-status-get-current-program-option-list-diff option-list argument))
	  ((eq command 'commit)
	   (mvc-status-get-current-program-option-list-commit option-list argument))
	  ((eq command 'status)
	   (mvc-status-get-current-program-option-list-status option-list))
	  (t
	   option-list))))


(defun mvc-status-insert-button (label function)
  (let ((map (copy-keymap mvc-status-mode-map))
	(start (point)))
    (define-key map [mouse-1] function)
    (define-key map "\C-m" function)
    (mvc-insert-with-face label
			  'mvc-face-button-active)
    (put-text-property start
		       (point)
		       'local-map map)))


(defun mvc-status-insert-toggle-button (flag label-true label-false function)
  (let ((map (copy-keymap mvc-status-mode-map))
	(start (point)))
    (define-key map [mouse-1] function)
    (define-key map "\C-m" function)
    (if flag
	(mvc-insert-with-face label-true
			      'mvc-face-toggle-button-active)
      (mvc-insert-with-face label-false
			    'mvc-face-toggle-button-inactive))
    (put-text-property start
		       (point)
		       'local-map map)))


(defun mvc-status-update-header-line ()
  (mapcar #'(lambda (b)
	      (with-current-buffer b
		(setq header-line-format nil)
		(mapcar #'(lambda (a)
			    (let ((map (make-sparse-keymap))
				  tmp)
			      (funcall 'define-key map [header-line mouse-1]
				       `(lambda ()
					  (interactive)
					  (switch-to-buffer ,a)))
			      (with-current-buffer a
				(setq tmp (substring (buffer-name a) (length (cdr (assq 'status-base mvc-local-buffer-name-list))))))
			      (set-text-properties 0
						   (length tmp)
						   (list 'face
							 (if (eq a b)
							     'mvc-face-tab-active
							   'mvc-face-tab-inactive)
							 'local-map
							 map)
						   tmp)
			      (setq header-line-format (cons tmp header-line-format))
			      (setq header-line-format (cons " " header-line-format))))
			mvc-status-buffer-list)))
	  mvc-status-buffer-list))


(defun mvc-status-kill-buffer-hook ()
  (when (member (current-buffer) mvc-status-buffer-list)
    (setq mvc-status-buffer-list (delete (current-buffer) mvc-status-buffer-list))
    (puthash (expand-file-name default-directory) nil mvc-status-buffer-name-base-hash)
    (mvc-status-update-header-line)

    (when mvc-default-use-animation-timer
      (cancel-timer mvc-local-timer)
      (setq mvc-local-timer nil))
    (when mvc-local-async-process
      (delete-process (cdr (assq 'process-async mvc-local-buffer-name-list)))
      (setq mvc-local-async-process nil))))


(defun mvc-status-timer-function (buffer)
  (when (buffer-name buffer)
    (with-current-buffer buffer
      (if mvc-local-async-process
	  (let (decoration
		(tmp (concat mvc-running-header-running))
		(i (% (lsh mvc-local-timer-counter -1) (length mvc-running-header-running))))
	    (setq decoration (if (= (logand 1 i) 0)
				 mvc-running-header-decoration-0
			       mvc-running-header-decoration-1))
	    (store-substring tmp
			     i
			     (substring (upcase tmp) i (+ i 1)))
	    (save-excursion
	      (goto-char (point-min))
	      (let ((start (point)) (backup-buffer-read-only buffer-read-only) string)
		(end-of-line)
		(setq string (concat decoration
				     " " mvc-local-program-name " "
				     mvc-local-async-process-last-command
				     " "
				     tmp))
		(setq buffer-read-only nil)
		(delete-region start (point))
		(mvc-insert-with-face string
				      (nth (% mvc-local-timer-counter 8)
					   '(mvc-face-animation-timer-0
					     mvc-face-animation-timer-1
					     mvc-face-animation-timer-2
					     mvc-face-animation-timer-3
					     mvc-face-animation-timer-4
					     mvc-face-animation-timer-5
					     mvc-face-animation-timer-6
					     mvc-face-animation-timer-7)))
		(setq buffer-read-only backup-buffer-read-only)))
	    (let ((mode-line-string (concat ": "
					    decoration
					    " "
					    mvc-local-async-process-last-command
					    " "
					    tmp
					    " "
					    decoration)))
	      (unless (string= mvc-local-timer-last-mode-line-string mode-line-string)
		(setq mvc-local-timer-last-mode-line-string mode-line-string)
		(setq mode-line-process mode-line-string)
		(force-mode-line-update)))
	    (setq mvc-local-timer-counter (1+ mvc-local-timer-counter))
	    (when (> mvc-local-timer-counter 65535)
	      (setq mvc-local-timer-counter 0)))
	(when mode-line-process
	  (setq mvc-local-timer-counter 0)
	  (setq mode-line-process nil)
	  (force-mode-line-update))))))


(defun mvc-status-beginning-of-list ()
  (goto-char mvc-local-file-list-begin-point))

(defun mvc-status-end-of-list ()
  (goto-char mvc-local-file-list-end-point)
  (beginning-of-line))


;; 現在のバッファのカレント行を文字列として返します。無効な行ならば
;; nil を返します。
(defun mvc-status-get-current-line-string ()
  (if mvc-local-ready-p
      (save-excursion
	(let (current-start)
	  (beginning-of-line)
	  (setq current-start (point))
	  (if (or (< current-start mvc-local-file-list-begin-point)
		  (> current-start mvc-local-file-list-end-point))
	      nil
	    (goto-char current-start)
	    (end-of-line)
	    (buffer-substring current-start (point)))))
    nil))

(defun mvc-status-get-current-line-file-name ()
  (let ((file-name (mvc-status-get-current-line-string)))
    (if file-name
	(progn
	  (cond ((eq mvc-local-program 'mercurial)
		 (setq file-name (substring file-name 7)))
		((eq mvc-local-program 'git)
		 (setq file-name (substring file-name 7)))
		((eq mvc-local-program 'bazaar)
		 (setq file-name (substring file-name 7)))
		((eq mvc-local-program 'subversion)
		 (setq file-name (substring file-name 36)))
		((eq mvc-local-program 'cvs)
		 (message "CVS FIXME!")))
	  file-name)
      nil)))


(defun mvc-status-redraw-footer-marks ()
  (save-excursion
    (goto-char mvc-local-file-list-end-point)
    (search-forward "marks:" nil t)
    (setq buffer-read-only nil)
    (delete-region (point) (+ (point) 5))
    (mvc-insert-with-face (format "%-5d" mvc-local-marks)
			  'mvc-face-status-footer)
    (setq buffer-read-only t)))


(defun mvc-status-draw-mercurial ()
  (let ((expanded-dir (expand-file-name default-directory)))
    (maphash #'(lambda (key value)
		 (let ((flag t) (mark (gethash key mvc-local-mark-hash)))
		   (unless (string= mark "*")
		     (cond ((and (not mvc-local-display-unknown-p)
				 (string= value "?"))
			    (setq mvc-local-display-unknown-masks (1+ mvc-local-display-unknown-masks))
			    (setq flag nil))
			   ((and (not mvc-local-display-ignore-p)
				 (string= value "I"))
			    (setq mvc-local-display-ignore-masks (1+ mvc-local-display-ignore-masks))
			    (setq flag nil))
			   ((and (not mvc-local-display-unmodified-p)
				 (not (stringp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash)))
				 (or (string= value "C")
				     (and (string= value "d")
					  (not (string= key ".")))))
			    (setq mvc-local-display-unmodified-masks (1+ mvc-local-display-unmodified-masks))
			    (setq flag nil))))
		   (when (and (not mvc-local-display-backup-p)
			      (string-match "[~#]$" key)
			      (string= value "?")
			      (not (string= mark "*")))
		     (setq mvc-local-display-backup-masks (1+ mvc-local-display-backup-masks))
		     (setq flag nil))
		   (when flag
		     (puthash key (point) mvc-local-point-hash)
		     (insert (cond ((not mark) "  ")
				   ((string= mark "*") "* ")))

		     (insert " ")
		     (let ((tmp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash)))
		       (if (stringp tmp)
			   (mvc-insert-with-face tmp 'mvc-face-status-modified)
			 (mvc-insert-with-face " " 'mvc-face-status-modified)))

		     (cond ((string= "d" value)
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-directory))
			   ((string= "M" value)
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-modified))
			   ((stringp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash))
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-modified))
			   (t
			    (insert (concat " " value " " key "\n")))))))
	     mvc-local-code-hash)))

(defun mvc-status-draw-git ()
  (let ((expanded-dir (expand-file-name default-directory)))
    (maphash #'(lambda (key value)
		 (let ((flag t) (mark (gethash key mvc-local-mark-hash)))
		   (unless (string= mark "*")
		     (cond ((and (not mvc-local-display-unknown-p)
				 (string= value "?"))
			    (setq mvc-local-display-unknown-masks (1+ mvc-local-display-unknown-masks))
			    (setq flag nil))
			   ((and (not mvc-local-display-ignore-p)
				 (string= value "I"))
			    (setq mvc-local-display-ignore-masks (1+ mvc-local-display-ignore-masks))
			    (setq flag nil))
			   ((and (not mvc-local-display-unmodified-p)
				 (not (stringp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash)))
				 (or (string= value " ")
				     (and (string= value "d")
					  (not (string= key ".")))))
			    (setq mvc-local-display-unmodified-masks (1+ mvc-local-display-unmodified-masks))
			    (setq flag nil))))
		   (when (and (not mvc-local-display-backup-p)
			      (string-match "[~#]$" key)
			      (string= value "?")
			      (not (string= mark "*")))
		     (setq mvc-local-display-backup-masks (1+ mvc-local-display-backup-masks))
		     (setq flag nil))
		   (when flag
		     (puthash key (point) mvc-local-point-hash)
		     (insert (cond ((not mark) "  ")
				   ((string= mark "*") "* ")))

		     (insert " ")
		     (let ((tmp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash)))
		       (if (stringp tmp)
			   (mvc-insert-with-face tmp 'mvc-face-status-modified)
			 (mvc-insert-with-face " " 'mvc-face-status-modified)))

		     (cond ((string= "d" value)
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-directory))
			   ((string= "M" value)
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-modified))
			   ((stringp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash))
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-modified))
			   (t
			    (insert (concat " " value " " key "\n")))))))
	     mvc-local-code-hash)))

(defun mvc-status-draw-bazaar ()
  (let ((expanded-dir (expand-file-name default-directory)))
    (maphash #'(lambda (key value)
		 (let ((flag t) (mark (gethash key mvc-local-mark-hash)))
		   (unless (string= mark "*")
		     (cond ((and (not mvc-local-display-unknown-p)
				 (string= value "?"))
			    (setq mvc-local-display-unknown-masks (1+ mvc-local-display-unknown-masks))
			    (setq flag nil))
			   ((and (not mvc-local-display-ignore-p)
				 (string= value "I"))
			    (setq mvc-local-display-ignore-masks (1+ mvc-local-display-ignore-masks))
			    (setq flag nil))
			   ((and (not mvc-local-display-unmodified-p)
				 (not (stringp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash)))
				 (or (string= value " ")
				     (and (string= value "d")
					  (not (string= key ".")))))
			    (setq mvc-local-display-unmodified-masks (1+ mvc-local-display-unmodified-masks))
			    (setq flag nil))))
		   (when (and (not mvc-local-display-backup-p)
			      (string-match "[~#]$" key)
			      (string= value "?")
			      (not (string= mark "*")))
		     (setq mvc-local-display-backup-masks (1+ mvc-local-display-backup-masks))
		     (setq flag nil))
		   (when flag
		     (puthash key (point) mvc-local-point-hash)
		     (insert (cond ((not mark) "  ")
				   ((string= mark "*") "* ")))

		     (insert " ")
		     (let ((tmp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash)))
		       (if (stringp tmp)
			   (mvc-insert-with-face tmp 'mvc-face-status-modified)
			 (mvc-insert-with-face " " 'mvc-face-status-modified)))

		     (cond ((string= "d" value)
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-directory))
			   ((string= "M" value)
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-modified))
			   ((stringp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash))
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-modified))
			   (t
			    (insert (concat " " value " " key "\n")))))))
	     mvc-local-code-hash)))

(defun mvc-status-draw-subversion ()
  (let ((expanded-dir (expand-file-name default-directory)))
    (maphash #'(lambda (key value)
		 (let ((flag t) (mark (gethash key mvc-local-mark-hash)))
		   (unless (string= mark "*")
		     (cond ((and (not mvc-local-display-unknown-p)
				 (string= value "?"))
			    (setq mvc-local-display-unknown-masks (1+ mvc-local-display-unknown-masks))
			    (setq flag nil))
			   ((and (not mvc-local-display-unmodified-p)
				 (not (stringp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash)))
				 (string= value " ")
				 (not (string= key ".")))
			    (setq mvc-local-display-unmodified-masks (1+ mvc-local-display-unmodified-masks))
			    (setq flag nil))))
		   (when (and (not mvc-local-display-backup-p)
			      (string-match "[~#]$" key)
			      (string= value "?")
			      (not (string= mark "*")))
		     (setq mvc-local-display-backup-masks (1+ mvc-local-display-backup-masks))
		     (setq flag nil))
		   (when flag
		     (puthash key (point) mvc-local-point-hash)
		     (insert (cond ((not mark) "  ")
				   ((string= mark "*") "* ")))

		     (insert (gethash key mvc-local-information-hash))

		     (let ((tmp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash)))
		       (if (stringp tmp)
			   (mvc-insert-with-face tmp 'mvc-face-status-modified)
			 (mvc-insert-with-face " " 'mvc-face-status-modified)))

		     (cond ((or (string-match "/$" key)
				(string= "." key))
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-directory))
			   ((string= "M" value)
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-modified))
			   ((stringp (gethash (concat expanded-dir key) mvc-local-after-save-hook-hash))
			    (mvc-insert-with-face (concat " " value " " key "\n")
						  'mvc-face-status-modified))
			   (t
			    (insert (concat " " value " " key "\n")))))))
	     mvc-local-code-hash)))

(defun mvc-status-draw-cvs ()
  )

;; hash の内容からステータスバッファを描画します。
(defun mvc-status-draw ()
  (setq mvc-local-ready-p nil)

  (setq mvc-local-display-unknown-masks 0)
  (setq mvc-local-display-unmodified-masks 0)
  (setq mvc-local-display-backup-masks 0)
  (setq mvc-local-display-ignore-masks 0)

  (setq buffer-read-only nil)
  (erase-buffer)
  (clrhash mvc-local-point-hash)
  (mvc-insert-with-face (concat (expand-file-name default-directory) "\n")
			'mvc-face-default-directory)

  (setq mvc-local-file-list-begin-point (point))

  (cond ((eq mvc-local-program 'mercurial)
	 (mvc-status-draw-mercurial))
	((eq mvc-local-program 'git)
	 (mvc-status-draw-git))
	((eq mvc-local-program 'bazaar)
	 (mvc-status-draw-bazaar))
	((eq mvc-local-program 'subversion)
	 (mvc-status-draw-subversion))
	((eq mvc-local-program 'cvs)
	 (mvc-status-draw-cvs))
	(t
	 (message "UNKNOWN PROGRAM!")))

  (setq mvc-local-file-list-end-point (1- (point)))

  (mvc-insert-with-face (format "files:%-5d  marks:%-5d " mvc-local-files mvc-local-marks)
			'mvc-face-status-footer)
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)

  (mvc-insert-with-face "mask file(s):"
			'mvc-face-status-footer)
  (mvc-insert-with-face (if mvc-local-display-unknown-p
			    "?=show all   "
			  (format "?=HIDE %-5d " mvc-local-display-unknown-masks))
			'mvc-face-status-footer)
  (mvc-insert-with-face (if mvc-local-display-unmodified-p
			    "C=show all   "
			  (format "C=HIDE %-5d " mvc-local-display-unmodified-masks))
			'mvc-face-status-footer)
  (mvc-insert-with-face (if mvc-local-display-backup-p
			    "~#=show all   "
			  (format "~#=HIDE %-5d " mvc-local-display-backup-masks))
			'mvc-face-status-footer)
  (mvc-insert-with-face (if mvc-local-display-ignore-p
			    "I=show all   "
			  (format "I=HIDE %-5d " mvc-local-display-ignore-masks))
			'mvc-face-status-footer)
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)

  (mvc-status-insert-button "<=(M-S-TAB)" 'mvc-status-mode-previous-status)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "=>(M-TAB)" 'mvc-status-mode-next-status)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "push(>)" 'mvc-status-mode-push)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "pull(<)" 'mvc-status-mode-pull)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "status(s or g)" 'mvc-status-mode-status)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "update(U)" 'mvc-status-mode-update)
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)

  (mvc-status-insert-button "unmark all(*!)" 'mvc-status-mode-unmark-all)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "mark modified(*M)" 'mvc-status-mode-mark-modified)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "mark add(*A)" 'mvc-status-mode-mark-add)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "mark unknown(*?)" 'mvc-status-mode-mark-unknown)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "mark remove(*D)" 'mvc-status-mode-mark-remove)
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)

  (mvc-status-insert-button "  mark path regexp(*R)" 'mvc-status-mode-mark-path-regexp)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-button "unmark path regexp(*R)" 'mvc-status-mode-unmark-path-regexp)
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)

  (mvc-status-insert-toggle-button (not mvc-local-display-unknown-p)
				   "unknown mask enabled (\\C-c?)"
				   "unknown mask disabled(\\C-c?)"
				   'mvc-status-mode-toggle-display-unknown)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-toggle-button (not mvc-local-display-unmodified-p)
				   "unmodified mask enabled (\\C-c_)"
				   "unmodified mask disabled(\\C-c_)"
				   'mvc-status-mode-toggle-display-unmodified)
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)

  (mvc-status-insert-toggle-button (not mvc-local-display-backup-p)
				   " backup mask enabled (\\C-c~)"
				   " backup mask disabled(\\C-c~)"
				   'mvc-status-mode-toggle-display-backup)
  (mvc-insert-with-face " "
			'mvc-face-status-footer)
  (mvc-status-insert-toggle-button (not mvc-local-display-ignore-p)
				   "    ignore mask enabled (\\C-ci)"
				   "    ignore mask disabled(\\C-ci)"
				   'mvc-status-mode-toggle-display-ignore)
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)

  (mvc-status-insert-toggle-button mvc-local-recursive-p
				   "recursive enabled "
				   "recursive disabled"
				   'mvc-status-mode-toggle-recursive)
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)

  (mvc-insert-with-face "last update:"
			'mvc-face-status-footer)
  (if mvc-local-last-execute-time-update
      (mvc-insert-with-face (current-time-string mvc-local-last-execute-time-update)
			    'mvc-face-status-footer)
    (mvc-insert-with-face mvc-header-string-time
			  'mvc-face-status-footer))
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)
  (mvc-insert-with-face "last status:"
			'mvc-face-status-footer)
  (if mvc-local-last-execute-time-status
      (mvc-insert-with-face (current-time-string mvc-local-last-execute-time-status)
			    'mvc-face-status-footer)
    (mvc-insert-with-face mvc-header-string-time
			  'mvc-face-status-footer))
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)
  (mvc-insert-with-face "last   push:"
			'mvc-face-status-footer)
  (if mvc-local-last-execute-time-push
      (mvc-insert-with-face (current-time-string mvc-local-last-execute-time-push)
			    'mvc-face-status-footer)
    (mvc-insert-with-face mvc-header-string-time
			  'mvc-face-status-footer))
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)
  (mvc-insert-with-face "last   pull:"
			'mvc-face-status-footer)
  (if mvc-local-last-execute-time-pull
      (mvc-insert-with-face (current-time-string mvc-local-last-execute-time-pull)
			    'mvc-face-status-footer)
    (mvc-insert-with-face mvc-header-string-time
			  'mvc-face-status-footer))
  (mvc-insert-with-face "\n"
			'mvc-face-status-footer)

  (setq buffer-read-only t)
  (setq mvc-local-ready-p t))

(defun mvc-status-draw-with-save-load-point ()
  (mvc-status-save-point)
  (mvc-status-draw)
  (mvc-status-load-point))


(defun mvc-status-save-point-elscreen ()
  (setq mvc-local-save-load-buffer-list (buffer-list))
  (setq mvc-local-save-load-window-extension-hash (make-hash-table))
  (mapcar #'(lambda (screen)
	      ;; exclude current screen
	      (unless (= screen (elscreen-get-current-screen))
		(unless (gethash screen mvc-local-save-load-window-extension-hash)
		  (puthash screen
			   (list (cons 'point-hash (make-hash-table)) (cons 'file-name-hash (make-hash-table)))
			   mvc-local-save-load-window-extension-hash))
		(let ((point-hash (cdr (assq 'point-hash (gethash screen
								  mvc-local-save-load-window-extension-hash))))
		      (file-name-hash (cdr (assq 'file-name-hash (gethash screen
									  mvc-local-save-load-window-extension-hash))))
		      (buffer-name (buffer-name))
		      (file-name (mvc-status-get-current-line-file-name)))
		  (save-excursion
		    (save-window-excursion
		      (set-window-configuration (car (elscreen-get-window-configuration screen)))
		      (mapcar #'(lambda (a)
				  (puthash a (window-point a) point-hash)
				  (save-excursion
				    (goto-char (window-point a))
				    (puthash a file-name file-name-hash)))
			      (get-buffer-window-list buffer-name 'ignore t)))))))
	  (elscreen-get-screen-list)))

(defun mvc-status-save-point ()
  (setq mvc-local-save-load-point (point))
  (if (>= (point) mvc-local-file-list-end-point)
      (setq mvc-local-save-load-file-list-end-point mvc-local-file-list-end-point)
    (setq mvc-local-save-load-file-list-end-point nil)
    (setq mvc-local-save-load-file-name (mvc-status-get-current-line-file-name))
    (setq mvc-local-save-load-window-point-hash (make-hash-table))
    (setq mvc-local-save-load-window-file-name-hash (make-hash-table))
    (let ((status-buffer (current-buffer)))
      (mapcar #'(lambda (a)
		  (puthash a (window-point a) mvc-local-save-load-window-point-hash)
		  (save-excursion
		    (goto-char (window-point a))
		    (puthash a
			     (mvc-status-get-current-line-file-name)
			     mvc-local-save-load-window-file-name-hash)))
	      (get-buffer-window-list (buffer-name status-buffer) 'ignore t)))
    (when (boundp 'elscreen-frame-confs)
      (mvc-status-save-point-elscreen))))

(defun mvc-status-load-point-elscreen ()
  (mapcar #'(lambda (screen)
	      ;; exclude current screen
	      (unless (= screen (elscreen-get-current-screen))
		(let ((point-hash (cdr (assq 'point-hash (gethash screen
								  mvc-local-save-load-window-extension-hash))))
		      (file-name-hash (cdr (assq 'file-name-hash (gethash screen
									  mvc-local-save-load-window-extension-hash))))
		      (end-point mvc-local-file-list-end-point))
		  (when file-name-hash
		    (save-excursion
		      (save-window-excursion
			(set-window-configuration (car (elscreen-get-window-configuration screen)))
			(maphash #'(lambda (key value)
				     (when (window-live-p key)
				       (let ((point (gethash key point-hash)))
					 (when (> point end-point)
					   (with-current-buffer (window-buffer key)
					     (save-window-excursion
					       (goto-char end-point)
					       (beginning-of-line)
					       (setq point (point)))))
					 (set-window-point key point))))
				 file-name-hash)
			(elscreen-set-window-configuration screen (elscreen-current-window-configuration))))))))
	  (elscreen-get-screen-list))
  (while mvc-local-save-load-buffer-list
    (bury-buffer (car mvc-local-save-load-buffer-list))
    (setq mvc-local-save-load-buffer-list (cdr mvc-local-save-load-buffer-list))))

(defun mvc-status-load-point ()
  (if mvc-local-save-load-file-list-end-point
      (if mvc-local-first-point-set-p
	  (goto-char (+ mvc-local-file-list-end-point
			(- mvc-local-save-load-point
			   mvc-local-save-load-file-list-end-point)))
	(setq mvc-local-first-point-set-p t)
	(goto-char mvc-local-file-list-begin-point))
    (let ((point (gethash mvc-local-save-load-file-name mvc-local-point-hash)))
      (if point
	  (goto-char point)
	(if (> mvc-local-save-load-point mvc-local-file-list-end-point)
	    (goto-char mvc-local-file-list-end-point)
	  (goto-char mvc-local-save-load-point))
	(beginning-of-line)))
    (when (boundp 'elscreen-frame-confs)
      (mvc-status-load-point-elscreen))
    (maphash #'(lambda (key value)
		 (when (and (window-live-p key)
			    (not (eq (selected-window) key)))
		   (let ((point (gethash value mvc-local-point-hash)))
		     (if point
			 (set-window-point key point)
		       (set-window-point key (gethash key mvc-local-save-load-window-point-hash))))))
	     mvc-local-save-load-window-file-name-hash))
  (setq mvc-local-save-load-window-point-hash nil)
  (setq mvc-local-save-load-window-file-name-hash nil))




;;; mvc-command

(defun mvc-command-get-current-or-mark-file-name-list (current-only-p)
  (let (file-name-list)
    (if (or (= mvc-local-marks 0)
	    current-only-p)
	(progn
	  (setq file-name-list (mvc-status-get-current-line-file-name))
	  (when file-name-list
	    (setq file-name-list (list file-name-list))))
      (maphash #'(lambda (key value)
		   (when (string= value "*")
		     (setq file-name-list (append file-name-list (list key)))))
	       mvc-local-mark-hash))
    file-name-list))


;; search-mode
;;    'add
;;    'modified
;;    'deleted
;;    'renamed
(defun mvc-command-get-current-or-mark-file-name-list-with-search-mode (current-only-p search-mode)
  (let (file-name-list)
    (if (or (= mvc-local-marks 0)
	    current-only-p)
	(progn
	  (setq file-name-list (mvc-status-get-current-line-file-name))
	  (when file-name-list
	    (cond ((eq search-mode 'add)
		   (if (string= (gethash file-name-list mvc-local-code-hash) "A")
		       (setq file-name-list (list file-name-list))
		     (setq file-name-list nil)))
		  ((eq search-mode 'modified)
		   (if (or (string= (gethash file-name-list mvc-local-code-hash) "M")
			   (string= (gethash (concat (expand-file-name default-directory) file-name-list) mvc-local-after-save-hook-hash) "m"))
		       (setq file-name-list (list file-name-list))
		     (setq file-name-list nil)))
		  ((eq search-mode 'deleted)
		   (if (string= (gethash file-name-list mvc-local-code-hash) "D")
		       (setq file-name-list (list file-name-list))
		     (setq file-name-list nil)))
		  ((eq search-mode 'renamed)
		   (if (string= (gethash file-name-list mvc-local-code-hash) "R")
		       (setq file-name-list (list file-name-list))
		     (setq file-name-list nil))))))
      (maphash #'(lambda (key value)
		   (when (string= value "*")
		     (cond ((eq search-mode 'add)
			    (when (string= (gethash key mvc-local-code-hash) "A")
			      (setq file-name-list (append file-name-list (list key)))))
			   ((eq search-mode 'modified)
			    (when (or (string= (gethash key mvc-local-code-hash) "M")
				      (string= (gethash (concat (expand-file-name default-directory) key) mvc-local-after-save-hook-hash) "m"))
			      (setq file-name-list (append file-name-list (list key)))))
			   ((eq search-mode 'deleted)
			    (when (string= (gethash key mvc-local-code-hash) "D")
			      (setq file-name-list (append file-name-list (list key)))))
			   ((eq search-mode 'renamed)
			    (when (string= (gethash key mvc-local-code-hash) "R")
			      (setq file-name-list (append file-name-list (list key))))))))
	       mvc-local-mark-hash))
    file-name-list))


(defun mvc-command-current-or-mark-git-revert-core (status-buffer search-mode option-key-list)
  (setq file-name-list (mvc-command-get-current-or-mark-file-name-list-with-search-mode current-only-p search-mode))
  (when file-name-list
    (mapcar #'(lambda (option-key)
		(setq command-list (mvc-status-get-current-program-option-list option-key option-argument))
		(setq command-name (nth 0 command-list))
		(setq command-list (cdr command-list))
		(save-current-buffer
		  (mvc-call-process-temporary status-buffer
					      program-name
					      (append command-list
						      option-list
						      file-name-list)
					      status-default-directory)))
	    option-key-list)))

(defun mvc-command-current-or-mark-git-revert ()
  (let (file-name-list
	command-list
	command-name
	(status-buffer (current-buffer))
	temporary-process-buffer-name)
    ;; new file: git rm --cached file...
    (mvc-command-current-or-mark-git-revert-core status-buffer 'add '(git-rm-cached))
    ;; modified: git checkout -- file...
    (mvc-command-current-or-mark-git-revert-core status-buffer 'modified '(git-checkout))
    ;; deleted: git reset -- file... and git checkout -- file..
    (mvc-command-current-or-mark-git-revert-core status-buffer 'deleted '(git-reset git-checkout))
    ;; renamed: git reset -- file... -> deleted:
    (mvc-command-current-or-mark-git-revert-core status-buffer 'renamed '(git-reset))
    ;; 内部状態が変化しないので、ここでは 'deleted できない
;;;     (mvc-command-current-or-mark-git-revert-core status-buffer 'deleted '(git-reset git-checkout))

    (with-current-buffer status-buffer
      (setq temporary-process-buffer-name (cdr (assq 'process-temporary mvc-local-buffer-name-list))))
    (set-buffer (get-buffer-create temporary-process-buffer-name))))

;; カレント位置のファイルまたはマークされたファイルに対し、バッファ
;; buffer-name でコマンドを実行します。

;; option-list が Non-nil ならば
;; mvc-status-get-current-program-option-list で生成されるデフォルトの
;; オプションの後ろに追加されます。 option-list が nil ならばデフォルト
;; のオプションがそのまま適用されます。 option-argument は
;; mvc-status-get-current-program-option-list の argument としてわたさ
;; れます。tail-file-list は引数の最後に追加されます。 current-only-p
;; が Non-nil ならばカレント位置のファイルに対してのみコマンドを実行し
;; ます。

;; mvc-command 系関数では必要な内部変数などは更新されますが window の操
;; 作はせず、バッファやポイントの状態が保存されないことに注意が必要です。
;; 実行が成功した場合は Non-nil を返し、対象のファイルが指定されていな
;; い場合や yes-or-no-p で no が返された場合などに nil を返します。
(defun mvc-command-current-or-mark (command-key
				    &optional
				    yes-or-no-p option-list option-argument tail-file-list current-only-p)
  (let* (command-list
	 command-name
	 file-name-list
	 buffer-name)

    (cond ((or (eq command-key 'add)
	       (eq command-key 'annotate)
	       (eq command-key 'commit)
	       (eq command-key 'remove)
	       (eq command-key 'rename)
	       (eq command-key 'revert))
	   (setq buffer-name (cdr (assq 'process-temporary mvc-local-buffer-name-list))))
	  ((or (eq command-key 'diff)
	       (eq command-key 'log))
	   (setq buffer-name (cdr (assq command-key mvc-local-buffer-name-list)))))

    (setq command-list (mvc-status-get-current-program-option-list command-key option-argument))
    (setq command-name (nth 0 command-list))
    (setq command-list (cdr command-list))
    (setq file-name-list (mvc-command-get-current-or-mark-file-name-list current-only-p))

    (if file-name-list
	(progn
	  (let ((flag t))
	    (when yes-or-no-p
	      (setq flag (yes-or-no-p (if (<= mvc-local-marks 1)
					  (concat command-name " " (nth 0 file-name-list) "?")
					(format "%s %d files?" command-name mvc-local-marks)))))
	    (if flag
		(let ((program-name mvc-local-program-name)
		      (status-buffer (current-buffer))
		      (status-default-directory default-directory))
		  (message "%s %s ..." program-name command-name)
		  (if (string= buffer-name (cdr (assq 'process-temporary mvc-local-buffer-name-list)))
		      (if (and (eq mvc-local-program 'git)
			       (eq command-key 'revert))
			  (mvc-command-current-or-mark-git-revert)
			(mvc-call-process-temporary status-buffer
						    program-name
						    (append command-list
							    option-list
							    file-name-list
							    tail-file-list)
						    status-default-directory))
		    (set-buffer (get-buffer-create buffer-name))
		    (setq buffer-undo-list t)
		    (setq buffer-read-only nil)
		    (erase-buffer)
		    (apply 'call-process program-name nil t nil (append command-list
									option-list
									file-name-list
									tail-file-list)))
		  (message "%s %s ...done" program-name command-name)
		  (goto-char (point-min))
		  t)
	      (message  "%s %s canceled!" program-name command-name)
	      nil)))
      (message "unknown line!")
      nil)))


(defun mvc-command-diff-only-current (prefix-argument)
  (mvc-command-current-or-mark 'diff
			       nil
			       nil
			       prefix-argument
			       nil
			       t))

(defun mvc-command-diff-current-or-mark (prefix-argument)
  (mvc-command-current-or-mark 'diff
			       nil
			       nil
			       prefix-argument))


(defun mvc-command-add ()
  (save-current-buffer
    (mvc-command-current-or-mark 'add))
  (mvc-async-status))


(defun mvc-command-annotate ()
  (save-current-buffer
    (mvc-command-current-or-mark 'annotate)))


(defun mvc-command-commit ()
  (let ((marks mvc-local-marks)
	(code-hash mvc-local-code-hash)
	(file-name (mvc-status-get-current-line-file-name))
	(status-buffer (current-buffer))
	commitlog-buffer-name
	result-buffer-name
	file-name-list)
    (setq commitlog-buffer-name (cdr (assq 'commitlog mvc-local-buffer-name-list)))
    (setq result-buffer-name (cdr (assq 'result mvc-local-buffer-name-list)))
    (if (or file-name
	    (>= marks 1))
	(progn
	  (if (= marks 0)
	      (setq file-name-list (list file-name))
	    (maphash #'(lambda (key value)
			 (when (string= value "*")
			   (setq file-name-list (append file-name-list (list key)))))
		     mvc-local-mark-hash))

	  (setq mvc-local-last-point (point))
	  (setq mvc-local-last-window-configuration (current-window-configuration))

	  (let ((program-name mvc-local-program-name)
		(commit-message (cdr (assq mvc-local-program mvc-default-commit-message)))
		(log-option (cdr (mvc-status-get-current-program-option-list 'log))))
	    (switch-to-buffer (get-buffer-create result-buffer-name))
	    (setq buffer-undo-list t)
	    (setq buffer-read-only nil)

	    (erase-buffer)
	    (if (<= marks 1)
		(mvc-insert-with-face "** File to commit\n"
				      'mvc-face-commit-headline)
	      (mvc-insert-with-face (format "** Commit %d files\n" marks)
				    'mvc-face-commit-headline))

	    (mapcar #'(lambda (a)
			(when (string= a ".")
			  (mvc-insert-with-face "#### ROOT DIRECTORY ####\n"
						'mvc-face-commit-warning))
			(when (string= (gethash a code-hash) "?")
			  (mvc-insert-with-face "#### UNKNOWN FILE ####\n"
						'mvc-face-commit-warning))
			(insert (concat "    " a))
			(insert "\n"))
		    file-name-list)
	    (insert "\n")

	    (mvc-insert-with-face (format "** Log  ($ %s log \".\")\n" program-name)
				  'mvc-face-commit-headline)

	    (apply 'call-process program-name nil t nil (append log-option
								(list ".")))

	    (let ((buffer (get-buffer commitlog-buffer-name)))
	      (goto-char (point-min))
	      (mvc-insert-with-face "** Information\n"
				    'mvc-face-commit-headline)
	      (when (and (not buffer)
			 commit-message)
		(mvc-insert-with-face "    * insert default commit message\n"
				      'mvc-face-commit-warning))
	      (mvc-insert-with-face (concat "    * directory:" default-directory "\n")
				    'mvc-face-commit-information)
	      (insert "\n")
	      (mvc-log-mode)
	      (setq buffer-read-only t)
	      (goto-char (point-min))

	      (if buffer
		  (switch-to-buffer-other-window buffer)
		(switch-to-buffer-other-window (get-buffer-create commitlog-buffer-name))
		(when commit-message
		  (insert commit-message)))))

	  (mvc-commitlog-mode status-buffer))
      (message "no commit file!"))))


(defun mvc-command-log (prefix-argument)
  (let (result
	(log-buffer-name (cdr (assq 'log mvc-local-buffer-name-list))))
    (save-current-buffer
      (setq result (mvc-command-current-or-mark 'log
						nil
						nil
						prefix-argument)))
    (with-current-buffer log-buffer-name
      (mvc-log-mode))
    result))


(defun mvc-command-revert ()
  (save-current-buffer
    (let ((especial-buffer-name (cdr (assq 'especial mvc-local-buffer-name-list))))
      (mvc-command-current-or-mark 'revert
				   t)
      (when (get-buffer especial-buffer-name)
	(kill-buffer especial-buffer-name)
	(message "buffer \"%s\" killed by revert" especial-buffer-name))))
  (mvc-async-status))


(defun mvc-command-remove ()
  (save-current-buffer
    (mvc-command-current-or-mark 'remove))
  (mvc-async-status))


(defun mvc-command-rename (destination)
  (save-current-buffer
    (mvc-command-current-or-mark 'rename
				 nil
				 nil
				 nil
				 (list destination)))
  (mvc-async-status))




;;; mvc-async

(defun mvc-async-set-status-variable (status-variable-symbol value)
  (with-current-buffer mvc-async-local-buffer-name-status
    (set status-variable-symbol value)))


(defun mvc-async-status-process-sentinel-mercurial-add (key code old-hash)
  (with-current-buffer mvc-async-local-buffer-name-status
    (puthash key code mvc-local-code-hash)
    (puthash key 0 mvc-local-point-hash)
    (puthash (concat (expand-file-name default-directory) key) t mvc-local-after-save-hook-hash)
    (let ((oldvalue (gethash key old-hash)))
      (if oldvalue
	  (progn
	    (when (string= oldvalue "*")
	      (setq mvc-local-marks (1+ mvc-local-marks)))
	    (puthash key "*" mvc-local-mark-hash))
	(puthash key nil mvc-local-mark-hash)))))

(defun mvc-async-status-process-sentinel-mercurial (process event)
  (cond ((string= event "finished\n")
	 (with-current-buffer (process-buffer process)
	   (setq buffer-read-only nil)

	   (mvc-async-set-status-variable 'mvc-local-last-execute-time-status (current-time))

	   (let (sort-start)
	     ;; sort-start を求めます。
	     (goto-char (point-min))
	     (re-search-forward "^ *[^ ]+ +" nil t)
	     (setq sort-start (1- (point)))

	     (goto-char (point-min))
	     (sort-subr nil 'forward-line 'end-of-line #'(lambda () (forward-char sort-start))))

	   ;; ソート済みバッファから hash を生成します。
	   (goto-char (point-min))
	   (let ((dir-hash (make-hash-table :test 'equal))
		 (old-hash))

	     (with-current-buffer mvc-async-local-buffer-name-status
	       (setq old-hash (copy-hash-table mvc-local-mark-hash))
	       (setq mvc-local-files 0)
	       (setq mvc-local-marks 0)
	       (clrhash mvc-local-mark-hash)
	       (clrhash mvc-local-code-hash)
	       (clrhash mvc-local-after-save-hook-hash))

	     (mvc-async-status-process-sentinel-mercurial-add "." "d" old-hash)

	     (let ((files 0))
	       (while (< (point) (point-max))
		 (setq files (1+ files))
		 (if (re-search-forward "^\\([^ ]+\\) +\\(.+\\)" nil t)
		     (let ((code (match-string 1))
			   (key (match-string 2)))
		       (when (string-match "\\(.+/\\)[^/]+$" key)
			 (let ((dir (match-string 1 key)))
			   (unless (gethash dir dir-hash)
			     (mvc-async-status-process-sentinel-mercurial-add dir "d" old-hash)
			     (puthash dir t dir-hash))))
		       (forward-line)
		       (mvc-async-status-process-sentinel-mercurial-add key code old-hash))
		   (forward-line)))
	       (mvc-async-set-status-variable 'mvc-local-files files)))

	   ;; read only にした後、描画しておしまい。
	   (setq buffer-read-only t)
	   (with-current-buffer mvc-async-local-buffer-name-status
	     (mvc-status-draw)
	     (mvc-status-load-point)
	     (message "%s status ...done" mvc-local-program-name))))
	(t
	 (message (concat "status process error error:" event))))
  (with-current-buffer (process-buffer process)
    (mvc-async-set-status-variable 'mvc-local-async-process nil))
  (kill-buffer (process-buffer process)))

(defun mvc-async-status-process-sentinel-git-add (key code old-hash)
  (with-current-buffer mvc-async-local-buffer-name-status
    (puthash key code mvc-local-code-hash)
    (puthash key 0 mvc-local-point-hash)
    (puthash (concat (expand-file-name default-directory) key) t mvc-local-after-save-hook-hash)
    (let ((oldvalue (gethash key old-hash)))
      (if oldvalue
	  (progn
	    (when (string= oldvalue "*")
	      (setq mvc-local-marks (1+ mvc-local-marks)))
	    (puthash key "*" mvc-local-mark-hash))
	(puthash key nil mvc-local-mark-hash)))))

(defun mvc-async-status-process-sentinel-git (process event)
  (cond ((or (string= event "finished\n")
	     (eq (compare-strings event 0 28 "exited abnormally with code " nil nil) t))
	 (with-current-buffer (process-buffer process)
	   (setq buffer-read-only nil)

	   (mvc-async-set-status-variable 'mvc-local-last-execute-time-status (current-time))

	   (let (parameter-file-list
		 (new-file-hash (make-hash-table :test 'equal))
		 (modified-hash (make-hash-table :test 'equal))
		 (deleted-hash (make-hash-table :test 'equal))
		 (renamed-hash (make-hash-table :test 'equal))
		 (untracked-hash (make-hash-table :test 'equal))
		 (dir-hash (make-hash-table :test 'equal))
		 (old-hash))
	     (mvc-get-ls-parameter-file-list (get-buffer mvc-async-local-buffer-name-status))
	     (goto-char (point-min))
	     (while (re-search-forward "^#	new file: +\\(.+\\)$" nil t)
	       (puthash (match-string 1) t new-file-hash))
	     (goto-char (point-min))
	     (while (re-search-forward "^#	modified: +\\(.+\\)$" nil t)
	       (puthash (match-string 1) t modified-hash))
	     (goto-char (point-min))
	     (while (re-search-forward "^#	deleted: +\\(.+\\)$" nil t)
	       (puthash (match-string 1) t deleted-hash))
	     (goto-char (point-min))
	     (while (re-search-forward "^#	renamed: +.+ -> \\(.+\\)$" nil t)
	       (puthash (match-string 1) t renamed-hash))

	     (when (re-search-forward "^# Untracked files:$" nil t)
	       (while (re-search-forward "^#	\\(.+\\)$" nil t)
		 (puthash (match-string 1) t untracked-hash)))

	     (goto-char (point-min))

	     (with-current-buffer mvc-async-local-buffer-name-status
	       (setq old-hash (copy-hash-table mvc-local-mark-hash))
	       (setq mvc-local-files 0)
	       (setq mvc-local-marks 0)
	       (clrhash mvc-local-mark-hash)
	       (clrhash mvc-local-code-hash)
	       (clrhash mvc-local-after-save-hook-hash))

	     (mvc-async-status-process-sentinel-git-add "." "d" old-hash)

	     (let ((files 0)
		   (code " "))
	       (mapcar #'(lambda (key)
			   (when (string-match "\\(.+/\\)[^/]+$" key)
			     (let ((dir (match-string 1 key)))
			       (unless (gethash dir dir-hash)
				 (mvc-async-status-process-sentinel-git-add dir "d" old-hash)
				 (setq files (1+ files))
				 (puthash dir t dir-hash))))
			   (when (string-match ".+[^/]$" key)
			     (cond ((gethash key new-file-hash)
				    (setq code "A"))
				   ((gethash key modified-hash)
				    (setq code "M"))
				   ((gethash key renamed-hash)
				    (setq code "R"))
				   ((or (gethash key untracked-hash)
					(catch 'map
					  (maphash #'(lambda (k v)
						       (when (and (string-match "/$" k)
								  (string-match (concat "^" (regexp-quote k)) key))
							 (throw 'map t)))
						   untracked-hash)
					  nil))
				    (setq code "?"))
				   (t
				    (setq code " ")))

			     (let (delete-key)
			       (when (catch 'map
				       (maphash #'(lambda (k v)
						    (when (and v
							       (string< k key))
						      (setq delete-key k)
						      (remhash k deleted-hash)
						      (throw 'map t)))
						deleted-hash)
				       nil)
				 (mvc-async-status-process-sentinel-git-add delete-key "D" old-hash)
				 (setq files (1+ files))))

			     (mvc-async-status-process-sentinel-git-add key code old-hash)
			     (setq files (1+ files))))
		       parameter-file-list)
	       (mvc-async-set-status-variable 'mvc-local-files files)))

	   ;; read only にした後、描画しておしまい。
	   (setq buffer-read-only t)
	   (with-current-buffer mvc-async-local-buffer-name-status
	     (mvc-status-draw)
	     (mvc-status-load-point)
	     (message "%s status ...done" mvc-local-program-name))))
	(t
	 (message (concat "status process error error:" event))))
  (with-current-buffer (process-buffer process)
    (mvc-async-set-status-variable 'mvc-local-async-process nil))
  (kill-buffer (process-buffer process)))

(defun mvc-async-status-process-sentinel-bazaar-add (key code old-hash)
  (with-current-buffer mvc-async-local-buffer-name-status
    (puthash key code mvc-local-code-hash)
    (puthash key 0 mvc-local-point-hash)
    (puthash (concat (expand-file-name default-directory) key) t mvc-local-after-save-hook-hash)
    (let ((oldvalue (gethash key old-hash)))
      (if oldvalue
	  (progn
	    (when (string= oldvalue "*")
	      (setq mvc-local-marks (1+ mvc-local-marks)))
	    (puthash key "*" mvc-local-mark-hash))
	(puthash key nil mvc-local-mark-hash)))))

(defun mvc-async-status-process-sentinel-bazaar (process event)
  (cond ((or (string= event "finished\n")
	     (eq (compare-strings event 0 28 "exited abnormally with code " nil nil) t))
	 (with-current-buffer (process-buffer process)
	   (setq buffer-read-only nil)

	   (mvc-async-set-status-variable 'mvc-local-last-execute-time-status (current-time))

	   (let (parameter-file-list
		 (new-file-hash (make-hash-table :test 'equal))
		 (modified-hash (make-hash-table :test 'equal))
		 (deleted-hash (make-hash-table :test 'equal))
		 (renamed-hash (make-hash-table :test 'equal))
		 (untracked-hash (make-hash-table :test 'equal))
		 (dir-hash (make-hash-table :test 'equal))
		 (ls-versioned-hash (make-hash-table :test 'equal))
		 (ls-ignored-hash (make-hash-table :test 'equal))
		 (old-hash))

	     (mvc-get-ls-parameter-file-list (get-buffer mvc-async-local-buffer-name-status))

	     (save-current-buffer
	       (let (program-name)
		 (with-current-buffer mvc-async-local-buffer-name-status
		   (setq program-name mvc-local-program-name))
		 (mvc-call-process-temporary (get-buffer mvc-async-local-buffer-name-status)
					     program-name
					     (list "ls"
						   "--verbose")
					     default-directory))
	       (goto-char (point-min))
	       (while (< (point) (point-max))
		 (when (re-search-forward "^\\(.\\) +\\(.+\\)" nil t)
		   (let ((code (match-string 1))
			 (key (match-string 2)))
		     (cond ((string= code "V")
			    (puthash key t ls-versioned-hash))
			   ((string= code "I")
			    (puthash key t ls-ignored-hash)))))
		 (forward-line)))

	     (goto-char (point-min))
	     (while (< (point) (point-max))
	       (when (re-search-forward "^\\(.\\)\\(.\\)\\(.\\) +\\(.+\\)" nil t)
		 (let ((code-1 (match-string 1))
		       (code-2 (match-string 2))
		       (code-3 (match-string 3))
		       (key (match-string 4))
		       code)
		   (cond ((string= code-1 "?")
			  (puthash key t untracked-hash))
			 ((string= code-1 "R")
			  (puthash key t renamed-hash))
			 ((string= code-1 "C")
			  )
			 ((string= code-2 "N")
			  (puthash key t new-file-hash))
			 ((string= code-2 "D")
			  (puthash key t deleted-hash))
			 ((string= code-2 "M")
			  (puthash key t modified-hash)))))
	       (forward-line))

	     (with-current-buffer mvc-async-local-buffer-name-status
	       (setq old-hash (copy-hash-table mvc-local-mark-hash))
	       (setq mvc-local-files 0)
	       (setq mvc-local-marks 0)
	       (clrhash mvc-local-mark-hash)
	       (clrhash mvc-local-code-hash)
	       (clrhash mvc-local-after-save-hook-hash))

	     (mvc-async-status-process-sentinel-bazaar-add "." "d" old-hash)

	     (let ((files 0)
		   (code " "))
	       (mapcar #'(lambda (key)
			   (when (string-match "\\(.+/\\)[^/]*$" key)
			     (let ((dir (match-string 1 key)))
			       (unless (gethash dir dir-hash)
				 (mvc-async-status-process-sentinel-bazaar-add dir "d" old-hash)
				 (setq files (1+ files))
				 (puthash dir t dir-hash))))
			   (when (string-match ".+[^/]$" key)
			     (cond ((gethash key new-file-hash)
				    (setq code "A"))
				   ((gethash key modified-hash)
				    (setq code "M"))
				   ((gethash key renamed-hash)
				    (setq code "R"))
				   ((gethash key ls-ignored-hash)
				    (setq code "I"))
				   ((or (gethash key untracked-hash)
					(catch 'map
					  (maphash #'(lambda (k v)
						       (when (and (string-match "/$" k)
								  (string-match (concat "^" (regexp-quote k)) key))
							 (throw 'map t)))
						   untracked-hash)
					  nil))
				    (setq code "?"))
				   (t
				    (setq code " ")))

			     (let (delete-key)
			       (when (catch 'map
				       (maphash #'(lambda (k v)
						    (when (and v
							       (string< k key))
						      (setq delete-key k)
						      (remhash k deleted-hash)
						      (throw 'map t)))
						deleted-hash)
				       nil)
				 (mvc-async-status-process-sentinel-bazaar-add delete-key "D" old-hash)
				 (setq files (1+ files))))

			     (mvc-async-status-process-sentinel-bazaar-add key code old-hash)
			     (setq files (1+ files))))
		       parameter-file-list)
	       (mvc-async-set-status-variable 'mvc-local-files files)))

	   ;; read only にした後、描画しておしまい。
	   (setq buffer-read-only t)
	   (with-current-buffer mvc-async-local-buffer-name-status
	     (mvc-status-draw)
	     (mvc-status-load-point)
	     (message "%s status ...done" mvc-local-program-name))))
	(t
	 (message (concat "status process error error:" event))))
  (with-current-buffer (process-buffer process)
    (mvc-async-set-status-variable 'mvc-local-async-process nil))
  (kill-buffer (process-buffer process)))

(defun mvc-async-status-process-sentinel-subversion-add (key code information old-hash)
  (with-current-buffer mvc-async-local-buffer-name-status
    (puthash key information mvc-local-information-hash)
    (puthash key code mvc-local-code-hash)
    (puthash key 0 mvc-local-point-hash)
    (puthash (concat (expand-file-name default-directory) key) t mvc-local-after-save-hook-hash)
    (let ((oldvalue (gethash key old-hash)))
      (if oldvalue
	  (progn
	    (when (string= oldvalue "*")
	      (setq mvc-local-marks (1+ mvc-local-marks)))
	    (puthash key "*" mvc-local-mark-hash))
	(puthash key nil mvc-local-mark-hash)))))

(defun mvc-async-status-process-sentinel-subversion (process event)
  (cond ((string= event "finished\n")
	 (with-current-buffer (process-buffer process)
	   (goto-char (point-min))
	   (if (string= (thing-at-point 'word) "svn")
	       (progn
		 (save-current-buffer
		   (set-buffer (get-buffer-create mvc-async-local-buffer-name-status))
		   (setq buffer-read-only nil)
		   (erase-buffer)
		   (insert "**************************\n")
		   (insert "**** svn status failed ***\n")
		   (insert "**************************\n")
		   (insert-buffer-substring (cdr (assq 'process-async mvc-local-buffer-name-list)))
		   (setq buffer-read-only t))
		 (message "svn status failed"))
	     (setq buffer-read-only nil)

	     ;; 1.6 からの "      >   local add, incoming add upon update" を削る
	     (goto-char (point-min))
	     (while (< (point) (point-max))
	       (let ((code (char-after (+ (point) 6))))
		 (if (and code
			  (= code ?>))
		     (let ((s (point)))
		       (message "found")
		       (end-of-line)
		       (delete-region s (if (= (point) (point-max))
					    (point)
					  (1+ (point)))))
		   (forward-line))))

	     (mvc-async-set-status-variable 'mvc-local-last-execute-time-status (current-time))

	     (let ((sort-start 41))
	       (let ((dir-hash (make-hash-table :test 'equal)) dir-list key-start)
		 ;; ディレクトリを dir-hash と dir-list に取り出します。
		 (goto-char (point-min))
		 (while (< (point) (point-max))
		   (beginning-of-line)
		   (setq key-start (+ (point) sort-start))
		   (end-of-line)
		   (let ((key (buffer-substring key-start (point))) (times 0) (s 0) dir)
		     (forward-line)
		     (while (setq s (string-match "/" key s))
		       (setq s (1+ s))
		       (setq times (1+ times)))
		     (when (and (> times 0)
				(string-match "\\(.+/\\)[^/]+$" key))
		       (setq dir (match-string 1 key))
		       (unless (gethash dir dir-hash)
			 (puthash dir t dir-hash)
			 (setq dir-list (cons dir dir-list))))))

		 ;; ディレクトリの末尾に / を付加します。
		 (goto-char (point-min))
		 (while (< (point) (point-max))
		   (beginning-of-line)
		   (setq key-start (+ (point) sort-start))
		   (end-of-line)
		   (let ((key (buffer-substring key-start (point))) (times 0) (s 0) dir)
		     (when (gethash (concat key "/") dir-hash)
		       (end-of-line)
		       (insert "/"))
		     (forward-line))))

	       (goto-char (point-min))
	       (sort-subr nil 'forward-line 'end-of-line #'(lambda () (forward-char sort-start)))

	       ;; ソート済みバッファから hash を生成します。
	       (goto-char (point-min))
	       (let (old-hash)

		 (with-current-buffer mvc-async-local-buffer-name-status
		   (setq old-hash (copy-hash-table mvc-local-mark-hash))
		   (setq mvc-local-files 0)
		   (setq mvc-local-marks 0)
		   (clrhash mvc-local-mark-hash)
		   (clrhash mvc-local-code-hash)
		   (clrhash mvc-local-information-hash)
		   (clrhash mvc-local-after-save-hook-hash))
		 (let ((files 0))
		   (while (< (point) (point-max))
		     (setq files (1+ files))

		     (let ((code (buffer-substring (point) (1+ (point)))) ; base code
			   (key-start (+ (point) sort-start))
			   key
			   information)
		       (when (string= code " ")
			 (setq code (buffer-substring (1+ (point)) (+ (point) 2)))) ; property code
		       ;; 7 桁目までのどこかに conflict があれば conflict 扱いとして上書きします。
		       (let ((case-fold-search t))
			 (beginning-of-line)
			 (when (search-forward "C" (+ (point) 7) t)
			   (setq code "C")))
		       (beginning-of-line)
		       (setq information (buffer-substring (+ (point) 9) (+ (point) 39)))
		       (end-of-line)
		       (setq key (buffer-substring key-start (point)))
		       (forward-char 1)
		       (mvc-async-status-process-sentinel-subversion-add key code information old-hash)))
		   (mvc-async-set-status-variable 'mvc-local-files files)))

	       ;; read only にした後、描画しておしまい。
	       (setq buffer-read-only t)
	       (with-current-buffer mvc-async-local-buffer-name-status
		 (mvc-status-draw)
		 (mvc-status-load-point)
		 (message "%s status ...done" mvc-local-program-name))))))
	(t
	 (message (concat "status process error error:" event))))
  (with-current-buffer (process-buffer process)
    (mvc-async-set-status-variable 'mvc-local-async-process nil))
  (kill-buffer (process-buffer process)))

(defun mvc-async-status-process-sentinel-cvs-add (key code information old-hash)
  (with-current-buffer mvc-async-local-buffer-name-status
    (puthash key information mvc-local-information-hash)
    (puthash key code mvc-local-code-hash)
    (puthash key 0 mvc-local-point-hash)
    (puthash (concat (expand-file-name default-directory) key) t mvc-local-after-save-hook-hash)
    (let ((oldvalue (gethash key old-hash)))
      (if oldvalue
	  (progn
	    (when (string= oldvalue "*")
	      (setq mvc-local-marks (1+ mvc-local-marks)))
	    (puthash key "*" mvc-local-mark-hash))
	(puthash key nil mvc-local-mark-hash)))))

(defun mvc-async-status-process-sentinel-cvs (process event)
  (cond ((string= event "finished\n")
	 (with-current-buffer (process-buffer process)
	   (goto-char (point-min))
	   (if (not (string= (thing-at-point 'word) "cvs"))
	       (progn
		 (save-current-buffer
		   (set-buffer (get-buffer-create mvc-async-local-buffer-name-status))
		   (setq buffer-read-only nil)
		   (erase-buffer)
		   (insert "*****************************\n")
		   (insert "**** cvs -n update failed ***\n")
		   (insert "*****************************\n")
		   (insert-buffer-substring (cdr (assq 'process-async mvc-local-buffer-name-list)))
		   (setq buffer-read-only t))
		 (message "cvs -n update failed"))
	     (setq buffer-read-only nil)

	     (mvc-async-set-status-variable 'mvc-local-last-execute-time-status (current-time))

	     (let ((sort-start 40))
	       (let ((dir-hash (make-hash-table :test 'equal)) dir-list key-start)
		 ;; ディレクトリを dir-hash と dir-list に取り出します。
		 (goto-char (point-min))
		 (while (< (point) (point-max))
		   (beginning-of-line)
		   (setq key-start (+ (point) sort-start))
		   (end-of-line)
		   (let ((key (buffer-substring key-start (point))) (times 0) (s 0) dir)
		     (forward-line)
		     (while (setq s (string-match "/" key s))
		       (setq s (1+ s))
		       (setq times (1+ times)))
		     (when (and (> times 0)
				(string-match "\\(.+/\\)[^/]+$" key))
		       (setq dir (match-string 1 key))
		       (unless (gethash dir dir-hash)
			 (puthash dir t dir-hash)
			 (setq dir-list (cons dir dir-list))))))

		 ;; ディレクトリの末尾に / を付加します。
		 (goto-char (point-min))
		 (while (< (point) (point-max))
		   (beginning-of-line)
		   (setq key-start (+ (point) sort-start))
		   (end-of-line)
		   (let ((key (buffer-substring key-start (point))) (times 0) (s 0) dir)
		     (when (gethash (concat key "/") dir-hash)
		       (end-of-line)
		       (insert "/"))
		     (forward-line))))

	       (goto-char (point-min))
	       (sort-subr nil 'forward-line 'end-of-line #'(lambda () (forward-char sort-start)))

	       ;; ソート済みバッファから hash を生成します。
	       (goto-char (point-min))
	       (let (old-hash)

		 (with-current-buffer mvc-async-local-buffer-name-status
		   (setq old-hash (copy-hash-table mvc-local-mark-hash))
		   (setq mvc-local-files 0)
		   (setq mvc-local-marks 0)
		   (clrhash mvc-local-mark-hash)
		   (clrhash mvc-local-code-hash)
		   (clrhash mvc-local-information-hash)
		   (clrhash mvc-local-after-save-hook-hash))
		 (let ((files 0))
		   (while (< (point) (point-max))
		     (setq files (1+ files))

		     (let ((code (buffer-substring (point) (1+ (point))))
			   (key-start (+ (point) sort-start))
			   key
			   information)
		       (beginning-of-line)
		       (setq information (buffer-substring (+ (point) 9) (+ (point) 38)))
		       (end-of-line)
		       (setq key (buffer-substring key-start (point)))
		       (forward-char 1)
		       (mvc-async-status-process-sentinel-cvs-add key code information old-hash)))
		   (mvc-async-set-status-variable 'mvc-local-files files)))

	       ;; read only にした後、描画しておしまい。
	       (setq buffer-read-only t)
	       (with-current-buffer mvc-async-local-buffer-name-status
		 (mvc-status-draw)
		 (mvc-status-load-point)
		 (message "%s status ...done" mvc-local-program-name))))))
	(t
	 (save-current-buffer
	   (set-buffer (get-buffer-create mvc-async-local-buffer-name-status))
	   (setq buffer-read-only nil)
	   (erase-buffer)
	   (insert "**************************\n")
	   (insert "**** cvs status failed ***\n")
	   (insert "**************************\n")
	   (insert-buffer-substring (cdr (assq 'process-async mvc-local-buffer-name-list)))
	   (setq buffer-read-only t))
	 (message (concat "status process error error:" event))))
  (with-current-buffer (process-buffer process)
    (mvc-async-set-status-variable 'mvc-local-async-process nil))
  (kill-buffer (process-buffer process)))

;; 非同期で "mvc status" を実行します。
;;
;; initial-program に Non-nil を指定するのは
;;
;; 「status バッファが存在しない初回起動時のみ」
;;
;; であることに注意が必要です。
(defun mvc-async-status (&optional initial-program ignore-save-reset-point-p)
  (if (mvc-status-mode initial-program)
      (progn
	(if mvc-local-async-process
	    (message mvc-message-process-already-running)

	  (unless ignore-save-reset-point-p
	    (mvc-status-save-point))

	  (setq buffer-read-only nil)
	  (setq overwrite-mode t)
	  (save-excursion
	    (goto-char (point-min))
	    (insert mvc-running-header-head)
	    (insert "status")
	    (insert mvc-running-header-tail)
	    (let ((p (point)))
	      (end-of-line)
	      (delete-region p (point))))
	  (setq overwrite-mode nil)
	  (setq buffer-read-only t)

	  (let ((option-list (mvc-status-get-current-program-option-list 'status)))
	    (setq mvc-local-async-process-last-command (nth 0 option-list))
	    (let ((process-connection-type mvc-default-process-connection-type)
		  (status-buffer (current-buffer))
		  (async-process-name (cdr (assq 'process-async mvc-local-buffer-name-list)))
		  (async-process-buffer-name (cdr (assq 'process-async mvc-local-buffer-name-list))))
	      (setq mvc-local-async-process (apply 'start-process
						   async-process-name
						   async-process-buffer-name
						   mvc-local-program-name
						   (cdr option-list)))
	      (with-current-buffer async-process-buffer-name
		(set (make-local-variable 'mvc-async-local-buffer-name-status) status-buffer))))

	  (cond ((eq mvc-local-program 'mercurial)
		 (set-process-sentinel mvc-local-async-process
				       'mvc-async-status-process-sentinel-mercurial))
		((eq mvc-local-program 'git)
		 (set-process-sentinel mvc-local-async-process
				       'mvc-async-status-process-sentinel-git))
		((eq mvc-local-program 'bazaar)
		 (set-process-sentinel mvc-local-async-process
				       'mvc-async-status-process-sentinel-bazaar))
		((eq mvc-local-program 'subversion)
		 (set-process-sentinel mvc-local-async-process
				       'mvc-async-status-process-sentinel-subversion))
		((eq mvc-local-program 'cvs)
		 (set-process-sentinel mvc-local-async-process
				       'mvc-async-status-process-sentinel-cvs))
		(t
		 (message "UNKNOWN PROGRAM!"))))
	t)
    (message "mvc.el : SETUP FAILED!")
    nil))


(defun mvc-async-update-process-sentinel (process event)
  (cond ((string= event "finished\n")
	 (with-current-buffer (process-buffer process)
	   (mvc-async-set-status-variable 'mvc-local-last-execute-time-update (current-time))
	   (let (result-buffer-name
		 async-process-buffer-name)
	     (with-current-buffer mvc-async-local-buffer-name-status
	       (setq result-buffer-name (cdr (assq 'result mvc-local-buffer-name-list)))
	       (setq async-process-buffer-name (cdr (assq 'process-async mvc-local-buffer-name-list))))
	     (save-current-buffer
	       (save-selected-window
		 (switch-to-buffer-other-window (get-buffer-create result-buffer-name))
		 (setq buffer-undo-list t)
		 (setq buffer-read-only nil)
		 (erase-buffer)
		 (insert-buffer-substring async-process-buffer-name)
		 (setq buffer-read-only t))))
	   (mvc-async-set-status-variable 'mvc-local-async-process nil)
	   (let ((status-buffer-name mvc-async-local-buffer-name-status))
	     (kill-buffer (process-buffer process))
	     (with-current-buffer status-buffer-name
	       (mvc-async-status nil t)))))
	(t
	 (with-current-buffer (process-buffer process)
	   (mvc-async-set-status-variable 'mvc-local-async-process nil))
	 (kill-buffer (process-buffer process))
	 (message "update process error"))))

(defun mvc-async-update ()
  (let ((status-buffer (current-buffer)))
    (if mvc-local-async-process
	(message mvc-message-process-already-running)

      (mvc-status-save-point)

      (setq buffer-read-only nil)
      (setq overwrite-mode t)
      (save-excursion
	(goto-char (point-min))
	(insert mvc-running-header-head)
	(insert (nth 0 (cddr (assq mvc-local-program
				   (cdr (assq 'update mvc-default-option-list-system))))))
	(insert mvc-running-header-tail)
	(let ((p (point)))
	  (end-of-line)
	  (delete-region p (point))))
      (setq overwrite-mode nil)
      (setq buffer-read-only t)

      (setq mvc-local-async-process-last-command (nth 0 (cddr (assq mvc-local-program
								    (cdr (assq 'update mvc-default-option-list-system))))))
      (let ((process-connection-type mvc-default-process-connection-type)
	    (async-process-name (cdr (assq 'process-async mvc-local-buffer-name-list)))
	    (async-process-buffer-name (cdr (assq 'process-async mvc-local-buffer-name-list))))
	(setq mvc-local-async-process (apply 'start-process
					     async-process-name
					     async-process-buffer-name
					     mvc-local-program-name
					     (cddr (assq mvc-local-program
							 (cdr (assq 'update mvc-default-option-list-system))))))
	(with-current-buffer async-process-buffer-name
	  (set (make-local-variable 'mvc-async-local-buffer-name-status) (buffer-name status-buffer))))
      (set-process-sentinel mvc-local-async-process 'mvc-async-update-process-sentinel))))


(defun mvc-async-push-pull-process-sentinel (process event)
  (cond ((string= event "finished\n")
	 (with-current-buffer (process-buffer process)
	   (let (result-buffer-name
		 async-process-buffer-name)
	     (with-current-buffer mvc-async-local-buffer-name-status
	       (setq result-buffer-name (cdr (assq 'result mvc-local-buffer-name-list)))
	       (setq async-process-buffer-name (cdr (assq 'process-async mvc-local-buffer-name-list)))
	       (if (string= mvc-local-async-process-last-command "push")
		   (setq mvc-local-last-execute-time-push (current-time))
		 (setq mvc-local-last-execute-time-pull (current-time))))
	     (save-current-buffer
	       (save-selected-window
		 (switch-to-buffer-other-window (get-buffer-create result-buffer-name))
		 (setq buffer-undo-list t)
		 (setq buffer-read-only nil)
		 (erase-buffer)
		 (insert-buffer-substring async-process-buffer-name)
		 (setq buffer-read-only t))))
	   (with-current-buffer mvc-async-local-buffer-name-status
	     (mvc-status-draw)
	     (mvc-status-load-point))
	   (mvc-async-set-status-variable 'mvc-local-async-process nil)
	   (kill-buffer (process-buffer process))))
	(t
	 (with-current-buffer (process-buffer process)
	   (mvc-async-set-status-variable 'mvc-local-async-process nil))
	 (kill-buffer (process-buffer process))
	 (message "update process error"))))

(defun mvc-async-push-pull-core (status-buffer command-list path to-from)
  (if (yes-or-no-p (concat (nth 0 command-list) " " to-from " \"" path "\"?  "))
      (progn
	(set-buffer status-buffer)

	(mvc-status-save-point)

	(setq buffer-read-only nil)
	(setq overwrite-mode t)
	(save-excursion
	  (goto-char (point-min))
	  (insert mvc-running-header-head)
	  (insert (nth 0 command-list))
	  (insert mvc-running-header-tail)
	  (let ((p (point)))
	    (end-of-line)
	    (delete-region p (point))))
	(setq overwrite-mode nil)
	(setq buffer-read-only t)

	(setq mvc-local-async-process-last-command (nth 0 command-list))
	(let ((process-connection-type mvc-default-process-connection-type)
	      (async-process-name (cdr (assq 'process-async mvc-local-buffer-name-list)))
	      (async-process-buffer-name (cdr (assq 'process-async mvc-local-buffer-name-list))))
	  (setq mvc-local-async-process (apply 'start-process
					       async-process-name
					       async-process-buffer-name
					       mvc-local-program-name
					       command-list))
	  (with-current-buffer async-process-buffer-name
	    (set (make-local-variable 'mvc-async-local-buffer-name-status) (buffer-name status-buffer))))
	(set-process-sentinel mvc-local-async-process 'mvc-async-push-pull-process-sentinel))
    (message  "%s %s canceled!" mvc-local-program-name (nth 0 command-list))))

(defun mvc-async-push-mercurial ()
  (let ((command-list (cddr (assq mvc-local-program
				  (cdr (assq 'push mvc-default-option-list-system)))))
	(path "?")
	(program-name mvc-local-program-name)
	(status-buffer (current-buffer))
	(status-default-directory default-directory))
    (save-current-buffer
      (mvc-call-process-temporary status-buffer
				  program-name
				  (list "showconfig")
				  status-default-directory)
      (goto-char (point-min))
      (if (re-search-forward "^paths.default-push=\\(.+\\)" nil t)
	  (setq path (match-string 1))
	(when (re-search-forward "^paths.default=\\(.+\\)" nil t)
	  (setq path (match-string 1)))))

    (mvc-async-push-pull-core status-buffer command-list path "to")))

(defun mvc-async-push ()
  (cond ((eq mvc-local-program 'mercurial)
	 (mvc-async-push-mercurial))
	((eq mvc-local-program 'git)
	 (message "UNSUPPORTED"))
	((eq mvc-local-program 'bazaar)
	 (message "UNSUPPORTED"))
	((eq mvc-local-program 'subversion)
	 (message "UNSUPPORTED"))
	((eq mvc-local-program 'cvs)
	 (message "UNSUPPORTED"))
	(t
	 (message "UNKNOWN PROGRAM!"))))


(defun mvc-async-pull-mercurial ()
  (let ((command-list (cddr (assq mvc-local-program
				  (cdr (assq 'pull mvc-default-option-list-system)))))
	(path "?")
	(program-name mvc-local-program-name)
	(status-buffer (current-buffer))
	(status-default-directory default-directory))
    (save-current-buffer
      (mvc-call-process-temporary status-buffer
				  program-name
				  (list "showconfig")
				  status-default-directory)
      (goto-char (point-min))
      (when (re-search-forward "^paths.default=\\(.+\\)" nil t)
	(setq path (match-string 1))))

    (mvc-async-push-pull-core status-buffer command-list path "from")))

(defun mvc-async-pull-bazaar ()
  (let ((command-list (cddr (assq mvc-local-program
				  (cdr (assq 'pull mvc-default-option-list-system)))))
	(path "?")
	(program-name mvc-local-program-name)
	(status-buffer (current-buffer))
	(status-default-directory default-directory))
    (save-current-buffer
      (mvc-call-process-temporary status-buffer
				  program-name
				  (list "info")
				  status-default-directory)
      (goto-char (point-min))
      (when (re-search-forward "^ +parent branch: \\(.+\\)" nil t)
	(setq path (match-string 1))))

    (mvc-async-push-pull-core status-buffer command-list path "from")))

(defun mvc-async-pull ()
  (cond ((eq mvc-local-program 'mercurial)
	 (mvc-async-pull-mercurial))
	((eq mvc-local-program 'git)
	 (message "UNSUPPORTED"))
	((eq mvc-local-program 'bazaar)
	 (mvc-async-pull-bazaar))
	((eq mvc-local-program 'subversion)
	 (message "UNSUPPORTED"))
	((eq mvc-local-program 'cvs)
	 (message "UNSUPPORTED"))
	(t
	 (message "UNKNOWN PROGRAM!"))))




;;; mvc-status-mode

(defun mvc-status-mode (initial-program)
  "M(enu|ulti) Version Control Interface"

  (when initial-program
    (kill-all-local-variables)
    (use-local-map mvc-status-mode-map)
    (setq mode-name mvc-mode-name-status)
    (setq major-mode 'mvc-status-mode)

    (set (make-local-variable 'mvc-local-program) initial-program)
    (set (make-local-variable 'mvc-local-program-name) (cdr (assq mvc-local-program mvc-program-name)))
    (let ((base (concat "*mvc-"
			(cdr (assq mvc-local-program mvc-program-display-name)))))
      (set (make-local-variable 'mvc-local-buffer-name-list)
	   (list (cons 'status-base (concat base "-status*"))
		 (cons 'status (mvc-create-buffer-name (concat base "-status*") default-directory))
		 (cons 'diff (mvc-create-buffer-name (concat base "-diff*") default-directory))
		 (cons 'log (mvc-create-buffer-name (concat base "-log*") default-directory))
		 (cons 'commitlog (mvc-create-buffer-name (concat base "-commitlog*") default-directory))
		 (cons 'result (mvc-create-buffer-name (concat base "-result*") default-directory))
		 (cons 'especial (mvc-create-buffer-name (concat base "-especial*") default-directory))
		 (cons 'process-temporary (mvc-create-buffer-name (concat base "-PROCESS-temporary*") default-directory))
		 (cons 'process-async (mvc-create-buffer-name (concat base "-PROCESS-async*") default-directory)))))

    (mvc-status-update-header-line)

    (set (make-local-variable 'mvc-local-timer) nil)
    (set (make-local-variable 'mvc-local-timer-counter) 0)
    (set (make-local-variable 'mvc-local-timer-last-mode-line-string) "")
    (set (make-local-variable 'mvc-local-first-point-set-p) nil)
    (set (make-local-variable 'mvc-local-ready-p) nil)
    (set (make-local-variable 'mvc-local-async-process) nil)
    (set (make-local-variable 'mvc-local-last-execute-time-status) nil)
    (set (make-local-variable 'mvc-local-last-execute-time-update) nil)
    (set (make-local-variable 'mvc-local-last-execute-time-push) nil)
    (set (make-local-variable 'mvc-local-last-execute-time-pull) nil)
    (make-local-variable 'mvc-local-save-load-point)
    (make-local-variable 'mvc-local-save-load-file-name)
    (make-local-variable 'mvc-local-save-load-window-point-hash)
    (make-local-variable 'mvc-local-save-load-window-file-name-hash)
    (make-local-variable 'mvc-local-save-load-window-extension-hash)
    (make-local-variable 'mvc-local-save-load-file-list-end-point)
    (make-local-variable 'mvc-local-save-load-buffer-list)
    (set (make-local-variable 'mvc-local-async-process-last-command) "")
    (make-local-variable 'mvc-local-files)
    (set (make-local-variable 'mvc-local-display-unknown-p) mvc-default-status-display-unknown)
    (make-local-variable 'mvc-local-display-unknown-masks)
    (set (make-local-variable 'mvc-local-display-unmodified-p) mvc-default-status-display-unmodified)
    (make-local-variable 'mvc-local-display-unmodified-masks)
    (set (make-local-variable 'mvc-local-display-backup-p) mvc-default-status-display-backup)
    (make-local-variable 'mvc-local-display-backup-masks)
    (set (make-local-variable 'mvc-local-display-ignore-p) mvc-default-status-display-ignore)
    (make-local-variable 'mvc-local-display-ignore-masks)
    (set (make-local-variable 'mvc-local-recursive-p) t)
    (catch 'mapcar
      (mapcar #'(lambda (a)
		  (when (cdr a)
		    (mapcar #'(lambda (path)
				(when (or (string-match path default-directory)
					  (string-match path (expand-file-name default-directory)))
				  (setq mvc-local-recursive-p nil)
				  (throw 'mapcar nil)))
			    (cdr a))))
	      mvc-default-ignore-recursive-regexp-list))

    (set (make-local-variable 'mvc-local-file-list-begin-point) (point-min))
    (set (make-local-variable 'mvc-local-file-list-end-point) (point-max))

    (set (make-local-variable 'mvc-local-marks) 0)
    (set (make-local-variable 'mvc-local-mark-hash) (make-hash-table :test 'equal))
    (set (make-local-variable 'mvc-local-code-hash) (make-hash-table :test 'equal))
    (set (make-local-variable 'mvc-local-information-hash) (make-hash-table :test 'equal))
    (set (make-local-variable 'mvc-local-point-hash) (make-hash-table :test 'equal))
    (set (make-local-variable 'mvc-local-after-save-hook-hash) (make-hash-table :test 'equal))
    (make-local-variable 'mvc-local-last-point)
    (set (make-local-variable 'mvc-local-last-window-configuration) nil)

    (add-hook 'kill-buffer-hook 'mvc-status-kill-buffer-hook))

  (when mvc-default-use-animation-timer
    (unless mvc-local-timer
      (setq mvc-local-timer (run-at-time "0 sec" 0.2 'mvc-status-timer-function (current-buffer)))))

  (run-hooks 'mvc-status-mode-hook)
  t)


(defun mvc-status-mode-find-file ()
  "find-file"
  (interactive)

  (let ((cursor-file-name (mvc-status-get-current-line-file-name)))
    (if cursor-file-name
	(find-file cursor-file-name)
      (message "unknown line!"))))


(defun mvc-status-mode-next ()
  "next"
  (interactive)

  (let ((column (current-column)))
    (forward-line)
    (move-to-column column)))


(defun mvc-status-mode-previous ()
  "previous"
  (interactive)

  (let ((column (current-column)))
    (forward-line -1)
    (move-to-column column)))


(defun mvc-status-mode-next-button-search ()
  (let ((candidates (point-max))
	tmp)
    (setq tmp (text-property-any (point) (point-max) 'face 'mvc-face-button-active))
    (when (and tmp (> candidates tmp))
      (setq candidates tmp))
    (setq tmp (text-property-any (point) (point-max) 'face 'mvc-face-button-inactive))
    (when (and tmp (> candidates tmp))
      (setq candidates tmp))
    (setq tmp (text-property-any (point) (point-max) 'face 'mvc-face-toggle-button-inactive))
    (when (and tmp (> candidates tmp))
      (setq candidates tmp))
    (setq tmp (text-property-any (point) (point-max) 'face 'mvc-face-toggle-button-active))
    (when (and tmp (> candidates tmp))
      (setq candidates tmp))
    (if (= candidates (point-max))
	nil
      candidates)))


(defun mvc-status-mode-next-button ()
  "next button"
  (interactive)

  (let ((p (next-single-property-change (point) 'face (current-buffer))))
    (if p
	(goto-char p)
      (goto-char (point-min)))
    (setq p (mvc-status-mode-next-button-search))
    (if p
	(goto-char p)
      (goto-char (point-min))
      (setq p (mvc-status-mode-next-button-search))
      (when p
	(goto-char p)))))


(defun mvc-status-mode-next-status ()
  "next status"
  (interactive)

  (when (>= (length mvc-status-buffer-list) 2)
    (let* ((last (1- (length mvc-status-buffer-list)))
	   (i last)
	   (previous last)
	   buffer)
      (while (and (>= i 0)
		  (setq buffer (nth i mvc-status-buffer-list)))
	(when (eq (current-buffer) buffer)
	  (setq previous (1- i)))
	(setq i (1- i)))
      (if (>= previous 0)
	  (switch-to-buffer (nth previous mvc-status-buffer-list))
	(switch-to-buffer (nth last mvc-status-buffer-list))))))


(defun mvc-status-mode-previous-status ()
  "previous status"
  (interactive)

  (when (>= (length mvc-status-buffer-list) 2)
    (let ((i 0)
	  (next 0)
	  buffer)
      (while (setq buffer (nth i mvc-status-buffer-list))
	(when (eq (current-buffer) buffer)
	  (setq next (1+ i)))
	(setq i (1+ i)))
      (if (nth next mvc-status-buffer-list)
	  (switch-to-buffer (nth next mvc-status-buffer-list))
	(switch-to-buffer (nth 0 mvc-status-buffer-list))))))


(defun mvc-status-mode-beginning-of-list ()
  "beginning-of-list"
  (interactive)

  (if mvc-local-async-process
      (progn
	(goto-char (point-min))
	(forward-line 2))
    (mvc-status-beginning-of-list)))


(defun mvc-status-mode-end-of-list ()
  "end-of-list"
  (interactive)

  (if mvc-local-async-process
      (progn
	(goto-char (point-max))
	(forward-line -13))
    (mvc-status-end-of-list)))


(defun mvc-status-mode-toggle-display-unknown ()
  "toggle display unknown"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (if mvc-local-display-unknown-p
	(setq mvc-local-display-unknown-p nil)
      (setq mvc-local-display-unknown-p t))

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-toggle-display-unmodified ()
  "toggle display unmodified"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (if mvc-local-display-unmodified-p
	(setq mvc-local-display-unmodified-p nil)
      (setq mvc-local-display-unmodified-p t))

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-toggle-display-backup ()
  "toggle display backup"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (if mvc-local-display-backup-p
	(setq mvc-local-display-backup-p nil)
      (setq mvc-local-display-backup-p t))

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-toggle-display-ignore ()
  "toggle display ignore"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (if mvc-local-display-ignore-p
	(setq mvc-local-display-ignore-p nil)
      (setq mvc-local-display-ignore-p t))

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-toggle-recursive ()
  "toggle status recursive"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (if (eq mvc-local-program 'subversion)
	(progn
	  (if mvc-local-recursive-p
	      (setq mvc-local-recursive-p nil)
	    (setq mvc-local-recursive-p t))

	  (mvc-status-draw-with-save-load-point))
      (message "recursive control unsupported"))))


(defun mvc-status-mode-mark ()
  "mark"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (let ((cursor-file-name (mvc-status-get-current-line-file-name)))
      (if cursor-file-name
	  (progn
	    (unless (gethash cursor-file-name mvc-local-mark-hash)
	      (setq mvc-local-marks (1+ mvc-local-marks)))
	    (puthash cursor-file-name "*" mvc-local-mark-hash)
	    (goto-char (gethash cursor-file-name mvc-local-point-hash))
	    (setq buffer-read-only nil)
	    (delete-char 1)
	    (insert "*")
	    (setq buffer-read-only t)
	    (forward-line)
	    (mvc-status-redraw-footer-marks))
	(message "file-name not found")))))


(defun mvc-status-mode-unmark ()
  "unmark"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (let ((cursor-file-name (mvc-status-get-current-line-file-name)))
      (if cursor-file-name
	  (progn
	    (when (gethash cursor-file-name mvc-local-mark-hash)
	      (setq mvc-local-marks (1- mvc-local-marks)))
	    (puthash cursor-file-name nil mvc-local-mark-hash)
	    (goto-char (gethash cursor-file-name mvc-local-point-hash))
	    (setq buffer-read-only nil)
	    (delete-char 1)
	    (insert " ")
	    (setq buffer-read-only t)
	    (forward-line)
	    (mvc-status-redraw-footer-marks))
	(message "file-name not found")))))


(defun mvc-status-mode-unmark-all ()
  "unmark all"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (setq mvc-local-marks 0)
    (maphash #'(lambda (key value)
		 (puthash key nil mvc-local-mark-hash))
	     mvc-local-mark-hash)

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-mark-unknown ()
  "mark unknown"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (maphash #'(lambda (key value)
		 (when (string= (gethash key mvc-local-code-hash) "?")
		   (unless (string= value "*")
		     (setq mvc-local-marks (1+ mvc-local-marks))
		     (puthash key "*" mvc-local-mark-hash))))
	     mvc-local-mark-hash)

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-mark-add ()
  "mark add"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (maphash #'(lambda (key value)
		 (when (string= (gethash key mvc-local-code-hash) "A")
		   (unless (string= value "*")
		     (setq mvc-local-marks (1+ mvc-local-marks))
		     (puthash key "*" mvc-local-mark-hash))))
	     mvc-local-mark-hash)

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-mark-remove ()
  "mark remove"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (maphash #'(lambda (key value)
		 (when (string= (gethash key mvc-local-code-hash) "D")
		   (unless (string= value "*")
		     (setq mvc-local-marks (1+ mvc-local-marks))
		     (puthash key "*" mvc-local-mark-hash))))
	     mvc-local-mark-hash)

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-mark-modified ()
  "mark modified"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (maphash #'(lambda (key value)
		 (when (string= (gethash key mvc-local-code-hash) "M")
		   (unless (string= value "*")
		     (setq mvc-local-marks (1+ mvc-local-marks))
		     (puthash key "*" mvc-local-mark-hash))))
	     mvc-local-mark-hash)

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-mark-path-regexp-core (arg)
  "mark path regexp"

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (let ((regexp (completing-read (if arg
				       "unmark regexp: "
				     "mark regexp: ")
				   '(("Makefile$")
				     ("\.cpp$")))))
      (maphash #'(lambda (key value)
		   (when (string-match regexp key)
		     (if arg
			 (when (string= value "*")
			   (setq mvc-local-marks (1- mvc-local-marks))
			   (puthash key nil mvc-local-mark-hash))
		       (unless (string= value "*")
			 (setq mvc-local-marks (1+ mvc-local-marks))
			 (puthash key "*" mvc-local-mark-hash)))))
	       mvc-local-mark-hash))

    (mvc-status-draw-with-save-load-point)))


(defun mvc-status-mode-mark-path-regexp (arg)
  "mark path regexp"
  (interactive "P")

  (mvc-status-mode-mark-path-regexp-core arg))


(defun mvc-status-mode-unmark-path-regexp (arg)
  "unmark path regexp"
  (interactive "P")

  (mvc-status-mode-mark-path-regexp-core (not arg)))


(defun mvc-status-mode-push ()
  "push"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (mvc-async-push)))


(defun mvc-status-mode-pull ()
  "pull"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (mvc-async-pull)))


(defun mvc-status-mode-add ()
  "add"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (mvc-command-add)))


(defun mvc-status-mode-annotate ()
  "annotate"
  (interactive)

  (when (mvc-command-annotate)
    (switch-to-buffer-other-window (cdr (assq 'process-temporary mvc-local-buffer-name-list)))))


(defun mvc-status-mode-commit ()
  "commit"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (mvc-command-commit)))


(defun mvc-status-mode-log (arg)
  "log"
  (interactive "P")

  (let ((log-buffer-name (cdr (assq 'log mvc-local-buffer-name-list))))
    (when (mvc-command-log arg)
      (switch-to-buffer-other-window log-buffer-name))))


(defun mvc-status-mode-revert ()
  "revert"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (mvc-command-revert)))


(defun mvc-status-mode-status ()
  "status"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (mvc-async-status)))


(defun mvc-status-mode-remove ()
  "remove"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (mvc-command-remove)))


(defun mvc-status-mode-quit ()
  "quit"
  (interactive)

  (if (yes-or-no-p (concat "quit? "))
      (progn
	(mapcar #'(lambda (a)
		    (when (and (get-buffer (cdr a))
			       (not (eq (car a) 'status)))
		      (kill-buffer (cdr a))))
		mvc-local-buffer-name-list)
	(let (new-list first after)
	  (catch 'mapcar
	    (mapcar #'(lambda (a)
			(when (and (not (eq a (current-buffer)))
				   (string-match "^\\*mvc-[^-]+-status\\*" (buffer-name a)))
			  (setq first a)
			  (throw 'mapcar nil)))
		    (buffer-list)))
	  (when first
	    (mapcar #'(lambda (a)
			(unless (eq first a)
			  (setq after (append after (list a)))))
		    (buffer-list))
	    (setq new-list (append (list first) after))
	    (while new-list
	      (bury-buffer (car new-list))
	      (setq new-list (cdr new-list)))))
	(kill-buffer (cdr (assq 'status mvc-local-buffer-name-list))))
    (message "canceled!")))


(defun mvc-status-mode-rename (file-name)
  "rename"
  (interactive (let ((insert-default-directory nil))
		 (list (read-file-name (concat "rename \""
					       (mvc-status-get-current-line-file-name)
					       "\" to: ")))))

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (mvc-command-rename file-name)))


(defun mvc-status-mode-update ()
  "update"
  (interactive)

  (if mvc-local-async-process
      (message mvc-message-process-already-running)
    (mvc-async-update)))


(defun mvc-status-mode-diff-only-current (arg)
  "diff"
  (interactive "P")

  (let ((diff-buffer-name (cdr (assq 'diff mvc-local-buffer-name-list))))
    (when (mvc-command-diff-only-current arg)
      (with-current-buffer (get-buffer-create diff-buffer-name)
	(diff-mode))
      (display-buffer diff-buffer-name))))


(defun mvc-status-mode-diff-current-or-mark (arg)
  "diff"
  (interactive "P")

  (let ((diff-buffer-name (cdr (assq 'diff mvc-local-buffer-name-list))))
    (when (mvc-command-diff-current-or-mark arg)
      (with-current-buffer (get-buffer-create diff-buffer-name)
	(diff-mode))
      (display-buffer diff-buffer-name))))


(defun mvc-status-mode-especial ()
  "especial"
  (interactive)

  (let ((program mvc-local-program)
	(file-name-list (mvc-command-get-current-or-mark-file-name-list nil))
	(status-buffer (current-buffer))
	especial-buffer-name)
    (with-current-buffer status-buffer
      (setq especial-buffer-name (cdr (assq 'especial mvc-local-buffer-name-list))))
    (cond ((eq program 'mercurial)
	   (message "UNSUPPORTED"))
	  ((eq program 'git)
	   (message "UNSUPPORTED"))
	  ((eq program 'bazaar)
	   (message "UNSUPPORTED"))
	  ((eq program 'subversion)
	   (if file-name-list
	       (progn
		 (pop-to-buffer (get-buffer-create especial-buffer-name))
		 (mvc-especial-mode status-buffer)

		 (mvc-especial-mode-draw-internal status-buffer)
		 (goto-char (point-min))
		 (re-search-forward "^SET" nil t)
		 (beginning-of-line))
	     (when (get-buffer especial-buffer-name)
	       (kill-buffer especial-buffer-name))
	     (message "especial failed (no target file)")))
	  ((eq program 'cvs)
	   (message "UNSUPPORTED"))
	  (t
	   (message "UNKNOWN PROGRAM!")))))




;;; mvc-commitlog-mode

(defun mvc-commitlog-mode (status-buffer)
  (interactive)

  (kill-all-local-variables)
  (use-local-map mvc-commitlog-mode-map)
  (setq mode-name mvc-mode-name-commitlog)
  (setq major-mode 'mvc-commitlog-mode)

  (set (make-local-variable 'mvc-commitlog-mode-local-buffer-name-status) (buffer-name status-buffer))

  (message "\\C-c\\C-c to commit")

  (run-hooks 'mvc-commitlog-mode-hook))


(defun mvc-commitlog-mode-done ()
  (interactive)

  (let ((tmpfile (make-temp-name (concat mvc-default-tmp-directory "/mvcel")))
	commitlog-buffer-name)
    (with-current-buffer mvc-commitlog-mode-local-buffer-name-status
      (setq commitlog-buffer-name (cdr (assq 'commitlog mvc-local-buffer-name-list))))
    (with-temp-file tmpfile
      (insert-buffer-substring commitlog-buffer-name))
    (with-current-buffer mvc-commitlog-mode-local-buffer-name-status
      (goto-char mvc-local-last-point)
      (save-current-buffer
	(cond ((eq mvc-local-program 'mercurial)
	       (mvc-command-current-or-mark 'commit
					    nil
					    nil
					    tmpfile))
	      ((eq mvc-local-program 'git)
	       (mvc-command-current-or-mark 'commit
					    nil
					    nil
					    tmpfile))
	      ((eq mvc-local-program 'bazaar)
	       (mvc-command-current-or-mark 'commit
					    nil
					    nil
					    tmpfile))
	      ((eq mvc-local-program 'subversion)
	       (mvc-command-current-or-mark 'commit
					    nil
					    nil
					    tmpfile))
	      ((eq mvc-local-program 'cvs)
	       (message "UNSUPPORTED"))
	      (t
	       (message "UNKNOWN PROGRAM!"))))
      (delete-file tmpfile)

      (mvc-async-status)
      (set-window-configuration mvc-local-last-window-configuration)
      (save-current-buffer
	(save-selected-window
	  (let ((status-buffer (current-buffer)))
	    (switch-to-buffer-other-window (get-buffer-create (cdr (assq 'result mvc-local-buffer-name-list))))
	    (mvc-show-call-process-temporary-result status-buffer))))))

  (fundamental-mode))




;;; mvc-log-mode

(defun mvc-log-mode ()
  (interactive)

  (kill-all-local-variables)
  (use-local-map mvc-log-mode-map)
  (setq mode-name mvc-mode-name-log)
  (setq major-mode 'mvc-log-mode)
  (setq buffer-read-only nil)

  (save-excursion
    (goto-char (point-min))
    (while (or (re-search-forward "^changeset: +[0-9]+:[0-9a-f]+" nil t)
	       (re-search-forward "^r[0-9]+ +|" nil t))
      (beginning-of-line)
      (let ((start (point)))
	(forward-line)
	(backward-char 1)
	(set-text-properties start (point) (list 'face 'mvc-face-log-revision)))))

  (setq buffer-read-only t)

  (run-hooks 'mvc-log-mode-hook))


(defun mvc-log-mode-next ()
  (interactive)

  (catch 'loop
    (while t
      (let ((start (next-property-change (point))))
	(if start
	    (progn
	      (goto-char start)
	      (recenter 0)
	      (when (eq (get-text-property (point) 'face) 'mvc-face-log-revision)
		(throw 'loop nil)))
	  (goto-char (point-min))
	  (throw 'loop nil))))))


(defun mvc-log-mode-previous ()
  (interactive)

  (catch 'loop
    (let ((minp (eq (point) (point-min))))
      (while t
	(let ((start (previous-property-change (point))))
	  (if start
	      (progn
		(goto-char start)
		(recenter 0)
		(when (eq (get-text-property (point) 'face) 'mvc-face-log-revision)
		  (throw 'loop nil)))
	    (if minp
		(progn
		  (goto-char (point-max))
		  (mvc-log-mode-previous))
	      (goto-char (point-min)))
	    (throw 'loop nil)))))))


(defun mvc-log-mode-return ()
  (interactive)

    (save-excursion
      (let ((limit (point)))
	(end-of-line)
	(setq limit (point))
	(beginning-of-line)
	(if (or (re-search-forward "^\\(changeset\\): +\\([0-9]+:[0-9a-f]+\\)" limit t)
		(re-search-forward "^\\(r\\)\\([0-9]+\\) +|" limit t))
	    (let ((key (match-string 1))
		  (revision (match-string 2)))
	      (cond ((string= key "changeset")
		     (message (concat "Mercurial revision " revision)))
		    ((string= key "r")
		     (message (concat "Subversion revision " revision)))
		    (t
		     (message "UNKNOWN PROGRAM!!"))))
	(message "not found")))))




;;; mvc-especial-mode

(defun mvc-especial-mode (status-buffer)
  (interactive)

  (kill-all-local-variables)
  (use-local-map mvc-especial-mode-map)
  (setq mode-name mvc-mode-name-especial)
  (setq major-mode 'mvc-especial-mode)

  (set (make-local-variable 'mvc-especial-mode-local-buffer-name-status) (buffer-name status-buffer))
  (set (make-local-variable 'mvc-especial-mode-local-prop-recursive-p) nil)

  (run-hooks 'mvc-especial-mode-hook))


(defun mvc-especial-mode-insert-toggle-svn-recursive ()
  (mvc-status-insert-toggle-button mvc-especial-mode-local-prop-recursive-p
				   "recursive enabled "
				   "recursive disabled"
				   'mvc-especial-mode-toggle-svn-recursive)
  (insert "  propset/propdel --recursive\n"))


(defun mvc-especial-mode-draw-internal-insert-all-file-name-list (file-name-list)
  (catch 'mapcar
    (let ((loop 0))
      (mapcar #'(lambda (a)
		  (setq loop (1+ loop))
		  (when (> loop 3)
		    (mvc-insert-with-face " ..." 'mvc-face-especial-path)
		    (throw 'mapcar nil))
		  (mvc-insert-with-face (concat " " a) 'mvc-face-especial-path))
	      file-name-list)))
  (insert "\n"))


(defun mvc-especial-mode-draw-internal (status-buffer)
  (let (file-name-list
	program
	temporary-process-buffer-name)
    (with-current-buffer status-buffer
      (setq file-name-list (mvc-command-get-current-or-mark-file-name-list nil))
      (setq program mvc-local-program)
      (setq temporary-process-buffer-name (cdr (assq 'process-temporary mvc-local-buffer-name-list))))
    (setq buffer-read-only nil)
    (erase-buffer)

    (insert "mvc especial (Subversion property) mode\n\n")

    (mapcar #'(lambda (a)
		(mvc-especial-insert-button (nth 0 a) (nth 2 a))
		(insert (concat "  " (nth 1 a) "\n")))
	    (cdr (assq program mvc-default-especial-list)))
    (mvc-especial-mode-insert-toggle-svn-recursive)

    (insert "================\n")
    (when (>= (length file-name-list) 2)
      (mvc-especial-insert-button "SET ALL"
				  'mvc-especial-mode-svn-propset
				  (cons 'path file-name-list))
      (mvc-especial-mode-draw-internal-insert-all-file-name-list file-name-list)
      (mvc-especial-insert-button "DEL ALL"
				  'mvc-especial-mode-svn-propdel
				  (cons 'path file-name-list))
      (mvc-especial-mode-draw-internal-insert-all-file-name-list file-name-list))
    (mapcar #'(lambda (a)
		(mvc-especial-insert-button "SET"
					    'mvc-especial-mode-svn-propset
					    (cons 'path (list a)))
		(insert " ")
		(mvc-insert-with-face (concat a "\n") 'mvc-face-especial-path)
		(with-current-buffer status-buffer
		  (mvc-call-process-temporary status-buffer
					      mvc-local-program-name
					      (list "proplist"
						    a)
					      default-directory)
		  (goto-char (point-min))
		  (save-excursion
		    (while (and (= (forward-line) 0)
				(char-after))
		      (re-search-forward " +\\(.+\\)" nil t)
		      (let ((property (match-string 1)))
			(beginning-of-line)
			(insert "        ")
			(mvc-especial-insert-button "GET"
						    'mvc-especial-mode-svn-propget
						    (cons 'path a)
						    (cons 'property property))
			(insert " ")
			(mvc-especial-insert-button "SET"
						    'mvc-especial-mode-svn-propset
						    (cons 'path (list a))
						    (cons 'property property))
			(insert " ")
			(mvc-especial-insert-button "DEL"
						    'mvc-especial-mode-svn-propdel
						    (cons 'path (list a))
						    (cons 'property property)))))
		  (delete-region (point) (progn
					   (forward-line)
					   (point))))
		(insert-buffer-substring temporary-process-buffer-name))
	    file-name-list)
    (insert "================\n")
    (insert (format "%4d file(s)\n" (length file-name-list)))

    (setq buffer-read-only t)

    (message "especial ...done")))


(defun mvc-especial-mode-draw ()
  "draw"
  (interactive)

  (let (program
	file-name-list
	(status-buffer (get-buffer mvc-especial-mode-local-buffer-name-status))
	especial-buffer-name)
    (with-current-buffer status-buffer
      (setq program mvc-local-program)
      (setq file-name-list (mvc-command-get-current-or-mark-file-name-list nil))
      (setq especial-buffer-name (cdr (assq 'especial mvc-local-buffer-name-list))))
    (cond ((eq program 'mercurial)
	   (message "UNSUPPORTED"))
	  ((eq program 'git)
	   (message "UNSUPPORTED"))
	  ((eq program 'bazaar)
	   (message "UNSUPPORTED"))
	  ((eq program 'subversion)
	   (if file-name-list
	       (progn
		 (let (backup-recursive-p)
		   (when (get-buffer especial-buffer-name)
		     (with-current-buffer especial-buffer-name
		       (setq backup-recursive-p mvc-especial-mode-local-prop-recursive-p)))
		   (pop-to-buffer (get-buffer-create especial-buffer-name))
		   (mvc-especial-mode status-buffer)
		   (when backup-recursive-p
		     (setq mvc-especial-mode-local-prop-recursive-p backup-recursive-p)))
		 (mvc-especial-mode-draw-internal status-buffer)
		 (goto-char (point-min))
		 (re-search-forward "^SET" nil t)
		 (beginning-of-line))
	     (when (get-buffer especial-buffer-name)
	       (kill-buffer especial-buffer-name))
	     (message "especial failed (no target file)")))
	  ((eq program 'cvs)
	   (message "UNSUPPORTED"))
	  (t
	   (message "UNKNOWN PROGRAM!")))))


(defun mvc-especial-insert-button (label function &optional property-a property-b)
  (let ((map (copy-keymap mvc-especial-mode-map))
	(start (point)))
    (define-key map [mouse-1] function)
    (define-key map "\C-m" function)
    (mvc-insert-with-face label
			  'mvc-face-button-active)
    (when property-a
      (put-text-property start
			 (point)
			 (car property-a) (cdr property-a)))
    (when property-b
      (put-text-property start
			 (point)
			 (car property-b) (cdr property-b)))
    (put-text-property start
		       (point)
		       'local-map map)))


(defun mvc-especial-mode-next ()
  "next"
  (interactive)

  (let ((p (next-single-property-change (point) 'face (current-buffer))))
    (if p
	(goto-char p)
      (goto-char (point-min)))
    (setq p (text-property-any (point) (point-max) 'face 'mvc-face-button-active))
    (if p
	(goto-char p)
      (goto-char (point-min))
      (setq p (text-property-any (point) (point-max) 'face 'mvc-face-button-active))
      (when p
	(goto-char p)))))


(defun mvc-especial-mode-previous-previous (loop)
  (let (p)
    (while (> loop 0)
      (setq loop (1- loop))
      (setq p (previous-single-property-change (point) 'face))
      (when p
	(goto-char p)))))


(defun mvc-especial-mode-previous ()
  "previous"
  (interactive)

  (let ((p (previous-single-property-change (point) 'face (current-buffer))))
    (if p
	(progn
	  (goto-char p)
	  (if (get-text-property p 'face)
	      (mvc-especial-mode-previous-previous 2)
	    (mvc-especial-mode-previous-previous 1)))
      (goto-char (point-max))
      (mvc-especial-mode-previous-previous 2))))


(defun mvc-especial-mode-call-process-temporary (status-buffer arg-list)
  (with-current-buffer status-buffer
    (mvc-call-process-temporary status-buffer
				mvc-local-program-name
				arg-list
				default-directory)))


(defun mvc-especial-mode-puthash-and-draw-status (status-buffer path)
  (with-current-buffer status-buffer
    (mapcar #'(lambda (a)
		(puthash (concat (expand-file-name default-directory) a) "m" mvc-local-after-save-hook-hash))
	    path)
    (mvc-status-draw-with-save-load-point)))


(defun mvc-especial-mode-svn-add-ignore ()
  "svn set ignore"
  (interactive)

  (let ((start-column (current-column))
	(status-buffer-name mvc-especial-mode-local-buffer-name-status)
	(prop-recursive-p mvc-especial-mode-local-prop-recursive-p))
    (mvc-especial-mode-call-process-temporary (get-buffer status-buffer-name)
					      (list "propget"
						    "--strict"
						    "svn:ignore"
						    "."))
    (mvc-especial-mode-puthash-and-draw-status (get-buffer status-buffer-name) (list "."))
    (let ((add-length 0)
	  current-or-mark-list
	  temporary-process-buffer-name)
      (with-current-buffer status-buffer-name
	(setq current-or-mark-list (mvc-command-get-current-or-mark-file-name-list nil))
	(setq temporary-process-buffer-name (cdr (assq 'process-temporary mvc-local-buffer-name-list))))
      (mapcar #'(lambda (a)
		  (save-excursion
		    (if (string= a ".")
			(message (concat "\".\" cannot set ignore"))
		      (with-current-buffer temporary-process-buffer-name
			(goto-char (point-min))
			(if (re-search-forward (concat "^" (regexp-quote a) "$") nil t)
			    (message (concat "    ignore already:" a))
			  (setq add-length (1+ add-length))
			  (insert (concat a "\n")))))))
	      current-or-mark-list)
      (if (> add-length 0)
	  (progn
	    (mvc-show-call-process-temporary-result (get-buffer status-buffer-name) t)
	    (mvc-especial-commit-mode (get-buffer status-buffer-name) "svn:ignore" "." nil start-column prop-recursive-p)
	    (message (format "\"\\C-c\\C-c\" to set ignore %d file(s)" add-length)))
	(message "propset failed (no target file)")))))


(defun mvc-especial-mode-svn-remove-ignore ()
  "svn remove ignore"
  (interactive)

  (let ((start-column (current-column))
	(status-buffer-name mvc-especial-mode-local-buffer-name-status)
	(prop-recursive-p mvc-especial-mode-local-prop-recursive-p))
    (mvc-especial-mode-call-process-temporary (get-buffer status-buffer-name)
					      (list "propget"
						    "--strict"
						    "svn:ignore"
						    "."))
    (mvc-especial-mode-puthash-and-draw-status (get-buffer status-buffer-name) (list "."))
    (let ((remove-length 0)
	  current-or-mark-list
	  temporary-process-buffer-name)
      (with-current-buffer status-buffer-name
	(setq current-or-mark-list (mvc-command-get-current-or-mark-file-name-list nil))
	(setq temporary-process-buffer-name (cdr (assq 'process-temporary mvc-local-buffer-name-list))))
      (mapcar #'(lambda (a)
		  (save-excursion
		    (if (string= a ".")
			(message (concat "\".\" cannot remove ignore"))
		      (with-current-buffer temporary-process-buffer-name
			(goto-char (point-min))
			(if (re-search-forward (concat "^" (regexp-quote a) "$") nil t)
			    (progn
			      (setq remove-length (1+ remove-length))
			      (replace-match "")
			      (delete-blank-lines))
			  (message (concat "    ignore yet:" a)))))))
	      current-or-mark-list)
      (if (> remove-length 0)
	  (progn
	    (mvc-show-call-process-temporary-result (get-buffer status-buffer-name) t)
	    (mvc-especial-commit-mode (get-buffer status-buffer-name) "svn:ignore" "." nil start-column prop-recursive-p)
	    (message (format "\"\\C-c\\C-c\" to remove ignore %d file(s)" remove-length)))
	(message "propdel failed (no target file)")))))


(defun mvc-especial-mode-svn-propget ()
  "svn propget"
  (interactive)

  (let ((status-buffer-name mvc-especial-mode-local-buffer-name-status))
    (mvc-especial-mode-call-process-temporary (get-buffer status-buffer-name)
					      (list "propget"
						    "--strict"
						    (get-text-property (point) 'property)
						    (get-text-property (point) 'path)))
    (mvc-show-call-process-temporary-result (get-buffer status-buffer-name))))


(defun mvc-especial-mode-reset-cursor (property path start-column)
  (goto-char (point-min))
  (re-search-forward (concat "^SET +" (regexp-quote (nth 0 path)) "$") nil t)
  (let (limit)
    (save-excursion
      (if (re-search-forward "^SET +.+" nil t)
	  (progn
	    (beginning-of-line)
	    (setq limit (point)))
	(setq limit (point-max))))
    (if (re-search-forward (concat "^ +GET +SET +DEL +" (regexp-quote property) "$") limit t)
	(progn
	  (beginning-of-line)
	  (goto-char (+ (point) start-column)))
      (beginning-of-line))))


(defun mvc-especial-mode-svn-propset (arg)
  "svn propset"
  (interactive "P")

  (let ((property (get-text-property (point) 'property))
	(path (get-text-property (point) 'path))
	(start-point (point))
	(start-column (current-column))
	(status-buffer-name mvc-especial-mode-local-buffer-name-status)
	(prop-recursive-p mvc-especial-mode-local-prop-recursive-p)
	value)
    (unless property
      (setq property (completing-read (if mvc-especial-mode-local-prop-recursive-p
					  "property RECURSIVE: "
					"property: ")
				      '(("svn:ignore")
					("svn:keywords")
					("svn:executable")
					("svn:eol-style")
					("svn:mime-type")
					("svn:externals")
					("svn:needs-lock")))))
    (unless arg
      (cond ((or (string= property "svn:executable")
		 (string= property "svn:needs-lock"))
	     (setq value "*"))
	    ((string= property "svn:keywords")
	     (setq value (completing-read "property value: " '(("URL") ("HeadURL")
							       ("Author") ("LastChangedBy")
							       ("Date") ("LastChangedDate")
							       ("Rev") ("Revision") ("LastChangedRevision")
							       ("Id")))))
	    ((string= property "svn:eol-style")
	     (setq value (completing-read "property value: " '(("native")
							       ("LF")
							       ("CR")
							       ("CRLF")))))
	    ((string= property "svn:mime-type")
	     (setq value (completing-read "property value: " '(("application/")
							       ("binary/")
							       ("text/")))))))
    (if (or arg
	    (not value))
	(progn
	  (mvc-especial-mode-call-process-temporary (get-buffer status-buffer-name)
						    (append (list "propget" "--strict" property)
							    path))
	  (mvc-show-call-process-temporary-result (get-buffer status-buffer-name) t)
	  (mvc-especial-commit-mode (get-buffer status-buffer-name) property path start-point start-column prop-recursive-p)
	  (setq buffer-read-only nil)
	  (message (format "\"\\C-c\\C-c\" to propset")))
      (when value
	(mvc-especial-mode-call-process-temporary (get-buffer status-buffer-name)
						  (if prop-recursive-p
						      (append (list "propset" "--recursive" property value)
							      path)
						    (append (list "propset" property value)
							    path)))
	(if prop-recursive-p
	    (progn
	      (with-current-buffer status-buffer-name
		(mvc-async-status)))
	  (mvc-especial-mode-puthash-and-draw-status (get-buffer status-buffer-name) path))
	(mvc-show-call-process-temporary-result (get-buffer status-buffer-name))
	(mvc-especial-mode-draw-internal (get-buffer status-buffer-name))
	(if (= (length path) 1)
	    (mvc-especial-mode-reset-cursor property path start-column)
	  (goto-char start-point))))))


(defun mvc-especial-mode-svn-propdel ()
  "svn propdel"
  (interactive)

  (let ((path (get-text-property (point) 'path))
	(property (get-text-property (point) 'property))
	(start-point (point))
	(start-column (current-column))
	(status-buffer-name mvc-especial-mode-local-buffer-name-status))
    (unless property
      (setq property (completing-read (if mvc-especial-mode-local-prop-recursive-p
					  "property RECURSIVE: "
					"property: ")
				      '(("svn:ignore")
					("svn:keywords")
					("svn:executable")
					("svn:eol-style")
					("svn:mime-type")
					("svn:externals")
					("svn:needs-lock")))))
    (if (yes-or-no-p (concat (if mvc-especial-mode-local-prop-recursive-p
				 "propdel RECURSIVE? "
			       "propdel? ")))
	(progn
	  (mvc-especial-mode-call-process-temporary (get-buffer status-buffer-name)
						    (if mvc-especial-mode-local-prop-recursive-p
							(append (list "propdel" "--recursive" property)
								path)
						      (append (list "propdel" property)
							      path)))
	  (if mvc-especial-mode-local-prop-recursive-p
	      (progn
		(with-current-buffer status-buffer-name
		  (mvc-async-status)))
	    (mvc-especial-mode-puthash-and-draw-status (get-buffer status-buffer-name) path))
	  (mvc-show-call-process-temporary-result (get-buffer status-buffer-name))
	  (mvc-especial-mode-draw-internal (get-buffer status-buffer-name))
	  (if (= (length path) 1)
	      (mvc-especial-mode-reset-cursor property path start-column)
	    (goto-char start-point)))
      (message "canceled!"))))


(defun mvc-especial-mode-toggle-svn-recursive ()
  "toggle svn recursive"
  (interactive)

  (let ((status-buffer-name mvc-especial-mode-local-buffer-name-status)
	window-point-list)
    (if mvc-especial-mode-local-prop-recursive-p
	(setq mvc-especial-mode-local-prop-recursive-p nil)
      (setq mvc-especial-mode-local-prop-recursive-p t))
    (mapcar #'(lambda (a)
		(setq window-point-list (append window-point-list (list (cons a (window-point a))))))
	    (get-buffer-window-list (current-buffer) t))
    (save-excursion
      (goto-char (point-min))
      (search-forward "recursive")
      (beginning-of-line)
      (setq buffer-read-only nil)
      (let ((point (point)))
	(end-of-line)
	(delete-region point (1+ (point))))
      (mvc-especial-mode-insert-toggle-svn-recursive)
      (setq buffer-read-only t))
    (mapcar #'(lambda (a)
		(set-window-point (car a) (cdr a)))
	    window-point-list)))




;;; mvc-especial-commit-mode

(defun mvc-especial-commit-mode (status-buffer property path start-point start-column prop-recursive-p)
  (interactive)

  (kill-all-local-variables)
  (use-local-map mvc-especial-commit-mode-map)
  (setq mode-name mvc-mode-name-especial-commit)
  (setq major-mode 'mvc-especial-commit-mode)

  (set (make-local-variable 'mvc-especial-commit-mode-local-buffer-name-status) (buffer-name status-buffer))
  (set (make-local-variable 'mvc-especial-commit-mode-local-property) property)
  (set (make-local-variable 'mvc-especial-commit-mode-local-path) path)
  (set (make-local-variable 'mvc-especial-commit-mode-local-start-point) start-point)
  (set (make-local-variable 'mvc-especial-commit-mode-local-start-column) start-column)
  (set (make-local-variable 'mvc-especial-commit-mode-local-prop-recursive-p) prop-recursive-p)

  (run-hooks 'mvc-especial-commit-mode-hook))


(defun mvc-especial-commit-mode-done ()
  "svn proplist"
  (interactive)

  (let ((property mvc-especial-commit-mode-local-property)
	(path mvc-especial-commit-mode-local-path)
	(start-point mvc-especial-commit-mode-local-start-point)
	(start-column mvc-especial-commit-mode-local-start-column)
	(status-buffer-name mvc-especial-commit-mode-local-buffer-name-status))
    (let ((tmpfile (make-temp-name (concat mvc-default-tmp-directory "/mvcel")))
	  result-buffer-name)
      (with-current-buffer status-buffer-name
	(setq result-buffer-name (cdr (assq 'result mvc-local-buffer-name-list))))
      (with-temp-file tmpfile
	(insert-buffer-substring result-buffer-name))
      (mvc-especial-mode-call-process-temporary (get-buffer status-buffer-name)
						(if mvc-especial-commit-mode-local-prop-recursive-p
						    (append (list "propset" "--recursive" property)
							    path
							    (list "--file" tmpfile))
						  (append (list "propset" property)
							   path
							   (list "--file" tmpfile))))
      (mvc-especial-mode-puthash-and-draw-status (get-buffer status-buffer-name) path)
      (delete-file tmpfile))

    (fundamental-mode)

    (mvc-show-call-process-temporary-result (get-buffer status-buffer-name))

    (let (especial-buffer-name)
      (with-current-buffer status-buffer-name
	(setq especial-buffer-name (cdr (assq 'especial mvc-local-buffer-name-list))))
      (switch-to-buffer-other-window (get-buffer-create especial-buffer-name)))
    (mvc-especial-mode-draw-internal (get-buffer status-buffer-name))
    (if (= (length path) 1)
	(mvc-especial-mode-reset-cursor property path start-column)
      (goto-char start-point))))




;;; after-save-hook

(defun mvc-after-save-hook ()
  (when mvc-status-buffer-list
    (mapcar #'(lambda (a)
		(when (string-match "^\\*mvc-[^-]+-status\\*" (buffer-name a))
		  (let ((check-file-name buffer-file-name))
		    (with-current-buffer a
		      (when (and (boundp 'mvc-local-buffer-name-list)
				 (string= (cdr (assq 'status mvc-local-buffer-name-list)) (buffer-name a))
				 (gethash check-file-name mvc-local-after-save-hook-hash))
			(puthash check-file-name "m" mvc-local-after-save-hook-hash)
			(mvc-status-draw-with-save-load-point))))))
	    (buffer-list))))

(add-hook 'after-save-hook 'mvc-after-save-hook)




(provide 'mvc)
