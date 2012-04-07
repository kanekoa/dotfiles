;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;; [使い方]
;;;   ・kill-ring 選択用バッファを別ウィンドウに表示
;;;   ・p,n (j,k) で前と次の候補を選択し元編集バッファヘヤンク
;;;     C-p (previous-line),C-n (next-line) なら候補を移動するだけ．
;;;     SPC で現在行を選択．C-v (scroll-up) でスクロール
;;;   ・ヤンク領域は yank-pop 風に次々に切り替わる
;;;   ・yank-pop との整合性を確保．M-y に割り当てればひとまず同じように使
;;;     える（まだリング動作はできない…）．
;;;   ・表示中は他の操作はできない．RET (newline) で選択決定．
;;;     中止 (q, C-g (keyboard-quit), C-xo (other-window)) 時は
;;;     ヤンク領域を消去
;;;   ・d で現在行のキルリングを即座に消去
;;;   ・'.' で現在行をヤンクポインタに設定
;;;   ・t で各キルの行数表示を切り替え
;;;   ・^,_ でサマリー高さを変更
;;;   ・初期ウィンドウ高さは ~/.emacs 中で次のように行なう（デフォルト値 10）．
;;;     (setq kill-summary-window-height 10)

(autoload 'kill-summary "kill-summary" nil t)
(define-key global-map "\ey" 'kill-summary)
