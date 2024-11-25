;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with SPC . and enter text in its buffer.


(require 'consult)
(require 'ffap)
(require 'dash)
(require 'cc-mode)
(require 'zyt-ffap-handler)
(defconst zyt/prog-link-header-regexp
  "\\[\\[\\*\\*  \(bookmark--jump-via \"\\(.*\\)\" 'switch-to-buffer-other-window)  \\*\\*\\]\\]"
  )
(defvar zyt/prog-link-face font-lock-doc-face)
(defun zyt/prog-link--bookmark-handler(buf)
  (unless (get-buffer-window buf)
	(display-buffer buf)
	)
  )
(defun zyt/prog-goto-link()
  (interactive)
  (save-excursion
	(goto-char (pos-bol))
	(when
		(re-search-forward
		 zyt/prog-link-header-regexp
		 (pos-eol)
		 t
		 )
	  (let ((bookmark-str (match-string 1))
			(display-buffer-overriding-action
			 '(display-buffer-use-least-recent-window)
			 )
			)
		(if (consp (car (read-from-string bookmark-str)))
			(bookmark--jump-via (car (read-from-string bookmark-str)) 'zyt/prog-link--bookmark-handler)
		  (if (bookmark-get-bookmark bookmark-str t)
			  (bookmark--jump-via bookmark-str 'zyt/prog-link--bookmark-handler)
			(find-file-at-point bookmark-str)
			)
		  )
		)
	  )
	)
  )
;; https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace
;; [[**  (bookmark--jump-via "("(dir) Top" (front-context-string . "Dired-Preview: (") (rear-context-string . " Menu:\n\nEmacs\n* ") (position . 551) (last-modified 26426 63699 715916 0) (filename . "dir") (info-node . "Top") (handler . Info-bookmark-jump) (defaults "(dir) Top" "dir" "Top" "*info*"))" 'switch-to-buffer-other-window)  **]]
;; [[**  (bookmark--jump-via "("all-the-icons-dired-20231207.1324" (filename . "~/.emacs.d/elpa/all-the-icons-dired-20231207.1324/") (front-context-string . "all-the-icons-di") (rear-context-string . ".3k 06-03 16:57 ") (position . 419) (last-modified 26426 65092 923851 0) (defaults "all-the-icons-dired-20231207.1324"))" 'switch-to-buffer-other-window)  **]]
(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))
(defvar zyt/prog-temp-link nil
  "Store temporarily created link to later insert into program-mode files"
  )
(defun zyt/prog-store-link()
  "Create temporary link to insert into program-mode files"
  (interactive)
  ;; (setq zyt/prog-temp-link (string-replace "\n" "" (pp-to-string (bookmark-make-record))))
  (setq zyt/prog-temp-link (bookmark-make-record))
  nil
  )

(defvar consult--source-links
  `(:name "link buffer"
		  :narrow ?*
		  :hidden nil
		  :category buffer
		  :face consult-buffer
		  :items
		  ,(lambda()
			 (let ((display-buffer-overriding-action '((display-buffer-pop-up-frame) nil)))
			 (consult--buffer-query
			  :sort 'visibility
			  ;; :filter 'invert
			  :as #'consult--buffer-pair
			  :buffer-list (--keep (window-buffer it) (window-list))
			  :predicate
			  (lambda(buf)
				(and
				 (null (eq buf (current-buffer)))
				 ;; buffer should, either "Return the current buffer's file in a way useful for bookmarks." or provide customized ~bookmark-make-record-function~, to make ~bookmark-make-record~ success.
				 (with-current-buffer buf
				   (or (condition-case nil
						   ;; bookmark-buffer-file-name throw error in some buffers such as *Info* buffer
						   (bookmark-buffer-file-name)
						 (error nil))
					   (null (eq bookmark-make-record-function
								 'bookmark-make-record-default
								 ))))
				 )
				)
			  )
			 )
			 )
		  :state ,#'consult--buffer-state)
  )

;; (defun zyt()
;;   (consult--buffer-query
;;    :sort 'visibility
;;    ;; :filter 'invert
;;    :as #'consult--buffer-pair
;;    :buffer-list (--keep (window-buffer it) (window-list))
;;    ;; :buffer-list (buffer-list)
;;    :predicate
;;    (lambda(buf)
;; 	 (with-current-buffer buf
;; 	   t
;; 	   ;; (null (window-minibuffer-p (get-buffer-window)))
;; 	   ;; (window-minibuffer-p (get-buffer-window))
;; 	   )
;; 	 )
;;    )
;;   )

(defun _zyt/prog-store-link()
  "Create temporary link to insert into program-mode files"
  ;; (lt ((selected (consult--multi consult-buffer-sources
  (when-let* (
		 (cur-buf (current-buffer))
		 (consult-source (list consult--source-links))
		 (selected
		  (and (funcall (plist-get consult--source-links :items))
			   (consult--multi
				consult-source
				:require-match
				(confirm-nonexistent-file-or-buffer)
				:prompt "Insert link from buffer: "
				:history 'consult--buffer-history
				:sort nil))))
	
	(if (get-buffer (car selected))
		(prog1
			(with-current-buffer (car selected)
			  ;; (string-replace "\n" "" (pp-to-string (bookmark-make-record)))
			  (bookmark-make-record)
			  )
		  (pop-to-buffer cur-buf 'display-buffer-same-window)
		  )
	  ;; (and (ffap-url-p (car selected))
		   ;; (car selected))
	  (substring-no-properties (car selected))
	  )
	)
  )
(defun _zyt/prog-select-link-from-bookmarks()
  (progn
	(bookmark-maybe-load-default-file)
	(consult--read
	 (--map 
	  (substring-no-properties (bookmark-name-from-full-record it))
	  (bookmark-maybe-sort-alist))
	 :prompt "Insert link from bookmarks: "
	 ;; :state (consult--bookmark-preview)
	 :state
	 (lambda(action cand)
	   ;; (when (bookmark-get-handler (bookmark-bmenu-bookmark))
	   ;; (bookmark-get-bookmark (bookmark-bmenu-bookmark))
	   (let ((cur-buf (current-buffer)))
		 (progn
		   (ignore-errors
			 (when (or t
					   (funcall (or (bookmark-get-handler cand)
									'bookmark-default-handler)
								cand)) 
			   (save-excursion
				 (ignore-errors
				   (bookmark--jump-via cand 'switch-to-buffer-other-window)
				   )
				 ;; (pop-to-buffer cur-buf '(display-buffer-in-previous-window . ()) t)
				 (pop-to-buffer cur-buf '(display-buffer-use-least-recent-window . ()) t)
				 )
			   )
			 )
		   )
		 ))
	 ))
  )
;; (defsubst fontify--bm-link-line(map &optional bookmark-str)
(defun fontify--bm-link-line-old(map &optional bookmark-str whole-link-str)
  ;; (insert (format "[[**  (bookmark--jump-via \"%s\" 'switch-to-buffer-other-window)  **]]" bookmark-str))
  (set-text-properties
   (pos-bol) (pos-eol)
   `(
	 keymap ,map
	 mouse-face highlight
	 help-echo "mouse-2: visit this file in other window"
	 font-lock-fontified nil
	 ;; face org-document-title
	 ;; face tdr-font-mode
	 face org-link
	 )
   )
  (beginning-of-line)
  (when-let (
			 (bookmark-name
			  (and bookmark-str
				   (if (consp (car (read-from-string bookmark-str)))
					   (car (car (read-from-string bookmark-str)))
					 bookmark-str
					 )
				   )
			  ))
	;; (re-search-forward bookmark-name (pos-eol) 'noerror)
	(re-search-forward bookmark-name (pos-eol) 'noerror)
	(set-text-properties
	(+ (length comment-start) (pos-bol))
	 (match-beginning 0)
	 `(
	   invisible t
	   )
	 )
	(set-text-properties
	 (match-beginning 0)
	 (match-end 0)
	 `(
	   face "info-xref"
	   )
	 )
	(set-face-underline zyt/prog-link-face t)
	(set-text-properties
	 (match-end 0)
	 (- (pos-eol) (length comment-end))
	 `(
	   invisible t
	   )
	 )
	)
  )
;; (defsubst fontify--bm-link-line(map &optional bookmark-str)
(defun fontify--bm-link-line(map &optional bookmark-str whole-link-str)
  ;; (insert (format "[[**  (bookmark--jump-via \"%s\" 'switch-to-buffer-other-window)  **]]" bookmark-str))
  ;; cursor在最后一行行尾时，pos-eol 经常和预期结果不同；稳妥起见，先挪到行首
  (beginning-of-line)
  (set-text-properties
   (pos-bol) (pos-eol)
   `(
	 keymap ,map
	 mouse-face highlight
	 help-echo "mouse-2: visit this file in other window"
	 font-lock-fontified nil
	 ;; face org-document-title
	 ;; face tdr-font-mode
	 face org-link
	 )
   )
  (when-let (
			 ;; (zyt nil)
			 (bookmark-name
			  (and bookmark-str
				   (if (consp (car (read-from-string bookmark-str)))
					   (car (car (read-from-string bookmark-str)))
					 bookmark-str
					 )
				   )
			  ))
	;; (re-search-forward bookmark-name (pos-eol) 'noerror)
	(re-search-forward whole-link-str (pos-eol) t)
	(let (
		  (beginning-0 (match-beginning 0))
		  (end-0 (match-end 0))
		  )
	  (goto-char beginning-0)
	  (re-search-forward bookmark-str end-0 t)
	  (set-text-properties
	   beginning-0
	   (match-beginning 0)
	   `(
		 invisible t
		 )
	   )
	  ;; (set-text-properties
	  ;;  (match-beginning 0)
	  ;;  (match-end 0)
	  ;;  `(
	  ;; 	 face "info-xref"
	  ;; 	 )
	  ;;  )
	  (set-face-underline zyt/prog-link-face t)
	  (set-text-properties
	   (match-end 0)
	   end-0
	   `(
		 invisible t
		 )
	   )
	  )
	)
  )

(defun zyt/prog-insert-link(&optional default-label-name)
  (interactive "P")
  (let* (
		 (cur-buf (current-buffer))
		 (bookmark (or zyt/prog-temp-link
					   (_zyt/prog-store-link)
					   (_zyt/prog-select-link-from-bookmarks)
					   ))
		 (bookmark-str
		  (if (consp bookmark)
			  (string-replace "\n" "" (pp-to-string bookmark))
			bookmark
			)
		  )
		 link-name
		 )
	(when (and (consp bookmark) (null default-label-name))
	  (setq link-name (read-from-minibuffer
					   "Link Name:"
					   (if (consp bookmark) (car bookmark) bookmark-str)
					   )
			)
	  (setq bookmark (cons link-name (cdr bookmark)))
	  )
	(with-current-buffer cur-buf
	  (unless (current-line-empty-p)
		(if (featurep 'evil)
			(evil-open-below 1)
		  (end-of-line)
		  (newline))
		)
	  (if link-name
		  (if (consp bookmark)
			  (setq bookmark-str (string-replace "\n" "" (pp-to-string bookmark)))
			)
		)
	  (setq whole-link-str (format "[[**  (bookmark--jump-via \"%s\" 'switch-to-buffer-other-window)  **]]" bookmark-str))
	  ;; (setq whole-link-str (format "[[**(bookmark--jump-via:\"%s\"'switch-to-buffer-other-window)**]]" bookmark-str))
	  (insert whole-link-str)
	  ;; C mode 对于长度超过(含) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 的行做 comment-line后
	  ;; 如果选中区域进行indent操作，会将 /* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx */
	  ;; 转换成如下形式
	  ;; /* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
	  ;; */
	  ;; 导致font-lock出错;针对c-mode，change comment style to ~line comments~
	  (if (derived-mode-p 'c-mode)
		  (let (
				;; (c-block-comment-flag-orig c-block-comment-flag)
				)
			;; (c-toggle-comment-style -1)
			(comment-line 1)
			;; (if c-block-comment-flag-orig
				;; (c-toggle-comment-style 1)
				;; )
			)
		(comment-line 1)
		)
	  ;; [[**  (bookmark-jump "org link font lock")  **]]
	  ;; 如是最后一行，comment-line 会将cursor折返至当前行行首，而非下一行行首
	  (unless (= (pos-eol) (buffer-end 1))
		  (forward-line -1))
	  (let ((map (make-sparse-keymap)))
		(define-key map [down-mouse-1] 'zyt/prog-goto-link)
		;; (fontify--bm-link-line map bookmark-str)
		;; (fontify--bm-link-line map (regexp-quote (substring-no-properties bookmark-str)) (regexp-quote (substring-no-properties whole-link-str)))
		(if (consp bookmark)
			(fontify--bm-link-line map (regexp-quote (substring-no-properties (car bookmark))) (regexp-quote (substring-no-properties whole-link-str)))
		  (fontify--bm-link-line map (regexp-quote (substring-no-properties bookmark)) (regexp-quote (substring-no-properties whole-link-str)))
		  )
		)
	  (setq zyt/prog-temp-link nil)
	  )
	)
  nil
  )


;; [[**  (bookmark--jump-via "SCSI_Peripheral_Device_Type" 'switch-to-buffer-other-window)  **]]

;; [[**  (bookmark--jump-via "usb_20.pdf" 'switch-to-buffer-other-window)  **]]

;; [[**  (bookmark--jump-via "("(emacs) Compilation Mode" (front-context-string . "determined by th") (rear-context-string . "is highlight is\n") (position . 1083129) (last-modified 26426 46916 212964 0) (filename . "d:/Software/Editor/Emacs/emacs-29.4/share/info/emacs") (info-node . "Compilation Mode") (handler . Info-bookmark-jump) (defaults "(emacs) Compilation Mode" "emacs" "Compilation Mode" "*info*"))" 'switch-to-buffer-other-window)  **]]
(define-key prog-mode-map (kbd "C-c C-l") #'zyt/prog-insert-link)
(define-key global-map (kbd "C-c C-i") #'zyt/prog-store-link)
;; c-mode-base-map 默认将 ~C-c C-l~绑定在了 ~c-toggle-electric-state~
(define-key c-mode-base-map (kbd "C-c C-l") #'zyt/prog-insert-link)

;; (defun zyt()
;;   (interactive)
;;   ;; (let ((bookmark-name
;;   ;; (ido-completing-read
;;   (ivy-read
;;    "插入标签: "
;;    (--map 
;; 	(substring-no-properties (bookmark-name-from-full-record it))
;; 	(bookmark-maybe-sort-alist))
;;    :action
;;    #'action
;;    ;; (lambda(bookmark)
;;    ;; 	(bookmark--jump-via bookmark 'switch-to-buffer-other-window)
;;    ;; 	)
;;    :update-fn
;;    (lambda ()
;; 	 (let ((cur-buf(current-buffer)))
;; 	   (progn
;; 		 (save-excursion
;; 		   (bookmark--jump-via  (ivy-state-current ivy-last) 'switch-to-buffer-other-window)
;; 		   (pop-to-buffer cur-buf '(display-buffer-in-previous-window . ()) t)
;; 		   )
;; 		 (message (format "content of bookmark %s" (ivy-state-current ivy-last)))
;; 		 )
;; 	   )
;; 	 )))
;; (message (format "Hello %s\n" bookmark-name))
;; (bookmark--jump-via bookmark-name 'switch-to-buffer-other-window)
;; )))

(defvar zyt/prog-link-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c C-l") #'zyt/prog-insert-link)
	(define-key map (kbd "C-c C-o") #'zyt/prog-goto-link)
	map))

;;;###autoload
(define-minor-mode zyt/prog-link-minor-mode
  "在编程模式下添加文档连接"
  :init-value nil
  :lighter " IL"
  :keymap zyt/prog-link-mode-map
  (save-excursion
	;; evil-indent
	;; clangd 的对齐建议会导致 lsp-format-region将本模式的长注释截断成为多行
	;; 导致font-lock无法正常显示，prog-link的解析跳转也会出现问题
	(with-eval-after-load 'lsp-mode
	  (when (derived-mode-p 'c-mode)
		;; (setq indent-region-function 'c-indent-region)
		;; lsp-mode 可能在 zyt/prog-link-minor-mode之后启用
		(add-hook 'lsp-configure-hook 'zyt/prog-link-minor-mode)
		))
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (goto-char (pos-bol))
	  (when
		  (re-search-forward
		   zyt/prog-link-header-regexp
		   (pos-eol)
		   t
		   )
		(let* ((map (make-sparse-keymap))
			  (bookmark-str (match-string 1))
			  (bookmark (car (read-from-string bookmark-str)))
			  (modified-flag (buffer-modified-p)))
		  (define-key map [down-mouse-1] 'zyt/prog-goto-link)
		  (font-lock-add-keywords
		   nil
		   ;; `((,zyt/prog-link-header-regexp  1 font-lock-doc-face prepend))
		   `((,zyt/prog-link-header-regexp  1 zyt/prog-link-face t))
		   )
		  ;; (fontify--bm-link-line map bookmark-str)
		  ;; (fontify--bm-link-line map (substring-no-properties (match-string 1)) (substring-no-properties (match-string 0)))
		  (if (consp bookmark)
			  (fontify--bm-link-line map (regexp-quote (substring-no-properties  (car bookmark))) (regexp-quote (substring-no-properties (match-string 0))))
			(fontify--bm-link-line map (regexp-quote (substring-no-properties  bookmark-str)) (regexp-quote (substring-no-properties (match-string 0))))
			)
		  (set-buffer-modified-p modified-flag)
		  )
		)
	  (forward-line)
	  )
	))
(provide 'proglink)


;; [[**  (bookmark--jump-via "usb_20.pdf" 'switch-to-buffer-other-window)  **]]

;; Local Variables:
;; coding: chinese-gb18030-dos
;; End:
