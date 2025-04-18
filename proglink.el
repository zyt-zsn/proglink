;; -*- coding: chinese-gb18030-dos; -*-
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with SPC . and enter text in its buffer.


(require 'consult)
(require 'ffap)
(require 'dash)
(require 'cc-mode)
(require 'bookmark)
(require 'zyt-ffap-handler)
(require 'evil-common)
(defconst zyt/prog-link-header-regexp
  "\\[\\[\\*\\*  \(bookmark--jump-via \"\\(.*\\)\" 'switch-to-buffer-other-window)  \\*\\*\\]\\(.*\\)\\]"
  )
;; (defvar zyt/prog-link-face font-lock-doc-face)
(defface zyt/prog-link-face
  '((t :inherit font-lock-comment-face :underline t))
  "prog link minor mode face used to indicate link to whatever current text points to"
  :group 'font-lock-faces)

(defface zyt/prog-hide-face
  '((t :inherit font-lock-string-face))
  "prog link minor mode face used to indicate link to whatever current text points to"
  :group 'font-lock-faces)

(defvar zyt/prog-link-face 'zyt/prog-link-face)
(defun zyt/prog-link--bookmark-handler(buf)
  (unless (get-buffer-window buf)
	(display-buffer buf)
	)
  )
(defun zyt/prog-goto-link()
  (interactive)
  (if
	  (save-excursion
		(goto-char (pos-bol))
		(re-search-forward
		 zyt/prog-link-header-regexp
		 (pos-eol)
		 t
		 )
		)
	  (let ((bookmark-str (match-string 1))
			(display-buffer-overriding-action
			 '(display-buffer-use-least-recent-window)
			 )
			(ret 'found)
			)
		(if (consp (car (read-from-string bookmark-str)))
			(bookmark--jump-via (car (read-from-string bookmark-str)) 'zyt/prog-link--bookmark-handler)
		  (if (bookmark-get-bookmark bookmark-str t)
			  (bookmark--jump-via bookmark-str 'zyt/prog-link--bookmark-handler)
			(find-file-at-point bookmark-str)
			(setq ret nil)
			)
		  )
		ret
		)
	(if (eq major-mode 'org-mode)
		(org-open-at-point)
	  )
	)
  )
(with-eval-after-load 'org
  (advice-add 'org-src-font-lock-fontify-block
			  :after
			  #'zyt/prog-link-font-lock-fontify-block
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

(defun _zyt/prog-select-link-from-displayed-buffers()
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

(with-eval-after-load 'org
  (setq org-highlight-latex-and-related
		  (remove 'entities org-highlight-latex-and-related))
  )
;; (defsubst fontify--bm-link-line(map &optional bookmark-str)
(defun fontify--bm-link-line(map &optional bookmark-str whole-link-str)
  ;; 正常情况下，插入标签后，调用fontify--bm-link-line即可正常fontify显示
  ;; 但在buffer尾端插入时，时常不能即时正常显示，暂无时间分析，临时用此笨拙方法规避
  ;; 考虑效率问题，后应避免使用after-save-hook
  (add-hook 'after-save-hook 'zyt/prog-link-minor-mode)
  ;; (insert (format "[[**  (bookmark--jump-via \"%s\" 'switch-to-buffer-other-window)  **]]" bookmark-str))
  ;; cursor在最后一行行尾时，pos-eol 经常和预期结果不同；稳妥起见，先挪到行首
  (beginning-of-line)
  (set-face-underline zyt/prog-link-face t)
  ;; [[**  (bookmark--jump-via "("org.el add text properties" (filename . "~/.emacs.d/straight/repos/org/lisp/org.el") (front-context-string . "e-property (pcas") (rear-context-string . "            (fac") (position . 212381) (last-modified 26602 13963 284016 0) (defaults "org.el"))" 'switch-to-buffer-other-window)  **]]
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
	(goto-char (pos-bol))
	(re-search-forward whole-link-str (pos-eol) t)
	(let (
		  (beginning-0 (match-beginning 0))
		  (end-0 (match-end 0))
		  )
	  (goto-char beginning-0)
	  (re-search-forward bookmark-str end-0 t)
	  ;; (set-face-underline zyt/prog-link-face t)
	  (set-text-properties beginning-0 end-0 nil)
	  (set-text-properties beginning-0 (match-beginning 0) `(invisible t))
	  (set-text-properties (match-end 0) end-0 `(invisible t))
	  )
	)
  )

(defun zyt/prog-insert-link(&optional default-label-name)
  (interactive "P")
  (let* (
		 (cur-buf (current-buffer))
		 (bookmark (or zyt/prog-temp-link
					   (_zyt/prog-select-link-from-displayed-buffers)
					   (_zyt/prog-select-link-from-bookmarks)
					   ))
		 (link-name
		  (read-from-minibuffer "Link Name:"
								(or default-label-name (if (consp bookmark) (car bookmark) bookmark-str))))
		 (bookmark-str	;;bookmark's serialization representation
		  (if (consp bookmark)
			  (progn
				(setq bookmark (cons link-name (cdr bookmark)))
			  (string-replace "\n" "" (pp-to-string bookmark))
			  )
			bookmark
			)
		  )
		 )
	(with-current-buffer cur-buf
	  (unless (current-line-empty-p)
		(if (featurep 'evil)
			(evil-open-below 1)
		  (end-of-line)
		  (newline))
		)
	  (if (eq major-mode 'org-mode)
		  (setq whole-link-str (format "[[**  (bookmark--jump-via \"%s\" 'switch-to-buffer-other-window)  **][%s]]" bookmark-str link-name))
		(setq whole-link-str (format "[[**  (bookmark--jump-via \"%s\" 'switch-to-buffer-other-window)  **]]" bookmark-str))
		)
	  (insert whole-link-str)
	  (cond
	   ((derived-mode-p 'c-mode)
		;; C mode 对于长度超过(含) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 的行做 comment-line后
		;; 如果选中区域进行indent操作，会将 /* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx */
		;; 转换成如下形式
		;; /* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
		;; */
		;; 导致font-lock出错;针对c-mode，change comment style to ~line comments~
		;; ZYT: 上述描述在新版本代码中不再成立? 不需要再针对c-mode特殊处理?
		(let (
			  ;; (c-block-comment-flag-orig c-block-comment-flag)
			  )
		  ;; (c-toggle-comment-style -1)
		  (comment-line 1)
		  ;; (if c-block-comment-flag-orig
		  ;; (c-toggle-comment-style 1)
		  ;; )
		  )
		)
	   ((eq major-mode 'org-mode)
		(if (org-in-src-block-p 'inside)
			(let* (
				   (element (org-element-at-point-no-context))
				   ;; (begin (org-element-property :begin element))
				   ;; (end (org-element-property :end element))
				   (src-block-mode (org-src-get-lang-mode (nth 0 (org-babel-get-src-block-info))))
				   (pos (point))
				   )
			  ;; (evil-with-active-region begin end
			  (evil-with-active-region (pos-bol) (pos-eol)
				  (funcall src-block-mode)
				  (zyt/prog-link-minor-mode -1)
				  (comment-line 1)
				  (setq pos (point))
				  (zyt/prog-link-minor-mode 1)
				  ;; (let ((org-startup-with-latex-preview))
				  ;; (funcall-interactively 'org-mode)
				  (org-mode)
				  (org-indent-line)
				  ;; )
				)
			  (goto-char pos)
			  )
		  )
		;;todo: zyt comment according to src block's language
		)
	   (t
		(comment-line 1)
		)
	   )
	  ;; [[**  (bookmark-jump "org link font lock")  **]]
	  ;; 如是最后一行，comment-line 会将cursor折返至当前行行首，而非下一行行首
	  (unless (or
			   (= (pos-eol) (buffer-end 1))
			   ;; org-mode: 或未做comment(out of src block)或对cusor做了调整(inside src block)
			   (eq major-mode 'org-mode)
			   )
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

(defvar zyt/prog-link-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c C-l") #'zyt/prog-insert-link)
	(define-key map (kbd "C-c C-o") #'zyt/prog-goto-link)
	map))

(defun zyt/prog-link-font-lock-fontify-block (lang start end)
	(goto-char start)
	(while (< (point) end)
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
			  (modified-flag (buffer-modified-p))
			  (undo-list buffer-undo-list)
			  )
		  ;; [[**  (bookmark--jump-via "("(elisp) Maintaining Undo" (front-context-string . "File: elisp.info") (rear-context-string) (position . 2689922) (last-modified 26454 27114 540208 0) (filename . "d:/Software/Editor/Emacs/emacs-29.4/share/info/elisp") (info-node . "Maintaining Undo") (handler . Info-bookmark-jump) (defaults "(elisp) Maintaining Undo" "elisp" "Maintaining Undo" "*info*"))" 'switch-to-buffer-other-window)  **]]
		  (buffer-disable-undo)
		  (define-key map [down-mouse-1] 'zyt/prog-goto-link)
		  (if (null zyt/prog-link-minor-mode)
			  (progn
				(font-lock-remove-keywords
				 nil
				 ;; `((,zyt/prog-link-header-regexp  (1 font-lock-doc-face prepend)))
				 `((,zyt/prog-link-header-regexp  1 zyt/prog-link-face t))
				 )
				(beginning-of-line)
				(set-text-properties (pos-bol) (pos-eol) nil)
				)
			(font-lock-add-keywords
			 nil
			 ;; `((,zyt/prog-link-header-regexp  (1 font-lock-doc-face prepend)))
			 `((,zyt/prog-link-header-regexp  1 zyt/prog-link-face t))
			 )
			(if (consp bookmark)
				(fontify--bm-link-line map (regexp-quote (substring-no-properties  (car bookmark))) (regexp-quote (substring-no-properties (match-string 0))))
			  (fontify--bm-link-line map (regexp-quote (substring-no-properties  bookmark-str)) (regexp-quote (substring-no-properties (match-string 0))))
			  )
			)
		  (set-buffer-modified-p modified-flag)
		  (buffer-enable-undo)
		  (setq buffer-undo-list undo-list)
		  )
		)
	  (forward-line)
	  )
  )

;;;###autoload
(define-minor-mode zyt/prog-link-minor-mode
  "在编程模式下添加文档连接"
  :init-value nil
  :lighter " IL"
  :keymap zyt/prog-link-mode-map
  (save-excursion
	(bookmark-maybe-load-default-file)
	;; evil-indent
	;; clangd 的对齐建议会导致 lsp-format-region将本模式的长注释截断成为多行
	;; 导致font-lock无法正常显示，prog-link的解析跳转也会出现问题
	(with-eval-after-load 'lsp-mode
	  (when (derived-mode-p 'c-mode)
		;; (setq indent-region-function 'c-indent-region)
		;; lsp-mode 可能在 zyt/prog-link-minor-mode之后启用
		(add-hook 'lsp-configure-hook
				  (lambda()
					(setq indent-region-function 'c-indent-region)
					)
				  ;; 'zyt/prog-link-minor-mode
				  )
		)
	  ;; (add-hook 'after-save-hook 'zyt/prog-link-minor-mode)
	  )
	(zyt/prog-link-font-lock-fontify-block nil (point-min) (point-max))
	)
  )
(provide 'proglink)

;; Local Variables:
;; coding: chinese-gb18030-dos
;; End:
