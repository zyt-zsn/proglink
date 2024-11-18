;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with SPC . and enter text in its buffer.


(require 'consult)
(require 'dash)
(require 'cc-mode)
(defconst zyt/prog-link-header-regexp
  "\\[\\[\\*\\*  \(bookmark--jump-via \"\\(.*\\)\" 'switch-to-buffer-other-window)  \\*\\*\\]\\]"
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
	  (let ((bookmark-str (match-string 1)))
		(if (consp (car (read-from-string bookmark-str)))
			(bookmark--jump-via (car (read-from-string bookmark-str)) 'switch-to-buffer-other-window)
			;; (bookmark-jump (car (read-from-string bookmark-str)))
		  (bookmark--jump-via bookmark-str 'switch-to-buffer-other-window)
		  ;; (bookmark-jump bookmark-str)
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
  (setq zyt/prog-temp-link (string-replace "\n" "" (pp-to-string (bookmark-make-record))))
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
				   (or (bookmark-buffer-file-name)
					   (null (eq bookmark-make-record-function
								 'bookmark-make-record-default
								 ))))
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
	(prog1
		(with-current-buffer (car selected)
		  (string-replace "\n" "" (pp-to-string (bookmark-make-record)))
		  )
	  (pop-to-buffer cur-buf 'display-buffer-same-window)
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
(defun zyt/prog-insert-link()
  (interactive)
  (let* (
		 (cur-buf (current-buffer))
		 (bookmark-str (or zyt/prog-temp-link
						   (_zyt/prog-store-link)
						   (_zyt/prog-select-link-from-bookmarks)
						   ))
		 )
	(with-current-buffer cur-buf
	  (unless (current-line-empty-p)
		(if (featurep 'evil)
			(evil-open-below 1)
		  (end-of-line)
		  (newline))
		)
	  (insert (format "[[**  (bookmark--jump-via \"%s\" 'switch-to-buffer-other-window)  **]]" bookmark-str))
	  (comment-line 1)
	  (setq zyt/prog-temp-link nil)
	  ;; [[**  (bookmark-jump "org link font lock")  **]]
	  (forward-line -1)
	  (let ((map (make-sparse-keymap)))
		(define-key map [down-mouse-1] 'zyt/prog-goto-link)
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
		)
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
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (goto-char (pos-bol))
	  (when
		  (re-search-forward
		   zyt/prog-link-header-regexp
		   (pos-eol)
		   t
		   )
		(let ((map (make-sparse-keymap)))
		  (define-key map [down-mouse-1] 'zyt/prog-goto-link)
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
		  )
		)
	  (forward-line)
	  )
	))
(provide 'proglink)


;; [[**  (bookmark--jump-via "usb_20.pdf" 'switch-to-buffer-other-window)  **]]
