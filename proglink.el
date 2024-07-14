;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with SPC . and enter text in its buffer.


(require 'consult)
(require 'dash)
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
	  (bookmark--jump-via (match-string 1) 'switch-to-buffer-other-window)
	  )
	)
  )

;; https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace
(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun zyt/prog-insert-link()
  (interactive)
  (let* ((cur-buff (current-buffer))
		 (bookmark (consult--read
					(--map 
					 (substring-no-properties (bookmark-name-from-full-record it))
					 (bookmark-maybe-sort-alist))
					:prompt "插入标签: "
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
								(pop-to-buffer cur-buf '(display-buffer-in-previous-window . ()) t)
								)
							  )
							)
						  )
						))
					)))
	(with-current-buffer cur-buff
	  (unless (current-line-empty-p)
		(if (featurep 'evil)
			(evil-open-below 1)
		  (end-of-line)
		  (newline))
		)
	  (insert (format "[[**  (bookmark--jump-via \"%s\" 'switch-to-buffer-other-window)  **]]" bookmark))
	  (comment-line 1)
	  ;; [[**  (bookmark-jump "org link font lock")  **]]
	  (forward-line -1)
	  (set-text-properties
	   (pos-bol) (pos-eol)
	   '(
		 mouse-face highlight
		 help-echo "mouse-2: visit this file in other window"
		 font-lock-fontified nil
		 ;; face org-document-title
		 ;; face tdr-font-mode
		 face org-link
		 ))
	  )
	))
;; [[**  (bookmark--jump-via "SCSI_Peripheral_Device_Type" 'switch-to-buffer-other-window)  **]]

;; [[**  (bookmark--jump-via "usb_20.pdf" 'switch-to-buffer-other-window)  **]]



(define-key prog-mode-map (kbd "C-c C-l") #'zyt/prog-insert-link)
;; c-mode-base-map 默认将 ~C-c C-l~绑定在了 ~c-toggle-electric-state~
(define-key c-mode-base-map (kbd "C-c C-l") #'zyt/prog-insert-link)

(defun zyt()
  (interactive)
  ;; (let ((bookmark-name
  ;; (ido-completing-read
  (ivy-read
   "插入标签: "
   (--map 
	(substring-no-properties (bookmark-name-from-full-record it))
	(bookmark-maybe-sort-alist))
   :action
   #'action
   ;; (lambda(bookmark)
   ;; 	(bookmark--jump-via bookmark 'switch-to-buffer-other-window)
   ;; 	)
   :update-fn
   (lambda ()
	 (let ((cur-buf(current-buffer)))
	   (progn
		 (save-excursion
		   (bookmark--jump-via  (ivy-state-current ivy-last) 'switch-to-buffer-other-window)
		   (pop-to-buffer cur-buf '(display-buffer-in-previous-window . ()) t)
		   )
		 (message (format "content of bookmark %s" (ivy-state-current ivy-last)))
		 )
	   )
	 )))
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
		(set-text-properties
		 (pos-bol) (pos-eol)
		 '(
		   mouse-face highlight
		   help-echo "mouse-2: visit this file in other window"
		   font-lock-fontified nil
		   ;; face org-document-title
		   ;; face tdr-font-mode
		   face org-link
		   ))
		)
	  (forward-line)
	  )
	))
(provide 'proglink)


;; [[**  (bookmark--jump-via "usb_20.pdf" 'switch-to-buffer-other-window)  **]]
