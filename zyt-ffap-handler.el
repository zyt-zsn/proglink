(require 'ffap)
(require 'browse-url)
(require 'eww)
(require 'bookmark-w3m)
(require 'cl)
(require 'pp)

(defconst zyt/chm-url-prefix
  "mk:@MSITStore:"
  "chm url prefix"
  ;; [[**  (bookmark--jump-via "("20241012221325-chm_hh_exe.org" (filename . "~/org-roam-files/20241012221325-chm_hh_exe.org") (front-context-string . " Fetch chm url\n:") (rear-context-string . "msg265#msg265\n**") (position . 1111) (last-modified 26429 48039 197821 0) (defaults "20241012221325-chm_hh_exe.org"))" 'switch-to-buffer-other-window)  **]]
  )
(defun zyt/chm-browse-url (URL &rest ARGS)
  "zyt: Open chm url, which is inserted with zyt's proglink, with KeyHH.exe"
  ;; [[**  (bookmark--jump-via "("20241012221325-chm_hh_exe.org" (filename . "~/org-roam-files/20241012221325-chm_hh_exe.org") (front-context-string . "sage]]\n  ~If you") (rear-context-string . "H_default.htm][U") (position . 478) (last-modified 26429 43315 437685 0) (defaults "20241012221325-chm_hh_exe.org"))" 'switch-to-buffer-other-window)  **]]
  (let* (
		 (URL (eww-decode-url-file-name (substring-no-properties URL)))
		 (chm-url (string-trim-left URL zyt/chm-url-prefix))
		 (chm-path (progn
					 (string-match "\\(.*\.chm\\)::" chm-url nil nil)
					 (match-string 1 chm-url)))
		 )
	(w32-shell-execute "open" "KeyHH.exe" (format "-ID-%s %s" (md5 chm-path) chm-url))
	)
  )

(advice-add 'eww-bookmark-jump
			:around
			#'url-bookmark-jump-wrapper
			)
(advice-add 'bookmark-w3m-bookmark-jump
			:around
			#'url-bookmark-jump-wrapper
			)

;; (advice-remove
;;  'bookmark-w3m-bookmark-jump
;;  ;; 'eww-bookmark-jump
;;  ;; #'url-bookmark-jump-wrapper
;;  #'eww-bookmark-jump-wrapper.
;;  )
;; (defun eww-bookmark-jump-wrapper (orig bookmark)
;;   )

(defun url-bookmark-jump-wrapper (orig bookmark)
  "ZYT:Automatically jump to target pos after browser rendered"
  ;; (eww (bookmark-prop-get bookmark 'location))
  (lexical-let* (
				(cur-buffer (current-buffer))
				(pos (alist-get 'position (cddr bookmark)))
				(location (alist-get 'location (cddr bookmark)))
				(orig-name (pp orig))
				(browser-name
				 (progn
				   (string-match "\\(eww\\|w3m\\)"
								 orig-name)
				   (match-string 1 orig-name)
				   )
				 )
				(browser-buffer-name (concat "*" browser-name "*"))
				)
	(if (and
		 pos
		 (get-buffer browser-buffer-name)
		 (with-current-buffer (get-buffer browser-buffer-name)
		   (string= location (eww-current-url))
		   )
		 )
		(progn
		(pop-to-buffer browser-buffer-name)
		(with-current-buffer browser-buffer-name
		  (goto-char pos)
		  (hl-line-mode)
		  )
		(pop-to-buffer cur-buffer)
		)
	  (funcall orig bookmark)
	  (when pos
		(defun zyt/eww-goto-char ()
		  (with-current-buffer browser-buffer-name
			(goto-char pos)
			(hl-line-mode)
			(remove-hook 'eww-after-render-hook
						 #'zyt/eww-goto-char
						 )
			)
		  (pop-to-buffer cur-buffer)
		  )
		(add-hook 'eww-after-render-hook
				  #'zyt/eww-goto-char
				  )
		)
	  )
	)
  )

(setq ffap-url-regexp (concat zyt/chm-url-prefix "\\|" ffap-url-regexp))
(add-to-list 'browse-url-handlers `(,zyt/chm-url-prefix . zyt/chm-browse-url))
;; devdocs--bookmark-jump
;; helpful--bookmark-jump
;; Info-bookmark-jump
;; bookmark-w3m-bookmark-jump
;; eww-bookmark-jump
(defun zyt-wrapper (orig bookmark-name-or-record)
  (condition-case err
	  (progn
		(funcall orig bookmark-name-or-record)
		)
	(error
	 (if-let* (
			   (handler (bookmark-get-handler bookmark-name-or-record))
			   (handler-name (symbol-name handler))
			   (handler-feature (intern
								 ;; (nth 0 (split-string handler-name "-"))
								 (and
								  (string-match "\\([^-]*\\)--?bookmark-jump" handler-name)
								  (match-string 1 handler-name)
								  )
								 ))
			   )
		 (condition-case err
			 (progn
			   (require handler-feature)
			   (funcall orig bookmark-name-or-record)
			   )
		   (error
			(message "The feature, which is assumed to be %s, does not contain %s, please re-check for the correct feature name" (symbol-name handler-feature) handler-name)
			)
		   ) 
	   (signal (car err) (cdr err))
	   )
	 )
	)
  )
(advice-add 'bookmark-handle-bookmark
			:around
			#'zyt-wrapper)
(provide 'zyt-ffap-handler)
