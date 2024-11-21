(require 'ffap)
(require 'browse-url)
(require 'eww)

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
(setq ffap-url-regexp (concat zyt/chm-url-prefix "\\|" ffap-url-regexp))
(add-to-list 'browse-url-handlers `(,zyt/chm-url-prefix . zyt/chm-browse-url))
(provide 'zyt-ffap-handler)
