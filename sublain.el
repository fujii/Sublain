;;; sublain.el --- Subversion repository browser

;; Copyright (C) 2006,2012 Fujii Hironori

;; License: GNU GPL
;; Author: Fujii Hironori
;; Keywords: tools
;; Package-Version: 1.0
;; Package-Requires: 

;; (autoload 'sublain-list "sublain" "List directory entries in the repository." t)
;; (autoload 'sublain-bookmark "sublain" "Display bookmark file." t)

;;; macro
(defmacro sublain-save-line (&rest body)
  `(let ((sublain-save-line (1+ (count-lines (point-min) (line-beginning-position)))))
     ,@body
     (goto-char (point-min))
     (forward-line (1- sublain-save-line))))

;;; sublain-list

(defvar sublain-list-default-verbose t)
(defvar sublain-list-default-recursive nil)

(defvar sublain-list-buffer-name "*sublain-list*")

(defvar sublain-list-target)
(defvar sublain-list-revision "HEAD")
(defvar sublain-list-verbose sublain-list-default-verbose)
(defvar sublain-list-recursive sublain-list-default-recursive)

(defun sublain-list-update (&optional ignore-auto noconfirm)
  (let ((inhibit-read-only t)
	(arg (sublain-flatten-list "list" "--non-interactive"
				   (when sublain-list-verbose "--verbose")
				   (when sublain-list-recursive "--recursive")
				   (concat sublain-list-target "@" sublain-list-revision))))
    (erase-buffer)
    (insert (format "Target: %s\nRevision: %s\n\n" sublain-list-target sublain-list-revision))
    (apply 'sublain-call-svn arg)
    (goto-char (point-min))
    (sublain-list-forward-line 3)
    (set-buffer-modified-p nil)))

(defvar sublain-target-history nil)

(defun sublain-list-read-target ()
  (let* ((dir (expand-file-name default-directory))
	 (prompt (format "Target (%s): " dir)))
    (read-string prompt nil 'sublain-target-history dir)))

;;;###autoload
(defun sublain-list (target)
  "List directory entries in the repository."
  (interactive (list (sublain-list-read-target)))
  (switch-to-buffer (sublain-get-new-buffer sublain-list-buffer-name))
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (sublain-list-mode)
  (setq sublain-list-target target)
  (sublain-list-update))

(defvar sublain-list-mode-map nil)
(unless sublain-list-mode-map
  (setq sublain-list-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map " " 'scroll-up)
	  (define-key map "B" 'sublain-bookmark)
	  (define-key map "L" 'sublain-list-show-log-point)
	  (define-key map "P" 'sublain-list-proplist)
	  (define-key map "R" 'sublain-list-toggle-recursive)
	  (define-key map "U" 'rename-uniquely)
	  (define-key map "V" 'sublain-list-toggle-verbose)
	  (define-key map "\C-?" 'scroll-down)
	  (define-key map "^" 'sublain-list-visit-parent-directory)
	  (define-key map "b" 'sublain-list-blame)
	  (define-key map "c" 'sublain-list-cat)
	  (define-key map "f" 'sublain-list-visit)
	  (define-key map "g" 'revert-buffer)
	  (define-key map "i" 'sublain-list-info)
	  (define-key map "l" 'sublain-list-show-log)
	  (define-key map "n" 'sublain-list-next-line)
	  (define-key map "p" 'sublain-list-previous-line)
	  (define-key map "q" 'bury-buffer)
	  (define-key map "r" 'sublain-list-revision)
	  map)))

(defvar sublain-list-mode-hook nil)

(defun sublain-list-mode ()
  "Major mode for output of svn list
Special commands: \\{sublain-list-mode-map}
Turning on sublain-list-mode runs the hook `sublain-list-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (sublain-list-make-local-variable)
  (use-local-map sublain-list-mode-map)
  (setq mode-name "Sublain list")
  (setq major-mode 'sublain-list-mode)
  (set (make-local-variable 'revert-buffer-function) 'sublain-list-update)
  (run-hooks 'sublain-list-mode-hook))

(defun sublain-list-make-local-variable ()
  (make-local-variable 'sublain-list-target) 
  (make-local-variable 'sublain-list-revision) 
  (make-local-variable 'sublain-list-verbose) 
  (make-local-variable 'sublain-list-recursive))

(defun sublain-list-show-log ()
  (interactive)
  (sublain-log sublain-list-target))

(defun sublain-list-show-log-point ()
  (interactive)
  (let ((target (sublain-list-get-target)))
    (unless target
      (error "No file"))
    (sublain-log target)))

(defun sublain-list-proplist ()
  (interactive)
  (sublain-proplist sublain-list-target sublain-list-revision))

(defun sublain-list-blame ()
  (interactive)
  (let ((target (sublain-list-get-target)))
    (unless target
      (error "No file"))
    (sublain-blame target sublain-list-revision)))

(defun sublain-list-cat ()
  (interactive)
  (let ((target (sublain-list-get-target)))
    (unless target
      (error "No file"))
    (sublain-cat target sublain-list-revision)))

(defun sublain-list-info ()
  (interactive)
  (sublain-info sublain-list-target sublain-list-revision))

(defun sublain-list-line-info ()
  (save-excursion
    (beginning-of-line)
    (and (re-search-forward " \\([^ ]+\\)$" (line-end-position) t)
	 (list (match-beginning 1) (match-end 1)))))

(defun sublain-list-forward-line (&optional n)
  (forward-line n)
  (let ((info (sublain-list-line-info)))
    (when info
      (goto-char (car info)))))

(defun sublain-list-next-line (&optional n)
  (interactive "p")
  (sublain-list-forward-line n))

(defun sublain-list-previous-line (&optional n)
  (interactive "p")
  (sublain-list-forward-line (- n)))

(defun sublain-list-resolve-url (base relative)
  (if (string-match "/$" base)
      (concat base relative)
    (concat base "/" relative)))

(defun sublain-list-get-target ()
  (let ((info (sublain-list-line-info)))
    (when info
      (sublain-list-resolve-url sublain-list-target (buffer-substring (car info) (cadr info))))))

(defun sublain-list-visit ()
  (interactive)
  (let ((target (sublain-list-get-target)))
    (unless target
      (error "No file"))
    (setq sublain-list-target target)
    (sublain-list-update)))

(defun sublain-list-parent-url (url)
  (save-match-data
    (cond (;; "http://example.com/" or "file:///"
	   (string-match "^[a-z+]+://[^/]*/$" url)
	   url)
	  ((string-match "\\(.*/\\)[^/]+/?$" url)
	   (match-string 1 url))
	  ;; "." or "foo"
	  (t url))))
    
(defun sublain-list-visit-parent-directory ()
  (interactive)
  (setq sublain-list-target (sublain-list-parent-url sublain-list-target))
  (sublain-list-update))

(defun sublain-list-revision (rev)
  (interactive "sRevison: ")
  (setq sublain-list-revision (if (string= rev "") "HEAD" rev))
  (sublain-list-update))

(defun sublain-list-toggle-recursive ()
  (interactive)
  (setq sublain-list-recursive (not sublain-list-recursive))
  (sublain-list-update))

(defun sublain-list-toggle-verbose ()
  (interactive)
  (setq sublain-list-verbose (not sublain-list-verbose))
  (sublain-list-update))

;;; sublain-log
(defvar sublain-log-buffer-name "*sublain-log*")
(defvar sublain-log-default-limit 10)

(defvar sublain-log-limit nil)
(defvar sublain-log-target)

(defun sublain-call-svn-log (target limit verbose)
  (let ((arg (sublain-flatten-list "log" "--non-interactive"
				   (when verbose "--verbose")
				   (when (> limit 0) (list "--limit" (number-to-string limit)))
				   target)))
    (apply 'sublain-call-svn arg)))

(defun sublain-log-update (&optional ignore-auto noconfirm)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (sublain-call-svn-log sublain-log-target sublain-log-limit t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun sublain-log (target)
  (interactive "sTarget: ")
  (switch-to-buffer (sublain-get-new-buffer sublain-log-buffer-name))
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (sublain-log-mode)
  (setq sublain-log-target target)
  (setq sublain-log-limit sublain-log-default-limit)
  (sublain-log-update))

(defvar sublain-log-mode-map nil)
(unless sublain-log-mode-map
  (setq sublain-log-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map " " 'scroll-up)
	  (define-key map "=" 'sublain-log-diff)
	  (define-key map "E" 'sublain-log-ediff)
	  (define-key map "U" 'rename-uniquely)
	  (define-key map "\C-?" 'scroll-down)
	  (define-key map "g" 'revert-buffer)
	  (define-key map "l" 'sublain-log-limit)
	  (define-key map "n" 'sublain-log-next)
	  (define-key map "p" 'sublain-log-previous)
	  (define-key map "j" 'sublain-log-ediff-next)
	  (define-key map "k" 'sublain-log-ediff-previous)
	  (define-key map "q" 'bury-buffer)
	  map)))

(defvar sublain-log-mode-hook nil)

(defun sublain-log-mode ()
  "Major mode for output of svn log
Special commands: \\{sublain-log-mode-map}
Turning on sublain-log-mode runs the hook `sublain-log-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'sublain-log-target)
  (make-local-variable 'sublain-log-limit)
  (use-local-map sublain-log-mode-map)
  (setq mode-name "Sublain log")
  (setq major-mode 'sublain-log-mode)
  (set (make-local-variable 'revert-buffer-function) 'sublain-log-update)
  (run-hooks 'sublain-log-mode-hook))

(defvar sublain-log-separator "^------------------------------------------------------------------------$")
(defvar sublain-log-ediff-separator "^ *[MA] \\(.*\\)")

(defun sublain-log-next ()
  (interactive)
  (re-search-forward sublain-log-separator)
  (forward-line 1))

(defun sublain-log-previous ()
  (interactive)
  (re-search-backward sublain-log-separator)
  (re-search-backward sublain-log-separator)
  (forward-line 1))

(defun sublain-log-ediff-next ()
  (interactive)
  (end-of-line)
  (re-search-forward sublain-log-ediff-separator nil t)
  (beginning-of-line))

(defun sublain-log-ediff-previous ()
  (interactive)
  (beginning-of-line)
  (re-search-backward sublain-log-ediff-separator nil t)
  (beginning-of-line))

(defun sublain-log-get-revision ()
  (save-excursion
    (re-search-backward sublain-log-separator nil t)
    (when (re-search-forward "^r\\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun sublain-log-ediff-get-target ()
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward sublain-log-ediff-separator (line-end-position) t)
      (match-string 1))))

(defun sublain-log-ediff-match-ext ()
  (save-excursion
    (end-of-line)
    (when (re-search-backward ".*\\(\\..*\\)$" (line-beginning-position) t)
      (match-string 1))))

(defun sublain-log-diff ()
  (interactive)
  (let ((rev (sublain-log-get-revision)))
    (unless rev
      (error "No revision"))
    (sublain-diff sublain-log-target rev)))

(defun sublain-log-ediff ()
  (interactive)
  (let ((changed (sublain-log-ediff-get-target))
	(matched-ext (sublain-log-ediff-match-ext))
	(rev (sublain-log-get-revision)))
    (unless rev
      (error "No revision"))
    (unless changed
      (error "No file"))
    ;; (message "log-target: %s, changed: %s, rev: %s" sublain-log-target changed rev)
    ;;(sublain-ediff sublain-log-target changed rev)
    (sublain-ediff sublain-log-target changed rev matched-ext)))

(defun sublain-log-limit (n)
  (interactive "nLimit: ")
  (setq sublain-log-limit n)
  (sublain-log-update))

;;; sublain-diff

(defvar sublain-diff-buffer-name "*sublain-diff*")

(defun sublain-diff (target rev)
  (interactive "sTarget: 
nRev: ")
  (switch-to-buffer (sublain-get-new-buffer sublain-diff-buffer-name))
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (let ((inhibit-read-only t)
	(range (concat (number-to-string (1- rev)) ":" (number-to-string rev))))
    (erase-buffer)
    (sublain-call-svn "diff" "--revision" range target)
    (goto-char (point-min))
    (set-buffer-modified-p nil))
  (diff-mode))

;;; sublain-ediff

(defvar sublain-log-ediff-info-name "*sublain-log-info*")
(defvar sublain-log-ediff-transient-buffers)
(defvar sublain-log-ediff-after-quit-destination-buffer)

(defvar sublain-log-repository-root-separator "^Repository Root: ")

(defun sublain-log-repository-root ()
  (re-search-backward sublain-log-repository-root-separator)
  (beginning-of-line)
    (when (re-search-forward (concat sublain-log-repository-root-separator "\\(.*\\)") nil t)
      (match-string 1)))

(defun sublain-log-ediff-startup-hook ()
;;   (message "svn-ediff-startup-hook: ediff-after-quit-hook-internal: %S" ediff-after-quit-hook-internal)
  (add-hook 'ediff-after-quit-hook-internal
            `(lambda ()
               (sublain-log-ediff-exit-hook
                ',sublain-log-ediff-after-quit-destination-buffer ',sublain-log-ediff-transient-buffers))
            nil 'local))

(defun sublain-log-ediff-exit-hook (svn-buf tmp-bufs)
  ;; (message "svn-ediff-exit-hook: svn-buf: %s, tmp-bufs: %s" svn-buf tmp-bufs)
  ;; kill the temp buffers (and their associated windows)
  (dolist (tb tmp-bufs)
    (when (and tb (buffer-live-p (get-buffer tb)))
      (kill-buffer tb)))
  (delete-other-windows)
  ;; switch back to the *svn* buffer
  (when (and svn-buf (buffer-live-p svn-buf)
             (not (get-buffer-window svn-buf t)))
    (ignore-errors (switch-to-buffer svn-buf)))
)

(defun sublain-ediff-mode (ext)
    (when (string= ext ".c") (c-mode))
    (when (string= ext ".h") (c-mode))
    (when (string= ext ".asp") (javascript-mode))
)

(defun sublain-ediff (target changed rev ext)
  (interactive "sTarget: 
sRevision: ")
  (switch-to-buffer (sublain-get-new-buffer sublain-log-ediff-info-name))
  (setq buffer-read-only t)
  (sublain-call-svn "info" "--revision" (number-to-string rev) target)
  (let ((inhibit-read-only t)
	(log-root (sublain-log-repository-root))
	(sublain-log-ediff-after-quit-destination-buffer (get-buffer sublain-log-buffer-name))
	(sublain-ediff-changed-prev (concat changed "@" (number-to-string (1- rev))))
	(sublain-ediff-changed-base (concat changed "@" (number-to-string rev)))
	(startup-hook '(sublain-log-ediff-startup-hook))
	)
    (kill-buffer sublain-log-ediff-info-name)
    ;;
    (switch-to-buffer (sublain-get-new-buffer sublain-ediff-changed-prev))
    (setq buffer-read-only t)
    (sublain-call-svn "cat" (concat log-root sublain-ediff-changed-prev))
    (sublain-ediff-mode ext)
    (set-buffer-modified-p nil)
    ;;
    (switch-to-buffer (sublain-get-new-buffer sublain-ediff-changed-base))
    (setq buffer-read-only t)
    (sublain-call-svn "cat" (concat log-root sublain-ediff-changed-base))
    (sublain-ediff-mode ext)
    (set-buffer-modified-p nil)
    ;;
    (setq sublain-log-ediff-transient-buffers (list sublain-ediff-changed-prev sublain-ediff-changed-base))
    (ediff-buffers sublain-ediff-changed-base sublain-ediff-changed-prev startup-hook)
    )
  (goto-char (point-min))
  (view-mode))

;;; sublain-bookmark
(defvar sublain-bookmark-buffer-name "*sublain-bookmark*")
(defvar sublain-bookmark-file-name "~/.sublain/bookmark.txt")

(defvar sublain-bookmark-mode-map nil)
(unless sublain-bookmark-mode-map
  (setq sublain-bookmark-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map " " 'scroll-up)
	  (define-key map "\C-?" 'scroll-down)
	  (define-key map "e" 'sublain-bookmark-edit)
	  (define-key map "f" 'sublain-bookmark-visit)
	  (define-key map "g" 'revert-buffer)
	  (define-key map "n" 'next-line)
	  (define-key map "p" 'previous-line)
	  (define-key map "q" 'bury-buffer)
	  map)))

(defvar sublain-bookmark-default-bookmark "Type 'f' on a URL to visit the repository.
Type 'e' to edit this bookmark file.

Apache Subversion
https://svn.apache.org/repos/asf/subversion/trunk/

Apache HTTP Server
https://svn.apache.org/repos/asf/httpd/httpd/trunk/
")

;;;###autoload
(defun sublain-bookmark ()
  "Display bookmark file."
  (interactive)
  (switch-to-buffer (sublain-get-new-buffer sublain-bookmark-buffer-name))
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (sublain-bookmark-update)
  (sublain-bookmark-mode))

(defvar sublain-bookmark-mode-hook nil)

(defun sublain-bookmark-mode ()
  "Major mode for Sublain bookmark
Special commands: \\{sublain-bookmark-mode-map}
Turning on sublain-bookmark-mode runs the hook `sublain-bookmark-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sublain-bookmark-mode-map)
  (setq mode-name "Sublain bookmark")
  (setq major-mode 'sublain-bookmark-mode)
  (set (make-local-variable 'revert-buffer-function) 'sublain-bookmark-update)
  (run-hooks 'sublain-bookmark-mode-hook))

(defun sublain-bookmark-visit ()
  (interactive)
  (sublain-list (buffer-substring (line-beginning-position) (line-end-position))))

(defun sublain-bookmark-edit ()
  (interactive)
  (find-file-other-window sublain-bookmark-file-name))

(defun sublain-bookmark-update (&optional ignore-auto noconfir)
  (interactive)
  (make-directory (file-name-directory sublain-bookmark-file-name) t)
  (unless (file-exists-p sublain-bookmark-file-name)
    (with-temp-file sublain-bookmark-file-name
      (insert sublain-bookmark-default-bookmark)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents sublain-bookmark-file-name))
  (set-buffer-modified-p nil))

;;; sublain-blame

(defvar sublain-blame-default-verbose nil)

(defvar sublain-blame-buffer-name "*sublain-blame*")

(defvar sublain-blame-target)
(defvar sublain-blame-target-revision)
(defvar sublain-blame-revision "HEAD")
(defvar sublain-blame-verbose sublain-blame-default-verbose)

(defun sublain-blame-update ()
  (let ((inhibit-read-only t)
	(arg (sublain-flatten-list "blame" "--non-interactive"
				   (when sublain-blame-verbose "--verbose")
				   "--revision" sublain-blame-revision
				   (concat sublain-blame-target "@" sublain-blame-target-revision))))
    (erase-buffer)
    (apply 'sublain-call-svn arg)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun sublain-blame (url rev)
  (interactive "sTarget: 
sRevision: ")
  (switch-to-buffer (sublain-get-new-buffer sublain-blame-buffer-name))
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (sublain-blame-mode)
  (setq sublain-blame-target url)
  (setq sublain-blame-target-revision rev)
  (setq sublain-blame-revision rev)
  (sublain-blame-update))

(defvar sublain-blame-mode-map nil)
(unless sublain-blame-mode-map
  (setq sublain-blame-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map " " 'scroll-up)
	  (define-key map "B" 'sublain-bookmark)
	  (define-key map "U" 'rename-uniquely)
	  (define-key map "V" 'sublain-blame-toggle-verbose)
	  (define-key map "\C-?" 'scroll-down)
	  (define-key map "l" 'sublain-blame-show-log)
	  (define-key map "n" 'next-line)
	  (define-key map "p" 'previous-line)
	  (define-key map "q" 'bury-buffer)
	  (define-key map "r" 'sublain-blame-revision)
	  map)))

(defvar sublain-blame-mode-hook nil)

(defun sublain-blame-mode ()
  "Major mode for output of svn blame
Special commands: \\{sublain-blame-mode-map}
Turning on sublain-blame-mode runs the hook `sublain-blame-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (sublain-blame-make-local-variable)
  (use-local-map sublain-blame-mode-map)
  (setq mode-name "Sublain blame")
  (setq major-mode 'sublain-blame-mode)
  (run-hooks 'sublain-blame-mode-hook))

(defun sublain-blame-make-local-variable ()
  (make-local-variable 'sublain-blame-target)
  (make-local-variable 'sublain-blame-target-revision)
  (make-local-variable 'sublain-blame-revision)
  (make-local-variable 'sublain-blame-verbose))

(defun sublain-blame-show-log ()
  (interactive)
  (sublain-log sublain-blame-target))

(defun sublain-blame-line-info ()
  (save-excursion
    (beginning-of-line)
    (and (re-search-forward " \\([^ ]+\\)$" (line-end-position) t)
	 (list (match-beginning 1) (match-end 1)))))

(defun sublain-blame-revision (rev)
  (interactive "sRevison: ")
  (setq sublain-blame-revision (if (string= rev "") "HEAD" rev))
  (sublain-save-line
   (sublain-blame-update)))

(defun sublain-blame-toggle-verbose ()
  (interactive)
  (setq sublain-blame-verbose (not sublain-blame-verbose))
  (sublain-save-line
   (sublain-blame-update)))

;;; sublain-cat
(defun sublain-cat (target rev)
  (interactive "sTarget: 
sRevision: ")
  (switch-to-buffer (generate-new-buffer (concat target "@" rev)))
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (sublain-call-svn "cat" (concat target "@" rev))
    (set-buffer-modified-p nil))
  (goto-char (point-min))
  (let ((buffer-file-name target))
    (set-auto-mode))
  (view-mode))

;;; sublain-proplist
(defvar sublain-proplist-buffer-name "*sublain-proplist*")

(defun sublain-proplist (target rev)
  (interactive "sTarget: 
sRevision: ")
  (with-current-buffer (get-buffer-create sublain-proplist-buffer-name)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (sublain-call-svn "proplist" "-v" (concat target "@" rev))
      (set-buffer-modified-p nil))
    (goto-char (point-min)))
  (display-buffer sublain-proplist-buffer-name))

;;; sublain-info
(defvar sublain-info-buffer-name "*sublain-info*")

(defun sublain-info (target rev)
  (interactive "sTarget: 
sRevision: ")
  (with-current-buffer (get-buffer-create sublain-info-buffer-name)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (sublain-call-svn "info" (concat target "@" rev))
      (set-buffer-modified-p nil))
    (goto-char (point-min)))
  (display-buffer sublain-info-buffer-name))

;;; process
(defvar sublain-process-name "sublain")
(defvar sublain-svn-program-name "svn")

(defun sublain-call-svn (&rest arg)
  ;(message "%s" arg)
  (apply 'call-process sublain-svn-program-name nil t nil arg))

;;; misc
(defun sublain-flatten-list (&rest xs)
  (sublain-flatten-list-1 xs))

(defun sublain-flatten-list-1 (xs)
  (cond ((null xs) nil)
	((listp (car xs)) (append (sublain-flatten-list-1 (car xs))
				  (sublain-flatten-list-1 (cdr xs)))) 
	(t (cons (car xs) (sublain-flatten-list-1 (cdr xs))))))

(defun sublain-get-new-buffer (name)
  (let ((buf (get-buffer name)))
    (when buf
      ;; Kill buffer to delete previous ``default-directory''
      (kill-buffer buf)))
  (get-buffer-create name))

;;; sublain.el ends here
