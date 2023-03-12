;; -*- coding: utf-8; lexical-binding: t; -*-

;; Use the system clipboard
;; @see https://www.emacswiki.org/emacs/CopyAndPaste
;; So `C-y' could paste from clipboard if you are NOT using emacs-nox
;; I only use `paste-from-x-clipboard', not `C-y'.
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; kill-ring and clipboard are same? No, it's annoying!
(setq save-interprogram-paste-before-kill nil)

(defun my-pclip (str-val)
  "Put STR-VAL into clipboard."
  (let* ((win64-clip-program (executable-find "clip.exe"))
         ssh-client)
    (cond
     ;; Windows 10 or Windows 7
     ((and win64-clip-program)
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) win64-clip-program)))

     ;; Windows
     ((and (eq system-type 'windows-nt)  (fboundp 'w32-set-clipboard-data))
      ;; Don't know why, but on Windows 7 this API does not work.
      (w32-set-clipboard-data str-val))

     ;; If Emacs is inside an ssh session, place the clipboard content
     ;; into "~/.tmp-clipboard" and send it back into ssh client
     ;; Make sure you already set up ssh correctly.
     ;; Only enabled if ssh server is macOS
     ((and (setq ssh-client (getenv "SSH_CLIENT"))
           (not (string= ssh-client ""))
           *mac*)
      (let* ((file "~/.tmp-clipboard")
             (ip (car (split-string ssh-client "[ \t]+")))
             (cmd (format "scp %s %s@%s:~/" file my-ssh-client-user ip)))
        (when my-ssh-client-user
          (my-write-to-file str-val file)
          (shell-command cmd)
          ;; clean up
          (delete-file file))))

     ;; xclip can handle
     (t
      (xclip-set-selection 'clipboard str-val)))))
;; }}

(defun copy-yank-str (msg &optional clipboard-only)
  (unless clipboard-only (kill-new msg))
  (my-pclip msg)
  msg)

(defun cp-filename-of-current-buffer (&optional n)
  "Copy file name (NOT full path) into the yank ring and OS clipboard.
If N is not nil, copy file name and line number."
  (interactive "P")
  (when buffer-file-name
    (let* ((filename (file-name-nondirectory buffer-file-name))
           (s (if n (format "%s:%s" filename (line-number-at-pos)) filename)))
      (copy-yank-str s)
       (message "%s => clipboard&kill-ring" s))))

(defun cp-fullpath-of-current-buffer ()
  "Copy full path into the yank ring and OS clipboard"
  (interactive)
  (when buffer-file-name
    (copy-yank-str (file-truename buffer-file-name))
    (message "file full path => clipboard & yank ring")))

(defun my-gclip ()
  "Get clipboard content."
  (let* ((powershell-program (executable-find "powershell.exe")))
    (cond
     ;; Windows
     ((and (eq system-type 'windows-nt) (fboundp 'w32-get-clipboard-data))
      ;; `w32-set-clipboard-data' makes `w32-get-clipboard-data' always return null
      (w32-get-clipboard-data))

     ;; Windows 10
     (powershell-program
      (string-trim-right
       (with-output-to-string
         (with-current-buffer standard-output
           (call-process powershell-program nil t nil "-command" "Get-Clipboard")))))

     ;; xclip can handle
     (t
      (xclip-get-selection 'clipboard)))))

(defun my-use-selected-string-or-ask (&optional hint)
  "Use selected region or ask for input.
If HINT is empty, use symbol at point."
  (cond
   ((region-active-p)
    (my-selected-str))
   ((or (not hint) (string= "" hint))
    (thing-at-point 'symbol))
   (t
    (read-string hint))))

(defun clipboard-to-kill-ring ()
  "Copy from clipboard to `kill-ring'."
  (interactive)
  (let* ((warning-minimum-level :emergency))
    (kill-new (my-gclip)))
  (message "clipboard => kill-ring"))

;; {{
(defun my-prepare-candidate-fit-into-screen (s)
  (let* ((w (frame-width))
         ;; display kill ring item in one line
         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
    ;; strip the whitespace
    (setq key (replace-regexp-in-string "^[ \t]+" "" key))
    ;; fit to the minibuffer width
    (if (> (length key) w)
        (setq key (concat (substring key 0 (- w 4)) "...")))
    (cons key s)))

;; designe for kill ring to clipboard
(defun my-select-from-kill-ring (fn)
  "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse `kill-ring'."
  (interactive "P")
  (let* ((candidates (cl-remove-if
                       (lambda (s)
                         (or (< (length s) 5)
                             (string-match-p "\\`[\n[:blank:]]+\\'" s)))
                       (delete-dups kill-ring)))
          (ivy-height (/ (frame-height) 2)))
     (ivy-read "Browse `kill-ring':"
               (mapcar #'my-prepare-candidate-fit-into-screen candidates)
               :action fn)))

(defun kill-ring-to-clipboard ()
  "Copy from `kill-ring' to clipboard."
  (interactive)
  (my-select-from-kill-ring (lambda (s)
                              (let* ((summary (car s))
                                     (hint " => clipboard" )
                                     (msg (if (string-match-p "\.\.\.$" summary)
                                              (substring summary 0 (- (length summary) (length hint)))
                                            msg)))
                                ;; cc actual string
                                (my-pclip (cdr s))
                                ;; echo
                                (message "%s%s" msg hint)))))
;; }}

(defun copy-to-x-clipboard (&optional num)
  "If NUM equals 1, copy the down-cased string.
If NUM equals 2, copy the capitalized string.
If NUM equals 3, copy the up-cased string.
If NUM equals 4, indent 4 spaces."
  (interactive "P")
  (let* ((thing (my-use-selected-string-or-ask "")))
    (if (region-active-p) (deactivate-mark))
    (cond
     ((not num))
     ((= num 1)
      (setq thing (downcase thing)))
     ((= num 2)
      (setq thing (capitalize thing)))
     ((= num 3)
      (setq thing (upcase thing)))
     ((= num 4)
      (setq thing (string-trim-right (concat "    "
                                             (mapconcat 'identity (split-string thing "\n") "\n    ")))))
     (t
      (message "C-h f copy-to-x-clipboard to find right usage")))

    (my-pclip thing)
    (if (not (and num (= 4 num))) (message "kill-ring => clipboard")
      (message "thing => clipboard!"))))

(defun paste-from-x-clipboard(&optional n)
  "Remove selected text and paste string clipboard.
If N is 1, we paste diff hunk whose leading char should be removed.
If N is 2, paste into `kill-ring' too.
If N is 3, converted dashed to camel-cased then paste.
If N is 4, rectangle paste."
  (interactive "P")
  (when (and (functionp 'evil-normal-state-p)
             (functionp 'evil-move-cursor-back)
             (evil-normal-state-p)
             (not (eolp))
             (not (eobp)))
    (forward-char))
  (let* ((str (my-gclip))
         (fn 'insert))

    (when (> (length str) (* 256 1024))
      ;; use light weight `major-mode' like `js-mode'
      (when (derived-mode-p 'js2-mode) (js-mode 1))
      ;; turn off syntax highlight
      (font-lock-mode -1))

    ;; past a big string, stop lsp temporarily
    (when (and (> (length str) 1024)
               (boundp 'lsp-mode)
               lsp-mode)
      (lsp-disconnect)
      (run-at-time 300 nil  #'lsp-deferred))

    (my-delete-selected-region)

    ;; paste after the cursor in evil normal state
    (cond
     ((not n)) ; do nothing
     ((= 1 n)
      (setq str (replace-regexp-in-string "^\\(+\\|-\\|@@ $\\)" "" str)))
     ((= 2 n)
      (kill-new str))
     ((= 3 n)
      (setq str (mapconcat (lambda (s) (capitalize s)) (split-string str "-") "")))
     ((= 4 n)
      (setq fn 'insert-rectangle)
      (setq str (split-string str "[\r]?\n"))))
    (funcall fn str)))

(provide 'init-clipboard)
