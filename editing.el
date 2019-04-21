;; TODO: This does not write comment on empty line, and also does not
;; keep things highlighted
(defun my-comment-line (n)
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))
       (save-excursion
         (goto-char (region-end))
         (line-end-position)))
    (when (and (eq last-command 'comment-line-backward)
               (natnump n))
      (setq n (- n)))
    (let ((range
           (list (line-beginning-position)
                 ;; (goto-char (line-end-position n)))))
                 (line-end-position n))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    ;; (forward-line 1)
    ;; (back-to-indentation)
    (unless (natnump n)
      (setq this-command 'comment-line-backward))))

(global-unset-key (kbd "C-;"))
(global-set-key (kbd "C-;") 'my-comment-line)

(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-/") 'my-comment-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-region (&optional n)
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1))
                          (newline))))))
        (dotimes (i (abs (or n 1)))
          (insert text))))
    (if use-region nil
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (forward-line 1)
        (forward-char pos)))))

(global-unset-key (kbd "C-S-d"))
(global-set-key (kbd "C-S-d") 'duplicate-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quick-cut-line ()
  "Cut the whole line that point is on.  Consecutive calls to this command
    append each line to the kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'quick-cut-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end)))
    (delete-region beg end))
  (beginning-of-line 1)
  (setq this-command 'quick-cut-line))

(global-unset-key (kbd "C-S-k"))
(global-set-key (kbd "C-S-k") 'quick-cut-line)

;; make this line or region

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))

(defun copy-line (&optional arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (let* ((deactivate-mark nil)
           (pos-col (current-column))
           (top-col (save-excursion (goto-char (region-beginning)) (current-column)))
           (bot-col (save-excursion (goto-char (region-end)) (current-column)))
           (pos-line (line-number-at-pos))
           (top-line (line-number-at-pos (region-beginning)))
           (bot-line (line-number-at-pos (region-end)))
           (adjust (if (< 0 arg) 1 -1)))
      (copy-line)
      (goto-line top-line)
      (dotimes (_ (+ 1 (- bot-line top-line)))
        (kill-whole-line))
      (forward-line adjust)
      (yank)
      (if (= pos-line bot-line)
          (progn
            (goto-line (+ adjust top-line))
            (move-to-column top-col t)
            (push-mark (point))
            (activate-mark)
            (goto-line (+ adjust bot-line))
            (move-to-column bot-col t))
        (progn
          (goto-line (+ adjust bot-line))
          (move-to-column bot-col t)
          (push-mark (point))
          (activate-mark)
          (goto-line (+ adjust top-line))
          (move-to-column top-col t)))))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  (interactive "*p")
  (save-column
   (move-text-internal arg)))

(defun move-text-up (arg)
  (interactive "*p")
  (save-column
   (move-text-internal (- arg))))

(global-set-key (kbd "C-S-<up>") 'move-text-up)
(global-set-key (kbd "C-S-<down>") 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(require 'simpleclip)

(defun nonbreaking-return (&optional n)
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (end-of-line)
    (newline)
    (indent-according-to-mode)))

(with-eval-after-load 'cua-base
  (define-key cua-global-keymap [C-return] nil)
  (global-set-key [(C-return)] 'nonbreaking-return))

(global-set-key [C-S-return] 'open-line)

;; (delete-selection-mode 1)

(setq save-interprogram-paste-before-kill t)
(setq select-enable-clipboard nil)

(defun cua-paste (&optional _)
  (interactive "p")
  (simpleclip-paste))

(defun cua-copy-region (&optional _)
  (interactive "p")
  (simpleclip-copy (region-beginning) (region-end)))

(defun cua-cut-region (&optional _)
  (interactive "p")
  (simpleclip-cut (region-beginning) (region-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-forward (&optional arg)
  "Move ARG times to start of a set of the same syntax characters."
  (interactive "p")
  (setq arg (or arg 1))
  (while (and (> arg 0)
              (not (eobp))
              (skip-syntax-forward (string (char-syntax (char-after)))))
    (setq arg (1- arg)))
  (while (and (< arg 0)
              (not (bobp))
              (skip-syntax-backward
               (string (char-syntax (char-before)))))
    (setq arg (1+ arg))))

(defun my-backward (&optional arg)
  (interactive "p")
  (my-forward (- (or arg 1))))

(defun my-select-forward (&optional arg)
  (interactive "p")
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (backwards (and mark-active (> (mark) (point))))
        (beg (and mark-active (mark-marker))))
    (unless beg
      (setq beg (point-marker)))
    (my-forward)
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

(defun my-select-backward (&optional arg)
  (interactive "p")
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (backwards (and mark-active (> (mark) (point))))
        (beg (and mark-active (mark-marker))))
    (unless beg
      (setq beg (point-marker)))
    (my-backward)
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

(defun my-control-delete (&optional arg)
  (interactive "p")
  (my-select-forward)
  (kill-region (region-beginning) (region-end)))

(defun my-control-backspace (&optional arg)
  (interactive "p")
  (my-select-backward)
  (kill-region (region-beginning) (region-end)))

(global-unset-key (kbd "C-<right>"))
(global-unset-key (kbd "C-<left>"))

(global-set-key (kbd "C-<right>") 'my-forward)
(global-set-key (kbd "C-<left>") 'my-backward)

(global-set-key (kbd "C-S-<right>") 'my-select-forward)
(global-set-key (kbd "C-S-<left>") 'my-select-backward)

(global-unset-key [C-delete])
(global-unset-key [C-backspace])

(global-set-key [C-delete] 'my-control-delete)
(global-set-key [C-backspace] 'my-control-backspace)

(defun my-backspace (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-backward-chars (or arg 1))))

(defun my-delete (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-forward-chars (or arg 1))))

(global-unset-key [delete])
(global-unset-key [backspace])

(global-set-key [delete] 'my-delete)
(global-set-key [backspace] 'my-backspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mark-whole-word (&optional arg allow-extend)
  (interactive "P\np")
  (let ((num (prefix-numeric-value arg)))
    (unless (eq last-command this-command)
      (if (natnump num)
          (skip-syntax-forward "\\s-")
        (skip-syntax-backward "\\s-")))
    (unless (or (eq last-command this-command)
                (if (natnump num)
                    (looking-at "\\b")
                  (looking-back "\\b")))
      (if (natnump num)
          (left-word)
        (right-word)))
    (mark-word arg allow-extend)))

(global-unset-key (kbd "C-d"))
(global-set-key (kbd "C-d") 'mark-whole-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (beg (point-marker)))
    (when mark-active
      (forward-line 1))
    (end-of-line)
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

(global-set-key (kbd "C-S-L") 'select-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
