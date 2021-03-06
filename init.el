(load-theme 'misterioso)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Fullscreen the new window
(set-frame-parameter nil 'fullscreen 'fullboth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default cursor-type '(bar . 3))
(set-cursor-color "#fff")

;; To prevent emacsclient getting settings from elsewhere or not listening
(require 'frame)
(defun after-creation-hook (frame)
 ; (global-display-line-numbers-mode 1)
  (modify-frame-parameters
   frame (list (cons 'cursor-color "#fff") ; DeepSkyBlue is a nice color too
               (cons 'cursor-type '(bar . 3)))))

(add-hook 'after-make-frame-functions 'after-creation-hook)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; editor background color #2d3743

(set-face-attribute 'line-number nil
                    :background "#28313c"
                    :foreground "#666")

(set-face-attribute 'line-number-current-line nil
                    :background "#28313c"
                    :foreground "#ccc")

(set-fringe-mode '(0 . 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq require-final-newline t)

(delete-selection-mode t)
(global-auto-revert-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq scroll-conservatively 10000)
;; (setq auto-window-vscroll nil)
;; (setq scroll-margin 3)
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (require 'pixel-scroll)
; (pixel-scroll-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq jit-lock-defer-time 0)
(setq fast-but-imprecise-scrolling t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :height 136)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default mode-line-format
          (list
           "%b"
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default display-line-numbers-current-absolute nil
              display-line-numbers-width 4
              display-line-numbers-widen t)

(global-display-line-numbers-mode)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-quit ()
  (interactive)
  (save-some-buffers t)
  (if (daemonp)
      (delete-frame)
    (kill-emacs)))

;; Save and close emacs (without killing daemon)
(global-unset-key (kbd "C-q"))
(global-set-key (kbd "C-q") 'my-quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; saveplace remembers the location in a file when saving files
(setq save-place-file
      (expand-file-name "saveplace" (concat user-emacs-directory "auto-backups")))

(save-place-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save backups in ~/.emacs.d/backups
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "auto-backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-backups") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To change the expiration date of all buffers to 1 day
(setq clean-buffer-list-delay-general 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-all ()
  (interactive)
  (save-some-buffers t))
  
(add-hook 'focus-out-hook 'save-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cua-mode t)
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

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
  (delete-region (region-beginning) (region-end)))

(defun my-control-backspace (&optional arg)
  (interactive "p")
  (my-select-backward)
  (delete-region (region-beginning) (region-end)))

(global-unset-key [C-delete])
(global-unset-key [C-backspace])

(global-set-key [C-delete] 'my-control-delete)
(global-set-key [C-backspace] 'my-control-backspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l") 'select-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'save-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-dup (arg)
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin
                    arg
                    (* arg
                       (length region)))))))

(global-unset-key (kbd "C-S-d"))
(global-set-key (kbd "C-S-d") 'my-dup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tab inserts 2 spaces
(global-set-key "\t" (lambda ()
                       (interactive) (insert-char 32 2)))
