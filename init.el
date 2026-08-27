;; -*- lexical-binding: t -*-

;;;;;;;;;;;;
;;; init ;;;
;;;;;;;;;;;;

;; no message in scratch buffer
(setq initial-scratch-message "")

;; no startup screen
(setq inhibit-startup-screen t)

;; Super handy macro for loading packages but not stopping the init
;; process if they aren't found.
;; https://www.gnu.org/software/emacs/manual/html_node/use-package/index.html
(require 'use-package)
;; install packages from their git repositories with straight.el
;; (bootstrapped in early-init.el)
(setq straight-use-package-by-default t)

;; Use Emacs' built-in versions of these libraries, even though they are
;; also available from package repositories. Registering them with
;; straight prevents repository checkouts when other packages depend on
;; them (e.g. magit requires seq and transient).
(dolist (pkg '(seq let-alist xref jsonrpc use-package transient))
  (straight-use-package (list pkg :type 'built-in)))

(use-package xdg
  :straight nil  ;; Emacs built-in
  )

;; organize files out of main emacs directory
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :commands (no-littering-expand-etc-file-name no-littering-var-directory)
  :config (no-littering-theme-backups))

;; Stop customize from writing to my init file
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package auto-compile
  :defer 1
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t)
  :config
  (auto-compile-on-save-mode))

;; Start an Emacs server if one is not already running
;; this allows use of emacsclient
(use-package server
  :straight nil  ;; Emacs built-in
  :demand t
  :config (unless (server-running-p) (server-start)))

;; Theme
;; https://github.com/bbatsov/batppuccin-emacs
(use-package batppuccin
  :config
  (load-theme 'batppuccin-latte t))

;; Follow system light/dark mode
;; https://github.com/LionyxML/auto-dark-emacs
(use-package auto-dark
  :demand
  :init (auto-dark-mode)
  :custom
  (auto-dark-themes '((batppuccin-mocha) (batppuccin-latte))))

;; Pretty icons
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))
  (setq all-the-icons-scale-factor 1))

;; Cooler modeline
;; https://seagle0128.github.io/doom-modeline/
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-buffer-encoding nil)
  :custom-face
  (mode-line ((t (:family "Hack" :height 130))))
  (mode-line-active ((t (:family "Hack" :height 130))))
  (mode-line-inactive ((t (:family "Hack" :height 130))))
  )

(use-package diminish)

;; On Linux Emacs doesn't use the shell env if it's not started from the shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Constants
(defconst my-org-directory (expand-file-name "~/Org"))
(defconst my-snippets-dir (expand-file-name "snippets" user-emacs-directory))

;;;;;;;;;;;;;;;;
;;; behavior ;;;
;;;;;;;;;;;;;;;;

;; no backup files, be bold!
(setq-default make-backup-files nil)

;; no lockfiles (.#<file>), they cause trouble with autoreloading code
(setq create-lockfiles nil)

;; no messages in echo area about saving files
(setq save-silently t)

;; mondays are the first day of my week (for M-x calendar)
(setq-default calendar-week-start-day 1)

;; Delay syntax-highlighting until typing stops to avoid micro-stutters
(setq redisplay-skip-fontification-on-input t)

;; Cycle between candidates when there are not a lot of them
(setq completion-cycle-threshold 3)

;; hide non-relevant candidated in M-x
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; keep popping mark ring with C-SPC after the initial C-u C-SPC
(setq set-mark-command-repeat-pop t)

;; remember point location when reopening a file
(use-package saveplace
  :straight nil  ;; Emacs built-in
  :init
  (save-place-mode)
  :config
  (setq save-place-file (no-littering-expand-var-file-name "saveplace")))

;; Emacs built-in diff interface
;; https://www.gnu.org/software/emacs/manual/html_node/ediff/index.html
(use-package ediff
  :straight nil  ;; Emacs built-in
  :config
  ;; don't open another frame, reuse current one
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package repeat
  :straight nil
  :config
  (repeat-mode 1))

;; run garbage collection when frame loses focus, which should mean I'm not using
;; Emacs at that time so I won't care about any slowdown
(add-function :after after-focus-change-function
              (defun my-garbage-collect-maybe ()
                (unless (frame-focus-state)
                  (garbage-collect))))

(context-menu-mode)

;;;;;;;;;;;;;;
;;; editor ;;;
;;;;;;;;;;;;;;

(setq my-line-length 88)

;; always indent with spaces unless major mode overrides
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; enable y/n answers
(setopt use-short-answers t)

;; save files to home directory by default
(setq-default default-directory '~)

;; (much) bigger kill ring
(setq-default kill-ring-max 5000)

;; delete the selection with a keypress
(delete-selection-mode t)

;; enable view-mode automatically in read-only buffers
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/View-Mode.html
(setq view-read-only t)

(use-package scroll-lock-mode
  :straight nil ;; built-in
  :hook (view-mode . scroll-lock-mode)
  )

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; automatically revert buffers for files that changed on disk when the open buffer has no
;; unsaved changes
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)

;; echo keystrokes after 0.1s (default is 1s)
(setq echo-keystrokes 0.1)

;; integrate copy/paste with X
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      )

;; avoid duplicates in kill ring
(setq kill-do-not-save-duplicates t)

;; save the existing clipboard content into kill ring before overwriting it
(setq save-interprogram-paste-before-kill t)

;; Mouse yank inserts at point, not at cursor
(setq mouse-yank-at-point t)

;; UTF-8 all the way
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; required for non-English keys (like dead acute) to work
;; http://osdir.com/ml/help-gnu-emacs-gnu/2009-05/msg00170.hotel
(require 'iso-transl)

;; disable right-to-left language support
;; improves performance on large buffers
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; use directory name in buffer names of files with the same name
(use-package uniquify
  :straight nil  ;; Emacs built-in
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        ;; rename after killing uniquified
        uniquify-after-kill-buffer-p t
        ;; don't muck with special buffers
        uniquify-ignore-buffers-re "^\\*"))

;; savehist keeps track of some history
(use-package savehist
  :straight nil  ;; Emacs built-in
  :config
  (setq savehist-additional-variables
        '(kill-ring
          my-git-commit-assisted-history
          regexp-search-ring
          search-ring
          vertico-repeat-history)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (no-littering-expand-var-file-name "savehist"))
  ;; keep histories across sessions
  (savehist-mode +1))

;; save recent files
(use-package recentf
  :straight nil  ;; Emacs built-in
  :config
  (setq recentf-save-file (no-littering-expand-var-file-name "recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory)))

(use-package isearch
  :straight nil
  :custom
  (isearch-lazy-count t))

;; automatically save buffers associated with files on buffer and window switch
;; https://github.com/bbatsov/super-save
(use-package super-save
  :diminish
  :config
  ;; save buffers automatically when Emacs is idle
  (setq super-save-auto-save-when-idle t)
  ;; don't display "Wrote file..." messages in the echo area
  (setq super-save-silent t)
  ;; disable the built-in auto-save (backup files) since super-save handles it
  (setq auto-save-default nil)
  (super-save-mode +1))

;; TRAMP is awesome
;; https://www.gnu.org/software/tramp/
(use-package tramp
  :straight (:type built-in)  ;; Emacs built-in
  :config
  (setq
   ;; don't pollute .emacs.d directory
   tramp-persistency-file-name (no-littering-expand-var-file-name "tramp")
   ;; default to SSH
   tramp-default-method "ssh")
  ;; manage yadm using Magit (also see yadm defun in magit section)
  ;; https://philjackson.github.io/yadm/emacs/magit/2021/07/25/using-yadm-via-magit/
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL" "/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))
                 ))
  ;; Make the yadm Tramp method inherit the user's local PATH, so
  ;; subprocesses spawned by Magit (git, GIT_SSH_COMMAND wrapper,
  ;; onlykey-agent, …) can find user-installed tools. Scoped to the
  ;; "yadm" protocol so other Tramp connections keep their defaults.
  (connection-local-set-profile-variables
   'tramp-yadm-profile
   '((tramp-remote-path . (tramp-own-remote-path
                           tramp-default-remote-path))))
  (connection-local-set-profiles
   '(:application tramp :protocol "yadm")
   'tramp-yadm-profile))

(auto-compression-mode 1)

(set-default 'imenu-auto-rescan t)

;; enable set goal column (C-x C-n)
(put 'set-goal-column 'disabled nil)

(use-package simple
  :straight nil  ;; Emacs built-in
  :bind (("C-x C-M-t" . transpose-regions)
         ;; upcase-downcase word at point or region if set
         ("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim))
  :config
  ;; enabled change region case commands
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

;; bookmarks
(use-package bookmark
  :straight nil  ;; Emacs built-in
  :config
  (setq bookmark-default-file (no-littering-expand-var-file-name "bookmarks")
        bookmark-save-flag 1))

(use-package align
  :straight nil  ;; Emacs built-in
  :bind (("C-x \\" . align-regexp)))

;; Visually align org and markdown tables
;; https://github.com/casouri/valign
(use-package valign
  :custom
  (valign-max-table-size 10000))

(use-package ffap
  :straight nil  ;; Emacs built-in
  :bind (("C-x C-." . find-file-at-point)
         ("C-x C->" . ffap-next))
  :custom
  ;; do not attempt to resolve what look like hostnames
  (ffap-machine-p-known 'reject))

;; avy allows us to effectively navigate to visible things
(use-package avy
  :after (embark)
  :preface
  ;; By Chmouel Boudjnah https://mastodon.social/@chmouel@fosstodon.org/109715305722356540
  (defun my-avy-copy-word (_arg)
    (interactive "p")
    (let ((start-window (selected-window))
          (start-point (point)))
      (call-interactively 'avy-goto-symbol-1)
      (let ((symbol (thing-at-point 'symbol)))
        (select-window start-window)
        (goto-char start-point)
        (when symbol
          (kill-new symbol)))))
  (defun my-avy-embark (_arg)
    (interactive "p")
    (let ((start-window (selected-window))
          (start-point (point)))
      (call-interactively 'avy-goto-symbol-1)
      (let ((symbol (thing-at-point 'symbol)))
        (select-window start-window)
        (goto-char start-point)
        (when symbol
          (embark-act symbol)))))
  :config (setq avy-background t
                avy-style 'at-full))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(setq-default fill-column my-line-length)

(use-package emacs
  :straight nil  ;; Emacs built-in
  :preface
  (defun my-fill-or-unfill ()
    "Toggle between filling or unfilling the current paragraph."
    (interactive)
    ;; keep an active region so the toggle works on it repeatedly
    (let (deactivate-mark)
      (if (eq last-command this-command)
          (progn
            ;; reset the toggle so a third invocation fills again
            (setq this-command nil)
            (call-interactively #'unfill-paragraph))
        (call-interactively #'fill-paragraph))))
  :bind (("M-q" . my-fill-or-unfill)))

;; progressively expand region around cursor, tree-sitter based
;; https://emacsredux.com/blog/2026/03/03/expreg-expand-region-reborn/
;; https://github.com/casouri/expreg
(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract))
  (:repeat-map expreg-repeat-map
               ("=" . expreg-expand)
               ("-" . expreg-contract)))

;; automatically clean up whitespace on save only on initially clean buffers
;; disable by setting whitespace-cleanup-mode to nil in dir or local variables
(use-package whitespace-cleanup-mode
  :bind (("C-c M-w" . whitespace-cleanup))
  :config (global-whitespace-cleanup-mode))

;; saner regex syntax
(use-package re-builder
  :straight nil  ;; Emacs built-in
  :config
  (setq reb-re-syntax 'string))

(use-package eshell
  :straight nil  ;; Emacs built-in
  :config
  (setq eshell-directory-name (no-littering-expand-var-file-name "eshell")))

(setq semanticdb-default-save-directory
      (no-littering-expand-var-file-name "semanticdb"))

;; crux is a collection of general editing utilities, see below for keybindings
;; https://github.com/bbatsov/crux
(use-package crux
  :custom
  ;; use ghostel as terminal for `crux-visit-term-buffer'
  (crux-term-func #'ghostel)
  (crux-term-buffer-name "ghostel")
  :bind (("C-c O" . crux-open-with)
         ("C-a" . crux-move-beginning-of-line)
         ("C-c C-i" . crux-indent-defun)
         ("C-c e" . crux-eval-and-replace)
         ("C-c C-d" . crux-duplicate-current-line-or-region)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c C-k" . crux-kill-whole-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)))

;; Move line or region up and down
;; https://github.com/emacsfodder/move-text
(use-package move-text
  :bind (([(control shift up)]  . move-text-up)
         ([(control shift down)]  . move-text-down)))

;; display undo history as a tree and allow moving around its branches
;; https://github.com/casouri/vundo
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; show uncommitted changes in the gutter
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (global-diff-hl-mode +1)
  ;; disable on slow TRAMP connections with diff-hl-disable-on-remote to t
  )

;; improved killing and yanking
;; https://github.com/leoliu/easy-kill
(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; use settings from .editorconfig file when present
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :straight (:type built-in)  ;; built-in since Emacs 30
  :diminish
  :config (editorconfig-mode 1))

;; show all remaining key combinations when doing multi-key commands
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight (:type built-in)  ;; built-in since Emacs 30
  :custom
  (which-key-idle-delay 0.5)
  (which-key-max-description-length 80)
  :init
  (which-key-setup-side-window-right-bottom)
  :hook
  (after-init . which-key-mode))

;; better Emacs help
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind (;; includes macros, default describe-function
         ("C-h f" . helpful-callable)
         ;; excludes macros, default Info-goto-emacs-command-node
         ("C-h F" . helpful-function)
         ("C-h v" . helpful-variable)
         ;; default describe-key
         ("C-h k" . helpful-key)
         ;; default describe-coding-system
         ("C-h C" . helpful-command)
         ;; default display-local-help
         ("C-h ." . helpful-at-point)
         ))

;; multiple cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this-symbol)
         ("C-<" . mc/mark-previous-like-this-symbol)
         ("C-M->" . mc/unmark-next-like-this)
         ("C-M-<" . mc/unmark-previous-like-this)
         ("C-S-c c" . mc/edit-lines)
         ("C-S-c C-c" . mc/edit-lines)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c a" . mc/edit-beginnings-of-lines)
         ("C-S-c C-a" . mc/edit-beginnings-of-lines)
         ("C-S-c e" . mc/edit-ends-of-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)
         ("C-S-c d" . mc/mark-all-dwim)
         ("C-S-c C-d" . mc/mark-all-dwim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window and frame management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-left-column-modes '(magit-mode org-mode)
  "Major modes that share the left column window, replacing each other.")

(defvar my-side-window-size 77 "Size of my side bars.")

(defvar my-frame-width-limit-for-sidebars
  (* my-side-window-size 3)
  "Use the right-side window when frame width is above this limit.")

(defvar my-frame-width-limit-for-left-column
  (* my-side-window-size 2)
  "Use the left column for Org/Magit when frame width is above this limit.")

;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(defun make-display-buffer-matcher-function (major-modes)
  "Match buffers by MAJOR-MODES for DISPLAY-BUFFER-ALIST."
  (lambda (buffer-name _action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

(defun maybe-display-in-direction (buffer action)
  "Open BUFFER in a directional window only if the current frame is wide enough."
  (when (> (frame-width) my-frame-width-limit-for-sidebars)
    (display-buffer-in-direction buffer action)))

;; Conditionally open a buffer in a side window only if frame is large enough
(defun maybe-display-in-side-window (buffer action)
  "Open BUFFER in a side window only if the current frame is wide enough."
  (when (> (frame-width) my-frame-width-limit-for-sidebars)
    (display-buffer-in-side-window buffer action)))

;; Custom matcher for popper popups in display-buffer-alist, since
;; popper's own `popper-display-control-p' is inert when
;; `popper-display-control' is nil.
(defun my-popper-popup-matcher (buffer-or-name _action)
  "Match popper popup buffers for DISPLAY-BUFFER-ALIST.
Matches buffers designated as popups by `popper-reference-buffers'
\(or lowered with `popper-toggle-type'), but not popups raised to
regular status."
  (when (boundp 'popper-popup-status)
    (let* ((buffer (if (bufferp buffer-or-name)
                       buffer-or-name
                     (get-buffer buffer-or-name)))
           (status (and buffer (buffer-local-value 'popper-popup-status buffer))))
      (and buffer
           (or (memq status '(popup user-popup))
               (and (not (eq status 'raised))
                    (popper-popup-p buffer)))))))

;; Narrow-frame fallbacks for the width-gated rules below: they act only
;; when the frame is too narrow for the corresponding sidebar, so they
;; cannot fire while the wide-frame path applies.
(defun my-display-full-frame-left-narrow (buffer action)
  "Display BUFFER full frame when too narrow for the left column."
  (unless (> (frame-width) my-frame-width-limit-for-left-column)
    (display-buffer-full-frame buffer action)))

(defun my-display-at-bottom-sidebar-narrow (buffer action)
  "Display BUFFER at the bottom when too narrow for the right sidebar."
  (unless (> (frame-width) my-frame-width-limit-for-sidebars)
    (display-buffer-at-bottom buffer action)))

(defun my-display-in-left-column (buffer alist)
  "Display BUFFER in the Magit/Org left column.

If a window already shows a `my-left-column-modes' buffer, reuse it
(Magit and Org buffers replace each other there).  Otherwise, if the
frame is too narrow for the full sidebars and already shows more than
one window, take over its leftmost window instead of crowding in a
third one.  Otherwise create a new left column sized
`my-side-window-size' and rebalance the remaining windows."
  (when (> (frame-width) my-frame-width-limit-for-left-column)
    (let ((family-win
           (seq-find (lambda (win)
                       (with-current-buffer (window-buffer win)
                         (apply #'derived-mode-p my-left-column-modes)))
                     (window-list nil 'no-mini)))
          leftmost-win)
      (cond
       (family-win
        (set-window-buffer family-win buffer)
        family-win)
       ((and (<= (frame-width) my-frame-width-limit-for-sidebars)
             (> (length (window-list nil 'no-mini)) 1)
             (setq leftmost-win
                   (seq-find (lambda (win) (not (window-dedicated-p win)))
                             (window-list nil 'no-mini))))
        (set-window-buffer leftmost-win buffer)
        leftmost-win)
       (t
        (let ((new-window
               (display-buffer-in-direction
                buffer
                (append (list (cons 'direction 'left)
                              (cons 'window 'main)
                              (cons 'window-width my-side-window-size))
                        alist))))
          (when (window-live-p new-window)
            ;; rebalance only the sibling subtree; balancing the whole
            ;; root would undo the column's fixed width
            (balance-windows (window-next-sibling new-window)))
          new-window))))))

;; This is my window configuration
;;
;; Emacs manual:
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers.html
;;
;; Great article by Mastering Emacs:
;;   https://www.masteringemacs.org/article/demystifying-emacs-window-manager
;;
;; It's tailored for my main displays: 4k and 1080p.
;;
;; On 4k it's a 4-column layout:
;; 1. Left column for Magit, Org buffers (they replace each other)...
;; 2. A file I'm working on.
;; 3. Another file (optional, if I split column 2 vertically).
;; 4. Right side window dedicated to help, documentation, flymake, grep, imenu...
;;
;; On 1080p it's a 2-column layout:
;; 1. Left column for Magit, Org buffers, as above...
;; 2. Files or anything else.
;;
;; Narrower frames use a single-column layout.
;;
;; All popper popups go to a bottom buffer by default (catch-all rule).
;; Some special rules display popups on the right.
;;
;; f12 is bound to toggle any window full-frame and back (see zoom-window).
(setq display-buffer-alist
      `(;; bottom side window
        (;; `org-capture' key selection and `org-add-log-note'
         "\\*Org \\(Select\\|Note\\)\\*"
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))
        ;; display on left preferentially; full frame when too narrow
        (,(make-display-buffer-matcher-function my-left-column-modes)
         (display-buffer-reuse-mode-window my-display-in-left-column my-display-full-frame-left-narrow))
        ;; right side window
        ;; These exceptions take precedence over the popper catch-all below.
        (,(rx (| (regexp "\\*eldoc.*\\*")
                 (regexp "\\*Embark Collect:.*\\*")
                 (regexp "\\*Embark Export:.*\\*")))
         (maybe-display-in-side-window my-display-at-bottom-sidebar-narrow)
         (dedicated . t)
         (side . right)
         (slot . 0)
         (window-width . ,my-side-window-size))
        (,(make-display-buffer-matcher-function
           '(embark-collect-mode
             flymake-diagnostics-buffer-mode
             grep-mode
             help-mode
             helpful-mode))
         (maybe-display-in-side-window my-display-at-bottom-sidebar-narrow)
         (dedicated . t)
         (side . right)
         (slot . 0)
         (window-width . ,my-side-window-size)
         (body-function . select-window))
        ;; popper catch-all: any remaining popup goes to the bottom
        (my-popper-popup-matcher
         (display-buffer-at-bottom)
         (dedicated . t)
         (window . root)
         (window-height . 20))
        ))

;; Try reusing windows
(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-reuse-mode-window)
        (reusable-frames . 'nil)))  ; nil means consider only the selected frame

;; Manually switching buffer must still respect window config constraints. For example
;; switching to a buffer that already has a dedicated window will switch to that window
;; instead of opening another copy of the buffer in the current window.
(setq switch-to-buffer-obey-display-actions t)

;; Pop new window if trying to switch buffer in a dedicated window
(setq switch-to-buffer-in-dedicated-window 'pop)

;; Rebalance windows when splitting
(setq window-combination-resize t)

;; maximum number of side windows per side (left, top, right, bottom)
(setq window-sides-slots '(1 0 1 2))

;; prefer splitting windows horizontally whenever possible
(defun split-window-sensibly-prefer-horizontal (&optional window)
"Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (interactive)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
         ;; Split window horizontally
         (with-selected-window window
           (split-window-right)))
    (and (window-splittable-p window)
         ;; Split window vertically
         (with-selected-window window
           (split-window-below)))
    (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
     (not (window-minibuffer-p window))
     (let ((split-width-threshold 0))
       (when (window-splittable-p window t)
         (with-selected-window window
               (split-window-right))))))))

(defun split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

(setq
   split-height-threshold 4
   split-width-threshold 40
   split-window-preferred-function 'split-window-really-sensibly)


;; quickly move/split/swap/copy windows
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :bind (("M-s" . ace-window))
  :config
  (setq
   ;; keys for selecting windows
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   ;; jump only in the current frame
   aw-scope 'frame
   ;; don't gray out contents when jumping
   aw-background nil
   ))

;; tab-based workspaces: one tab per project or ad-hoc context
;; (built-in tab-bar)
(use-package tab-bar
  :straight (:type built-in)
  :bind (;; switch workspaces (shadows the low-value `scroll-left'/
         ;; `scroll-right' defaults on C-<next>/C-<prior>)
         ("C-<prior>"   . tab-bar-switch-to-prev-tab)
         ("C-<next>"    . tab-bar-switch-to-next-tab)
         ;; reorder the current workspace
         ("C-S-<prior>" . tab-bar-move-tab-backward)
         ("C-S-<next>"  . tab-bar-move-tab))
  :custom
  ;; always show the workspace tabs, even when only one exists
  (tab-bar-show t)
  ;; new tabs start on scratch instead of inheriting the current buffer
  ;; (keeps previous buffers out of the new workspace's buffer list)
  (tab-bar-new-tab-choice "*scratch*")
  ;; number the tabs to match the C-M-<N> jumps below
  (tab-bar-tab-hints t)
  ;; jump straight to workspace N with C-M-1..C-M-9 (9 = last tab,
  ;; 0 = most recent tab).  This shadows `digit-argument' on
  ;; C-M-<digit>; M-<digit> and C-u still provide numeric arguments.
  (tab-bar-select-tab-modifiers '(control meta))
  :init
  (tab-bar-mode))

;; isolated project workspaces on top of tab-bar
;; https://codeberg.org/mclear-tools/tabspaces
(use-package tabspaces
  :straight (:host codeberg :repo "mclear-tools/tabspaces")
  :hook ((after-init . tabspaces-mode)
         ;; route file visits to their owning workspace (see the
         ;; auto-routing section below for the machinery)
         (find-file . my-workspaces-switch-for-file))
  :bind (("C-x C-b" . tabspaces-switch-to-buffer)
         (:map tabspaces-command-map
               ("C-z"     . tabspaces-switch-or-create-workspace)
               ("z"       . tab-bar-switch-to-recent-tab)
               ("n"       . tab-bar-switch-to-next-tab)
               ("p"       . tab-bar-switch-to-prev-tab)
               ("<right>" . tab-bar-switch-to-next-tab)
               ("<left>"  . tab-bar-switch-to-prev-tab)
               ("N"       . tabspaces-rename-workspace)))
  :custom
  ;; workspace command prefix
  (tabspaces-keymap-prefix "C-z")
  ;; route `project-switch-project' (C-x p p) through workspaces
  (tabspaces-project-switch-opens-workspace t)
  ;; skip the action dispatch when opening a project: a symbol value is
  ;; invoked immediately by `project-switch-project'
  (tabspaces-project-switch-commands #'my-project-open-magit-or-dired)
  ;; Resolve ".", "..", when opening projects
  (tabspaces-fully-resolve-paths t)
  ;; don't drop a todo file into newly created projects
  (tabspaces-initialize-project-with-todo nil)
  ;; sessions: the global session (non-project tabs) is saved on exit
  ;; and restored on startup (daemon-aware: on the first client frame);
  ;; project tabs are saved to per-project files and restored when the
  ;; project is opened
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tabspaces-session-file
   (no-littering-expand-var-file-name "tabspaces/session.el"))
  ;; keep per-project session files out of the project directories
  (tabspaces-session-project-session-store
   (no-littering-expand-var-file-name "tabspaces/projects/"))
  :config
  (make-directory tabspaces-session-project-session-store t)
  ;; back up the previous session file before the exit-time save
  ;; overwrites it; runs before tabspaces' own save (hook depth -50)
  (defun my-tabspaces-session-backup ()
    "Back up `tabspaces-session-file' as FILE.bak before it is overwritten."
    (when (file-exists-p tabspaces-session-file)
      (copy-file tabspaces-session-file
                 (concat tabspaces-session-file ".bak") t)))
  (add-hook 'kill-emacs-hook #'my-tabspaces-session-backup -50))

;;;; workspace auto-routing ;;;;

;; Visiting a file, dired or magit status switches to the workspace it
;; belongs to:
;; - buffers in a project create or open that project's tabspace
;; - otherwise open in current tabspace
;; - `C-u C-z o' force-creates an extra workspace for a project

(defvar my-workspaces-auto-switch nil
  "Non-nil once file-driven workspace switching is armed.
See `my-workspaces-arm': arming waits for the startup session restore.")

(defun my-workspaces-project-for-tab (tab-name)
  "Return project root mapped to TAB-NAME, nil if not a project tab.
Numbered duplicate tabs like \"proj<2>\" count as their base project."
  (or (car (rassoc tab-name tabspaces-project-tab-map))
      (when (string-match "\\`\\(.+\\)<[0-9]+>\\'" tab-name)
        (car (rassoc (match-string 1 tab-name)
                     tabspaces-project-tab-map)))))

(defun my-workspaces-tab-for-project (root)
  "Return the workspace tab mapped to project ROOT, or nil."
  (cdr (assoc root tabspaces-project-tab-map)))

(defun my-workspaces-root-for-directory (dir)
  "Return the project root containing DIR, or nil."
  (when-let* ((project (project-current nil dir)))
    (expand-file-name (project-root project))))

(defun my-workspaces-switch-for-root (root)
  "Switch to the workspace owning project ROOT, creating it if needed.
No-op when the current tab already belongs to ROOT (making \"proj<2>\"
duplicates sticky)."
  ;; `tabspaces--current-tab-name' and `tabspaces--list-tabspaces' are
  ;; documented by tabspaces as its stable integration API, despite the
  ;; `--' prefix.  The `save-current-buffer' preserves `current-buffer'
  ;; across the tab switch: `find-file-noselect-1' returns the current
  ;; buffer after running `find-file-hook', and `after-find-file' (which
  ;; runs this hook) would otherwise keep operating in the new tab's
  ;; buffer.
  (save-current-buffer
    (let ((current (tabspaces--current-tab-name)))
      (unless (equal (my-workspaces-project-for-tab current) root)
        (if-let* ((tab (my-workspaces-tab-for-project root)))
            (tab-bar-switch-to-tab tab)
          (let ((name (tabspaces-generate-descriptive-tab-name
                       root (tabspaces--list-tabspaces))))
            (tabspaces-switch-or-create-workspace name)
            ;; the generator registers the mapping only on the
            ;; conflict-free path; make sure we recorded it
            (unless (assoc root tabspaces-project-tab-map)
              (push (cons root name) tabspaces-project-tab-map))))))))

(defun my-workspaces-switch-for-file ()
  "Switch to the workspace matching the visited file, if needed.

Files inside a project switch to its workspace unless the current tab
already belongs to it (making \"proj<2>\" duplicates sticky); anything else
stays in the current workspace."
  (when (and my-workspaces-auto-switch
             (bound-and-true-p tabspaces-mode)
             (buffer-file-name)
             (not (active-minibuffer-window)))
    (when-let* ((root (my-workspaces-root-for-directory
                       (file-name-directory (buffer-file-name)))))
      (my-workspaces-switch-for-root root))))

(defun my-workspaces-arm ()
  "Arm file-driven workspace switching."
  (setq my-workspaces-auto-switch t)
  (remove-hook 'server-after-make-frame-hook #'my-workspaces-arm))

;; arm only after the startup session state has been restored: with a
;; daemon the restore happens on the first client frame (tabspaces'
;; restore lands on the same hook later, hence in front of ours, and
;; runs first); otherwise it has run before `emacs-startup-hook' fires.
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my-workspaces-arm)
  (add-hook 'emacs-startup-hook #'my-workspaces-arm))

;; Find file's existing-buffer branch never runs `find-file-hook', so
;; opening an already-visited file of another project would land in the
;; current tab.  This advice routes interactive `find-file' calls to
;; the tab that owns the buffer instead; programmatic calls are left
;; alone (e.g. xref keeps default display behavior).
(defun my-workspaces-tab-owning-buffer (buffer)
  "Return name of the workspace tab uniquely owning BUFFER, or nil.
Buffers contained in more than one tab's list (e.g. those injected
into every tab via `tabspaces-include-buffers', or deliberately
displayed in two workspaces) are not uniquely owned and yield nil --
routing such a buffer to an \"owning\" tab would be wrong: innocent
`switch-to-buffer' calls on shared buffers such as *scratch* would
jump tabs, e.g. during tab creation inside `tab-bar-new-tab-to'."
  (cl-loop with tabs = (tabspaces--list-tabspaces)
           for tab in tabs
           for i from 0
           when (memq buffer (tabspaces--buffer-list nil i))
           collect tab into owned
           finally return (and (= (length owned) 1) (car owned))))

(defun my-workspaces-goto-owning-tab (buffer)
  "Switch to the first workspace tab owning BUFFER, if it is elsewhere."
  (when-let* ((tab (my-workspaces-tab-owning-buffer buffer))
              ((not (equal tab (tabspaces--current-tab-name)))))
    (tab-bar-switch-to-tab tab)))

(defun my-find-file-route-to-owning-workspace (filename &optional _wildcards)
  "Switch to the workspace tab owning FILENAME's buffer before visiting it.
Installed as :before advice on `find-file'; acts only on interactive
calls, coordinating with `my-workspaces-switch-for-file' (which covers
first-time opens through `find-file-hook')."
  (when (and (called-interactively-p 'interactive)
             (bound-and-true-p tabspaces-mode)
             my-workspaces-auto-switch)
    (let ((buffer (find-buffer-visiting
                   (abbreviate-file-name (expand-file-name filename)))))
      (when buffer
        (my-workspaces-goto-owning-tab buffer)))))

(advice-add #'find-file :before #'my-find-file-route-to-owning-workspace)

;; Buffer switching (consult-buffer, ibuffer, plain C-x b): if the
;; target buffer belongs to another workspace tab, jump there first
;; instead of displaying it in the current tab.  The minibuffer guard
;; keeps consult's previews local: they cycle buffers through
;; `switch-to-buffer' under an active minibuffer, while real selections
;; run after the minibuffer has exited.
(defun my-switch-to-buffer-route-to-owning-workspace (buffer-or-name
                                                      &rest _)
  "Switch to the workspace tab owning BUFFER-OR-NAME before displaying it.
Installed as :before advice on `switch-to-buffer'."
  (when (and (bound-and-true-p tabspaces-mode)
             my-workspaces-auto-switch
             (not (active-minibuffer-window)))
    (when-let* ((buffer (get-buffer buffer-or-name)))
      (my-workspaces-goto-owning-tab buffer))))

(advice-add #'switch-to-buffer
            :before #'my-switch-to-buffer-route-to-owning-workspace)

;; Dired and magit status join the routing: directories never run
;; `find-file-hook' (`find-file-noselect' delegates them to
;; `find-directory-functions'), so both need their own seams.

(defun my-dired-route-to-owning-workspace (dir-or-list &optional _switches)
  "Switch to the workspace of the directory dired is about to visit.
Installed as :before advice on `dired-noselect': the single choke point
of all dired entry points (`dired', `dired-jump', C-x C-f on a
directory, ...), covering both new and existing dired buffers (existing
ones skip `dired-mode-hook', and `dired' displays via
`pop-to-buffer-same-window', bypassing the `switch-to-buffer' advice).
Routing before buffer setup makes the caller's display land in the
owning workspace."
  (when (and my-workspaces-auto-switch
             (bound-and-true-p tabspaces-mode)
             (not (active-minibuffer-window)))
    (when-let* ((root (my-workspaces-root-for-directory
                       (if (consp dir-or-list)
                           (car dir-or-list)
                         (or dir-or-list default-directory)))))
      (my-workspaces-switch-for-root root))))

(defun my-magit-status-route-to-owning-workspace ()
  "Switch to the workspace owning the repository of this status buffer.
Runs on `magit-status-mode-hook': magit calls the mode function before
`magit-display-buffer', for both new and existing status buffers, with
`default-directory' already at the repository top-level -- so the
subsequent display lands in the owning workspace."
  (when (and my-workspaces-auto-switch
             (bound-and-true-p tabspaces-mode)
             (not (active-minibuffer-window)))
    (when-let* ((root (my-workspaces-root-for-directory default-directory)))
      (my-workspaces-switch-for-root root))))

(defun my-tabspaces-project-action ()
  "Open the project at `default-directory' in its own workspace.
Used as `projectile-switch-project-action', so `default-directory' is
the project root.  New projects get a fresh tab and land on magit (git
projects) or dired via `my-project-open-magit-or-dired'; known projects
just switch tabs."
  ;; `tabspaces-open-or-create-project-and-workspace' reads
  ;; `project--list' without ensuring it was read from
  ;; `project-list-file'; only its interactive spec does that.  Force
  ;; the read through the public API first (upstream fix candidate).
  (project-known-project-roots)
  (tabspaces-open-or-create-project-and-workspace default-directory))

(defun my-project-open-magit-or-dired ()
  "Open the project being switched to: magit if git-controlled, dired otherwise.
Installed as `tabspaces-project-switch-commands': a symbol value makes
`project-switch-project' invoke this command immediately instead of
showing the `project-switch-commands' dispatch menu.  Like the stock
project commands, this finds the target through `project-current',
which honors `project-current-directory-override'."
  (interactive)
  (let ((root (project-root (project-current t))))
    (if (locate-dominating-file root ".git")
        (magit-status root)
      (dired root))))

;; tame the flood of ephemeral windows Emacs produces
;; https://github.com/karthink/popper
(use-package popper
  ;; popper-mode classifies existing buffers at enable time using popper-group-function,
  ;; needs project.el loaded (tabspaces requires project)
  :after tabspaces
  :preface
  (defun my-popper-group-by-workspace ()
    "Group popups by the current tabspaces workspace (tab name).
Falls back to project.el grouping when workspaces are off."
    (or (and (bound-and-true-p tabspaces-mode)
             (fboundp 'tabspaces--current-tab-name)
             (tabspaces--current-tab-name))
        (popper-group-by-project)))
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("M-~" . popper-cycle-backwards)
         ("C-M-`" . popper-kill-latest-popup))
  :init
  (popper-mode +1)
  (popper-echo-mode +1)
  :custom
  ;; have popper respect display-buffer-alist rules
  (popper-display-control nil)
  ;; enable actions in echo area (k to kill buffer)
  (popper-echo-dispatch-actions t)
  ;; how to group popups
  (popper-group-function #'my-popper-group-by-workspace)
  ;; which buffers should be considered popups
  (popper-reference-buffers
   '("\\*Messages\\*"
     "^\\*Warnings\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*Compile-Log\\*"
     "\\*eldoc.*\\*"
     "^\\*eshell.*\\*$" eshell-mode
     "^\\*ghostel.*\\*$" ghostel-mode
     "^\\*ielm.*\\*$"
     "^\\*shell.*\\*$"  shell-mode
     "^\\*term.*\\*$"   term-mode
     "^\\*vterm.*\\*$"  vterm-mode
     "\\*Embark Collect.*\\*"
     "\\*Embark Export.*\\*"
     "\\*Flymake diagnostics.*\\*"
     "\\*Shell Command Output\\*"
     "\\*envrc\\*"
     ;; include derived modes, e.g. inferior-python-mode extends comint-mode
     (lambda (buf) (with-current-buffer buf
                     (derived-mode-p 'compilation-mode
                                     'comint-mode
                                     'help-mode
                                     'helpful-mode
                                     )))
     )))

;; zoom and unzoom windows like tmux
;; https://github.com/emacsorphanage/zoom-window
(use-package zoom-window
  :after popper
  :preface
  (defun my-popper-unzoom-before (&rest _)
    "Unzoom before popper closes or kills a zoomed popup window.
Popper dismisses popups with `quit-window', which cannot delete a
sole zoomed window and would leave the frame showing an unrelated
buffer instead of restoring the previous window layout."
    (when (frame-parameter nil 'zoom-window-enabled)
      (zoom-window-zoom)))
  :bind (("<f12>" . zoom-window-zoom))
  :config
  (advice-add 'popper-close-latest :before #'my-popper-unzoom-before)
  (advice-add 'popper-kill-latest-popup :before #'my-popper-unzoom-before))

(use-package winner
  :straight nil
  :preface
  (defun toggle-delete-other-windows ()
    "Delete other windows in frame if any, or restore previous window config."
    (interactive)
    (if (and winner-mode
             (equal (selected-window) (next-window)))
        (winner-undo)
      (delete-other-windows)))
  :bind ("C-x 1" . toggle-delete-other-windows)
  :config
  (winner-mode 1)
)

;;;;;;;;;;;;;
;;; Shell ;;;
;;;;;;;;;;;;;

;; use libghostty as terminal emulator
;; https://github.com/dakra/ghostel
(use-package ghostel
  :custom
  ;; let F12 reach Emacs (it toggles a popup's full-frame zoom);
  ;; char mode still sends all keys to the terminal
  (ghostel-keymap-exceptions '("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\\"
                               ;; allow popper use
                               "C-`" "M-`"
                               ;; tab switching
                               "C-<next>" "C-<prior>"
                               ;; zoom-window
                               "<f12>")))

;; make eshell-visual-commands run in a Ghostel buffer.
(use-package ghostel-eshell
  :straight nil  ;; bundled in ghostel
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

;; run all compile commands in a Ghostel buffer.
(use-package ghostel-compile
  :straight nil  ;; bundled in ghostel
  :hook (after-init . ghostel-compile-global-mode))

;; replace comint's built-in ansi-color-process-output with Ghostel's VT parser.
(use-package ghostel-comint
  :straight nil  ;; bundled in ghostel
  :hook (after-init . ghostel-comint-global-mode))

;; .zsh file is shell script too
(use-package sh-script
  :straight nil  ;; Emacs built-in
  :mode ("\\.zsh" . shell-script-mode))

;; major mode for fish shell script (https://fishshell.com/)
;; https://github.com/wwwjfy/emacs-fish
(use-package fish-mode
  :mode "\\.fish")

;;;;;;;;;;;;;
;;; dired ;;;
;;;;;;;;;;;;;

(use-package dired
  :straight nil
  :after all-the-icons-dired
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("^"   . dired-up-directory))
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-agho --group-directories-first")
  ;; always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (dired-dwim-target t)
  :config
  ;; route dired buffers to their project workspace; in :config because
  ;; `dired-noselect' is autoloaded and advice-add would load it eagerly
  (advice-add #'dired-noselect
              :before #'my-dired-route-to-owning-workspace))

(use-package all-the-icons-dired
  :after all-the-icons)

;;;;;;;;;;;;;;;
;;; ibuffer ;;;
;;;;;;;;;;;;;;;

(use-package ibuffer
  :straight nil  ;; Emacs built-in
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-projectile
  :hook (ibuffer . (lambda ()
                     (ibuffer-projectile-set-filter-groups)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))

(use-package all-the-icons-ibuffer
  :after (all-the-icons ibuffer)
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;;;;;;;;;;;;;;;
;;; compile ;;;
;;;;;;;;;;;;;;;

(use-package compile
  :straight nil  ;; Emacs built-in
  :config
  (setq
   ;; save before compiling
   compilation-ask-about-save nil
   ;; kill old compile processes before starting the new one
   compilation-always-kill t
   ;; automatically scroll to first error
   compilation-scroll-output 'first-error
   ))

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(use-package ansi-color
  :straight nil  ;; Emacs built-in
  :hook
  (compilation-filter . colorize-compilation-buffer)
  :preface
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; performant and minimalistic vertical completion UI
;; https://github.com/minad/vertico
(use-package vertico
  :bind (:map vertico-map
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group))
  :preface
  ;; M-n in vertico minibuffer copies the thing under point from the buffer
  ;; https://github.com/minad/vertico/issues/22#issuecomment-826663342
  (defun my-minibuffer-default-add-function ()
    (with-selected-window (minibuffer-selected-window)
      (delete-dups
       (delq nil
             (list (thing-at-point 'symbol)
                   (thing-at-point 'list)
                   (ffap-guesser)
                   (thing-at-point-url-at-point))))))
  :init
  (setq minibuffer-default-add-function 'my-minibuffer-default-add-function)
  (vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  (vertico-sort-function 'vertico-sort-history-alpha)
  )

;; better directory navigation in vertico
;; https://github.com/minad/vertico/blob/main/extensions/vertico-directory.el
(use-package vertico-directory
  :after vertico
  :straight nil  ;; bundled with vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; practical commands based on core function completing-read
;; https://github.com/minad/consult
(use-package consult
  :after projectile
  :bind (("C-c f" . consult-fd)
         ("C-c H" . my-consult-fd-home)
         ("C-c R" . my-consult-fd-root)
         ("C-c j" . consult-outline)
         ("M-i" . consult-imenu)
         ("C-c i" . consult-imenu)
         ("C-c g" . consult-ripgrep)
         ("C-c G" . my-consult-ripgrep-at-point)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-s" . consult-line)
         ("C-S-s" . my-consult-line-at-point)
         ("C-c s" . my-consult-line-at-point)
         ("C-c M-s" . consult-line-multi)
         ("C-c M-x" . consult-mode-command)
         ("C-c C-m" . consult-minor-mode-menu)
         ("C-h C-m" . consult-man)
         ("C-c ! j" . consult-flymake)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ;; originally mark-word, I use expreg
         ("M-@" . consult-register-store)
         ("C-M-#" . consult-register)
         ([remap copy-to-register] . consult-register-store)
         ([remap insert-register] . consult-register-load)

         ([remap goto-line] . consult-goto-line)
         ([remap Info-search] . consult-info)
         ([remap projectile-switch-to-buffer] . consult-project-buffer)
         ([remap repeat-complex-command] . consult-complex-command)
         :map consult-narrow-map
         ([C-right] .  consult-narrow-right)
         ([C-left] .  consult-narrow-left)
         :map minibuffer-local-map
         ;; originally next-matching-history-element
         ("M-s" . consult-history)
         ;; originally previous-matching-history-element
         ("M-r" . consult-history)
         )
  :preface
  (defun get-project-root ()
    (if (fboundp 'projectile-project-root)
        (projectile-project-root)
      (vc-root-dir)))
  (defun my-consult-fd-home ()
    (interactive)
    (consult-fd "~"))
  (defun my-consult-fd-root ()
    (interactive)
    (consult-fd "/"))
  (defun my-consult-line-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun my-consult-ripgrep-at-point ()
    (interactive)
    (consult-ripgrep (get-project-root) (thing-at-point 'symbol)))
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (orderless-compile input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))
  (defun consult-narrow-left ()
    (interactive)
    (when consult--narrow-keys
      (consult-narrow
       (if consult--narrow
           (let ((idx (seq-position consult--narrow-keys
                                    (assq consult--narrow consult--narrow-keys))))
             (unless (eq idx 0)
               (car (nth (1- idx) consult--narrow-keys))))
         (caar (last consult--narrow-keys))))))
  (defun consult-narrow-right ()
    (interactive)
    (when consult--narrow-keys
      (consult-narrow
       (if consult--narrow
           (let ((idx (seq-position consult--narrow-keys
                                    (assq consult--narrow consult--narrow-keys))))
             (unless (eq idx (1- (length consult--narrow-keys)))
               (car (nth (1+ idx) consult--narrow-keys))))
         (caar consult--narrow-keys)))))
  ;; consult-buffer source limited to buffers of the current workspace,
  ;; added to `consult-buffer-sources' in :config.  Uses
  ;; `consult--buffer-state'/`consult--buffer-query' and
  ;; `tabspaces--local-buffer-p': the upstream-documented pattern for
  ;; this integration despite the `--' prefixes.
  (defvar my-consult-source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "consult-buffer source listing buffers of the current workspace.")
  :custom
  (consult-async-min-input 2)
  (consult-narrow-key ",")
  ;; search hidden files and dirs, e.g. ~/.config
  (consult-fd-args '((if
                         (executable-find "fdfind" 'remote)
                         "fdfind" "fd")
                     "--full-path --color=never --hidden"))
  (consult-find-command "fd --hidden --color=never --full-path ARG OPTS")
  (consult--regexp-compiler consult--orderless-regexp-compiler)
  :config
  (consult-customize
   consult-source-hidden-buffer
   consult-source-buffer
   consult-source-recent-file
   consult-source-bookmark
   consult-source-project-buffer
   consult-source-project-recent-file
   :preview-key '"M-.")
  ;; filter buffers to the current workspace by default via the source
  ;; defined in :preface; narrow with "b" to see all buffers.
  ;; plist-put instead of consult-customize: that macro can
  ;; mis-validate in some build orders (consult#345, tabspaces#76).
  (plist-put consult-source-buffer :hidden t)
  (plist-put consult-source-buffer :default nil)
  (add-to-list 'consult-buffer-sources 'my-consult-source-workspace)
  (setq
   ;; use consult for xref navigation
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref
   ))

;; insert directory paths into the minibuffer prompt
;; https://github.com/karthink/consult-dir
(use-package consult-dir
  :after (consult projectile)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-c d" . consult-dir)
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :preface
  ;; Quick access to docker containers
  ;; Taken from consult-dir's README: https://github.com/karthink/consult-dir#docker-hosts
  (defcustom consult-dir--tramp-container-executable "podman"
    "Default executable to use for querying container hosts."
    :group 'consult-dir
    :type 'string)
  (defcustom consult-dir--tramp-container-args nil
    "Optional list of arguments to pass when querying container hosts."
    :group 'consult-dir
    :type '(repeat string))
  (defun consult-dir--tramp-container-hosts ()
    "Get a list of hosts from a container host."
    (cl-loop for line in (cdr
                          (ignore-errors
                            (apply #'process-lines consult-dir--tramp-container-executable
                                   (append consult-dir--tramp-container-args (list "ps")))))
             for cand = (split-string line "[[:space:]]+" t)
             collect (let ((user (unless (string-empty-p (car cand))
                                   (concat (car cand) "@")))
                           (hostname (car (last cand))))
                       (format "/docker:%s%s:/" user hostname))))
  (defvar consult-dir--source-tramp-docker
    `(:name     "Docker"
                :narrow   ?d
                :category file
                :face     consult-file
                :history  file-name-history
                :items    ,#'consult-dir--tramp-container-hosts)
    "Docker candidate source for `consult-dir'.")
  :config
  (setq
   ;; integrate with projectile to find project directories
   consult-dir-project-list-function #'consult-dir-projectile-dirs
   ;; default command to execute on candidates
   consult-dir-default-command 'find-file)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-docker t))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :bind (("C-c C-s" . consult-yasnippet)))

;; adds marginalia annotations to the minibuffer completions
;; https://github.com/minad/marginalia
(use-package marginalia
  :after (:any consult vertico)
  :config
  (marginalia-mode))

;; pretty icons in completion minibuffer
;; https://github.com/iyefrat/all-the-icons-completion
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; choose a command to run based on what is near point, both during a
;; minibuffer completion session and in normal buffers
;; https://github.com/oantolin/embark/
(use-package embark
  :after which-key
  :bind
  (("C-." . embark-act)
   ("C-M-." . embark-act-noquit)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-." . embark-act))
  :preface
  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))
  :config
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using completing-read."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; Store Org links to file and URL targets: push onto the public
  ;; `org-stored-links' (mirroring what `org-store-link' does; the
  ;; helper `org-link--add-to-stored-links' is private), so the next
  ;; `org-insert-link' offers them.
  (defvar org-stored-links)
  (defun my-embark-org-store-file-link (file)
    "Store an Org link to FILE in `org-stored-links'."
    (interactive "fStore Org link to file: ")
    (push (list (concat "file:" (expand-file-name file))
                (file-name-nondirectory file))
          org-stored-links)
    (message "Stored Org link to %s" file))
  (defun my-embark-org-store-url-link (url)
    "Store an Org link to URL in `org-stored-links'."
    (interactive "sStore Org link to URL: ")
    (push (list url nil) org-stored-links)
    (message "Stored Org link to %s" url))
  (keymap-set embark-file-map "l" #'my-embark-org-store-file-link)
  (keymap-set embark-url-map "l" #'my-embark-org-store-url-link)

  ;; gptel actions on the region, under an "a" prefix matching the
  ;; global C-c a prefix.  Displaces `align' in `embark-region-map'
  ;; (`align-regexp' remains on "A").
  (defvar-keymap my-embark-gptel-map
    :doc "Keymap for Embark actions sending the region to gptel."
    "r" #'gptel-rewrite
    "a" #'gptel-add
    "s" #'gptel-send
    "R" 'my-gptel-review-malicious-code) ; quoted, defined later in init.el
  (fset 'my-embark-gptel-map my-embark-gptel-map)
  (keymap-set embark-region-map "a" 'my-embark-gptel-map)
  (keymap-set embark-file-map "a" #'gptel-add-file)

  ;; Ask gptel to explain the Flymake diagnostic at point.
  (defun my-embark-gptel-explain-diagnostic ()
    "Ask gptel to explain the Flymake diagnostic at point.
Stream the response to the *gptel-diagnostic* buffer."
    (interactive)
    (require 'gptel)
    (if-let* ((diag (car (flymake-diagnostics (point)))))
        (let* ((text (format
                      "Explain this diagnostic in %s and suggest a fix.\n\n\
Diagnostic (%s): %s\n\nCode:\n%s"
                      (buffer-name)
                      (flymake-diagnostic-type diag)
                      (flymake-diagnostic-text diag)
                      (buffer-substring-no-properties
                       (flymake-diagnostic-beg diag)
                       (flymake-diagnostic-end diag))))
               (buffer (get-buffer-create "*gptel-diagnostic*"))
               (marker (with-current-buffer buffer
                         (erase-buffer)
                         (org-mode)
                         (goto-char (point-min))
                         (point-marker))))
          (pop-to-buffer buffer)
          (gptel-request text
                         :system "You are a concise programming assistant.  \
Explain diagnostics briefly: what causes them and how to fix them."
                         :stream t
                         :buffer buffer
                         :position marker))
      (user-error "No Flymake diagnostic at point")))
  (keymap-set embark-flymake-map "a" #'my-embark-gptel-explain-diagnostic)

  ;; Magit, timemachine and terminal actions on file/directory targets.
  (defun my-embark-magit-status (dir)
    "Run `magit-status' for the repository containing DIR."
    (interactive "DMagit status: ")
    (magit-status dir))
  (defun my-embark-git-timemachine (file)
    "Visit FILE and browse its history with `git-timemachine'."
    (interactive "fFile: ")
    (find-file file)
    (git-timemachine))
  (defun my-embark-ghostel (dir)
    "Open a Ghostel terminal in DIR."
    (interactive "DTerminal in directory: ")
    (let ((default-directory (if (file-directory-p dir)
                                 (file-name-as-directory dir)
                               (file-name-directory (expand-file-name dir)))))
      (ghostel)))
  (keymap-set embark-file-map "g" #'my-embark-magit-status)
  (keymap-set embark-file-map "T" #'my-embark-git-timemachine)
  (keymap-set embark-file-map "t" #'my-embark-ghostel)

  ;; Magit commit target: the commit at point in any Magit buffer
  ;; (log, status, refs, revision), found with `magit-commit-at-point'.
  ;; Registered near the front of `embark-target-finders' so it beats
  ;; the generic identifier target on hashes (mirrors embark-org).
  (defun my-embark-target-magit-commit ()
    "Target the commit at point in a Magit buffer."
    (when (derived-mode-p 'magit-mode)
      (when-let* ((rev (magit-commit-at-point)))
        (if-let* ((bounds (bounds-of-thing-at-point 'git-revision)))
            `(magit-commit ,rev ,(car bounds) . ,(cdr bounds))
          (cons 'magit-commit rev)))))
  (defun my-embark-org-store-commit-link (rev)
    "Store an Org link to commit REV via orgit.
The link is pushed onto `org-stored-links' and offered by
`org-insert-link'."
    (interactive (list (or (magit-commit-at-point)
                           (magit-read-branch-or-commit
                            "Store Org link to commit"))))
    (unless (require 'orgit nil t)
      (user-error "Storing Org links to commits requires the orgit package"))
    (save-window-excursion
      ;; `magit-show-commit' selects the revision window (unless
      ;; `magit-display-buffer-noselect' is non-nil), so
      ;; `org-store-link' runs in the revision buffer and orgit
      ;; stores an orgit-rev link.
      (magit-show-commit rev)
      (org-store-link nil t)))
  (defvar-keymap my-embark-magit-commit-map
    :doc "Keymap for Embark actions on Magit commits."
    :parent embark-general-map
    "RET" #'magit-show-commit
    "l" #'my-embark-org-store-commit-link)
  (let ((tail (memq 'embark-target-active-region embark-target-finders)))
    (cl-pushnew 'my-embark-target-magit-commit (cdr tail)))
  (add-to-list 'embark-keymap-alist
               '(magit-commit my-embark-magit-commit-map))

  ;; jinx instead of ispell for word targets.
  (keymap-set embark-identifier-map "$" #'jinx-correct)

  ;; helpful supersedes describe-*; elisp-refs finds references.
  (keymap-set embark-symbol-map "h" #'helpful-symbol)
  (keymap-set embark-variable-map "h" #'helpful-variable)
  (keymap-set embark-function-map "h" #'helpful-callable)
  (keymap-set embark-command-map "h" #'helpful-command)
  (keymap-set embark-symbol-map "R" #'elisp-refs-symbol))

;; Embark-Consult integration (export/preview glue for consult commands)
;; https://github.com/oantolin/embark/blob/master/embark-consult.el
(use-package embark-consult
  :after (embark consult))

;; completion style that divides the pattern into space-separated components
;; and matches candidates that match all of the components in any order
;; https://github.com/oantolin/orderless
(use-package orderless
  :bind (:map minibuffer-local-map
              ("C-l" . my-match-components-literally))
  :preface
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?# . orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))
  ;; collate accents to unaccentuated letter
  (defvar my-orderless-accent-replacements
    '(("a" . "[aàáâãäå]")
      ("e" . "[eèéêë]")
      ("i" . "[iìíîï]")
      ("o" . "[oòóôõöœ]")
      ("u" . "[uùúûü]")
      ("c" . "[cç]")
      ("n" . "[nñ]")))
  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * #initialism initialism#
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun my-orderless-dispatch (pattern _index _total)
  (cond
   ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
   ((string-suffix-p "$" pattern)
    `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
   ;; File extensions
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." pattern))
    `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x200000-\x300000]*$")))
   ;; Ignore single !
   ((string= "!" pattern) `(orderless-literal . ""))
   ;; Explicit prefix/suffix style dispatchers
   ((if-let* ((x (assq (aref pattern 0) +orderless-dispatch-alist)))
        (cons (cdr x) (substring pattern 1))
      (when-let* ((x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist)))
        (cons (cdr x) (substring pattern 0 -1)))))
   ;; Default: accent-fold the pattern
   (t (let ((new-pattern (seq-reduce
                          (lambda (prev val)
                            (replace-regexp-in-string (car val) (cdr val) prev))
                          my-orderless-accent-replacements
                          pattern)))
        (unless (string= new-pattern pattern)
          (cons 'orderless-regexp new-pattern))))))
  (defun my-match-components-literally ()
    "Components match literally for the rest of the session."
    (interactive)
    (setq-local orderless-matching-styles '(orderless-literal)
                orderless-style-dispatchers nil))
  :config
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
                                   (eglot (styles orderless))
                                   ))
  (orderless-style-dispatchers '(my-orderless-dispatch))
  ;; allow escaping space with backslash
  (orderless-component-separator #'orderless-escapable-split-on-space))

;;;;;;;;;;;
;;; git ;;;
;;;;;;;;;;;

;; https://magit.vc/
(use-package magit
  :if (executable-find "git")
  :hook ((git-commit-setup . my-git-commit-setup)
         ;; route status buffers to their project workspace
         (magit-status-mode . my-magit-status-route-to-owning-workspace))
  :bind (("C-c v m" . magit-status)
         ("C-c v v" . magit-status)
         ("C-c v d" . magit-dispatch)
         ("C-c v f" . magit-file-dispatch)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v b" . magit-blame)
         ("C-c v U" . my-straight-incoming-diffs))
  :preface
  (defun my-git-commit-setup ()
    (setq-local fill-column 72)
    (git-commit-turn-on-auto-fill))
  ;; persisted across sessions by savehist
  (defvar my-git-commit-assisted-history nil
    "History of values entered for `my-git-commit-assisted'.")
  (defun my-git-commit-assisted ()
    "Insert an \"Assisted-By\" trailer crediting an AI coding agent.
Prompt for the agent description (free text, e.g. \"Claude Opus 4.7\"),
offering previously entered values as completion."
    (interactive)
    ;; NOTE: `git-commit--insert-trailer' is a private API; magit
    ;; offers no public function for inserting an arbitrary trailer.
    (let ((value (completing-read "Assisted by: "
                                  my-git-commit-assisted-history
                                  nil nil nil
                                  'my-git-commit-assisted-history)))
      (when (string-empty-p value)
        (user-error "Empty agent description"))
      (git-commit--insert-trailer "Assisted-by" value)))
  (defun my-straight-repo-behind-p (repo)
    "Return non-nil if REPO's current branch is behind its upstream."
    (let* ((default-directory repo)
           (lines (magit-git-lines "status" "--porcelain=2" "--branch")))
      (seq-some (lambda (line)
                  (string-match-p "^# branch\\.ab [+][0-9]+ -[1-9]" line))
                lines)))
  (defun my-straight-incoming-diffs ()
    "Concatenate the full patches of all incoming upstream changes.
Create a `diff-mode' buffer listing, for every straight.el checkout
that is behind its upstream, the incoming commits with author and
date, followed by the net patch that merging would apply.  The
buffer is left writable so hunks can be trimmed before feeding it
to a reviewing agent."
    (interactive)
    (let* ((repos (magit-list-repos-1
                   (expand-file-name "straight/repos" user-emacs-directory)
                   1))
           (behind (seq-filter #'my-straight-repo-behind-p repos)))
      (if (null behind)
          (message "No incoming upstream changes")
        (with-current-buffer (get-buffer-create "*straight-incoming-diffs*")
          (erase-buffer)
          (dolist (repo behind)
            (let* ((default-directory repo)
                   (branch (magit-git-string "rev-parse" "--abbrev-ref"
                                             "HEAD"))
                   (upstream (magit-git-string "rev-parse" "--abbrev-ref"
                                               "@{upstream}"))
                   (commits (magit-git-lines
                             "log" "--format=%h  %ad  %an <%ae>  %s"
                             "--date=short" "HEAD..@{upstream}"))
                   (n (length commits)))
              (insert (make-string 80 ?=) "\n")
              (insert
               (format "%s  (%s <- %s, %d commit%s)\n"
                       (file-name-nondirectory (directory-file-name repo))
                       (or branch "HEAD") (or upstream "?")
                       n (if (= n 1) "" "s")))
              (insert (make-string 80 ?-) "\n")
              (insert (string-join commits "\n") "\n\n")
              (magit-git-insert "diff" "HEAD...@{upstream}")
              (insert "\n")))
          (diff-mode)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer))))))
  (defun my-straight-incoming-diffs-after-fetch (&rest _)
    "Show incoming full diffs after an interactive `straight-fetch-all'."
    (when (eq this-command 'straight-fetch-all)
      (condition-case err
          (my-straight-incoming-diffs)
        (error (message "my-straight-incoming-diffs failed: %S"
                        (error-message-string err))))))
  :init
  ;; automatically show the incoming diffs after fetching all remotes
  (with-eval-after-load 'straight
    (advice-add 'straight-fetch-all
                :after #'my-straight-incoming-diffs-after-fetch))
  :custom
  ;; I set up my own keybindings
  (magit-define-global-key-bindings nil)
  ;;
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  ;; Visual warning if commit first line gets too long
  (git-commit-summary-max-length 60)
  ;; path to my root code dir, so I can do C-x g from anywhere
  (magit-repository-directories `(("~/Code" . 2)
                                  ("~/Sync/Research" . 1)
                                  ;; straight.el package checkouts, to review
                                  ;; package updates and hack on packages
                                  (,(expand-file-name "straight/repos"
                                                      user-emacs-directory)
                                   . 1)))
  ;; create a local tracking branch when visiting a remote branch
  (magit-visit-ref-create t)
  ;; don't ask for confirmation when pushing branches
  (magit-push-always-verify nil)
  ;; put history.el in the custom savefile dir
  (transient-history-file (no-littering-expand-var-file-name "transient-history.el"))
  ;; do not ask confirmation for actions easily reverted when wip-mode is enabled
  (magit-no-confirm '(set-and-push safe-with-wip))
  :config
  ;; save work-in-progress before potentially dangerous operations
  ;; https://magit.vc/manual/magit.html#Wip-Modes
  (magit-wip-mode +1)
  ;; do not display diff in commits by default, show with C-c C-d when necessary
  ;; https://magit.vc/manual/magit.html#Committing-Performance
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  ;; C-c TAB A inserts an "Assisted-By" trailer crediting an AI agent
  (transient-append-suffix 'git-commit-insert-trailer 'git-commit-co-developed
                           '("A" "Assisted" my-git-commit-assisted))
  ;; highlight the trailer token
  (add-to-list 'git-commit-trailers "Assisted-by")
  ;; yadm (dotfiles manager) is a special git repository, see tramp for /yadm::
  (defun yadm ()
    "Open yadm's magit status in its own workspace tab."
    (interactive)
    (tabspaces-switch-or-create-workspace "yadm")
    (magit-status "/yadm::"))
  )

;; https://github.com/dandavison/magit-delta
(use-package magit-delta
  :if (executable-find "delta")
  :after (auto-dark magit)
  :hook ((magit-mode . magit-delta-mode)
         ((auto-dark-dark-mode auto-dark-light-mode)
          . my-magit-delta-sync-appearance))
  :preface
  (defun my-magit-refresh-all-visible ()
    "Refresh Magit buffers visible in any window on any frame."
    (walk-windows
     (lambda (win)
       (with-current-buffer (window-buffer win)
         (when (derived-mode-p 'magit-mode)
           (magit-refresh))))
     'no-minibuf t))
  (defun my-magit-delta-sync-appearance ()
    "Sync delta's --light/--dark flag with the current appearance.
Reads `frame-background-mode', which auto-dark sets on every switch
before running its hooks, then refreshes visible Magit buffers.  No-op
until magit-delta is loaded; the `:config' call covers theme switches
that happened before the auto-dark hooks were registered."
    (when (boundp 'magit-delta-delta-args)
      (dolist (item '("--dark" "--light"))
        (setq magit-delta-delta-args (delete item magit-delta-delta-args)))
      (add-to-list 'magit-delta-delta-args
                   (if (eq frame-background-mode 'dark) "--dark" "--light") t)
      (my-magit-refresh-all-visible)))
  :config
  (add-to-list 'magit-delta-delta-args "--no-gitconfig")
  (my-magit-delta-sync-appearance)
  )

;; https://magit.vc/manual/forge/
(use-package forge
  :after magit
  :config
  (setq forge-database-file
        (no-littering-expand-var-file-name "forge-database.sqlite"))
  )

;; browse previous revisions of any git-controlled file
;; https://codeberg.org/pidu/git-timemachine
(use-package git-timemachine
  :bind (("C-c v t" . git-timemachine)))

(use-package consult-git-log-grep
  :after magit
  :if (executable-find "git")
  :bind (("C-c L" . consult-git-log-grep))
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

;;;;;;;;;;;;;;;;
;;; projects ;;;
;;;;;;;;;;;;;;;;

(use-package project
  :straight (:type built-in)  ;; Emacs built-in
  :config
  (setq project-list-file (no-littering-expand-var-file-name "projects"))
  (add-to-list 'project-switch-commands '(magit-status "Magit" "g") t)
  (add-to-list 'project-switch-commands '(ghostel-project "Term" "t") t))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :bind-keymap (("C-c p" . projectile-command-map)
                ("s-p" . projectile-command-map))
  :init
  (setq projectile-project-search-path `(("~/Code/" . 2)
                                         "~/Sync/Research/"
                                         ,(expand-file-name "straight/repos" user-emacs-directory)))
  :custom
  (projectile-dynamic-mode-line nil)
  (projectile-shell-backend 'ghostel)
  :config
  (setq projectile-cache-file (no-littering-expand-var-file-name  "projectile.cache")
        projectile-known-projects-file (no-littering-expand-var-file-name "projectile-bookmarks.eld")
        ;; switch to the project's workspace when switching projects;
        ;; a new project's tab lands on magit (git projects) or dired
        ;; via `my-project-open-magit-or-dired'; an existing project
        ;; tab is just switched to
        projectile-switch-project-action #'my-tabspaces-project-action
        ;; https://docs.projectile.mx/projectile/configuration.html#project-specific-compilation-buffers
        projectile-per-project-compilation-buffer t
        )
  (projectile-mode t))

;;;;;;;;;;;;;;;;;;;;
;;; autocomplete ;;;
;;;;;;;;;;;;;;;;;;;;

;; inline preview of the completion-at-point suggestion
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/completion-preview.el
(use-package completion-preview
  :straight nil ;; built-in
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate))
  :hook (text-mode . completion-preview-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

;; enhance completion at point with a small completion popup
;; https://github.com/minad/corfu
(use-package corfu
  :hook ((minibuffer-setup . corfu-enable-in-minibuffer)
         ;; use corfu in programming modes (non-programming use completion-preview)
         (prog-mode . corfu-mode))
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)
              ("C-g" . corfu-quit)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<escape>" . corfu-quit)
              ("<return>" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location)
              ("M-m" . corfu-move-to-minibuffer)
              ;; manual toggle for the documentation popup
              ([remap corfu-show-documentation] . corfu-popupinfo-toggle)
              ;; scroll in the documentation window
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down))
  :preface
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  :config
  (corfu-indexed-mode)
  (corfu-popupinfo-mode)
  :custom
  ;; autocompletion only pops up automatically in programming modes
  (corfu-auto t)
  ;; show candidates as soon as 1 character is pressed
  (corfu-auto-prefix 1)
  ;; show candidates after this many seconds
  (corfu-auto-delay 0.2)
  (corfu-min-width 20)
  ;; always have the same width
  (corfu-max-width 80)
  (corfu-count 20)
  (corfu-scroll-margin 3)
  (corfu-cycle t)
  (corfu-quit-at-boundary 'separator)
  ;; don't quit if there is corfu-separator inserted
  (corfu-quit-no-match 'separator)
  (corfu-separator ?\s)
  (corfu-preselect-first t)
  ;; preview current candidate
  (corfu-preview-current 'insert)
  ;; don't show documentation in echo area, as corfu-doc is set up below
  (corfu-echo-documentation nil)
  )

;; icons for autocomplete results
;; https://github.com/jdtsmith/kind-icon
(use-package kind-icon
  :after corfu
  :hook ((auto-dark-dark-mode auto-dark-light-mode) . (lambda () (interactive)
                                                        (kind-icon-reset-cache)))
  :custom
  ;; explicitly enable icons
  (kind-icon-use-icons t)
  ;; have background color be the same as corfu face background
  (kind-icon-default-face 'corfu-default)
  ;; use midpoint color between foreground and background colors
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  ;; don't allow svg-lib to litter with its cache directory
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/"))
  :config
  ;; enable for corfu
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; use dabbrev with corfu
(use-package dabbrev
  :straight nil  ;; Emacs built-in
  ;; swap M-/ and C-M-/, as M-/ will use corfu
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;;;;;;;;;;;;;;;;;;
;;; spell check ;;;
;;;;;;;;;;;;;;;;;;;

(use-package jinx
  :hook (text-mode . jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;;;;;;;;;;;;;;;;;;
;;; keybindings ;;;
;;;;;;;;;;;;;;;;;;;

;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :straight nil  ;; Emacs built-in
  :config (windmove-default-keybindings))

;; define a new minor mode
;; https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings/358#358
(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

(define-minor-mode my-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " my-mode"
  :keymap 'my-mode-map)

(define-globalized-minor-mode global-my-mode my-mode my-mode :group my-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (my-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)

;; Display/hide sidebars
(define-key my-mode-map [f2] 'window-toggle-side-windows)

;; I hate minimize
(global-unset-key (kbd "C-x C-z"))

;; quicker window splitting
(define-key my-mode-map (kbd "M-1") 'delete-other-windows) ; was digit-argument
(define-key my-mode-map (kbd "M-2") 'split-window-vertically) ; was digit-argument
(define-key my-mode-map (kbd "M-3") 'split-window-horizontally ) ; was digit-argument
(define-key my-mode-map (kbd "M-0") 'delete-window) ; was digit-argument
(define-key my-mode-map (kbd "M-s") 'ace-window) ; was center-line

;; quick access to calculator
(define-key my-mode-map (kbd "C-c C") 'calc)

;; Global org-mode keybindings
(define-key my-mode-map (kbd "C-c o a") 'org-agenda)
(define-key my-mode-map (kbd "C-c o b") 'org-switchb)
(define-key my-mode-map (kbd "C-c o c") 'org-capture)
(define-key my-mode-map (kbd "C-c o g") 'org-clock-goto)
(define-key my-mode-map (kbd "C-c o l") 'org-store-link)

;; Switch light/dark theme
(define-key my-mode-map [f5] 'auto-dark-toggle-appearance)

;; Consult
(define-key my-mode-map (kbd "C-c C-j") 'consult-org-agenda)  ; was org-goto in org mode

;; rename current file
(define-key my-mode-map (kbd "C-c r") 'rename-visited-file)

;;;;;;;;;;;;;;;;;
;;; keychords ;;;
;;;;;;;;;;;;;;;;;

;; trigger commands by pressing keys in quick succession
;; https://github.com/emacsorphanage/key-chord
(use-package key-chord
  :demand t
  :config
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  (key-chord-define-global "jk" 'my-avy-embark)
  (key-chord-define-global "JK" 'my-avy-copy-word)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "qq" 'dired-jump)
  (key-chord-define-global "uu" 'vundo)
  (key-chord-define-global "xx" 'magit-status)
  (key-chord-define-global "XX" 'magit-dispatch)
  (key-chord-define-global "xz" 'projectile-find-file)
  (key-chord-define-global "XZ" 'magit-file-dispatch)
  (key-chord-define-global "yy" 'consult-yank-from-kill-ring)
  (key-chord-mode +1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General programming ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; automatically add closing symbol (parentheses, brackets, quotes...)
;; https://www.emacswiki.org/emacs/ElectricPair
(use-package elec-pair
  :straight nil  ;; Emacs built-in
  :hook (prog-mode . electric-pair-mode)
  )

;; color parentheses and other delimiters by nesting level
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Set background color to strings that match color
;; https://elpa.gnu.org/packages/rainbow-mode.html
(use-package rainbow-mode
  :diminish
  :hook ((prog-mode)))

;;;;;;;;;;;
;;; LSP ;;;
;;;;;;;;;;;

;; https://joaotavora.github.io/eglot/
(use-package eglot
  :straight (:type built-in)  ;; Emacs built-in
  :hook ((go-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (eglot-managed-mode . my-eglot-eldoc)
         (eglot-managed-mode . my-eglot-format-on-save-setup))
  :bind (:map eglot-mode-map
              ("C-c c a" . eglot-code-actions)
              ("C-c c d" . eglot-find-declaration)
              ("C-c c e" . eglot-events-buffer)
              ("C-c c E" . eglot-stderr-buffer)
              ("C-c c f" . eglot-format-buffer)
              ("C-c c h" . eglot-inlay-hints-mode)
              ("C-c c i" . eglot-find-implementation)
              ("C-c c o" . eglot-code-action-organize-imports)
              ("C-c c r" . eglot-rename)
              ("C-c c R" . eglot-reconnect)
              ("C-c c s" . eglot-shutdown)
              ("C-c c S" . eglot-shutdown-all)
              ("C-c c t" . eglot-find-type-definition))
  :preface
  (defun my-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  (defun my-eglot-format-on-save ()
    "Organize imports and format the buffer."
    (when (eglot-managed-p)
      ;; Servers that don't advertise `source.organizeImports' will
      ;; raise `user-error'; swallow it so saving still proceeds.
      (ignore-errors
        (eglot-code-actions nil nil "source.organizeImports" t))
      (eglot-format-buffer)))
  (defun my-eglot-format-on-save-setup ()
    "Toggle the format-on-save hook with `eglot-managed-mode'."
    (if (eglot-managed-p)
        (add-hook 'after-save-hook #'my-eglot-format-on-save nil t)
      (remove-hook 'after-save-hook #'my-eglot-format-on-save t)))
  :custom
  ;; shut down LSP server after last managed buffer is killed
  (eglot-autoshutdown t)
  ;; increase when need to debug LSP sessions
  (eglot-events-buffer-size 0)
  ;; use same eglot session when navigating outside project through Xref
  (eglot-extend-to-xref t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code diagnostics ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flymake
  :straight (:type built-in)  ;; Emacs built-in
  :hook ((prog-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! c" . flymake-start)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! t" . my-flymake-toggle-show-end-of-line)
              ("C-c ! L" . flymake-switch-to-log-buffer)
              ("C-c ! P" . flymake-show-project-diagnostics)
              ;; see consult for consult-flymake binding
              )
  :preface
  (defun my-flymake-toggle-show-end-of-line ()
      "Toggle flymake display fancy-error-display"
      (interactive)
      ;; need to disable flymake and then reenable, otherwise won't refresh
      (flymake-mode -1)
      (setopt flymake-show-diagnostics-at-end-of-line
              (if flymake-show-diagnostics-at-end-of-line nil 'short))
      (flymake-mode))
  )


;;;;;;;;;;;;;;;;;;;
;;; Tree-sitter ;;;
;;;;;;;;;;;;;;;;;;;

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;;;;;;;;;
;;; Rust ;;;
;;;;;;;;;;;;

(use-package rustic
  :after (eglot)
  :mode "\\.rs\'"
  :config
  (setq rustic-lsp-client 'eglot)
  ;; rust-analyzer should be installed with rustup
  ;; https://rust-analyzer.github.io/manual.html#rustup
  (setq rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  )

;;;;;;;;;;;;;;;;;;
;;; Emacs Lisp ;;;
;;;;;;;;;;;;;;;;;;

(defun start-or-visit-ielm ()
  "Switch to default `ielm' buffer. Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'start-or-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun my-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'my-lisp-coding-hook)
  (setq mode-name "EL")
  )
(setq my-emacs-lisp-mode-hook 'my-emacs-lisp-mode-defaults)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'my-emacs-lisp-mode-hook)))

(use-package eldoc
  :straight (:type built-in)  ;; Emacs built-in
  :commands (eldoc-mode)
  :hook (emacs-lisp-mode . eldoc-mode)
  :config (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;;;;;;;;;;;;;;
;;; Docker ;;;
;;;;;;;;;;;;;;

;; Docker
(use-package docker
  :bind (("C-c D" . docker)))

(use-package dockerfile-mode)

(use-package docker-compose-mode
  :mode "docker-compose.*\\.yml")

;;;;;;;;;;;;;;;;;
;;; org-mode  ;;;
;;;;;;;;;;;;;;;;;

(use-package org
  :straight (:type built-in)  ;; Emacs built-in
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . (lambda ()
                      (org-indent-mode +1)
                      (variable-pitch-mode -1)
                      (display-line-numbers-mode -1)
                      (visual-line-mode +1)
                      ))
  :bind (:map org-mode-map
              ("C-c j" . consult-org-heading)
              ("C-c r" . org-refile)
              ("C-c T" . valign-table))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     ))
  :custom
  (org-directory my-org-directory)
  ;; add all *.org files in the org-directory defined above
  (org-agenda-files (list org-directory))
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-log-done t)
  ;; save time when a task is done
  (org-log-done 'time)
  ;; open files folded
  (org-startup-folded t)
  (org-disputed-keys (quote (([(shift up)] . [(super shift up)])
                             ([(shift down)] . [(super shift down)])
                             ([(shift left)] . [(super shift left)])
                             ([(shift right)] . [(super shift right)])))
                     org-replace-disputed-keys t)
  ;; don't ask for confirmation before running an org-babel block
  (org-confirm-babel-evaluate nil)
  ;; custom org-agenda views
  (org-agenda-custom-commands '(("r" tags "refile")))
  ;; templates for org-capture
  (org-capture-templates
   `(("n" "Note" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Notes")
      "** %?  :refile:\n  %i\n  %a")
     ("t" "Task" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Tasks")
      "** TODO %?")
     ("m" "Media review")
     ("mb" "Book" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Books")
      "*** %^{Title} - %^{Author}\n    %?")
     ("mm" "Movie" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Movies")
      "*** %^{Title}\n    %?")
     ("mp" "Podcast" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Podcasts")
      "*** [[%^{URL}][%^{Title}]]\n    %?")
     ("mv" "Video" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Videos")
      "*** [[%^{URL}][%^{Title}]]\n    %?")
     ("mw" "Web page" entry (file+headline ,(expand-file-name "notes.org" org-directory) "Web pages")
      "*** [[%^{URL}][%^{Title}]]\n    %?")
     ("c" "Code" entry (file+headline ,(expand-file-name "code.org" org-directory) "Notes")
      "** TODO %?  :refile:\n  %i\n  %a")
     ))
  )

(use-package orgit
  :defer t)

(use-package orgit-forge
  :defer t)

;; syntax highlighting for exported source code blocks, needs listings and color latex
;; packages (texlive-latex-recommended package in Debian/Ubuntu)
(use-package ox-latex
  :straight nil  ;; Emacs built-in
  :defer t
  :config
  (setq org-latex-src-block-backend 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; export Org to Markdown
(use-package ox-md
  :straight nil  ;; Emacs built-in
  )

;; color links in Latex PDF output
(add-to-list 'org-latex-packages-alist "\\hypersetup{colorlinks=true,linkcolor=blue,urlcolor=blue}")

;;;;;;;;;;;;;;;;
;;; Markdown ;;;
;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :defer t
  :bind (:map markdown-mode-map
              ("C-c T" . valign-table)))

;; Major mode for Markdown using tree-sitter
(use-package markdown-ts-mode
  :straight nil  ;; Emacs built-in
  :mode ("\\.md\\'" . markdown-ts-mode)
  :bind (:map markdown-ts-mode-map
              ("C-c T" . valign-table)
              ("C-c C-l" . markdown-insert-link)))

;;;;;;;;;;;
;;; CSV ;;;
;;;;;;;;;;;

(use-package csv-mode
  :mode "\\.csv\'"
  ;; always enter CSV mode in align mode, easier to read
  :hook (csv-mode . csv-align-mode))

;;;;;;;;;;;;
;;; JSON ;;;
;;;;;;;;;;;;

(use-package json-ts-mode
  :straight nil  ;; Emacs built-in
  :mode "\\.json\'")

;;;;;;;;;;;;
;;; YAML ;;;
;;;;;;;;;;;;

(use-package yaml-mode
  :mode ("\\.yml\'"
         "\\.yaml\'")
  :hook ((yaml-mode . whitespace-mode)
         (yaml-mode . subword-mode)))

;;;;;;;;;;;;
;;; TOML ;;;
;;;;;;;;;;;;

(use-package toml-ts-mode
  :straight nil  ;; Emacs built-in
  :mode ("\\.toml\'"))

;;;;;;;;;;;;;;;;
;;; Snippets ;;;
;;;;;;;;;;;;;;;;

(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode text-mode) . my-maybe-enable-yas)
  :preface
  (defun my-maybe-enable-yas ()
  "Enable yas-minor-mode except in *scratch* buffers."
  (unless (string= (buffer-name) "*scratch*")
    (yas-minor-mode 1)))
  :config
  (add-to-list 'yas-snippet-dirs my-snippets-dir))

(use-package yasnippet-snippets
  :after yasnippet)

;;;;;;;;;;;;;;;
;;; Writing ;;;
;;;;;;;;;;;;;;;

;; detect opportunities to improve writing (passive voice, weasel words...)
;; https://github.com/bnbeckwith/writegood-mode
(use-package writegood-mode
  :hook (text-mode . writegood-mode)
  :bind (("C-x M-w" . 'writegood-mode)
         ("C-x M-g" . 'writegood-grade-level)
         ("C-x M-r" . 'writegood-reading-ease)))

;;;;;;;;;;;;;;
;;; Ebooks ;;;
;;;;;;;;;;;;;;

(use-package calibredb
  :defer t
  :init
  (setq sql-sqlite-program "sqlite3")
  :config
  (setq calibredb-root-dir "~/Calibre"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~Calibre"))))

(use-package nov
  :mode ("\\.epub\'" "\\.kepub\'")
  :custom
  (nov-place-file (no-littering-expand-var-file-name "nov-places"))
  )

;;;;;;;;;;;;;;
;;; visual ;;;
;;;;;;;;;;;;;;

;; no blinking cursor
(blink-cursor-mode -1)

;; blinking top and bottom lines instead of speaker buzz
(setq-default visible-bell t)

;; better scrolling
(setq scroll-margin 1
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; display line numbers in programming modes
(use-package display-line-numbers
  :straight nil  ;; Emacs built-in
  :hook (prog-mode . display-line-numbers-mode))

;; more useful frame title, showing either a file or a buffer name
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; better diff coloring
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

;; move the mouse away from the cursor
(mouse-avoidance-mode 'animate)

;; highlight the current line
(use-package hl-line
  :straight nil  ;; Emacs built-in
  :config
  (global-hl-line-mode 1)
  )

;; highlight parts changing because of some operations
(use-package volatile-highlights
  :diminish
  :config (volatile-highlights-mode t))

;; display emojis (and nice interactive picker)
;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :hook (after-init . global-emojify-mode)
  :bind ("C-c C-e" . 'emojify-insert-emoji)
  :config
  ;; disable emojify in more major modes
  (add-to-list 'emojify-inhibit-major-modes 'conf-mode)
  (add-to-list 'emojify-inhibit-major-modes 'magit-mode)
  (add-to-list 'emojify-inhibit-major-modes 'prog-mode)
  (add-to-list 'emojify-inhibit-major-modes 'restclient-mode)
  )

;;;;;;;;;;;;;;
;;; direnv ;;;
;;;;;;;;;;;;;;

;; use direnv to update Emacs environment
;; https://github.com/purcell/envrc
;; https://direnv.net/
(use-package envrc
  :config
  (envrc-global-mode))

;;;;;;;;;;;;;;;
;;; Systemd ;;;
;;;;;;;;;;;;;;;

;; Major mode for editing systemd units
;; https://github.com/holomorph/systemd-mode
(use-package systemd)

;;;;;;;;;;;;;;;;;;
;;; Screencast ;;;
;;;;;;;;;;;;;;;;;;

;; Show current command and its binding
;; https://github.com/tarsius/keycast/
(use-package keycast
  :config
  ;; integrate with doom-modeline
  ;; https://github.com/tarsius/keycast/issues/7#issuecomment-881469067
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for doom-mode-line)."
    :group 'keycast
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  )

;;;;;;;;;;
;;; AI ;;;
;;;;;;;;;;

;; read API secrets from KWallet via the freedesktop Secret Service API,
;; with authinfo files as fallback
(setq auth-sources '("secrets:kdewallet" "~/.authinfo.gpg" "~/.authinfo"))

;; LLM chat client: chat buffers, send region/buffer text, rewrite in place
;; https://github.com/karthink/gptel
;; C-c a c   open/start a chat buffer
;; C-c a RET send region (or buffer) to the LLM
;; C-c a m   menu: switch model (-m), parameters, context...
;; C-c a r   rewrite region in place
;; C-c a R   review region (or buffer) for malicious code
(use-package gptel
  :preface
  (defvar my-gptel-review-system-prompt
    "You are a meticulous security reviewer auditing third-party code
before it is merged or used. The user will show you one or more git
diffs, each preceded by the list of incoming commits with author and
date.

Scan for anything that could act maliciously once merged or called:
- backdoors, remote code execution, persistence mechanisms
- credential, token, key or data exfiltration (also covert channels)
- unexpected network activity, downloads or connections to new hosts
- obfuscation designed to hide behavior (heavy encoding, dead stores)
- tampering with build, installation or packaging scripts
- anomalous commit authorship: new maintainer, changed email address,
  commits by someone unrelated to the project

For every finding, state: severity (high/medium/low), the file and
hunk, and a one-paragraph rationale.  Close with an overall verdict:
whether the changes look safe to merge.  If nothing is suspicious,
say so plainly and briefly; do not invent findings."
    "System prompt for `my-gptel-review-malicious-code'.")
  (defun my-gptel-review-malicious-code ()
    "Review the region, or the whole buffer, for malicious code.
Send the text to the LLM with a security-audit system prompt and
show the response in the *gptel-review* buffer."
    (interactive)
    (require 'gptel)
    (let* ((text (buffer-substring-no-properties
                  (if (use-region-p) (region-beginning) (point-min))
                  (if (use-region-p) (region-end) (point-max))))
           (buffer (get-buffer-create "*gptel-review*"))
           (marker (with-current-buffer buffer
                     (erase-buffer)
                     (org-mode)
                     (goto-char (point-min))
                     (point-marker))))
      (pop-to-buffer buffer)
      (gptel-request
       (format
        "Review the following content from %s for malicious code or \
exploits.\n\n%s"
        (buffer-name) text)
       :system my-gptel-review-system-prompt
       :stream t
       :buffer buffer
       :position marker)))
  :bind (("C-c a c"   . gptel)
         ("C-c a RET" . gptel-send)
         ("C-c a a"   . gptel-add)
         ("C-c a A"   . gptel-add-file)
         ("C-c a m"   . gptel-menu)
         ("C-c a r"   . gptel-rewrite)
         ("C-c a R"   . my-gptel-review-malicious-code)
         :map org-mode-map
         ("C-c a o"   . gptel-org-set-topic)
         ("C-c a O"   . gptel-org-set-properties)
         )
  :config
  (setq gptel-default-mode 'org-mode
        gptel-backend
        (gptel-make-openai "Venice"
          :host "api.venice.ai"
          :endpoint "/api/v1/chat/completions"
          ;; API key via auth-source: KWallet item with attributes
          ;; host=api.venice.ai user=apikey
          :key #'gptel-api-key
          :stream t
          :models '(kimi-k3
                    claude-opus-5
                    openai-gpt-56-sol
                    grok-4-6
                    z-ai-glm-5-3-flash))
        gptel-model 'z-ai-glm-5-3-flash)
  (gptel-make-openai "llama.cpp"
    :protocol "http"
    :host "localhost:11434"
    :stream t
    :models '((Qwen3.8-27b
               :description "Local Qwen3.8 27B (Q4_K_M)"
               :capabilities (media)
               :mime-types
               ("image/jpeg" "image/png" "image/gif" "image/webp")
               :context-window 64))))

;; Agentic coding frontend driving external agents via ACP; one opencode
;; session per project, each in its own firejail sandbox
;; https://github.com/xenodium/agent-shell
;; C-c a s   start opencode for this project, or switch to its session
(use-package agent-shell
  :bind (("C-c a s" . my-agent-shell-opencode))
  :commands (agent-shell-opencode-start-agent)
  :hook (agent-shell-mode . my-agent-shell-completion-setup)
  :preface
  ;; declare special so the lets in `my-agent-shell-opencode' bind
  ;; dynamically even before the package loads (init.el is lexical)
  (defvar agent-shell-command-prefix)
  (defvar agent-shell-opencode-acp-command)
  (defvar my-agent-shell-command-override nil
    "Full agent command to run; set per project via dir-locals.")
  (defvar my-agent-shell-extra-firejail-args nil
    "Extra firejail arguments for the agent command; set via dir-locals.
Inserted after the project --whitelist, e.g. additional whitelists.")
  (defvar my-agent-shell-extra-args nil
    "Extra arguments appended to the agent command; set via dir-locals.")
  (put 'my-agent-shell-command-override 'safe-local-variable #'stringp)
  (put 'my-agent-shell-extra-firejail-args 'safe-local-variable
       (lambda (v) (and (listp v) (seq-every-p #'stringp v))))
  (put 'my-agent-shell-extra-args 'safe-local-variable
       (lambda (v) (and (listp v) (seq-every-p #'stringp v))))
  (defun my-agent-shell--dir-locals (dir)
    "Read agent-shell dir-local variables for DIR.
Return (OVERRIDE EXTRA-FIREJAIL-ARGS EXTRA-ARGS)."
    (with-temp-buffer
      (setq default-directory (file-name-as-directory dir))
      (hack-dir-local-variables-non-file-buffer)
      (list (and (boundp 'my-agent-shell-command-override)
                 my-agent-shell-command-override)
            (and (boundp 'my-agent-shell-extra-firejail-args)
                 my-agent-shell-extra-firejail-args)
            (and (boundp 'my-agent-shell-extra-args)
                 my-agent-shell-extra-args))))
  (defun my-agent-shell--default-command (dir extra-firejail-args extra-args)
    "Default jailed opencode command for DIR.
EXTRA-FIREJAIL-ARGS are inserted after the project whitelist,
EXTRA-ARGS are appended at the end."
    (append (list "firejail" "--profile=opencode"
                  (format "--whitelist=%s" dir))
            extra-firejail-args
            (list "/usr/bin/opencode" "acp")
            extra-args))
  (defun my-agent-shell-opencode ()
    "Start an opencode agent session for the current Projectile project.
If a session buffer already exists for the project, switch to it.
Otherwise propose the full jailed command in the minibuffer for
review and editing, then start it.  Outside projects, jail the
current directory."
    (interactive)
    (let ((root (file-truename (or (ignore-errors (projectile-project-root))
                                   default-directory))))
      (if-let* ((buf (seq-find (lambda (b)
                                 (with-current-buffer b
                                   (and (derived-mode-p 'agent-shell-mode)
                                        (string-equal
                                         (file-truename default-directory)
                                         root))))
                               (buffer-list))))
          (pop-to-buffer buf)
        (pcase-let ((`(,override ,xtra-jail ,xtra)
                     (my-agent-shell--dir-locals root)))
          (let* ((default (or override
                              (combine-and-quote-strings
                               (my-agent-shell--default-command
                                root xtra-jail xtra))))
                 (input (read-shell-command "Agent command: " default))
                 (cmd (split-string-and-unquote input)))
            (unless cmd (user-error "Empty command, aborted"))
            (let ((agent-shell-opencode-acp-command cmd)
                  (agent-shell-command-prefix nil)
                  (default-directory root))
              (agent-shell-opencode-start-agent)))))))
  (defun my-agent-shell-completion-setup ()
    "Use Corfu for @ and / completion in agent-shell buffers."
    ;; leave only agent-shell's @ / capfs (plus comint's), avoiding
    ;; duplicate candidates from the global cape capfs
    (setq-local completion-at-point-functions
                (seq-remove (lambda (f) (memq f '(cape-file cape-dabbrev)))
                            completion-at-point-functions))
    ;; popups only via the @ and / triggers or explicit C-M-i
    (setq-local corfu-auto nil)
    (corfu-mode 1))
  :custom
  ;; same binary as the shell alias
  (agent-shell-opencode-acp-command '("/usr/bin/opencode" "acp"))
  ;; safety net for sessions not started via C-c a s (e.g. M-x
  ;; agent-shell-opencode-start-agent): plain jail, session dir only
  (agent-shell-command-prefix
   (lambda (buffer)
     (list "firejail" "--profile=opencode"
           (format "--whitelist=%s"
                   (with-current-buffer buffer default-directory))))))

;;;;;;;;;;;;;;;;;
;;; Profiling ;;;
;;;;;;;;;;;;;;;;;

;; Uncomment to profile startup time (see also early-start.el)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (run-with-idle-timer 1 nil #'profiler-report)
;;             (run-with-idle-timer 1 nil #'profiler-stop)))
