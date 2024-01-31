;; -*- lexical-binding: t -*-

;;;;;;;;;;;;
;;; init ;;;
;;;;;;;;;;;;

;; supress native compilation warnings
(setq comp-deferred-compilation-deny-list '())
(setq native-comp-async-report-warnings-errors nil)

;; no message in scratch buffer
(setq initial-scratch-message "")

;; no startup screen
(setq inhibit-startup-screen t)

;; Super handy macro for loading packages but not stopping the init
;; process if they aren't found.
;; https://www.gnu.org/software/emacs/manual/html_node/use-package/index.html
(require 'use-package)
;; Pin packages to MELPA stable unless explicitly changed with :pin
(setq use-package-always-pin "melpa-stable")
(setq use-package-always-demand t)
(setq use-package-always-ensure t)

;; Use .el if it is newer
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(defvar my-line-length 90)

(defvar my-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "Where to put all automatically generated save/history-files.")
(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir))

;; Stop customize from writing to my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Theme
;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-solarized-dark t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(defvar my-toggle-theme-hook nil)

;; Toggle between light and dark themes
(defun my-toggle-theme ()
  (interactive)
  (cond
   ((eq (car custom-enabled-themes) 'doom-solarized-dark)
    (disable-theme 'doom-solarized-dark)
    (load-theme 'doom-solarized-light t))
   ((eq (car custom-enabled-themes) 'doom-solarized-light)
    (disable-theme 'doom-solarized-light)
    (load-theme 'doom-solarized-dark t))
   )
  (run-hooks 'my-toggle-theme-hook))

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

;; Cycle between candidates when there are not a lot of them
(setq completion-cycle-threshold 3)

;; remember point location when reopening a file
(use-package saveplace
  :init
  (save-place-mode)
  :config
  (setq save-place-file (expand-file-name "saveplace" my-savefile-dir)))

;; ediff - don't start another frame
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package smex
  :init
  (setq smex-save-file (expand-file-name "smex-items" my-savefile-dir)))

;; run garbage collection when frame loses focus, which should mean I'm not using
;; Emacs at that time so I won't care about any slowdown
(add-function :after after-focus-change-function
              (defun my-garbage-collect-maybe ()
                (unless (frame-focus-state)
                  (garbage-collect))))

;;;;;;;;;;;;;;
;;; editor ;;;
;;;;;;;;;;;;;;

;; always indent with spaces unless major mode overrides
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; save files to home directory by default
(setq-default default-directory '~)

;; (much) bigger kill ring
(setq-default kill-ring-max 5000)

;; delete the selection with a keypress
(delete-selection-mode t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

;; use directory name in buffer names of files with the same name
(use-package uniquify
  :ensure nil  ;; Emacs built-in
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        ;; rename after killing uniquified
        uniquify-after-kill-buffer-p t
        ;; don't muck with special buffers
        uniquify-ignore-buffers-re "^\\*"))

;; savehist keeps track of some history
(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" my-savefile-dir)))

;; save recent files
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" my-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never))

(use-package elec-pair
  ;; enabled for specific modes with electric-pair-local-mode
  )

;; smarter kill-ring navigation
(use-package browse-kill-ring
  :bind (("C-x C-k" . browse-kill-ring)))

;; automatically save buffers associated with files on buffer and window switch
;; https://github.com/bbatsov/super-save
(use-package super-save
  :diminish
  :config (super-save-mode +1))

;; TRAMP is awesome
;; https://www.gnu.org/software/tramp/
(use-package tramp
  :config
  (setq
   ;; don't pollute .emacs.d directory
   tramp-persistency-file-name (expand-file-name "tramp" my-savefile-dir)
   ;; default to SSH
   tramp-default-method "ssh"))

(set-default 'imenu-auto-rescan t)

;; enable set goal column (C-x C-n)
(put 'set-goal-column 'disabled nil)

(use-package simple
  :ensure nil  ;; Emacs built-in
  ;; upcase-downcase word at point or region if set
  :bind (("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim))
  :config
  ;; enabled change region case commands
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

;; bookmarks
(use-package bookmark
  :ensure nil  ;; Emacs built-in
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" my-savefile-dir)
        bookmark-save-flag 1))

(use-package align
  :ensure nil  ;; Emacs built-in
  :bind (("C-x \\" . align-regexp)))

;; avy allows us to effectively navigate to visible things
(use-package avy
  :after (embark)
  :preface
  ;; By Chmouel Boudjnah https://mastodon.social/@chmouel@fosstodon.org/109715305722356540
  (defun my-avy-copy-word (arg)
    (interactive "p")
    (save-excursion
      (call-interactively  'avy-goto-symbol-1)
      (let ((symbol (thing-at-point 'symbol)))
        (when symbol
          (kill-new symbol)
          (message "\"%s\" has been copied" symbol)))))
  (defun my-avy-embark (arg)
    (interactive "p")
    (save-excursion
      (call-interactively  'avy-goto-symbol-1)
      (let ((symbol (thing-at-point 'symbol)))
        (when symbol
          (embark-act symbol)))))
  :config (setq avy-background t
                avy-style 'at-full))

;; highlight results in search and replace commands
;; https://github.com/emacsorphanage/anzu
(use-package anzu
  :diminish
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode))

;; live visual feedback when defining a regexp for replace
;; https://github.com/benma/visual-regexp.el
(use-package visual-regexp
  :bind (("s-v r" . vr/replace)
         ("s-v q" . vr/query-replace)))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; it's 2020 already
(setq-default fill-column my-line-length)

;; progressively expand region around cursor
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; automatically clean up whitespace on save only on initially clean buffers
;; disable by setting whitespace-cleanup-mode to nil in dir or local variables
(use-package whitespace-cleanup-mode
  :bind (("C-c M-w" . whitespace-cleanup))
  :config (global-whitespace-cleanup-mode))

;; saner regex syntax
(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package eshell
  :config
  (setq eshell-directory-name (expand-file-name "eshell" my-savefile-dir)))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" my-savefile-dir))

;; crux is a collection of general editing utilities, see below for keybindings
;; https://github.com/bbatsov/crux
(use-package crux
  :bind (("C-c o" . crux-open-with)
         ("C-a" . crux-move-beginning-of-line)
         ("C-c C-i" . crux-indent-defun)
         ("C-c e" . crux-eval-and-replace)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c C-d" . crux-duplicate-current-line-or-region)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
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
  :pin gnu
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; show uncommitted changes in the gutter
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
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
  :diminish
  :config (editorconfig-mode 1))

;; show all remaining key combinations when doing multi-key commands
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
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

;; edit a grep buffer and apply those changes to the file buffer like sed interactively
;; https://github.com/mhayashi1120/Emacs-wgrep
(use-package wgrep
  :after (embark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window and frame management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; manage window configurations
;; https://depp.brause.cc/eyebrowse/
(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)
  ("C-x k" . persp-kill-buffer*)
  :hook
  (kill-emacs . persp-state-save)
  :custom
  (persp-mode-prefix-key (kbd "C-z"))
  (persp-state-default-file (expand-file-name "perspective" my-savefile-dir))
  :init
  (persp-mode)
  )

;; tame the flood of ephemeral windows Emacs produces
;; https://github.com/karthink/popper
(use-package popper
  :after (perspective projectile)
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq
   ;; how to group popups
   popper-group-function #'popper-group-by-perspective
   ;; which buffers should be considered popups
   popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*eldoc.*\\*"
     "\\*Flymake diagnostics.*\\*"
     help-mode
     helpful-mode
     compilation-mode
     ))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (setq popper-window-height 20)
  )

;;;;;;;;;;;;;
;;; Shell ;;;
;;;;;;;;;;;;;

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(use-package fish-mode
  :mode ".fish")

;;;;;;;;;;;;;;;;;;;;;;;
;;; dired / dirvish ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package dirvish
  :bind
  (("C-x C-j" . dired-jump)
   :map dired-mode-map
   ("^"   . 'dired-up-directory)
   :map dirvish-mode-map  ; Dirvish inherits `dired-mode-map'
   ("a"   . 'dirvish-quick-access)
   ("f"   . 'dirvish-file-info-menu)
   ("y"   . 'dirvish-yank-menu)
   ("N"   . 'dirvish-narrow)
   ("h"   . 'dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . 'dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . 'dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . 'dirvish-subtree-toggle)
   ("M-f" . 'dirvish-history-go-forward)
   ("M-b" . 'dirvish-history-go-backward)
   ("M-l" . 'dirvish-ls-switches-menu)
   ("M-m" . 'dirvish-mark-menu)
   ("M-t" . 'dirvish-layout-toggle)
   ("M-s" . 'dirvish-setup-menu)
   ("M-e" . 'dirvish-emerge-menu)
   ("M-j" . 'dirvish-fd-jump)
   )
  :init
  (dirvish-override-dired-mode)
  :custom
  (dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-attributes '(vc-state
                        subtree-state
                        all-the-icons
                        collapse
                        git-msg
                        ;; file-time
                        file-size))
  :config
  (dirvish-peek-mode)
  (setq dirvish-quick-access-entries '(("h" "~/"                   "Home")
                                       ("c" "~/Code/"              "Code")
                                       ("d" "~/Downloads/"         "Downloads")
                                       ("D" "~/Documents/"         "Documents")
                                       ("m" "~/Music/"             "Music")
                                       ("M" "~/Music to sort/"     "Music to sort")
                                       ("o" "~/Org/"               "Org")
                                       ("p" "~/Pictures/"          "Photos")
                                       ("P" "~/Pictures to sort/"  "Photos to sort")
                                       ("s" "~/Sync/"  "Sync"))
        dirvish-cache-dir (expand-file-name "dirvish" my-savefile-dir)
        ;; slightly larger dirvish-side
        dirvish-side-width 50
        ;; dirvish-side
        dirvish-side-follow-mode t
        ;; by default jump inside home
        dirvish-fd-default-dir "~"
        ;; revert dired (and dirvish) buffers on revisiting their directory
        dired-auto-revert-buffer t
        ;; always delete and copy recursively
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        ;; if there is a dired buffer displayed in the next window, use its
        ;; current subdir, instead of the current subdir of this dired buffer
        dired-dwim-target t
        ;; TODO: enable in Emacs 29, drag & drop support
        ;; dired-mouse-drag-files t
        ;; mouse-drag-and-drop-region-cross-program t
        )
  )

;;;;;;;;;;;;;;;
;;; ibuffer ;;;
;;;;;;;;;;;;;;;

(use-package ibuffer
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
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)
  :config
  (setq vertico-count 20)
  (setq vertico-cycle t)
  )

;; better directory navigation in vertico
;; https://github.com/minad/vertico/blob/main/extensions/vertico-directory.el
(use-package vertico-directory
  :after vertico
  :ensure nil  ;; bundled with vertico
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
  :after (perspective projectile)
  :bind (("C-c f" . consult-fd)
         ("C-c H" . my-consult-fd-home)
         ("C-c R" . my-consult-fd-root)
         ("C-c j" . consult-outline)
         ("C-c i" . consult-imenu)
         ("C-c g" . consult-ripgrep)
         ("C-c G" . my-consult-ripgrep-at-point)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-s" . consult-line)
         ("C-c s" . my-consult-line-at-point)
         ("C-c M-s" . consult-line-multi)
         ("C-c M-x" . consult-mode-command)
         ("C-c C-s" . consult-yasnippet)
         ("C-c C-m" . consult-minor-mode-menu)
         ("C-h C-m" . consult-man)
         ("C-x l" . consult-locate)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ;; originally mark-word, I use expand-region
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
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))
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
  :custom
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
   consult--source-hidden-buffer
   consult--source-buffer
   consult--source-recent-file
   consult--source-bookmark
   consult--source-project-buffer
   consult--source-project-recent-file
   :preview-key '"M-.")
  ;; filter buffers in current perspective by default
  ;; can narrow to see all buffers if necessary
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
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
  :after (perspective which-key)
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
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

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
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))
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
                                   (eglot (styles orderless))))
  (orderless-style-dispatchers '(my-orderless-dispatch))
  ;; allow escaping space with backslash
  (orderless-component-separator #'orderless-escapable-split-on-space))

;;;;;;;;;;;
;;; git ;;;
;;;;;;;;;;;

;; https://magit.vc/
(use-package magit
  :pin melpa
  :if (executable-find "git")
  :bind (("s-m m" . magit-status)
         ("s-m s-m" . magit-status)
         ("s-m j" . magit-dispatch)
         ("s-m k" . magit-file-dispatch)
         ("s-m l" . magit-log-buffer-file)
         ("s-m b" . magit-blame))
  :config
  (setq
   magit-wip-after-apply-mode' t
   magit-wip-after-save-mode' t
   magit-wip-before-change-mode' t
   ;; path to my root code dir, so I can do C-x g from anywhere
   magit-repository-directories '(("~/Code" . 2))
   ;; create a local tracking branch when visiting a remote branch
   magit-visit-ref-create t
   ;; don't ask for confirmation when pushing branches
   magit-push-always-verify nil
   ;; put history.el in the custom savefile dir
   transient-history-file (expand-file-name "transient-history.el" my-savefile-dir)
   )
  ;; enable magit-clean
  (put 'magit-clean 'disabled nil)
  )

;; https://magit.vc/manual/forge/
(use-package forge
  :pin melpa
  :after magit
  :config
  (setq forge-database-file
        (expand-file-name "forge-database.sqlite" my-savefile-dir))
  )

;; browse previous revisions of any git-controlled file
;; https://codeberg.org/pidu/git-timemachine
(use-package git-timemachine
  :bind (("s-m t" . git-timemachine)))


;;;;;;;;;;;;;;;;
;;; projects ;;;
;;;;;;;;;;;;;;;;

(use-package project
  :config
  (setq project-list-file (expand-file-name "projects" my-savefile-dir)))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :bind-keymap (("s-p" . projectile-command-map))
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" my-savefile-dir)
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-savefile-dir)
        ;; open a dirvish buffer when switching projects
        projectile-switch-project-action 'dirvish
        ;; https://docs.projectile.mx/projectile/configuration.html#project-specific-compilation-buffers
        projectile-per-project-compilation-buffer t
        )
  (projectile-mode t))

;;;;;;;;;;;;;;;;;;;;
;;; autocomplete ;;;
;;;;;;;;;;;;;;;;;;;;

;; enhance completion at point with a small completion popup
;; https://github.com/minad/corfu
(use-package corfu
  :hook ((minibuffer-setup . corfu-enable-in-minibuffer)
         ;; display autocomplete popup automatically in programming modes
         (prog-mode . corfu-set-local-auto))
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
  (defun corfu-set-local-auto ()
    "Enable popup appearing automatically only for current buffer"
    (setq-local corfu-auto t))
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :config
  (setq corfu-excluded-modes '(typescript-mode web-mode))
  (global-corfu-mode)
  (corfu-indexed-mode)
  (corfu-popupinfo-mode)
  :custom
  ;; autocompletion only pops up automatically in programming modes
  (corfu-auto nil)
  ;; show candidates as soon as 1 character is pressed
  (corfu-auto-prefix 1)
  ;; show candidates after this many seconds
  (corfu-auto-delay 0.2)
  (corfu-min-width 20)
  ;; always have the same width
  (corfu-max-width 80)
  (corfu-count 20)
  (corfu-scroll-margin 3)
  (corfu-cycle nil)
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

(use-package cape
  :hook ((org-mode . my-cape-capf-setup-org))
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)  ; etags
         ("C-c p d" . cape-dabbrev)  ; basically `dabbrev-completion'
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :preface
  ;; Org
  (defun my-cape-capf-setup-org ()
    (let (result)
      (dolist (element (list
                        (cape-capf-super #'cape-ispell #'cape-dabbrev))
                       result)
        (add-to-list 'completion-at-point-functions element))))
  )

;; icons for autocomplete results
;; https://github.com/jdtsmith/kind-icon
(use-package kind-icon
  :pin gnu
  :after corfu
  :hook (my-toggle-theme . (lambda () (interactive) (kind-icon-reset-cache)))
  :custom
  ;; explicitly enable icons
  (kind-icon-use-icons t)
  ;; have background color be the same as corfu face background
  (kind-icon-default-face 'corfu-default)
  ;; use midpoint color between foreground and background colors
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  ;; don't allow svg-lib to litter with its cache directory
  (svg-lib-icons-dir (expand-file-name "svg-lib/cache/" my-savefile-dir))
  :config
  ;; enable for corfu
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; use dabbrev with corfu
(use-package dabbrev
  ;; swap M-/ and C-M-/, as M-/ will use corfu
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; yasnippet support in cape
;; https://github.com/elken/yasnippet-capf
(use-package yasnippet-capf
  :after cape
  :pin melpa
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;;;;;;;;;;;;;;;;;;
;;; spell check ;;;
;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  ;; (prog-mode . flyspell-prog-mode) is too noisy, too many false positives
  :hook ((text-mode . flyspell-mode))
  :init
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--camel-case")))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"))
   (t
    (setq ispell-program-name nil)))
  :config
  (setq flyspell-default-dictionary "american-w_accents"))

;;;;;;;;;;;;;;;;;;;
;;; keybindings ;;;
;;;;;;;;;;;;;;;;;;;

;; use shift + arrow keys to switch between visible buffers
(use-package windmove
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

(define-globalized-minor-mode global-my-mode my-mode my-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (my-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-mode)

;; I hate minimize
(global-unset-key (kbd "C-x C-z"))

;; quicker window splitting
(define-key my-mode-map (kbd "M-1") 'delete-other-windows) ; was digit-argument
(define-key my-mode-map (kbd "M-2") 'split-window-vertically) ; was digit-argument
(define-key my-mode-map (kbd "M-3") (lambda ()
                                      (interactive)
                                      (funcall 'split-window-horizontally
                                               (+ fill-column 11)))) ; was digit-argument
(define-key my-mode-map (kbd "M-0") 'delete-window) ; was digit-argument
(define-key my-mode-map (kbd "M-s") 'ace-window) ; was center-line

;; quick access to calculator
(define-key my-mode-map (kbd "C-x c") 'calc)
(define-key my-mode-map (kbd "C-x C-c") 'calc) ; was save-buffers-kill-terminal
(define-key my-mode-map (kbd "C-x C") 'full-calc)

;; Global org-mode keybindings
(define-key my-mode-map (kbd "C-c c") 'org-capture)
(define-key my-mode-map (kbd "C-c l") 'org-store-link)
(define-key my-mode-map (kbd "C-c a") 'org-agenda)
(define-key my-mode-map (kbd "C-c b") 'org-switchb)
(define-key my-mode-map (kbd "C-c C-x C-j") 'org-clock-goto)

;; discover-my-major
(define-key my-mode-map (kbd "C-h M-m") 'discover-my-major)

;; Switch light/dark theme
(define-key my-mode-map [f5] 'my-toggle-theme)

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
  :after (avy consult crux dirvish magit projectile vundo)
  :config
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  (key-chord-define-global "jk" 'my-avy-embark)
  (key-chord-define-global "JK" 'my-avy-copy-word)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "qq" 'dirvish-dwim)
  (key-chord-define-global "qs" 'dirvish-side)
  (key-chord-define-global "uu" 'vundo)
  (key-chord-define-global "xx" 'magit-status)
  (key-chord-define-global "xz" 'projectile-find-file)
  (key-chord-define-global "yy" 'consult-yank-from-kill-ring)
  (key-chord-mode +1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; structured editing ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; smart pairing for all programming modes
;; https://github.com/AmaiKinono/puni
;; (use-package puni
;;   :pin melpa
;;   :hook (term-mode . puni-disable-puni-mode)
;;   :bind (("M-<up>" . puni-splice)
;;          ("M-S-<up>" . puni-raise)
;;          ("M-<down>" . puni-split)
;;          ("M-S-<down>" . puni-squeeze)
;;          ("M-[" . puni-slurp-backward)
;;          ("M-]" . puni-slurp-forward)
;;          ("M-{" . puni-barf-backward)
;;          ("M-}" . puni-barf-forward))
;;   :hook ((minibuffer-setup . puni-disable-puni-mode)
;;          (org-mode . puni-disable-puni-mode))
;;   :preface
;;   (defun my-disable-puni-in-minibuffer ()
;;   "Disable `puni-mode' in minibuffer unless when eval-expression"
;;   (unless (eq this-command 'eval-expression)
;;       (puni-disable-puni-mode)))
;;   :config
;;   (puni-global-mode))

;;;;;;;;;;;;;;;;;;;;;;
;;; Autoformatting ;;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "-"))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(isort black))
  )


;;;;;;;;;;;
;;; LSP ;;;
;;;;;;;;;;;

;; https://joaotavora.github.io/eglot/
(use-package eglot
  :preface
  (defun my-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  (defun my-eglot-organize-imports () (interactive)
         (if (and (eglot-managed-p)
                  (eglot--server-capable :OrganizeImports))
             (eglot-code-actions nil nil "source.organizeImports" t)))
  (defun my-eglot-format-on-save () (interactive)
         (if (eglot-managed-p)
             (eglot-format-buffer)))
  :hook ((go-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (eglot-managed-mode . my-eglot-eldoc)
         (before-save . my-eglot-organize-imports)
         (before-save . my-eglot-format-on-save))
  :bind (:map eglot-mode-map
              ("s-l a" . eglot-code-actions)
              ("s-l d" . eglot-find-declaration)
              ("s-l f" . eglot-format)
              ("s-l h" . eglot-inlay-hints-mode)
              ("s-l i" . eglot-find-implementation)
              ("s-l o" . eglot-code-action-organize-imports)
              ("s-l r" . eglot-rename)
              ("s-l t" . eglot-find-type-definition))
  :config
  (setq
   ;; increase when need to debug LSP sessions
   eglot-events-buffer-size 0
   ;; use same eglot session when navigating outside project through Xref
   eglot-extend-to-xref t
   )
  ;; (lsp-register-custom-settings
  ;;  '(("pylsp.plugins.pyls_mypy.enabled" t t)
  ;;    ("pylsp.plugins.pyls_mypy.live_mode" nil t)
  ;;    ("pylsp.plugins.pyls_black.enabled" t t)
  ;;    ("pylsp.plugins.pyls_isort.enabled" t t)))
  )

;;;;;;;;;;;;;;;;;;;
;;; Tree-sitter ;;;
;;;;;;;;;;;;;;;;;;;

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;
;;; Javascript ;;;
;;;;;;;;;;;;;;;;;;

;; Major mode for Javascript files
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :hook (js2-mode . (lambda () (run-hooks 'my-js-mode-overrides)))
  :config
  (defun my-js-mode-overrides ()
    (setq mode-name "JS2")
    (js2-imenu-extras-mode +1)
    (subword-mode +1)
    ))

;; Refactor operations on top of JS2
;; https://github.com/js-emacs/js2-refactor.el
(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  )

;; Support for JSX files
;; https://github.com/felipeochoa/rjsx-mode
(use-package rjsx-mode
  :mode "\\.jsx"
  )

;;;;;;;;;;;;;;;;;;
;;; Typescript ;;;
;;;;;;;;;;;;;;;;;;

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;;;;;;;;;;;
;;; Web ;;;
;;;;;;;;;;;

;; https://web-mode.org/
(use-package web-mode
  :mode (
         "\\.html\\'"
         "\\.phtml\\'"
         "\\.php\\'"
         "\\.tsx\\'"
         "\\.tpl\\.php\\'"
         "\\.tpl\\'"
         "\\.hbs\\'"
         "\\.blade\\.php\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.html?\\'"
         )
  :hook (web-mode . my-web-mode-set-engine-django)
  :preface
  ;; detect if Django project and set web-mode engine to django
  (defun my-web-mode-set-engine-django ()
    (if (projectile-project-p)
        (if (file-exists-p (concat (projectile-project-root) "manage.py"))
            (web-mode-set-engine "django"))))
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-enable-auto-indentation nil))

;;;;;;;;;;;
;;; CSS ;;;
;;;;;;;;;;;

(use-package css-mode
  :ensure nil  ;; Emacs built-in
  :config
  (setq css-indent-offset 2)
  (defun my-css-mode-defaults ()
    (rainbow-mode +1))
  (setq my-css-mode-hook 'my-css-mode-defaults)
  (add-hook 'css-mode-hook (lambda ()
                             (run-hooks 'my-css-mode-hook))))

;;;;;;;;;;;;;;
;;; Python ;;;
;;;;;;;;;;;;;;

(use-package python
  :hook (python-ts-mode . my-python-config)
  :preface
  (defun my-python-config ()
    "My personal configuration for python-ts-mode"
    (subword-mode +1)
    ;; (python-docstring-mode +1)  ; not available in Guix yet
    (eglot-ensure)
    ))

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

;;;;;;;;;;
;;; Go ;;;
;;;;;;;;;;

(use-package go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ("M-RET" . godef-jump)
              ("M-." . godef-jump)
              ("C-M-RET" . godef-jump-other-window)))

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
  :commands (eldoc-mode)
  :hook (emacs-lisp-mode . eldoc-mode)
  :config (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package rainbow-mode
  :pin gnu
  :diminish
  :hook (emacs-lisp-mode))

;;;;;;;;;;;;;;;;;;;
;;; Scheme Lisp ;;;
;;;;;;;;;;;;;;;;;;;

(use-package geiser
  :hook (scheme-mode . my-lisp-coding-defaults)
  :config
  (setq
   ;; geiser replies on a REPL to provide autodoc and completion
   geiser-mode-start-repl-p t
   ;; save geiser history to savefile dir
   geiser-repl-history-filename (expand-file-name "geiser-history" my-savefile-dir)
   ))

;;;;;;;;;;;;;;
;;; Docker ;;;
;;;;;;;;;;;;;;

;; Docker
(use-package docker
  :bind (("s-D" . docker)))

(use-package dockerfile-mode)

(use-package docker-compose-mode
  :mode "docker-compose.*\\.yml")

;;;;;;;;;;;;;;;;;
;;; org-mode  ;;;
;;;;;;;;;;;;;;;;;

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . (lambda ()
                      (org-indent-mode)
                      (variable-pitch-mode -1)
                      (display-line-numbers-mode -1)
                      ))
  :bind (:map org-mode-map
              ("C-c j" . consult-org-heading)
              ("C-c r" . org-refile))
  :custom
  (org-directory (expand-file-name "~/Org"))
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
     ("s" "Sesame Labs")
     ("sn" "Note" entry (file ,(expand-file-name "sesamelabs/notes.org" org-directory))
      "* %?")
     ("si" "Interruption" entry (file+headline ,(expand-file-name "sesamelabs/meetings.org" org-directory) "Interruptions")
      "** %?" :clock-in t :clock-resume t)
     ("sm" "Meeting" entry (file ,(expand-file-name "sesamelabs/meetings.org" org-directory))
      "* %?" :clock-in t :clock-resume t)
     ("st" "Task" entry (file ,(expand-file-name "sesamelabs/tasks.org" org-directory))
      "* TODO %?" :clock-in t :clock-resume t)
     ("sr" "PR review" entry (file ,(expand-file-name "sesamelabs/tasks.org" org-directory))
      "* TODO [[%^{PR URL}][PR #%^{PR description}]]" :clock-in t :clock-resume t)
     ))
  )

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Make leading stars truly invisible, by rendering them as spaces!
  (setq
   org-superstar-leading-bullet ?\s
   org-superstar-leading-fallback ?\s
   org-hide-leading-stars nil
   org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                     ("[ ]"  . 9744)
                                     ("DONE" . 9745)
                                     ("[X]"  . 9745))
   ))

;; enable more languages support in org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   ;;   emacs-ob-rust not in Guix yet
   ;;   (rust . t)
   (shell . t)
   ;;   emacs-ob-sql-mode not in Guix yet
   ;;   (sql-mode . t)
   ))

(use-package orgit
  :pin melpa)

(use-package orgit-forge
  :pin melpa)

;; syntax highlighting for exported source code blocks, needs listings and color latex
;; packages (texlive-latex-recommended package in Debian/Ubuntu)
(require 'ox-latex)
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; color links in Latex PDF output
(add-to-list 'org-latex-packages-alist "\\hypersetup{colorlinks=true,linkcolor=blue,urlcolor=blue}")

;;;;;;;;;;;;;;;;
;;; Markdown ;;;
;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  )

;;;;;;;;;;;
;;; CSV ;;;
;;;;;;;;;;;

(use-package csv-mode
  :pin gnu
  :mode "\\.csv\'"
  ;; always enter CSV mode in align mode, easier to read
  :hook (csv-mode . csv-align-mode))

;;;;;;;;;;;;
;;; JSON ;;;
;;;;;;;;;;;;

(use-package json-mode
  :mode "\\.json\'"
  :config (setq js-indent-level 2))

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

(use-package toml-mode
  :pin melpa
  :mode ("\\.toml\'"))

;;;;;;;;;;;;;;;;
;;; Snippets ;;;
;;;;;;;;;;;;;;;;

;; load yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  ;; term-mode does not play well with yasnippet
  :hook (term-mode . (lambda () (yas-minor-mode -1)))
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1))

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
  :defer t
  :mode "\\.epub\'"
  :custom
  (nov-place-file (expand-file-name "nov-places" my-savefile-dir))
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
(global-hl-line-mode +1)

;; color parentheses and other delimiters by pair
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; highlight parts changing because of some operations
(use-package volatile-highlights
  :diminish
  :config (volatile-highlights-mode t))

;; olivetti-mode centers text
;; https://github.com/rnkn/olivetti
(use-package olivetti
  :hook ((markdown-mode . olivetti-mode)))

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

(use-package systemd
  :pin nongnu)
