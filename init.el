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

;; Enable MELPA if we need the package system (i.e. not running under Guix)
(when (not guix-p)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Super handy macro for loading packages but not stopping the init
;; process if they aren't found.
;; https://github.com/jwiegley/use-package
(require 'use-package)

;; Use .el if it is newer
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

(defvar my-line-length 90)

(defvar my-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "Where to put all automatically generated save/history-files.")
(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir))

;; Stop customize from writing to my init file
(setq custom-file "~/.emacs.d/custom.el")

;; Theme
;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-solarized-dark t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; Toggle betweem light and dark themes
(defun my-toggle-theme ()
  (interactive)
  (cond
   ((eq (car custom-enabled-themes) 'doom-solarized-dark)
    (disable-theme 'doom-solarized-dark)
    (load-theme 'doom-solarized-light t))
   ((eq (car custom-enabled-themes) 'doom-solarized-light)
    (disable-theme 'doom-solarized-light)
    (load-theme 'doom-solarized-dark t))
   ))

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
  :ensure t
  :init (doom-modeline-mode 1)
  :custom-face
  (mode-line ((t (:family "Hack" :height 130))))
  (mode-line-active ((t (:family "Hack" :height 130))))
  (mode-line-inactive ((t (:family "Hack" :height 130))))
  )

(use-package diminish :ensure t)

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

;; mondays are the first day of my week (for M-x calendar)
(setq-default calendar-week-start-day 1)

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
  :config
  (setq smex-save-to-file (expand-file-name "smex" my-savefile-dir)))

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
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      )

;; Mouse yank inserts at point, not at cursor
(setq mouse-yank-at-point t)

;; UTF-8 all the way
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; required for non-English keys (like dead acute) to work
;; http://osdir.com/ml/help-gnu-emacs-gnu/2009-05/msg00170.hotel
(require 'iso-transl)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;; use directory name in buffer names of files with the same name
(use-package uniquify
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

;; smarter kill-ring navigation
(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

;; automatically save buffers associated with files on buffer and window switch
;; https://github.com/bbatsov/super-save
(use-package super-save
  :diminish
  :config (super-save-mode +1)
)

;; TRAMP is awesome
;; https://www.gnu.org/software/tramp/
(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(set-default 'imenu-auto-rescan t)

;; enable set goal column (C-x C-n)
(put 'set-goal-column 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; bookmarks
(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" my-savefile-dir)
        bookmark-save-flag 1))

;; avy allows us to effectively navigate to visible things
(use-package avy
  :config (setq avy-background t
                avy-style 'at-full))

;; highlight results in search and replace commands
;; https://github.com/emacsorphanage/anzu
(use-package anzu
  :diminish
  :config (global-anzu-mode))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; it's 2020 already
(setq-default fill-column my-line-length)

;; progressively expand region around cursor
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t)

;; automatically clean up whitespace on save only on initially clean buffers
;; disable by setting whitespace-cleanup-mode to nil in dir or local variables
(use-package whitespace-cleanup-mode
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
(use-package crux)

;; Move line or region up and down
;; https://github.com/emacsfodder/move-text
(use-package move-text)

;; undo-tree is great to never lose an edit step
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :diminish
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; show uncommitted changes in the gutter
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
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
  :config
  (add-hook 'after-init-hook 'which-key-mode))

;; better Emacs help
;; https://github.com/Wilfred/helpful
(use-package helpful
  :after (counsel)
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

;;;;;;;;;;;;;;;;;
;;; eyebrowse ;;;
;;;;;;;;;;;;;;;;;

;; manage window configurations
;; https://depp.brause.cc/eyebrowse/
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))

;;;;;;;;;;;;;;;;;;;;;;;
;;; dired / dirvish ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package dirvish
  :ensure t
  :bind
  (("C-x C-j" . dired-jump)
   :map dirvish-mode-map  ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)
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
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ivy, counsel, swiper ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://oremacs.com/swiper/
(use-package ivy
  :diminish
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ;; show more candidates
        ivy-height 20
        ;; don't start filtering with ^
        ivy-initial-inputs-alist nil
        ;; case-insensitive search when running counsel-git-log
        counsel-git-log-cmd "GIT_PAGER=cat git log --no-color -i --grep '%s'"
        ;; show file at point as candidate in counsel-find-file
        counsel-find-file-at-point t
        ))

;;;;;;;;;;;
;;; git ;;;
;;;;;;;;;;;

;; https://magit.vc/
(use-package magit
  :if (executable-find "git")
  :ensure t
  :config
  (setq
   magit-wip-after-apply-mode' t
   magit-wip-after-save-mode' t
   magit-wip-before-change-mode' t
   ; path to my root code dir, so I can do C-x g from anywhere
   magit-repository-directories '(("~/Code" . 2))
   ; create a local tracking branch when visiting a remote branch
   magit-visit-ref-create t
   ; don't ask for confirmation when pushing branches
   magit-push-always-verify nil
   ; put history.el in the custom savefile dir
   transient-history-file (expand-file-name "transient-history.el" my-savefile-dir)
   )
  ;; enable magit-clean
  (put 'magit-clean 'disabled nil)
  )

;; https://magit.vc/manual/forge/
(use-package forge
  :after magit
  :config
  (setq forge-database-file
        (expand-file-name "forge-database.sqlite" my-savefile-dir))
  )

;;;;;;;;;;;;;;;;
;;; projects ;;;
;;;;;;;;;;;;;;;;

;; https://github.com/bbatsov/projectile
(use-package projectile
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" my-savefile-dir)
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-savefile-dir)
        ;; open a dirvish buffer when switching projects
        projectile-switch-project-action 'dirvish
        )
  (projectile-mode t))

;;;;;;;;;;;;;;;;;;;;
;;; autocomplete ;;;
;;;;;;;;;;;;;;;;;;;;

;; https://company-mode.github.io/
(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
              ([tab] . 'company-indent-or-complete-common)
              ("TAB" . 'company-indent-or-complete-common)
              )
  :config
  (setq ;; bigger popup window
        company-tooltip-limit 20
        ;; wait until at least one character before autocompleting
        company-minimum-prefix-length 1
        ;; shorter delay before autocompletion popup
        company-idle-delay 0.3
        ;; removes annoying blinking
        company-echo-delay 0
        ;; show quick-access numbers
        company-show-numbers t
        ))

;; https://github.com/tumashu/company-posframe
(use-package company-posframe
  :diminish
  :config
  (setq company-posframe-quickhelp-delay 0.1)
  (company-posframe-mode t)
  )

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

;;;;;;;;;;;;;;;;;;;;
;;; syntax check ;;;
;;;;;;;;;;;;;;;;;;;;

;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :hook ((after-init . global-flycheck-mode)
         (lsp-mode . flycheck-mode)
         (flycheck-mode . use-eslint-from-node-modules))
  :config
  (setq-default flycheck-highlighting-mode 'sexps)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info)
  ;; Use global flake8 configuration for Python code linting
  (setq flycheck-flake8rc "~/.config/.flake8")
  )

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

;; use hippie-expand instead of dabbrev
(define-key my-mode-map (kbd "M-/") 'hippie-expand)

;; I hate minimize
(define-key my-mode-map (kbd "C-z") 'ignore)
(define-key my-mode-map (kbd "C-x C-z") 'ignore)

;; replace buffer-menu with ibuffer
(define-key my-mode-map (kbd "C-x C-b") 'ibuffer)

;; quicker window splitting
(define-key my-mode-map (kbd "M-1") 'delete-other-windows) ; was digit-argument
(define-key my-mode-map (kbd "M-2") 'split-window-vertically) ; was digit-argument
(define-key my-mode-map (kbd "M-3") (lambda ()
                                      (interactive)
                                      (funcall 'split-window-horizontally
                                               (+ fill-column 4)))) ; was digit-argument
(define-key my-mode-map (kbd "M-0") 'delete-window) ; was digit-argument
(define-key my-mode-map (kbd "M-s") 'other-window) ; was center-line

;; quick access to calculator
(define-key my-mode-map (kbd "C-x c") 'calc)
(define-key my-mode-map (kbd "C-x C-c") 'calc) ; was save-buffers-kill-terminal
(define-key my-mode-map (kbd "C-x C") 'full-calc)

;; whitespace cleanup
(define-key my-mode-map (kbd "C-c M-w") 'whitespace-cleanup)

;; expand region
(define-key my-mode-map (kbd "C-=") 'er/expand-region)

;; align code
(define-key my-mode-map (kbd "C-x \\") 'align-regexp)

;; crux
(define-key my-mode-map (kbd "C-c o") 'crux-open-with)
(define-key my-mode-map (kbd "C-a") 'crux-move-beginning-of-line)
(define-key my-mode-map (kbd "C-c n") 'crux-cleanup-buffer-or-region)
(define-key my-mode-map (kbd "C-c f")  'crux-recentf-find-file)
(define-key my-mode-map (kbd "C-M-z") 'crux-indent-defun)
(define-key my-mode-map (kbd "C-c u") 'crux-view-url)
(define-key my-mode-map (kbd "C-c e") 'crux-eval-and-replace)
(define-key my-mode-map (kbd "C-c s") 'crux-swap-windows)
(define-key my-mode-map (kbd "C-c D") 'crux-delete-file-and-buffer)
(define-key my-mode-map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(define-key my-mode-map (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
(define-key my-mode-map (kbd "C-c r") 'crux-rename-buffer-and-file)
(define-key my-mode-map (kbd "C-c t") 'crux-visit-term-buffer)
(define-key my-mode-map (kbd "C-c k") 'crux-kill-other-buffers)
(define-key my-mode-map (kbd "C-c I") 'crux-find-user-init-file)
(define-key my-mode-map (kbd "C-c S") 'crux-find-shell-init-file)
(define-key my-mode-map (kbd "s-k") 'crux-kill-whole-line)
(define-key my-mode-map [(shift return)] 'crux-smart-open-line)
(define-key my-mode-map [(control shift return)] 'crux-smart-open-line-above)
(define-key my-mode-map [(meta shift up)]  'move-text-up)
(define-key my-mode-map [(meta shift down)]  'move-text-down)
(define-key my-mode-map [remap kill-whole-line] 'crux-kill-whole-line)

;; browse-kill-ring
(define-key my-mode-map (kbd "s-y") 'browse-kill-ring)
(define-key my-mode-map (kbd "C-x C-k") 'browse-kill-ring)

;; anzu
(define-key my-mode-map (kbd "M-%") 'anzu-query-replace)
(define-key my-mode-map (kbd "C-M-%") 'anzu-query-replace-regexp)

;; projectile
(define-key my-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key my-mode-map (kbd "s-p") 'projectile-command-map)

;; magit
(define-key my-mode-map (kbd "s-m m") 'magit-status)
(define-key my-mode-map (kbd "s-m s-m") 'magit-status)
(define-key my-mode-map (kbd "s-m j") 'magit-dispatch)
(define-key my-mode-map (kbd "s-m k") 'magit-file-dispatch)
(define-key my-mode-map (kbd "s-m l") 'magit-log-buffer-file)
(define-key my-mode-map (kbd "s-m b") 'magit-blame)

;; visual-regexp
(define-key my-mode-map (kbd "s-v r") 'vr/replace)
(define-key my-mode-map (kbd "s-v q") 'vr/query-replace)
(define-key my-mode-map (kbd "C-M-r") 'vr/isearch-backward)
(define-key my-mode-map (kbd "C-M-s") 'vr/isearch-forward)

;; Global org-mode keybindings
(define-key my-mode-map (kbd "C-c c") 'org-capture)
(define-key my-mode-map (kbd "C-c l") 'org-store-link)
(define-key my-mode-map (kbd "C-c a") 'org-agenda)
(define-key my-mode-map (kbd "C-c b") 'org-switchb)
(define-key my-mode-map (kbd "C-c C-x C-j") 'org-clock-goto)

;; ivy, counsel, swiper
(define-key my-mode-map (kbd "M-x") 'counsel-M-x)
(define-key my-mode-map (kbd "M-y") 'counsel-yank-pop)
(define-key my-mode-map (kbd "C-c g") 'counsel-git)
(define-key my-mode-map (kbd "C-c j") 'counsel-git-grep)
(define-key my-mode-map (kbd "C-c v") 'ivy-push-view)
(define-key my-mode-map (kbd "C-c V") 'ivy-pop-view)
(define-key my-mode-map (kbd "C-c C-r") 'ivy-resume)
(define-key my-mode-map (kbd "C-c C-s") 'ivy-yasnippet)
(define-key my-mode-map (kbd "C-c L") 'counsel-git-log)
(define-key my-mode-map (kbd "C-c k") 'counsel-rg)
(define-key my-mode-map (kbd "C-c j") 'counsel-org-goto)
(define-key my-mode-map (kbd "C-c C-j") 'counsel-org-goto-all)
(define-key my-mode-map (kbd "C-h f") 'counsel-describe-function)
(define-key my-mode-map (kbd "C-h v") 'counsel-describe-variable)
(define-key my-mode-map (kbd "C-h l") 'counsel-find-library)
(define-key my-mode-map (kbd "C-h S") 'counsel-info-lookup-symbol)
(define-key my-mode-map (kbd "C-h u") 'counsel-unicode-char)
(define-key my-mode-map (kbd "C-x l") 'counsel-locate)
(define-key my-mode-map (kbd "C-x C-f") 'counsel-find-file)
(define-key my-mode-map (kbd "C-r") 'swiper-backward)
(define-key my-mode-map (kbd "C-s") 'swiper)

;; helpful
(define-key my-mode-map (kbd "C-h f") 'helpful-callable)  ; includes macros, default describe-function
(define-key my-mode-map (kbd "C-h F") 'helpful-function)  ; excludes macros, default Info-goto-emacs-command-node
(define-key my-mode-map (kbd "C-h v") 'helpful-variable)  ; default counsel-describe-variable
(define-key my-mode-map (kbd "C-h k") 'helpful-key)       ; default describe-key
(define-key my-mode-map (kbd "C-h C") 'helpful-command)   ; default describe-coding-system
(define-key my-mode-map (kbd "C-h .") 'helpful-at-point)  ; default display-local-help

;; discover-my-major
(define-key my-mode-map (kbd "C-h C-m") 'discover-my-major)

;; Docker
(define-key my-mode-map (kbd "s-D") 'docker)

;; git-timemachine
(define-key my-mode-map (kbd "s-m t") 'git-timemachine)

;; Guix keybindings
(define-key my-mode-map (kbd "s-g") 'guix)

;; Switch light/dark theme
(define-key my-mode-map [f5] 'my-toggle-theme)

;;;;;;;;;;;;;;;;;
;;; keychords ;;;
;;;;;;;;;;;;;;;;;

;; trigger commands by pressing keys in quick succession
;; https://github.com/emacsorphanage/key-chord
(use-package key-chord
  :config
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line)
  (key-chord-define-global "jk" 'avy-goto-char)
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  (key-chord-define-global "qq" 'dirvish-dwim)
  (key-chord-define-global "qs" 'dirvish-side)
  (key-chord-define-global "uu" 'undo-tree-visualize)
  (key-chord-define-global "xx" 'execute-extended-command)
  (key-chord-define-global "yy" 'browse-kill-ring)
  (key-chord-mode +1)
  )

;;;;;;;;;;;;;;;;;;;
;;; smartparens ;;;
;;;;;;;;;;;;;;;;;;;

;; smart pairing for all programming modes
;; https://smartparens.readthedocs.io/en/latest/
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  :init (show-smartparens-global-mode +1)
)

;;;;;;;;;;;
;;; LSP ;;;
;;;;;;;;;;;

;; https://emacs-lsp.github.io/lsp-mode/
;; https://github.com/emacs-lsp/lsp-mode/
(use-package lsp-mode
  :hook ((js2-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-RET" . 'lsp-find-definition)
              ("M-?" . 'lsp-find-references)
              )
  :init (setq lsp-keymap-prefix "s-l")
  :config
  (setq lsp-headerline-breadcrumb-enable t
        ;; lock files will kill `npm start'
        create-lockfiles nil
        lsp-idle-delay 0.5
        lsp-eldoc-render-all t
        ;; set lsp-log-io to t to log LSP messages to *lsp-log* buffer
        lsp-log-io nil
        ;; Rust settings
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil
        )
                                        ; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (lsp-register-custom-settings
   '(("pylsp.plugins.pyls_mypy.enabled" t t)
     ("pylsp.plugins.pyls_mypy.live_mode" nil t)
     ("pylsp.plugins.pyls_black.enabled" t t)
     ("pylsp.plugins.pyls_isort.enabled" t t))))

;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
              ("M-j" . lsp-ui-imenu))
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-doc-position 'top
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-max-height 30
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-webkit nil
        ))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;;;;;;;;;;;;;;;;;;
;;; Javascript ;;;
;;;;;;;;;;;;;;;;;;

;; Find and use a specific executable from node_modules.
;; This is more secure than using add-node-modules-path, which will add ALL npm
;; binaries to path, and potentially introduce security holes
;; borrowed from tko's spacemacs:
;; https://github.com/tko/spacemacs/commit/1e66dfc0138c6a5337d3d9fb89466bb6dbe5573a#diff-4327bf953a571a99da1c8fcb473d516c50ebd83294a91bbf15f93280eccd948d
(defun my-node-executable-find (command &rest extra-modules)
  "Find COMMAND in node_modules and return its absolute path.
This function searches directories \"node_modules/.bin\",
\"node_modules/MODULE/node_modules/.bin\" for each extra module
in EXTRA-MODULES, and the directories searched by `executable-find'."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (node_modules (expand-file-name "node_modules" root))
         (bindirs (nconc
                   (list
                    ;; node_modules/.bin/{command}
                    ".bin"
                    ;; node_modules/{command}/bin/{command}
                    ;; (format "%s/bin" command)
                    )
                   ;; node_modules/{moduleN}/node_modules/.bin/{command}
                   (--map (f-join it "node_modules" ".bin") extra-modules))))
    (or
     (dolist (bindir bindirs)
       (let ((path (f-join node_modules bindir command)))
         (when (file-executable-p path) (cl-return path))))
     (executable-find command))))

;; Major mode for Javascript files
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (defun my-js-mode-overrides ()
    ;; electric-layout-mode doesn't play nice with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2")
    (js2-imenu-extras-mode +1)
    (subword-mode +1)
    )
  (add-hook 'js2-mode-hook (lambda () (run-hooks 'my-js-mode-overrides)))
  )

;; Refactor operations on top of JS2
;; https://github.com/js-emacs/js2-refactor.el
(use-package js2-refactor
  :hook (js2-mode-hook . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  )

;; Support for JSX files
;; https://github.com/felipeochoa/rjsx-mode
(use-package rjsx-mode
  :mode "\\.jsx"
  )

(use-package prettier-js
  :after (js2-mode rjsx-mode)
  :hook ((js2-mode . 'my-prettier-setup)
         (rjsx-mode . 'my-prettier-setup)))

(defun my-prettier-setup ()
  "Setup prettier."
  (interactive)
  (progn
    (let ((prettier (my-node-executable-find "prettier")))
      (when prettier (setq-local prettier-js-command prettier)))
    (setq prettier-js-args '(
                             "--trailing-comma" "es5"
                             "--single-quote" "true"
                             "--print-width" "100"
                             ))
    (prettier-js-mode)))

(defun use-eslint-from-node-modules ()
  "Use local eslint if available."
  (let ((eslint (my-node-executable-find "eslint")))
    (when eslint (setq-local flycheck-javascript-eslint-executable eslint))))

;;;;;;;;;;;;;;;;;;
;;; Typescript ;;;
;;;;;;;;;;;;;;;;;;

(defun use-tsserver-from-node-modules ()
  "Have tide use tsserver from local node_modules if available."
  (let ((tsserver (my-node-executable-find "tsserver")))
    (when tsserver (setq-local tide-tsserver-executable tsserver))))

(defun my-typescript-setup ()
  (interactive)
  (use-tsserver-from-node-modules)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (subword-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (let ((width 2))
    (setq-local typescript-indent-level width
                typescript-expr-indent-offset width
                indent-level width
                tab-width width))
  )

(defun my-typescript-web-mode-setup ()
  "Personal tide setup for .tsx files."
  (interactive)
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (my-typescript-setup)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . my-typescript-setup)
         (web-mode . my-typescript-web-mode-setup))
)

(use-package tide
  :after (company flycheck typescript-mode web-mode)
  :bind (:map typescript-mode-map
              ("C-c C-f" . tide-format))
  :commands (tide-setup)
  :init
  (with-eval-after-load 'flycheck
    ;; check .tsx files with typescript linter
    (flycheck-add-mode 'typescript-tslint 'web-mode))
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  )

;;;;;;;;;;;
;;; Web ;;;
;;;;;;;;;;;

;; https://web-mode.org/
(use-package web-mode
  :after (smartparens flycheck)
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
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        ;; make web-mode play nice with smartparens
        web-mode-enable-auto-pairing nil
        )
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")
    )
  ; TSX support
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (my-typescript-web-mode-setup))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;;;;;;;;;;;
;;; CSS ;;;
;;;;;;;;;;;

;; css-mode comes built-in with Emacs
(with-eval-after-load 'css-mode
  (setq css-indent-offset 2)
  (defun my-css-mode-defaults ()
    (rainbow-mode +1)
    (smartparens-mode +1)
    )
  (setq my-css-mode-hook 'my-css-mode-defaults)
  (add-hook 'css-mode-hook (lambda ()
                             (run-hooks 'my-css-mode-hook))))

;;;;;;;;;;;;;;
;;; Python ;;;
;;;;;;;;;;;;;;

;; activate Python virtualenvs
;; https://github.com/jorgenschaefer/pyvenv
(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

(defun my-python-config ()
  "My personal configuration for python-mode"
  (subword-mode +1)
  (pyvenv-mode +1)
  ; (python-docstring-mode +1)  ; not available in Guix yet
  ; (auto-virtualenv-set-virtualenv)  ; not available in Guix yet
  (lsp-deferred)
  )
(add-hook 'python-mode-hook #'my-python-config)

(defun lsp-python-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t))
(add-hook 'python-mode-hook #'lsp-python-install-save-hooks)

;;;;;;;;;;;;
;;; Rust ;;;
;;;;;;;;;;;;

(use-package rustic
  :ensure
  :mode "\\.rs\'"
  :bind (:map rustic-mode-map
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ;; ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  (add-hook 'rustic-mode-hook 'my-rustic-mode-hook))

(defun my-rustic-mode-hook ()
  ;; format code on save
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;;;;;;;;;;
;;; Go ;;;
;;;;;;;;;;

(use-package go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ("M-RET" . godef-jump)
              ("M-." . godef-jump)
              ("C-M-RET" . godef-jump-other-window)))

;;;;;;;;;;;;
;;; Lisp ;;;
;;;;;;;;;;;;

(defun my-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq my-lisp-coding-hook 'my-lisp-coding-defaults)

;;; Emacs Lisp

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
  :hook (emacs-lisp-mode . eldoc-mode))

(use-package rainbow-mode
  :diminish
  :hook (emacs-lisp-mode))

;;; Scheme

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

(use-package dockerfile-mode)

(use-package docker-compose-mode
  :mode "docker-compose.*\\.yml")

;;;;;;;;;;;;;;;;;
;;; org-mode  ;;;
;;;;;;;;;;;;;;;;;

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . (lambda ()
                      (org-indent-mode)
                      (variable-pitch-mode -1)
                      (display-line-numbers-mode -1)
                      ))
  :custom
  (org-directory (expand-file-name "~/Org"))
  ;; add all *.org files in the org-directory defined above
  ;; (setq org-agenda-files (list org-directory))
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
  ;; custom org-agenda views
  (org-agenda-custom-commands '(("r" tags "refile")))
  ;; hide org native markup (e.g. / / for italics, * * for bold, ~ ~ for code)
  (org-hide-emphasis-markers t)
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
   (ipython . t)
   ;;   emacs-ob-rust not in Guix yet
   ;;   (rust . t)
   (shell . t)
   ;;   emacs-ob-sql-mode not in Guix yet
   ;;   (sql-mode . t)
   ))

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
  :mode "\\.csv\'")

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
  :mode ("\\.toml\'"))

;;;;;;;;;;;;;;;;;;
;;; Prisma ORM ;;;
;;;;;;;;;;;;;;;;;;

(use-package prisma-mode
  :mode "\\.prisma\'")

;;;;;;;;;;;;;;;;
;;; Snippets ;;;
;;;;;;;;;;;;;;;;

;; load yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1)
  ;; term-mode does not play well with yasnippet
  (add-hook 'term-mode-hook (lambda ()
                              (yas-minor-mode -1))))

;;;;;;;;;;;;;;;
;;; Writing ;;;
;;;;;;;;;;;;;;;

;; detect opportunities to improve writing (passive voice, weasel words...)
;; https://github.com/bnbeckwith/writegood-mode
(use-package writegood-mode
  :hook (text-mode . writegood-mode)
  :bind (("C-c M-w" . 'writegood-mode)
         ("C-c M-g" . 'writegood-grade-level)
         ("C-c M-r" . 'writegood-reading-ease)))

;;;;;;;;;;;;;;
;;; Ebooks ;;;
;;;;;;;;;;;;;;

(use-package calibredb
  :defer t
  :bind ("C-c M-e" . 'calibredb-find-counsel)
  :init
  (setq sql-sqlite-program "sqlite3")
  :config
  (setq calibredb-root-dir "~/Calibre"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~Calibre"))))

(use-package nov-mode
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
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

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
  :hook (text-mode . olivetti-mode))

;; display emojis (and nice interactive picker)
;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :hook (after-init . global-emojify-mode)
  :bind ("C-c C-e" . 'emojify-insert-emoji))
