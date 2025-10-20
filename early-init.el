;; Are we running under Guix?
(setq guix-p (stringp (getenv "GUIX_PROFILE")))

;; Use .el files over .elc if they are newer
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

;; make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq comp-deferred-compilation-deny-list '())
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

;; Fix TRAMP compatibility with fish shell
;; TRAMP and other Emacs internals need a POSIX shell
(setenv "SHELL" "/bin/bash")
(setq shell-file-name "/bin/bash")

;; Increase garbage collection threshold during startup, but once Emacs is finished
;; loading then set it at a reasonable level. Using large thresholds would lead to
;; stuttering/freezes when Emacs hit it as it's single-threaded.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16mb
                  gc-cons-percentage 0.1)))

;; read more from subprocesses
;; default is 4 KiB, it's already $YEAR so we can do more
(setq read-process-output-max (* 8 1024 1024))

;; native compilation settings
(when (fboundp 'native-comp-available-p)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive
   native-comp-async-report-warnings-errors nil))

;; It's nice to use packages only on Guix, but it's too restrictive sometimes
;; (when guix-p
;;   ;; disable package system if running under guix
;;   (setq package-enable-at-startup nil)
;;   (setq package-archives nil))
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("gnu"          . 30)
        ("nongnu"       . 20)
        ("melpa-stable" . 10)
        ("melpa"        . 5)
        ("gnu-devel"    . 0)))
(package-initialize)

;; don't resize frame as font, menu, tool bar... change
(setq frame-inhibit-implied-resize t)

;; Default font, https://sourcefoundry.org/hack/
(set-face-attribute 'default t :font "Hack 12")

;; Set up default frame properties early on to avoid UI flicker
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Frame-Parameters.html
(setq default-frame-alist
      '(
        (alpha-background         . 97)
        (cursor-type              . box)
        (font                     . "Hack 12")
        (menu-bar-lines           . 0)
        (tool-bar-lines           . 0)
        (vertical-scroll-bars     . nil)
        (undecorated              . t)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
