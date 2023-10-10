;; Are we running under Guix?
(setq guix-p (stringp (getenv "GUIX_PROFILE")))

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

;; increase garbage collection threshold for faster startup
;; default is 800 KiB, it's already $YEAR so we can do more
(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.2)

;; read more from subprocesses
;; default is 4 KiB, it's already $YEAR so we can do more
(setq read-process-output-max (* 8 1024 1024))

;; native compilation settings
(when (fboundp 'native-comp-available-p)
  (setq ;; Silence compiler warnings as they can be pretty disruptive
        native-comp-async-report-warnings-errors nil))

;; don't resize frame as font, menu, tool bar... change
(setq frame-inhibit-implied-resize t)

;; Default font, https://sourcefoundry.org/hack/
(set-face-attribute 'default t :font "Hack 13")

;; Turn off mouse interface early in startup to avoid momentary display
(setq default-frame-alist
        '(
          (alpha 100 100)
          (cursor-type              . box)
          (font                     . "Hack 13")
          (menu-bar-lines           . 0)
          (tool-bar-lines           . 0)
          (vertical-scroll-bars     . right)
          (undecorated              . t)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
