;; Are we running under Guix?
(setq guix-p (stringp (getenv "GUIX_PROFILE")))

(when guix-p
  ;; Disable package system if running under Guix
  (setq package-enable-at-startup nil))

;; Turn off mouse interface early in startup to avoid momentary display
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
