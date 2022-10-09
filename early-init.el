;; Are we running under Guix?
(setq guix-p (stringp (getenv "GUIX_PROFILE")))

(when guix-p
  ;; Disable package system if running under Guix
  (setq package-enable-at-startup nil))
