;; profile startup time (also uncomment end of init.el)
;; (profiler-start 'cpu)

;; Increase garbage collection threshold during startup, but once Emacs is finished
;; loading then set it at a reasonable level. Using large thresholds would lead to
;; stuttering/freezes when Emacs hit it as it's single-threaded.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer
             5 nil
             (lambda ()
               (setq gc-cons-threshold (* 32 1024 1024)
                     gc-cons-percentage 0.1)
               (garbage-collect)))))

;; Use .el files over .elc if they are newer
(when (boundp 'load-prefer-newer)
  (setq load-prefer-newer t))

;; make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq comp-deferred-compilation-deny-list '())
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

;; put native compilation files inside var/ (see no-littering in init.el)
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Fix TRAMP compatibility with fish shell
;; TRAMP and other Emacs internals need a POSIX shell
(setenv "SHELL" "/bin/bash")
(setq shell-file-name "/bin/bash")

;; read more from subprocesses
;; default is 4 KiB, it's already $YEAR so we can do more
(setq read-process-output-max (* 8 1024 1024))

;; native compilation settings
(when (fboundp 'native-comp-available-p)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive
   native-comp-async-report-warnings-errors nil))

;; package.el is disabled, straight.el is the package manager
(setq package-enable-at-startup nil)

;; straight.el: install packages as git clones of their own repositories, this makes it
;; easy to review changes, hack on them and contribute changes upstream
;; https://github.com/radian-software/straight.el

;; SHA-256 checksum of straight.el's install.el bootstrap script. The
;; download is verified before being evaluated; on mismatch init stops
;; here for manual review. If the change is legitimate (see
;; https://github.com/radian-software/straight.el/commits/develop/install.el)
;; update the hash with: curl -s <url> | sha256sum
(defconst my-straight-install-el-sha256
  "e29e07d52d16d4136971f0a822cb6a1a6e1e764a1cb9fe67cccbc7c048aba553")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      ;; verify the bootstrap script against its pinned checksum before
      ;; evaluating it
      (require 'url-http)
      (when url-http-end-of-headers
        (delete-region (point-min) url-http-end-of-headers)
        (delete-region (point-min)
                       (progn (skip-chars-forward " \t\n") (point))))
      (let ((checksum (secure-hash 'sha256 (current-buffer))))
        (unless (string= checksum my-straight-install-el-sha256)
          (error
           (concat
            "straight.el bootstrap checksum mismatch!\n"
            "Expected: %s\n"
            "Got:      %s\n"
            "Review the change at https://github.com/radian-software/straight.el/commits/develop/install.el\n"
            "and update my-straight-install-el-sha256 in early-init.el if legitimate")
           my-straight-install-el-sha256 checksum)))
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; scratch buffer mode. With its default of elisp it triggers modes meant for
;; programming, which delay startup.
(setq initial-major-mode 'text-mode)

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
