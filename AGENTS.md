# AGENTS.md

Personal GNU Emacs 31.1 configuration. Main files: `init.el`, `early-init.el`.
Packages are managed with straight.el as git clones (`straight/repos/`), pinned
in the lockfile `straight/versions/default.el`. See README.md for the upgrade
workflow.

## Hard rules for agents

1. **NEVER exfiltrate sensitive data.** Do not read, print, copy, or transmit
   secrets: API keys, tokens, passwords, `~/.authinfo*`, GPG/SSH material,
   KWallet items. Do not run network commands that send any local data off this
   machine. The Venice API key lives in KWallet — reference it, never retrieve
   it.
2. **No git mutations.** Never commit, push, stage, tag, reset, rebase, or
   amend. Show diffs and leave all git operations to the user.
3. **Preserve preexisting worktree changes.** Before editing, check
   `git status` and `git diff --cached` to learn what changes already exist
   that are not yours. Never stage, overwrite, revert, or delete them; never
   run `git checkout --`, `git restore`, `git clean`, or similar. Make only
   targeted edits to the files you were asked to change.
4. **No live or non-batch Emacs processes.** Never run `emacsclient` (not
   even `--eval` for read-only inspection) and never start a non-batch
   Emacs (`emacs --daemon`, GUI). If the server socket is unreachable,
   `ALTERNATE_EDITOR=""` makes emacsclient spawn a full daemon whose
   startup and `kill-emacs-hook` run the entire config. Even a successful
   `--eval` executes code in the user's stateful session. Inspect state
   from files, use `emacs -Q --batch` for checks, and ask the user to run
   any in-session evaluation themselves.

## Style

- Keep declarations organized by sections (`;;; section ;;;` headers). Put new
  configuration in the matching section; create a new section for a new topic.
- Prefer public package APIs (no double dash `--` in the name). Avoid
  referencing other packages' private (`--`) variables and functions — they can
  change without notice. If a private API is unavoidable, say so in a comment.
- Favor built-in behavior instead of custom functions whenever possible.
- Give every `use-package` block a one-line description comment with the
  upstream URL, matching the existing blocks.
- Register packages that ship with Emacs as built-ins via `:type built-in`
  instead of installing them from a repository.
- Prefer use-package style declarations. E.g. `:bind` instead of `keymap-set`, `:hook`
  instead of `add-hook`, `:custom` better than using `setq` in `:config`.
- Keep state files out of the config root with
  `no-littering-expand-etc-file-name` / `no-littering-expand-var-file-name`.
- Wrap comments and docstrings at 88 columns (`my-line-length`).
- After editing `init.el`, syntax-check in batch:
  `emacs -Q --batch --eval "(with-temp-buffer (emacs-lisp-mode) (insert-file-contents \"init.el\") (check-parens))"`
- Propose fixes to third-party packages when identifying bugs, user will either
  fork or submit the fix upstream.
- Update the list of currently used packages in `README.md` as they get added or
  removed.

## Learnings

Operational findings discovered while working on this config. Append new ones
below (newest last).

- **`straight-freeze-versions` is interactive-only.** It pins only the packages
  straight knows about in the current session. Running it in batch mode
  (`emacs --batch -l early-init.el --eval ...`) rewrites
  `straight/versions/default.el` with just that session's dependency chain,
  silently dropping all other lockfile entries. When adding new packages instruct
  the user to regenerate the lockfile with `M-x straight-freeze-versions` or edit
  the lockfile by hand: it's an alphabetical alist of `("name" . "commit")` entries.
- **Batch Emacs loads can pick built-in copies of straight packages.** A batch
  session bootstrapped with only `early-init.el` activates just the packages
  explicitly requested; everything else resolves to Emacs' built-in copies.
  Example (Emacs 30 era, when transient was a pinned git checkout): loading
  agent-shell in batch pulled the built-in transient 0.7.2.2 and died on
  `transient--set-layout`.  Transient is built-in-only since Emacs 31.1, but
  the hazard applies to any straight package that also ships with Emacs.
  Activate every package the load depends on in the batch eval
  (`(straight-use-package 'magit)` etc.) before requiring or compiling.
- **`:bind` with an unloaded `:map` defers to the BLOCK's package, not the
  map's owner.** use-package emits `(bind-keys :package NAME :map MODE-MAP ...)`
  and bind-key wraps it as `(if (boundp 'MODE-MAP) bind (eval-after-load 'NAME
  bind))`. If the keymap belongs to another package, the binding never lands
  (or errors in `after-load-functions` when NAME loads first). Put each `:bind
  (:map ...)` in the use-package block of the package that owns the keymap.
  Raw `bind-keys` without `:package` has no such guard and errors
  `void-variable` on unbound maps, so test through full use-package forms.
- **Dependency resolution can clone Emacs core packages from ELPA.** A package
  that lists a core package in `Package-Requires` (e.g. tabspaces requiring
  `project`, which requires `xref`) makes straight pull the ELPA-mirror clone
  before init's own `:type built-in` declaration for it is even reached; the
  clone then shadows the Emacs-shipped copy on every load. early-init.el sets
  `straight-recipe-overrides` ((project :type built-in), (xref :type built-in))
   right after the bootstrap to prevent this. When adding new packages, watch
   the build log for unexpected "Cloning <core-package>" lines and extend that
   override list instead of letting the clone live.
- **use-package `:custom` value forms are evaluated at package load time, not
  when init.el runs.** For a deferred package the form is stored unevaluated
  in the `use-package` custom theme and evaluated when the `defcustom`
  executes (verified by macroexpansion on Emacs 30.2). So `:custom` must not
  read transient init-time state — e.g. a `command-line-args` flag that an
  `:init` form later removes will read as absent. Stash such state in a
  variable in `:init` and reference the variable from `:custom`.
- **Directories bypass `find-file-hook`; `find-file`/`dired` display via
  `pop-to-buffer-same-window`.** `find-file-noselect` hands directories to
  `find-directory-functions` (`dired-noselect`) before any visit logic, and
  the `pop-to-buffer-same-window` display path never runs `switch-to-buffer`
  advice. Workspace routing therefore covers dired with a `:before` advice on
  `dired-noselect` (single choke point, new and existing buffers; the
  advice-add lives in dired's `:config` because `dired-noselect` is
  autoloaded and advising it eagerly would load dired at startup) and magit
  status with `magit-status-mode-hook` (magit calls the mode function before
  `magit-display-buffer`, for new and existing status buffers alike).
 - **Batch tests must register core packages as `:type built-in` too.** In a
   batch test, `(straight-use-package 'tramp)` (without `:type built-in`, as
   init.el declares it) silently cloned and built the ELPA-mirror tramp into
   `straight/repos/tramp` — the exact hazard of the ELPA-clone learning above.
   Mirror init.el's `(dolist (pkg '(seq let-alist xref jsonrpc use-package))
   (straight-use-package (list pkg :type 'built-in)))` in test scripts, and
   use `(list 'tramp :type 'built-in)` for tramp.
 - **`:after` defers `:hook` registration too.** use-package wraps the whole
   block body (including `add-hook` calls) in `eval-after-load`, so a hook on
   an eagerly-loaded package (e.g. auto-dark, loaded with `:demand`) placed
   in a block gated on a lazy one (e.g. magit) is only registered once the
   lazy package loads — silently missing earlier events, like auto-dark's
   startup theme set.  Pattern: make the hook function a no-op until its
   package is loaded (`boundp` guard on the package's defcustom) and call it
   from `:config` to sync with the current state at load time, reading
   public state (`frame-background-mode`, which auto-dark sets on every
   switch before running its hooks) instead of assuming an initial value.
   See `my-magit-delta-sync-appearance`.
- **`:hook` on a symbol removed upstream manufactures a bogus autoload.**
  When a deferred use-package block has `:hook (some-mode . some-fn)` and
  `some-fn` is no longer defined (nor autoloaded) by its package, use-package
  emits `(unless (fboundp 'some-fn) (autoload #'some-fn "BLOCKS-PACKAGE" nil t))`.
  The hook then errors at run time with "Autoloading file X failed to define
  function some-fn" — pointing at the block's package, not the symbol's real
  owner. Example: embark-consult's `:hook (embark-collect-mode .
  consult-preview-at-point-mode)` broke every embark-export falling back to
  `embark-collect` (consult-imenu, consult-outline, symbol exports...) after
  consult 3.7 removed `consult-preview-at-point-mode` (obsoleted 2025-12;
  collect-buffer previews are enabled automatically by embark-consult itself).
