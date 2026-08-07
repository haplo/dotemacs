# AGENTS.md

Personal GNU Emacs 30.2 configuration. Main files: `init.el`, `early-init.el`.
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

## Style

- Keep declarations organized by sections (`;;; section ;;;` headers). Put new
  configuration in the matching section; create a new section for a new topic.
- Prefer public package APIs (no double dash `--` in the name). Avoid
  referencing other packages' private (`--`) variables and functions — they can
  change without notice. If a private API is unavoidable, say so in a comment.
- Give every `use-package` block a one-line description comment with the
  upstream URL, matching the existing blocks.
- Register packages that ship with Emacs as built-ins via `:type built-in`
  instead of installing them from a repository.
- Keep state files out of the config root with
  `no-littering-expand-etc-file-name` / `no-littering-expand-var-file-name`.
- Wrap comments and docstrings at 88 columns (`my-line-length`).
- After editing `init.el`, syntax-check in batch:
  `emacs -Q --batch --eval "(with-temp-buffer (emacs-lisp-mode) (insert-file-contents \"init.el\") (check-parens))"`

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
  Example: loading agent-shell in batch pulled the built-in transient 0.7.2.2
  instead of the pinned git checkout and died on `transient--set-layout`.
  Activate every package the load depends on in the batch eval
  (`(straight-use-package 'transient)` etc.) before requiring or compiling.
- **`:bind` with an unloaded `:map` defers to the BLOCK's package, not the
  map's owner.** use-package emits `(bind-keys :package NAME :map MODE-MAP ...)`
  and bind-key wraps it as `(if (boundp 'MODE-MAP) bind (eval-after-load 'NAME
  bind))`. If the keymap belongs to another package, the binding never lands
  (or errors in `after-load-functions` when NAME loads first). Put each `:bind
  (:map ...)` in the use-package block of the package that owns the keymap.
  Raw `bind-keys` without `:package` has no such guard and errors
  `void-variable` on unbound maps, so test through full use-package forms.
