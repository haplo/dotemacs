# Fidel Ramos' GNU Emacs configuration

This is my very own Emacs configuration.
There are many like it, but this one's mine.

This configuration is meant and has only been tested with Emacs 30.2.

## Package management

Packages are managed with [straight.el](https://github.com/radian-software/straight.el),
which installs them as git clones of their own repositories (in `straight/repos/`),
instead of tarballs like package.el does. This makes it easy to hack on packages (`M-x
straight-visit-package`) and contribute changes upstream.

Packages that ship with Emacs are used as built-ins; they are registered with straight
via `:type built-in` so that package dependencies don't pull repository checkouts
anyway.

Upgrades are manual and reviewable:

1. `M-x straight-fetch-all` fetches all package remotes without changing anything.
2. After fetching, `my-straight-incoming-diffs` (`C-c v U`) opens automatically with
   one concatenated `diff-mode` buffer: for each checkout that is behind upstream, the
   incoming commits (with authors and dates) followed by the net patch that merging
   would apply, ready to be skimmed or fed to a reviewing agent.
3. `M-x my-gptel-review-malicious-code` (`C-c a R`) can be called on the diff buffer
   to have an AI review the diff for malicious code.
4. `M-x straight-merge-all` merges each package, but allows dropping into Magit with a
   recursive edit.
5. Restart Emacs, verify that everything works, then run `M-x straight-freeze-versions`
   and commit `straight/versions/default.el`. This lockfile pins every package to an
   exact commit; `M-x straight-thaw-versions` restores those revisions.

Security notes:

- The straight.el bootstrap script is verified against a pinned SHA-256 checksum; on
  mismatch startup stops for manual review (see `early-init.el`).
- The lockfile pins everything: package repositories, recipe repositories (MELPA, the
  ELPA mirrors) and straight.el itself. Nothing moves without an explicit merge and
  `straight-freeze-versions`. When committing an upgrade, review `git diff
  straight/versions/default.el` for a concise list of what moved.
- `M-x straight-get-recipe` shows where a package is actually fetched from. Recipe
  changes only take effect when the recipe repositories themselves are merged;
  spot-check with `git -C straight/repos/melpa diff HEAD..@{upstream} -- recipes/<pkg>`.

## Packages I use

This list might be outdated, you would do better by grepping [init.el](init.el) for `use-package` uses.

* [`ace-window`](https://github.com/abo-abo/ace-window): quickly move/split/swap/copy windows.
* [`agent-shell`](https://github.com/xenodium/agent-shell): frontend for agentic coding agents via ACP; runs opencode per project, each session in its own firejail sandbox.
* [`all-the-icons`](https://github.com/domtronn/all-the-icons.el): pretty icons.
* [`all-the-icons-ibuffer`](https://github.com/seagle0128/all-the-icons-ibuffer): pretty icons in *ibuffer*.
* [`all-the-icons-completion`](https://github.com/iyefrat/all-the-icons-completion): pretty icons in completion minibuffer.
* [`all-the-icons-dired`](https://github.com/wyuenho/all-the-icons-dired): pretty icons in *dired*.
* [`auto-dark`](https://github.com/LionyxML/auto-dark-emacs): follow OS light/dark theme.
* [`auto-compile`](https://github.com/emacscollective/auto-compile/): compile Elisp files on load and/or save.
* [`avy`](https://github.com/abo-abo/avy): quick jumps.
* [`batppuccin`](https://github.com/bbatsov/batppuccin-emacs): catppuccin-like theme, light/dark versions.
* [`calibredb`](https://github.com/chenyanming/calibredb.el): an Emacs interface to the Calibre DB.
* [`cape`](https://github.com/minad/cape): completion-at-point extensions (dabbrev, file and more).
* [`corfu`](https://github.com/minad/corfu/): completion-at-point popup. Like [company](https://company-mode.github.io/) but lighter.
* [`compile`](https://www.emacswiki.org/emacs/CompileCommand): built-in command to compile stuff.
* [`consult`](https://github.com/minad/consult): practical commands based on the Emacs completion function `completing-read`.
* [`consult-dir`](https://github.com/karthink/consult-dir): insert directory paths into the minibuffer prompt.
* [`consult-git-log-grep`](https://github.com/ghosty141/consult-git-log-grep): grep a repository's git history with consult.
* [`crux`](https://github.com/bbatsov/crux): collection of random utilities, originally bundled with [Prelude](https://github.com/bbatsov/prelude).
* [`csv-mode`](https://elpa.gnu.org/packages/csv-mode.html): major mode to edit CSV (Comma-Separated Values) files.
* [`diff-hl`](https://github.com/dgutov/diff-hl): show uncommitted changes.
* [`diminish`](https://github.com/myrjola/diminish.el): hide minor-modes from modeline.
* [`docker`](https://github.com/Silex/docker.el): manage Docker (and Podman) from Emacs.
* [`docker-compose-mode`](https://github.com/meqif/docker-compose-mode): major mode for docker-compose files.
* [`dockerfile-mode`](https://github.com/spotify/dockerfile-mode): major mode to edit Dockerfiles.
* [`doom-modeline`](https://seagle0128.github.io/doom-modeline/): a very nice modeline.
* [`easy-kill`](https://github.com/leoliu/easy-kill): mark/kill words, sexps, lines and more.
* [`editorconfig`](https://github.com/editorconfig/editorconfig-emacs): support for *.editorconfig* files.
* [`eglot`](https://joaotavora.github.io/eglot/): LSP client built-in since Emacs 29.
* [`eldoc`](https://elpa.gnu.org/packages/eldoc.html): display documentation about current function in echo area.
* [`embark`](https://github.com/oantolin/embark/): choose a command to run based on what is near point, both during a minibuffer completion session and in normal buffers.
* [`emojify`](https://github.com/iqbalansari/emacs-emojify): display emojis.
* [`envrc`](https://github.com/purcell/envrc): use [direnv](https://direnv.net/) to update Emacs environment when visiting files.
* [`exec-path-from-shell`](https://github.com/purcell/exec-path-from-shell): have Emacs inherit environment variables.
* [`expreg`](https://github.com/casouri/expreg): increase selected region by semantic units, with tree-sitter.
* [`fish-mode`](https://github.com/wwwjfy/emacs-fish): major mode for [fish](https://fishshell.com/) shell scripts.
* [`flymake`](https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html): Emacs built-in error checker. It integrates natively with Eglot.
* [`forge`](https://magit.vc/manual/forge/): Magit extension to work with code hosting sites (e.g. GitHub).
* [`git-timemachine`](https://codeberg.org/pidu/git-timemachine): browse previous revisions of any git-controlled file.
* [`ghostel`](https://github.com/dakra/ghostel): terminal emulator powered by libghostty-vt.
* [`gptel`](https://github.com/karthink/gptel): LLM chat client: chat buffers, send/rewrite text.
* [`helpful`](https://github.com/Wilfred/helpful): better help buffers.
* [`ibuffer`](https://www.emacswiki.org/emacs/IbufferMode): better buffer view. Like dired for buffers.
* [`ibuffer-projectile`](https://github.com/purcell/ibuffer-projectile): group buffers by project in *ibuffer* view.
* [`jinx`](https://github.com/minad/jinx): fast just-in-time spell checker (uses [enchant](https://rrthomas.github.io/enchant/)).
* [`key-chord`](https://github.com/emacsorphanage/key-chord): execute commands by pressing keys quickly.
* [`keycast`](https://github.com/tarsius/keycast/): show current command and its binding.
* [`kind-icon`](https://github.com/jdtsmith/kind-icon): icons for completion-at-point candidates.
* [`magit`](https://magit.vc/): the best Git interface there is.
* [`magit-delta`](https://github.com/dandavison/magit-delta): integrate [delta diff](https://github.com/dandavison/delta) with [Magit](https://magit.vc/).
* [`marginalia`](https://github.com/minad/marginalia): adds marginalia to the minibuffer completions.
* [`markdown-mode`](https://github.com/jrblevin/markdown-mode): major mode to edit Markdown files.
* [`move-text`](https://github.com/emacsfodder/move-text): move lines or regions up and down.
* [`multiple-cursors`](https://github.com/magnars/multiple-cursors.el): what it says in the label, allows editing multiple lines simultaneously.
* [`no-littering`](https://github.com/emacscollective/no-littering): keep Emacs config directory clean.
* [`nov-el`](https://depp.brause.cc/nov.el/): major mode for reading EPUB files in Emacs.
* [`orderless`](https://github.com/oantolin/orderless): completion style that divides the pattern into space-separated components, and matches candidates that match all of the components in any order.
* [`org`](https://orgmode.org/): the most powerful note taking and planning software ever created.
* [`orgit`](https://github.com/magit/orgit/): link to Magit buffers from Org-mode.
* [`orgit-forge`](https://github.com/magit/orgit-forge/): link to Forge buffers from Org-mode.
* [`popper`](https://github.com/karthink/popper): tame the flood of ephemeral windows Emacs produces.
* [`projectile`](https://github.com/bbatsov/projectile): project management.
* [`rainbow-delimiters`](https://github.com/Fanael/rainbow-delimiters): color delimiters such as parentheses or braces as they nest.
* [`rainbow-mode`](https://elpa.gnu.org/packages/rainbow-mode.html): color strings that describe colors.
* [`rustic`](https://github.com/brotzeit/rustic): major mode for [Rust](https://www.rust-lang.org/) coding.
* [`saveplace`](https://www.emacswiki.org/emacs/SavePlace): built-in to remember last position on each open file.
* [`super-save`](https://github.com/bbatsov/super-save): automatically save buffers on certain events.
* [`systemd`](https://github.com/holomorph/systemd-mode): major mode for editing systemd files.
* [`tab-bar`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html): Emacs built-in tab bar.
* [`tabspaces`](https://codeberg.org/mclear-tools/tabspaces): lightweight workspaces in tabs using Emacs built-in tab-bar and project.
* [`tramp`](https://www.gnu.org/software/tramp/): remote file editing, beyond cool.
* [`treesit-auto`](https://github.com/renzmann/treesit-auto): automatically install and use [tree-sitter grammars](https://tree-sitter.github.io/tree-sitter/).
* [`unfill`](https://github.com/purcell/unfill): the inverse of Emacs' `fill-paragraph` and `fill-region`.
* [`valign`](https://github.com/casouri/valign): visual alignment for tables.
* [`vundo`](https://github.com/casouri/vundo): visualize and navigate the undo history as a tree, never lose an edit.
* [`vertico`](https://github.com/minad/vertico): performant and minimalistic vertical completion UI based on the default completion system.
* [`volatile-highlights`](https://github.com/k-talo/volatile-highlights.el): visual feedback for some operations.
* [`wgrep`](https://github.com/mhayashi1120/Emacs-wgrep): edit a grep buffer and apply those changes to the file buffer.
* [`which-key`](https://github.com/justbur/emacs-which-key): show all possible key combinations.
* [`whitespace-cleanup-mode`](https://github.com/purcell/whitespace-cleanup-mode): minor mode to clean up a buffer's whitespace on save.
* [`writegood-mode`](https://github.com/bnbeckwith/writegood-mode): minor mode to aid in finding common writing problems in English.
* [`yaml-mode`](https://github.com/yoshiki/yaml-mode): major mode for YAML files.
* [`yasnippet`](https://joaotavora.github.io/yasnippet/): snippet extension.
* [`yasnippet-snippets`](https://github.com/AndreaCrotti/yasnippet-snippets/): snippet collection for yasnippet.
* [`zoom-window`](https://github.com/emacsorphanage/zoom-window): toggle windows to display full-frame.

## Inspiration

I have drawn inspiration from many other people's configurations of Emacs.
I thank them all for sharing, usually with much better comments than mine.
Here are the ones I have studied more in depth:

- [Bozhidar Batsov's Emacs Prelude](https://github.com/bbatsov/prelude).
- [Jamie Collinson's Emacs configuration](https://jamiecollinson.com/blog/my-emacs-config/).
- [Sacha Chua's Emacs configuration](https://pages.sachachua.com/.emacs.d/Sacha.html).
- [Zoltán Király’s Emacs configuration](https://github.com/zoliky/dotemacs).
- [FrostyX's Emacs configuration](https://github.com/FrostyX/dotfiles/blob/master/.emacs.d/frostyx.org).
- [Emacs Siren](https://github.com/jimeh/.emacs.d).
- [Crandel](https://github.com/Crandel/home/blob/master/.config/emacs/early-init.el)
- [Serghei Iakovlev's Emacs configuration](https://github.com/sergeyklay/.emacs.d)
- [Patrick M. Niedzielski](https://github.com/pniedzielski/emacs.d)
