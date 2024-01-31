# Fidel Ramos' GNU Emacs configuration

This is my very own Emacs configuration.
There are many like it, but this one's mine.

This configuration is meant and has only been tested with Emacs 29.2.

I try to be explicit about what I use.
Magic is limited to using external packages.
I know every line of the configuration and why are they there.

I use [use-package](https://github.com/jwiegley/use-package) extensively.

I used to have an [Emacs configuration based on Prelude](https://github.com/haplo/prelude)
which is now discontinued.

## Packages I use

This list might be outdated, you would do better by grepping [init.el](init.el) for `use-package` uses.

* [`ace-window`](https://github.com/abo-abo/ace-window): quickly move/split/swap/copy windows.
* [`all-the-icons`](https://github.com/domtronn/all-the-icons.el): pretty icons.
* [`all-the-icons-ibuffer`](https://github.com/seagle0128/all-the-icons-ibuffer): pretty icons in *ibuffer*.
* [`all-the-icons-completion`](https://github.com/iyefrat/all-the-icons-completion): pretty icons in completion minibuffer.
* [`anzu`](https://github.com/emacsorphanage/anzu): highlight search and replace.
* [`apheleia`](https://github.com/radian-software/apheleia): async autoformatter for many languages.
* [`avy`](https://github.com/abo-abo/avy): quick jumps.
* [`browse-kill-ring`](https://github.com/browse-kill-ring/browse-kill-ring): Browse. The. Kill. Ring.
* [`calibredb`](https://github.com/chenyanming/calibredb.el): an Emacs interface to the Calibre DB.
* [`cape`](https://github.com/minad/cape): completion-at-point extensions, pairs with corfu.
* [`corfu`](https://github.com/minad/corfu/): completion-at-point popup. Like [company](https://company-mode.github.io/) but lighter.
* [`compile`](https://www.emacswiki.org/emacs/CompileCommand): built-in command to compile stuff.
* [`consult`](https://github.com/minad/consult): practical commands based on the Emacs completion function `completing-read`.
* [`consult-dir`](https://github.com/karthink/consult-dir): insert directory paths into the minibuffer prompt.
* [`crux`](https://github.com/bbatsov/crux): collection of random utilities, originally bundled with [Prelude](https://github.com/bbatsov/prelude).
* [`csv-mode`](https://elpa.gnu.org/packages/csv-mode.html): major mode to edit CSV (Comma-Separated Values) files.
* [`diff-hl`](https://github.com/dgutov/diff-hl): show uncommitted changes.
* [`diminish`](https://github.com/myrjola/diminish.el): hide minor-modes from modeline.
* [`dirvish`](https://github.com/alexluigit/dirvish): an improved batteries-included dired replacement.
* [`discover-my-major`](https://framagit.org/steckerhalter/discover-my-major): show keybindings associated to the current major mode.
* [`dockerfile-mode`](https://github.com/spotify/dockerfile-mode): major mode to edit Dockerfiles.
* [`doom-modeline`](https://seagle0128.github.io/doom-modeline/): a very nice modeline.
* [`doom-themes`](https://github.com/hlissner/emacs-doom-themes): collection of themes for [Doom Emacs](https://github.com/doomemacs/doomemacs), they place nicely with `doom-modeline`.
* [`easy-kill`](https://github.com/leoliu/easy-kill): mark/kill words, sexps, lines and more.
* [`editorconfig`](https://github.com/editorconfig/editorconfig-emacs): support for *.editorconfig* files.
* [`eglot`](https://joaotavora.github.io/eglot/): LSP client built-in since Emacs 29.
* [`eldoc`](https://elpa.gnu.org/packages/eldoc.html): display documentation about current function in echo area.
* [`embark`](https://github.com/oantolin/embark/): choose a command to run based on what is near point, both during a minibuffer completion session and in normal buffers.
* [`emojify`](https://github.com/iqbalansari/emacs-emojify): display emojis.
* [`envrc`](https://github.com/purcell/envrc): use [direnv](https://direnv.net/) to update Emacs environment when visiting files.
* [`exec-path-from-shell`](https://github.com/purcell/exec-path-from-shell): have Emacs inherit environment variables.
* [`expand-region`](https://github.com/magnars/expand-region.el): increase selected region by semantic units.
* [`flymake`](https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html): Emacs built-in error checker. It integrates natively with Eglot.
* [`flyspell`](https://www.emacswiki.org/emacs/FlySpell): spell checking.
* [`forge`](https://magit.vc/manual/forge/): Magit extension to work with code hosting sites (e.g. GitHub).
* [`geiser`](https://nongnu.org/geiser/): [Scheme](https://www.scheme.org/) hacking.
* [`git-timemachine`](https://codeberg.org/pidu/git-timemachine): browse previous revisions of any git-controlled file.
* [`go-mode`](https://github.com/dominikh/go-mode.el): major mode for [Go](https://go.dev/) files.
* [`helpful`](https://github.com/Wilfred/helpful): better help buffers.
* [`ibuffer`](https://www.emacswiki.org/emacs/IbufferMode): better buffer view. Like dired for buffers..
* [`ibuffer-projectile`](https://github.com/purcell/ibuffer-projectile): group buffers by project in *ibuffer* view.
* [`js2-mode`](https://github.com/mooz/js2-mode): major mode for Javascript files.
* [`js2-refactor`](https://github.com/js-emacs/js2-refactor.el): refactor operations for Javascript code.
* [`json-mode`](https://github.com/joshwnj/json-mode): major mode for JSON files.
* [`key-chord`](https://github.com/emacsorphanage/key-chord): execute commands by pressing keys quickly.
* [`keycast`](https://github.com/tarsius/keycast/): show current command and its binding.
* [`kind-icon`](https://github.com/jdtsmith/kind-icon): icons for completion-at-point candidates.
* [`magit`](https://magit.vc/manual/forge/): the best Git interface there is.
* [`marginalia`](https://github.com/minad/marginalia): adds marginalia to the minibuffer completions.
* [`markdown-mode`](https://jblevins.org/projects/markdown-mode/): major mode to edit Markdown files.
* [`move-text`](https://github.com/emacsfodder/move-text): move lines or regions up and down.
* [`nov-el`](https://depp.brause.cc/nov.el/): major mode for reading EPUB files in Emacs.
* [`olivetti`](https://github.com/rnkn/olivetti): center text for better writing.
* [`orderless`](https://github.com/oantolin/orderless): completion style that divides the pattern into space-separated components, and matches candidates that match all of the components in any order.
* [`org`](https://orgmode.org/): the most powerful note taking and planning software ever created.
* [`org-superstar`](https://github.com/integral-dw/org-superstar-mode): prettier headings in org-mode.
* [`orgit`](https://github.com/magit/orgit/): link to Magit buffers from Org-mode.
* [`orgit-forge`](https://github.com/magit/orgit-forge/): link to Forge buffers from Org-mode.
* [`perspective`](https://github.com/nex3/perspective-el): per-project named workspaces with window layouts and buffer lists.
* [`popper`](https://github.com/karthink/popper): tame the flood of ephemeral windows Emacs produces.
* [`projectile`](https://github.com/bbatsov/projectile): project management.
* [`puni`](https://github.com/AmaiKinono/puni): minor mode to work with delimiters that come in pairs, e.g. parentheses, braces, tags, etc.
* [`rainbow-delimiters`](https://github.com/Fanael/rainbow-delimiters): color delimiters such as parentheses or braces as they nest.
* [`rainbow-mode`](https://elpa.gnu.org/packages/rainbow-mode.html): color strings that describe colors.
* [`rjsx-mode`](https://github.com/felipeochoa/rjsx-mode/): minor mode for React JSX files.
* [`rustic`](https://github.com/brotzeit/rustic): major mode for [Rust](https://www.rust-lang.org/) coding.
* [`saveplace`](https://www.emacswiki.org/emacs/SavePlace): built-in to remember last position on each open file.
* [`super-save`](https://github.com/bbatsov/super-save): automatically save buffers on certain events.
* [`systemd`](https://github.com/holomorph/systemd-mode): major mode for editing systemd files.
* [`toml-mode`](https://github.com/dryman/toml-mode.el): major mode for TOML files.
* [`tramp`](https://www.gnu.org/software/tramp/): remote file editing, beyond cool.
* [`treesit-auto`](https://github.com/renzmann/treesit-auto): automatically install and use [tree-sitter grammars](https://tree-sitter.github.io/tree-sitter/).
* [`vundo`](https://github.com/casouri/vundo): visualize and navigate the undo history as a tree, never lose an edit.
* [`vertico`](https://github.com/minad/vertico): performant and minimalistic vertical completion UI based on the default completion system.
* [`visual-regexp`](https://github.com/emacsmirror/visual-regexp): live view of the matches of a regular expression.
* [`volatile-highlights`](https://github.com/k-talo/volatile-highlights.el): visual feedback for some operations.
* [`web-mode`](https://web-mode.org/): major mode for web templates (HTML and much more).
* [`wgrep`](https://github.com/mhayashi1120/Emacs-wgrep): edit a grep buffer and apply those changes to the file buffer.
* [`which-key`](https://github.com/justbur/emacs-which-key): show all possible key combinations.
* [`whitespace-cleanup-mode`](https://github.com/purcell/whitespace-cleanup-mode): minor mode to clean up a buffer's whitespace on save.
* [`writegood-mode`](https://github.com/bnbeckwith/writegood-mode): minor mode to aid in finding common writing problems in English.
* [`yaml-mode`](https://github.com/yoshiki/yaml-mode): major mode for YAML files.
* [`yasnippet`](https://joaotavora.github.io/yasnippet/): snippet extension.

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
