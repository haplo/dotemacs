# Fidel Ramos' GNU Emacs configuration

This is my very own Emacs configuration.
There are many like it, but this one's mine.

The stated goals:
- Explicit about what I use.
  Magic is limited to using external packages.
  I know every line of the configuration and why are they there.
- Primarily based on [Guix](https://guix.gnu.org/).
  This means no need to use external package repositories, not ELPA nor MELPA.
  I use [use-package](https://github.com/jwiegley/use-package) extensively, so the configuration should potentially work without Guix, but that case goes usually untested.

I used to have an [Emacs configuration based on Prelude](https://github.com/haplo/prelude)
which is now discontinued.

## Packages I use

This list might be outdated, you would do better by grepping [init.el](init.el) for `use-package` uses.

* [`ace-window`](https://github.com/abo-abo/ace-window): quickly move/split/swap/copy windows.
* [`all-the-icons`](https://github.com/domtronn/all-the-icons.el): pretty icons.
* [`all-the-icons-ibuffer`](https://github.com/seagle0128/all-the-icons-ibuffer): pretty icons in *ibuffer*.
* [`all-the-icons-completion`](https://github.com/iyefrat/all-the-icons-completion): pretty icons in completion minibuffer.
* [`anzu`](https://github.com/emacsorphanage/anzu): highlight search and replace.
* [`avy`](https://github.com/abo-abo/avy): quick jumps.
* [`browse-kill-ring`](https://github.com/browse-kill-ring/browse-kill-ring): Browse. The. Kill. Ring.
* [`calibredb`](https://github.com/chenyanming/calibredb.el): an Emacs interface to the Calibre DB.
* [`cape`](https://github.com/minad/cape): completion-at-point extensions, pairs with corfu.
* [`corfu`](https://github.com/minad/corfu/): completion-at-point popup. Like company but lighter.
* [`corfu-doc`](https://github.com/galeo/corfu-doc): display documentation for autocompletion candidates.
* [`compile`](https://www.emacswiki.org/emacs/CompileCommand): built-in command to compile stuff.
* [`consult`](https://github.com/minad/consult): practical commands based on the Emacs completion function `completing-read`.
* [`consult-dir`](https://github.com/karthink/consult-dir): insert directory paths into the minibuffer prompt.
* [`crux`](https://github.com/bbatsov/crux): collection of random utilities, originally bundled with [Prelude](https://github.com/bbatsov/prelude).
* [`csv-mode`](https://elpa.gnu.org/packages/csv-mode.html): major mode to edit CSV (Comma-Separated Values) files.
* [`desktop-mode`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html): save and restore Emacs editor state.
* [`diff-hl`](https://github.com/dgutov/diff-hl): show uncommitted changes.
* [`diminish`](https://github.com/myrjola/diminish.el): hide minor-modes from modeline.
* [`direnv`](https://github.com/wbolster/emacs-direnv): use [direnv](https://direnv.net/) to update Emacs environment when visiting files.
* [`dirvish`](https://github.com/alexluigit/dirvish): an improved batteries-included dired replacement.
* [`discover-my-major`](https://framagit.org/steckerhalter/discover-my-major): show keybindings associated to the current major mode.
* [`dockerfile-mode`](https://github.com/spotify/dockerfile-mode): major mode to edit Dockerfiles.
* [`doom-modeline`](https://seagle0128.github.io/doom-modeline/): a very nice modeline.
* [`doom-themes`](https://github.com/hlissner/emacs-doom-themes): collection of themes for [Doom Emacs](https://github.com/doomemacs/doomemacs), they place nicely with `doom-modeline`.
* [`easy-kill`](https://github.com/leoliu/easy-kill): mark/kill words, sexps, lines and more.
* [`editorconfig`](https://github.com/editorconfig/editorconfig-emacs): support for *.editorconfig* files.
* [`eldoc`](https://elpa.gnu.org/packages/eldoc.html): display documentation about current function in echo area.
* [`embark`](https://github.com/oantolin/embark/): choose a command to run based on what is near point, both during a minibuffer completion session and in normal buffers.
* [`emojify`](https://github.com/iqbalansari/emacs-emojify): display emojis.
* [`exec-path-from-shell`](https://github.com/purcell/exec-path-from-shell): have Emacs inherit environment variables.
* [`expand-region`](https://github.com/magnars/expand-region.el): increase selected region by semantic units.
* [`eyebrowse`](https://depp.brause.cc/eyebrowse/): manage [window](https://www.emacswiki.org/emacs/Window) configurations.
* [`flycheck`](https://www.flycheck.org/): syntax checking.
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
* [`key-chord`](https://github.com/emacsorphanage/key-chord): execute commands by pressing quicks quickly.
* [`kind-icon`](https://github.com/jdtsmith/kind-icon): icons for completion-at-point candidates.
* [`lsp-mode`](https://emacs-lsp.github.io/lsp-mode/): [Language Server Protocol](https://en.wikipedia.org/wiki/Language_Server_Protocol) support for Emacs.
* [`lsp-ui`](https://github.com/emacs-lsp/lsp-ui): UI components for *lsp-mode*.
* [`magit`](https://magit.vc/manual/forge/): the best Git interface there is.
* [`marginalia`](https://github.com/minad/marginalia): adds marginalia to the minibuffer completions.
* [`markdown-mode`](https://jblevins.org/projects/markdown-mode/): major mode to edit Markdown files.
* [`move-text`](https://github.com/emacsfodder/move-text): move lines or regions up and down.
* [`nov-el`](https://depp.brause.cc/nov.el/): major mode for reading EPUB files in Emacs.
* [`olivetti`](https://github.com/rnkn/olivetti): center text for better writing.
* [`orderless`](https://github.com/oantolin/orderless): completion style that divides the pattern into space-separated components, and matches candidates that match all of the components in any order.
* [`org`](https://orgmode.org/): the most powerful note taking and planning software ever created.
* [`org-superstar`](https://github.com/integral-dw/org-superstar-mode): prettier headings in org-mode.
* [`popper`](https://github.com/karthink/popper): tame the flood of ephemeral windows Emacs produces.
* [`prettier-js`](https://github.com/prettier/prettier-emacs): autoformatter for Javascript code.
* [`prisma-mode`](https://github.com/pimeys/emacs-prisma-mode): a major mode for editing Prisma ORM schemas.
* [`projectile`](https://github.com/bbatsov/projectile): project management.
* [`puni`](https://github.com/AmaiKinono/puni): minor mode to work with delimiters that come in pairs, e.g. parentheses, braces, tags, etc.
* [`rainbow-delimiters`](https://github.com/Fanael/rainbow-delimiters): color delimiters such as parentheses or braces as they nest.
* [`rainbow-mode`](https://elpa.gnu.org/packages/rainbow-mode.html): color strings that describe colors.
* [`rjsx-mode`](https://github.com/felipeochoa/rjsx-mode/): minor mode for React JSX files.
* [`rustic`](https://github.com/brotzeit/rustic): major mode for [Rust](https://www.rust-lang.org/) coding.
* [`saveplace`](https://www.emacswiki.org/emacs/SavePlace): built-in to remember last position on each open file.
* [`super-save`](https://github.com/bbatsov/super-save): automatically save buffers on certain events.
* [`tide`](https://github.com/ananthakumaran/tide): *TypeScript Interactive Development Environment*.
* [`toml-mode`](https://github.com/dryman/toml-mode.el): major mode for TOML files.
* [`tramp`](https://www.gnu.org/software/tramp/): remote file editing, beyond cool.
* [`typescript-mode`](https://github.com/emacs-typescript/typescript.el): major mode for Typescript files.
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
