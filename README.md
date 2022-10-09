# Fidel Ramos' GNU Emacs configuration

This is my very own Emacs configuration.
There are many like it, but this one's mine.

The stated goals:
- Explicit about what I use.
  Magic is limited to using external packages.
  I know every line of the configuration and why are they there.
- Primarily based on [Guix](https://guix.gnu.org/).
  This means no need to use external package repositories, not ELPA nor MELPA.
  I use [use-package](https://github.com/jwiegley/use-package) extensively, so the configuration should potentially work without Guix, but that case is usually untested.

I used to have an [Emacs configuration based on Prelude](https://github.com/haplo/prelude)
which is now discontinued.

## Packages I use

This list might be outdated, you would do better by grepping [init.el](init.el) for `use-package` uses.

* [`all-the-icons`](https://github.com/domtronn/all-the-icons.el): pretty icons.
* [`all-the-icons-dired`](https://github.com/jtbm37/all-the-icons-dired): pretty icons in *dired*.
* [`all-the-icons-ibuffer`](https://github.com/seagle0128/all-the-icons-ibuffer): pretty icons in *ibuffer*.
* [`anzu`](https://github.com/emacsorphanage/anzu): highlight search and replace.
* [`avy`](https://github.com/abo-abo/avy): quick jumps.
* [`browse-kill-ring`](https://github.com/browse-kill-ring/browse-kill-ring): Browse. The. Kill. Ring.
* [`company`](https://company-mode.github.io/): autocomplete.
* [`company-posframe`](https://github.com/tumashu/company-posframe): display documentation for autocompletion candidates.
* [`compile`](https://www.emacswiki.org/emacs/CompileCommand): built-in command to compile stuff.
* [`crux`](https://github.com/bbatsov/crux): collection of random utilities, originally bundled with [Prelude](https://github.com/bbatsov/prelude).
* [`diff-hl`](https://github.com/dgutov/diff-hl): show uncommitted changes.
* [`diminish`](https://github.com/myrjola/diminish.el): hide minor-modes from modeline.
* [`dired`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html):  built-in directory editor.
* [`dired-hacks`](https://github.com/Fuco1/dired-hacks): useful extensions to `dired`, such as [`dired-narrow`](https://github.com/Fuco1/dired-hacks#dired-narrow) and [`dired-subtree`](https://github.com/Fuco1/dired-hacks#dired-subtree).
* [`dockerfile-mode`](https://github.com/spotify/dockerfile-mode): major mode to edit Dockerfiles.
* [`doom-modeline`](https://seagle0128.github.io/doom-modeline/): a very nice modeline.
* [`doom-themes`](https://github.com/hlissner/emacs-doom-themes): collection of themes for [Doom Emacs](https://github.com/doomemacs/doomemacs), they place nicely with `doom-modeline`.
* [`easy-kill`](https://github.com/leoliu/easy-kill): mark/kill words, sexps, lines and more.
* [`editorconfig`](https://github.com/editorconfig/editorconfig-emacs): support for *.editorconfig* files.
* [`eldoc`](https://elpa.gnu.org/packages/eldoc.html): display documentation about current function in echo area.
* [`exec-path-from-shell`](https://github.com/purcell/exec-path-from-shell): have Emacs inherit environment variables.
* [`expand-region`](https://github.com/magnars/expand-region.el): increase selected region by semantic units.
* [`eyebrowse`](https://depp.brause.cc/eyebrowse/): manage [window](https://www.emacswiki.org/emacs/Window) configurations.
* [`flycheck`](https://www.flycheck.org/): syntax checking.
* [`flyspell`](https://www.emacswiki.org/emacs/FlySpell): spell checking.
* [`forge`](https://magit.vc/manual/forge/): Magit extension to work with code hosting sites (e.g. GitHub).
* [`geiser`](https://nongnu.org/geiser/): [Scheme](https://www.scheme.org/) hacking.
* [`go-mode`](https://github.com/dominikh/go-mode.el): major mode for [Go](https://go.dev/) files.
* [`helpful`](https://github.com/Wilfred/helpful): better help buffers.
* [`ibuffer-projectile`](https://github.com/purcell/ibuffer-projectile): group buffers by project in *ibuffer* view.
* [`ivy`](https://oremacs.com/swiper/): better candidate selection for Emacs completion.
* [`js2-mode`](https://github.com/mooz/js2-mode): major mode for Javascript files.
* [`js2-refactor`](https://github.com/js-emacs/js2-refactor.el): refactor operations for Javascript code.
* [`json-mode`](https://github.com/joshwnj/json-mode): major mode for JSON files.
* [`key-chord`](https://github.com/emacsorphanage/key-chord): execute commands by pressing quicks quickly.
* [`lsp-ivy`](https://github.com/emacs-lsp/lsp-ivy): jump to LSP workspace symbols using ivy.
* [`lsp-mode`](https://emacs-lsp.github.io/lsp-mode/): [Language Server Protocol](https://en.wikipedia.org/wiki/Language_Server_Protocol) support for Emacs.
* [`lsp-ui`](https://github.com/emacs-lsp/lsp-ui): UI components for *lsp-mode*.
* [`magit`](https://magit.vc/manual/forge/): the best Git interface there is.
* [`markdown-mode`](https://jblevins.org/projects/markdown-mode/): major mode to edit Markdown files.
* [`move-text`](https://github.com/emacsfodder/move-text): move lines or regions up and down.
* [`olivetti`](https://github.com/rnkn/olivetti): center text for better writing.
* [`org`](https://orgmode.org/): the most powerful note taking and planning software ever created.
* [`org-superstar`](https://github.com/integral-dw/org-superstar-mode): prettier headings in org-mode.
* [`prettier-js`](https://github.com/prettier/prettier-emacs): autoformatter for Javascript code.
* [`projectile`](https://github.com/bbatsov/projectile): project management.
* [`pyvenv`](https://github.com/jorgenschaefer/pyvenv): work with Python virtualenvs.
* [`rainbow-delimiters`](https://github.com/Fanael/rainbow-delimiters): color delimiters such as parentheses or braces as they nest.
* [`rainbow-mode`](https://elpa.gnu.org/packages/rainbow-mode.html): color strings that describe colors.
* [`rjsx-mode`](https://github.com/felipeochoa/rjsx-mode/): minor mode for React JSX files.
* [`rustic`](https://github.com/brotzeit/rustic): major mode for [Rust](https://www.rust-lang.org/) coding.
* [`saveplace`](https://www.emacswiki.org/emacs/SavePlace): built-in to remember last position on each open file.
* [`smartparens`](https://smartparens.readthedocs.io/): minor mode to work with delimiters that come in pairs, e.g. parentheses, braces, tags, etc.
* [`super-save`](https://github.com/bbatsov/super-save): automatically save buffers on certain events.
* [`tide`](https://github.com/ananthakumaran/tide): *TypeScript Interactive Development Environment*.
* [`toml-mode`](https://github.com/dryman/toml-mode.el): major mode for TOML files.
* [`tramp`](https://www.gnu.org/software/tramp/): remote file editing, beyond cool.
* [`typescript-mode`](https://github.com/emacs-typescript/typescript.el): major mode for Typescript files.
* [`undo-tree`](https://www.dr-qubit.org/undo-tree.html): visualize and navigate the undo tree for a buffer, never lose an edit.
* [`volatile-highlights`](https://github.com/k-talo/volatile-highlights.el): visual feedback for some operations.
* [`web-mode`](https://web-mode.org/): major mode for web templates (HTML and much more).
* [`which-key`](https://github.com/justbur/emacs-which-key): show all possible key combinations.
* [`whitespace-cleanup-mode`](https://github.com/purcell/whitespace-cleanup-mode): minor mode to clean up a buffer's whitespace on save.
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
