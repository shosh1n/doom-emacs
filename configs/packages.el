;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
;;
;;useability & completion of sorts...
(package! irony)
(package! company)
(package! diredfl)
(package! info-colors :pin "47ee73cc19...")
(package! org-pretty-table
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "474ad84a8f...")
(package! org-mime)
(package! embark)
(package! embark-consult)
(package! ivy-prescient)
(package! yasnippet-snippets)
(package! org-bullets)
(package! svg-tag-mode)
;;auto activating snippets
(package! aas
  :recipe (:host github
           :repo "ymarco/auto-activating-snippets")
  :pin "566944e3b336c29d3ac11cd739a954c9d112f3fb")


;;IDE-Stuff
;;(package! lsp-ui)
;;(package! lsp-treemacs)
;;(package! lsp-ivy)
;;(package! dap-mode)
;;(package! lsp-pyright)
;;visual candy e.g. graphical progress-bar

;;syntax checking
(package! flycheck)
(package! toml-mode)
;;(package! nix-mode)
(package! scad-mode)

;;social stuff
;;(package! elcord)
;;(package! per-buffer-theme)
;;(package! scad-preview)
;;(package! virtualenv)
;;(package! zmq)
;;(package! pyvenv)
;;
;;web-page
(package! simple-httpd)
(package! websocket)
(package! evil-nerd-commenter)
;;(package! org-pretty-tags :pin "5c7521651b...")


;;documentation and office
(package! org-transclusion)
(package! org-ref)
(package! elfeed)
(package! elfeed-score)
(package! citar)
;;documenting in numpy-style
(package! numpydoc)
;;show dependencies in org-documents
(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view") :pin "13314338d7...")
(package! org-roam-bibtex )
;;render latex on mouse-cursor-exit
(package! org-fragtog)
(package! quarto-mode)
(package! conf-data-toml :recipe (:host github :repo "tecosaur/conf-data-toml"))
(package! shell-maker :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))
(package! chatgpt-shell :recipe (:host github :repo "xenodium/chatgpt-shell"))
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! graphviz-dot-mode :pin "3642a0a5f4...")

;;create useful images of my code
(package! carbon-now-sh)

;;misc
(package! xkcd :pin "66e928706f...")
;;getting colors from emacs-themes
(package! theme-magic :pin "844c4311bd...")
(package! ess-view :pin "925cafd876...")
;;some themes
(package! eziam-themes)
(package! flucui-themes)
(package! leuven-theme)
(package! tao-theme )
;;view large files
(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f25337..." :disable t)
                                        ;(package! realgud-ipdb :pin "f18f907aa4ddd3e59dc19ca296d4ee2dc5e436b0")
(package! super-save
  :disable t
  :pin "3313f38ed7d23947992e19f1e464c6d544124144")
;;rotating windows in emacs
;;(package! rotate :pin "4e9ac3ff80...")
;;(package! nov)
;;visual-undo history
(package! vundo
  :recipe (:host github
           :repo "casouri/vundo")
  :pin "16a09774ddfbd120d625cdd35fcf480e76e278bb")
;;(package! focus :pin "9dd85fc474bbc1ebf22c287752c960394fcd465a")
(package! good-scroll
  :disable EMACS29+
  :pin "a7ffd5c0e5935cebd545a0570f64949077f71ee3")
;;(package! evil-textobj-tree-sitter)
(package! shell-maker :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))
(package! chatgpt-shell :recipe (:host github :repo "xenodium/chatgpt-shell"))
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! speed-type)
(package! virtualenvwrapper)
(package! detached)
(package! modus-themes)
(package! cursory)
(package! hyperbole)
;;(package! conventional-commit)
(provide 'packages)
