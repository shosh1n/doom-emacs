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

;;Beautify emacs
;;(package! all-the-icons :recipe (:local-repo "~/.emacs.d/modules/AddPackage/all-the-icons"))
;;Beautfiy dired
;;(package! all-the-icons-dired :recipe (:local-repo  "~/.emacs.d/modules/AddPackage/all-the-icons-dired-master"))
;;organize in org-mode
(package! org-super-agenda :pin "f5e80e4d0d...")
;;beautify some more org-mode
(package! org-pretty-table
  :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "474ad84a8f...")
;;use gnuplot
;;(package! org-mime)
(package! ccls)
(package! gnuplot)
(package! irony)
;;use discord add-on
(package! elcord)
;;beautify heading in org-mode
(package! org-bullets)
(package! straight)
(package! lsp-ui)
(package! org-superstar)
(package! lsp-treemacs)
(package! yasnippet-snippets)
(package! lsp-ivy)
(package! jupyter)
(package! dap-mode)
(package! lsp-pyright)
(package! svg-tag-mode)
(package! flycheck)
(package! toml-mode)
(package! nix-mode)
(package! scad-mode)
(package! mastodon)
(package! per-buffer-theme)
;;(package! scad-preview)
(package! virtualenv)
;;(package! zmq)
(package! pyvenv)
(package! simple-httpd)
(package! websocket)
(package! company)
(package! org-ref)
(package! evil-nerd-commenter)

(package! eterm-256color)
;;(package! engrave-faces)
(package! lsp-mode)
;;beautify some more
(package! org-pretty-tags :pin "5c7521651b...")

;;(package! engrave-faces :recipe (:local-repo "~/.emacs.d/modules/misc/engrave-faces"))
(package! numpydoc)
;;(package! lilypond :recipe (:local-repo "/usr/local/lilypond/usr/share/emacs/site-lisp"))

(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view") :pin "13314338d7...")

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; When using org-roam via the `+roam` flag

(package! info-colors :pin "47ee73cc19...")
(package! diredfl)

;;(package! ob-async)
(package! org-fragtog :pin "0151cabc7a...")

(package! graphviz-dot-mode :pin "3642a0a5f4...")

(package! beancount :recipe (:host github :repo "beancount/beancount-mode")
  :pin "3c04745fa5...")

(package! rotate :pin "4e9ac3ff80...")

(package! xkcd :pin "66e928706f...")


(package! theme-magic :pin "844c4311bd...")




(package! ess-view :pin "925cafd876...")


(package! info-colors :pin "47ee73cc19...")



(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f25337..." :disable t)



;;(package! mixed-pitch :recipe (:local-repo "~/.emacs.d/modules/AddPackage/mixed-pitch-master") :pin nil)

(unpin! org-roam)
(package! simple-httpd)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! embark)
;;(package!marginalia)
(package! embark-consult)
(package! ivy-prescient)
;;(package! ditaa)
