(setq user-full-name "cherma"
      user-mail-address "hermannschris@gmail.com")

(when (daemonp)
  (add-hook! 'server-after-make-frame-hook
    (unless (string-match-p "\\*draft\\|\\*stdin\\|emacs-everywhere" (buffer-name))
      (switch-to-buffer +doom-dashboard-name))))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 80)
(defvar efs/default-variable-font-size 80)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(100 . 80))


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


(require 'package)
;;(require 'lilypond)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")


(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "JetBrains Mono" :height efs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height efs/default-variable-font-size :weight 'regular)

(setq doom-font (font-spec :family "JetBrainsMonoNL Nerd Font Mono" :size 16))
(setq doom-unicode-font (font-spec :family "JetBrainsMonoNL Nerd Font Mono"))
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'zmq)

(use-package doom-themes
  :init (load-theme 'doom-palenight t))
(after! doom-modeline
  (setq! doom-modeline-buffer-file-name-style 'auto
         doom-modeline-height 30
         doom-modeline-icon t ;; for some reason this line causes an error
         doom-modeline-modal-icon nil
         doom-modeline-env-version t
         doom-modeline-buffer-modification-icon t
         doom-modeline-enable-word-count t
         doom-modeline-continuous-word-count-modes '(text-mode)
         doom-modeline-icon (display-graphic-p)
         doom-modeline-persp-name t
         doom-modeline-persp-icon t
         doom-modeline-github t
         doom-modeline-mu4e t))

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(misc-info vcs persp-name grip irc mu4e github debug repl lsp minor-modes input-method indent-info buffer-encoding checker major-mode process " " bar " ")))


(setq dired-dwim-target t)
(setq markdown-split-window-direction 'right)

(setq auth-sources '("~/.password-store/Email/hermannschris@gmail.com.gpg")
      auth-source-do-cache t
      auth-source-cache-expiry 86400 ; All day, defaut is 2h (7200)
      password-cache t
      password-cache-expiry 86400)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(use-package! super-save
  :config
  (setq auto-save-default t ;; nil to switch off the built-in `auto-save-mode', maybe leave it t to have a backup!
        super-save-exclude '(".gpg")
        super-save-remote-files nil
        super-save-auto-save-when-idle t)
  (super-save-mode +1))
(use-package evil-textobj-tree-sitter :ensure t)
(setq auto-save-default t)

;; Increase undo history limits even more
(after! undo-fu
  ;; Emacs undo defaults
  (setq undo-limit        10000000    ;; 1MB   (default is 160kB, Doom's default is 400kB)
        undo-strong-limit 100000000   ;; 100MB (default is 240kB, Doom's default is 3MB)
        undo-outer-limit  1000000000) ;; 1GB   (default is 24MB,  Doom's default is 48MB)

  ;; Undo-fu customization options
  (setq undo-fu-allow-undo-in-region t ;; Undoing with a selection will use undo within that region.
        undo-fu-ignore-keyboard-quit t)) ;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.

;; Evil undo
(after! evil
  (setq evil-want-fine-undo t)) ;; By default while in insert all changes are one big blob

(use-package! vundo
  :defer t
  :init
  (defconst +vundo-unicode-symbols
   '((selected-node   . ?●)
     (node            . ?○)
     (vertical-stem   . ?│)
     (branch          . ?├)
     (last-branch     . ?╰)
     (horizontal-stem . ?─)))

  (map! :leader
        (:prefix ("o")
         :desc "vundo" "v" #'vundo))

  :config
  (setq vundo-glyph-alist +vundo-unicode-symbols
        vundo-compact-display t
        vundo-window-max-height 6))

(use-package! focus
  :commands focus-mode)

(use-package! good-scroll
  :config (good-scroll-mode 1))

;;(when emacs-major-version 29
;;  (pixel-scroll-precision-mode 1))

(setq hscroll-step 1
      hscroll-margin 0
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil
      fast-but-imprecise-scrolling nil)

(after! all-the-icons
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))
(after! centaur-tabs
  ;; For some reason, setting `centaur-tabs-set-bar' this to `right'
  ;; instead of Doom's default `left', fixes this issue with Emacs daemon:
  ;; https://github.com/doomemacs/doomemacs/issues/6647#issuecomment-1229365473
  (setq centaur-tabs-set-bar 'under
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "⨂"
        centaur-tabs-modified-marker "⨀"
        centaur-tabs-style "bar"))


(after! writeroom-mode
  ;; Show mode line
  (setq writeroom-mode-line t)

  ;; Disable line numbers
  (add-hook! 'writeroom-mode-enable-hook
    (when (bound-and-true-p display-line-numbers-mode)
      (setq-local +line-num--was-activate-p display-line-numbers-type)
      (display-line-numbers-mode -1)))

  (add-hook! 'writeroom-mode-disable-hook
    (when (bound-and-true-p +line-num--was-activate-p)
      (display-line-numbers-mode +line-num--was-activate-p)))

  (after! org
    ;; Increase latex previews scale in Zen mode
    (add-hook! 'writeroom-mode-enable-hook (+org-format-latex-set-scale 2.0))
    (add-hook! 'writeroom-mode-disable-hook (+org-format-latex-set-scale 1.4)))

  (after! blamer
    ;; Disable blamer in zen (writeroom) mode
    (add-hook! 'writeroom-mode-enable-hook
      (when (bound-and-true-p blamer-mode)
        (setq +blamer-mode--was-active-p t)
        (blamer-mode -1)))
    (add-hook! 'writeroom-mode-disable-hook
      (when (bound-and-true-p +blamer-mode--was-active-p)
        (blamer-mode 1)))))


(after! highlight-indent-guides
  (setq highlight-indent-guides-character ?│
        highlight-indent-guides-responsive 'top))

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

;; Enable horizontal scrolling with the second mouse wheel or the touchpad
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-progressive-speed nil)

(use-package numpydoc
  :ensure t)

(straight-use-package 'numpydoc)

;; with use-package
(use-package numpydoc
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

(use-package all-the-icons)


(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package org-superstar
  ;;:if (not dw/is-termux)
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil))

;; TODO: Others to consider
;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;;(defun dw/search-org-files ()
;;  (interactive)
;;  (counsel-rg "" "~/Notes" nil "Search Notes: "))


(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	'("~/Dropbox/orgzly/Char.org"
	  "~/Dropbox/orgzly/Fitness.org"
	  "~/Dropbox/orgzly/Skill.org"
          "~/Dropbox/orgzly/Strife.org"
          "~/Dropbox/orgzly/System.org"
          "~/Dropbox/orgzly/Termine.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;;(setq org-todo-keywords
   ;; '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
    ;;  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;;(setq org-refile-targets
  ;;  '(("Archive.org" :maxlevel . 1)
  ;;    ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/org/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/org/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
      ("wi" "Write Idea" entry (file+olp+datetree "~/org/Journal.org")
           "* Write Idea :idea:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/org/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (efs/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
;;(setq lsp-clients-clangd-args '("-j=3"
;;				"--background-index"
;;				"--clang-tidy"
;;				"--completion-style=detailed"
;;				"--header-insertion=never"
;;				"--header-insertion-decorators=0"))
;;(after! lsp-clangd (set-lsp-priority! 'clangd 2))
;;
;;(add-to-list 'org-latex-classes
;;             '("my-letter"
;;               "\\documentclass\[%
;;DIV=14,
;;fontsize=12pt,
;;parskip=full,
;;subject=untitled,
;;backaddress=true,
;;fromalign=left,
;;fromemail=true,
;;fromphone=true\]\{scrlttr2\}
;;\[DEFAULT-PACKAGES]
;;\[PACKAGES]
;nnccls
;;  :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;         (lambda () (require 'ccls) (lsp))))



(defun efs/lsp-mode-setup()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;;(use-package envrc
;;  :commands (envrc-mode)
;;  :hook ((python-mode . envrc-mode)
;;         (org-jupyter-mode . envrc-mode))
;;  )

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
   :config
   (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))


 (use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda()
                     (require 'lsp-pyright)
                     (lsp)))) ; or lsp-deferred

(add-hook 'java-mode-hook #'evil-tree-edit-mode)
(defun me:c-mode-config ()
  (c-set-style "ellemtel"))

;;(which-key-mode)
;;(add-hook 'c-mode-hook 'lsp)
;;(add-hook 'c++-mode-hook 'lsp)
;;
;;(setq gc-cons-threshold (* 100 1024 1024)
;;      read-process-output-max (* 1024 1024)
;;      treemacs-space-between-root-nodes nil
;;      company-idle-delay 0.0
;;      company-minimum-prefix-length 1
;;      lsp-idle-delay 0.1)

(after! treemacs
  (require 'dired))

  ;; My custom stuff (from tecosaur's config)
  (setq +treemacs-file-ignore-extensions
        '(;; LaTeX
          "aux" "ptc" "fdb_latexmk" "fls" "synctex.gz" "toc"
          ;; LaTeX - bibliography
          "bbl"
          ;; LaTeX - glossary
          "glg" "glo" "gls" "glsdefs" "ist" "acn" "acr" "alg"
          ;; LaTeX - pgfplots
          "mw"
          ;; LaTeX - pdfx
          "pdfa.xmpi"
          ;; Python
          "pyc"))

  (setq +treemacs-file-ignore-globs
        '(;; LaTeX
          "*/_minted-*"
          ;; AucTeX
          "*/.auctex-auto"
          "*/_region_.log"
          "*/_region_.tex"
          ;; Python
          "*/__pycache__"))


(after! company-box
  (defun +company-box--reload-icons-h ()
    (setq company-box-icons-all-the-icons
          (let ((all-the-icons-scale-factor 0.8))
            `((Unknown       . ,(all-the-icons-faicon   "code"                 :face 'all-the-icons-purple))
              (Text          . ,(all-the-icons-material "text_fields"          :face 'all-the-icons-green))
              (Method        . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-red))
              (Function      . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-blue))
              (Constructor   . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-blue-alt))
              (Field         . ,(all-the-icons-faicon   "tag"                  :face 'all-the-icons-red))
              (Variable      . ,(all-the-icons-material "adjust"               :face 'all-the-icons-blue))
              (Class         . ,(all-the-icons-material "class"                :face 'all-the-icons-red))
              (Interface     . ,(all-the-icons-material "tune"                 :face 'all-the-icons-red))
              (Module        . ,(all-the-icons-faicon   "cubes"                :face 'all-the-icons-red))
              (Property      . ,(all-the-icons-faicon   "wrench"               :face 'all-the-icons-red))
              (Unit          . ,(all-the-icons-material "straighten"           :face 'all-the-icons-red))
              (Value         . ,(all-the-icons-material "filter_1"             :face 'all-the-icons-red))
              (Enum          . ,(all-the-icons-material "plus_one"             :face 'all-the-icons-red))
              (Keyword       . ,(all-the-icons-material "filter_center_focus"  :face 'all-the-icons-red-alt))
              (Snippet       . ,(all-the-icons-faicon   "expand"               :face 'all-the-icons-red))
              (Color         . ,(all-the-icons-material "colorize"             :face 'all-the-icons-red))
              (File          . ,(all-the-icons-material "insert_drive_file"    :face 'all-the-icons-red))
              (Reference     . ,(all-the-icons-material "collections_bookmark" :face 'all-the-icons-red))
              (Folder        . ,(all-the-icons-material "folder"               :face 'all-the-icons-red-alt))
              (EnumMember    . ,(all-the-icons-material "people"               :face 'all-the-icons-red))
              (Constant      . ,(all-the-icons-material "pause_circle_filled"  :face 'all-the-icons-red))
              (Struct        . ,(all-the-icons-material "list"                 :face 'all-the-icons-red))
              (Event         . ,(all-the-icons-material "event"                :face 'all-the-icons-red))
              (Operator      . ,(all-the-icons-material "control_point"        :face 'all-the-icons-red))
              (TypeParameter . ,(all-the-icons-material "class"                :face 'all-the-icons-red))
              (Template      . ,(all-the-icons-material "settings_ethernet"    :face 'all-the-icons-green))
              (ElispFunction . ,(all-the-icons-faicon   "cube"                 :face 'all-the-icons-blue))
              (ElispVariable . ,(all-the-icons-material "adjust"               :face 'all-the-icons-blue))
              (ElispFeature  . ,(all-the-icons-material "stars"                :face 'all-the-icons-orange))
              (ElispFace     . ,(all-the-icons-material "format_paint"         :face 'all-the-icons-pink))))))

  (when (daemonp)
    ;; Replace Doom defined icons with mine
    (when (memq #'+company-box--load-all-the-icons server-after-make-frame-hook)
      (remove-hook 'server-after-make-frame-hook #'+company-box--load-all-the-icons))
    (add-hook 'server-after-make-frame-hook #'+company-box--reload-icons-h))

  ;; Reload icons even if not in Daemon mode
  (+company-box--reload-icons-h))

  ;; Reload treemacs theme
  (setq doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)

  (setq treemacs-show-hidden-files nil
        treemacs-hide-dot-git-directory t
        treemacs-width 30)

 ;; (defvar +treemacs-file-ignore-extensions '()
 ;;   "File extension which `treemacs-ignore-filter' will ensure are ignored")

 ;; (defvar +treemacs-file-ignore-globs '()
 ;;   "Globs which will are transformed to `+treemacs-file-ignore-regexps' which `+treemacs-ignore-filter' will ensure are ignored")

 ;; (defvar +treemacs-file-ignore-regexps '()
 ;;   "RegExps to be tested to ignore files, generated from `+treeemacs-file-ignore-globs'")

 ;; (defun +treemacs-file-ignore-generate-regexps ()
 ;;   "Generate `+treemacs-file-ignore-regexps' from `+treemacs-file-ignore-globs'"
 ;;   (setq +treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp +treemacs-file-ignore-globs)))

;;  (unless (equal +treemacs-file-ignore-globs '())
;;    (+treemacs-file-ignore-generate-regexps))
;;
;;  (defun +treemacs-ignore-filter (file full-path)
;;    "Ignore files specified by `+treemacs-file-ignore-extensions', and `+treemacs-file-ignore-regexps'"
;;    (or (member (file-name-extension file) +treemacs-file-ignore-extensions)
;;        (let ((ignore-file nil))
;;          (dolist (regexp +treemacs-file-ignore-regexps ignore-file)
;;            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
;;
;;  (add-to-list 'treemacs-ignored-file-predicates #'+treemacs-ignore-filter))






(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(after! lsp-mode
  (setq lsp-lens-enable t
        lsp-semantic-tokens-enable t ;; hide unreachable ifdefs
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable nil
        ;; LSP UI related tweaks
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-code-actions nil))


(use-package! cpp-auto-include
  :commands cpp-auto-include)


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(setq flycheck-python-flake8-executable "flake8")

(after! flycheck
  (setq flycheck-cppcheck-checks '("information"
                                   "missingInclude"
                                   "performance"
                                   "portability"
                                   "style"
                                   "unusedFunction"
                                   "warning")))

(after! dap-mode
  ;; Set latest versions
  (setq dap-cpptools-extension-version "1.11.5")
  (require 'dap-cpptools)

  (setq dap-codelldb-extension-version "1.7.4")
  (require 'dap-codelldb)

  (setq dap-gdb-lldb-extension-version "0.26.0")
  (require 'dap-gdb-lldb)

  ;; More minimal UI
  (setq dap-auto-configure-features '(breakpoints locals expressions tooltip)
        dap-auto-show-output nil ;; Hide the annoying server output
        lsp-enable-dap-auto-configure t)

  ;; Automatically trigger dap-hydra when a program hits a breakpoint.
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))

  ;; Automatically delete session and close dap-hydra when DAP is terminated.
  (add-hook 'dap-terminated-hook
            (lambda (arg)
              (call-interactively #'dap-delete-session)
              (dap-hydra/nil)))

  ;; A workaround to correctly show breakpoints
  ;; from: https://github.com/emacs-lsp/dap-mode/issues/374#issuecomment-1140399819
  (add-hook! +dap-running-session-mode
    (set-window-buffer nil (current-buffer))))

;;(add-to-list 'minimap-major-modes 'cuda-mode)
(defun +debugger/clear-last-session ()
  "Clear the last stored session"
  (interactive)
  (doom-store-clear "+debugger"))

(map! :leader :prefix ("l" . "custom")
      (:when (modulep! :tools debugger +lsp)
       :prefix ("d" . "debugger")
       :desc "Clear last DAP session" "c" #'+debugger/clear-last-session))

(after! realgud
  (require 'hydra)

  ;; Add some missing gdb/rr commands
  (defun +realgud:cmd-start (arg)
    "start = break main + run"
    (interactive "p")
    (realgud-command "start"))

  (defun +realgud:cmd-reverse-next (arg)
    "Reverse next"
    (interactive "p")
    (realgud-command "reverse-next"))

  (defun +realgud:cmd-reverse-step (arg)
    "Reverse step"
    (interactive "p")
    (realgud-command "reverse-step"))

  (defun +realgud:cmd-reverse-continue (arg)
    "Reverse continue"
    (interactive "p")
    (realgud-command "reverse-continue"))

  (defun +realgud:cmd-reverse-finish (arg)
    "Reverse finish"
    (interactive "p")
    (realgud-command "reverse-finish"))

  ;; Define a hydra binding
  (defhydra realgud-hydra (:color pink :hint nil :foreign-keys run)
    "
 Stepping  |  _n_: next      |  _i_: step    |  _o_: finish  |  _c_: continue  |  _R_: restart  |  _u_: until-here
 Revese    | _rn_: next      | _ri_: step    | _ro_: finish  | _rc_: continue  |
 Breakpts  | _ba_: break     | _bD_: delete  | _bt_: tbreak  | _bd_: disable   | _be_: enable   | _tr_: backtrace
 Eval      | _ee_: at-point  | _er_: region  | _eE_: eval    |
           |  _!_: shell     | _Qk_: kill    | _Qq_: quit    | _Sg_: gdb       | _Ss_: start
"
    ("n"  realgud:cmd-next)
    ("i"  realgud:cmd-step)
    ("o"  realgud:cmd-finish)
    ("c"  realgud:cmd-continue)
    ("R"  realgud:cmd-restart)
    ("u"  realgud:cmd-until-here)
    ("rn" +realgud:cmd-reverse-next)
    ("ri" +realgud:cmd-reverse-step)
    ("ro" +realgud:cmd-reverse-finish)
    ("rc" +realgud:cmd-reverse-continue)
    ("ba" realgud:cmd-break)
    ("bt" realgud:cmd-tbreak)
    ("bD" realgud:cmd-delete)
    ("be" realgud:cmd-enable)
    ("bd" realgud:cmd-disable)
    ("ee" realgud:cmd-eval-at-point)
    ("er" realgud:cmd-eval-region)
    ("tr" realgud:cmd-backtrace)
    ("eE" realgud:cmd-eval)
    ("!"  realgud:cmd-shell)
    ("Qk" realgud:cmd-kill)
    ("Sg" realgud:gdb)
    ("Ss" +realgud:cmd-start)
    ("q"  nil "quit" :color blue) ;; :exit
    ("Qq" realgud:cmd-quit :color blue)) ;; :exit

  (defun +debugger/realgud:gdb-hydra ()
    "Run `realgud-hydra'."
    (interactive)
    (realgud-hydra/body))

  (map! :leader :prefix ("l" . "custom")
        (:when (modulep! :tools debugger)
         :prefix ("d" . "debugger")
         :desc "RealGUD hydra" "h" #'+debugger/realgud:gdb-hydra)))

(after! realgud
  (defun +debugger/rr-replay ()
    "Launch `rr replay'."
    (interactive)
    (realgud:gdb (+str-replace "gdb" "rr replay" realgud:gdb-command-name)))

;; (defun +debugger/rr-record ()
;;    "Launch `rr record' with parameters from launch.json or `+launch-json-debug-config'."
;;    (interactive)
;;    (let* ((conf (launch-json--config-choice))
;;           (args (launch-json--substite-special-vars (plist-get conf :program) (plist-get conf :args))))
;;      (unless (make-process :name "rr-record"
;;                            :buffer "*rr record*"
;;                            :command (append '("rr" "record") args))
;;        (message "Cannot start the 'rr record' process"))))

  (map! :leader :prefix ("l" . "custom")
        (:when (modulep! :tools debugger)
         :prefix ("d" . "debugger")
         :desc "rr record" "r" #'+debugger/rr-record
         :desc "rr replay" "R" #'+debugger/rr-replay)))

(defun hide-dotfiles-neotree()
  (setq neo-show-hidden-files nil))
(add-hook 'after-init-hook 'hide-dotfiles-neotree)


(use-package! gdb-mi
  :init
  (fmakunbound 'gdb)
  (fmakunbound 'gdb-enable-debug)

  :config
  (setq gdb-window-setup-function #'gdb--setup-windows ;; TODO: Customize this
        gdb-ignore-gdbinit nil) ;; I use gdbinit to define some useful stuff
  ;; History
  (defvar +gdb-history-file "~/.gdb_history")
  (defun +gud-gdb-mode-hook-setup ()
    "GDB setup."

    ;; Suposes "~/.gdbinit" contains:
    ;; set history save on
    ;; set history filename ~/.gdb_history
    ;; set history remove-duplicates 2048
    (when (and (ring-empty-p comint-input-ring)
               (file-exists-p +gdb-history-file))
      (setq comint-input-ring-file-name +gdb-history-file)
      (comint-read-input-ring t)))

  (add-hook 'gud-gdb-mode-hook '+gud-gdb-mode-hook-setup))

;;(with-eval-after-load 'ox-latex
;;(add-to-list 'org-latex-classes
;;           '("report-noparts"
;;              "\\documentclass{report}
;;        [NO-DEFAUKT-PACKAGES]
;;        [PACKAGES]
;;        [EXTRA]"
;;              ("\\chapter{%s}" . "\\chapter*{%s}")
;;              ("\\section{%s}" . "\\section*{%s}")
;;              ("\\subsection{%s}" . "\\subsection*{%s}")
;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;;(setq org-image-actual-width nil)


(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/roam/")
  (org-roam-dailies-directory "journals/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "math" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("a" "Article" plain
      (file "~/roam/Templates/ArticleNoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ))
    :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-caputes-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;;ensure the keymap is available


  :config
  (org-roam-db-autosync-mode))
(setq gdb-many-windows nil)


(defun set-gdb-layout(&optional c-buffer)
  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

  ;; from http://stackoverflow.com/q/39762833/846686
  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all

  (let* ((w-source (selected-window)) ;; left top
         (w-gdb (split-window w-source nil 'right)) ;; right bottom
         (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
         (w-stack (split-window w-locals nil 'above)) ;; right middle top
         (w-breakpoints (split-window w-stack nil 'above)) ;; right top
         (w-io (split-window w-source (floor(* 0.9 (window-body-height))) 'below))) ;; left bottom
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)

    (set-window-buffer w-gdb gud-comint-buffer)

    (select-window w-source)
    (set-window-buffer w-source c-buffer)))


(defadvice gdb (around args activate)
  "Change the way to gdb works."
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let ((c-buffer (window-buffer (selected-window)))) ;; save current buffer
    ad-do-it
    (set-gdb-layout c-buffer)))


(defadvice gdb-reset (around args activate)
  "Change the way to gdb exit."
  ad-do-it
  (set-window-configuration global-config-editing))
(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")


(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov gud-overlay)
         (bf (gud-find-file true-file)))
    (with-current-buffer bf
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    ;; (move-overlay ov (line-beginning-position) (line-end-position)
                    (current-buffer)))))


(defun gud-kill-buffer ()
  (if (derived-mode-p 'gud-mode)
      (delete-overlay gud-overlay)))

(add-hook 'kill-buffer-hook 'gud-kill-buffer)



;; A variable which to be used in .dir-locals.el, formatted as a list of plists;
;; '((:program "..." :args ("args1" "arg2" ...)))
;;(defvar +launch-json-debug-config nil)
;;
;;
;;(defvar launch-json--gud-debugger-regex
;;  (rx (seq bol (group-n 1 (or "gdb" "gud-gdb" "perldb" "pdb" "jdb" "guiler" "dbx" "sdb" "xdb") eol))))
;;
;;
;;(defvar launch-json--realgud-debugger-regex
;;  (rx (seq bol (or (seq "realgud:" (group-n 1 (or "gdb" "pdb"
;;                                                  "bashdb"  "kshdb" "zshd"
;;                                                  "perldb" "rdebug" "remake"
;;                                                  "trepan" "trepan2" "trepan3k" "trepanjs" "trepan.pl")))
;;                   (seq "realgud-" (group-n 1 (or "gub")))
;;                   ;; Additional debuggers
;;                   (seq "realgud:" (group-n 1 (or "xdebug" "pry" "jdb" "ipdb" "trepan-xpy" "trepan-ni" "node-inspect")))
;;                   ;; `realgud-lldb' defines the debug command as `realgud--lldb',
;;                   ;; We accept both `realgud:lldb' and `realgud--lldb' in the config
;;                   (seq "realgud" (or ":" "--") (group-n 1 (or "lldb")))) eol)))
;;
;;
;;;; Define aliases for realgud-lldb
;;(with-eval-after-load 'realgud-lldb
;;  (defalias 'realgud:lldb 'realgud--lldb)
;;  (defalias 'realgud:lldb-command-name 'realgud--lldb-command-name))
;;
;;
;;;; Define aliases for realgud-ipdb
;;(with-eval-after-load 'realgud-ipdb
;;  (defalias 'realgud:ipdb-command-name 'realgud--ipdb-command-name))
;;
;;
;;(defvar launch-json--last-config nil)
;;
;;
;;(defun launch-json-last-config-clear ()
;;  (interactive)
;;  (setq-local launch-json--last-config nil))
;;
;;(defun launch-json--substite-special-vars (program &optional args)
;;  "Substitue variables in PROGRAM and ARGS.
;;Return a list, in which processed PROGRAM is the first element, followed by ARGS."
;;  (let* ((curr-file (ignore-errors (expand-file-name (buffer-file-name))))
;;         (ws-root (string-trim-right
;;                   (expand-file-name
;;                    (or (projectile-project-root)
;;                        (ignore-errors (file-name-directory curr-file))
;;                        "."))
;;                   "/"))
;;         (ws-basename (file-name-nondirectory ws-root)))
;;    ;; Replace special variables
;;    (mapcar
;;     (lambda (str)
;;       (+str-replace-all
;;        (append
;;         (list
;;          (cons "${workspaceFolder}" ws-root)
;;          (cons "${workspaceFolderBasename}" ws-basename)
;;          (cons "${userHome}" (or (getenv "HOME") (expand-file-name "~")))
;;          (cons "${pathSeparator}" (if (memq system-type
;;                                             '(windows-nt ms-dos cygwin))
;;                                       "\\" "/"))
;;          (cons "${selectedText}" (if (use-region-p)
;;                                      (buffer-substring-no-properties
;;                                       (region-beginning) (region-end)) "")))
;;         ;; To avoid problems if launched from a non-file buffer
;;         (when curr-file
;;           (list
;;            (cons "${file}" curr-file)
;;            (cons "${relativeFile}" (file-relative-name curr-file ws-root))
;;            (cons "${relativeFileDirname}" (file-relative-name
;;                                            (file-name-directory curr-file) ws-root))
;;            (cons "${fileBasename}" (file-name-nondirectory curr-file))
;;            (cons "${fileBasenameNoExtension}" (file-name-base curr-file))
;;            (cons "${fileDirname}" (file-name-directory curr-file))
;;            (cons "${fileExtname}" (file-name-extension curr-file))
;;            (cons "${lineNumber}" (line-number-at-pos (point) t)))))
;;        str))
;;     (cons program args))))
;;
;;(defun launch-json--debugger-params (type)
;;  (let* ((front/backend
;;          (cond ((string-match launch-json--realgud-debugger-regex type)
;;                 (cons 'realgud (intern (match-string 1 type))))
;;                ((string-match launch-json--gud-debugger-regex type)
;;                 (cons 'gud (intern (match-string 1 type))))
;;                (t
;;                 (cons 'unknown 'unknown))))
;;         (frontend (car front/backend))
;;         (backend (cdr front/backend))
;;         (cmd-sym (unless (eq frontend 'unknown)
;;                    (intern (format (cond ((eq frontend 'gud) "gud-%s-%s")
;;                                          ((eq frontend 'realgud) "%s-%s")
;;                                          (t "%s-%s"))
;;                                    type
;;                                    "command-name")))))
;;    (message "[launch-json:params]: Found type: %s -> { frontend: %s | backend: %s }"
;;             type (symbol-name frontend) (symbol-name backend))
;;    (cond ((memq backend '(gud-gdb gdb))
;;           ;; Special case for '(gud . gdb), uses `gdb-mi'
;;           (let ((use-gdb-mi (equal front/backend '(gud . gdb))))
;;             `(:type ,type
;;               :debug-cmd ,(if use-gdb-mi 'gdb (intern type))
;;               :args-format " --args %s %s"
;;               :cmd ,cmd-sym
;;               :require ,(if use-gdb-mi 'gdb-mi frontend))))
;;          ((eq backend 'lldb)
;;           `(:type ,type
;;             :debug-cmd ,(intern type)
;;             :args-format " -- %s %s"
;;             :cmd ,cmd-sym
;;             :require ,(intern (if (eq frontend 'realgud)
;;                                   (+str-replace-all '(("--" . "-") (":" . "-")) type)
;;                                 type))))
;;          (t ;; TODO: to be expanded for each debugger
;;           `(:type ,type
;;             :debug-cmd ,(intern type)
;;             :args-format " %s %s"
;;             :cmd ,(if (equal front/backend '(realgud . ipdb)) 'realgud--ipdb-command-name cmd-sym)
;;             :require ,(cond ((equal front/backend '(realgud . trepan-ni)) 'realgud-trepan-ni)
;;                             (t frontend)))))))
;;
;;(defun launch-json--debug-command (params debuggee-args)
;;  "Return the debug command for PARAMS with DEBUGGEE-ARGS."
;;  (when-let* ((prog (car debuggee-args))
;;              (cmd (plist-get params :cmd))
;;              (pkg (plist-get params :require)))
;;    (if (or (not pkg) (eq pkg 'unknown))
;;        (progn (message "[launch-json:command]: Unknown debugger")
;;               nil)
;;      (if (require (plist-get params :require) nil t)
;;          (let ((args (+str-join " " (cdr debuggee-args))))
;;            (when args (setq args (format (plist-get params :args-format) prog args)))
;;            (if (bound-and-true-p cmd)
;;                (concat (eval cmd) (if args args ""))
;;              (message "[launch-json:command]: Invalid command for type %s" (plist-get params :type))
;;              nil))
;;        (message "[launch-json:command]: Cannot add package %s" (symbol-name pkg))
;;        nil))))
;;
;;(defun launch-json-read (&optional file)
;;  "Return the configurations section from a launch.json FILE.
;;If FILE is nil, launch.json will be searched in the current project,
;;if it is set to a launch.json file, it will be used instead."
;;  (let ((launch-json (expand-file-name (or file "launch.json") (or (projectile-project-root) "."))))
;;    (when (file-exists-p launch-json)
;;      (message "[launch-json]: Found \"launch.json\" at %s" launch-json)
;;      (let* ((launch (with-temp-buffer
;;                       (insert-file-contents launch-json)
;;                       (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil :false-object nil)))
;;             (configs (plist-get launch :configurations)))
;;        (+filter (lambda (conf)
;;                   (or (string-match-p launch-json--gud-debugger-regex (plist-get conf :type))
;;                       (string-match-p launch-json--realgud-debugger-regex (plist-get conf :type))))
;;                 configs)))))
;;
;;(defun launch-json--config-choice (&optional file)
;;  (let* ((confs (or (launch-json-read file)
;;                    +launch-json-debug-config))
;;         (candidates (mapcar (lambda (conf)
;;                               (cons (format "%s [%s]" (plist-get conf :name) (plist-get conf :type))
;;                                     conf))
;;                             confs)))
;;    (cond ((eq (length confs) 1)
;;           (car confs))
;;          ((> (length confs) 1)
;;           (cdr (assoc (completing-read "Configuration: " candidates) candidates))))))
;;
;;(defun launch-json-debug (&optional file)
;;  "Launch RealGUD or GDB with parameters from `+launch-json-debug-config' or launch.json file."
;;  (interactive)
;;  (let* ((conf (or launch-json--last-config
;;                   (launch-json--config-choice file)))
;;         (args (launch-json--substite-special-vars (plist-get conf :program) (plist-get conf :args)))
;;         (type (plist-get conf :type))
;;         (params (launch-json--debugger-params type)))
;;    (when params
;;      (let ((debug-cmd (plist-get params :debug-cmd)))
;;        (when (fboundp debug-cmd)
;;          (setq-local launch-json--last-config conf)
;;          (funcall debug-cmd
;;                   (launch-json--debug-command params args)))))))
;;
;;(map! :leader :prefix ("l" . "custom")
;;      (:when (modulep! :tools debugger)
;;       :prefix ("d" . "debugger")
;;       :desc "GUD/RealGUD launch.json" "d" #'launch-json-debug))






(defun dw/show-server-edit-buffer (buffer)
  ;; TODO: Set a transient keymap to close with 'C-c C-c'
  (split-window-vertically -15)
  (other-window 1)
  (set-buffer buffer))

(setq server-window #'dw/show-server-edit-buffer)

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(use-package embark
  :ensure t
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config

  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;; (use-package embark-consult
;;   :straight '(embark-consult :host github
;;                              :repo "oantolin/embark"
;;                              :files ("embark-consult.el"))
;;   :after (embark consult)
;;   :demand t
;;   :hook
;;   (embark-collect-mode . embark-consult-preview-minor-mode))
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(setq evil-vsplit-window-right t
      evil-split-window-below t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist))

 ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7)

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package wgrep)

(use-package ivy-posframe
  :disabled
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))


(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))
(setq split-width-threshold 0)
(setq split-height-threshold nil)


(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c h" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents")
    (setq projectile-project-search-path '("~/Documents")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))



(defun org-babel-execute:chess (body params)
  "Execute a block of Chess code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((output-file (cdr (assq :file params)))
         (notation (cdr (assq :notation params)))
         (extension (if (equal notation "fen") ".fen" ".pgn"))
         (notation-file (make-temp-file "chess-notation" nil extension))
         (cmd (format "python ~/.config/elchess/elchess.py %s %s %s" notation-file output-file notation)))
    (with-temp-buffer
      (insert body)
      (write-file notation-file))
    (shell-command cmd)
    (org-babel-result-to-file output-file)))

(setq org-babel-default-header-args:chess
      '((:results . "raw")))

(require 'simple-httpd)
(setq httpd-root "/var/www")
(httpd-start)





(use-package nix-mode
:mode "\\.nix\\'")

(use-package toml-mode
  :mode "\\.toml\\'")

;;(setq per-buffer-theme-use-timer t)
;;(setq per-buffer-theme-timer-idle-delay 0.1)
;;(setq per-buffer-theme-default-theme 'doom-palenight)
;;(setq per-buffer-theme-themes-alist
;;      '(((:theme . doom-1337)
;;         (:buffernames . ("*.cu"))
;;         (:modes . (cuda-mode)))
;;      ((:theme . leuven)
;;       (:bufernames .("*mu4e" "*mu4e-main*" "*.org" "*draft*"))
;;       (:modes . (mu4e-headers-mode org-msg-mode mu4e-main-mode org-msg-edit-mode draft org-msg-mode-mu4e mu4e-compose-mode mu4e-view-mode org-org mode-mu4e-mode mu4e-loading-mode mu4e-update-mode)))))
;;(per-buffer-theme-mode)


(use-package mu4e
  :ensure nil
  :defer 10 ; Wait until 10 seconds after startup
  :config
(use-package org-mime)
  (setq sendmail-program (executable-find "msmtp")
      send-mail-function #'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from"); , "--read-recipients")
      message-send-mail-function #'message-send-mail-with-sendmail)

  (setq mu4e-compose-format-flowed t)

  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-update-interval (* 45 60))
  (add-to-list 'mu4e-view-actions
	       '("View in browser" . mu4e-action-view-in-browser) t)

  (set-email-account! "gmail"
          '((mu4e-drafts-folder  . "/[Google Mail]/Entw&APw-rfe")
           (mu4e-sent-folder  . "/[Google Mail]/Gesendet")
           (mu4e-refile-folder  . "/Archiv ")
           (smtpmail-smtp-user . "hermannschris@gmail.com")
           (mu4e-trash-folder  . "/[Google Mail]/Papierkorb")
           (mu4e-compose-signature . "Christoph via Gmail"))t)

         ;; Other Account
         ;; (make-mu4e-context
         ;; :name "Personal"
         ;; :match-func
         ;;     (lambda (msg)
         ;;     (when msg
         ;;         (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
         ;; :vars '((user-mail-address . "systemcrafterstest@fastmail.com")
         ;;         (user-full-name    . "System Crafters Fastmail")
         ;;         (smtpmail-smtp-server  . "smtp.fastmail.com")
         ;;         (smtpmail-smtp-service . 465)
         ;;         (smtpmail-stream-type  . ssl)
         ;;         (mu4e-compose-signature . "David via Fastmail")
         ;;         (mu4e-drafts-folder  . "/Fastmail/Drafts")
         ;;         (mu4e-sent-folder  . "/Fastmail/Sent")
         ;;         (mu4e-refile-folder  . "/Fastmail/Archive")
         ;;         (mu4e-trash-folder  . "/Fastmail/Trash")))
         ;;
          

(setq mu4e-mail-dir-shortcuts
 '((:maildir "/Inbox" :key ?i)
   (:maildir "/[Google Mail]/Alle Nachrichten" :key ?a)
   (:maildir "/[Google Mail]/Papierkorb" :key ?d)
   (:maildir "/[Google Mail]/drafts" :key ?D)
   (:maildir "/[Google Mail]/Wichtig" :key ?i)
   (:maildir "/[Google Mail]/Gesendet" :key ?s)
   (:maildir "/[Google Mail]/Markiert" :key ?S)))

(setq mu4e-bookmarks
  '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?i)
    (:name "Today's messages" :query "date:today..now" :key ?t)
    (:name "Mutter" :query "from:yeshe-dawa" :key ?s)
    (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
    (:name "Messages with images" :query "mime:image/*" :key ?p)))
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(mu4e t)
 )




;;sending mails
;;
(envrc-global-mode)
(setq mastodon-instance-url "https://fem.social/explore"
          mastodon-active-user "cherma")


(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode ) .
         (lambda () (require 'ccls) (lsp))))



(use-package! evil-tree-edit
  :hook (python-mode . evil-tree-edit-mode))


(use-package! carbon-now-sh
  :config
  (defun yeet/carbon-use-eaf ()
    (interactive)
    (split-window-right)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url (concat carbon-now-sh-baseurl "?code="
                          (url-hexify-string (carbon-now-sh--region))))))
  (map! :n "g C-c" #'yeet/carbon-use-eaf))

(setq dired-dwim-target t)

(add-hook! 'dired-mode-hook #'dired-hide-details-mode)

(use-package! aas
  :commands aas-mode)

(load! "lisp/org-roam-logseq")

(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :config
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "E" (cmd! (lexic-return-from-lexic) ; expand
                     (switch-to-buffer (lexic-get-buffer)))
        :n "M" (cmd! (lexic-return-from-lexic) ; minimise
                     (lexic-goto-lexic))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))


(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))



(use-package! wttrin
  :commands wttrin)


;;(use-package! calibredb
;;  :defer t
;;  :config
;;  (setq calibredb-root-dir "~/Documents/reading/calibre"
;;        calibredb-db-dir   (expand-file-name "metadata.db" calibredb-root-dir))
;;  ;; the view for all books
;;  (map! :map calibredb-search-mode-map
;;        :ne "?" #'calibredb-entry-dispatch
;;        :ne "a" nil
;;        :ne "a" #'calibredb-add
;;        :ne "A" nil
;;        :ne "A" #'calibredb-add-dir
;;        :ne "." #'calibredb-open-dired
;;        :ne "e" #'calibredb-export-dispatch
;;        :ne "m" #'calibredb-mark-at-point
;;        :ne "o" #'calibredb-find-file
;;        :ne "O" #'calibredb-find-file-other-frame
;;        :ne "q" #'calibredb-search-quit
;;        :ne "s" nil
;;        :ne "s" #'calibredb-sort-dispatch
;;        :ne "S" #'calibredb-set-metadata-dispatch
;;        :ne "u" #'calibredb-unmark-at-point
;;        :ne "V" #'calibredb-open-file-with-default-tool
;;        :ne [tab] #'calibredb-toggle-view-at-point)
;;  ;; the veiw for one book
;;  (map! :map calibredb-show-mode-map
;;        :ne [mouse-3] #'calibredb-search-mouse
;;        :ne "RET" #'calibredb-find-file
;;        :ne "?" #'calibredb-dispatch
;;        :ne "a" #'calibredb-add
;;        :ne "A" #'calibredb-add-dir
;;        :ne "c" #'calibredb-clone
;;        :ne "d" #'calibredb-remove
;;        :ne "D" #'calibredb-remove-marked-items
;;        :ne "j" #'calibredb-next-entry
;;        :ne "k" #'calibredb-previous-entry
;;        :ne "l" #'calibredb-virtual-library-list
;;        :ne "L" #'calibredb-library-list
;;        :ne "n" #'calibredb-virtual-library-next
;;        :ne "N" #'calibredb-library-next
;;        :ne "p" #'calibredb-virtual-library-previous
;;        :ne "P" #'calibredb-library-previous
;;        :ne "s" #'calibredb-set-metadata-dispatch
;;        :ne "S" #'calibredb-switch-library
;;        :ne "o" #'calibredb-find-file
;;        :ne "O" #'calibredb-find-file-other-frame
;;        :ne "v" #'calibredb-view
;;        :ne "V" #'calibredb-open-file-with-default-tool
;;        :ne "." #'calibredb-open-dired
;;        :ne "b" #'calibredb-catalog-bib-dispatch
;;        :ne "e" #'calibredb-export-dispatch
;;        :ne "r" #'calibredb-search-refresh-and-clear-filter
;;        :ne "R" #'calibredb-search-clear-filter
;;        :ne "q" nil
;;        :ne "q" #'calibredb-search-quit
;;        :ne "m" #'calibredb-mark-and-forward
;;        :ne "f" #'calibredb-toggle-favorite-at-point
;;        :ne "x" #'calibredb-toggle-archive-at-point
;;        :ne "h" #'calibredb-toggle-highlight-at-point
;;        :ne "u" #'calibredb-unmark-and-forward
;;        :ne "i" #'calibredb-edit-annotation
;;        :ne "DEL" #'calibredb-unmark-and-backward
;;        :ne [backtab] #'calibredb-toggle-view
;;        :ne [tab] #'calibredb-toggle-view-at-point
;;        :ne "M-n" #'calibredb-show-next-entry
;;        :ne "M-p" #'calibredb-show-previous-entry
;;        :ne "/" #'calibredb-search-live-filter
;;        :ne "M-t" #'calibredb-set-metadata--tags
;;        :ne "M-a" #'calibredb-set-metadata--author_sort
;;        :ne "M-A" #'calibredb-set-metadata--authors
;;        :ne "M-T" #'calibredb-set-metadata--title
;;        :ne "M-c" #'calibredb-set-metadata--comments))
;;
;;(defun +book/quit ())
;;
;;(defun =book ()
;;  (interactive)
;;  (if (modulep! :ui workspaces)
;;      (progn
;;        (+workspace-switch "*book*" t)
;;        (doom/switch-to-scratch-buffer)
;;        (calibredb)
;;        (+workspace/display))
;;    (calibredb)))
;;
;;;; I read books more than I read files in my buffer
;;(map! :leader
;;      "ob" nil
;;      "ob" #'=book
;;      "oB" #'browse-url-of-file)


;;(use-package! nov
;;  :mode ("\\.epub\\'" . nov-mode)
;;  :config
;;  (add-hook! 'nov-mode-hook #'olivetti-mode ;; Centers the text making it easier to read
;;    (mixed-pitch-mode +1)
;;    (defun yeet/nov-setup ()
;;      (setq-local olivetti-body-width 125))))
;;
;;(after! olivetti)

;; configure nix-path first!
;;(use-package languagetool
;;  :ensure t
;;  :defer t
;;  :commands (languagetool-check
;;             languagetool-clear-suggestions
;;             languagetool-correct-at-point
;;             languagetool-correct-buffer
;;             languagetool-set-language
;;             languagetool-server-mode
;;             languagetool-server-start
;;             languagetool-server-stop)
