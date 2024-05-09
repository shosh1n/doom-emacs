(setq user-full-name "christoph"
      user-mail-address "c-a.hermanns@sielaff.de")

(eval-when-compile
  (require 'use-package))
;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!
;; You will most likely need to adjust this font size for your system!
;;(setq fancy-splash-image "~/.config/doom/Header_Sielaff_Guide_2024_EN.jpg")
(setq inhibit-startup-message t)
(defvar efs/default-font-size 80)
(defvar efs/default-variable-font-size 80)

;; Make frame transparency overridable
;; doesnt work on hyprland
;;(defvar efs/frame-transparency '(90 . 80))


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
;;(require 'lilypond)
;; Initialize use-package on non-Linux platforms
;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
                                        ;(setq user-emacs-directory "~/.cache/emacs")
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;;(column-number-mode)
;;(global-display-line-numbers-mode t)

;; Set frame transparency
;;(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;;(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;set fonts
(set-face-attribute 'default nil :font "JetBrains Mono" :height efs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height efs/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height efs/default-variable-font-size :weight 'regular)
(setq doom-font (font-spec :family "JetBrains Mono" :size 16))
(setq doom-unicode-font (font-spec :family "JetBrains Mono NL"))
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;load this
(add-to-list 'load-path "/home/hermanns/.config/emacs/.local/straight/repos/tree-sitter-langs/")
;;(use-package doom-themes
;;  :init (load-theme 'doom-henna nil))

;;(defun ivy-rich-switch-buffer-icon (candidate)
;;  (with-current-buffer
;;      (get-buffer candidate)
;;    (let ((icon (nerd-icons-dired-mode major-mode)))
;;      (if (symbolp icon)
;;          (nerd-icons-dired-mode 'fundamental-mode)
;;        icon))))
;;
;;(setq ivy-rich-display-transformers-list
;;      '(ivy-switch-buffer
;;        (:columns
;;         ((ivy-rich-switch-buffer-icon (:width 2))
;;          (ivy-rich-candidate (:width 30))
;;          (ivy-rich-switch-buffer-size (:width 7))
;;          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
;;          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
;;          (ivy-rich-switch-buffer-project (:width 15 :face success))
;;          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
;;         :predicate
;;         (lambda (cand) (get-buffer cand)))))
;;(load! "lisp/org-roam-logseq")
(setq org-roam-directory "~/.org")

(setq dired-dwim-target t)
(setq markdown-split-window-direction 'right)

(setq auth-sources '("~/.ssh/id_rsa.pub")
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
;;(use-package evil-textobj-tree-sitter :ensure t)
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

;;(after! all-the-icons
;;  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
;;          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))

(define-key global-map (kbd "C-c j")
            (lambda () (interactive) (org-capture nil "jj")))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))



(after! treemacs
  (require 'dired)
  (follow-mode 1))
;;
;;;; My custom stuff (from tecosaur's config)
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

;;(setq doom-themes-treemacs-enable-variable-pitch nil
;;      doom-themes-treemacs-theme "doom-colors")
;;(doom-themes-treemacs-config)
;;
(setq treemacs-show-hidden-files nil
      treemacs-hide-dot-git-directory t
      treemacs-width 46
      )


(after! eglot
  ;; A hack to make it works with projectile
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))

  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function))

  ;; Use clangd with some options
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (set-eglot-client! 'c++-mode '("clangd" "-j=3" "--clang-tidy"))
  (set-eglot-client! 'python-ts-mode  '("pylsp")))

;;(setq exec-path (append exec-path '(
;;                                    (concat (getenv "HOME") "/.micromamba/envs/pybottlefigure/bin/")))))
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;(setq flycheck-python-flake8-executable "flake8")
(setq-default flycheck-disabled-checkers '(python-pylint))

(after! flycheck
  (setq flycheck-cppcheck-checks '("information"
                                   "performance"
                                   "portability"
                                   "style"
                                   "unusedFunction"
                                   "warning")))
;;having tabs for emacs-buffer
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
      (display-line-numbers-mode +line-num--was-activate-p))))

;;showing indentation lines
(after! highlight-indent-guides
  (setq highlight-indent-guides-character ? highlight-indent-guides-responsive 'top))

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

(use-package! focus
  :commands focus-mode)

;;
;;org
;;

(setq org-time-stamp-formats '("<%Y-%m-%d %a %H:%M>" . "<%Y-%m-%d %a %H:%M:%S>"))

(define-key global-map (kbd "C-c j")
            (lambda () (interactive) (org-capture nil "jj")))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (global-set-key "\C-cb" 'org-switchb)
  (require 'org-tempo)
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive|txt\\)$" . org-mode))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  )

(use-package  org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list'("◉" "○" "●" "○" "●" "○" "●")))

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
    (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'medium :height (cdr face))))

(setq orgStuff '(
                 (org-indent-mode)
                 (variable-pitch-mode 1)
                 (auto-fill-mode 0)
                 (visual-line-mode 1)
                 (setq evil-auto-indent nil)))

(after! org
  (setq org-beamer-theme "[progressbar=foot]metropolis"
        org-beamer-frame-level 2))

(setq org-image-actual-width nil)
(after! org
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-agenda-files (quote ("~/.org")))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
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

;;(use-package marginalia
;;  :ensure t
;;  :config
;;  (marginalia-mode))

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

;;beautiful images of my code thanks!
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

(use-package! graphviz-dot-mode)
(setq magit-repository-directories '(("~/src". 3))
      magit-save-repository-buffers nil
      magit-inhibit-save-previous-winconf t)

(use-package! aas
  :commands aas-mode)

(after! langtool
  (setq langtool-language-tool-jar "/home/hermanns/.config/LanguageTool-6.1/"))

(defun +version-control|git-gutter-maybe ()
  (when buffer-file-name
    (require 'git-gutter-fringe)
    (git-gutter-mode +1)))


(after! magit
  ;; Show gravatars
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

;;(use-package! magit-pretty-graph
;;  :after magit
;;  :init
;;  (setq magit-pg-command
;;        (concat "git --no-pager log"
;;                " --topo-order --decorate=full"
;;                " --pretty=format:\"%H%x00%P%x00%an%x00%ar%x00%s%x00%d\""
;;                " -n 2000")) ;; Increase the default 100 limit
;;
;;  (map! :localleader
;;        :map (magit-mode-map)
;;        :desc "Magit pretty graph" "p" (cmd! (magit-pg-repo (magit-toplevel)))))

;;------cpp-dev----->
;;(setq dap-auto-configure-mode t)

;;(which-key-mode)
;;add-hook 'c-mode-hook 'lsp)
;;(add-hook 'c++-mode-hook 'lsp)
;;(use-package-hook! lsp-mode
;;  :post-config
;;  (setq lsp-enable-on-type-formatting nil))
;;(setq +lsp-company-backends '(company-tabnine :separate company-yasnippet))
;;(with-eval-after-load 'lsp-mode
;;  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;  (require 'dap-cpptools)
;;  (setq dtrt-mode nil)
;;  (setq lsp-enable-indentation nil)
;;  (setq lsp-clients-clangd-args
;;    '("--header-insertion=never"))
;;  (setq c-syntactic-indentation nil)
;;  (setq c-indentation-style nil)
;;  (setq c-syntactic-indentation-in-macros nil)
;;  (setq lsp-enable-on-type-formatting nil)
;;  (setq lsp-clients-clangd-args '("--header-insertion=never")))
;;;;(use-package pyenv-mode
;;  :init
;;  (add-to-list 'exec-path "~/mambaforge/envs/pybottlefigure")
;;  (setenv "WORKON_HOME" "~/mambaforge/versions/")
;;  :config
;;  (pyenv-mode)
;;  :bind
;;  ("C-x p e" . pyenv-activate-current-project))


;;(pyvenv-mode)
;;(require 'pyenv-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
;;(use-package conda
;;  :ensure t
;;  :init
;;  (setq conda-anaconda-home (expand-file-name "~/mambaforge"))
;;  (setq conda-env-home-directory (expand-file-name "~/mambaforge")))

(use-package! modus-themes
  :init
  (setq modus-themes-hl-line '(accented intense)
        modus-themes-subtle-line-numbers t
        modus-themes-region '(bg-only no-extend) ;; accented
        modus-themes-variable-pitch-ui nil
        modus-themes-fringes 'subtle
        modus-themes-diffs nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-intense-mouseovers t
        modus-themes-paren-match '(bold intense)
        modus-themes-syntax '(green-strings)
        modus-themes-links '(neutral-underline background)
        modus-themes-mode-line '(borderless padded)
        modus-themes-tabs-accented nil ;; default
        modus-themes-completions
        '((matches . (extrabold intense accented))
          (selection . (semibold accented intense))
          (popup . (accented)))
        modus-themes-headings '((1 . (rainbow 1.4))
                                (2 . (rainbow 1.3))
                                (3 . (rainbow 1.2))
                                (4 . (rainbow bold 1.1))
                                (t . (rainbow bold)))
        modus-themes-org-blocks 'gray-background
        modus-themes-org-agenda
        '((header-block . (semibold 1.4))
          (header-date . (workaholic bold-today 1.2))
          (event . (accented italic varied))
          (scheduled . rainbow)
          (habit . traffic-light))
        modus-themes-markup '(intense background)
        modus-themes-mail-citations 'intense
        modus-themes-lang-checkers '(background))

  (defun +modus-themes-tweak-packages ()
    (modus-themes-with-colors
      (set-face-attribute 'cursor nil :background (modus-themes-color 'blue))
      (set-face-attribute 'font-lock-type-face nil :foreground (modus-themes-color 'magenta-alt))
      (custom-set-faces
       ;; Tweak `evil-mc-mode'
       `(evil-mc-cursor-default-face ((,class :background ,magenta-intense-bg)))
       ;; Tweak `git-gutter-mode'
       `(git-gutter-fr:added ((,class :foreground ,green-fringe-bg)))
       `(git-gutter-fr:deleted ((,class :foreground ,red-fringe-bg)))
       `(git-gutter-fr:modified ((,class :foreground ,yellow-fringe-bg)))
       ;; Tweak `doom-modeline'
       `(doom-modeline-evil-normal-state ((,class :foreground ,green-alt-other)))
       `(doom-modeline-evil-insert-state ((,class :foreground ,red-alt-other)))
       `(doom-modeline-evil-visual-state ((,class :foreground ,magenta-alt)))
       `(doom-modeline-evil-operator-state ((,class :foreground ,blue-alt)))
       `(doom-modeline-evil-motion-state ((,class :foreground ,blue-alt-other)))
       `(doom-modeline-evil-replace-state ((,class :foreground ,yellow-alt)))
       ;; Tweak `diff-hl-mode'
       `(diff-hl-insert ((,class :foreground ,green-fringe-bg)))
       `(diff-hl-delete ((,class :foreground ,red-fringe-bg)))
       `(diff-hl-change ((,class :foreground ,yellow-fringe-bg)))
       ;; Tweak `solaire-mode'
       `(solaire-default-face ((,class :inherit default :background ,bg-alt :foreground ,fg-dim)))
       `(solaire-line-number-face ((,class :inherit solaire-default-face :foreground ,fg-unfocused)))
       `(solaire-hl-line-face ((,class :background ,bg-active)))
       `(solaire-org-hide-face ((,class :background ,bg-alt :foreground ,bg-alt)))
       ;; Tweak `display-fill-column-indicator-mode'
       `(fill-column-indicator ((,class :height 0.3 :background ,bg-inactive :foreground ,bg-inactive)))
       ;; Tweak `mmm-mode'
       `(mmm-cleanup-submode-face ((,class :background ,yellow-refine-bg)))
       `(mmm-code-submode-face ((,class :background ,bg-active)))
       `(mmm-comment-submode-face ((,class :background ,blue-refine-bg)))
       `(mmm-declaration-submode-face ((,class :background ,cyan-refine-bg)))
       `(mmm-default-submode-face ((,class :background ,bg-alt)))
       `(mmm-init-submode-face ((,class :background ,magenta-refine-bg)))
       `(mmm-output-submode-face ((,class :background ,red-refine-bg)))
       `(mmm-special-submode-face ((,class :background ,green-refine-bg))))))

  :config
  (load-theme 'doom-henna :no-confirm)
  (map! :leader
        :prefix "t" ;; toggle
        :desc "Toggle Modus theme" "m" #'modus-themes-toggle))
(setq lsp-log-io nil)
(after! ccls
  (setq ccls-initialization-options
        '(:index (:comments 2
                  :trackDependency 1
                  :threads 4)
          :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2))

;;(use-package! conventional-commit
;;  :after (magit company)
;;  :config
;;  (add-hook
;;   'git-commit-setup-hook
;;   (lambda ()
;;     (add-to-list 'company-backends 'company-conventional-commits))))

(use-package! blamer
  :commands (blamer-mode)
  ;; :hook ((prog-mode . blamer-mode))
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 60)
  (blamer-prettify-time-p t)
  (blamer-entire-formatter "    %s")
  (blamer-author-formatter " %s ")
  (blamer-datetime-formatter "[%s], ")
  (blamer-commit-formatter "“%s”")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 80
                   :italic t))))

;;(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(use-package combobulate
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook ((python-ts-mode . combobulate-mode)
         (js-mode . combobulate-mode)
         (css-mode . combobulate-mode)
         (yaml-mode . combobulate-mode)
         (typescript-mode . combobulate-mode)
         (tsx-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  :load-path ("/home/hermanns/.config/emacs/addOns/combobulate"))
(defun open-remote-file()
  "open a remote file using tramp."
  (interactive)
  (find-file (concat "/ssh:hermanns@gpu:/home/hermanns" (read-string "Enter file path: ")))
  (message "connecting to gpu-server ...!"))

(defun hc/open-work-folder()
  (interactive)
  (dired "~/src/__workbench/__scratch")
  (message  "%s to working directory!"
            (nth (random 7) '(meandering walking hiking going pathing going Si-lining))
            )
  )

(evil-global-set-key 'normal (kbd "SPC o w") 'hc/open-work-folder)

(evil-global-set-key 'normal (kbd "SPC o g") 'open-remote-file)

;;dired
;;(add-hook! 'dired-mode-hook #'dired-hide-details-mode)
;;(use-package! tree-sitter
;;  :config
;;  (require 'tree-sitter-langs)
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
;;(setq treesit-language-source-alist
;;   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;     (cmake "https://github.com/uyha/tree-sitter-cmake")
;;     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;     (html "https://github.com/tree-sitter/tree-sitter-html")
;;     (json "https://github.com/tree-sitter/tree-sitter-json")
;;     (make "https://github.com/alemuller/tree-sitter-make")
;;     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;     (python "https://github.com/tree-sitter/tree-sitter-python")
;;     (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;     (c "https://github.com/tree-sitter/tree-sitter-c")
;;     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;(add-to-list 'load-path "/path-to/emacs-tree-sitter/langs")
;;(setq treesit-extra-load-path '("/home/hermanns/.config/emacs/.local/cache/tree-sitter/"))
;;(push '(python-mode . python-ts-mode) major-mode-remap-alist)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(defun python-ts-mode-setup ()
  (treesit-font-lock-recompute-features
   '(function variable)'(definition)))
(add-hook 'python-ts-mode-hook #'python-ts-mode-setup)
(require 'iso-transl)
;;(add-to-list 'major-mode-remap-alist '(cpp-mode . cpp-ts-mode))
;;(defun cpp-ts-mode-setup ()
;;  (treesit-font-lock-recompute-features
;;   '(function variable)'(definition)))
;;(add-hook 'cpp-ts-mode-hook #'cpp-ts-mode-setup)

;;
;;(use-package rtags
;;  :ensure nil
;;  :hook (c++-mode . rtags-start-process-unless-running)
;;  :config (setq rtags-completions-enabled t
;; rtags-path "/home/hermanns/.config/emacs/modules/ide/rtags/src/rtags.el"
;; rtags-rc-binary-name "/home/hermanns/.config/emacs/modules/ide/rtags/bin/rc"
;; rtags-use-helm t
;; rtags-rdm-binary-name "/home/hermanns/.config/emacs/modules/ide/rtags/bin/rdm")
;;  :bind (("C-c E" . rtags-find-symbol)
;;   ("C-c e" . rtags-find-symbol-at-point)
;;   ("C-c o" . rtags-find-references-at-point)
;;   ("C-c s" . rtags-find-file)
;;   ("C-c v" . rtags-find-virtuals-at-point)
;;   ("C-c F" . rtags-fixit)
;;   ("C-c f" . rtags-location-stack-forward)
;;   ("C-c b" . rtags-location-stack-back)
;;   ("C-c n" . rtags-next-match)
;;   ("C-c p" . rtags-previous-match)
;;   ("C-c P" . rtags-preprocess-file)
;;   ("C-c R" . rtags-rename-symbol)
;;   ("C-c x" . rtags-show-rtags-buffer)
;;   ("C-c T" . rtags-print-symbol-info)
;;   ("C-c t" . rtags-symbol-type)
;;   ("C-c I" . rtags-include-file)
;;   ("C-c i" . rtags-get-include-file-for-symbol))
;;  )
;;(setq rtags-display-result-backend 'helm)
;;(add-hook 'c++-mode-hook
;;          (lambda () (setq flycheck-clang-language-standard "c++11")))

;;(require 'cmake-ide)
;;(cmake-ide-setup)
;; Set cmake-ide-flags-c++ to use C++11
;;(setq cmake-ide-flags-c++ (append '("-std=c++11")))
;;(setq rtags-autostart-diagnostics t)
;;(rtags-diagnostics)

;; Turn flycheck on everywhere
;;(global-flycheck-mode)
                                        ;(rtags-diagnostics)
;;(global-company-mode)
;;(rtags-autostart-diagnostics t)
;;(add-hook 'c++-mode-hook
;;          (lambda () (setq flycheck-clang-language-standard "c++11")))
;;(push 'company-rtags company-backends)
;;;;(envrc-global-mode)

;;(defun my-flycheck-rtags-setup ()
;;  (flycheck-select-checker 'rtags)
;;  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;  (setq-local flycheck-check-syntax-automatically nil)
;;  (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
;;  (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
;;  (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup))
;;(global-company-mode)

(require 'cursory)

(setq cursory-presets
      '((bar . ( :cursor-type (bar . 2)
                              :cursor-in-non-selected-windows hollow
                              :blink-cursor-blinks 10
                              :blink-cursor-interval 0.5
                              :blink-cursor-delay 0.2))

        (box  . ( :cursor-type box
                               :cursor-in-non-selected-windows hollow
                               :blink-cursor-blinks 10
                               :blink-cursor-interval 0.5
                               :blink-cursor-delay 0.2))

        (underscore . ( :cursor-type (hbar . 3)
                                     :cursor-in-non-selected-windows hollow
                                     :blink-cursor-blinks 50
                                     :blink-cursor-interval 0.2
                                     :blink-cursor-delay 0.2))))

(setq cursory-latest-state-file (locate-user-emacs-file "cursory-latest-state"))

(cursory-restore-latest-preset)

;; Set `cursory-recovered-preset' or fall back to desired style from
;; `cursory-presets'.
(if cursory-recovered-preset
    (cursory-set-preset cursory-recovered-preset)
  (cursory-set-preset 'bar))

;; The other side of `cursory-restore-latest-preset'.
(add-hook 'kill-emacs-hook #'cursory-store-latest-preset)

;; We have to use the "point" mnemonic, because C-c c is often the
;; suggested binding for `org-capture'.
(define-key global-map (kbd "C-c p") #'cursory-set-preset)


(setq lsp-completion-enable-additional-text-edit nil)



(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))


(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("ethz"
               "\\documentclass[a4paper,11pt,titlepage]{memoir}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage[ngerman]{babel}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))


(add-to-list 'org-latex-classes '("ebook"
                                  "\\documentclass[11pt, oneside]{memoir}
\\setstocksize{9in}{6in}
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
\\checkandfixthelayout
% Much more laTeX code omitted
"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")))

;;(use-package lsp-mode
;;  :ensure t
;;  :custom
;;  (lsp-headerline-breadcrumb-enable nil)
;;  (lsp-enable-indentation nil)
;;  (lsp-enable-on-type-formatting nil)
;;  (lsp-modeline-code-actions-enable nil)
;;  (lsp-modeline-diagnostics-enable nil)
;;  (lsp-clients-clangd-args '("--header-insertion=never")))











;;
;;
;;
;;(setq user-full-name "cherma"
;;      user-mail-address "hermannschris@gmail.com")
;;
;;(eval-when-compile
;;  (require 'use-package))
;;;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;;;       in Emacs and init.el will be generated automatically!
;;;; You will most likely need to adjust this font size for your system!
;;(setq inhibit-startup-message t)
;;(defvar efs/default-font-size 80)
;;(defvar efs/default-variable-font-size 80)
;;
;;;; Make frame transparency overridable
;;;; doesnt work on hyprland
;;;;(defvar efs/frame-transparency '(90 . 80))
;;
;;
;;;; The default is 800 kilobytes.  Measured in bytes.
;;(setq gc-cons-threshold (* 50 1000 1000))
;;
;;(defun efs/display-startup-time ()
;;  (message "Emacs loaded in %s with %d garbage collections."
;;           (format "%.2f seconds"
;;                   (float-time
;;                     (time-subtract after-init-time before-init-time)))
;;           gcs-done))
;;
;;(add-hook 'emacs-startup-hook #'efs/display-startup-time)
;;;;(require 'lilypond)
;;  ;; Initialize use-package on non-Linux platforms
;;;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;;;; reliably, set `user-emacs-directory` before loading no-littering!
;;;(setq user-emacs-directory "~/.cache/emacs")
;;(scroll-bar-mode -1)        ; Disable visible scrollbar
;;(tool-bar-mode -1)          ; Disable the toolbar
;;(tooltip-mode -1)           ; Disable tooltips
;;(set-fringe-mode 10)        ; Give some breathing room
;;(menu-bar-mode -1)            ; Disable the menu bar
;;
;;;; Set up the visible bell
;;(setq visible-bell t)
;;
;;;;(column-number-mode)
;;;;(global-display-line-numbers-mode t)
;;
;;;; Set frame transparency
;;;;(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;;;;(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;;;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;;;set fonts
;;(set-face-attribute 'default nil :font "JetBrains Mono" :height efs/default-font-size)
;;;; Set the fixed pitch face
;;(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height efs/default-font-size)
;;;; Set the variable pitch face
;;(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height efs/default-variable-font-size :weight 'regular)
;;(setq doom-font (font-spec :family "JetBrainsMonoNL" :size 16))
;;(setq doom-unicode-font (font-spec :family "JetBrainsMonoNL"))
;;;; Make ESC quit prompts
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;
;;
;;(use-package doom-themes
;;  :init (load-theme 'doom-palenight t))
;;
;;(after! centaur-tabs
;;  ;; For some reason, setting `centaur-tabs-set-bar' this to `right'
;;  ;; instead of Doom's default `left', fixes this issue with Emacs daemon:
;;  ;; https://github.com/doomemacs/doomemacs/issues/6647#issuecomment-1229365473
;;  (setq centaur-tabs-set-bar 'under
;;        centaur-tabs-gray-out-icons 'buffer
;;        centaur-tabs-set-modified-marker t
;;        centaur-tabs-close-button "⨂"
;;        centaur-tabs-modified-marker "⨀"
;;        centaur-tabs-style "bar"))
