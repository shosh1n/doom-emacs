(setq user-full-name "cherma"
      user-mail-address "hermannschris@gmail.com")

(eval-when-compile
  (require 'use-package))
;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!
;; You will most likely need to adjust this font size for your system!
(setq inhibit-startup-message t)
(defvar efs/default-font-size 80)
(defvar efs/default-variable-font-size 80)

;; Make frame transparency overridable
;; doesnt work on wayland
(defvar efs/frame-transparency '(90 . 80))


;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

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
(setq doom-font (font-spec :family "JetBrainsMonoNL Nerd Font Mono" :size 16))
(setq doom-unicode-font (font-spec :family "JetBrainsMonoNL Nerd Font Mono"))
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(use-package doom-themes
  :init (load-theme 'doom-palenight t))

;;(load! "lisp/org-roam-logseq")


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

(after! all-the-icons
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))

(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))

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

(setq doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)

(setq treemacs-show-hidden-files nil
      treemacs-hide-dot-git-directory t
      treemacs-width 30)

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
;;; :checks grammar
(after! langtool
  (setq langtool-bin "languagetool-commandline"))


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



(setq! citar-library-paths '("~/Dropbox/papers/")
       citar-bibliography '("~/Dropbox/reference/book.bib")
       citar-notes-paths '("~/Dropbox/roam/notes"))
;;
;;org
;;
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

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

;;using it for scrlttr2
(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
           '("report-noparts"
              "\\documentclass{report}
        [NO-DEFAULT-PACKAGES]
        [PACKAGES]
        [EXTRA]"
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))



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

(use-package! org-transclusion
  :after org)

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
(mu4e t))

(after! elfeed-search
  (map! :map elfeed-search-mode-map
        :localleader
        :n "m" #'my/elfeed-search-view-hydra/body
        :n "s" #'elfeed-toggle-star
        :n "v" #'elfeed-view-mpv
        :n "r" #'elfeed-update))
;; Set max width
(after! elfeed
  (setq elfeed-search-title-max-width 120
        elfeed-search-filter "@1-week-ago--1-day-ago -youtube"))

(use-package! graphviz-dot-mode)
;;
;;Mastodon
(use-package! mastodon
  :init
  (setq mastodon-instance-url "https://fem.social/explore"
        mastodon-active-user "cherma"))

(use-package! conf-data-toml
  :magic ("\\`data_config_version = [0-9]" . conf-data-toml-mode))

(use-package! chatgpt-shell
  :init
  (setq! chatgpt-shell-openai-key
         (lambda ()
           (auth-source-pick-first-password :host "api.openai.com"))
         chatgpt-shell-chatgpt-streaming t))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ;; accept completion from copilot and fallback to company
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq copilot-idle-delay 2))

;;
;;; Custom Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((ssh-deploy-async-with-threads . 1)
     (ssh-deploy-on-explicity-save . t)
     (ssh-deploy-async . 1))))

(setq enable-local-variables :all)

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

(add-hook! 'dired-mode-hook #'dired-hide-details-mode)

(use-package! aas
  :commands aas-mode)
(require 'virtualenvwrapper)

(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(setq venv-location "/home/shoshin/micromamba/envs/")
;;(load! "academic/academic")
(setq rustic-lsp-server 'rust-analyzer)

;;; :tools lsp
;; Disable invasive lsp-mode features
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some systems I don't care to have a
        ;; whole development environment for some ecosystems.
        lsp-enable-server-download nil
        lsp-enable-suggest-server-download nil))
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        lsp-ui-doc-enable nil))     ; redundant with K

;;; :tools magit
(setq magit-repository-directories '(("~/Projects" . 3))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-commit "--gpg-sign=AE891E7F56DC7789")
                         (magit-rebase "--autosquash" "--autostash" "--gpg-sign=AE891E7F56DC7789")
                         (magit-pull "--rebase" "--autostash" "--gpg-sign=AE891E7F56DC7789")))

;; Enable git gutter on tramp sessions
(defun +version-control|git-gutter-maybe ()
  (when buffer-file-name
    (require 'git-gutter-fringe)
    (git-gutter-mode +1)))


(envrc-global-mode)
