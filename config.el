(setq user-full-name "christoph"
      user-mail-address "christoph-alexander.hermanns@proton.me")
;;
(eval-when-compile
  (require 'use-package))
;;;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;;;       in Emacs and init.el will be generated automatically!
;;;; You will most likely need to adjust this font size for your system!
(setq inhibit-startup-message t)
(defvar my/default-font-size 80)
(defvar my/default-variable-font-size 80)
;;
(require 'exec-path-from-shell)
(dolist (var '("PATH"))
  (add-to-list 'exec-path-from-shell-variables var))
(when (daemonp)
  (exec-path-from-shell-initialize))
;;;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'my/display-startup-time)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)
;;
;;;;set fonts
(set-face-attribute 'default nil :font "JetBrains Mono" :height my/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height my/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height my/default-variable-font-size :weight 'regular)
(setq doom-font (font-spec :family "JetBrains Mono" :size 16))
(setq doom-symbol-font (font-spec :family "JetBrains Mono NL"))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;
;;;;load this
(add-to-list 'load-path "/home/hermanns/.config/emacs/.local/straight/repos/tree-sitter-langs/")
(add-to-list 'load-path "/home/hermanns/.config/doom/lisp/")
;;;;roam and bib directories
(setq org-roam-directory "~/.org")
(setq bibtex-completion-bibliography
      '("~/.org/arxiv.bib"))
;
(setq bibtex-completion-library-path '("~/Documents/arxiv"))
(setq dired-dwim-target t)
(setq markdown-split-window-direction 'right)
;;
(setq auth-sources '("~/.ssh/id_rsa.pub")
      auth-source-do-cache t
      auth-source-cache-expiry 86400 ; All day, default is 2h (7200)
      password-cache t
      password-cache-expiry 86400)
;;
;;(defadvice! prompt-for-buffer (&rest _)
;;  :after '(evil-window-split evil-window-vsplit)
;;  (consult-buffer))
;;
;;(use-package! super-save
;;  :config
;;  (setq auto-save-default t ;; nil to switch off the built-in `auto-save-mode', maybe leave it t to have a backup!
;;        super-save-exclude '(".gpg")
;;        super-save-remote-files nil
;;        super-save-auto-save-when-idle t)
;;  (super-save-mode +1))
;;;;(use-package evil-textobj-tree-sitter :ensure t)
;;(setq auto-save-default t)

;;;; Increase undo history limits even more
(after! undo-fu
  (setq undo-limit        100000000    ;; 1MB   (default is 160kB, Doom's default is 400kB)
        undo-strong-limit 100000000   ;; 100MB (default is 240kB, Doom's default is 3MB)
        undo-outer-limit  1000000000) ;; 1GB   (default is 24MB,  Doom's default is 48MB)

  (setq undo-fu-allow-undo-in-region t ;; Undoing with a selection will use undo within that region.
        undo-fu-ignore-keyboard-quit t)) ;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.

(after! evil
  (setq evil-want-fine-undo t)) ;; By default while in insert all changes are one big blob

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
;; treemacs
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
;;
(setq +treemacs-file-ignore-globs
     '(;; LaTeX
        "*/_minted-*"
       ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"
        ;; Python
        "*/__pycache__"))

(setq treemacs-show-hidden-files nil
      treemacs-hide-dot-git-directory t
      treemacs-width 46
      )

(autoload 'helm-bibtex "helm-bibtex" "" t)
(require 'org-ref)
(require 'org-ref-helm)

;;(after! writeroom-mode
;;  ;; Show mode line
;;  (setq writeroom-mode-line t)
;;
;;  ;; Disable line numbers
;;  (add-hook! 'writeroom-mode-enable-hook
;;    (when (bound-and-true-p display-line-numbers-mode)
;;      (setq-local +line-num--was-activate-p display-line-numbers-type)
;;      (display-line-numbers-mode -1)))
;;
;;  (add-hook! 'writeroom-mode-disable-hook
;;    (when (bound-and-true-p +line-num--was-activate-p)
;;      (display-line-numbers-mode +line-num--was-activate-p))))
;;
;;;;showing indentation lines
;;(after! highlight-indent-guides
;;  (setq highlight-indent-guides-character ? highlight-indent-guides-responsive 'top))
;;
;;(map! :n [mouse-8] #'better-jumper-jump-backward
;;      :n [mouse-9] #'better-jumper-jump-forward)
;;
;;(use-package! focus
;;  :commands focus-mode)
;;
;;;; IDE-Features
;;(after! eglot
;;  ;; A hack to make it works with projectile
;;  (defun projectile-project-find-function (dir)
;;    (let* ((root (projectile-project-root dir)))
;;      (and root (cons 'transient root))))
;;
;;  (with-eval-after-load 'project
;;    (add-to-list 'project-find-functions 'projectile-project-find-function))
;;
;;  ;; Use clangd with some options
;;  (set-eglot-client! 'c++-mode '("clangd" "-j=3" "--clang-tidy")))
;;
;;(use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode))
;;
;;;;(setq flycheck-python-flake8-executable "flake8")
;;(setq-default flycheck-disabled-checkers '(python-pylint))
;;
;;(after! flycheck
;;  (setq flycheck-cppcheck-checks '("information"
;;                                   "performance"
;;                                   "portability"
;;                                   "style"
;;                                   "unusedFunction"
;;                                   "warning")))

;;;;org
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
         (file+olp+datetree "~/.org/Journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
         :clock-in :clock-resume
         :empty-lines 1)
        ("jm" "Meeting" entry
         (file+olp+datetree "~/.org/Journal.org")
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)

        ("w" "Workflows")
        ("we" "Checking Email" entry (file+olp+datetree "~/.org/Journal.org")
         "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
        ("wi" "Write Idea" entry (file+olp+datetree "~/org/Journal.org")
         "* Write Idea :idea:\n\n%?" :clock-in :clock-resume :empty-lines 1)

        ("m" "Metrics Capture")
        ("mw" "Weight" table-line (file+headline "~/.org/Metrics.org" "Weight")
         "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

(use-package! orgnote
  :defer t
  :hook (org-mode . orgnote-sync-mode))

;;;;(defun my/show-server-edit-buffer (buffer)
;;;;  ;; TODO: Set a transient keymap to close with 'C-c C-c'
;;;;  (split-window-vertically -15)
;;;;  (other-window 1)
;;;;  (set-buffer buffer))
;;;;
;;;;(setq server-window #'my/show-server-edit-buffer)
;;;;
;;;;(setq display-buffer-base-action
;;;;      '(display-buffer-reuse-mode-window
;;;;        display-buffer-reuse-window
;;;;        display-buffer-same-window))
;;
;;;; If a popup does happen, don't resize windows to be equal-sized
;;;;(setq even-window-sizes nil)
;;;;
;;;;
(use-package embark
  :bind
  (("C-c e" . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command))
;;;;(use-package embark
;;;;  :ensure t
;;;;  :bind (("C-S-a" . embark-act)
;;;;         :map minibuffer-local-map
;;;;         ("C-d" . embark-act))
;;;;  :config
;;;;
;;;;  ;; Show Embark actions via which-key
;;;;  (setq embark-action-indicator
;;;;        (lambda (map)
;;;;          (which-key--show-keymap "Embark" map nil nil 'no-paging)
;;;;          #'which-key--hide-popup-ignore-command)
;;;;        embark-become-indicator embark-action-indicator))
;;;;
;;;;(use-package embark-consult
;;;;  :ensure t
;;;;  :after (embark consult)
;;;;  :demand t ; only necessary if you have the hook below
;;;;  ;; if you want to have consult previews as you move around an
;;;;  ;; auto-updating embark collect buffer
;;;;  :hook
;;;;  (embark-collect-mode . consult-preview-at-point-mode))
;;;;
;;;;(setq evil-vsplit-window-right t
;;;;      evil-split-window-below t)
;;
;;;;beautiful images of my code thanks!
;;;;(use-package! carbon-now-sh
;;;;  :config
;;;;  (defun yeet/carbon-use-eaf ()
;;;;    (interactive)
;;;;    (split-window-right)
;;;;    (let ((browse-url-browser-function 'browse-url-firefox))
;;;;      (browse-url (concat carbon-now-sh-baseurl "?code="
;;;;                          (url-hexify-string (carbon-now-sh--region))))))
;;;;  (map! :n "g C-c" #'yeet/carbon-use-eaf))
;;;;
;;;;(setq dired-dwim-target t)
;;;;
;;;;;;(use-package! graphviz-dot-mode)
;;;;(setq magit-repository-directories '(("~/src". 3))
;;;;      magit-save-repository-buffers nil
;;;;      magit-inhibit-save-previous-winconf t)
;;
;;;;TODO check aas
;;;;(use-package! aas
;;;;  :commands aas-mode)
;;
;;;;academic
(require 'elfeed-org)
(setq rmh-elfeed-org-files (list "papers.org"))
(defun concatenate-authors (authors-list)
  "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
  (if (> (length authors-list) 1)
      (format "%s et al." (plist-get (nth 0 authors-list) :name))
    (plist-get (nth 0 authors-list) :name)))

(defun my-search-print-fn (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (entry-authors (concatenate-authors
                         (elfeed-meta entry :authors)))
         (title-width (- (window-width) 100
                         elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title 100
                        :left))
         (entry-score (elfeed-format-column (number-to-string (elfeed-score-scoring-get-score-from-entry entry)) 10 :left))
         (authors-column (elfeed-format-column entry-authors 40 :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")

    (insert (propertize title-column
                        'face title-faces 'kbd-help title) " ")
    (insert (propertize authors-column
                        'kbd-help entry-authors) " ")
    (insert entry-score " ")))

(setq elfeed-search-print-entry-function #'my-search-print-fn)
(setq elfeed-search-date-format '("%y-%m-%d" 10 :left))
(setq elfeed-search-title-max-width 110)
(setq elfeed-feeds '("http://export.arxiv.org/api/query?search_query=cat:stat.ML&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending" "http://export.arxiv.org/api/query?search_query=cat:cs.LG&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending" "http://export.arxiv.org/api/query?search_query=cat:cs.CL&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending"))
(setq elfeed-search-filter "@2-week-ago +unread")
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(use-package! elfeed-score
  :after elfeed
  :config
  (elfeed-score-load-score-file "~/.config/doom/elfeed/elfeed.score") ; See the elfeed-score documentation for the score file syntax
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map))

(setq arxiv_bib "~/.org/arxiv.bib")
(setq arxiv_pdf_loc "~/Documents/arxiv/")

(setq bibtex-completion-bibliography (list arxiv_bib))
(setq bibtex-completion-library-path '("~/Documents/arxiv"))
(setq bibtex-completion-pdf-field "file")

(setq org-ref-pdf-directory arxiv_pdf_loc)
(defun is-arxiv-entry? ()
  "Check if the current buffer is an Elfeed buffer showing an Arxiv entry."
  (and (eq major-mode 'elfeed-show-mode)
       (let ((url (elfeed-entry-link elfeed-show-entry)))
         (string-match-p "arxiv\\.org" url))))

(defun my/elfeed-entry-to-arxiv ()
    "Fetch an arXiv paper into the local library from the current elfeed entry.

This is a customized version from the one in https://gist.github.com/rka97/57779810d3664f41b0ed68a855fcab54
New features to this version:

- Update the bib entry with the pdf file location
- Add a TODO entry in my papers.org to read the paper
"
    (interactive)
    (setq bibtex-dialect 'BibTeX)
    (require 'bibtex)
    ;;(setq bibtex-set-dialect 'BibTeX)
    ;(bibtex-set-dialect ('BibTeX))
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (match-idx (string-match "arxiv.org/abs/\\([0-9.]*\\)" link))
           (matched-arxiv-number (match-string 1 link))
           (last-arxiv-key "")
           (last-arxiv-title ""))
      (when matched-arxiv-number
        (message "Going to arXiv: %s" matched-arxiv-number)
        (arxiv-get-pdf-add-bibtex-entry matched-arxiv-number arxiv_bib arxiv_pdf_loc)
        ;; Now, we are updating the most recent bib file with the pdf location
        (message "Update bibtex with pdf file location")
        (save-window-excursion
                ;; Get the bib file
                (find-file arxiv_bib)
                ;; get to last line
                (goto-char (point-max))
                ;; get to the first line of bibtex
                (bibtex-beginning-of-entry)
                (let* ((entry (bibtex-parse-entry))
                        (key (cdr (assoc "=key=" entry)))
                        (title (bibtex-completion-apa-get-value "title" entry))
                        (pdf (org-ref-get-pdf-filename key)))
                        (message (concat "checking for key: " key))
                        (message (concat "value of pdf: " pdf))
                        (when (file-exists-p pdf)
                        (bibtex-set-field "file" pdf)
                        (setq last-arxiv-key key)
                        (setq last-arxiv-title title)
                        (save-buffer)
                        )))
        ;; (message (concat "outside of save window, key: " last-arxiv-key))
        ;; Add a TODO entry with the cite key and title
        ;; This is a bit hacky solution as I don't know how to add the org entry programmatically
        (save-window-excursion
          (find-file (concat org-directory "papers.org"))
          (goto-char (point-max))
          (insert (format "** TODO Read paper (cite:%s) %s" last-arxiv-key last-arxiv-title))
          (save-buffer)
          )
        )
      )
  )

(setq org-ref-pdf-directory arxiv_pdf_loc)

(map! :leader
      :desc "arXiv paper to library" "n a" #'my/elfeed-entry-to-arxiv
      :desc "Elfeed" "n e" #'elfeed)

;;;;langtool
(require 'langtool)
(setq langtool-language-tool-jar "/home/shoshin/Documents/lang-tool/LanguageTool-stable/LanguageTool-6.4/languagetool-commandline.jar")
(add-hook 'org-mode-hook
          (lambda () (set (make-local-variable 'langtool-java-user-arguments)
                          '("-Dfile.encoding=UTF-8"))))

(with-eval-after-load 'org
    (setq org-directory "~/.org/"))

(load-theme 'doom-henna :no-confirm)

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

;;;;;;;;;;;;;COMBULATE-------------------------------------------------->
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
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
         ;;(js-mode . combobulate-mode)
         ;;(css-mode . combobulate-mode)
         (yaml-mode . combobulate-mode))
         ;;(typescript-mode . combobulate-mode)
         ;;(tsx-mode . combobulate-mode)
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  ;; ~/.config/emacs/addOns/combobulate/
  :load-path ("/home/shoshin/.config/emacs/addOns/combobulate/"))
;;;;;;;;;;;;;COMBULATE-------------------------------------------------->
;;
(defun open-remote-file()
  "open a remote file using tramp."
  (interactive)
  (find-file (concat "/ssh:hermanns@gpu:/home/hermanns" (read-string "Enter file path: ")))
  (message "connecting to gpu-server ...!"))

(defun hc/open-work-folder()
  (interactive)
  (dired "~/src/__workbench/__scratch")
  (message  "%s to working directory!"
            (nth (random 7) '(meandering walking hiking going pathing going Si-lining))))

(evil-global-set-key 'normal (kbd "SPC o w") 'hc/open-work-folder)
(evil-global-set-key 'normal (kbd "SPC o g") 'open-remote-file)
(evil-global-set-key 'normal (kbd "C-c l") 'universal-argument)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . eglot-format-buffer))
  :hook ((python-mode . eglot-ensure)
         (python-mode . flyspell-prog-mode)
         (python-mode . superword-mode)
         (python-mode . hs-minor-mode)
         (python-mode . (lambda () (set-fill-column 88))))
  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                             :plugins (
                                       :pycodestyle (:enabled :json-false)
                                       :mccabe (:enabled :json-false)
                                       :pyflakes (:enabled :json-false)
                                       :flake8 (:enabled :json-false
                                                :maxLineLength 88)
                                       :ruff (:enabled t
                                              :lineLength 88)
                                       :pydocstyle (:enabled t
                                                    :convention "numpy")
                                       :yapf (:enabled :json-false)
                                       :autopep8 (:enabled :json-false)
                                       :black (:enabled t
                                               :line_length 88
                                               :cache_config t)))))))
(use-package python-pytest)
(use-package python-black)
(use-package python-isort)
(use-package ruff-format)

;;(use-package pet
;;  :ensure-system-package (dasel sqlite3)
;;  :config
;;  (add-hook 'python-mode-hook
;;            (lambda ()
;;              (setq-local python-shell-interpreter (pet-executeable-find "/home/shoshin/miniconda3/bin/python3")
;;                          python-shell-virtualenv-root (pet-virtualenv-root))
;;              (pet-flycheck-setup)
;;              (flycheck-mode)
;;              (lsp)
;;              (setq-local python-pytest-executable (pet-executable-find "/home/shoshin/miniconda3/bin/pytest"))
;;
;;
;;              (when-let ((black-executable (pet-executable-find "black")))
;;                (setq-local python-black-command black-executable)
;;                (python-black-on-save-mode))
;;
;;              (when-let ((isort-executable (pet-executable-find "isort")))
;;                (setq-local python-isort-command isort-executable)
;;                (python-isort-on-save-mode)))))

              ;;(setq-local flycheck-python-mypy-executable "/home/shoshin/miniconda3/bin/mypy")


;;(use-package cursory
;;(setq cursory-presets
;;      '((bar . ( :cursor-type (bar . 2)
;;                              :cursor-in-non-selected-windows hollow
;;                              :blink-cursor-blinks 10
;;                              :blink-cursor-interval 0.5
;;                              :blink-cursor-delay 0.2))
;;
;;        (box  . ( :cursor-type box
;;                               :cursor-in-non-selected-windows hollow
;;                               :blink-cursor-blinks 10
;;                               :blink-cursor-interval 0.5
;;                               :blink-cursor-delay 0.2))
;;
;;        (underscore . ( :cursor-type (hbar . 3)
;;                                     :cursor-in-non-selected-windows hollow
;;                                     :blink-cursor-blinks 50
;;                                     :blink-cursor-interval 0.2
;;                                     :blink-cursor-delay 0.2)))))
;;(setq cursory-latest-state-file (locate-user-emacs-file "cursory-latest-state"))
;;(cursory-restore-latest-preset)
;;;;
;;;;;; Set `cursory-recovered-preset' or fall back to desired style from
;;;;;; `cursory-presets'.
;;(if cursory-recovered-preset
;;    (cursory-set-preset cursory-recovered-preset)
;;  (cursory-set-preset 'bar))
;;;;
;;;;;; The other side of `cursory-restore-latest-preset'.
;;(add-hook 'kill-emacs-hook #'cursory-store-latest-preset)
;;;;
;;;;;; We have to use the "point" mnemonic, because C-c c is often the
;;;;;; suggested binding for `org-capture'.
;;(define-key global-map (kbd "C-c p") #'cursory-set-preset)
;;
;;(setq org-latex-pdf-process
;;      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
;;
;;
;;unless (boundp 'org-latex-classes)

;;(add-hook 'julia-mode-hook #'rainbow-delimiters-mode-enable)
;;(add-hook! 'julia-mode-hook
;;  (setq-local lsp-enable-folding t
;;             lsp-folding-range-limit 100))


;;(setq julia-repl-executable-records
;;      '((default "/home/shoshin/julia-1.10.4/bin/julia")))

;;(setenv "PATH" (concat (getenv "PATH") ":/home/shoshin/julia-1.10.4/bin/julia"))
;;(add-to-list 'exec-path "/home/shoshin/julia-1.10.4/bin/julia")
;;(setq julia-shell-executable "/home/shoshin/julia-1.10.4/bin/julia")

;;(use-package julia-repl
;;  :ensure t
;;  :config
;;  (setq julia-repl-executable-records
;;        '((default "julia")))
;;  (add-hook 'julia-mode-hook 'julia-repl-mode))
(require 'ox-latex
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
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

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
\\usepackage{enumerate
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

(eval-after-load 'ox-koma-letter
  '(progn
     (add-to-list 'org-latex-classes
                  '("my-letter"
                    "\\documentclass\{scrlttr2\}
     \\usepackage[english]{babel}
     \\setkomavar{frombank}{(1234)\\,567\\,890}
     \[DEFAULT-PACKAGES]
     \[PACKAGES]
     \[EXTRA]"))

     (setq org-koma-letter-default-class "my-letter")))))
