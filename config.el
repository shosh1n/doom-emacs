(setq user-full-name "christoph"
      user-mail-address "christoph-alexander@proton.me")

(eval-when-compile
  (require 'use-package))
;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!
;; You will most likely need to adjust this font size for your system!
;;(setq fancy-splash-image "~/.config/doom/Header_Sielaff_Guide_2024_EN.jpg")
(setq inhibit-startup-message t)
(defvar my/default-font-size 80)
(defvar my/default-variable-font-size 80)

;; Make frame transparency overridable
;; doesnt work on hyprland
;;(defvar my/frame-transparency '(90 . 80))


;; The default is 800 kilobytes.  Measured in bytes.
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

;;set fonts
(set-face-attribute 'default nil :font "JetBrains Mono" :height my/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height my/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height my/default-variable-font-size :weight 'regular)
(setq doom-font (font-spec :family "JetBrains Mono" :size 16))
(setq doom-symbol-font (font-spec :family "JetBrains Mono NL"))
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;load this
(add-to-list 'load-path "/home/hermanns/.config/emacs/.local/straight/repos/tree-sitter-langs/")

(setq org-roam-directory "~/.org")

(setq bibtex-completion-bibliography
      '("~/.org/arxiv.bib"))

(setq bibtex-completion-library-path '("~/Documents/arxiv"))
(setq dired-dwim-target t)
(setq markdown-split-window-direction 'right)

(setq auth-sources '("~/.ssh/id_rsa.pub")
      auth-source-do-cache t
      auth-source-cache-expiry 86400 ; All day, default is 2h (7200)
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
  (setq undo-limit        100000000    ;; 1MB   (default is 160kB, Doom's default is 400kB)
        undo-strong-limit 100000000   ;; 100MB (default is 240kB, Doom's default is 3MB)
        undo-outer-limit  1000000000) ;; 1GB   (default is 24MB,  Doom's default is 48MB)

  ;; Undo-fu customization options
  (setq undo-fu-allow-undo-in-region t ;; Undoing with a selection will use undo within that region.
        undo-fu-ignore-keyboard-quit t)) ;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.

;; Evil undo
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


;;(setq bibtex-completion-bibliography
;;      '("~/.org/arxiv.bib"
;;        "/path/to/.org"
;;        ("/path/to/org-bibtex-file2.org" . "/path/to/bibtex-file.bib")))

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

;; IDE-Features
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

(defun my/show-server-edit-buffer (buffer)
  ;; TODO: Set a transient keymap to close with 'C-c C-c'
  (split-window-vertically -15)
  (other-window 1)
  (set-buffer buffer))

(setq server-window #'my/show-server-edit-buffer)

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

;;TODO check aas
(use-package! aas
  :commands aas-mode)

;;academic
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

;;(defun extract-bibtex-key (bibtex-content)
;;  "Extract the citation key from a BibTeX entry."
;;  (when (string-match "@[a-zA-Z]+{\\([^,]+\\)," bibtex-content)
;;    (match-string 1 bibtex-content)))
;;
;;(defun download-and-open-arxiv ()
;;  (interactive)
;;  (let* ((url (if (is-arxiv-entry?)
;;                  (elfeed-entry-link elfeed-show-entry)
;;                (read-string "Enter the PDF URL: ")))
;;         (bibtex-url (replace-regexp-in-string "/\\(abs\\|pdf\\)/" "/bibtex/" url))
;;         ;; Fetch BibTeX content to extract key only if it's an Arxiv entry
;;         (bibtex-content (when bibtex-url
;;                           (with-current-buffer (url-retrieve-synchronously bibtex-url)
;;                             (goto-char url-http-end-of-headers)
;;                             (buffer-substring-no-properties (point) (point-max)))))
;;         ;; Extract citation key from BibTeX content if vailable, else use file name
;;         (bibtex-key (if bibtex-content
;;                         (extract-bibtex-key bibtex-content)
;;                       (file-name-nondirectory url)))
;;         ;; Update URL for PDF download if it's an Arxiv entry
;;         (pdf-url (if (is-arxiv-entry?) (concat (replace-regexp-in-string "/abs/" "/pdf/" url) ".pdf") url))
;;         (download-path (concat (if (boundp 'my-arxiv-library-path)
;;                                    my-arxiv-library-path
;;                                  "~/Documents/arxiv/")
;;                                bibtex-key ".pdf")))
;;    (url-copy-file pdf-url download-path t)
;;    (find-file-other-window download-path)))
;;(bibtex-set-dialect 'BibTeX)
;;
;;


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
    ;;(setq bibtex-dialect 'BibTeX)
    ;;(setq bibtex-set-dialect 'BibTeX)
    ;;(bibtex-set-dialect ('BibTeX))
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


;;(defun my/elfeed-entry-to-arxiv ()
;;  "Fetch an arXiv paper into the local library from the current elfeed entry."
;;  (interactive)
;;  (let* ((link (elfeed-entry-link elfeed-show-entry))
;;         (match-idx (string-match "arxiv.org/abs/\\([0-9.]*\\)" link))
;;         (matched-arxiv-number (match-string 1 link)))
;;    (when matched-arxiv-number
;;      (message "Going to arXiv: %s" matched-arxiv-number)
;;      (arxiv-get-pdf-add-bibtex-entry matched-arxiv-number arxiv_bib arxiv_pdf_loc)
;;      message "Update bibtex with pdf file location")
;;    (let* ((pdf (org-ref-get-pdf-filename matched-arxiv-number)))
;;      (when (and pdf (file-exists-p pdf))
;;        (with-current-buffer (find-file-noselect arxiv_bib)
;;          (bibtex-search-entry matched-arxiv-number)
;;          (bibtex-set-field "file" pdf)
;;          (save-buffer)))
;;      (with-current-buffer (find-file-noselect (concat org-directory "papers.org"))
;;        (goto-char (point-max))
;;        (insert (format "** TODO Read paper (cite:%s)\n" matched-arxiv-number))
;;        (save-buffer)))))



(map! :leader
      :desc "arXiv paper to library" "n a" #'my/elfeed-entry-to-arxiv
      :desc "Elfeed" "n e" #'elfeed)


(after! langtool
  (setq langtool-language-tool-jar "~/opt/LanguageTool-5.3/languagetool-commandline.jar"))

(defun +version-control|git-gutter-maybe ()
  (when buffer-file-name
    (require 'git-gutter-fringe)
    (git-gutter-mode +1)))

(with-eval-after-load 'org
    (setq org-directory "~/.org/"))

(after! magit
  ;; Show gravatars
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

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
