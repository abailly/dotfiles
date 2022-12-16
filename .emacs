
;; General layout

;; package installation
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(setq default-frame-alist
      '((fullscreen . maximized) (fullscreen-restore . fullheight)))

;;; Code:
(add-hook 'window-setup-hook
         (lambda nil
            ;; font setting
           (set-frame-parameter (selected-frame) 'alpha '(100 100))
           (set-face-attribute 'default nil
                               :family "Hack"
                               :height 140)

           ))

(use-package unicode-fonts
   :ensure t
   :config
   (unicode-fonts-setup))

;; from https://www.emacswiki.org/emacs/iTerm2
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  ;; map super+alpha key
  (cl-loop for char from ?a to ?z
           do (define-key input-decode-map (format "\e[1;P%c" char) (kbd (format "s-%c" char))))

  ;; https://stackoverflow.com/questions/10660060/how-do-i-bind-c-in-emacs/40222318#40222318
  (defun my/global-map-and-set-key (key command &optional prefix suffix)
    "`my/map-key' KEY then `global-set-key' KEY with COMMAND.
 PREFIX or SUFFIX can wrap the key when passing to `global-set-key'."
    (my/map-key key)
    (global-set-key (kbd (concat prefix key suffix)) command))

  (defun my/map-key (key)
    "Map KEY from escape sequence \"\e[emacs-KEY\."
    (define-key function-key-map (concat "\e[emacs-" key) (kbd key))))

(my/map-key "s-l")
(my/map-key "C->")
(my/map-key "C-<")

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi))

(require 'cl-lib)
(cl-loop for char from ?a to ?z
         do (define-key input-decode-map (format "\e[1;P%c" char) (kbd (format "s-%c" char))))

(menu-bar-mode 0)
(tool-bar-mode 0)

(set-fill-column 132)

(global-set-key (kbd "C-x M-a") "α")
(global-set-key (kbd "C-x M-b") "β")
(global-set-key (kbd "C-x M-d") "δ")
(global-set-key (kbd "C-x M-l") "λ")
(global-set-key (kbd "C-x M-p") "π")
(global-set-key (kbd "C-x M-r") "ρ")
(global-set-key (kbd "C-x C-g") "γ")
(global-set-key (kbd "C-x C-n") "ν")
(global-set-key (kbd "C-x M-x") "ξ")
(global-set-key (kbd "C-x M-P") "Π")
(global-set-key (kbd "C-x M-S") "Σ")
(global-set-key (kbd "C-x M-i") (lambda ()
                                  (interactive)
                                  (insert "·")))

(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)

(setq fill-column 132)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-c\C-g" 'rgrep)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-hl-line-mode 1)
(global-set-key "\C-c\C-o" 'browse-url-at-point)


(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(show-paren-mode)

(use-package direnv
 :ensure t
 :config
 (direnv-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; https://github.com/emacsmirror/expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; helm
;; from http://tuhdo.github.io/helm-intro.html
(use-package helm	
   :ensure t)
(use-package helm-config
   :ensure t)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(setq helm-mode-fuzzy-match t)
(helm-autoresize-mode 1)

(helm-mode 1)

;; to input pinyin accented chars using right option/alt key
(setq mac-right-option-modifier 'none)
(setq mac-command-modifier 'super)

;; form http://stackoverflow.com/questions/2903426/display-path-of-file-in-status-bar
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Org stuff
(require 'org)
(require 'org-protocol)


(setq org-directory "~/log")
(setq org-agenda-files '("~/log/"))

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/log/todo.org" "Tasks")
         "*** TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/log/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("p" "Palo-IT" entry (file+datetree "~/log/palo-it.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("P" "Capture" entry (file+datetree "~/log/journal.org")
         "* %c\n%i" :immediate-finish t)
        ("w" "Handle email@work" entry (file+datetree "~/log/palo-it.org")
         "**** TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
;;        ("w" "Handle email@home" entry (file+datetree "~/log/journal.org")
;;         "**** TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
        ("L" "Capture link" entry (file+datetree "~/log/journal.org")
         "* %? [[%:link][%:description]] \nCaptured On: %U" :immediate-finish t)))

(add-hook 'org-mode-hook 'auto-fill-mode)

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "FOLLOWUP(f)" "|" "DONE(d)")
        (sequence "WAITING(w)" "|" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("FOLLOWUP" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("DOING" :foreground "orange" :weight bold)
              ("WAITING" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 3))))

(setq org-agenda-default-appointment-duration 60)

(eval-after-load "org"
  '(require 'ox-md nil t))

;; format string used when creating CLOCKSUM lines and when generating a
;; time duration (avoid showing days)
;; http://stackoverflow.com/questions/17929979/emacs-org-mode-how-to-stop-total-in-column-view-showing-number-of-days
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; multiple-cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; flx
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)

;; ;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)

;; use space for indentation, 2 spaces wide
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; ensure javascript indentation is 2 spaces
(setq js-indent-level 2)
(setq-default js2-basic-offset 2)

;; activate smerge when opening conflict files
(defun sm-try-smerge ()
     (save-excursion
       (goto-char (point-min))
       (when (re-search-forward "^<<<<<<< " nil t)
   	   (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

;; Haskell stuff
(setenv "PATH"
        (concat (getenv "HOME") "/.local/bin:"
                (getenv "HOME") "/.cabal/bin:"
                (getenv "HOME") "/.ghcup/bin:"
                "/usr/local/bin:"
                (getenv "PATH")))

(setq exec-path
      (reverse
       (append
        (reverse exec-path)
        (list (concat (getenv "HOME") "/.local/bin")
              (concat (getenv "HOME") "/.cabal/bin")
              (concat (getenv "HOME") "/.ghcup/bin")
              "/usr/local/bin" ))))

(use-package nix-sandbox)

;;LSP Haskell
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package yasnippet
  :ensure t)

(defun lsp-format-buffer-for-haskell ()
  (when (eq major-mode 'haskell-mode)
    (lsp-format-buffer)))

(defun format-cabal-buffer ()
  (when (eq major-mode 'haskell-cabal-mode)
    (haskell-mode-buffer-apply-command "cabal-fmt")))

(use-package helm-lsp
  :ensure t)

(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

;; from https://blog.sumtypeofway.com/posts/emacs-config.html
(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'before-save-hook #'lsp-format-buffer-for-haskell)
  (add-hook 'before-save-hook #'format-cabal-buffer)

  :config
  ;; haskell-mode doesn't know about newer GHC features.
  (let ((new-extensions '("QuantifiedConstraints"
                          "DerivingVia"
                          "BlockArguments"
                          "DerivingStrategies"
                          "StandaloneKindSignatures"
                          )))
    (setq
     haskell-ghc-supported-extensions
     (append haskell-ghc-supported-extensions new-extensions))))

(use-package haskell-snippets
  :after (haskell-mode yasnippet)
  :defer)

(use-package lsp-mode
  :ensure t
  ;; we need to defer running lsp because in case there's a direnv
  ;; with use nix, it takes some time to load and lsp won't find the
  ;; language server until the env is setup properly
  :hook ((haskell-mode . lsp-deferred) (typescript-mode . lsp))
  :custom  (lsp-lens-enable nil)
  :commands (lsp lsp-deferred))

(use-package lsp-haskell
  :ensure t
  :custom
  (lsp-haskell-fourmolu-on 't)
  (lsp-haskell-formatting-provider "fourmolu"))

(setq lsp-log-io 't)

(use-package lsp-treemacs
  :ensure t)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; seems like for some reason the function cannot be foudn at startup
(defun string-join (sl delim)
  (mapconcat 'identity sl delim))

;; (use-package haskell-mode
;;   :ensure t
;;   :init (progn
;;           (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;           (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
;;           (add-hook 'haskell-mode-hook 'linum-mode))
;;   :config (progn
;;             (setq haskell-font-lock-symbols t)
;;             (setq haskell-process-use-presentation-mode t)
;;             (setq haskell-ghci-options
;;                   '("-ferror-spans"
;;                     "-fdefer-typed-holes"
;;                     "-fmax-relevant-binds=0"
;;                     "-fno-diagnostics-show-caret"
;;                     ))
;;             (setq haskell-process-args-stack-ghci
;;                   (list (concat "--ghci-options=" (string-join haskell-ghci-options " "))
;;                         "--no-build"
;;                         "--no-load"))
;;             (setq haskell-font-lock-symbols-alist
;;                   '(("\\" . "λ")
;;                     ("." "∘" haskell-font-lock-dot-is-not-composition)
;;                     ("forall" . "∀")))
;;             (setq haskell-interactive-popup-errors nil)
;;             (setq haskell-indentation-left-offset 2)
;;             (setq haskell-indentation-layout-offset 2)
;;             (setq haskell-tags-on-save t)))


(put 'downcase-region 'disabled nil)

;; I never remember it...
(defalias 'filter-lines 'keep-lines)

;; Elm
;; (setq elm-tags-on-save t)
;; (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
;; (setq elm-format-on-save t)
;; (setq elm-tags-exclude-elm-stuff nil)

;; magit
(use-package magit
  :ensure t)
(global-set-key "\C-xg" 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; markdown
(use-package markdown-mode
  :custom
  (markdown-command "pandoc -s --highlight-style pygments"))

(defun my-set-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 10)
  (setq right-margin-width 10))

(add-hook 'markdown-mode-hook 'my-set-margins)

(add-hook 'markdown-mode-hook 'outline-minor-mode)

;; Python
;; requires (package-install 'elpy)
;; https://github.com/jorgenschaefer/elpy
(use-package elpy
  :ensure t)
(elpy-enable)

;; Idris
;; development mode
(use-package prop-menu
  :ensure t)

(let ((idris2-mode-dir (concat (getenv "HOME") "/projects/idris/idris2-mode")))
  (when (file-exists-p idris2-mode-dir)
    (add-to-list 'load-path idris2-mode-dir)
    (require 'idris2-mode)))

;; projectile
(use-package projectile
  :ensure t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(use-package helm-projectile
  :ensure t)

(helm-projectile-on)

;; provides ag powered search for projectile, among other things
(use-package ag
  :ensure t)

;; elfeed
(use-package elfeed
  :ensure t)

(setq elfeed-feeds
      '("https://abailly.github.io/atom.xml"
        "http://planet.emacsen.org/atom.xml"
        "https://reasonablypolymorphic.com/feed.rss"))

(global-set-key (kbd "C-x w") 'elfeed)

;; yaml
(use-package yaml-mode
  :ensure t)

;; highlight-todo
(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold)))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

;; javascript
(require 'flycheck)
(use-package js2-mode
  :ensure t)
(use-package js2-refactor
  :ensure t)
(use-package xref-js2
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; from https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package eglot
  :ensure t)

;; (use-package tide
;;   :ensure t)

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (add-hook 'js2-mode-hook #'setup-tide-mode)
;; (add-hook 'js-mode-hook #'setup-tide-mode)
;; (add-hook 'js2-mode-hook
;;           #'(lambda ()
;;               (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
;;               (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; ;; configure javascript-tide checker to run after your default javascript checker
;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "jsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))
;; ;; configure jsx-tide checker to run after your default jsx checker
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))

;; supports .editorconfig file in project
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; elm
(use-package elm-mode
 :ensure t)

;; terraform
(use-package terraform-mode
 :ensure t)

;; shellcheck
;;http://www.skybert.net/emacs/bash-linting-in-emacs/
(add-hook 'sh-mode-hook 'flycheck-mode)

;; mu4e
(let ((mu4e-config (concat (getenv "HOME") ".mu4e.el")))
  (when (file-exists-p mu4e-config)
    (load-file mu4e-config)))

;; kill all cal-* buffers
;; inspired by https://www.emacswiki.org/emacs/KillingBuffers#toc3
(defun kill-cal-buffers ()
  "This function kills all opened buffers named 'cal-*'. This is useful
when refreshing the calendars reaped out of gmail"
  (interactive)
  (mapc (lambda (buffer)
          (let ((buf (buffer-name buffer)))
            (when (string-match "cal-.*" buf)
              (kill-buffer buffer)
              (message "Killed buffer %s" buf))))
        (buffer-list)))

(global-set-key (kbd "C-x C-S-C") 'kill-cal-buffers)

;; CRUX
;; https://github.com/bbatsov/crux
(recentf-mode 1)

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-c o" . crux-open-with)
         ("C-c f" . crux-recentf-find-file)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c I" . crux-find-user-init-file)
         ([(shift return)] . crux-smart-open-line)
         ("s-r" . crux-recentf-find-file)
         ("C-<backspace>" . crux-kill-line-backwards)
         ([remap kill-whole-line] . crux-kill-whole-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.emacswiki.org/emacs/InsertFileName
(defun bjm/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

;; bind it
(global-set-key (kbd "C-c b i") 'bjm/insert-file-name)

;; https://blog.sulami.xyz/posts/literate-calc-mode/
;; couldn't resist trying this
(use-package literate-calc-mode
  :ensure t)

(use-package outshine
  :init (add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'outline-regexp)
                                      "-- \\*+")
            (outline-minor-mode)))
  :ensure t)

(use-package browse-at-remote
  :ensure t
  :bind
  (("C-c M-o" . show-remote-get-url))
  :custom
  (browse-at-remote-prefer-symbolic t "Use commit hash for more permanent links."))

(defun show-remote-get-url ()
  "Print the output of browse-at-remote-get-url for current line."
  (interactive)
  (let ((url (browse-at-remote-kill)))
    (message "Source code URL: %s" url)))


(use-package git-timemachine
  :ensure t
  :bind
  (("C-c M-m" . git-timemachine)))

(use-package rust-mode
  :ensure t
  :custom
  (rust-format-on-save t)
  :hook
  ((rust-mode . lsp)))

(use-package go-mode
  :ensure t
  :hook
  ((go-mode . lsp)))

(use-package magit-todos
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aa72e5b41780bfff2ff55d0cc6fcd4b42153386088a4025fed606c1099c2d9b8" "31f1723fb10ec4b4d2d79b65bcad0a19e03270fe290a3fc4b95886f18e79ac2f" "0568a5426239e65aab5e7c48fa1abde81130a87ddf7f942613bf5e13bf79686b" "076ee9f2c64746aac7994b697eb7dbde23ac22988d41ef31b714fc6478fee224" "0f7fa4835d02a927d7d738a0d2d464c38be079913f9d4aba9c97f054e67b8db9" default))
 '(lsp-haskell-server-path "haskell-language-server")
 '(package-selected-packages
   '(magit-todos helm-lsp go-mode rust-mode vline hl-fill-column haskell-cabal-mode git-timemachine browse-at-remote hl-todo unicode-fonts modus-themes monokai helm-ag ag direnv lsp nix-sandbox nix-mode yaml-mode xref-js2 web-mode use-package tide terraform-mode rainbow-delimiters prop-menu projectile outshine org-mime magit lsp-ui lsp-haskell literate-calc-mode js2-refactor intero helm google-translate expand-region elpy elm-mode elfeed editorconfig crux color-theme))
 '(safe-local-variable-values
   '((lsp-haskell-fourmolu-on nil)
     (lsp-haskell-formatting-provider . "ormolu"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package sensei
  :ensure t
  :bind
  (("C-x r n" . sensei-record-note))
  (("C-x r f" . sensei-record-flow)))

;; highlight-todo
(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold)))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

(provide '.emacs)
;;; .emacs ends here
