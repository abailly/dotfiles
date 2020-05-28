;; General layout

;; package installation
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(setq default-frame-alist
      '((fullscreen . maximized) (fullscreen-restore . fullheight)))

;;; Code:
(add-hook 'window-setup-hook
         (lambda nil
            ;; font setting
           (set-frame-parameter (selected-frame) 'alpha '(80 80))
           (set-face-background 'hl-line "#ff0")
           (set-face-attribute 'default nil
                               :background "white"
                               :foreground "black"
                               :family "Source Code Pro"
                               :height 140)

           ))

(global-set-key (kbd "C-x M-a") "α")
(global-set-key (kbd "C-x M-b") "β")
(global-set-key (kbd "C-x M-d") "δ")
(global-set-key (kbd "C-x M-l") "λ")
(global-set-key (kbd "C-x M-p") "π")
(global-set-key (kbd "C-x M-r") "ρ")
(global-set-key (kbd "C-x C-g") "γ")
(global-set-key (kbd "C-x M-P") "Π")
(global-set-key (kbd "C-x M-S") "Σ")
(global-set-key (kbd "C-x M-i") (lambda ()
                                  (interactive)
                                  (insert "·")))

(setq fill-column 132)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-c\C-g" 'rgrep)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-hl-line-mode 1)

(global-set-key "\C-c\C-o" 'browse-url-at-point)

;; https://github.com/emacsmirror/expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; helm
;; from http://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

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

(menu-bar-mode 0)
(tool-bar-mode 0)

(global-set-key (kbd "C-c C-/") 'comment-or-uncomment-region)

;; multiple-cursors
;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)

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

;; activate smerge when opening conflict files
(defun sm-try-smerge ()
     (save-excursion
       (goto-char (point-min))
       (when (re-search-forward "^<<<<<<< " nil t)
   	   (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

;; Haskell stuff
(setenv "PATH" (concat (getenv "HOME") "/.local/bin:" "/usr/local/bin:" (getenv "PATH")))

(setq exec-path
      (reverse
       (append
        (reverse exec-path)
        (list (concat (getenv "HOME") "/.local/bin")  "/usr/local/bin" ))))

;; LSP Haskell
;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode t))
;; (use-package yasnippet
;;   :ensure t)
;; (use-package lsp-mode
;;   :ensure t
;;   :hook (haskell-mode . lsp)
;;   :commands lsp)
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)
;; (use-package lsp-haskell
;;  :ensure t
;;  :config
;;  (setq lsp-haskell-process-path-hie "ghcide")
;;  (setq lsp-haskell-process-args-hie '())
;;  ;; Comment/uncomment this line to see interactions between lsp client/server.
;;  ;;(setq lsp-log-io t)
;;  )

;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)

;; seems like for some reason the function cannot be foudn at startup
(defun string-join (sl delim)
  (mapconcat 'identity sl delim))

(use-package haskell-mode
  :ensure t
  :init (progn
          (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
          (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
          (add-hook 'haskell-mode-hook 'linum-mode))
  :config (progn
            (setq haskell-font-lock-symbols t)
            (setq haskell-process-use-presentation-mode t)
            (setq haskell-ghci-options
                  '("-ferror-spans"
                    "-fdefer-typed-holes"
                    "-fmax-relevant-binds=0"
                    "-fno-diagnostics-show-caret"
                    ))
            (setq haskell-process-args-stack-ghci
                  (list (concat "--ghci-options=" (string-join haskell-ghci-options " "))
                        "--no-build"
                        "--no-load"))
            (setq haskell-font-lock-symbols-alist
                  '(("\\" . "λ")
                    ("." "∘" haskell-font-lock-dot-is-not-composition)
                    ("forall" . "∀")))
            (setq haskell-interactive-popup-errors nil)
            (setq haskell-indentation-left-offset 2)
            (setq haskell-indentation-layout-offset 2)
            (setq haskell-tags-on-save t)))

;; ormolu formatting
;; https://github.com/vyorkin/ormolu.el
(use-package ormolu
  :init (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  :bind (:map haskell-mode-map
              ("C-c r" . ormolu-format-buffer)))


(put 'downcase-region 'disabled nil)

;; I never remember it...
(defalias 'filter-lines 'keep-lines)

;; Elm
(setq elm-tags-on-save t)
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
(setq elm-format-on-save t)
(setq elm-tags-exclude-elm-stuff nil)

;; magit
(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; markdown
(require 'markdown-mode)
(setq markdown-command "pandoc -s --highlight-style pygments")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-compile-before-require t)
 '(haskell-stylish-on-save nil)
 '(idris-interpreter-flags (quote ("-p" "contrib")))
 '(idris-interpreter-path "idris")
 '(idris-log-events t)
 '(package-selected-packages
   (quote
    (plantuml-mode ormolu proof-general gnuplot-mode graphviz-dot-mode powershell rainbow-delimiters lsp-haskell lsp-ui lsp-mode typo typopunct ess dockerfile-mode org-mu4e mu4e org-mime ag magit expand-region company-go go-autocomplete go-complete go-mode go company-coq helm-core helm helm-ag-r helm-company helm-git helm-google helm-hoogle helm-idris helm-ls-git markdown-mode multiple-cursors magit-gh-pulls intero flx-isearch flx-ido)))
 '(safe-local-variable-values
   (quote
    ((intero-targets "sensei:lib" "sensei:exe:sensei-exe" "sensei:test:sensei-test")
     (intero-targets "minilang:lib" "minilang:exe:mli" "minilang:test:minilang-test")
     (intero-targets "rex-api:lib" "rex-api:exe:rex-server" "rex-api:test:rex-tests")
     (intero-targets "pet-store:lib" "pet-store:exe:driver-petstore" "pet-store:exe:mock-petstore" "pet-store:test:tests" "pet-store-backend:lib" "pet-store-backend:exe:pet-store-server")
     (intero-targets "pet-store-backend:lib" "pet-store-backend:exe:pet-store-server" "pet-store-backend:test:runner")
     (intero-targets "rex-cli:lib" "rex-cli:exe:rex-cli" "rex-cli:test:rex-tests" "rex-contracts:lib" "rex-depgraph:lib" "rex-depgraph:test:rex-depgraph-tests" "rex-feeder:lib" "rex-feeder:test:rex-feeder-tests" "rex-tester:lib" "rex-tester:test:rex-tester-tests")
     (intero-targets "rex-depgraph:lib" "rex-depgraph:exe:rex-callgraph" "rex-depgraph:exe:rex-generator" "rex-depgraph:test:rex-depgraph-tests")
     (intero-targets "tents-and-trees:lib" "tents-and-trees:exe:tents-and-trees-exe" "tents-and-trees:test:tents-and-trees-test")
     (intero-targets "hpaie:lib" "hpaie:exe:assign-keys" "hpaie:exe:gen-ledger" "hpaie:test:hpaie-test")
     (intero-targets "circuit-breaker:lib" "circuit-breaker:exe:circuit-breaker" "circuit-breaker:test:circuit-breaker-test" "log-controller:lib" "log-controller:exe:control" "log-controller:test:log-controller-test" "one-log:lib" "one-log:exe:one-log-exe" "one-log:test:one-log-test" "pet-store:lib" "pet-store:exe:driver-petstore" "pet-store:exe:mock-petstore" "pet-store:test:tests" "pet-store-backend:lib" "pet-store-backend:exe:pet-store-server")
     (time-stamp-active . t)
     (intero-targets "day7:exe:day7" "intcode:lib")
     (intero-targets "day7:exe:day7")
     (intero-targets "survey:lib" "survey:exe:quizz" "survey:test:survey-test")
     (intero-targets "haskell-sqlite:lib" "haskell-sqlite:exe:haskell-sqlite-exe" "haskell-sqlite:test:haskell-sqlite-test")
     (haskell-indentation-where-pre-offset . 4)
     (haskell-indentation-starter-offset . 4)
     (haskell-indentation-left-offset . 4)
     (haskell-indentation-layout-offset . 4)
     (haskell-indent-spaces . 4)
     (intero-targets "deptrack-core:lib" "deptrack-devops:lib" "deptrack-devops-examples:lib" "deptrack-devops-examples:exe:deptrack-devops-example-devbox" "deptrack-devops-recipes:lib")
     (intero-targets "sym-test:lib" "sym-test:exe:test-runner" "sym-test:test:tasty" "txe:lib" "txe:exe:humanize" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:test:tasty" "sym-test:lib" "sym-test:exe:test-runner" "sym-test:test:tasty")
     (intero-targets "sym-client:lib" "sym-test:lib" "sym-test:exe:test-runner" "sym-test:test:tasty")
     (intero-targets "sym-client:lib" "sym-core:lib" "sym-test:lib" "sym-test:exe:test-runner" "sym-test:test:tasty")
     (intero-targets "sym-client:lib" "sym-core:lib" "sym-crypto:lib" "sym-test:lib" "sym-test:exe:test-runner" "sym-test:test:tasty")
     (intero-targets "sym-client:lib" "sym-core:lib" "txe:lib")
     (intero-targets "sym-client:lib" "txe:lib")
     (intero-targets "sym-client:lib" "sym-crypto:lib" "sym-http:lib" "sym-logging:lib" "sym-test:lib" "sym-test:exe:test-runner" "sym-test:test:tasty" "sym-util:lib" "txe:lib")
     (intero-targets "sym-logging:lib" "sym-logging:test:tasty" "txe:lib" "txe:exe:humanize" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-test:lib" "sym-test:exe:test-runner" "sym-test:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:exe:contract" "sym-client:test:tasty" "sym-test:lib" "sym-test:exe:test-smartlog" "sym-test:test:tasty" "sym-util:lib" "sym-util:test:tasty" "txe:lib" "txe:exe:humanize" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:exe:contract" "sym-client:test:tasty" "sym-test:lib" "sym-test:exe:test-smartlog" "sym-test:test:tasty" "sym-util:lib" "sym-util:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:test:tasty" "txe:lib" "txe:exe:humanize" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:test:tasty" "sym-util:lib" "sym-util:test:tasty" "txe:lib" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:test:contract" "sym-client:test:tasty" "sym-util:lib" "sym-util:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:test:tasty" "sym-util:lib" "sym-util:test:tasty")
     (intero-targets "sym-util:lib" "sym-util:test:tasty" "txe:lib" "txe:exe:humanize" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "txe:lib" "txe:exe:humanize" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-http:lib" "sym-http:test:tasty")
     (intero-targets "sym-http:lib" "sym-http:test:tasty" "sym-logging:lib" "sym-logging:test:tasty" "sym-util:lib" "sym-util:test:tasty" "txe:lib" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:test:tasty" "sym-http:lib" "sym-http:test:tasty" "sym-logging:lib" "sym-logging:test:tasty" "sym-util:lib" "sym-util:test:tasty" "txe:lib" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:test:tasty" "sym-test:lib" "sym-test:exe:test-smartlog" "sym-test:test:tasty" "sym-util:lib" "sym-util:test:tasty")
     (intero-targets "sym-client:lib" "sym-client:test:tasty" "sym-test:lib" "sym-test:exe:test-smartlog" "sym-test:test:tasty")
     (intero-targets "sym-logging:lib" "sym-logging:test:tasty" "sym-util:lib" "sym-util:test:tasty")
     (coq-prog-args "-emacs" "-R" "/Users/arnaud/projects/cpdt/src" "Cpdt")
     (coq-prog-args "-emacs" "-R" "src" "Cpdt")
     (intero-targets "sym-cli:lib" "sym-test:lib" "sym-test:exe:test-smartlog" "sym-test:test:tasty" "txe:lib")
     (intero-targets "foo:lib" "foo:exe:foo-cli" "foo:exe:foo-service" "foo:test:tasty")
     (haskell-stylish-on-save)
     (stylish-haskell-on-save)
     (intero-targets "sym-cli:lib" "sym-cli:exe:symbiont" "sym-cli:test:tasty" "sym-http:lib" "sym-http:test:tasty" "sym-logging:lib" "sym-util:lib" "sym-util:test:tasty")
     (intero-targets "sym-cli:lib" "sym-test:lib" "sym-test:exe:test-assembly" "sym-test:test:tasty" "txe:lib")
     (intero-targets "sym-logging:lib" "sym-logging:test:tasty" "sym-util:lib" "sym-util:test:tasty" "txe:lib" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-proto:lib" "sym-proto:test:tasty")
     (intero-targets "sym-util:lib" "sym-util:test:tasty" "txe:lib" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-logging:lib" "sym-logging:test:tasty" "txe:lib" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "txe:lib" "txe:exe:txe" "txe:test:tasty")
     (intero-targets "sym-crypto:lib" "sym-logging:lib" "sym-test:lib" "sym-test:exe:test-assembly" "sym-test:test:tasty" "sym-util:lib" "txe:lib")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; https://github.com/atykhonov/google-translate
(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

;; Python
;; requires (package-install 'elpy)
;; https://github.com/jorgenschaefer/elpy
(elpy-enable)

;; Idris
;; development mode
(add-to-list 'load-path (concat (getenv "HOME") "/projects/idris/idris-mode"))
(require 'idris-mode)

;; projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

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

(use-package typescript-mode
 :ensure t)
(use-package ansi-color
 :ensure t)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package web-mode
 :ensure t)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

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
;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/
(use-package org-mime
 :ensure t)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))

; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
  ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
  mu4e-view-prefer-html t
  mu4e-update-interval 180
  mu4e-headers-auto-update t
  mu4e-compose-format-flowed t)

;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
  (lambda()
;; try to emulate some of the eww key-bindings
(local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
(local-set-key (kbd "<tab>") 'shr-next-link)
(local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
      (defun my/mu4e-change-headers ()
        (interactive)
        (setq mu4e-headers-fields
              `((:human-date . 25) ;; alternatively, use :date
                (:flags . 6)
                (:from . 22)
                (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                (:size . 7)))))

;; if you use date instead of human-date in the above, use this setting
;; give me ISO(ish) format date-time stamps in the header list
;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; spell check
(add-hook 'mu4e-compose-mode-hook
    (defun my-do-compose-stuff ()
       "My settings for message composition."
       (visual-line-mode)
       (org-mu4e-compose-org-mode)
           (use-hard-newlines -1)
       (flyspell-mode)))

(use-package smtpmail
 :ensure t)

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode

;;from the info manual
(setq mu4e-attachment-dir  "~/Downloads")

(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)

(require 'org-mu4e)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)
(define-key mu4e-headers-mode-map (kbd "C-c c") 'org-mu4e-store-and-capture)
(define-key mu4e-view-mode-map    (kbd "C-c c") 'org-mu4e-store-and-capture)

;;from vxlabs config
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; don't ask when quitting
(setq mu4e-confirm-quit nil)

;; mu4e-context
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-sent-messages-behavior 'sent)
(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "work" ;;for palo-it-gmail
        :enter-func (lambda () (mu4e-message "Entering context work"))
        :leave-func (lambda () (mu4e-message "Leaving context work"))
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches
                         msg '(:from :to :cc :bcc) "abailly@palo-it.com")))
        :vars '((user-mail-address . "abailly@palo-it.com")
                (user-full-name . "Arnaud Bailly")
                (mu4e-sent-folder . "/palo-it-gmail/[palo-it].Sent Mail")
                (mu4e-drafts-folder . "/palo-it-gmail/[palo-it].drafts")
                (mu4e-trash-folder . "/palo-it-gmail/[palo-it].Bin")
                (mu4e-compose-signature . (concat "Arnaud Bailly\n"
                                                  "Palo-IT Consultant Sénior\n"
                                                  "M +33 (0)6 17 12 19 78\n"
                                                  "1 rue Saint Julien, 44000 Nantes\n\n"
                                                  "[Emacs 25, org-mode 9, mu4e 1.0]\n"))
                (mu4e-compose-format-flowed . t)
                (smtpmail-queue-dir . "~/Maildir/palo-it-gmail/queue/cur")
                (message-send-mail-function . smtpmail-send-it)
                (smtpmail-smtp-user . "abailly@palo-it.com")
                (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                (smtpmail-default-smtp-server . "smtp.gmail.com")
                (smtpmail-smtp-server . "smtp.gmail.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-debug-info . t)
                (smtpmail-debug-verbose . t)
                (mu4e-maildir-shortcuts . ( ("/palo-it-gmail/INBOX"            . ?i)
                                            ("/palo-it-gmail/[palo-it].Sent Mail" . ?s)
                                            ("/palo-it-gmail/[palo-it].Bin"       . ?t)
                                            ("/palo-it-gmail/[palo-it].All Mail"  . ?a)
                                            ("/palo-it-gmail/[palo-it].Starred"   . ?r)
                                            ("/palo-it-gmail/[palo-it].drafts"    . ?d)
                                            ))))
       (make-mu4e-context
        :name "solina"
        :enter-func (lambda () (mu4e-message "Entering context solina"))
        :leave-func (lambda () (mu4e-message "Leaving context solina"))
        :match-func (lambda (msg)
                      (when msg
                        (or
                         (mu4e-message-contact-field-matches
                          msg '(:from :to :cc :bcc) "Arnaud.Bailly@solina-group.fr")
                         (mu4e-message-contact-field-matches
                          msg '(:from :to :cc :bcc) "Arnaud.BAILLY@solina-group.fr")
                         (mu4e-message-contact-field-matches
                          msg '(:from :to :cc :bcc) "arnaudb@solina-group.fr")
                        )))
        :vars '((user-mail-address . "Arnaud.Bailly@solina-group.fr")
                (user-full-name . "Arnaud Bailly")
                (mu4e-sent-folder . "/solina-gmail/[solina].Sent Mail")
                (mu4e-drafts-folder . "/solina-gmail/[solina].drafts")
                (mu4e-trash-folder . "/solina-gmail/[solina].Bin")
                (mu4e-compose-signature . (concat "Arnaud Bailly\n"
                                                  "Architect @ Project SWAP\n"
                                                  "M +33 (0)6 17 12 19 78\n"
                                                  "[Emacs 25, org-mode 9, mu4e 1.0]\n"))
                (mu4e-compose-format-flowed . t)
                (smtpmail-queue-dir . "~/Maildir/solina-gmail/queue/cur")
                (message-send-mail-function . smtpmail-send-it)
                (smtpmail-smtp-user . "arnaudb@solina-group.fr")
                (smtpmail-stream-type . starttls)
                (smtpmail-starttls-credentials . (("smtp.office365.com" 587 nil nil)))
                (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                (smtpmail-default-smtp-server . "smtp.office365.com")
                (smtpmail-smtp-server . "smtp.office365.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-debug-info . t)
                (smtpmail-debug-verbose . t)
                (mu4e-maildir-shortcuts . ( ("/solina-gmail/INBOX"            . ?i)
                                            ("/solina-gmail/[solina].Sent Mail" . ?s)
                                            ("/solina-gmail/[solina].Bin"       . ?t)
                                            ("/solina-gmail/[solina].All Mail"  . ?a)
                                            ("/solina-gmail/[solina].Starred"   . ?r)
                                            ("/solina-gmail/[solina].drafts"    . ?d)
                                            ))))
       ;; (make-mu4e-context
       ;;  :name "personal" ;;for acc2-gmail
       ;;  :enter-func (lambda () (mu4e-message "Entering context personal"))
       ;;  :leave-func (lambda () (mu4e-message "Leaving context personal"))
       ;;  :match-func (lambda (msg)
       ;;    (when msg
       ;;  (mu4e-message-contact-field-matches
       ;;   msg '(:from :to :cc :bcc) "acc2@gmail.com")))
       ;;  :vars '((user-mail-address . "acc2@gmail.com")
       ;;    (user-full-name . "User Account2")
       ;;    (mu4e-sent-folder . "/acc2-gmail/[acc2].Sent Mail")
       ;;    (mu4e-drafts-folder . "/acc2-gmail/[acc2].drafts")
       ;;    (mu4e-trash-folder . "/acc2-gmail/[acc2].Trash")
       ;;    (mu4e-compose-signature . (concat "Informal Signature\n" "Emacs is awesome!\n"))
       ;;    (mu4e-compose-format-flowed . t)
       ;;    (smtpmail-queue-dir . "~/Maildir/acc2-gmail/queue/cur")
       ;;    (message-send-mail-function . smtpmail-send-it)
       ;;    (smtpmail-smtp-user . "acc2")
       ;;    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
       ;;    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
       ;;    (smtpmail-default-smtp-server . "smtp.gmail.com")
       ;;    (smtpmail-smtp-server . "smtp.gmail.com")
       ;;    (smtpmail-smtp-service . 587)
       ;;    (smtpmail-debug-info . t)
       ;;    (smtpmail-debug-verbose . t)
       ;;    (mu4e-maildir-shortcuts . ( ("/acc2-gmail/INBOX"            . ?i)
       ;;  			("/acc2-gmail/[acc2].Sent Mail" . ?s)
       ;;  			("/acc2-gmail/[acc2].Trash"     . ?t)
       ;;  			("/acc2-gmail/[acc2].All Mail"  . ?a)
       ;;  			("/acc2-gmail/[acc2].Starred"   . ?r)
       ;;  			("/acc2-gmail/[acc2].drafts"    . ?d)
       ;;  			)
       ))

;; gpg
;; from https://github.com/kensanata/ggg#mac
(defun gpg-restart-agent ()
  "This kills and restarts the gpg-agent.

To kill gpg-agent, we use killall. If you know that the agent is
OK, you should just reload the environment file using
`gpg-reload-agent-info'."
  (interactive)
  (shell-command "killall gpg-agent")
  (shell-command "gpg-agent --daemon --enable-ssh-support --write-env-file")
  ;; read the environment file instead of parsing the output
  (gpg-reload-agent-info))

(defun gpg-reload-agent-info ()
  "Reload the ~/.gpg-agent-info file."
  (interactive)
  (let ((file (expand-file-name "~/.gpg-agent-info")))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "\\([A-Z_]+\\)=\\(.*\\)" nil t)
          (setenv (match-string 1) (match-string 2)))))))

(defun gpg-agent-startup ()
  "Initialize the gpg-agent if necessary.

Note that sometimes the gpg-agent can be up and running and still
be useless, in which case you should restart it using
`gpg-restart-agent'."
  (gpg-reload-agent-info)
  (let ((pid (getenv "SSH_AGENT_PID")))
    (when (and (fboundp 'list-system-processes)
               (or (not pid)
                   (not (member (string-to-number pid)
                                (list-system-processes)))))
      (gpg-restart-agent))))

(gpg-agent-startup)

(require 'go-mode)

;; plantuml
(setq plantuml-jar-path "/Users/arnaud/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))


;; powerbuilder
(load-file "/Users/arnaud/.emacs.d/powerbuilder-mode.el")
(require 'powerbuilder-mode)
(add-to-list 'auto-mode-alist '("\\.sr.*\\'" . powerbuilder-mode))

;; kill all cal-* buffers
(defun kill-cal-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (let ((buf (buffer-name buffer)))
            (when (string-match "cal-.*" buf)
              (kill-buffer buffer)
              (message "Killed buffer %s" buf))))
        (buffer-list)))

(global-set-key (kbd "C-x C-C") 'kill-cal-buffers)
