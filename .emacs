;; General layout

;; package installation
(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
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

(setq fill-column 132)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-c\C-g" 'rgrep)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-hl-line-mode 1)

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

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/log/todo.org" "Tasks")
         "*** TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/log/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("s" "Symbiont" entry (file+datetree "~/log/symbiont.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("p" "Capture" entry (file+datetree "~/log/journal.org")
         "* %c\n%i" :immediate-finish t)
        ("L" "Capture link" entry (file+datetree "~/log/journal.org")
         "* %? [[%:link][%:description]] \nCaptured On: %U" :immediate-finish t)))

(add-hook 'org-mode-hook 'auto-fill-mode)

(setq org-todo-keywords
           '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d)")
             (sequence "WAITING(w)" "|" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
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

(require 'intero)
(require 'flycheck)
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'linum-mode)

;; stylish haskell formatting
(setq haskell-stylish-on-save t)
(setq haskell-tags-on-save t)

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
 '(package-selected-packages
   (quote
    (magit expand-region company-go go-autocomplete go-complete go-mode go company-coq helm-core helm helm-ag-r helm-company helm-git helm-google helm-hoogle helm-idris helm-ls-git markdown-mode multiple-cursors magit-gh-pulls intero flx-isearch flx-ido)))
 '(safe-local-variable-values
   (quote
    ((haskell-indentation-where-pre-offset . 4)
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
(require 'yaml-mode)

;; javascript
(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(require 'typescript-mode)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'web-mode)
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
(require 'elm-mode)

;; terraform
(require 'terraform-mode)

;; shellcheck
;;http://www.skybert.net/emacs/bash-linting-in-emacs/
(add-hook 'sh-mode-hook 'flycheck-mode)
