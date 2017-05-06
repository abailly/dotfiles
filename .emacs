;; General layout

;; package installation
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;;; Code:
(add-hook 'window-setup-hook
         (lambda nil
            ;; font setting
           (set-frame-parameter (selected-frame) 'alpha '(80 80))
           (set-face-background 'hl-line "#ff0")
	   (set-face-attribute 'default nil
                    :background "white"
                    :foreground "black"
                    :family "Monaco"
                    :height 140)

           ))

(setq fill-column 132)
(global-set-key "\C-cg" 'goto-line)

(global-hl-line-mode 1)

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
        ("x" "Capture" entry (file+datetree "~/log/journal.org")
         "* %c" :immediate-finish t)))

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
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

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

;; load correct tags file for current file
(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))

(defun load-tags ()
  (let ((my-tags-file (find-file-upwards "TAGS")))
    (when my-tags-file
      (if (not (equal tags-file-name my-tags-file))
          (progn
            (message "Loading tags file: %s" my-tags-file)
            (setq tags-file-name nil)
            (setq tags-table-list nil)
            (visit-tags-table my-tags-file)
            )))))

(add-hook 'elm-mode-hook 'load-tags)
(add-hook 'find-file-hook 'load-tags)

(defun switch-to-buffer-and-load-tags ()
  (interactive)
  (ido-switch-buffer)
  (load-tags))

(defun goto-tag-at-point ()
  "Go to tag at point."
  (interactive)
  (let ((tag (find-tag-default)))
    (unless tag
      (user-error "No tag candidate found around point"))
    (find-tag tag)))

(global-set-key "\C-xb" 'switch-to-buffer-and-load-tags)

;; magit
(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
