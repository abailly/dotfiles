(require 'package)

; find package information from following archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(mapcar (lambda (package)
					; install package if not already installed
          (unless (package-installed-p package)
            (package-install package)))

        '(magit
          intero
          helm
          markdown-mode
          multiple-cursors
          expand-region
          yasnippet
          js2-mode
          js2-refactor
          xref-js2
          typescript-mode
          web-mode
          color-theme
          elpy
          prop-menu
          google-translate
          projectile
          yaml-mode
          use-package
          elm-mode
          terraform-mode
          nix-mode
          nix-sandbox))
