;; Emacs core settings
(setq custom-file "~/.emacs-custom.el")
(if (file-exists-p custom-file)
  (load custom-file))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(unless (string-equal system-type "darwin")
  (menu-bar-mode -1))

;; Check which fonts are available and set the font.
(cond
  ((find-font (font-spec :name "SauceCodePro Nerd Font Mono"))
    (set-frame-font "SauceCodePro Nerd Font Mono-12"))
  ((find-font (font-spec :name "Menlo"))
    (set-frame-font "Menlo-12"))
  ((find-font (font-spec :name "Monospace"))
    (set-frame-font "Monospace-12")))

;; Add the melpa repo
;; https://github.com/melpa/melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; use-package setup
(eval-when-compile
  (require 'use-package))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))
