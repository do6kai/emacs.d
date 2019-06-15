;; Emacs core settings
(setq custom-file "~/.emacs-custom.el")
(if (file-exists-p custom-file)
  (load custom-file))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(unless (string-equal system-type "darwin")
  (menu-bar-mode -1))

(blink-cursor-mode -1)

;; Check which fonts are available and set the font.
(cond
  ((find-font (font-spec :name "SauceCodePro Nerd Font Mono"))
    (set-frame-font "SauceCodePro Nerd Font Mono-12"))
  ((find-font (font-spec :name "Menlo"))
    (set-frame-font "Menlo-12"))
  ((find-font (font-spec :name "Monospace"))
    (set-frame-font "Monospace-12")))

;; Fix MacOS keys
(if (string-equal system-type "darwin")
    (progn
      (setq mac-option-key-is-meta nil)
      (setq mac-command-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package auctex
  :defer t
  :ensure t
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-save-query nil)
    (pdf-tools-install)
    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
    (setq 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    (setq TeX-source-correlate-method 'synctex)
    (setq TeX-source-correlate-mode t)
    (setq-default TeX-master nil)
    (setq reftex-plug-into-AUCTeX t)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)))

(use-package latex-pretty-symbols
  :ensure t)

(use-package magic-latex-buffer
  :ensure t
  :config
  (add-hook 'TeX-mode-hook 'magic-latex-buffer))

(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

(use-package auctex-latexmk
  :ensure t)

(use-package latex-preview-pane
  :ensure t)
