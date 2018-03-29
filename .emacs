;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; code:

;; .emacs stuff
(global-set-key (kbd "<f1>") (lambda() (interactive)(find-file "~/.emacs")))
(global-set-key (kbd "C-c r") (lambda() (interactive)(load-file "~/.emacs")))

;; packages
(require 'package)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("MELPA Stable" . "http://stable.melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;
;; Color scheme
;;;;;;;;;;;;;;;;;;

(defun setup-color-scheme ()
  "Set up the color scheme."
  (add-to-list 'default-frame-alist '(font . "Consolas-20"))
  (set-face-attribute 'default t :font "Consolas-20")
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
  (set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
  (set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
  (set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
  (set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
  (set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
  (set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3"))
(setup-color-scheme)

(eval-after-load "color shceme"
  (progn
    (menu-bar-mode -99)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1)
    (set-foreground-color "burlywood3")
    (set-background-color "#161616")
    (global-hl-line-mode 1)
    (set-face-background 'hl-line "#111111")
    (set-cursor-color "#00F5FF")))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "ANSI color in compilation buffer."
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;
;; Misc Settings
;;;;;;;;;;;;;;;;;;

(setq select-enable-clipboard t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.local/share/emacs/backups"))))
 '(compilation-context-lines 2)
 '(compilation-error-screen-columns nil)
 '(compilation-scroll-output t)
 '(compilation-search-path (quote (nil "src")))
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(indent-tabs-mode nil)
 '(line-move-visual t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(next-line-add-newlines nil)
 '(package-selected-packages
   (quote
    (key-chords evil-mc racer flx-ido helm-projectile projectile fiplr exec-path-from-shell flycheck-rust rustfmt rust-mode use-package auto-complete)))
 '(require-final-newline t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(visible-bell t))

(setq-default electric-indent-mode t)
(global-linum-mode 1)

(defun format-current-buffer()
  (indent-region (point-min) (point-max))
  (untabify-current-buffer))
(defun untabify-current-buffer()
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max)))
  nil)
(add-to-list 'write-file-functions 'untabify-current-buffer)
(add-to-list 'write-file-functions 'delete-trailing-whitespace)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
(global-set-key [C-tab] (lambda ()
                          (interactive)
                          (format-current-buffer)))

;; ;; ;; Some key bindings

(global-set-key [f3] 'next-match)
(defun prev-match () (interactive nil) (next-match -1))
(global-set-key [(shift f3)] 'prev-match)
(global-set-key (kbd "C-`") 'eshell)

;;;;;;;;;;;;;;;;;;;;;;
;; Package Settings
;;;;;;;;;;;;;;;;;;;;;;

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.4))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (key-chord-define evil-insert-state-map "jj" (lambda () (interactive)
                                                 (when (not evil-mc-cursor-list)
                                                   (evil-normal-state))))
  (defun delete-selection-and-paste ()
    (interactive)
    (delete-region (region-beginning) (region-end))
    (yank))
  (evil-define-operator evil-change-line-no-yank (beg end type register yank-handler)
    "Change to end of line without yanking."
    :motion evil-end-of-line
    (interactive "<R><x><y>")
    (evil-change beg end type ?_ yank-handler #'evil-delete-line))
  (evil-define-operator evil-change-no-yank (beg end type register yank-handler)
    "Change without yanking."
    (evil-change beg end type ?_ yank-handler))
  (evil-define-operator evil-change-whole-line-no-yank (beg end type register yank-handler)
    :motion evil-line
    (interactive "<R><x>")
    (evil-change beg end type ?_ yank-handler #'evil-delete-whole-line))

  (define-key evil-visual-state-map (kbd "p") 'delete-selection-and-paste)
  (define-key evil-normal-state-map (kbd "C") 'evil-change-line-no-yank)
  (define-key evil-normal-state-map (kbd "c") 'evil-change-no-yank)
  (define-key evil-visual-state-map (kbd "c") 'evil-change-no-yank)
  (define-key evil-visual-state-map (kbd "S") 'evil-change-whole-line-no-yank)
  (define-key evil-normal-state-map (kbd "m") 'push-bookmark)
  (define-key evil-visual-state-map (kbd "m") 'push-bookmark)
  (define-key evil-normal-state-map (kbd "C-k") 'pop-bookmark)
  (define-key evil-normal-state-map (kbd "'") 'pop-bookmark)

  (modify-syntax-entry ?_ "w"))

(use-package evil-mc
  :ensure t
  :after evil
  :config
  (global-evil-mc-mode 1)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-mc-make-and-goto-next-match)
  (define-key evil-visual-state-map (kbd "C-d") 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-mc-undo-all-cursors)
  (define-key evil-visual-state-map (kbd "C-l") 'evil-mc-undo-all-cursors)
  )

;; (use-package evil-multiedit
;;   :ensure t
;;   :after evil
;;   :config
;;   (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;;   (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-and-next)
;;   )

(use-package whitespace
  :ensure t
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; (use-package fiplr
;;   :ensure t
;;   :bind (("C-SPC" . fiplr-find-file)
;;          ("<f5>" . fiplr-clear-cache)))

;; (use-package projectile
;;   :ensure)

;; (use-package helm-projectile
;;   :ensure)

(use-package ido
  :ensure
  :init (ido-mode t))

(use-package flx-ido
  :ensure
  :init (flx-ido-mode)
  :bind (("C-SPC" . find-file)))
;; :config ((setq ido-enable-flex-matching t)))

;; (use-package auto-complete
;;   :ensure
;;   :bind (("TAB" . auto-complete)
;;          ([backtab] . auto-complete))
;;   :config (global-auto-complete-mode t)
;;   )

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

(use-package rust-mode
  :ensure
  :config (setq rust-format-on-save nil))

;; (use-package flycheck-rust
;;   :ensure
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;;   )

;; (use-package racer
;;   :ensure
;;   :config
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq bookmark-stack '())
(defun push-bookmark ()
  (interactive)
  (when buffer-file-name
    (let ((b (list buffer-file-name (point))))
      (setq bookmark-stack (cons b bookmark-stack))
      (prin1 b)
      )))

(defun pop-bookmark ()
  (interactive)
  (when bookmark-stack
    (let ((b (pop bookmark-stack)))
      (prin1 b)
      (find-file (elt b 0))
      (goto-char (elt b 1)))))

(provide '.emacs)
;;; .emacs ends here
