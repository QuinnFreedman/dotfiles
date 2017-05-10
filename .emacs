
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("MELPA Stable" . "http://stable.melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)


;; personal settings

(defun setup-color-scheme ()
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


(setq x-select-enable-clipboard t)
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
 '(electric-indent-mode nil)
 '(indent-tabs-mode nil)
 '(line-move-visual t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(next-line-add-newlines nil)
 '(package-selected-packages
   (quote
    (markdown-mode flycheck-ocaml ocp-indent merlin tuareg auto-complete)))
 '(require-final-newline t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(visible-bell t))

;; (setq-default electric-indent-mode t)
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


(ac-config-default)
(global-auto-complete-mode t)
(global-set-key (kbd "TAB") 'auto-complete)

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share"))))
      (opam-bin   (ignore-errors (car (process-lines "opam" "config" "var" "bin")))))
  (when (and opam-share opam-bin (file-directory-p opam-share) (file-directory-p opam-bin))

    (setq exec-path (append exec-path (list "/usr/local/bin")))
    (setq exec-path (append exec-path (list opam-bin)))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))

    (require 'ocp-indent)
    (setq ocp-indent-path (expand-file-name "ocp-indent" opam-bin))
    (setq ocp-indent-config "strict_with=always,match_clause=4,strict_else=never")

    (require 'merlin)
    ;; (setq merlin-command "/home/QuinnFreedman/.opam/4.04.0+mingw64c/bin/ocamlmerlin.exe")

    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (setq merlin-report-warnings t)

    (with-eval-after-load 'merlin
      (setq merlin-command 'opam))

    (add-to-list 'load-path "~/.elisp/tuareg-mode")
    (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
    (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
    ;; (autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
    ;; "Configuration of imenu for tuareg" t)

    ;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
    (setq auto-mode-alist
          (append '(("\\.ml[ily]?$" . tuareg-mode)
                    ("\\.topml$" . tuareg-mode))
                  auto-mode-alist))

    (require 'flycheck-ocaml)
    (with-eval-after-load 'merlin
      ;; Disable Merlin's own error checking
      (setq merlin-error-after-save nil)
      (flycheck-ocaml-setup))

    (autoload 'flycheck-mode "flycheck")

    ))

;; ANSI color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; ;; ;; Some key bindings

(global-set-key [f3] 'next-match)
(defun prev-match () (interactive nil) (next-match -1))
(global-set-key [(shift f3)] 'prev-match)
;;(global-set-key [backtab] 'auto-complete)
;; OCaml figuration
;;  - better error and backtrace matching

(defun set-ocaml-error-regexp ()
  (set
   'compilation-error-regexp-alist
   (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
           2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))

(add-hook 'tuareg-mode-hook 'set-ocaml-error-regexp)
(add-hook 'caml-mode-hook 'set-ocaml-error-regexp)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(global-set-key (kbd "C-b")
                (lambda ()
                  (interactive)
                  (shell-command "make")))

(setq merlin-use-auto-complete-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
