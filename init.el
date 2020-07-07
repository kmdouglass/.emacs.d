;;-------------------------------------------------------------------------------------------------
;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Read .bashrc on startup
(straight-use-package 'exec-path-from-shell)
(setq exec-path-from-shell-variables
      (list "PATH" "MANPATH" "ORG_SOURCE" "WORKON_HOME"))
(exec-path-from-shell-initialize)

;;-------------------------------------------------------------------------------------------------
;; Ergonomics and ease-of-use

;; Remap keys (Colemak friendly)
(global-set-key (kbd "C-t") 'forward-char)
(global-set-key (kbd "C-f") 'backward-char)

;; God mode
(straight-use-package 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; Fill paragraph
(defun unfill-paragraph ()
  "Undo the 'fill-paragraph' operation that is used for wrapping text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; transpose-frame
(straight-use-package 'transpose-frame)
(global-set-key (kbd "C-c t") 'transpose-frame)

;;-------------------------------------------------------------------------------------------------
;; ibuffer
(global-set-key (kbd"C-x C-b") 'ibuffer)

;;-------------------------------------------------------------------------------------------------
;; global line numbers mode
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;;-------------------------------------------------------------------------------------------------
;; interactively do things
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;;-------------------------------------------------------------------------------------------------
;; fill-column-indicator
(straight-use-package 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

(setq-default fill-column 99)

;;-------------------------------------------------------------------------------------------------
;; dired
(put 'dired-find-alternate-file 'disabled nil)

;;-------------------------------------------------------------------------------------------------
;; magit
(straight-use-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;-------------------------------------------------------------------------------------------------
;; flycheck
(straight-use-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;-------------------------------------------------------------------------------------------------
;; lsp
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;-------------------------------------------------------------------------------------------------
;; keychain-environment
(straight-use-package 'keychain-environment)

;;-------------------------------------------------------------------------------------------------
;; Company mode
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-limit 10) ; bigger popup window
(setq company-idle-delay 0.0)   ; decrease delay before autocompletion popup shows

(straight-use-package 'company-lsp)

;;-------------------------------------------------------------------------------------------------
;; yasnippet
(straight-use-package 'yasnippet)
(yas-global-mode 1)

;;-------------------------------------------------------------------------------------------------
;; Org mode

;; Use org-mode from source code if it exists
(defvar kmdouglass-org-source (getenv "ORG_SOURCE")
  "The home directory of the `org-mode` source code.")
(if kmdouglass-org-source
    (progn
      (message "ORG_SOURCE defined; using custom org-mode directory")
      (add-to-list 'load-path (concat kmdouglass-org-source "/lisp"))
      (add-to-list 'load-path (concat kmdouglass-org-source "/contrib/lisp") t)
      (require 'org)
      (require 'org-protocol))
    (straight-use-package 'org))

(setq org-directory "~/Dropbox/Org/")

;; Todos
(setq todo-file (concat org-directory "todo-kmd.org"))

(defun open-todos (switch)
  "Opens the todos file on startup"
  (setq initial-buffer-choice todo-file))
(add-to-list 'command-switch-alist '("-todos" . open-todos))

;; Agenda
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files (list todo-file))

;; Capture
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-default-notes-file (concat org-directory "todo-kmd.org"))
(setq org-capture-templates
      '(("t" "TODO" entry (file "")
         "* TODO %?\n  %i\n")
	("p" "TODO-org-protocol" entry (file "")
	 "* TODO %:link\n %i\n"
	 :immediate-finish 1 :empty-lines 1)))

;; Language integrations
(org-babel-do-load-languages 'org-babel-load-languages '(
    (ditaa . t)
    (python . t)
    (shell . t))
)
(setq org-babel-python-command "python3")

;; org-present
(straight-use-package 'org-present)
(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-present-hide-cursor)
            (org-present-read-only)
            (org-display-inline-images)
            (fci-mode -1)))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-present-show-cursor)
            (org-present-read-write)
            (org-remove-inline-images)
	    (fci-mode)))

;; Misc
(setq org-log-done t)

;;-------------------------------------------------------------------------------------------------
;; deft
(straight-use-package 'deft)
(setq deft-extensions '("org"))
(setq deft-directory "~/Dropbox/Org")
(setq deft-recursive t)

;;-------------------------------------------------------------------------------------------------
;; Docker
(straight-use-package 'docker)
(global-set-key (kbd "C-c d") 'docker)

(straight-use-package 'docker-tramp)

;;-------------------------------------------------------------------------------------------------
;; ripgrep
(straight-use-package 'rg)
(global-set-key (kbd "M-s g") 'rg)
(global-set-key (kbd "M-s d") 'rg-dwim)

;;-------------------------------------------------------------------------------------------------
;; gdb

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t)

;;-------------------------------------------------------------------------------------------------
;; Programming language specifics

;; Python
(load-file ".emacs.d/dev-python.el")

;; Rust
(load-file ".emacs.d/dev-rust.el")

;;-------------------------------------------------------------------------------------------------
;; shell

;; Set the cabal directory, where cabal is installed
(defvar shellcheck-executable (concat (getenv "HOME") "/.cabal/bin/shellcheck")
  "The location of the shellcheck executable.")
(if (file-exists-p shellcheck-executable)
    (setq flycheck-sh-shellcheck-executable shellcheck-executable))
(add-hook 'sh-mode-hook 'flycheck-mode)

;;-------------------------------------------------------------------------------------------------
;; Powershell
(straight-use-package 'powershell)

;;-------------------------------------------------------------------------------------------------
;; Go

(straight-use-package 'go-mode)
(straight-use-package 'govet)
(straight-use-package 'golint)
(straight-use-package 'company-go)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

;;-------------------------------------------------------------------------------------------------
;; HCL
(straight-use-package 'hcl-mode)
(add-to-list 'auto-mode-alist '("\\.tf" . hcl-mode))

;;-------------------------------------------------------------------------------------------------
;; NPM
(let ((npm-home (concat (getenv "HOME") "/npm/bin")))
  (setenv "PATH" (concat (getenv "PATH") (concat ":" npm-home)))
  (setq exec-path (append exec-path (list npm-home))))

;;-------------------------------------------------------------------------------------------------
;; JSON
(straight-use-package 'json-mode)
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;;-------------------------------------------------------------------------------------------------
;; misc syntax highlighting modes
(straight-use-package 'cmake-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'jinja2-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'yaml-mode)

;;-------------------------------------------------------------------------------------------------
;; Generic startup options
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;-------------------------------------------------------------------------------------------------
;; Remove the toolbar
(tool-bar-mode -1)

;;-------------------------------------------------------------------------------------------------
;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(frame-background-mode (quote dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-blue ((t (:background "deep sky blue" :foreground "deep sky blue"))))
 '(term-color-green ((t (:background "dark green" :foreground "green3")))))
