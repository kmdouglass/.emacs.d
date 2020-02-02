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
;; Remap keys (Colemak friendly)
(global-set-key (kbd "C-t") 'forward-char)
(global-set-key (kbd "C-f") 'backward-char)

;; ibuffer
(global-set-key (kbd"C-x C-b") 'ibuffer)

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
;; flycheck
(straight-use-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;-------------------------------------------------------------------------------------------------
;; Company mode
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-limit 10) ; bigger popup window
(setq company-idle-delay .3)    ; decrease delay before autocompletion popup shows

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

;; Misc
(setq org-log-done t)

;;-------------------------------------------------------------------------------------------------
;; ggtags
(straight-use-package 'ggtags)
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;;-------------------------------------------------------------------------------------------------
;; gdb

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t)

;;-------------------------------------------------------------------------------------------------
;; Python
;; Enable line numbers
(add-hook 'python-mode-hook 'display-line-numbers-mode)

;; elpy configuration
(straight-use-package 'elpy)
(elpy-enable)
(setq elpy-rpc-backend "jedi")

;; Disable flymake in elpy
(setq elpy-modules (delete 'elpy-module-flymake elpy-modules))

;; setup the Python linters for flycheck
(defvar linter-execs '((flycheck-python-flake8-executable "bin/flake8")
                       (flycheck-python-pylint-executable "bin/pylint")
                       (flycheck-python-pycompile-executable "bin/python")))
(defvar default-linter-venv-path (concat (getenv "WORKON_HOME") "/linters/"))


;; Credit: Gareth Rees
;; https://codereview.stackexchange.com/questions/203808/hook-to-switch-the-linter-binaries-in-emacs-lisp-according-to-virtual-environment
(defun switch-linters ()
  "Switch linter executables to those in the current venv.

If the venv does not have any linter packages, then they will be
set to those in the `default-linter-venv-path` venv.  If these do
not exist, then no linter will be set."
(cl-loop with dirs = (list pyvenv-virtual-env default-linter-venv-path)
         for (flycheck-var path) in linter-execs
         do (set flycheck-var (locate-file path dirs nil 'file-executable-p))))

(add-hook 'pyvenv-post-activate-hooks 'switch-linters)

;;-------------------------------------------------------------------------------------------------
;; C/C++

;; Enable line numbers
(add-hook 'c++-mode-hook 'display-line-numbers-mode)
(add-hook 'c-mode 'display-line-numbers-mode)

(require 'semantic)
(global-semanticdb-minor-mode t)
(global-semantic-idle-scheduler-mode t)
(semantic-mode t)

;; Enable semantic mode for just C/C++
(setq semantic-new-buffer-setup-functions '((c-mode . semantic-default-c-setup)
					    (c++-mode . semantic-default-c-setup)))

;; Enable EDE mode and project files (for code completion)
(global-ede-mode t)
(setq ede-custom-file (expand-file-name "cc-mode-projects.el" user-emacs-directory))
(when (file-exists-p ede-custom-file)
  (load ede-custom-file))

;; Use company-header for header completion
(straight-use-package 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

;;-------------------------------------------------------------------------------------------------
;; shell
(add-hook 'sh-mode-hook 'display-line-numbers-mode)

;; Set the cabal directory, where cabal is installed
(defvar shellcheck-executable (concat (getenv "HOME") "/.cabal/bin/shellcheck")
  "The location of the shellcheck executable.")
(if (file-exists-p shellcheck-executable)
    (setq flycheck-sh-shellcheck-executable shellcheck-executable))
(add-hook 'sh-mode-hook 'flycheck-mode)

;;-------------------------------------------------------------------------------------------------
;; Powershell
(straight-use-package 'powershell)
(add-hook 'powershell-mode-hook 'display-line-numbers-mode)

;;-------------------------------------------------------------------------------------------------
;; Rust
(straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook 'display-line-numbers-mode)

(straight-use-package 'cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(straight-use-package 'flycheck-rust)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

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
            (setq indent-tabs-mode 1)
	    (display-line-numbers-mode)))

;;-------------------------------------------------------------------------------------------------
;; HCL
(straight-use-package 'hcl-mode)
(add-hook 'hcl-mode-hook 'display-line-numbers-mode)
(add-to-list 'auto-mode-alist '("\\.tf" . hcl-mode))

;;-------------------------------------------------------------------------------------------------
;; NPM
(let ((npm-home (concat (getenv "HOME") "/npm/bin")))
  (setenv "PATH" (concat (getenv "PATH") (concat ":" npm-home)))
  (setq exec-path (append exec-path (list npm-home))))

;;-------------------------------------------------------------------------------------------------
;; JSON
(straight-use-package 'json-mode)
(add-hook 'json-mode-hook 'display-line-numbers-mode)
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
