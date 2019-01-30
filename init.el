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

;;-------------------------------------------------------------------------------------------------
;; Remap keys (Colemak friendly)
(global-set-key (kbd "C-t") 'forward-char)
(global-set-key (kbd "C-f") 'backward-char)

;;-------------------------------------------------------------------------------------------------
;; Company mode
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;-------------------------------------------------------------------------------------------------
;; Python

;; configure directory containing virtual environments
(setenv "WORKON_HOME"
	(concat
	 (getenv "HOME")
	 "/venvs"
	))

;; Enable line numbers
(add-hook 'python-mode-hook 'display-line-numbers-mode)

;; elpy configuration
(straight-use-package 'elpy)
(elpy-enable)
(setq elpy-rpc-backend "jedi")

;; Automatically run Black on buffer save
(add-hook 'elpy-mode-hook
          '(lambda ()
             (when (eq major-mode 'python-mode)
               (add-hook 'before-save-hook 'elpy-black-fix-code))))

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

(require 'semantic)
(global-semanticdb-minor-mode t)
(global-semantic-idle-scheduler-mode t)
(semantic-mode t)

;; Enable line numbers
(add-hook 'c++-mode-hook 'display-line-numbers-mode)
(add-hook 'c-mode 'display-line-numbers-mode)

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
;; interactively do things
(require 'ido)
(ido-mode t)

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
;; misc syntax highlighting modes
(straight-use-package 'cmake-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'hcl-mode)
(straight-use-package 'jinja2-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'yaml-mode)

;;-------------------------------------------------------------------------------------------------
;; Tip of the day
;; https://gist.github.com/saintaardvark/375aa054c15f02c42f45

(defun totd ()
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (cl-loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n"
               "========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
(buffer-string)))))))
(totd)
;;-------------------------------------------------------------------------------------------------
;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; Fix dark blue font color in the terminal
 '(term-color-blue ((t (:background "deep sky blue" :foreground "deep sky blue")))))
