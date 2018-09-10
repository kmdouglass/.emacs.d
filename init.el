;;--------------------------------------------------------------------
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

;;--------------------------------------------------------------------
;; python

;; configure directory containing virtual environments
(setenv "WORKON_HOME"
	(concat
	 (getenv "HOME")
	 "/venvs"
	))
(straight-use-package 'elpy)
(elpy-enable)

;; setup the Python linters for flycheck
(defvar linter-execs '((flycheck-python-flake8-executable "bin/flake8")
                       (flycheck-python-pylint-executable "bin/pylint")
                       (flycheck-python-pycompile-executable "bin/python")))
(defvar default-linter-venv-path (concat (getenv "WORKON_HOME") "/linters/"))

(defun switch-linters ()
  "Switch linter executables to those in the current venv.

If the venv does not have any linter packages, then they will be
set to those in the `default-linter-venv-path` venv.  If these do
not exist, then no linter will be set."
  (dolist (exec linter-execs)
    (let ((venv-linter-bin    (concat pyvenv-virtual-env (nth 1 exec)))
          (default-linter-bin (concat default-linter-venv-path (nth 1 exec)))
	  (flycheck-var       (nth 0 exec)))
      (cond ((file-exists-p venv-linter-bin)
	     (set flycheck-var venv-linter-bin))
            ((file-exists-p default-linter-bin)
	     (set flycheck-var default-linter-bin))
	    (t (set flycheck-var nil))))))

(add-hook 'pyvenv-post-activate-hooks 'switch-linters)

;;--------------------------------------------------------------------
;; interactively do things
(require 'ido)
(ido-mode t)

;;--------------------------------------------------------------------
;; fill-column-indicator
(straight-use-package 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

;;--------------------------------------------------------------------
;; auto-complete mode
(straight-use-package 'auto-complete)
(ac-config-default)
(global-auto-complete-mode t)

;;--------------------------------------------------------------------
;; flycheck
(straight-use-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;--------------------------------------------------------------------
;; misc syntax highlighting modes
(straight-use-package 'yaml-mode)
(straight-use-package 'hcl-mode)
(straight-use-package 'jinja2-mode)

;;--------------------------------------------------------------------
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
 )
