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
(setenv "WORKON_HOME"
	(concat
	 (getenv "HOME")
	 "/venvs"
	))
(straight-use-package 'elpy)
(elpy-enable)

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
(setq flycheck-python-flake8-executable
      (concat
       (getenv "WORKON_HOME")
       "/linters/bin/flake8"
      ))
(setq flycheck-python-pycompile-executable
      (concat
       (getenv "WORKON_HOME")
       "/linters/bin/python3"
      ))

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
