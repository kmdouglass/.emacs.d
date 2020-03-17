;;-------------------------------------------------------------------------------------------------
;; Python

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
