;;-------------------------------------------------------------------------------------------------
;; C/C++

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
