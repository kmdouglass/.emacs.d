;;; dev-rust.el --- Initialization routines for Rust development in Emacs

(straight-use-package 'rust-mode)
(straight-use-package 'toml-mode)

(straight-use-package 'cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'lsp)

(straight-use-package 'flycheck-rust)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
