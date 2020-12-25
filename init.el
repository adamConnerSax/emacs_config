(setq lexical-binding t)
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

(straight-use-package 'use-package)
(use-package helm
  :straight t
  :bind
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)  
  )

;; (use-package helm-show-kill-ring
;;   :straight t
;;   :after helm
;;   :bind ("M-y" 
;;   )

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-unset-key (kbd "C-x c"))
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;;(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


(let ((my-local-path (expand-file-name "~/.local/bin"))
      (my-cabal-path (expand-file-name "~/.cabal/bin"))
      (my-ghcup-path (expand-file-name "~/.ghcup/bin"))
      (user-local-path (expand-file-name "/usr/local/bin")))
  (setenv "PATH" (concat (getenv "PATH") path-separator my-ghcup-path path-separator my-cabal-path path-separator user-local-path path-separator my-local-path))
  (add-to-list 'exec-path my-cabal-path)
  (add-to-list 'exec-path user-local-path)
  (add-to-list 'exec-path my-local-path)
  (add-to-list 'exec-path my-ghcup-path)
  )
;; some additions 12/24/2020
(setq gc-cons-threshold 100000000)
(use-package gnu-elpa-keyring-update
  :straight t
  )
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)
(unbind-key "C-x C-d") ;; list-directory
(unbind-key "C-z") ;; suspend-frame
(unbind-key "M-o") ;; facemenu-mode
(unbind-key "<mouse-2>") ;; pasting with mouse-wheel click
(unbind-key "<C-wheel-down>") ;; text scale adjust
(use-package keychain-environment
  :straight t
  :config
  (keychain-refresh-environment))

(use-package undo-tree
  :straight t
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))

(ignore-errors (set-frame-font "Menlo-12"))

(use-package all-the-icons
  :straight t
  )

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-themes
  :straight t
  :config
  (let ((chosen-theme 'doom-one-light))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t)
    (load-theme chosen-theme)))

(use-package dimmer
  :straight t
  :custom (dimmer-fraction 0.1)
  :config (dimmer-mode))

(show-paren-mode)

(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package magit
  :straight t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/src" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package forge
  :straight t
  :after magit)
;;

(helm-mode 1)
;; LSP
(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode t)
  :custom
  (flycheck-haskell-hlint-executable "~/.cabal/bin/hlint")
  )
(use-package yasnippet
  :straight t
  )
(use-package lsp-mode
  :straight t
  :hook (haskell-mode . lsp)
  :commands (lsp lsp-execute-code-action)
  :custom
  (lsp-print-performance t)
  (lsp-log-io t)
  (lsp-file-watch-threshold 5000)
  (lsp-enable-file-watchers nil)
;;  (lsp-file-watch-ignored (append '("\\dist" "\\dist-newstyle") lsp-file-watch-ignored))  
  )

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
;;  :config
;;  (setq lsp-ui-doc-enable t
;;        lsp-ui-doc-use-childframe t
;;        lsp-ui-doc-position 'top
;;        lsp-ui-doc-include-signature t
;;        lsp-ui-sideline-enable nil
;;        lsp-ui-flycheck-enable t
;;        lsp-ui-flycheck-list-position 'right
;;        lsp-ui-flycheck-live-reporting t
;;        lsp-ui-peek-enable t
;;        lsp-ui-peek-list-width 60
;;        lsp-ui-peek-peek-height 25)
  
  ;;  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )
(use-package lsp-haskell
 :straight t
 :config
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;; (setq lsp-log-io t)
 )
(use-package haskell-mode
  :straight t
  :config
  ;; haskell-mode doesn't know about newer GHC features.
  (let ((new-extensions '("QuantifiedConstraints"
                          "DerivingVia"
                          "BlockArguments"
                          "DerivingStrategies"
                          "StandaloneKindSignatures"
                          )))
    (setq
     haskell-ghc-supported-extensions
     (append haskell-ghc-supported-extensions new-extensions)))

  :bind (("C-c a c" . haskell-cabal-visit-file)
         ("C-c a i" . haskell-navigate-imports)
         ("C-c a I" . haskell-navigate-imports-return))
  )

(use-package haskell-snippets
  :straight t
  :after (haskell-mode yasnippet)
  :defer)


;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)
;; This lets me say where my temp dir is.
(setq temporary-file-directory "~/.emacs.d/tmp/")

;; Stan
(use-package stan-mode
  :straight t
  )
(use-package company-stan
  :straight t
  :hook (stan-mode . company-stan-setup)
  )

(setq custom-file (make-temp-file "~/.emacs.d/emacs-custom/"))
