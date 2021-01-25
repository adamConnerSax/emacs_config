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

;; Keybonds for MacEmacs
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper x)] 'yank)

;; mac switch meta key
(defun mac-switch-meta nil
"switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'hyper)
	)
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  )
(setq mac-option-modifier 'meta)
(setq mac-command-modifier `hyper)
(setq
   ;; No need to see GNU agitprop.
   inhibit-startup-screen t
   ;; No need to remind me what a scratch buffer is.
   initial-scratch-message nil
   ;; Double-spaces after periods is morally wrong.
   sentence-end-double-space nil
   ;; Never ding at me, ever.
   ring-bell-function 'ignore
   ;; Prompts should go in the minibuffer, not in a GUI.
   use-dialog-box nil
   ;; Fix undo in commands affecting the mark.
   mark-even-if-inactive nil
   ;; Let C-k delete the whole line.
   kill-whole-line t
   ;; search should be case-sensitive by default
   case-fold-search nil
   )

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; Accept 'y' in lieu of 'yes'.

;; unicode defaults
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;;
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)

(global-unset-key (kbd "C-x c"))
(setq custom-safe-themes t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)


(use-package gnu-elpa-keyring-update
  :straight t
  )
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)
(unbind-key "C-x C-d") ;; list-directory
(unbind-key "C-z") ;; suspend-frame
(unbind-key "M-o") ;; facemenu-mode
(unbind-key "<mouse-2>") ;; pasting with mouse-wheel click
(unbind-key "<C-wheel-down>") ;; text scale adjust

(electric-pair-mode)

(use-package counsel
  :straight t
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :straight t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
	 ("C-x b" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :straight t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev))

(use-package swiper
  :straight t
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper))
  )

(let ((my-local-path (expand-file-name "~/.local/bin"))
      (my-cabal-path (expand-file-name "~/.cabal/bin"))
      (my-ghcup-path (expand-file-name "~/.ghcup/bin"))
      (user-local-path (expand-file-name "/usr/local/bin")))
  (setenv "PATH" (concat  path-separator my-ghcup-path path-separator my-cabal-path path-separator user-local-path path-separator my-local-path (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path)
  (add-to-list 'exec-path user-local-path)
  (add-to-list 'exec-path my-local-path)
  (add-to-list 'exec-path my-ghcup-path)
  )

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

;; (use-package doom-modeline
;;   :straight t
;;   :config (doom-modeline-mode)
;;   )

(use-package dimmer
  :straight t
  :custom (dimmer-fraction 0.1)
  :config (dimmer-mode))

(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom
  (which-key-idle-delay 1.2)
  (which-key-enable-extended-define-key t)
  )

(show-paren-mode)

(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package magit
  :straight t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status)
	 ("C-x g" . #'magit-status)
	 ("C-x M-g" . #'magit-dispatch-popup))
  :custom
  (magit-repository-directories '(("~/src" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package libgit
  :straight t
  )

(use-package magit-libgit
  :straight t
  :after (magit libgit)
  )

;;(global-set-key (kbd "C-x g") 'magit-status)
;;(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(use-package forge
  :straight t
  :after magit)

(use-package projectile
  :straight t
  :diminish
  :bind (("C-c k" . #'projectile-kill-buffers)
  ("C-c M" . #'projectile-compile-project))
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode)
  )

(use-package counsel-projectile
  :straight t
  :after (projectile counsel)
  :bind (("C-c f" . #'counsel-projectile)
         ("C-c F" . #'counsel-projectile-switch-project)))

(use-package deadgrep
  :straight t
  :bind (("C-c h" . #'deadgrep)))

(use-package visual-regexp
  :straight t
  :bind (("C-c 5" . #'vr/replace)))

(use-package company
  :straight t
  :diminish
  :bind (("C-." . #'company-complete))
  :hook (prog-mode . company-mode)
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers nil "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-tooltip-idle-delay 0.4 "Faster!")
  (company-async-timeout 20 "Some requests can take a long time. That's fine.")
  :config

  ;; Use the numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
   `(lambda () (interactive) (company-complete-number ,x))))
   (number-sequence 0 9))))

;; LSP
(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode t)
  (flycheck-set-indication-mode 'left-margin)
  :custom
  (flycheck-haskell-hlint-executable "~/.cabal/bin/hlint")
  (flycheck-display-errors-delay 0.1)
  )
(use-package yasnippet
  :straight t
  )

;; for lsp
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ; 1mb

(use-package lsp-mode
  :straight t
  :hook (haskell-mode . lsp)
  :commands (lsp lsp-execute-code-action)
  :custom
  (lsp-print-performance t)
;;  (lsp-log-io t)
  (lsp-file-watch-threshold 5000)
  (lsp-enable-file-watchers nil)
;;  (lsp-file-watch-ignored (append '("\\dist" "\\dist-newstyle") lsp-file-watch-ignored))
  )

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :init (setq lsp-ui-doc-enable t
         lsp-ui-doc-use-webkit nil
         lsp-ui-doc-header nil
         lsp-ui-doc-delay 0.2
         lsp-ui-doc-include-signature t
         lsp-ui-doc-alignment 'at-point
         lsp-ui-doc-use-childframe nil
         lsp-ui-doc-border (face-foreground 'default)
         lsp-ui-peek-enable t
         lsp-ui-peek-show-directory t
         lsp-ui-sideline-update-mode 'line
         lsp-ui-sideline-enable t
         lsp-ui-sideline-show-code-actions t
         lsp-ui-sideline-show-hover nil
         lsp-ui-sideline-ignore-duplicate t)
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
