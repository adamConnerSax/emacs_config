(setq lexical-binding t)
;; This is only needed once, near the top of the file
(require 'package)
(add-to-list 'package-archives
	     '(("gnu" . "https://elpa.gnu.org/packages/")
             ("melpa" . "https://melpa.org/packages/")
             ("marmalade" . "https://marmalade-repo.org/packages/")
             ("melpa-stable" . "https://stable.melpa.org/packages/")
             ("elpy" . "https://jorgenschaefer.github.io/packages/")))
(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; This will need updating if versions change.  How to avoid that?
  (add-to-list 'load-path "~/.emacs.d/use-package")
  (require 'use-package))


;; HELM
(use-package helm
  :ensure t)

;;(use-package helm-config
;;  :ensure t
;;  )


;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
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
(use-package
  :ensure t
  gnu-elpa-keyring-update)
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
  :ensure t
  :config
  (keychain-refresh-environment))

(use-package undo-tree
  :ensure t
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))

(ignore-errors (set-frame-font "Menlo-12"))

(use-package all-the-icons
  :ensure t
  )

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-themes
  :ensure t
  :config
  (let ((chosen-theme 'doom-one-light))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t)
    (load-theme chosen-theme)))

(use-package dimmer
  :ensure t
  :custom (dimmer-fraction 0.1)
  :config (dimmer-mode))

(show-paren-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/src" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package forge
  :ensure t
  :after magit)
;;

(helm-mode 1)
;; LSP
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)
(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands (lsp lsp-execute-code-action)
  :custom
  (lsp-print-performance t)
  (lsp-log-io t)
  (lsp-file-watch-threshold 5000)
  (lsp-enable-file-watchers nil)
  
;;  :config
;;  (setq lsp-prefer-flymake nil)
;;  (setq lsp-enable-file-watchers nil)
;;  (setq lsp-file-watch-ignored (append '("\\dist" "\\dist-newstyle") lsp-file-watch-ignored))
  )
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
;;  :config
;;  
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
 :ensure t
 :config
 ;; (setq lsp-haskell-process-path-hie "ghcide")
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;; (setq lsp-log-io t)
 )
;; (use-package haskell-mode

;;   :config
;;   ;; haskell-mode doesn't know about newer GHC features.
;;   (let ((new-extensions '("QuantifiedConstraints"
;;                           "DerivingVia"
;;                           "BlockArguments"
;;                           "DerivingStrategies"
;;                           "StandaloneKindSignatures"
;;                           )))
;;     (setq
;;      haskell-ghc-supported-extensions
;;      (append haskell-ghc-supported-extensions new-extensions)))

;;   :bind (("C-c a c" . haskell-cabal-visit-file)
;;          ("C-c a i" . haskell-navigate-imports)
;;          ("C-c a I" . haskell-navigate-imports-return)))

;; (use-package haskell-snippets
;;   :after (haskell-mode yasnippet)
;;   :defer)


;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)
;; This lets me say where my temp dir is.
(setq temporary-file-directory "~/.emacs.d/tmp/")

;; Stan
(use-package stan-mode
  :ensure t
  )
(use-package company-stan
  :ensure t
  :hook (stan-mode . company-stan-setup))


;; LSP/Hie
;;(require 'lsp)
;;(require 'lsp-mode)
;;(add-hook 'haskell-mode-hook #'lsp)

;;(require 'lsp-ui)
;;(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;;(require 'lsp-haskell)
;;(setq lsp-haskell-process-path-hie "/users/adam/.cabal/bin/hie-wrapper")


;;(let ((my-ghc-path (expand-file-name "/Applications/ghc-7.10.2.app/Contents/bin")))
;;  (setenv "PATH" (concat (getenv "PATH") path-separator my-ghc-path))
;;  (add-to-list 'exec-path my-ghc-path))

(add-to-list 'load-path ".")
;; Always load via this. If you contribute you should run `make all`
;; to regenerate this.
(load "haskell-mode-autoloads")

(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
;;(with-eval-after-load 'intero
;;    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))
;; Customization
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#f0f0f0" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#1b2229"])
 '(column-number-mode t)
 '(custom-enabled-themes '(tsdh-light))
 '(custom-safe-themes
   '("b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" default))
 '(dimmer-fraction 0.1)
 '(fci-rule-color "#383a42")
 '(flycheck-haskell-hlint-executable "~/.cabal/bin/hlint")
 '(flycheck-hlintrc nil)
 '(haskell-mode-contextual-import-completion nil)
 '(haskell-mode-stylish-haskell-path "brittany")
 '(haskell-notify-p t)
 '(haskell-process-args-stack-ghci '("--ghc-options=-ferror-spans -v"))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-load-or-reload-prompt t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(lsp-enable-file-watchers nil)
 '(lsp-file-watch-ignored
   '("[/\\\\]dist" "[/\\\\]dist-newstyle" "[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$"))
 '(lsp-file-watch-threshold 5000)
 '(lsp-log-io nil)
 '(lsp-print-performance t)
 '(magit-repository-directories '(("~/src" . 1)))
 '(package-selected-packages
   '(all-the-icons-dired undo-tree helm-config rainbow-delimiters all-the-icons dimmer doom-themes forge gnu-elpa-keyring-update keychain-environment flycheck-stan company-stan lsp-mode lsp-ui lsp-haskell magithub magit flycheck-pos-tip flycheck-hlint cl-lib shm helm haskell-mode flycheck-hdevtools flycheck-haskell flycheck-stack paredit ghci-completion ghc-imported-from flycheck company-ghc company))
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil))

(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'column-number-mode)
;;(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)
;;(add-hook 'haskell-mode-hook 'intero-mode)

;;(add-hook 'haskell-mode-hook 'structured-haskell-mode)
;;(set-face-background 'shm-current-face "#eee8d5")
;;(set-face-background 'shm-quarantine-face "lemonchiffon")

;; Haskell main editing mode key bindings.
(defun haskell-hook ()
  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
  (define-key haskell-mode-map [f5] 'haskell-process-load-file)

  ;; Switch to the REPL.
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  ;; Build the Cabal project.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  ;;(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))
  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1))))

;; Useful to have these keybindings for .cabal files, too.
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))


;; for ghcjs
(when nil
  (defun haskell-process-compile-ghcjs ()
    (interactive)
    (save-buffer)
    (haskell-process-file-loadish
     (format "!sh check.sh %s"
             (buffer-file-name))
     nil
     (current-buffer)))
  (define-key interactive-haskell-mode-map [f5] 'haskell-process-compile-ghcjs)
  (defun haskell-process-build-ghcjs ()
    (interactive)
    (save-buffer)
    (haskell-process-file-loadish
     (format "!ghcjs -O2 %s"
             (buffer-file-name))
     nil
     (current-buffer)))
  (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-compile-ghcjs))

(defvar haskell-stack-commands
  '("build"
    "update"
    "test"
    "bench"
    "install")
  "Stack commands.")

;;;###autoload
(defun haskell-process-stack-build ()
  "Build the Stack project."
  (interactive)
  (haskell-process-do-stack "build")
  (haskell-process-add-cabal-autogen))

;; ;;;###autoload
;; (defun haskell-process-stack (p)
;;   "Prompts for a Stack command to run."
;;   (interactive "P")
;;   (if p
;;       (haskell-process-do-stack
;;        (read-from-minibuffer "Stack command (e.g. install): "))
;;     (haskell-process-do-stack
;;      (funcall haskell-completing-read-function "Stack command: "
;;               (append haskell-stack-commands
;;                       (list "build --ghc-options=-fforce-recomp")
;;                       (list "build --ghc-options=-O0"))))))
;; (defun haskell-process-do-stack (command)
;;   "Run a Cabal command."
;;   (let ((process (haskell-interactive-process)))
;;     (cond
;;      ((let ((child (haskell-process-process process)))
;;         (not (equal 'run (process-status child))))
;;       (message "Process is not running, so running directly.")
;;       (shell-command (concat "stack " command)
;;                      (get-buffer-create "*haskell-process-log*")
;;                      (get-buffer-create "*haskell-process-log*"))
;;       (switch-to-buffer-other-window (get-buffer "*haskell-process-log*")))
;;      (t (haskell-process-queue-command
;;          process
;;          (make-haskell-command
;;           :state (list (haskell-interactive-session) process command 0)

;;           :go
;;           (lambda (state)
;;             (haskell-process-send-string
;;              (cadr state)
;;              (format ":!stack %s"
;;                      (cl-caddr state))))

;;           :live
;;           (lambda (state buffer)
;;             (let ((cmd (replace-regexp-in-string "^\\([a-z]+\\).*"
;;                                                  "\\1"
;;                                                  (cl-caddr state))))
;;               (cond ((or (string= cmd "build")
;;                          (string= cmd "install"))
;;                      (haskell-process-live-build (cadr state) buffer t))
;;                     (t
;;                      (haskell-process-cabal-live state buffer)))))

;;           :complete
;;           (lambda (state response)
;;             (let* ((process (cadr state))
;;                    (session (haskell-process-session process))
;;                    (message-count 0)
;;                    (cursor (haskell-process-response-cursor process)))
;;               (haskell-process-set-response-cursor process 0)
;;               (while (haskell-process-errors-warnings session process response)
;;                 (setq message-count (1+ message-count)))
;;               (haskell-process-set-response-cursor process cursor)
;;               (let ((msg (format "Complete: cabal %s (%s compiler messages)"
;;                                  (cl-caddr state)
;;                                  message-count)))
;;                 (haskell-interactive-mode-echo session msg)
;;                 (when (= message-count 0)
;;                   (haskell-interactive-mode-echo
;;                    session
;;                    "No compiler messages, dumping complete output:")
;;                   (haskell-interactive-mode-echo session response))
;;                 (haskell-mode-message-line msg)
;;                 (when (and haskell-notify-p
;;                            (fboundp 'notifications-notify))
;;                   (notifications-notify
;;                    :title (format "*%s*" (haskell-session-name (car state)))
;;                    :body msg
;;                    :app-name (cl-ecase (haskell-process-type)
;;                                ('ghci haskell-process-path-cabal)
;;                                ('cabal-repl haskell-process-path-cabal)
;;                                ('cabal-ghci haskell-process-path-cabal))
;;                    :app-icon haskell-process-logo)))))))))))

;; (defun haskell-setup-stack-commands ()
;;   "Setup stack keybindings."
;;   (interactive)
;;   (define-key interactive-haskell-mode-map (kbd "C-c C-c") 'haskell-process-stack-build)
;;   (define-key interactive-haskell-mode-map (kbd "C-c c") 'haskell-process-stack))



;; (defun stack-mode-save-flycheck ()
;;   "Save the buffer and flycheck it."
;;   (interactive)
;;   (save-buffer)
;;   (flycheck-buffer))

(setq flycheck-check-syntax-automatically nil)

;;(autoload 'ghc-init "ghc" nil t)
;;(autoload 'ghc-debug "ghc" nil t)
;;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
