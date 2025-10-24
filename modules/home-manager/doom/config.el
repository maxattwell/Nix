(setq user-full-name "Max Attwell"
      user-mail-address "max.attwell@hotmail.com")

(setq doom-font (font-spec :family "Mononoki Nerd Font Mono" :size 15 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font Mono" :size 16))

;; nerd-icons-font-family should be a string, not a font-spec
(setq nerd-icons-font-family "Mononoki Nerd Font Mono")

;; Auto-dark mode configuration
(use-package! auto-dark
  :defer t
  :init
  (setq auto-dark-allow-osascript t)
  (setq auto-dark-themes '((doom-gruvbox) (doom-gruvbox-light)))
  (setq! doom-theme nil)
  (setq! custom-safe-themes t)
  (defun my-auto-dark-init-h ()
    (auto-dark-mode)
    (remove-hook 'server-after-make-frame-hook #'my-auto-dark-init-h)
    (remove-hook 'after-init-hook #'my-auto-dark-init-h))
  (let ((hook (if (daemonp)
                  'server-after-make-frame-hook
                'after-init-hook)))
    ;; Depth -95 puts this before doom-init-theme-h, which sounds like a good
    ;; idea, if only for performance reasons.
    (add-hook hook #'my-auto-dark-init-h -95)))

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")

(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-unordered-key-sequence "jk"))

;; Set s, S as avy movements
(map!
 :n "s" nil
 :m "s" #'evil-avy-goto-char-2-below)
(map!
 :n "S" nil
 :m "S" #'evil-avy-goto-char-2-above)
;; Set easymotion for f,t,F,T
(map!
 :n "f" nil
 :m "f" #'evilem-motion-find-char)
(map!
 :n "F" nil
 :m "F" #'evilem-motion-find-char-backward)
(map!
 :n "t" nil
 :m "t" #'evilem-motion-find-char-to)
(map!
 :n "T" nil
 :m "T" #'evilem-motion-find-char-to-backward)

;; Dired nav shortcuts
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file
  )

;; Set the flags for dired when calling the ls command
(setq dired-listing-switches "-ahlgo")

;; Org-mode SQL source blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

;; Set vterm shell
(setq vterm-shell "/bin/zsh")

;; Web lsp
(add-hook 'web-mode-hook #'lsp-deferred)

;; Vue.js with Volar Language Server
(use-package! lsp-volar
  :after lsp-mode
  :config
  ;; Set to nil to disable take-over mode (if you want separate TS server)
  (setq lsp-volar-take-over-mode nil)

  ;; Customize completion settings
  (setq lsp-volar-completion-tag-casing 'both)
  (setq lsp-volar-completion-attr-casing 'kebabCase)

  ;; Filter out \u0000 warnings in completions (known issue fix)
  (defun my-lsp-volar-filter-null-chars (orig-fun &rest args)
    "Filter null characters from Volar responses."
    (let ((result (apply orig-fun args)))
      (when (and result (stringp result))
        (setq result (replace-regexp-in-string "\u0000" "" result)))
      result))
  (advice-add 'lsp--render-element :around #'my-lsp-volar-filter-null-chars))

;; Ensure web-mode uses LSP for .vue files
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "vue" (file-name-extension buffer-file-name))
              (lsp-deferred))))

;; Dont create new workspace when calling emacsclient
(after! persp-mode
  (setq! persp-emacsclient-init-frame-behaviour-override "main"))

(after! forge
  (setq forge-alist
        '(("github.com" "api.github.com" "github.com"
           forge-github-repository))))

(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package! gptel-magit
  :after (magit gptel)
  :hook (magit-mode . gptel-magit-install))

;; Org-LaTeX configuration
(after! org
  ;; Enable LaTeX preview in org-mode
  (setq org-startup-with-latex-preview t)

  ;; Custom LaTeX commands for quantum information
  (setq org-preview-latex-image-directory ".ltximg/")

  ;; Quantum information packages for LaTeX export
  (add-to-list 'org-latex-packages-alist '("" "amsmath" t))
  (add-to-list 'org-latex-packages-alist '("" "amssymb" t))
  (add-to-list 'org-latex-packages-alist '("" "physics" t))
  (add-to-list 'org-latex-packages-alist '("" "braket" t))

  ;; Better LaTeX preview scaling and options for Homebrew setup
  (setq org-format-latex-options
        (plist-put org-format-latex-options
                   :scale 1.5))

  ;; Add leader key shortcuts for LaTeX preview in org-mode
  (map! :leader
        (:prefix ("l" . "LaTeX")
         :map org-mode-map
         :desc "Toggle LaTeX preview" "p" #'org-latex-preview))

  ;; Fix LaTeX syntax highlighting colors
  (custom-set-faces!
    '(org-latex-and-related :inherit default :foreground nil :background nil)
    '(org-block :background nil)
    '(org-formula :inherit default :foreground nil :background nil)))

;; Code Review configuration
(use-package! code-review
  :after forge
  :config
  ;; Enable emoji support in code review buffers
  (add-hook 'code-review-mode-hook #'emojify-mode)

  ;; Configure line wrap for comments
  (setq code-review-fill-column 80)

  ;; Set download directory for binary files
  (setq code-review-download-dir "/tmp/code-review/")

  ;; Use forge authentication
  (setq code-review-auth-login-marker 'forge)

  ;; Doom Emacs workspace integration
  (add-hook 'code-review-mode-hook
            (lambda ()
              (when (bound-and-fun-p 'persp-add-buffer)
                (persp-add-buffer (current-buffer)))))

  ;; Keybindings
  (map! :leader
        (:prefix ("c" . "code")
         :desc "Start code review" "r" #'code-review-start
         :desc "Review PR at point" "v" #'code-review-forge-pr-at-point))

  ;; Add keybinding in forge topic mode
  (map! :map forge-topic-mode-map
        :n "C-c r" #'code-review-forge-pr-at-point)

  ;; Add magit menu integration
  (after! magit
    (transient-append-suffix 'magit-merge "i"
      '("y" "Review pull request" code-review-forge-pr-at-point))))

;; Agent Shell configuration for Claude AI
(use-package! shell-maker
  :defer t)

(use-package! acp
  :defer t)

(use-package! agent-shell
  :commands (agent-shell agent-shell-anthropic-start-claude-code agent-shell-toggle)
  :after (shell-maker acp)
  :init
  ;; Set agent-shell to use side window display action BEFORE loading
  (setq agent-shell-display-action
        '((display-buffer-in-side-window)
          (side . right)
          (window-width . 0.33)))
  :config
  ;; Close agent-shell window with 'q' in normal mode
  (map! :map agent-shell-mode-map
        :n "q" #'delete-window))

;; Agent Shell keybinding - just use built-in toggle
(map! :leader
      :desc "Toggle Agent Shell" "o i" #'agent-shell-toggle)

;; Terminal popup configuration (VSCode-style bottom terminal)
(add-to-list 'display-buffer-alist
             '("\\*terminal\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.3)))

(defun my/toggle-terminal ()
  "Toggle terminal at the bottom of the screen, VSCode-style."
  (interactive)
  (let* ((term-buffer (get-buffer "*terminal*"))
         (term-window (and term-buffer (get-buffer-window term-buffer))))
    (if term-window
        ;; Terminal is visible, hide it
        (delete-window term-window)
      ;; Terminal not visible, show or create it
      (if term-buffer
          ;; Buffer exists, just display it
          (select-window (display-buffer term-buffer))
        ;; Create new terminal
        (let ((default-directory (or (projectile-project-root) default-directory)))
          (term "/run/current-system/sw/bin/zsh")
          (select-window (get-buffer-window "*terminal*")))))))

;; Keybinding for terminal toggle (like Ctrl+` in VSCode)
(map! :leader
      :desc "Toggle terminal" "o t" #'my/toggle-terminal)

;; Close terminal window with 'q' in normal mode
(map! :map term-mode-map
      :n "q" #'delete-window)
