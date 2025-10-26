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

;; Shell toggle keybindings
(map! :leader
      (:prefix ("o" . "open")
       :desc "Toggle eshell" "e" #'+eshell/toggle))

;; Agent Shell configuration for Claude AI
(use-package! shell-maker
  :defer t)

(use-package! acp
  :defer t)

(use-package! agent-shell
  :commands (agent-shell agent-shell-anthropic-start-claude-code)
  :after (shell-maker acp)
  :config
  ;; Enable agent-shell-completion-mode by default
  (add-hook 'agent-shell-mode-hook #'agent-shell-completion-mode)

  ;; Use delta for syntax-highlighted diffs in agent-shell
  (setq agent-shell-diff-use-delta t)

  ;; Mode switching keybindings
  (map! :map agent-shell-mode-map
        :n "m" #'agent-shell-cycle-session-mode
        :n "M" #'agent-shell-set-session-mode))

(use-package! agent-shell-sidebar
  :after agent-shell
  :config
  ;; Sidebar appearance
  (setq agent-shell-sidebar-width "30%")
  (setq agent-shell-sidebar-minimum-width 60)
  (setq agent-shell-sidebar-position 'right)
  (setq agent-shell-sidebar-locked nil)

  ;; Set default agent provider
  (setq agent-shell-sidebar-default-config
        (agent-shell-anthropic-make-claude-code-config))

  ;; Close sidebar with 'q' in normal mode
  (map! :map agent-shell-mode-map
        :n "q" #'agent-shell-sidebar-toggle))

;; Bind SPC o i to toggle agent-shell sidebar (outside use-package! so it loads immediately)
(map! :leader
      :desc "Toggle Agent Sidebar" "o i" #'agent-shell-sidebar-toggle)

;; LLM provider setup for Gemini
(use-package! llm
  :init
  (require 'llm-gemini)
  :config
  (setq llm-gemini-provider
        (make-llm-gemini
         :key (string-trim (shell-command-to-string "pass Google/gemini-api-key"))
         :chat-model "gemini-2.0-flash-exp")))

;; Magit GPT Commit - AI-powered commit message generation
(use-package! magit-gptcommit
  :after (magit llm)
  :demand t
  :custom
  (magit-gptcommit-llm-provider llm-gemini-provider)
  :config
  ;; Enable in magit status buffer
  (magit-gptcommit-status-buffer-setup)

  ;; Keybinding in magit commit buffer
  (map! :map git-commit-mode-map
        :localleader
        :desc "Generate commit message with GPT" "g" #'magit-gptcommit-generate))
