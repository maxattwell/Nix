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

;; Set vterm shell
(setq vterm-shell "/bin/zsh")

;; Dont create new workspace when calling emacsclient
(after! persp-mode
  (setq! persp-emacsclient-init-frame-behaviour-override "main"))

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

;; LLM provider setup for Gemini
(use-package! llm
  :init
  (require 'llm-gemini)
  :config
  (setq llm-gemini-provider
        (make-llm-gemini
         :key (string-trim (shell-command-to-string "pass Google/gemini-api-key"))
         :chat-model "gemini-3-flash-preview")))

;; Magit GPT Commit - AI-powered commit message generation
(use-package! magit-gptcommit
  :after (magit llm)
  :demand t
  :custom
  (magit-gptcommit-llm-provider llm-gemini-provider)
  ;; Disable warning about non-free LLM services
  (llm-warn-on-nonfree nil)
  :config
  ;; Enable in magit status buffer
  (magit-gptcommit-status-buffer-setup))

(setq opencode-event-log-max-lines 200)

;; Bind SPC o i to run opencode
(map! :leader
      :desc "Run OpenCode" "o i" #'opencode)

;; Prefer minibuffer prompts over system dialog windows
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(use-package! msgpack)
(use-package! tramp-rpc)

(after! diff-hl
  (setq diff-hl-disable-on-remote t))
