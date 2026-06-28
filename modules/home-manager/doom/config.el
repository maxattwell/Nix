(setq user-full-name "Max Attwell"
      user-mail-address "max.attwell@hotmail.com")

(setq doom-font (font-spec :family "Mononoki Nerd Font Mono" :size 13 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font Mono" :size 14))

;; nerd-icons-font-family should be a string, not a font-spec
(setq nerd-icons-font-family "Mononoki Nerd Font Mono")

;; Auto-dark mode configuration
;; (use-package! auto-dark
;;   :defer t
;;   :init
;;   (setq auto-dark-allow-osascript t)
;;   (setq auto-dark-themes '((doom-gruvbox) (doom-gruvbox-light)))
;;   (setq! doom-theme nil)
;;   (setq! custom-safe-themes t)
;;   (defun my-auto-dark-init-h ()
;;     (auto-dark-mode)
;;     (remove-hook 'server-after-make-frame-hook #'my-auto-dark-init-h)
;;     (remove-hook 'after-init-hook #'my-auto-dark-init-h))
;;   (let ((hook (if (daemonp)
;;                   'server-after-make-frame-hook
;;                 'after-init-hook)))
;;     ;; Depth -95 puts this before doom-init-theme-h, which sounds like a good
;;     ;; idea, if only for performance reasons.
;;     (add-hook hook #'my-auto-dark-init-h -95)))

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

                                        ; ;; Dired nav shortcuts
                                        ; (evil-define-key 'normal dired-mode-map
                                        ;   (kbd "h") 'dired-up-directory
                                        ;   (kbd "l") 'dired-find-file
                                        ;   )

;; Set the flags for dired when calling the ls command
(setq dired-listing-switches "-ahlgo")

;; Ghostel terminal emulator
(use-package! ghostel
  :commands (ghostel ghostel-other ghostel-next ghostel-previous ghostel-list-buffers
                     ghostel-project ghostel-project-next ghostel-project-previous
                     ghostel-project-list-buffers)
  :init
  ;; Keep the native module outside straight's package tree so it survives
  ;; package rebuilds/syncs. First use will ask to download a prebuilt module.
  (setq ghostel-module-auto-install 'ask
        ghostel-module-directory (expand-file-name "ghostel/" doom-cache-dir)
        ghostel-shell "/bin/zsh")
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))

(use-package! evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

;; Let left/right side windows occupy the full frame height. This makes the pi
;; sidebar sit "over" bottom side windows instead of the bottom terminal spanning
;; underneath it.
(setq window-sides-vertical t)

(defun my/ghostel-project-pi ()
  "Open or create a project-scoped Ghostel terminal running pi."
  (interactive)
  (require 'ghostel)
  (let* ((default-directory (project-root (project-current t)))
         ;; This becomes a project-prefixed buffer name, e.g. a per-repo
         ;; "ghostel: pi" session, and `ghostel' will switch to it if it
         ;; already exists.
         (ghostel-buffer-name (project-prefixed-buffer-name "ghostel: pi"))
         (existing (ghostel--find-buffer-by-identity ghostel-buffer-name))
         (buffer (ghostel)))
    ;; Only start pi when the terminal was newly created; otherwise just switch.
    (unless existing
      (run-at-time
       0.2 nil
       (lambda (buf)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (ghostel-send-string "pi\n"))))
       buffer))
    buffer))

(defun my/ghostel-project-pi-sidebar-toggle ()
  "Toggle the current project's pi Ghostel terminal in a right sidebar."
  (interactive)
  (require 'ghostel)
  (let* ((origin-window (selected-window))
         (origin-buffer (current-buffer))
         (project-root (project-root (project-current t)))
         (ghostel-buffer-name (project-prefixed-buffer-name "ghostel: pi"))
         (existing (ghostel--find-buffer-by-identity ghostel-buffer-name))
         (existing-window (and existing (get-buffer-window existing))))
    (if existing-window
        (delete-window existing-window)
      (let ((buffer (or existing
                        (let ((default-directory project-root))
                          (my/ghostel-project-pi)))))
        ;; `my/ghostel-project-pi' may have switched the current window to the
        ;; terminal while creating it. Restore the editor window, then show the
        ;; terminal as a side window.
        (when (window-live-p origin-window)
          (set-window-buffer origin-window origin-buffer))
        (let ((side-window
               (display-buffer-in-side-window
                buffer
                '((side . right)
                  (slot . 0)
                  (window-width . 0.35)
                  (dedicated . t)
                  (window-parameters . ((no-delete-other-windows . t)
                                        (no-other-window . nil)))))))
          (set-window-dedicated-p side-window t)
          ;; Focus the sidebar after toggling it open so the cursor/input goes
          ;; straight to pi.  Keep it in the normal window cycle so Doom/Emacs
          ;; window commands can move focus to it later too.
          (select-window side-window))))))

(defun my/ghostel-project-terminal-bottom-toggle ()
  "Toggle the current project's Ghostel terminal at the bottom of the frame."
  (interactive)
  (require 'ghostel)
  (let* ((origin-window (selected-window))
         (origin-buffer (current-buffer))
         (project-root (project-root (project-current t)))
         ;; Use a dedicated per-project buffer so this behaves like the pi
         ;; sidebar: toggling reuses the same session instead of creating a new
         ;; one each time.
         (ghostel-buffer-name (project-prefixed-buffer-name "ghostel: terminal"))
         (existing (ghostel--find-buffer-by-identity ghostel-buffer-name))
         (existing-window (and existing (get-buffer-window existing))))
    (if existing-window
        (delete-window existing-window)
      (let ((buffer (or existing
                        (let ((default-directory project-root))
                          (ghostel)))))
        ;; `ghostel' may have switched the current window to the terminal while
        ;; creating it. Restore the editor window, then show the terminal as a
        ;; bottom side window.
        (when (window-live-p origin-window)
          (set-window-buffer origin-window origin-buffer))
        (let ((side-window
               (display-buffer-in-side-window
                buffer
                '((side . bottom)
                  (slot . 0)
                  (window-height . 0.30)
                  (dedicated . t)
                  (window-parameters . ((no-delete-other-windows . t)
                                        (no-other-window . nil)))))))
          (set-window-dedicated-p side-window t)
          ;; Focus the terminal after toggling it open so typing goes straight
          ;; into the shell, but keep it in the normal window cycle.
          (select-window side-window))))))

;; Ghostel workspace/session workflow
(map! :leader
      (:prefix ("a" . "Ghostel")
       ;; Global terminals
       :desc "New project terminal" "n" (cmd! (ghostel-project '(4)))
       :desc "New terminal" "N" (cmd! (ghostel '(4)))
       :desc "List all terminals" "a" #'ghostel-list-buffers
       :desc "Default terminal" "g" #'ghostel
       :desc "Other terminal" "o" #'ghostel-other
       :desc "Toggle pi sidebar" "s" #'my/ghostel-project-pi-sidebar-toggle
       :desc "Toggle bottom terminal" "t" #'my/ghostel-project-terminal-bottom-toggle
       :desc "Next terminal" "]" #'ghostel-next
       :desc "Previous terminal" "[" #'ghostel-previous

       ;; Project terminals
       (:prefix ("p" . "project")
        :desc "Project terminal" "p" #'ghostel-project
        :desc "New project terminal" "n" (cmd! (ghostel-project '(4)))
        :desc "List project terminals" "a" #'ghostel-project-list-buffers
        :desc "Next project terminal" "]" #'ghostel-project-next
        :desc "Previous project terminal" "[" #'ghostel-project-previous)

       ;; Current terminal actions / modes
       :desc "Copy mode" "v" #'ghostel-copy-mode
       :desc "Line mode" "l" #'ghostel-line-mode
       :desc "Char mode" "c" #'ghostel-char-mode
       :desc "Emacs mode" "e" #'ghostel-emacs-mode
       :desc "Clear screen" "x" #'ghostel-clear
       :desc "Clear scrollback" "X" #'ghostel-clear-scrollback
       :desc "Force redraw" "r" #'ghostel-force-redraw
       :desc "Kill current buffer" "k" #'kill-current-buffer))

;; Fast terminal/session picker. Overrides Doom's default SPC ' ivy/vertico resume.
(map! :leader
      :desc "List Ghostel terminals" "'" #'ghostel-list-buffers)

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
         :chat-model "gemini-2.5-flash-lite")))

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

;; Prefer minibuffer prompts over system dialog windows
(setq use-file-dialog nil)
(setq use-dialog-box nil)

(use-package! msgpack)
(use-package! tramp-rpc)

;; clutch - interactive database client
(use-package! clutch
  :commands (clutch-query-console
             clutch-query-sqlite-file
             clutch-switch-console
             clutch-execute
             clutch-dispatch)
  :mode ("\\.mysql\\'" . clutch-mode)
  :init
  ;; Conservative defaults; override per-connection with :connect-timeout,
  ;; :read-idle-timeout, :query-timeout, or :rpc-timeout as needed.
  (setq clutch-connect-timeout-seconds 10
        clutch-read-idle-timeout-seconds 30
        clutch-query-timeout-seconds 20
        clutch-jdbc-rpc-timeout-seconds 15
        clutch-connection-alist
        '(("sgs-dev-pg" . (:backend pg
                           :host "psql-sgs-dev.postgres.database.azure.com"
                           :port 5432
                           :user "dashboard_app"
                           :password "b083}cZUIvYbUHv:pz%<G%xU"
                           :database "system"
                           :sslmode require))))
  :config
  (map! :leader
        (:prefix ("d" . "database")
         :desc "Query console" "q" #'clutch-query-console
         :desc "SQLite file" "s" #'clutch-query-sqlite-file
         :desc "Switch console" "c" #'clutch-switch-console
         :desc "Clutch dispatch" "d" #'clutch-dispatch)))

(with-eval-after-load 'evil                                                                
  (require 'clutch-evil)                                                                   
  (clutch-evil-setup))                                                                     

(after! diff-hl
  (setq diff-hl-disable-on-remote t))

;; pi-coding-agent (Doom Emacs)
(use-package! pi-coding-agent
  :commands (pi-coding-agent pi)
  :init
  (defalias 'pi #'pi-coding-agent)
  :config
  ;; Optional defaults
  (setq pi-coding-agent-input-window-height 10
        pi-coding-agent-tool-preview-lines 10
        pi-coding-agent-bash-preview-lines 5
        pi-coding-agent-context-warning-threshold 70
        pi-coding-agent-context-error-threshold 90
        pi-coding-agent-visit-file-other-window t))

;; Agent Shell Sidebar - AI assistant sidebar
(use-package! agent-shell-sidebar
  :after agent-shell
  :vc (:url "https://github.com/cmacrae/agent-shell-sidebar")
  :custom
  (agent-shell-sidebar-width "25%")
  (agent-shell-sidebar-minimum-width 80)
  (agent-shell-sidebar-maximum-width "50%")
  (agent-shell-sidebar-position 'right)
  (agent-shell-sidebar-locked t)
  :config
  (setq agent-shell-opencode-default-model-id "openai/gpt-5.4/medium"))

;; Agent Shell Sidebar keybindings disabled for now; SPC a is used for Ghostel.
;; Re-enable this block if you bring agent-shell-sidebar back.
(when nil
  (map! :leader
        (:prefix ("a" . "AI/Agent")
         :desc "pi coding agent" "P" #'pi-coding-agent
         ;; Sidebar control
         :desc "Toggle sidebar" "s" #'agent-shell-sidebar-toggle
         :desc "Toggle focus" "f" #'agent-shell-sidebar-toggle-focus
         :desc "Reset sidebar" "r" #'agent-shell-sidebar-reset

         ;; Model & session management
         :desc "Set model" "m" #'agent-shell-set-session-model
         :desc "Cycle session mode" "t" #'agent-shell-cycle-session-mode
         :desc "Set session mode" "T" #'agent-shell-set-session-mode

         ;; Interaction control
         :desc "Interrupt" "i" #'agent-shell-interrupt
         :desc "Clear buffer" "k" #'agent-shell-clear-buffer

         ;; Send content
         :desc "Send current file" "." #'agent-shell-send-current-file
         :desc "Send file" "," #'agent-shell-send-file
         :desc "Send region" "v" #'agent-shell-send-region
         :desc "Send screenshot" "p" #'agent-shell-send-screenshot
         :desc "Send clipboard image" "y" #'agent-shell-send-clipboard-image

         ;; Shell management
         :desc "New shell" "n" #'agent-shell-new-shell
         :desc "Kill shell" "K" #'agent-shell-kill-buffer
         :desc "Rename shell" "R" #'agent-shell-rename-buffer
         :desc "Switch to other buffer" "o" #'agent-shell-other-buffer

         ;; Help
         :desc "Help menu" "h" #'agent-shell-help-menu)))

(after! lsp-mode
  (add-to-list 'lsp-disabled-clients 'vetur)
  (add-to-list 'lsp-disabled-clients 'vls)
  (add-to-list 'lsp-disabled-clients 'tailwindcss)
  (setq lsp-auto-guess-root t))

(after! lsp-volar
  (setq lsp-volar-location-for-typescript-plugin :auto
        lsp-volar-typescript-server-id 'ts-ls))

(after! lsp-clients
  (defun my/lsp-use-project-typescript-tsdk ()
    (when (and buffer-file-name (string-match-p "\\.vue\\'" buffer-file-name))
      (when-let* ((root (or (locate-dominating-file default-directory "package.json")
                            (locate-dominating-file default-directory "tsconfig.json")))
                  (tsdk (expand-file-name "node_modules/typescript/lib" root)))
        (when (file-directory-p tsdk)
          (setq-local lsp-clients-typescript-tsdk tsdk)))))
  (add-hook 'web-mode-hook #'my/lsp-use-project-typescript-tsdk))
