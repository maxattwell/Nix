(setq user-full-name "Max Attwell"
      user-mail-address "max.attwell@hotmail.com")

;; Beads issue tracker interface (https://github.com/r0man/beads.el).
;; Installed from our fork via packages.el (see the note there); invoke with
;; M-x beads.  The fork carries the agent-shell shell-buffer rename fix, so no
;; in-config workarounds are needed.
(use-package! beads
  :commands (beads beads-dashboard beads-dispatch beads-list beads-ready beads-show beads-create)
  :hook (after-init . beads-eldoc-mode)
  :init
  (defun my/insurgence-beads ()
    "Open my personal Insurgence Beads tracker."
    (interactive)
    (beads-dashboard :directory "/home/max/code/insurgence")))

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

;; Let left/right side windows occupy the full frame height.
(setq window-sides-vertical t)

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
  ;; :read-idle-timeout, :query-timeout, or :rpc-timeout as needed. Passwords
  ;; resolve through auth-source-pass (pass entries), e.g. "insurgence/sgs-local-pg".
  (require 'auth-source-pass)
  (auth-source-pass-enable)
  (setq clutch-connect-timeout-seconds 10
        clutch-read-idle-timeout-seconds 30
        clutch-query-timeout-seconds 20
        clutch-jdbc-rpc-timeout-seconds 15
        clutch-connection-alist
        '(("sgs-local-pg" . (:backend pg
                             :host "localhost"
                             :port 5432
                             :user "system"
                             :database "system"
                             :pass-entry "insurgence/sgs-local-pg"))))
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

;; agent-shell: SPC a
;; Default agent for every new shell is Oh My Pi (`omp'): an identifier here
;; makes agent-shell skip its picker (see `agent-shell--auto-preferred-config').
(setq agent-shell-preferred-agent-config 'omp)

;; Starting a shell otherwise auto-inserts the current file:line (the `line'
;; source) into the prompt; keep only explicit region/error context.
(setq agent-shell-context-sources '(region error))

(map! :leader
      (:prefix ("a" . "Agent shell")
       :desc "Agent shell" "a" #'agent-shell
       :desc "New shell" "n" #'agent-shell-new-shell
       :desc "Help menu" "?" #'agent-shell-help-menu
       :desc "Send region/error" "d" #'agent-shell-send-dwim
       :desc "HQ sidebar" "h" #'agent-shell-hq-toggle
       :desc "HQ peek" "p" #'agent-shell-hq-peek))

;; Agent Shell + Evil: RET sends in normal state, newlines in insert state,
;; and *agent-shell-diff* buffers open in Emacs state.
(after! (agent-shell evil)
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)
  (evil-define-key 'normal agent-shell-mode-map (kbd "?") #'agent-shell-help-menu)
  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                (evil-emacs-state)))))

;; LSP settings for Vue/TypeScript projects.
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

;; Agent Shell HQ - heads-up display and peek for agent-shell sessions
(use-package! agent-shell-hq
  :commands (agent-shell-hq-toggle agent-shell-hq-peek agent-shell-hq-label)
  :custom
  ;; Width of the sidebar listing agent-shell buffers (columns)
  (agent-shell-hq-toggle-sidebar-width 50)
  ;; Where the peek posframe is anchored: top, bottom, left, right
  (agent-shell-hq-peek-position 'right)
  ;; Width of the peek posframe (columns)
  (agent-shell-hq-peek-width 52)
  ;; Maximum height of the peek posframe (rows)
  (agent-shell-hq-peek-height 60)
  ;; Extra frame parameters for the peek posframe (nil: inherit theme colors)
  (agent-shell-hq-peek-parameters nil)
  ;; CLI command that receives the prompt as its final argument.
  ;; `claude' is not installed here; use omp's smol (fast/cheap) role with
  ;; Gemini flash-lite instead.  The appended system prompt keeps the model
  ;; from wrapping the title in preamble.
  (agent-shell-hq-label-command
   '("omp" "-p" "--no-session" "--smol" "google/gemini-3.5-flash-lite"
     "--append-system-prompt"
     "Output ONLY the title text. No preamble, no quotes, no explanation."))
  ;; Characters of buffer content (from the end) used as context for the title
  (agent-shell-hq-label-context-chars 2000)
  ;; Prompt template sent to the command (%s = buffer context)
  (agent-shell-hq-label-prompt
   "Reply with ONLY a terse 8-10 word title for this conversation, lowercase, no punctuation:\n\n%s")
  :bind
  ("C-c a h" . agent-shell-hq-toggle)
  ("C-c a p" . agent-shell-hq-peek))

;; agent-shell-hq ships no Evil integration: the sidebar and the peek posframe
;; are plain fundamental-mode read-only buffers, so Evil's normal/insert state
;; maps shadow the package's single-key map (q -> macro recording, n -> search
;; next, RET -> evil-ret).  Run them in Emacs state so the package's own map is
;; authoritative, re-asserting on every render/show.
;;
;; Defined at top level, not inside `after!': agent-shell-hq defers its own
;; submodules and its feature can load without this body ever running, so a
;; deferred definition silently never happens.  `advice-add' on a not-yet
;; defined function is fine; the advice is attached to the symbol and survives
;; the later `defun'.  The body itself is inert until the buffers exist.
(defun my/agent-shell-hq--emacs-state (&rest _)
  "Put the HQ sidebar and peek buffers in Emacs state.
Each keeps its own local keymap: the sidebar's `agent-shell-hq-toggle-map'
(which Evil's normal/insert state would otherwise shadow) and the peek
posframe's `agent-shell-hq-peek-map'.  `evil-emacs-state-map' is cleared
buffer-locally so it cannot outrank either one."
  (dolist (entry '((" *agent-shell-hq-sidebar*" . nil)
                   (" *agent-shell-hq-peek*" . agent-shell-hq-peek-map)))
    (when-let ((buf (get-buffer (car entry))))
      (with-current-buffer buf
        (evil-emacs-state)
        (setq-local evil-emacs-state-map (cdr entry))))))
(advice-add 'agent-shell-hq-toggle--render :after #'my/agent-shell-hq--emacs-state)
(advice-add 'agent-shell-hq-toggle :after #'my/agent-shell-hq--emacs-state)
(advice-add 'agent-shell-hq-peek :after #'my/agent-shell-hq--emacs-state)
