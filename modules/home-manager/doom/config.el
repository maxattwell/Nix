(setq user-full-name "Max Attwell"
      user-mail-address "max.attwell@hotmail.com")

(setq doom-font (font-spec :family "Mononoki Nerd Font Mono" :size 15 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font Mono" :size 16))

(setq nerd-icons-font-family (font-spec :family "Mononoki Nerd Font Mono" :size 22 :weight 'normal))

(setq doom-theme 'doom-gruvbox-light)

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
;; (setq-hook! 'typescript-mode-hook +format-with-lsp nil)
;; (setq-hook! '+web-react-mode-hook +format-with-lsp nil)
;; (setq-hook! 'web-mode-hook +format-with-lsp nil)
(add-hook 'web-mode-hook #'lsp-deferred)

;; Dont create new workspace when calling emacsclient
(after! persp-mode
  (setq! persp-emacsclient-init-frame-behaviour-override "main"))

(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(after! mcp
  (setq! mcp-hub-servers
         '(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "~/")))
           ("git" . (:command "uvx" :args ("mcp-server-git")))
           ("postgresql" . (:command "npx" :args ("-y"  "@modelcontextprotocol/server-postgres" "postgresql://postgres:postgres@127.0.0.1:54322/postgres")))
           ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))))

  ;; A function for registering all MCP tools with gptel
  (defun gptel-mcp-register-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (apply #'gptel-make-tool
                         tool))
              tools)))

  ;; Activate all MCP tools using gptel
  (defun gptel-mcp-use-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (let ((path (list (plist-get tool :category)
                                    (plist-get tool :name))))
                    (push (gptel-get-tool path)
                          gptel-tools)))
              tools))))

(defvar my/init-mcp-tools nil "Function to initialize MCP tools")


(use-package! gptel)

(after! gptel
  (map! :leader
        (:prefix ("e" . "GPTel")
         :desc "Add region or buffer to GPTel's context" "a" #'gptel-add
         :desc "Send all text up to (point) or the selection." "<RET>" #'gptel-send
         :desc "Send buffer to GPTel" "f" #'gptel-add-file
         :desc "Open GPTel dedicated chat buffer" "e" #'gptel
         :desc "Remove all GPTel's context" "d" #'gptel-context-remove-all
         :desc "Initialize MCP tools" "i" #'my/init-mcp-tools
         :desc "Rewrite, refactor or change the selected region" "r" #'gptel-rewrite
         :desc "Open GPTel menu" "m" #'gptel-menu))

  (defun get-api-key (pass-path)
    "Retrieve the API key from pass using the given path."
    (string-trim (shell-command-to-string (concat "pass show " pass-path))))

  (setq
   gptel-use-tools t
   gptel-model 'qwen2.5-coder:14b
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(qwen2.5-coder:14b)))

  (gptel-make-ollama "Ollama"
    :host "192.168.31.76:11434"
    :stream t
    :models '(qwen2.5-coder:14b PetrosStav/gemma3-tools:12b deepseek-r1:14b gemma3:12b))

  (gptel-make-gemini "Gemini"
    :key (get-api-key "Google/gemini-api-key")
    :stream t)

  (gptel-make-anthropic "Claude"
    :stream t
    :key (get-api-key "Anthropic/api-key"))

  (gptel-make-openai "OpenAI"
    :key (get-api-key "OpenAI/api-key")
    :stream t)

  (add-hook 'gptel-post-stream-hook 'gptel-end-of-response)
  ;; (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  ;; (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  (setq gptel-default-mode 'org-mode)

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

  (defun my/init-mcp-tools ()
    "Initialize MCP servers and register tools with gptel."
    (interactive)
    (message "Starting MCP servers...")
    (mcp-hub-start-all-server)
    (message "Registering MCP tools...")
    (gptel-mcp-register-tool)
    (message "Activating MCP tools...")
    (gptel-mcp-use-tool)
    (message "MCP initialization complete!")))
