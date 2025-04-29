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
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package! gptel)

(map! :leader
      (:prefix ("e" . "GPTel")
       :desc "Add region or buffer to GPTel's context" "a" #'gptel-add
       :desc "Send all text up to (point) or the selection." "<RET>" #'gptel-send
       :desc "Send buffer to GPTel" "f" #'gptel-add-file
       :desc "Open GPTel dedicated chat buffer" "e" #'gptel
       :desc "Remove all GPTel's context" "d" #'gptel-context-remove-all
       :desc "Rewrite, refactor or change the selected region" "r" #'gptel-rewrite
       :desc "Register MCP tool" "t" #'gptel-mcp-register-tool
       :desc "Use MCP tool" "T" #'gptel-mcp-use-tool
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

(gptel-make-ollama "Fedora"
  :host "192.168.31.76:11434"
  :stream t
  :models '(qwen2.5-coder:14b))

(gptel-make-gemini "Gemini"
  :key (get-api-key "Google/gemini-api-key")
  :stream t)

(gptel-make-anthropic "Claude"
  :stream t
  :key (get-api-key "Anthropic/api-key"))

(gptel-make-openai "OpenAI"
  :key (get-api-key "OpenAI/api-key")
  :stream t)

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(setq gptel-default-mode 'org-mode)

(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
(setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

(setq mcp-hub-servers
      '(
        ("File system" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/max/code")))
        ("Nixos server" . (:command "uvx" :args ("mcp-nixos")))
        ("Git" . (:command "uvx" :args ("mcp-server-git")))
        ("Sequential thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
        ("Playwright" . (:command "npx" :args ("-y" "@playwright/mcp@latest")))
        ("Playwright ExecuteAutomation" . (:command "npx" :args ("-y" "@executeautomation/playwright-mcp-server")))))

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
            tools)))

(add-hook 'after-init-hook
          (lambda ()
            (mcp-hub-start-all-server)
            (gptel-mcp-register-tool)
            (gptel-mcp-use-tool)))

;; tools
(gptel-make-tool
 :name "create_file"                    ; javascript-style  snake_case name
 :function (lambda (path filename content)   ; the function that runs
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :description "Create a new file with the specified content"
 :args (list '(:name "path"             ; a list of argument specifications
               :type string
               :description "The directory where to create the file")
             '(:name "filename"
               :type string
               :description "The name of the file to create")
             '(:name "content"
               :type string
               :description "The content to write to the file"))
 :category "filesystem")                ; An arbitrary label for grouping

(gptel-make-tool
 :name "read_file"
 :function (lambda (path)
             (with-temp-buffer
               (insert-file-contents path)
               (buffer-string)))
 :description "Read complete contents of a file"
 :args (list '(:name "path"
	       :type string
	       :description "The path to the file"))
 :category "filesystem")


;; Timing tools for GPTel
(defvar gptel-timer-start-time nil "Store the timer start time.")

(gptel-make-tool
 :name "start_timer"
 :function (lambda ()
             (setq gptel-timer-start-time (current-time))
             "Timer started")
 :description "Start a timer to measure elapsed time"
 :args nil
 :category "timing")

(gptel-make-tool
 :name "stop_timer"
 :function (lambda ()
             (if gptel-timer-start-time
                 (let ((elapsed (float-time (time-subtract (current-time) gptel-timer-start-time))))
                   (setq gptel-timer-start-time nil)
                   (format "Timer stopped. Elapsed time: %.4f seconds" elapsed))
               "No timer was running. Use start_timer first."))
 :description "Stop the timer and report elapsed time"
 :args nil
 :category "timing")

(gptel-make-tool
 :name "read_multiple_files"
 :function (lambda (paths)
             (mapconcat #'identity
                        (mapcar (lambda (path)
                                  (with-temp-buffer
                                    (insert-file-contents path)
                                    (buffer-string)))
                                paths)
                        "\n"))
 :description "Read multiple files simultaneously"
 :args (list '(:name "paths"
	       :type list
	       :description "A list of file paths"))
 :category "filesystem")

(gptel-make-tool
 :name "write_file"
 :function (lambda (path content)
             (with-temp-buffer
               (insert content)
               (write-region nil nil path)))
 :description "Create new file or overwrite existing"
 :args (list '(:name "path"
	       :type string
	       :description "File location")
             '(:name "content"
	       :type string
	       :description "File content"))
 :category "filesystem")

(gptel-make-tool
 :name "edit_file"
 :function (lambda (&optional path edits oldText newText dryRun)
             (with-temp-buffer
               (insert-file-contents path)
               (let ((inhibit-read-only t))
                 (dolist (edit edits)
                   (goto-char (point-min))
                   (while (search-forward oldText nil t)
                     (replace-match newText)))
                 (if dryRun
                     (diff-buffer-with-file path)
                   (write-region nil nil path))))
             (message "Edits applied."))
 :description "Make selective edits using advanced pattern matching and formatting"
 :args (list '(:name "path"
	       :type string
	       :description "File to edit")
             '(:name "edits"
	       :type list
	       :description "List of edit operations")
             '(:name "oldText"
	       :type string
	       :description "Text to search for")
             '(:name "newText"
	       :type string
	       :description "Text to replace with")
             '(:name "dryRun"
	       :type boolean
	       :description "Preview changes without applying (default: false)"))
 :category "filesystem")

(gptel-make-tool
 :name "create_directory"
 :function (lambda (path)
             (unless (file-exists-p path)
               (make-directory path t)))
 :description "Create new directory or ensure it exists"
 :args (list '(:name "path"
	       :type string
	       :description "The path to the directory"))
 :category "filesystem")

(gptel-make-tool
 :name "list_directory"
 :function (lambda (path)
             (let ((files (directory-files path t)))
               (mapconcat (lambda (file)
                            (if (file-directory-p file)
                                (format "[DIR] %s" file)
                              (format "[FILE] %s" file)))
                          files
                          "\n")))
 :description "List directory contents with [FILE] or [DIR] prefixes"
 :args (list '(:name "path"
	       :type string
	       :description "The path to the directory"))
 :category "filesystem")

(gptel-make-tool
 :name "move_file"
 :function (lambda (source destination)
             (when (file-exists-p source)
               (rename-file source destination t)))
 :description "Move or rename files and directories"
 :args (list '(:name "source"
	       :type string
	       :description "The path to the file or directory to move")
             '(:name "destination"
	       :type string
	       :description "The new path for the file or directory"))
 :category "filesystem")

(gptel-make-tool
 :name "search_files"
 :function (lambda (path pattern excludePatterns)
             (let ((default-directory path)
                   (result '()))
               (dolist (file (file-expand-wildcards pattern))
                 (unless (some (lambda (excludePattern)
                                 (string-match-p excludePattern file))
                               excludePatterns)
                   (push file result)))
               result))
 :description "Recursively search for files/directories"
 :args (list '(:name "path"
	       :type string
	       :description "The starting directory")
             '(:name "pattern"
	       :type string
	       :description "The search pattern")
             '(:name "excludePatterns"
	       :type list
	       :description "List of patterns to exclude"))
 :category "filesystem")
