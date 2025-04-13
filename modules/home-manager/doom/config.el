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

(defun get-gemini-api-key ()
  "Retrieve the Gemini API key from pass."
  (string-trim (shell-command-to-string "pass show Google/gemini-api-key")))

(setq
 gptel-model 'gemini-2.0-flash
 gptel-backend (gptel-make-gemini "Gemini"
                 :key (get-gemini-api-key)
                 :stream t))

(defun get-anthropic-api-key ()
  "Retrieve the Anthropic API key from pass."
  (string-trim (shell-command-to-string "pass show Anthropic/api-key")))

(gptel-make-anthropic "Claude"          ;Any name you want
  :stream t                             ;Streaming responses
  :key (get-anthropic-api-key))


(map! :leader
      (:prefix ("e" . "GPTel")
       :desc "Add region or buffer to GPTel's context" "a" #'gptel-add
       :desc "Send all text up to (point) or the selection." "<RET>" #'gptel-send
       :desc "Send buffer to GPTel" "f" #'gptel-add-file
       :desc "Open GPTel" "e" #'gptel
       :desc "Remove all GPTel's context" "d" #'gptel-context-remove-all
       :desc "Rewrite, refactor or change the selected region" "r" #'gptel-rewrite))

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(setq gptel-default-mode 'org-mode)


(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
(setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

;; (setenv "ANTHROPIC_API_KEY" (get-anthropic-api-key))
;; (use-package aider
;;   :config
;;   (setq aider-args '("--model" "sonnet")))
