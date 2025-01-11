;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Max Attwell"
      user-mail-address "max.attwell@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Mononoki Nerd Font Mono" :size 15 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Mononoki Nerd Font Mono" :size 16))

(setq nerd-icons-font-family (font-spec :family "Mononoki Nerd Font Mono" :size 22 :weight 'normal))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Remove evil-snipe
;; (remove-hook 'doom-first-input-hook #'evil-snipe-mode)

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

;; Set speechy database connections
(setq sql-connection-alist
      '((saa-live
         (sql-product 'mysql)
         (sql-server "us-cdbr-east-02.cleardb.com")
         (sql-user "bbed31e309cc99")
         (sql-database "heroku_bca5440be3c0983")
         (sql-port 3306))
        (saa-dev
         (sql-product 'mysql)
         (sql-server "127.0.0.1")
         (sql-user "root")
         (sql-password "password")
         (sql-database "SAA")
         (sql-port 3306))))

;; Org-mode SQL source blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

;; ;; Org mode theme customization
;; (defface org-custom-face
;;   '((t (:height 1.1 :family "Overpass Nerd Font")))
;;   "Custom face for Org Mode buffers"
;;   :group 'org-faces)

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (setq-local buffer-face-mode-face 'org-custom-face)
;;             (buffer-face-mode)))

;; (custom-set-faces
;;  '(org-level-1 ((t (:inherit outline-1 :height 1.6 :weight bold :family "Overpass Nerd Font"))))
;;  '(org-level-2 ((t (:inherit outline-2 :height 1.5 :weight bold :family "Overpass Nerd Font"))))
;;  '(org-level-3 ((t (:inherit outline-3 :height 1.4 :weight bold :family "Overpass Nerd Font"))))
;;  '(org-level-4 ((t (:inherit outline-4 :height 1.3 :weight bold :family "Overpass Nerd Font"))))
;;  '(org-level-5 ((t (:inherit outline-5 :height 1.2 :weight bold :family "Overpass Nerd Font"))))
;;  '(org-level-6 ((t (:inherit outline-6 :height 1.1 :weight bold :family "Overpass Nerd Font"))))
;;  '(org-default ((t (:height 1.0 :family "Overpass Nerd Font"))))
;;  '(org-block ((t (:height 1.0 :family "Mononoki Nerd Font Mono"))))
;;  '(org-table ((t (:height 1.0 :family "Mononoki Nerd Font Mono"))))
;;  ;; Add more levels as needed
;; )

;; Splash image
(setq fancy-splash-image "/Users/max/.config/doom/splash/doomEmacsGruvbox.svg")
(setq fancy-splash-image "/Users/max/.config/doom/splash/doomEmacsGruvbox.png")

;;
;; Set vterm shell
(setq vterm-shell "/bin/zsh")

;; Web lsp
;; (setq-hook! 'typescript-mode-hook +format-with-lsp nil)
;; (setq-hook! '+web-react-mode-hook +format-with-lsp nil)
;; (setq-hook! 'web-mode-hook +format-with-lsp nil)
(add-hook 'web-mode-hook #'lsp-deferred)

;; Vue
;; (add-hook 'vue-mode-hook #'lsp!)

;; Dont create new workspace when calling emacsclient
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

;; ;; Sync system clipboard with Emacs kill ring
;; (setq select-enable-clipboard t
;;       select-enable-primary t)

;; ;; Use system clipboard for pasting by default
;; (setq x-select-enable-clipboard-manager nil)

;; ;; Set up clipboard integration for Wayland
;; (when (eq window-system nil) ; running under Wayland (no X11)
;;   (setq interprogram-paste-function
;;         (lambda ()
;;           (shell-command-to-string "wl-paste --no-newline")))
;;   (setq interprogram-cut-function
;;         (lambda (text &optional _rest)
;;           (let ((process-connection-type nil))
;;             (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy")))
;;               (process-send-string proc text)
;;               (process-send-eof proc))))))
