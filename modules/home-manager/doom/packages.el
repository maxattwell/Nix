;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

(package! evil-snipe :disable t)

(package! nvm)

(package! magit-delta)

(package! pdf-tools)

(package! auto-dark)

;; (package! glab)
;; (package! gtea)
;; (package! gogs)
;; (package! buck)

;; GPT-powered commit message generation
(package! llm)
(package! magit-gptcommit
  :recipe (:host github :repo "douo/magit-gptcommit"))

(package! msgpack)
(package! tramp-rpc
  :recipe (:host github :repo "ArthurHeymans/emacs-tramp-rpc"))

(package! shell-maker)
(package! acp)
(package! agent-shell)

;; Interactive database client.
;;
;; Point straight at our fork on the contribution branch. The straight
;; repo slot ~/.config/emacs/.local/straight/repos/clutch is a symlink to
;; ~/code/clutch, so straight builds directly from the local checkout.
;; Upstream is preserved at repos/clutch.upstream-bak. Rebuild after edits
;; with M-x straight-rebuild-package RET clutch.
(package! clutch
  :recipe (:host github :repo "maxattwell/clutch" :branch "evil-mode"))

;; Required by clutch's PostgreSQL backend (clutch-db-pg.el).
(package! pgsql)

;; Beads issue tracker interface (not on MELPA; install from source).
;; Source files live in the repo's lisp/ subdirectory, hence the :files entry.
;;
;; Point at our fork on the fix/agent-shell-renamed-buffer branch. Upstream
;; beads.el renames the agent-shell shell buffer with a raw `rename-buffer',
;; which breaks shell-maker's buffer lookup (agent start dies with
;; "wrong-type-argument shell-maker-config nil" / "processp nil"). The fork
;; renames via `shell-maker-set-buffer-name' and adds a compatibility shim.
;; The straight repo slot ~/.config/emacs/.local/straight/repos/beads.el is a
;; symlink to ~/code/beads.el, so straight builds the local checkout directly;
;; upstream is preserved at repos/beads.el.upstream-bak.
(package! beads
  :recipe (:host github :repo "maxattwell/beads.el"
           :branch "fix/agent-shell-renamed-buffer"
           :files ("lisp/*.el")))

;; beads.el dependency (on MELPA; straight pulls it transitively, listed
;; explicitly so a doom sync always installs it).
(package! sesman)

(package! agent-shell-hq
  :recipe (:host nil :repo "https://github.com/sreenivasvrao/agent-shell-hq"
           :files ("*.el")))
