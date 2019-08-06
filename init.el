;;; init.el -- My Minimal Emacs config for Python, github and basics
;;;;;;;;;;;;;;;;;;  Ricardo Amaro 2018  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        PLACE: ~/emacsmini/.emacs.d/init.el                   ;;;
;;;        RUN:   `env HOME=~/emacsmini emacs`                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put file path on title bar
(setq frame-title-format '("%f [%m]  (" invocation-name "@" system-name ")"))
(require 'package)
;;; Code:
(defvar my-packages)
(setq my-packages
  '(;; generally useful packages
     ;; evil
     ;; evil-leader
     company-jedi company-anaconda company-irony company-tern
     company-rtags company-irony company-irony-c-headers
     company-php company-shell company-web
     cmake-ide
     lua-mode
     ;; auto-complete auto-complete-clang
     google-c-style
     rtags ggtags
     irony
     ;;helm helm-company helm-flyspell helm-rtags helm-gtags helm-projectile
     ;; projectile
     flycheck flycheck-pos-tip flycheck-irony flycheck-rtags
     paren symbol-overlay hl-line
     mode-icons all-the-icons-dired all-the-icons
     async
     bison-mode
     which-key
     popup
     markdown-mode
     web-mode
     json-mode
     ;; git/github
     quickrun
     dumb-jump
     ;; visual
     column-enforce-mode
     ;; org md
     markdown-mode
     ;; themes
     dracula-theme leuven-theme
     monokai-theme darkokai-theme
     ;;base16-theme
     yasnippet yasnippet-snippets auto-yasnippet)
  )
;; Add Melpa as the default Emacs Package repository
;; only contains a very limited number of packages

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(add-to-list 'package-archives
  '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
  '("org" . "https://orgmode.org/elpa/") t)

(defconst d/emacs-start-time (current-time))

;; Activate all the packages (in particular autoloads)
(package-initialize)
;;(package-refresh-contents)
;; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))
;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))
;; Install all missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; With use-package:
;;(use-package company-box
;;  :hook (company-mode . company-box-mode))
(load-theme 'monokai t)
;; To disable the menu bar
(menu-bar-mode -1)
;; on X disable scollbar and toobar
(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))


;; (setq company-backends '(( company-lsp company-yasnippet company-shell company-jedi company-capf enh-ruby-mode ruby-mode company-semantic company-files company-ac-php-backend company-elisp company-inf-ruby company-anaconda company-robe company-gtags company-rtags company-irony-c-headers company-web company-web-html company-web-jade company-web-slim company-go company-irony company-clang company-keywords company-cmake company-css
;;                            )
;;                           (company-dabbrev company-dabbrev-code))
;;   )

;;; Custom Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/lsp ()
  "Load lsp-mode."
  (interactive)
  (use-package lsp-mode
    :hook (prog-mode . lsp-deferred)
    :config
    ;; Disable readline based native completion
    (setq company-lsp-cache-candidates 'auto)
    :commands (lsp lsp-deferred))
  ;; optionally
  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package company-lsp :commands company-lsp)
  (use-package helm-lsp :commands helm-lsp-workspace-symbol)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
  ;; optionally if you want to use debugger
  (use-package dap-mode)
  ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
  (use-package lsp-ruby)
  (use-package lsp-python)
  (use-package lsp-sh)
  ;;(push 'company-lsp company-backends)
  )
(defun my/ruby ()
  "Load my Ruby configs."
  (interactive)
  (use-package ruby-mode
    :ensure nil
    :mode "\\.\\(rb\\|rake\\|\\gemspec\\|ru\\|\\(Rake\\|Gem\\|Guard\\|Cap\\|Vagrant\\)file\\)$"
    :interpreter "ruby"
    :config
    ;; Ruby refactoring helpers
    (use-package ruby-refactor
      :diminish ruby-refactor-mode
      :hook (ruby-mode . ruby-refactor-mode-launch))

    (use-package robe)
    ;;    (eval-after-load 'company
    ;;      '(push 'company-robe company-backends))

    (use-package rvm)
    (rvm-use-default) ;; use rvm's default ruby for the current Emacs session
    (use-package ruby-tools)
    (use-package rake)
    ;;(use-package enh-ruby-mode)
    ;;(use-package chruby)

    ;; Run a Ruby process in a buffer
    (use-package inf-ruby
      :hook ((ruby-mode . inf-ruby-minor-mode)
              (compilation-filter . inf-ruby-auto-enter)))

    ;; Rails
    (use-package projectile-rails
      :after projectile
      :diminish projectile-rails-mode
      :hook (projectile-mode . projectile-rails-global-mode))

    ;; Rubocop
    ;; Install: gem install rubocop
    (use-package rubocop
      :diminish rubocop-mode
      :hook (ruby-mode . rubocop-mode))

    ;; RSpec
    (use-package rspec-mode
      :diminish rspec-mode
      :commands rspec-install-snippets
      :hook (dired-mode . rspec-dired-mode)
      :config (with-eval-after-load 'yasnippet
                (rspec-install-snippets)))

    ;; Yet Another RI interface for Emacs
    (use-package yari
      :bind (:map ruby-mode-map ([f1] . yari)))

    ;; Ruby YARD comments
    (use-package yard-mode
      :diminish yard-mode
      :hook (ruby-mode . yard-mode)))

  ;; YAML mode
  (use-package yaml-mode)
  ;;  (push 'company-robe company-backends)
  )
(defun my/python ()
  ;; various settings for Jedi
  (interactive)

  (use-package python
    :ensure nil
    :defines gud-pdb-command-name pdb-path
    :config
    ;; Disable readline based native completion
    (setq python-shell-completion-native-enable nil)

    (add-hook 'inferior-python-mode-hook
      (lambda ()
        ;; (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
        (process-query-on-exit-flag (get-process "Python"))))

    ;; Live Coding in Python
    (use-package live-py-mode)

    ;; Format using YAPF
    ;; Install: pip install yapf
    (use-package yapfify
      :diminish yapf-mode
      :hook (python-mode . yapf-mode))

    ;; python related packages
    (use-package pytest)
    (use-package jedi-core)
    (use-package jedi)
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq
      jedi:complete-on-dot t
      jedi:setup-keys t
      py-electric-colon-active t
      py-smart-indentation t)
    )


  )
(defun my/sh ()
  "My SH packages and configs."
  (interactive)
  (use-package sh-script
    :ensure nil
    :hook (after-save . executable-make-buffer-file-executable-if-script-p))
  )
(defun my/cpp ()
  "Setup cmake-ide & rtags."
  (interactive)
  ;; Load rtags and start the cmake-ide-setup process
  (require 'rtags)
  (require 'cmake-ide)
  (cmake-ide-setup)
  ;; Set cmake-ide-flags-c++ to use C++11
  (setq cmake-ide-flags-c++ (append '("-std=c++11")))
  ;; We want to be able to compile with a keyboard shortcut
  (global-set-key (kbd "C-c m") 'cmake-ide-compile)
  ;; Set rtags to enable completions and use the standard keybindings.
  ;; A list of the keybindings can be found at:
  ;; http://syamajala.github.io/c-ide.html
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (rtags-enable-standard-keybindings)
  (yas-global-mode 1)

  (with-eval-after-load "projectile"
    (push '("cc" "h") projectile-other-file-alist)
    (push '("c" "h") projectile-other-file-alist)
    (push '("h" "cc" "c") projectile-other-file-alist))
  )
(defun my/fonts ()
  "Start my fonts."
  (interactive)
  (custom-set-faces
    ;; custom-set-faces was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(default ((t (:stipple nil
                    ;;:background "white" :foreground "black"
                    ;;:inverse-video nil :box nil :strike-through nil
                    ;;:overline nil :underline nil :slant normal
                    :weight normal
                    :height 110
                    :width normal
                    ;;:family "Inconsolata"
                    :family "DejaVu Sans Mono"
                    )))))

  ;; set a default font to Droid
  ;; (when (member "Droid Sans Mono" (font-family-list))
  ;;   (set-face-attribute 'default nil :font "Droid Sans Mono")
  ;;   (set-frame-font "Droid Sans Mono-11"))  ; set font for current window
  )
(defun my/ivy ()
  "Find files and other stuff."
  (interactive)
  (use-package swiper)
  (use-package ivy)
  (use-package counsel
    :diminish ivy-mode counsel-mode
    :defines (projectile-completion-system magit-completing-read-function recentf-list)
    :bind (("C-s" . swiper-isearch)
            ("s-f" . swiper)
            ("C-S-s" . swiper-all)

            ("C-c C-r" . ivy-resume)
            ("C-c v p" . ivy-push-view)
            ("C-c v o" . ivy-pop-view)
            ("C-c v ." . ivy-switch-view)

            :map counsel-mode-map
            ([remap swiper] . counsel-grep-or-swiper)
            ([remap dired] . counsel-dired)
            ("C-x C-r" . counsel-recentf)
            ("C-x j" . counsel-mark-ring)
            ("C-h F" . counsel-describe-face)

            ("C-c L" . counsel-load-library)
            ("C-c P" . counsel-package)
            ("C-c f" . counsel-find-library)
            ("C-c g" . counsel-grep)
            ("C-c h" . counsel-command-history)
            ("C-c i" . counsel-git)
            ("C-c j" . counsel-git-grep)
            ("C-c l" . counsel-locate)
            ("C-c r" . counsel-rg)
            ("C-c z" . counsel-fzf)

            ("C-c c F" . counsel-faces)
            ("C-c c L" . counsel-load-library)
            ("C-c c P" . counsel-package)
            ("C-c c a" . counsel-apropos)
            ("C-c c e" . counsel-colors-emacs)
            ("C-c c f" . counsel-find-library)
            ("C-c c g" . counsel-grep)
            ("C-c c h" . counsel-command-history)
            ("C-c c i" . counsel-git)
            ("C-c c j" . counsel-git-grep)
            ("C-c c l" . counsel-locate)
            ("C-c c m" . counsel-minibuffer-history)
            ("C-c c o" . counsel-outline)
            ("C-c c p" . counsel-pt)
            ("C-c c r" . counsel-rg)
            ("C-c c s" . counsel-ag)
            ("C-c c t" . counsel-load-theme)
            ("C-c c u" . counsel-unicode-char)
            ("C-c c w" . counsel-colors-web)
            ("C-c c z" . counsel-fzf)

            :map ivy-minibuffer-map
            ("C-w" . ivy-yank-word)

            :map counsel-find-file-map
            ("C-h" . counsel-up-directory)

            :map swiper-map
            ("M-s" . swiper-isearch-toggle)
            ("M-%" . swiper-query-replace)

            :map isearch-mode-map
            ("M-s" . swiper-isearch-toggle))
    :hook ((after-init . ivy-mode)
            (ivy-mode . counsel-mode))
    :init
    (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

    (setq ivy-use-selectable-prompt t
      ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
      ivy-height 10
      ivy-count-format "(%d/%d) "
      ivy-on-del-error-function nil
      ivy-initial-inputs-alist nil)

    (setq swiper-action-recenter t)

    (setq counsel-find-file-at-point t
      counsel-yank-pop-separator "\n────────\n")
    :config
    (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))

    ;; Use faster search tools: ripgrep or the silver search
    (let ((cmd (cond ((executable-find "rg")
                       "rg -S --no-heading --line-number --color never '%s' %s")
                 ((executable-find "ag")
                   "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
                 (t counsel-grep-base-command))))
      (setq counsel-grep-base-command cmd))

    ;; Build abbreviated recent file list.
    (defun my-counsel-recentf ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "Recentf: " (mapcar #'abbreviate-file-name recentf-list)
        :action (lambda (f)
                  (with-ivy-window
                    (find-file f)))
        :require-match t
        :caller 'counsel-recentf))
    (advice-add #'counsel-recentf :override #'my-counsel-recentf)

    ;; Pre-fill search keywords
    ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
    (defvar my-ivy-fly-commands
      '(query-replace-regexp
         flush-lines
         keep-lines
         ivy-read
         swiper
         swiper-all
         swiper-isearch
         counsel-grep-or-swiper
         counsel-grep
         counsel-ack
         counsel-ag
         counsel-rg
         counsel-pt))

    (defun my-ivy-fly-back-to-present ()
      ;; (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)
      (cond ((and (memq last-command my-ivy-fly-commands)
               (equal (this-command-keys-vector) (kbd "M-p")))
              ;; repeat one time to get straight to the first history item
              (setq unread-command-events
                (append unread-command-events
                  (listify-key-sequence (kbd "M-p")))))
        ((or (memq this-command '(self-insert-command
                                   yank
                                   ivy-yank-word
                                   counsel-yank-pop))
           (equal (this-command-keys-vector) (kbd "M-n")))
          (delete-region (point)
            (point-max)))))

    (defun my-ivy-fly-time-travel ()
      (when (memq this-command my-ivy-fly-commands)
        (let* ((kbd (kbd "M-n"))
                (cmd (key-binding kbd))
                (future (and cmd
                          (with-temp-buffer
                            (when (ignore-errors
                                    (call-interactively cmd) t)
                              (buffer-string))))))
          (when future
            (save-excursion
              (insert (propertize (replace-regexp-in-string
                                    "\\\\_<" ""
                                    (replace-regexp-in-string
                                      "\\\\_>" ""
                                      future))
                        'face 'shadow)))
            (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

    (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)

    ;; Improve search experience of `swiper'
    ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
    (defun my-swiper-toggle-counsel-rg ()
      "Toggle `counsel-rg' with current swiper input."
      (interactive)
      (let ((text (replace-regexp-in-string
                    "\n" ""
                    (replace-regexp-in-string
                      "\\\\_<" ""
                      (replace-regexp-in-string
                        "\\\\_>" ""
                        (replace-regexp-in-string "^.*Swiper: " ""
                          (thing-at-point 'line t)))))))
        (ivy-quit-and-run
          (counsel-rg text default-directory))))
    (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)

    (with-eval-after-load 'rg
      (defun my-swiper-toggle-rg-dwim ()
        "Toggle `rg-dwim' with current swiper input."
        (interactive)
        (ivy-quit-and-run (rg-dwim default-directory)))
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
      (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim ivy-minibuffer-map))

    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))

    ;; Integration with `magit'
    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read))

    ;; Enhance M-x
    (use-package amx
      :init (setq amx-history-length 20))

    ;; Enhance fuzzy matching
    (use-package flx
      :config (setq ivy-re-builders-alist
                '((swiper . ivy--regex-plus)
                   (swiper-all . ivy--regex-plus)
                   (swiper-isearch . ivy--regex-plus)
                   (counsel-ag . ivy--regex-plus)
                   (counsel-rg . ivy--regex-plus)
                   (counsel-pt . ivy--regex-plus)
                   (counsel-ack . ivy--regex-plus)
                   (counsel-grep . ivy--regex-plus)
                   (t . ivy--regex-fuzzy))))

    ;; Additional key bindings for Ivy
    (use-package ivy-hydra
      :bind (:map ivy-minibuffer-map
              ("M-o" . ivy-dispatching-done-hydra)))

    ;; Ivy integration for Projectile
    (use-package counsel-projectile
      :init
      (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
      (counsel-projectile-mode 1))

    ;; Integrate yasnippet
    (use-package ivy-yasnippet
      :commands ivy-yasnippet--preview
      :bind ("C-c C-y" . ivy-yasnippet)
      :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

    ;; Select from xref candidates with Ivy
    (use-package ivy-xref
      :init
      (when (boundp 'xref-show-definitions-function)
        (setq xref-show-definitions-function #'ivy-xref-show-defs))
      (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

    ;; Correcting words with flyspell via Ivy
    (use-package flyspell-correct-ivy
      :after flyspell
      :bind (:map flyspell-mode-map
              ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic)))

    ;; Quick launch apps
    (bind-key "s-<f6>" #'counsel-linux-app counsel-mode-map))

  ;; Display world clock using Ivy
  (use-package counsel-world-clock
    :bind (:map counsel-mode-map
            ("C-c c k" . counsel-world-clock)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (:map counsel-mode-map
            ("C-c c v" . counsel-tramp)))

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :defines (all-the-icons-icon-alist
               all-the-icons-dir-icon-alist
               bookmark-alist)
    :functions (all-the-icons-icon-for-file
                 all-the-icons-icon-for-mode
                 all-the-icons-icon-family
                 all-the-icons-match-to-alist
                 all-the-icons-faicon
                 all-the-icons-octicon
                 all-the-icons-dir-is-submodule)
    :preface
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((buffer (get-buffer candidate))
                (buffer-file-name (buffer-file-name buffer))
                (major-mode (buffer-local-value 'major-mode buffer))
                (icon (if (and buffer-file-name
                            (all-the-icons-auto-mode-match?))
                        (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                        (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
          (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((path (file-local-name (concat ivy--directory candidate)))
                (file (file-name-nondirectory path))
                (icon (cond
                        ((file-directory-p path)
                          (cond
                            ((and (fboundp 'tramp-tramp-file-p)
                               (tramp-tramp-file-p default-directory))
                              (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                            ((file-symlink-p path)
                              (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                            ((all-the-icons-dir-is-submodule path)
                              (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                            ((file-exists-p (format "%s/.git" path))
                              (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                            (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                                 (apply (car matcher) (list (cadr matcher) :v-adjust 0.01))))))
                        ((string-match "^/.*:$" path)
                          (all-the-icons-material "settings_remote" :height 1.0 :v-adjust -0.2))
                        ((not (string-empty-p file))
                          (all-the-icons-icon-for-file file :v-adjust -0.05)))))
          (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-dir-icon (candidate)
      "Display directory icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

    (defun ivy-rich-function-icon (_candidate)
      "Display function icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

    (defun ivy-rich-variable-icon (_candidate)
      "Display variable icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "tag" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-symbol-icon (_candidate)
      "Display symbol icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

    (defun ivy-rich-theme-icon (_candidate)
      "Display theme icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

    (defun ivy-rich-keybinding-icon (_candidate)
      "Display keybindings icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

    (defun ivy-rich-library-icon (_candidate)
      "Display library icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

    (defun ivy-rich-package-icon (_candidate)
      "Display package icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "archive" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-silver)))

    (when (display-graphic-p)
      (defun ivy-rich-bookmark-type-plus (candidate)
        (let ((filename (file-local-name (ivy-rich-bookmark-filename candidate))))
          (cond ((null filename)
                  (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))  ; fixed #38
            ((file-remote-p filename)
              (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
            ((not (file-exists-p filename))
              (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
            ((file-directory-p filename)
              (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
            (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
      (advice-add #'ivy-rich-bookmark-type :override #'ivy-rich-bookmark-type-plus))
    :hook ((ivy-mode . ivy-rich-mode)
            (ivy-rich-mode . (lambda ()
                               (setq ivy-virtual-abbreviate
                                 (or (and ivy-rich-mode 'abbreviate) 'name)))))
    :init
    ;; For better performance
    (setq ivy-rich-parse-remote-buffer nil)

    ;; Setting tab size to 1, to insert tabs as delimiters
    (add-hook 'minibuffer-setup-hook
      (lambda ()
        (setq tab-width 1)))

    (setq ivy-rich-display-transformers-list
      '(ivy-switch-buffer
         (:columns
           ((ivy-rich-buffer-icon)
             (ivy-rich-candidate (:width 30))
             (ivy-rich-switch-buffer-size (:width 7))
             (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
             (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
             (ivy-rich-switch-buffer-project (:width 15 :face success))
             (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
         ivy-switch-buffer-other-window
         (:columns
           ((ivy-rich-buffer-icon)
             (ivy-rich-candidate (:width 30))
             (ivy-rich-switch-buffer-size (:width 7))
             (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
             (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
             (ivy-rich-switch-buffer-project (:width 15 :face success))
             (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
         counsel-switch-buffer
         (:columns
           ((ivy-rich-buffer-icon)
             (ivy-rich-candidate (:width 30))
             (ivy-rich-switch-buffer-size (:width 7))
             (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
             (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
             (ivy-rich-switch-buffer-project (:width 15 :face success))
             (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
         counsel-switch-buffer-other-window
         (:columns
           ((ivy-rich-buffer-icon)
             (ivy-rich-candidate (:width 30))
             (ivy-rich-switch-buffer-size (:width 7))
             (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
             (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
             (ivy-rich-switch-buffer-project (:width 15 :face success))
             (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
         persp-switch-to-buffer
         (:columns
           ((ivy-rich-buffer-icon)
             (ivy-rich-candidate (:width 30))
             (ivy-rich-switch-buffer-size (:width 7))
             (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
             (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
             (ivy-rich-switch-buffer-project (:width 15 :face success))
             (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
         counsel-M-x
         (:columns
           ((ivy-rich-function-icon)
             (counsel-M-x-transformer (:width 50))
             (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
         counsel-describe-function
         (:columns
           ((ivy-rich-function-icon)
             (counsel-describe-function-transformer (:width 50))
             (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
         counsel-describe-variable
         (:columns
           ((ivy-rich-variable-icon)
             (counsel-describe-variable-transformer (:width 50))
             (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
         counsel-apropos
         (:columns
           ((ivy-rich-symbol-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-info-lookup-symbol
         (:columns
           ((ivy-rich-symbol-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-descbinds
         (:columns
           ((ivy-rich-keybinding-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-find-file
         (:columns
           ((ivy-rich-file-icon)
             (ivy-read-file-transformer))
           :delimiter "\t")
         counsel-file-jump
         (:columns
           ((ivy-rich-file-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-dired
         (:columns
           ((ivy-rich-file-icon)
             (ivy-read-file-transformer))
           :delimiter "\t")
         counsel-dired-jump
         (:columns
           ((ivy-rich-file-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-fzf
         (:columns
           ((ivy-rich-file-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-git
         (:columns
           ((ivy-rich-file-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-recentf
         (:columns
           ((ivy-rich-file-icon)
             (ivy-rich-candidate (:width 0.8))
             (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
           :delimiter "\t")
         counsel-bookmark
         (:columns
           ((ivy-rich-bookmark-type)
             (ivy-rich-bookmark-name (:width 40))
             (ivy-rich-bookmark-info))
           :delimiter "\t")
         counsel-package
         (:columns
           ((ivy-rich-package-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-find-library
         (:columns
           ((ivy-rich-library-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-load-library
         (:columns
           ((ivy-rich-library-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-load-theme
         (:columns
           ((ivy-rich-theme-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-projectile-switch-project
         (:columns
           ((ivy-rich-file-icon)
             (ivy-rich-candidate))
           :delimiter "\t")
         counsel-projectile-find-file
         (:columns
           ((ivy-rich-file-icon)
             (counsel-projectile-find-file-transformer))
           :delimiter "\t")
         counsel-projectile-find-dir
         (:columns
           ((ivy-rich-dir-icon)
             (counsel-projectile-find-dir-transformer))
           :delimiter "\t")
         treemacs-projectile
         (:columns
           ((ivy-rich-file-icon)
             (ivy-rich-candidate))
           :delimiter "\t")))
    )
  )
(defun my/helm()
  "Helm configs."
  (interactive)
  (autoload 'helm-company "helm-company") ;; Enable helm company
  ;; use helm for auto-complete commands
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-unset-key (kbd "M-m"))
  (global-set-key (kbd "M-m s s") #'helm-swoop)
  (global-set-key (kbd "M-m b b") #'helm-mini)
  (global-set-key (kbd "M-m p h") #'helm-projectile)
  (global-set-key (kbd "M-m p f") #'helm-projectile-find-file)
  (global-set-key (kbd "M-m p p") #'helm-projectile-switch-project)

  (global-set-key (kbd "M-m g s") #'magit-status)

  (helm-mode 1)
  )
(defun my/tabs()
  "Use centaur-tabs, https://github.com/ema2159/centaur-tabs ."
  (interactive)
  (use-package centaur-tabs
    :demand
    :config
    (centaur-tabs-mode t)
    (setq centaur-tabs-style "bar"
      centaur-tabs-height 24
      centaur-tabs-set-icons t
      ;;centaur-tabs-set-bar t
      centaur-tabs-close-button "⚫"
      centaur-tabs-set-modified-marker t
      centaur-tabs-modified-marker "⭕" ;;"⚠️"
      )
    :hook
    (dired-mode . centaur-tabs-local-mode)
    :bind
    ("C-<prior>" . centaur-tabs-backward)
    ("C-<next>" . centaur-tabs-forward))
  (centaur-tabs-group-by-projectile-project)
  )
(defun my/keys ()
  "Ricardo custom keys."
  (interactive)
  (cua-mode t)
  ;; == https://www.emacswiki.org/emacs/CuaMode (Ctrl+c, Ctrl+v...)
  ;; Unbind some keys
  ;; (define-key undo-tree-map (kbd "C-/") nil)

  (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
  (transient-mark-mode 1) ;; No region when it is not highlighted
  (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

  ;; hideshow folds of code keys
  (defun toggle-fold() (interactive)
    (save-excursion (end-of-line) (hs-toggle-hiding)))
  (global-set-key (kbd "<C-tab>")
    (lambda ()(interactive) (hs-minor-mode 1) (toggle-fold)))
  (global-set-key (kbd "<C-dead-grave>")
    (lambda ()(interactive) (hs-minor-mode 1) (hs-hide-all)))
  (global-set-key (kbd "<C-dead-acute>")
    (lambda ()(interactive) (hs-minor-mode 1) (hs-show-all)))
  ;; auto-complete
  (global-set-key (kbd "C-."  ) 'company-complete)
  (global-set-key (kbd "C-,"  ) 'company-yasnippet)

  ;; Setting SHIFT+arrow keys to select text
  (global-unset-key (vector (list 'shift 'left)))
  (global-unset-key (vector (list 'shift 'right)))
  (global-unset-key (vector (list 'shift 'up)))
  (global-unset-key (vector (list 'shift 'down)))
  (setq shift-selection-mode t)

  ;; (global-set-key (kbd "C-n") 'new-empty-buffer) ;; New. was next-line
  ;; (global-set-key (kbd "C-S-n") 'make-frame-command) ;; New Window. was nil
  ;; (global-set-key (kbd "C-o") 'ido-find-file) ;; Open. was open-line
  ;; (global-set-key (kbd "C-/")
  ;;                 'comment-or-uncomment-region-or-line) ;;comment/uncomment.was undo-tree-undo
  ;; (global-set-key (kbd "C-t") 'hs-toggle-hiding) ;; toggle hide/show block
  ;; (global-set-key (kbd "C-b") 'compile) ;; Build/Make Project using make -k
  ;; (global-set-key (kbd "C-;") 'iedit-mode) ;; Enter/Leave iedit mode
  ;; (global-set-key [f8] 'neotree-toggle)
  ;; ;; testing
  ;; ;;(global-set-key (kbd "TAB") 'company-complete)
  ;; ;;(setq tab-always-indent 'complete)
  ;; (setq neo-theme 'nerd)
  ;; (global-set-key [f10] 'save-buffers-kill-terminal)
  ;; (global-set-key (kbd "C-v") 'yank)  ;; Paste. was scroll-up-command
  ;; (global-set-key (kbd "C-w") 'kill-buffer)   ;; Close. was kill-region
  ;; (global-set-key (kbd "C-y") 'redo)  ;; Redo. was yank
  ;; (global-set-key (kbd "C-z") 'undo)  ;; Undo. was suspend-frame
  ;; (define-key evil-insert-state-map (kbd "C-c") 'cua-copy-region)
  ;; (define-key evil-insert-state-map (kbd "C-v") 'cua-paste)
  ;; (define-key evil-insert-state-map (kbd "C-x") 'cua-cut-region)
  ;; (define-key evil-insert-state-map (kbd "C-z") 'undo-tree-undo)
  ;; (define-key evil-insert-state-map (kbd "C-y") 'undo-treeredo)
  )
(defun my/cursor()
  "Create a nice blinking bar cursor"
  (interactive)
  (setq cursor-type '(bar . 3))
  (setq evil-insert-state-cursor '(bar . 3))
  (setq evil-emacs-state-cursor '(bar . 3))
  (custom-set-variables
    '(blink-cursor-mode t))
  (tooltip-mode t)
  ;; Mouse/scroll related
  (setq scroll-margin 1) ; Point can come within 1 line of top or bottom of window
  (setq scroll-step 1) ; Scroll by 1 line at a time
  (setq scroll-conservatively 10000) ; automatic scrolling never centers point
  (setq auto-window-vscroll nil) ; automatically modify the vertical scroll
                                        ; position to scroll through display rows
                                        ; that are taller than the height of the window
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; scroll one line at a time
  )
(defun my/company()
  (interactive)
  ;; company is the completion backend
  (use-package company
    :diminish company-mode
    :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
    :commands company-abort
    :bind (("M-/" . company-complete)
            ("<backtab>" . company-yasnippet)
            :map company-active-map
            ("C-p" . company-select-previous)
            ("C-n" . company-select-next)
            ("<tab>" . company-complete-common-or-cycle)
            ("<backtab>" . my-company-yasnippet)
            ;; ("C-c C-y" . my-company-yasnippet)
            :map company-search-map
            ("C-p" . company-select-previous)
            ("C-n" . company-select-next))
    :hook (after-init . global-company-mode)
    :init
    (defun my-company-yasnippet ()
      (interactive)
      (company-abort)
      (call-interactively 'company-yasnippet))
    :config
    (setq company-tooltip-align-annotations t
      company-tooltip-limit 12
      company-idle-delay .1
      company-echo-delay (if (display-graphic-p) nil 0)
      company-minimum-prefix-length 2
      company-require-match nil
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil)

    ;; Better sorting and filtering
    (use-package company-prescient
      :init (company-prescient-mode 1))

    ;; Icons and quickhelp
    (unless (version< emacs-version "26.0")
      ;;(message "emacs is before 26.0")
      (use-package company-box
        :diminish
        :functions (my-company-box--make-line my-company-box-icons--elisp)
        :hook (company-mode . company-box-mode)
        :config
        (setq company-box-backends-colors nil
          company-box-show-single-candidate t
          company-box-max-candidates 50
          company-box-doc-delay 0.5
          company-box-icons-alist 'company-box-icons-all-the-icons)
        ;; Support `company-common'
        (defun my-company-box--make-line (candidate)
          (-let* (((candidate annotation len-c len-a backend) candidate)
                   (color (company-box--get-color backend))
                   ((c-color a-color i-color s-color) (company-box--resolve-colors color))
                   (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
                   (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                       (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
                   (align-string (when annotation
                                   (concat " " (and company-tooltip-align-annotations
                                                 (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
                   (space company-box--space)
                   (icon-p company-box-enable-icon)
                   (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
                   (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                                   (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                           (company-box--apply-color icon-string i-color)
                           (company-box--apply-color candidate-string c-color)
                           align-string
                           (company-box--apply-color annotation-string a-color)))
                   (len (length line)))
            (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                         'company-box--color s-color)
              line)
            line))
        (advice-add #'company-box--make-line :override #'my-company-box--make-line)

        ;; Prettify icons
        (defun my-company-box-icons--elisp (candidate)
          (when (derived-mode-p 'emacs-lisp-mode)
            (let ((sym (intern candidate)))
              (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
        (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

        (with-eval-after-load 'all-the-icons
          (declare-function all-the-icons-faicon 'all-the-icons)
          (declare-function all-the-icons-material 'all-the-icons)
          (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
               (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
               (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
               (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
               (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
               (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
               (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
               (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
               (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
               (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
               (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
               (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
               (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
               (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
               (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
               (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
               (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
               (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
               (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
               (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
               (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
               (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
               (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
               (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
               (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
               (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
               (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2)))))))

    ;; Popup documentation for completion candidates
    ;;(when (and (not emacs/>=26p) (display-graphic-p))
    (if (and (version< emacs-version "26.0") (display-graphic-p))
      (use-package company-quickhelp
        :defines company-quickhelp-delay
        :bind (:map company-active-map
                ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
        :hook (global-company-mode . company-quickhelp-mode)
        :init (setq company-quickhelp-delay 0.5))))

  (global-company-mode t)
  (global-auto-complete-mode t)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.8)
  (setq company-minimum-prefix-length 2)

  ;; python specific stuff
  (require 'company-jedi)

  ;; Prog enable autocomplete+ snippets
  (add-hook 'prog-mode-hook
    (lambda ()
      (unless (derived-mode-p 'python-mode)
        (add-to-list 'company-backends '(company-lsp :with company-yasnippet)))))

  ;; Python enable autocomplete+ snippets
  (add-hook 'python-mode-hook
    (lambda()
      (add-to-list 'company-backends '(company-jedi :with company-yasnippet))))
  )
(defun my/git()
  "My git configs."
  (interactive)
  (use-package magit)
  (use-package magit-popup)
  (use-package magithub)
  ;; You need to install fringe-helper.el
  (use-package diff-hl
    :defines (diff-hl-margin-symbols-alist desktop-minor-mode-table)
    :commands diff-hl-magit-post-refresh
    :bind (:map diff-hl-command-map
            ("SPC" . diff-hl-mark-hunk))
    :hook ((after-init . global-diff-hl-mode)
            (dired-mode . diff-hl-dired-mode))
    :config
    ;; Highlight on-the-fly
    (diff-hl-flydiff-mode 1)

    ;; Set fringe style
    (setq-default fringes-outside-margins t)

    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector #b11100000)
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      (setq diff-hl-margin-symbols-alist
        '((insert . " ") (delete . " ") (change . " ")
           (unknown . " ") (ignored . " ")))
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
          '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

  ;; (require 'diff-hl)
  (global-diff-hl-mode 1)
  ;; (diff-hl-flydiff-mode 1)
  ;; (diff-hl-margin-minor-mode 1)
  (setq-default left-fringe-width  3)
  (setq-default right-fringe-width 3)
  (fringe-mode '( 3 . 3))
  ;; (setq-default linum-format " %2d " )
  (load-library "magit")
  (global-set-key "\C-xg" 'magit-status)

  )
(defun my/spaceline()
  "Create a spaceline."
  (interactive)
  ;; Icons
  ;; NOTE: Must run `M-x all-the-icons-install-fonts' manually on Windows
  (use-package all-the-icons
    :if (display-graphic-p)
    :init (unless (member "all-the-icons" (font-family-list))
            (all-the-icons-install-fonts t))
    :config
    (add-to-list 'all-the-icons-mode-icon-alist
      '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2))
    (add-to-list 'all-the-icons-icon-alist
      '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
    (add-to-list 'all-the-icons-icon-alist
      '("\\.lua$" all-the-icons-fileicon "lua" :face all-the-icons-dblue))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(lua-mode all-the-icons-fileicon "lua" :face all-the-icons-dblue))
    (add-to-list 'all-the-icons-icon-alist
      '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
    (add-to-list 'all-the-icons-icon-alist
      '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
    (add-to-list 'all-the-icons-icon-alist
      '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
    (add-to-list 'all-the-icons-icon-alist
      '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
    (add-to-list 'all-the-icons-icon-alist
      '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
    (add-to-list 'all-the-icons-mode-icon-alist
      '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-blue)))
  ;; enable spaceline
  (use-package spaceline)
  (require 'spaceline-config)
  (use-package spaceline-all-the-icons
    :after spaceline  ; eval-after-load doesn't work for this setup
    :config (progn
              ;; Initialization
              (spaceline-all-the-icons--setup-neotree)
              (spaceline-all-the-icons-theme)

              ;; Configuration
              (setq spaceline-highlight-face-func 'spaceline-highlight-face-default
                powerline-text-scale-factor 1.1
                powerline-default-separator 'bar
                spaceline-all-the-icons-icon-set-modified 'chain
                spaceline-all-the-icons-icon-set-window-numbering 'circle
                spaceline-all-the-icons-separator-type 'cup
                spaceline-all-the-icons-separators-type 'cup
                spaceline-all-the-icons-primary-separator "")

              ;; Toggles
              (spaceline-toggle-all-the-icons-buffer-size-off)
              (spaceline-toggle-all-the-icons-buffer-position-off)
              (spaceline-toggle-all-the-icons-vc-icon-off)
              (spaceline-toggle-all-the-icons-vc-status-on)
              (spaceline-toggle-all-the-icons-git-status-on)
              (spaceline-toggle-all-the-icons-flycheck-status-off)
              (spaceline-toggle-all-the-icons-time-on)
              (spaceline-toggle-all-the-icons-battery-status-off)
              (spaceline-toggle-hud-off)))
  (spaceline-spacemacs-theme)
  ;;(spaceline-emacs-theme)
  ;;(spaceline-all-the-icons-theme)
  ;;(spaceline-compile)
  )
(defun my/projectile()
  "make sure projectile is configured correctly"
  (interactive)
;; Manage and navigate projects
  (use-package projectile
    :diminish
    :bind (:map projectile-mode-map
            ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
            ("C-c p" . projectile-command-map))
    :hook (after-init . projectile-mode)
    :init
    (setq projectile-mode-line-prefix ""
      projectile-sort-order 'recentf
      projectile-use-git-grep t)
    :config
    ;; (projectile-update-mode-line)         ; Update mode-line at the first time

    ;; Use the faster searcher to handle project files: ripgrep `rg'.
    (when (and (not (executable-find "fd"))
            (executable-find "rg"))
      (setq projectile-generic-command
        (let ((rg-cmd ""))
          (dolist (dir projectile-globally-ignored-directories)
            (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
          (concat "rg -0 --files --color=never --hidden" rg-cmd))))

    ;; Support Perforce project
    (let ((val (or (getenv "P4CONFIG") ".p4config")))
      (add-to-list 'projectile-project-root-files-bottom-up val)))

  )
(defun my/ergoemacs ()
  "Ergoemacs."
  (interactive "P")
  ;; ergoemacs keys
  (use-package ergoemacs-mode)
  (use-package ergoemacs-status)
  ;;ergoemacs-mode
  (setq ergoemacs-theme "standard") ;; Uses Standard Ergoemacs keyboard theme
  (setq ergoemacs-keyboard-layout "pt-nativo") ;; Assumes QWERTY keyboard layout
  (ergoemacs-mode 1)
  )
(defun my/indent2spcs ()
  "2 spaces indent."
  (interactive)
  ;; Custom variables need to be set before anything
  (custom-set-variables
    '(c-default-style "k&r")
    '(c-basic-offset 2)
    '(default-tab-width 2 t)
    '(indent-tabs-mode nil)
    '(js-indent-level 2)
    '(lisp-indent-offset 2)
    '(perl-indent-level 2)
    '(ruby-indent-level 2)
    '(sh-basic-offset 2)
    '(sh-indentation 2)
    '(standard-indent 2)
    '(tab-width 2)
    '(python-indent-offset 4)
    '(tabbar-separator (quote (0.5))))
  ;; make the variables local also
  (setq c-basic-offset 2
    tab-width 2
    indent-tabs-mode nil
    c-default-style "k&r"
    c-basic-offset 2
    default-tab-width 2
    indent-tabs-mode nil
    js-indent-level 2
    lisp-indent-offset 2
    perl-indent-level 2
    ruby-indent-level 2
    sh-basic-offset 2
    sh-indentation 2
    standard-indent 2
    tab-width 2
    python-indent-offset 4)
  )
(defun my/highlight ()
  "highlights"
  (interactive)

  ;; Highlight the current line
  (use-package hl-line
    :ensure nil
    :hook (after-init . global-hl-line-mode))

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil)
  (dolist (hook '(prog-mode-hook outline-mode-hook conf-mode-hook))
    (add-hook hook (lambda ()
                     (setq show-trailing-whitespace t)
                     (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))))

  ;; Highlight the current line
  (use-package hl-line
    :ensure nil
    :hook (after-init . global-hl-line-mode))

  ;; Highlight matching parens
  (use-package paren
    :ensure nil
    :hook (after-init . show-paren-mode)
    :config (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t))

  ;; Highlight symbols
  (use-package symbol-overlay
    :diminish
    :custom-face
    (symbol-overlay-default-face ((t (:inherit 'region))))
    (symbol-overlay-face-1 ((t (:inherit 'highlight))))
    (symbol-overlay-face-2 ((t (:inherit 'font-lock-builtin-face :inverse-video t))))
    (symbol-overlay-face-3 ((t (:inherit 'warning :inverse-video t))))
    (symbol-overlay-face-4 ((t (:inherit 'font-lock-constant-face :inverse-video t))))
    (symbol-overlay-face-5 ((t (:inherit 'error :inverse-video t))))
    (symbol-overlay-face-6 ((t (:inherit 'dired-mark :inverse-video t :bold nil))))
    (symbol-overlay-face-7 ((t (:inherit 'success :inverse-video t))))
    (symbol-overlay-face-8 ((t (:inherit 'dired-symlink :inverse-video t :bold nil))))
    :bind (("M-i" . symbol-overlay-put)
            ("M-n" . symbol-overlay-jump-next)
            ("M-p" . symbol-overlay-jump-prev)
            ("M-N" . symbol-overlay-switch-forward)
            ("M-P" . symbol-overlay-switch-backward)
            ("M-C" . symbol-overlay-remove-all)
            ([M-f3] . symbol-overlay-remove-all))
    :hook ((prog-mode . symbol-overlay-mode)
            (iedit-mode . (lambda () (symbol-overlay-mode -1)))
            (iedit-mode-end . symbol-overlay-mode))
    :init (setq symbol-overlay-idle-time 0.01))

  ;; Highlight TODO and similar keywords in comments and strings
  (use-package hl-todo
    :bind (:map hl-todo-mode-map
            ([C-f3] . hl-todo-occur)
            ("C-c t p" . hl-todo-previous)
            ("C-c t n" . hl-todo-next)
            ("C-c t o" . hl-todo-occur))
    :hook (after-init . global-hl-todo-mode)
    :config
    (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
      (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
    (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
      (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

  )
(defun my/linenumbers ()
  "line lumbers"
  (interactive)
  ;; Line and Column
  (setq-default fill-column 100)
  (setq column-number-mode t)
  (setq line-number-mode t)

  ;; Show native line numbers if possible, otherwise use linum
  (if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
    (use-package linum-off
      :demand
      :defines linum-format
      :hook (after-init . global-linum-mode)
      :config
      (setq linum-format "%4d ")

      ;; Highlight current line number
      (use-package hlinum
        :defines linum-highlight-in-all-buffersp
        :hook (global-linum-mode . hlinum-activate)
        :custom-face (linum-highlight-face
                       ((t `(
                              :inherit default
                              :background nil
                              :foreground nil
                              ))))
        :init
        (setq linum-highlight-in-all-buffersp t))))
  (global-display-line-numbers-mode 1)
  )
(defun my/rainbow ()
  "Rainbow mode."
  (interactive)
  (use-package rainbow-mode)
  ;; Highlight brackets according to their depth
  (use-package rainbow-delimiters
    :defer 5
    :hook
    (prog-mode . rainbow-delimiters-mode)
    :config
    (rainbow-delimiters-mode +1)
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
      :foreground 'unspecified
      :inherit 'error))
  )
(defun my/wrap ()
  "adaptive-wrap."
  (interactive)
  (use-package adaptive-wrap)
  ;; Adaptive wrap anyways needs the `visual-line-mode' to be enabled. So
  ;; enable it only when the latter is enabled.
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  (global-visual-line-mode 1)
  )
(defun my/window()
  "define my window"
  (my/cursor)
  (my/tabs)
  (my/keys))
(defun my/ui ()
  "some ui options."
  (interactive)

  ;; mouse support
  (xterm-mouse-mode 1)
  ;; auto-save 1 minute
  (setq auto-save-timeout 120)
  (setq make-backup-files nil)     ; stop creating backup~ files
  (setq auto-save-default nil)     ; stop creating #autosave# files
  (setq create-lockfiles nil)      ; stop creating lock files

  ;; We don't want to type yes and no all the time so, do y and n
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; (setq initial-scratch-message nil)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
  (setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
  (setq adaptive-fill-first-line-regexp "^* *$")
  (setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
  (setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
  ;; (setq-default kill-whole-line t)           ; Kill line including '\n'

  (setq-default major-mode 'text-mode)

  ;; Delete selection if you insert
  (use-package delsel
    :ensure nil
    :hook (after-init . delete-selection-mode))

  ;; Rectangle
  (use-package rect
    :ensure nil
    :bind (("<C-return>" . rectangle-mark-mode)))

  ;; Automatically reload files was modified by external program
  (use-package autorevert
    :ensure nil
    :diminish
    :hook (after-init . global-auto-revert-mode))

  ;; Click to browse URL or to send to e-mail address
  (use-package goto-addr
    :ensure nil
    :hook ((text-mode . goto-address-mode)
            (prog-mode . goto-address-prog-mode)))

  ;; Automatic parenthesis pairing
  (use-package elec-pair
    :ensure nil
    :hook (after-init . electric-pair-mode)
    :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

  )

;;; End Custom functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show the function you are in
(which-function-mode 1)
;; show the parens
(show-paren-mode 1)

(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;;; Final Configs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start emacs daemon
;; Environment
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(if (and (fboundp 'server-running-p)
      (not (server-running-p)))
  (server-start))

(setenv "SHELL" "/bin/bash")
(my/fonts)
(my/linenumbers)
(my/window)
(my/highlight)
(my/indent2spcs)
(my/cpp)
(my/ruby)
(my/python)
(my/sh)
(my/lsp)
(my/company)
(my/git)
(my/ivy)
(my/spaceline)
;;;; The following may
;;;; not work on termux
(my/rainbow)
;;(my/ergoemacs)
(my/ui)
(my/wrap)
(my/projectile)
(add-hook 'after-change-major-mode-hook 'my/window)


;; keep custom setting in external file
(setq-default custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Check for errors
(require 'flycheck)
(global-flycheck-mode 1)
(flycheck-pos-tip-mode 1)
(use-package flycheck-pos-tip
  :defines flycheck-pos-tip-timeout
  :hook (global-flycheck-mode . flycheck-pos-tip-mode)
  :config (setq flycheck-pos-tip-timeout 30))
(flycheck-pos-tip-mode 1) ;; (flycheck-popup-tip-mode -1)
;; enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)
;; (yas-reload-all)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)

;; (with-eval-after-load 'yasnippet
;;   (setq yas-prompt-functions
;;     '(yas-x-prompt yas-dropdown-prompt yas-completing-prompt)))

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;     (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;           (null (do-yas-expand)))
;;       (if (check-expansion)
;;         (company-complete-common)
;;         (indent-for-tab-command)))))

;; (global-set-key [tab] 'tab-indent-or-complete)


;; Load my theme noctilux-theme dracula-theme or monokai-theme
;; (load-theme 'monokai)

(provide 'init)
;;; init.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
