;;; init.el -- My Minimal Emacs config for Python, github and basics
;;;;;;;;;;;;;;;;;;  Ricardo Amaro 2018  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        PLACE: ~/emacsmini/.emacs.d/init.el                   ;;;
;;;        RUN:   `env HOME=~/emacsmini emacs`                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;;; Code:
(defvar my-packages)
(setq my-packages
      '(;; generally useful packages
        evil
        evil-leader
        company
        company-jedi company-irony
        company-php company-shell company-rtags
        cmake-ide
        auto-complete auto-complete-clang
        google-c-style
        rtags
        irony
        helm helm-company
        flycheck flycheck-pos-tip flycheck-irony
        rainbow-mode rainbow-delimiters
        smartparens
        powerline
        async
        highlight-symbol
        auto-highlight-symbol
        which-key
        popup
        markdown-mode
        ;; git/github
        magit
        magithub
        git-gutter
        ;; python related packages
        jedi
        pytest
        ;; themes
        noctilux-theme
        dracula-theme
        monokai-theme
        yasnippet)
      )

;; Add Melpa as the default Emacs Package repository
;; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; Activate all the packages (in particular autoloads)
(package-initialize)
;; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))
;; Install all missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; show the function you are in
(which-function-mode 1)
;; company is the completion backend
(global-company-mode 1)
;; nicer parenthesis handling
(smartparens-global-mode 1)
;; show the parens
(show-paren-mode 1)
;; show column number
(column-number-mode 1)
;; show line number
(global-linum-mode 1)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

(autoload 'helm-company "helm-company") ;; Enable helm company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

(add-to-list 'company-backends 'company-irony)
(add-to-list 'company-backends 'company-shell)
(add-to-list 'company-backends 'company-jedi)
(add-to-list 'company-backends 'company-ac-php-backend)

(global-auto-complete-mode 1)
(global-company-mode 1)

;; Check for errors
(require 'flycheck)
(global-flycheck-mode 1)
(flycheck-pos-tip-mode 1)

;; rtags enable
(require 'rtags)
;; C++ Setup ;;;;;;;;;;;;
(require 'cmake-ide)
(cmake-ide-setup)
;; Set cmake-ide-flags-c++ to use C++11
(setq cmake-ide-flags-c++ (append '("-std=c++11")))
;; We want to be able to compile with a keyboard shortcut
(global-set-key (kbd "C-c m") 'cmake-ide-compile)
;; Set rtags to enable completions and use the standard keybindings.
;; A list of the keybindings can be found at:
;; http://syamajala.github.io/c-ide.html
(setq rtags-autostart-diagnostics 1)
(rtags-diagnostics)
(setq rtags-completions-enabled 1)
(rtags-enable-standard-keybindings)

;; python specific stuff
(require 'company-jedi)
;; enable powerline
(require 'powerline)
(powerline-center-evil-theme)
;; various settings for Jedi
(setq
 jedi:complete-on-dot t
 jedi:setup-keys t
 py-electric-colon-active t
 py-smart-indentation t)

;; global git-gutter
(global-git-gutter-mode 1)

(load-library "magit")
;; (load-library "magithub")
;; (magithub-feature-autoinject t)
(global-set-key "\C-xg" 'magit-status)

;; Enable evil mode
(require 'evil)
(evil-mode t)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "w" 'save-buffer)

;; use helm for auto-complete commands
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)

;; cua mode
(cua-mode 1)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; mouse support
(xterm-mouse-mode 1)

;; 2 space identation and safe theme
(custom-set-variables
 '(indent-tabs-mode nil)
 '(tab-width 2)
 '(standard-indent 2)
 '(js-indent-level 2)
 '(sh-indentation 2)
 '(custom-safe-themes
   (quote
    ("53f97243218e8be82ba035ae34c024fd2d2e4de29dc6923e026d5580c77ff702" default)))
 )

;; set a default font to Droid
(when (member "Droid Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Droid Sans Mono")
  (set-frame-font "Droid Sans Mono-12"))  ; set font for current window

;; keep custom setting in external file
(setq-default custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Load my theme noctilux-theme dracula-theme or monokai-theme
(load-theme 'monokai)

(provide 'init)
;;; init.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
