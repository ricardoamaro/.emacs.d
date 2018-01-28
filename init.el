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
        helm helm-company helm-flyspell helm-rtags
        flycheck flycheck-pos-tip flycheck-irony
        rainbow-mode rainbow-delimiters
        smartparens
        spaceline
        spaceline-all-the-icons
        tabbar-ruler
        mode-icons
        ;;all-the-icons
        async
        highlight-symbol
        auto-highlight-symbol
        which-key
        popup
        markdown-mode
        ;; git/github
        magit
        magithub
        git-gutter+ git-gutter-fringe+
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


;;; Custom Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mytabbar()
  (interactive)
  (custom-set-variables '(tabbar-separator (quote (0.5))))
  (setq tabbar-ruler-global-tabbar 1)    ; get tabbar
  (setq tabbar-ruler-excluded-buffers
        '("*Messages*" "*Completions*" "*ESS*" "*Packages*" "*log-edit-files*"
          "*helm-mini*" "*helm-mode-describe-variable*" "*Minibuf-0*" "*Minibuf-1*"
          "*Echo Area 0" "Echo Area 1*" "*which-key*"))
  (require 'tabbar-ruler)
  (run-hooks 'tabbar-load-hook)
  (set-face-attribute 'tabbar-default nil
                      :background "gray20" :foreground "gray60"
                      :box '(:line-width 1 :color "gray20" :style nil))
  (set-face-attribute 'tabbar-unselected nil
                      :background "gray30" :foreground "white"
                      :box '(:line-width 1 :color "gray30" :style nil))
  (set-face-attribute 'tabbar-unselected-highlight nil
                      :background "gray30" :foreground "yellow"
                      :box '(:line-width 1 :color "gray30" :style nil))
  (set-face-attribute 'tabbar-unselected-modified nil
                      :background "gray30" :foreground "#af5f00"
                      :box '(:line-width 1 :color "gray30" :style nil))
  (set-face-attribute 'tabbar-selected nil
                      :background "gray75"  :foreground "black"
                      :box '(:line-width 5 :color "gray75" :style nil))
  (set-face-attribute 'tabbar-selected-highlight nil
                      :background "yellow"  :foreground "black"
                      :box '(:line-width 5 :color "yellow" :style nil))
  (set-face-attribute 'tabbar-selected-modified nil
                      :background "gray75"  :foreground "red"
                      :box '(:line-width 5 :color "gray75" :style nil))
  (set-face-attribute 'tabbar-highlight nil
                      :background "gray75" :foreground "black"
                      :underline t
                      :box '(:line-width 5 :color "gray75" :style nil))
  (set-face-attribute 'tabbar-button nil
                      :box '(:line-width1 :color "gray20" :style nil))
  (set-face-attribute 'tabbar-separator nil
                      :background "gray20" :height 0.5)
  (setq tool-bar-border 0)
  (tabbar-ruler-group-by-projectile-project)
  )

(defun mycpp ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Setup cmake-ide & rtags
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  )

(defun mypython ()
  ;; various settings for Jedi
  (setq
   jedi:complete-on-dot t
   jedi:setup-keys t
   py-electric-colon-active t
   py-smart-indentation t)
  )

(defun mycursor()
  (setq-default cursor-type '("#ffffff" (bar . 2)))
  (setq-default evil-insert-state-cursor '("#ffffff" (bar . 2)))
  (setq evil-emacs-state-cursor '("#ffffff" (bar . 2)))
  (custom-set-variables
   '(blink-cursor-mode t))
  )

(defun mycompany()
  (require 'company)
  ;; company is the completion backend
  (global-company-mode 1)
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company)))
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-shell)
  (add-to-list 'company-backends 'company-jedi)
  (add-to-list 'company-backends 'company-ac-php-backend)
  ;; python specific stuff
  (require 'company-jedi)
  )

(defun myhelm()
  (autoload 'helm-company "helm-company") ;; Enable helm company
  )

(defun mygit()
  ;; global git-gutter
  (global-git-gutter+-mode 1)
  (load-library "magit")
  ;; (load-library "magithub")
  ;; (magithub-feature-autoinject t)
  (global-set-key "\C-xg" 'magit-status)
  )

(defun myspaceline()
  ;; enable spaceline
  (require 'spaceline)
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  ;;(require 'spaceline-all-the-icons)
  ;;(spaceline-all-the-icons-theme)
  (spaceline-compile)
  )
;;; End Custom functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show the function you are in
(which-function-mode 1)
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

(global-auto-complete-mode 1)

;; Check for errors
(require 'flycheck)
(global-flycheck-mode 1)
(flycheck-pos-tip-mode 1)

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

;;; Final Configs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(mytabbar)
(add-hook 'window-setup-hook #'mytabbar)
(mycpp)
;;(mycursor)
(mygit)
(myhelm)
(mycompany)

;; cua mode
(cua-mode 1)
;; mouse support
(xterm-mouse-mode 1)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

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
  (set-frame-font "Droid Sans Mono-11"))  ; set font for current window

;; keep custom setting in external file
(setq-default custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Load my theme noctilux-theme dracula-theme or monokai-theme
(load-theme 'monokai)
(setq monokai-distinct-fringe-background 1)

(global-auto-highlight-symbol-mode t)
(auto-highlight-symbol-mode t)
(ahs-set-idle-interval 1.0) ;; 1s before highlight

(tool-bar-mode 0)
;;(menu-bar-mode 1)
(set-scroll-bar-mode nil)
(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; last lines
(myspaceline)
(provide 'init)
;;; init.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
