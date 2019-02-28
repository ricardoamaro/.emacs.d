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
     ;; evil
     ;; evil-leader
     company
     company-jedi company-anaconda company-irony company-tern
     company-rtags company-irony company-irony-c-headers
     company-php company-shell company-web
     cmake-ide
     ;; auto-complete auto-complete-clang
     google-c-style
     rtags ggtags
     irony
     helm helm-swoop helm-company helm-flyspell helm-rtags helm-gtags
     flycheck flycheck-pos-tip flycheck-irony flycheck-rtags
     rainbow-mode rainbow-delimiters
     smartparens
     spaceline
     spaceline-all-the-icons
     tabbar-ruler
     mode-icons
     ;;all-the-icons
     async
     bison-mode
     projectile
     which-key
     popup
     markdown-mode
     web-mode
     json-mode
     ;; git/github
     magit
     magit-popup
     magithub
     diff-hl
     ;; ruby
     robe
     chruby
     rspec-mode
     rvm
     rubocop
     ruby-tools
     rake
     enh-ruby-mode
     ;; python related packages
     jedi
     pytest
     quickrun
     dumb-jump
     ;; themes
     noctilux-theme
     dracula-theme
     monokai-theme
     darkokai-theme
     base16-theme
     yasnippet yasnippet-snippets auto-yasnippet)
  )

;; Add Melpa as the default Emacs Package repository
;; only contains a very limited number of packages
(add-to-list 'package-archives
  '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade.ferrier.me.uk/packages") t)

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

(defun my/themes-reset ()
  "Reset themes."
  (interactive)
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  (set-face-attribute 'fringe nil :background nil)
  (set-face-attribute 'linum nil :background nil)
  ;; Selected text
  (set-face-attribute  'region nil :background nil)
  (set-face-foreground 'highlight nil)
  )

(defun my/themes-dark()
  "Apply dark themes values."
  (interactive)
  ;; remove spacemacs background
  ;; (my/themes-reset)

  ;; Fix darkokai colors
  (with-eval-after-load 'darkokai-theme
    (set-face-attribute 'region nil :background "#6b4e2e")
    (set-face-foreground 'highlight nil))
  (setq-default
    darkokai-mode-line-padding 1 ;; Default mode-line box width
    darkokai-distinct-fringe-background t
    darkokai-high-contrast-mode-line nil
    darkokai-use-variable-pitch nil
    darkokai-height-minus-1 0.8
    darkokai-height-plus-1 1.0
    darkokai-height-plus-2 1.05
    darkokai-height-plus-3 1.1
    darkokai-height-plus-4 1.15)

  ;; Fix monokai colors
  (with-eval-after-load 'monokai-theme
    (set-face-attribute 'fringe nil :background "#181a1b")
    (set-face-attribute 'linum nil :background "#202324")
    ;; Selected text
    (set-face-attribute  'region nil :background "#6b4e2e")
    (set-face-foreground 'highlight nil)
    ;; (load-theme 'monokai-theme)
    )
  (setq-default
    monokai-distinct-fringe-background t
    monokai-foreground     "#f8fbfc"
    monokai-background     "#0c0d0d"
    monokai-highlight      "#5D6365"
    monokai-highlight-alt  "#3E3D31"
    ;; current line bg
    monokai-highlight-line "#202324"
    monokai-line-number    "#5b5c57"
    monokai-emphasis       "#ffffff"
    monokai-comments       "#6A6D70"
    ;; colours
    monokai-blue           "#06d8ff"
    monokai-cyan           "#53f2dc"
    monokai-green          "#63de5d"
    monokai-gray           "#3E4451"
    monokai-violet         "#ab7eff"
    monokai-magenta        "#ff8eff"
    monokai-red            "#ff0066"
    monokai-orange         "#ffac4a"
    monokai-yellow         "#E6DB74"
    monokai-gray           "#35393b"
    ;; org-mode headers
    monokai-height-minus-1 0.8
    monokai-height-plus-1 1.0
    monokai-height-plus-2 1.05
    monokai-height-plus-3 1.1
    monokai-height-plus-4 1.15
    monokai-user-variable-pitch t)
  )

(defun my/tabbar()
  "Create a nice tabbar grouped by projectile project"
  (interactive)
  (require 'tabbar-ruler)
  (custom-set-variables '(tabbar-separator (quote (0.5))))
  (setq tabbar-ruler-global-tabbar 1)    ; get tabbar
  (setq tabbar-ruler-excluded-buffers
    '("*Messages*" "*Completions*" "*ESS*" "*Packages*" "*log-edit-files*"
       "*helm-mini*" "*helm-mode-describe-variable*" "*Minibuf-0*" "*Minibuf-1*"
       "*Echo Area 0" "Echo Area 1*" "*which-key*"))
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
    :box '(:line-width 1 :color "gray20" :style nil))
  (set-face-attribute 'tabbar-separator nil
    :background "gray20" :height 0.5)
  (setq tool-bar-border 0)
  (projectile-mode t)
  (tabbar-ruler-group-by-projectile-project)
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

(defun my/python ()
  ;; various settings for Jedi
  (interactive)
  (setq
    jedi:complete-on-dot t
    jedi:setup-keys t
    py-electric-colon-active t
    py-smart-indentation t)
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
  (require 'company)
  ;; company is the completion backend
  (global-company-mode t)
  (global-auto-complete-mode t)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.8)
  (setq company-minimum-prefix-length 2)
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company)))
  (setq company-backends '((company-shell company-jedi company-capf enh-ruby-mode ruby-mode company-semantic company-files company-ac-php-backend company-elisp company-inf-ruby company-anaconda company-robe company-gtags company-rtags company-irony-c-headers company-web company-web-html company-web-jade company-web-slim company-go company-irony company-clang company-keywords company-cmake company-css company-yasnippet)
(company-dabbrev company-dabbrev-code)))
  ;; python specific stuff
  (require 'company-jedi)
  )

(defun my/helm()
  (interactive)
  (autoload 'helm-company "helm-company") ;; Enable helm company
  )

(defun my/git()
  (interactive)
  ;; You need to install fringe-helper.el
  (require 'diff-hl)
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  ;;(diff-hl-margin-minor-mode 1)
  (setq-default left-fringe-width  5)
  (setq-default right-fringe-width 5)
  (fringe-mode '( 3 . 3))
  (setq-default linum-format " %2d " )
  (load-library "magit")
  (global-set-key "\C-xg" 'magit-status)
  )

(defun my/spaceline()
  (interactive)
  ;; enable spaceline
  (require 'spaceline)
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
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

;; Check for errors
(require 'flycheck)
(global-flycheck-mode 1)
(flycheck-pos-tip-mode 1)

;; use helm for auto-complete commands
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "M-s") #'helm-swoop)
(helm-mode 1)

;; enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-reload-all)

;;; Final Configs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start emacs daemon
(if (and (fboundp 'server-running-p)
      (not (server-running-p)))
  (server-start))

(setenv "SHELL" "/bin/bash")
(defun my/window()
(my/cursor)
(my/tabbar))
(my/cpp)
(my/python)
(my/company)
(my/themes-dark)
(my/git)
(my/helm)
(my/window)
(add-hook 'after-change-major-mode-hook 'my/window)

;; cua mode
(cua-mode 1)
;; mouse support
(xterm-mouse-mode 1)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; 2 space identation and safe theme
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

;; set a default font to Droid
(when (member "Droid Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Droid Sans Mono")
  (set-frame-font "Droid Sans Mono-11"))  ; set font for current window

;; keep custom setting in external file
(setq-default custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Put file path on title bar
(setq frame-title-format '("%f   (" invocation-name "@" system-name " | %m)") )
(tool-bar-mode 0)
(menu-bar-mode 0)

(if (display-graphic-p)
    (progn
      ;;(tool-bar-mode 0)
      (scroll-bar-mode 0)))

(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

(with-eval-after-load 'yasnippet
   (setq yas-prompt-functions
 	'(yas-x-prompt yas-dropdown-prompt yas-completing-prompt)))

;; Load my theme noctilux-theme dracula-theme or monokai-theme
;; (load-theme 'monokai)

;; last lines
(load-theme 'monokai)
(global-hl-line-mode )
(my/spaceline)
(provide 'init)

;;; init.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

