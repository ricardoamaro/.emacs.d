;;; init.el -- My Minimal Emacs config for Python, github and basics
;;;;;;;;;;;;;;;;;;  Ricardo Amaro 2018  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        PLACE: ~/emacsmini/.emacs.d/init.el                   ;;;
;;;        RUN:   `env HOME=~/emacsmini emacs`                   ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq my-packages
      '(;; generally useful packages
        evil
        evil-leader
        helm
        company
        company-jedi company-php company-shell
        flycheck
        rainbow-mode
        rainbow-delimiters
        smartparens
        powerline
        async
        highlight-symbol
        auto-highlight-symbol
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

                                        ; Add Melpa as the default Emacs Package repository
                                        ; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
                                        ; Activate all the packages (in particular autoloads)
(package-initialize)
                                        ; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))
                                        ; Install all missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

                                        ; show the function you are in
(which-function-mode t)
                                        ; company is the completion backend
(global-company-mode t)
                                        ; nicer parenthesis handling
(smartparens-global-mode t)
                                        ; show the parens
(show-paren-mode t)
                                        ; show column number
(column-number-mode t)
                                        ; show line number
(global-linum-mode t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
(yas-global-mode t)
                                        ; python specific stuff
(require 'company-jedi)
                                        ; enable powerline
(require 'powerline)
(powerline-center-evil-theme)
                                        ; various settings for Jedi
(setq
 jedi:complete-on-dot t
 jedi:setup-keys t
 py-electric-colon-active t
 py-smart-indentation t)

                                        ; global git-gutter
(global-git-gutter+-mode)

(load-library "magit")
;; (load-library "magithub")
;; (magithub-feature-autoinject t)
(global-set-key "\C-xg" 'magit-status)

                                        ; Enable evil mode
(require 'evil)
(evil-mode t)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "w" 'save-buffer)

                                        ; use helm for auto-complete commands
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

                                        ; cua mode
(cua-mode 1)

                                        ; 2 space identation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default jssetq-defaul-indent-level 2)
(setq-default sh-indentation 2)

                                        ; keep custom setting in external file
(setq-default custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

                                        ; Load my theme noctilux-theme dracula-theme or monokai-theme
(load-theme 'monokai)

(provide 'init)
;;; init.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
