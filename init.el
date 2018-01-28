;;; init.el --- My Minimal Emacs configuration for Python and github

;;; PLACE: ~/emacsmini/.emacs.d/init.el
;;; RUN:   `env HOME=~/emacsmini emacs`
;;

(require 'package)

(setq my-packages
      '(;; generally useful packages
        evil
        evil-leader
        company
        company-jedi
        rainbow-mode
        rainbow-delimiters
        smartparens
        ;; git/github
        magit
        magithub
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

;; show the function you are in
(which-function-mode t)

;; company is the completion backend
(global-company-mode t)

;; nicer parenthesis handling
(smartparens-global-mode t)

;; show the parens
(show-paren-mode t)

;; show column number
(column-number-mode t)

;; show line number
(global-linum-mode t)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

(yas-global-mode t)

;; python specific stuff
(require 'company-jedi)

;; various settings for Jedi
(setq
 jedi:complete-on-dot t
 jedi:setup-keys t
 py-electric-colon-active t
 py-smart-indentation t)

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

;; Save custom variables on a external file:
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Load my theme noctilux-theme dracula-theme or monokai-theme
(load-theme 'monokai)

(provide 'init)
;;; init.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

