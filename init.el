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
     company
     company-jedi company-anaconda company-irony company-tern
     company-rtags company-irony company-irony-c-headers
     company-php company-shell company-web
     company-lsp company-box
     lsp-mode lsp-ruby lsp-python lsp-sh lsp-ui
     cmake-ide
     sh-script
     lua-mode
     ;; auto-complete auto-complete-clang
     google-c-style
     rtags ggtags
     irony
     helm helm-swoop helm-company helm-flyspell helm-rtags helm-gtags
     projectile helm-projectile
     flycheck flycheck-pos-tip flycheck-irony flycheck-rtags
     rainbow-mode rainbow-delimiters
     paren symbol-overlay hl-line
     mode-icons all-the-icons-dired all-the-icons xpm
     spaceline
     spaceline-all-the-icons
     centaur-tabs ;; tabbar-ruler
     ;;highlight-thing
     ;;all-the-icons
     async
     bison-mode
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
     robe chruby rspec-mode rvm rubocop ruby-tools
     rake enh-ruby-mode
     ;; python related packages
     jedi pytest
     quickrun
     dumb-jump
     ;; visual
     adaptive-wrap
     column-enforce-mode
     ;; org md
     markdown-mode
     ;; ergoemacs keys
     ergoemacs-mode
     ergoemacs-status
     ;; themes
     noctilux-theme
     dracula-theme
     monokai-theme
     darkokai-theme
     ;;base16-theme
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
(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))
(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))
(use-package sh-script
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))
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
(yas-reload-all)
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

;;; Custom Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/fonts ()
  "start my fonts"
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

(defun my/tabs()
  "Use centaur-tabs"
  "https://github.com/ema2159/centaur-tabs"
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
  "Ricardo custom keys"
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
  (with-eval-after-load 'yasnippet
    (global-set-key (kbd "C-."  ) 'company-complete)
    (global-set-key (kbd "C-,"  ) 'company-yasnippet)
    )

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
    :config
    (setq company-backends '(( company-yasnippet)))
    :init
    (defun my-company-yasnippet ()
      (interactive)
      (company-abort)
      (call-interactively 'company-yasnippet))
    :config
    (setq company-tooltip-align-annotations t
      company-tooltip-limit 12
      company-idle-delay 0
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
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-:") 'helm-company)
       (define-key company-active-map (kbd "C-:") 'helm-company)))

  ;; python specific stuff
  (require 'company-jedi)
  )

(defun my/helm()
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

(defun my/git()
  (interactive)
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
  (setq-default left-fringe-width  5)
  (setq-default right-fringe-width 5)
  (fringe-mode '( 3 . 3))
  ;; (setq-default linum-format " %2d " )
  (load-library "magit")
  (global-set-key "\C-xg" 'magit-status)

  )

(defun my/spaceline()
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
  (require 'spaceline)
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-compile)
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
  )

(defun my/projectile()
  "make sure projectile is configured correctly"
  (interactive)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
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

  ;; Highlight brackets according to their depth
  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

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

  )
;;; End Custom functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show the function you are in
(which-function-mode 1)
;; nicer parenthesis handling
(smartparens-global-mode 1)
;; show the parens
(show-paren-mode 1)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

;;; Final Configs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start emacs daemon
(if (and (fboundp 'server-running-p)
      (not (server-running-p)))
  (server-start))

(setenv "SHELL" "/bin/bash")
(my/fonts)
(defun my/window()
  (my/cursor)
  (my/tabs)
  (my/keys))
(my/cpp)
(my/python)
(my/company)
(my/git)
(my/helm)
(my/projectile)
(my/window)
(my/indent2spcs)
(my/linenumbers)
(my/highlight)
(add-hook 'after-change-major-mode-hook 'my/window)

;;ergoemacs-mode
(setq ergoemacs-theme "standard") ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "pt-nativo") ;; Assumes QWERTY keyboard layout
;;(ergoemacs-mode 1)

;; mouse support
(xterm-mouse-mode 1)
;; auto-save 1 minute
(setq auto-save-timeout 120)
(setq make-backup-files nil)     ; stop creating backup~ files
;;(setq auto-save-default nil)     ; stop creating #autosave# files

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)
;; keep custom setting in external file
(setq-default custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

(with-eval-after-load 'yasnippet
  (setq yas-prompt-functions
    '(yas-x-prompt yas-dropdown-prompt yas-completing-prompt)))

;; Load my theme noctilux-theme dracula-theme or monokai-theme
;; (load-theme 'monokai)

;; last lines
(global-hl-line-mode 1)
;; Adaptive wrap anyways needs the `visual-line-mode' to be enabled. So
;; enable it only when the latter is enabled.
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(global-visual-line-mode 1)

(my/spaceline)
(provide 'init)

;;; init.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
