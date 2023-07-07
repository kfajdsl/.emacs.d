;; configure flags: --with-imagemagick --with-json --with-tree-sitter --with-pgtk --with-xwidgets --with-native-compilation=aot --without-pop

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package bug-hunter)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  ;; For evil-collection to work
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-ex-substitute-global t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package general
  :after evil-collection
  :config
  (general-create-definer my-global-definer
    :keymaps 'override
    :states  '(motion normal insert emacs)
    :prefix  "SPC"
    :non-normal-prefix "C-SPC")
  ;; https://github.com/noctuid/general.el#how-do-i-prevent-key-sequence-starts-with-non-prefix-key-errors
  (my-global-definer "" nil)
  
  
  ;; stolen from https://gist.github.com/progfolio/1c96a67fcec7584b31507ef664de36cc
  (defmacro my-general-global-menu! (name infix-key &rest body)
    "Create a definer named my-general-global-NAME wrapping global-definer.
Create prefix map: my-general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "my-general-global-" name))
         :wrapping my-global-definer
         :prefix-map (quote ,(intern (concat "my-general-global-" name "-map")))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "my-general-global-" name))
        ,@body))))

(use-package emacs
  :config
  (menu-bar-mode -1) 
  (scroll-bar-mode -1) 
  (tool-bar-mode -1) 
  (setq ring-bell-function 'ignore)
  (setq default-frame-alist '((undecorated . t)
			                  (font . "Iosevka-12")
                              (vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)))
  
  (setq-default electric-indent-inhibit t)
  (setq backward-delete-char-untabify-method 'hungry)
  
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (setq-default c-basic-offset 4)
  
  (add-hook 'c-mode-common-hook
            (lambda()
              (c-set-offset 'inextern-lang 0)))
  
  (pixel-scroll-precision-mode 1)
  
  (setq mouse-autoselect-window t)
  
  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  
  (setq xref-search-program 'ripgrep)
  
  (load-theme 'wheatgrass)
  (setq dired-dwim-target t)
  (setq mode-line-position (list "(%l,%c)"))

  (electric-pair-mode 1)

  (defun unique-shx ()
    (interactive)
    (call-interactively 'shx)
    (rename-uniquely))
  
  
  (general-define-key
   :keymaps '(override vterm-mode-map)
   "M-h" '(evil-window-left :which-key "Switch window left")
   "M-j" '(evil-window-down :which-key "Switch window down")
   "M-k" '(evil-window-up :which-key "Switch window up")
   "M-l" '(evil-window-right :which-key "Switch window right")
   "M-u" 'universal-argument)

  ; eshell is dumb
  (add-hook 'eshell-mode-hook (lambda () (general-define-key
                                          :keymaps 'eshell-mode-map
                                          "M-h" '(evil-window-left :which-key "Switch window left")
                                          "M-j" '(evil-window-down :which-key "Switch window down")
                                          "M-k" '(evil-window-up :which-key "Switch window up")
                                          "M-l" '(evil-window-right :which-key "Switch window right"))))



  (general-define-key
   :keymaps '(override vterm-mode-map)
   :states '(normal motion)
   ";" 'evil-ex)
  
  (my-global-definer
    "." '(find-file :which-key "Find file")
    "/" '(project-search :which-key "Project Search"))
  
  (my-general-global-menu! "eval" "e"
    "r" 'eval-region
    "b" 'eval-buffer
    "d" 'eval-defun)
  
  (my-general-global-menu! "buffer" "b"
    "i" 'ibuffer
    "k" 'kill-this-buffer
    "K" 'kill-buffer-ask
    "r" 'rename-buffer
    "R" 'revert-buffer)
  
  (my-general-global-menu! "project" "p")
  
  (my-general-global-menu! "code" "c"
    "a" 'eglot-code-actions
    "r" 'eglot-rename
    "c" 'recompile)
  (my-general-global-menu! "find" "f"
    "r" 'xref-find-references)

  (my-general-global-menu! "spawn" "s"
    "s" '(unique-shx :which-key "Spawn new shx"))
  
  (defun find-first-non-ascii-char ()
    "Find the first non-ascii character from point onwards."
    (interactive)
    (let (point)
      (save-excursion
        (setq point
              (catch 'non-ascii
                (while (not (eobp))
                  (or (eq (char-charset (following-char))
                          'ascii)
                      (throw 'non-ascii (point)))
                  (forward-char 1)))))
      (if point
          (goto-char point)
        (message "No non-ascii characters."))))

  (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-ts-mode))

  (setq compilation-scroll-output 'first-error)


  (defun change-font-size (new-size)
    "Change the font size to the given value"
    (interactive "nNew font size: ")
    (set-face-attribute 'default nil :height (* 10 new-size))))

(use-package treesit-auto
  :straight (treesit-auto :type git :host github :repo "renzmann/treesit-auto")
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode)
  (setq java-ts-mode-hook java-mode-hook)
  (setq c-ts-mode-hook c-mode-hook)
  (setq c++-ts-mode-hook c-mode-hook))

(use-package evil-snipe
  :init
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-scope 'whole-visible)
  :config
  (delete 'dired-mode evil-snipe-disabled-modes)

  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package gotham-theme
  :straight (gotham-theme :type git :repo "https://depp.brause.cc/gotham-theme.git")
  :config
                                        ;(load-theme 'gotham t))
  )

(use-package vertico
  :init
  (vertico-mode))

;(use-package orderless
;  :config
;  (setq completion-styles '(orderless basic)
;        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :config
  (my-general-global-buffer
    "s" 'consult-buffer))

(use-package consult-project-extra
  :config
  (my-general-global-project
    "s" 'consult-project-extra-find))

(use-package corfu
  :straight (corfu :files ("corfu.el" "extensions/corfu-popupinfo.el"))
  :init
  (setq corfu-auto t)
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 0)
  (setq completion-styles '(substring))
  (global-corfu-mode)
  (corfu-popupinfo-mode 1))

(use-package treesit
  :commands (treesit-install-language-grammar my/treesit-install-all-languages)
  :init
)

(use-package eldoc-box)

(use-package magit
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (my-general-global-menu! "git" "g"
    "g" 'magit-status
    "d" 'magit-dispatch
    "f" 'magit-file-dispatch
    "c" 'magit-clone
    "i" 'magit-init))

(use-package perspective 
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  (my-general-global-menu! "persp" "TAB"
    "s" 'persp-switch
    "`" 'persp-switch-by-number
    "k" 'persp-remove-buffer
    "c" 'persp-kill
    "r" 'persp-rename
    "a" 'persp-add-buffer
    "A" 'persp-set-buffer
    "b" 'persp-switch-to-buffer))

(use-package xterm-color
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

  ;; color for *compilation* buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (if (string-prefix-p "*compilation" (buffer-name (process-buffer proc)))
                        (xterm-color-filter string) string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

(use-package shx)


(use-package python-black)

(use-package neotree
  :config
  (defun my-neotree-toggle ()
    "Switch neotree root to project root and toggle"
    (interactive)
    (neo-global--open-dir (project-root (buffer-file-name))
                          (neotree-toggle))))

(use-package vterm)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package minimap)

;(use-package bash-completion
;  :config
;  (autoload 'bash-completion-dynamic-complete
;    "bash-completion"
;    "BASH completion hook")
;  (add-hook 'shell-dynamic-complete-functions
;            'bash-completion-dynamic-complete))

(use-package rotate)

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package leetcode)

(use-package poetry)

(use-package eglot-x
  :straight (eglot-x :type git :host github :repo "nemethf/eglot-x")
  :after (eglot)
  :config
  (eglot-x-setup))


;;; LANGUAGE MODES

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package flex
  :straight (flex :type git :host github :repo "manateelazycat/flex")
  :config
  (add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
  (autoload 'flex-mode "flex"))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package markdown-mode)

(use-package dockerfile-mode)

(use-package cmake-mode
  :config
  (add-to-list 'auto-mode-alist '("\\CMakeLists\\.txt\\'" . cmake-mode)))

(use-package groovy-mode)

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'rust-mode-hook 'eglot-ensure))

(use-package wgsl-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4 t)
 '(minimap-dedicated-window nil)
 '(minimap-window-location 'right))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-popupinfo ((t (:inherit corfu-default :height 1.0))))
 '(eldoc-box-border ((t (:background "#245361"))))
 '(fixed ((t nil)))
 '(fixed-pitch ((t (:inherit fixed))))
 '(fixed-pitch-serif ((t (:family "Iosevka Slab"))))
 '(persp-selected-face ((t (:weight bold)))))
