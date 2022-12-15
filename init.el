;; configure flags: --with-imagemagick --with-json --with-tree-sitter --with-pgtk --with-xwidgets --with-native-compilation=aot

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

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  ;; For evil-collection to work
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
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

  (setq-default indent-tabs-mode nil)
  (setq-default electric-indent-inhibit t)
  (setq backward-delete-char-untabify-method 'hungry)

  (pixel-scroll-precision-mode 1)

  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)



  (general-define-key
    "M-h" '(evil-window-left :which-key "Switch window left")
    "M-j" '(evil-window-down :which-key "Switch window down")
    "M-k" '(evil-window-up :which-key "Switch window up")
    "M-l" '(evil-window-right :which-key "Switch window right"))
  (general-define-key
    :states '(normal motion)
    ";" 'evil-ex)

  (my-global-definer
    "." '(find-file :which-key "Find file"))

  (my-general-global-menu! "eval" "e"
    "r" 'eval-region
    "b" 'eval-buffer
    "d" 'eval-defun)

  (my-general-global-menu! "buffer" "b"
    "i" 'ibuffer))



(use-package gotham-theme
  :straight (gotham-theme :type git :repo "https://depp.brause.cc/gotham-theme.git")
  :config
  (load-theme 'gotham t))

(use-package vertico
  :config
  (vertico-mode))

(use-package corfu
  :straight (corfu :files ("corfu.el" "extensions/corfu-popupinfo.el"))
  :config
  (setq corfu-auto t)
  (global-corfu-mode)
  (corfu-popupinfo-mode 1))

(use-package treesit
  :config
  (setq treesit-extra-load-path '("~/src/tree-sitter-module/dist")))

(use-package eglot
  ;; already included in emacs 29+, don't get the ELPA version
  :straight nil)

(use-package magit
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (my-general-global-menu! "git" "g"
    "g" 'magit-status
    "d" 'magit-dispatch
    "f" 'magit-file-dispatch))

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

;;; Language Modes

(use-package flex
  :straight (flex :type git :host github :repo "manateelazycat/flex")
  :config
  (add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
  (autoload 'flex-mode "flex"))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-popupinfo ((t (:inherit corfu-default :height 1.0))))
 '(persp-selected-face ((t (:weight bold)))))
