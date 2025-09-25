;;; Enhanced Minimal Emacs Configuration for C/C++/Assembly/Python
;;; Simple, fast, and easily extensible with essential productivity features
;;; No emojis/icons in terminal; minimal dependencies

;;; ----------------------------------------
;;; Performance Optimization
;;; ----------------------------------------
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;;; ----------------------------------------
;;; Package Management Setup
;;; ----------------------------------------
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; ----------------------------------------
;;; Basic Settings
;;; ----------------------------------------
(setq inhibit-startup-message t
      ring-bell-function 'ignore
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t))
      require-final-newline t
      scroll-conservatively 10000)

;; Sane defaults
(setq-default indent-tabs-mode nil)  ; Use spaces instead of tabs

;; Clean interface
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)

;; Font configuration - Martian Mono (only if present)
(when (find-font (font-spec :name "Martian Mono"))
  (set-face-attribute 'default nil :font "Martian Mono" :height 160))

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;; ----------------------------------------
;;; Theme - Simple and Clean
;;; ----------------------------------------
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

;;; ----------------------------------------
;;; Enforce Emacs keybindings (Guru Mode)
;;; ----------------------------------------
(use-package guru-mode
  :hook (after-init . guru-global-mode)
  :config
  ;; Warn instead of blocking; set to nil to hard-disable offending keys
  (setq guru-warn-only t))

;;; ----------------------------------------
;;; Discoverability / Minibuffer UX (lightweight, no icons)
;;; ----------------------------------------
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-min-display-lines 5))

;; Fast, minimal completion stack (works on Emacs 27+)
(use-package vertico
  :init (vertico-mode 1))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;;; ----------------------------------------
;;; Enhanced Search and Navigation
;;; ----------------------------------------
(use-package swiper
  :bind ("C-s" . swiper))

(use-package avy
  :bind ("C-;" . avy-goto-char-2))

;;; ----------------------------------------
;;; Window/Buffer Navigation (Ace Window)
;;; ----------------------------------------
(use-package ace-window
  :ensure t
  :commands (ace-window)
  :init
  ;; Bind early so it’s available even before the package fully loads
  (global-set-key (kbd "M-o")  #'ace-window)
  (setq aw-ignore-current t
        aw-scope 'frame
        aw-dispatch-always t))

        ;; --- Make M-o always ace-window (override facemenu or others) ---
(add-hook 'after-init-hook
          (lambda ()
            (global-set-key (kbd "M-o")  #'ace-window)  ;; primary
            (global-set-key (kbd "C-x o") #'ace-window) ;; override other-window
            (global-set-key (kbd "C-c o") #'ace-window))) ;; terminal-safe fallback


;;; ----------------------------------------
;;; Code Completion - Lightweight Alternative to LSP
;;; ----------------------------------------
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2
        company-show-numbers t))

;; Better completion for C/C++
(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

;;; ----------------------------------------
;;; Syntax Highlighting and Language Modes
;;; ----------------------------------------
;; C/C++ - Built-in with enhancements
(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "linux"
        c-basic-offset 4)
  (setq company-c-headers-path-system
        '("/usr/include/"
          "/usr/local/include/"
          "/usr/include/linux/")))

;; Assembly - Multiple syntax support
(use-package nasm-mode
  :mode "\\.\\(asm\\|nasm\\|s\\)\\'")

;; FASM (Flat Assembler) support - Manual installation on first run
(unless (file-exists-p (expand-file-name "fasm-mode" user-emacs-directory))
  (let ((fasm-dir (expand-file-name "fasm-mode" user-emacs-directory)))
    (unless (file-directory-p fasm-dir)
      (message "Installing FASM mode...")
      (shell-command (format "git clone https://github.com/the-little-language-designer/fasm-mode.git %s" fasm-dir)))))

(add-to-list 'load-path (expand-file-name "fasm-mode" user-emacs-directory))
(autoload 'fasm-mode "fasm-mode" "Major mode for editing FASM assembly files" t)
;; FIXED regex (original had a typo)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|fasm\\|inc\\)\\'" . fasm-mode))

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq python-indent-offset 4))

;; Python virtual environment support
(use-package pyvenv
  :config
  (pyvenv-mode 1))

;;; ----------------------------------------
;;; Code Navigation - Tags (lightweight)
;;; ----------------------------------------
(use-package ggtags
  :hook ((c-mode c++-mode python-mode asm-mode nasm-mode fasm-mode) . ggtags-mode)
  :config
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags))

(setq tags-revert-without-query t)
(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)

;;; ----------------------------------------
;;; Editing Aids
;;; ----------------------------------------
(use-package smartparens
  :config
  (smartparens-global-mode 1))

(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c h") 'hs-toggle-hiding)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"  . warning)
          ("FIXME" . error)
          ("NOTE"  . success)
          ("HACK"  . font-lock-constant-face))))

;;; ----------------------------------------
;;; Version Control & File Management
;;; ----------------------------------------
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

;;; ----------------------------------------
;;; Better Help System
;;; ----------------------------------------
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;;; ----------------------------------------
;;; Terminal Integration
;;; ----------------------------------------
(global-set-key (kbd "C-c t") 'term)
;; Disable line numbers inside terminal buffers
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode 0)))

;;; ----------------------------------------
;;; Header/File Templates (Auto Insert)
;;; ----------------------------------------
(use-package autoinsert
  :ensure nil
  :init
  (auto-insert-mode 1)
  :config
  (setq auto-insert-directory "~/.emacs.d/templates/"
        auto-insert-query nil)

  ;; C header template
  (define-auto-insert "\\.h\\'"
    '("Header guard: "
      "#ifndef " (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))) "_H\n"
      "#define " (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))) "_H\n\n"
      "/* " (file-name-nondirectory buffer-file-name) " - " (read-string "Description: ") " */\n\n"
      "#ifdef __cplusplus\n"
      "extern \"C\" {\n"
      "#endif\n\n"
      _ "\n\n"
      "#ifdef __cplusplus\n"
      "}\n"
      "#endif\n\n"
      "#endif /* " (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))) "_H */\n"))

  ;; C source template
  (define-auto-insert "\\.c\\'"
    '("C source: "
      "/* " (file-name-nondirectory buffer-file-name) " - " (read-string "Description: ") " */\n\n"
      "#include \"" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".h\"\n\n"
      _))

  ;; C++ source template
  (define-auto-insert "\\.cpp\\'"
    '("C++ source: "
      "/* " (file-name-nondirectory buffer-file-name) " - " (read-string "Description: ") " */\n\n"
      "#include \"" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".h\"\n\n"
      _))

  ;; Python template
  (define-auto-insert "\\.py\\'"
    '("Python script: "
      "#!/usr/bin/env python3\n"
      "# -*- coding: utf-8 -*-\n"
      "\"\"\"" (read-string "Description: ") "\"\"\"\n\n"
      "import sys\n"
      "import os\n\n"
      "def main():\n"
      "    \"\"\"Main function\"\"\"\n"
      "    " _ "\n\n"
      "if __name__ == '__main__':\n"
      "    main()\n"))

  ;; Assembly template (NASM)
  (define-auto-insert "\\.\\(asm\\|s\\)\\'"
    '("Assembly file: "
      "; " (file-name-nondirectory buffer-file-name) " - " (read-string "Description: ") "\n"
      "; Author: " (user-full-name) "\n"
      "; Date: " (format-time-string "%Y-%m-%d") "\n\n"
      ".section .data\n"
      "    ; Data section\n\n"
      ".section .text\n"
      "    .global _start\n\n"
      "_start:\n"
      "    " _ "\n\n"
      "    ; Exit program\n"
      "    mov rax, 60\n"
      "    mov rdi, 0\n"
      "    syscall\n"))

  ;; FASM template
  (define-auto-insert "\\.fasm\\'"
    '("FASM file: "
      "; " (file-name-nondirectory buffer-file-name) " - " (read-string "Description: ") "\n"
      "; Author: " (user-full-name) "\n"
      "; Date: " (format-time-string "%Y-%m-%d") "\n\n"
      "format ELF64 executable 3\n\n"
      "segment readable executable\n\n"
      "entry start\n\n"
      "start:\n"
      "    " _ "\n\n"
      "    ; Exit program\n"
      "    mov rax, 60\n"
      "    mov rdi, 0\n"
      "    syscall\n")))

;;; ----------------------------------------
;;; Compilation
;;; ----------------------------------------
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)

(defun my-compile-command ()
  "Set compile command based on current buffer's major mode."
  (let ((file (file-name-nondirectory buffer-file-name))
        (file-noext (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (cond
     ((eq major-mode 'c-mode)
      (format "gcc -Wall -Wextra -std=c99 -g -o %s %s" file-noext file))
     ((eq major-mode 'c++-mode)
      (format "g++ -Wall -Wextra -std=c++17 -g -o %s %s" file-noext file))
     ((eq major-mode 'python-mode)
      (format "python3 %s" file))
     ((or (eq major-mode 'nasm-mode) (eq major-mode 'asm-mode))
      (format "nasm -f elf64 %s -o %s.o && ld %s.o -o %s" file file-noext file-noext file-noext))
     ((eq major-mode 'fasm-mode)
      (format "fasm %s %s" file file-noext))
     (t "make"))))

(defun my-set-compile-command ()
  "Set compile-command for current buffer."
  (set (make-local-variable 'compile-command) (my-compile-command)))

(add-hook 'c-mode-hook 'my-set-compile-command)
(add-hook 'c++-mode-hook 'my-set-compile-command)
(add-hook 'python-mode-hook 'my-set-compile-command)
(add-hook 'nasm-mode-hook 'my-set-compile-command)
(add-hook 'fasm-mode-hook 'my-set-compile-command)

;;; ----------------------------------------
;;; Debugging Support
;;; ----------------------------------------
(setq gdb-many-windows t)
(global-set-key (kbd "<f9>") 'gdb)

;;; ----------------------------------------
;;; Project Management - Simple
;;; ----------------------------------------
(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'default))

;;; ----------------------------------------
;;; Helpful Key Bindings
;;; ----------------------------------------
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-c m") 'man)

;;; ----------------------------------------
;;; Window/Buffer Management
;;; ----------------------------------------
(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; ----------------------------------------
;;; Professional but Subtle Visual Polish (no emoji/icons)
;;; ----------------------------------------
;; Subtle line spacing
(setq-default line-spacing 0.15)

;; Enable pixel-precise scrolling only if available (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Crisp window borders
(window-divider-mode 1)
(setq window-divider-default-right-width 2
      window-divider-default-bottom-width 2)

;; Smooth scrolling behavior
(setq scroll-margin 4
      scroll-conservatively 101
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      fast-but-imprecise-scrolling t
      redisplay-dont-pause t)

;; Highlight current line and show a soft column guide
(global-hl-line-mode 1)
(setq display-fill-column-indicator-column 100)
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode 1)))

;; Sleek modeline without icons/emojis (keeps it lean for terminal)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 22)
  (doom-modeline-icon nil)             ;; <- no icons/emojis anywhere
  (doom-modeline-buffer-file-name-style 'truncate-with-project))

;; Frame title helpful when juggling projects
(setq frame-title-format
      '("%b — " (:eval (if (and (featurep 'projectile) (projectile-project-p))
                           (abbreviate-file-name (projectile-project-root))
                         "%f"))))

;;; ----------------------------------------
;;; Cheatsheet (curated keybindings in this config)
;;; ----------------------------------------
(defun my-config-cheatsheet ()
  "Show a curated cheatsheet of keybindings from this config.
Only lists items whose commands exist and are bound."
  (interactive)
  (let* ((items
          `(("Navigation & Windows"
             ,@(when (fboundp 'ace-window)
                 '(("M-o / C-x o" "Switch window (ace-window)")))
             (("C-x 2" "Split below")
              ("C-x 3" "Split right")
              ("C-x 0" "Delete window")
              ("C-x 1" "Delete other windows")
              ("C-x C-b" "ibuffer")))
            ("Search & Jump"
             (("C-s" "Swiper search")
              ,@(when (fboundp 'avy-goto-char-2) '(("C-;" "Avy: jump to 2 chars")))))
            ("Editing"
             ,@(when (fboundp 'smartparens-mode)
                 '(("smartparens" "Auto pairs (enabled globally)")))
             (("C-c h" "Toggle fold (hs-minor-mode)")
              ,@(when (fboundp 'mc/mark-next-like-this)
                  '(("C-> / C-<" "Multiple cursors next/prev")
                    ("C-c C->" "Multiple cursors: mark all")))))
            ("Completion"
             ,@(when (bound-and-true-p global-company-mode)
                 '(("company" "Completion (idle=0.2s; M-n/M-p cycle)"))))
            ("Project & VCS"
             ,@(when (fboundp 'projectile-command-map)
                 '(("C-c p" "Projectile commands")))
             ,@(when (fboundp 'magit-status)
                 '(("C-x g" "Magit status"))))
            ("Tags/Xref"
             (("M-." "Jump to definition")
              ("M-," "Pop mark")
              ,@(when (boundp 'ggtags-mode-map)
                  '(("C-c g s/h/r/f/c" "ggtags: sym/history/ref/file/create")))))
            ("Build & Run"
             (("<f5> / C-c c" "Compile")
              ("<f6> / C-c r" "Recompile")
              ("<f9>" "GDB (many windows)")))
            ("Terminal & Utilities"
             (("C-c t" "Open term")
              ("C-x C-r" "Recent files")
              ("C-c g" "grep-find")
              ("C-c m" "man page"))
             ,@(when (fboundp 'which-key-mode)
                 '(("which-key" "Popup key suggestions after 0.3s"))))))
         (buf (get-buffer-create "*Emacs Cheatsheet*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "Emacs Cheatsheet (from this config)\n")
      (insert "=================================\n\n")
      (dolist (group items)
        (let ((title (car group))
              (pairs (cdr group)))
          (insert (format "%s\n" title))
          (insert (make-string (length title) ?-))
          (insert "\n")
          (dolist (p pairs)
            (when (and (listp p) (= (length p) 2))
              (insert (format "  %-14s  %s\n" (car p) (cadr p)))))
          (insert "\n")))
      (goto-char (point-min))
      (view-mode 1))
    (pop-to-buffer buf)))

(global-set-key (kbd "C-c ?") #'my-config-cheatsheet)

;;; ----------------------------------------
;;; Setup Instructions (reference)
;;; ----------------------------------------
;; 1. Save this as ~/.emacs.d/init.el
;; 2. Start Emacs (packages will install automatically)
;; 3. For code navigation, run 'gtags' or 'ctags -R .' in project directories
;; 4. Create ~/.emacs.d/templates/ directory for custom templates

;;; ----------------------------------------
;;; Essential Key Bindings (reminder)
;;; ----------------------------------------
;; F5         - compile current file
;; F6         - recompile
;; F9         - start debugger (gdb)
;; M-o/C-x o  - switch window (ace-window)
;; M-.        - jump to definition
;; M-,        - go back from definition
;; C-s        - swiper search
;; C-;        - avy jump to char
;; C-> / C-<  - multiple cursors next/prev
;; C-x g      - magit status
;; C-x C-r    - recent files
;; C-c p      - projectile commands
;; C-c h      - toggle code folding
;; C-c t      - terminal
;; C-c g      - grep in project
;; C-c m      - man pages
;; C-c ?      - show this cheatsheet

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(modus-themes use-package swiper smartparens pyvenv python-mode projectile nasm-mode multiple-cursors magit helpful ggtags dired-subtree company-c-headers avy which-key vertico orderless rainbow-delimiters hl-todo doom-modeline ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
