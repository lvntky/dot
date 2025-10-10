;;; Enhanced Minimal Emacs Configuration for C/C++/Assembly/Python
;;; Simple, fast, Linux style (tabs=8, 80 cols), NO auto-indentation

;;; ----------------------------------------
;;; Performance
;;; ----------------------------------------
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;;; ----------------------------------------
;;; Package Management
;;; ----------------------------------------
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(unless package--initialized (package-initialize))

;; Refresh once if archive is empty
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; ----------------------------------------
;;; Compatibility flags
;;; ----------------------------------------
(defconst my/emacs-29+ (not (version< emacs-version "29.1"))
  "Non-nil when Emacs is 29.1 or newer.")

;;; ----------------------------------------
;;; Basic Settings
;;; ----------------------------------------
(setq inhibit-startup-message t
      ring-bell-function 'ignore
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t))
      require-final-newline t
      scroll-conservatively 10000
      byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Linux style (but no auto-indent)
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)
(setq-default fill-column 80)

;; Absolutely disable auto-indentation
(when (boundp 'electric-indent-mode)
  (electric-indent-mode -1))
(setq-default electric-indent-inhibit t)

;; Prompts & UI
(defalias 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.1)
(setq split-height-threshold nil
      split-width-threshold 160)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(delete-selection-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Font (Martian Mono if available)
(when (find-font (font-spec :name "Martian Mono"))
  (set-face-attribute 'default nil :font "Martian Mono" :height 160))

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 50
      recentf-max-saved-items 50)

;;; ----------------------------------------
;;; Theme
;;; ----------------------------------------
(use-package modus-themes
  :config (load-theme 'modus-vivendi t))

;;; ----------------------------------------
;;; Completion / Minibuffer (version-aware)
;;; ----------------------------------------
;; Save minibuffer history
(use-package savehist :ensure nil :init (savehist-mode 1))

;; On Emacs 29+: Vertico + Orderless + Consult + Embark
(when my/emacs-29+
  (use-package vertico :init (vertico-mode 1)
    :config (setq vertico-cycle t))
  (use-package orderless
    :config
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles basic partial-completion)))))
  (use-package consult
    :bind (("C-x b"   . consult-buffer)
           ("C-x 4 b" . consult-buffer-other-window)
           ("C-x r b" . consult-bookmark)
           ("M-y"     . consult-yank-pop)
           ("M-g g"   . consult-goto-line)
           ("M-g M-g" . consult-goto-line)
           ("M-s l"   . consult-line)
           ("M-s r"   . consult-ripgrep)
           ("C-x C-r" . consult-recent-file)
           ("C-c g"   . consult-ripgrep))
    :config (setq consult-narrow-key "<"
                  consult-line-start-from-top t))
  (use-package embark
    :bind (("C-." . embark-act)
           ("C-," . embark-dwim)
           ("C-h B" . embark-bindings))
    :config (setq prefix-help-command #'embark-prefix-help-command))
  (use-package embark-consult :after (embark consult)))

;; On Emacs < 29.1: built-in Fido/icomplete + basic fallbacks
(unless my/emacs-29+
  ;; Fido vertical is a great, zero-deps fallback
  (setq icomplete-compute-delay 0
        icomplete-max-matches 50)
  (fido-vertical-mode 1)

  ;; Provide "good enough" bindings where Consult would be
  (global-set-key (kbd "C-x b")   #'switch-to-buffer)
  (global-set-key (kbd "C-x 4 b") #'switch-to-buffer-other-window)
  (global-set-key (kbd "C-x r b") #'bookmark-jump)
  (global-set-key (kbd "M-y")     #'yank-pop)
  (global-set-key (kbd "M-g g")   #'goto-line)
  (global-set-key (kbd "M-g M-g") #'goto-line)
  (global-set-key (kbd "M-s l")   #'isearch-forward)
  (global-set-key (kbd "M-s r")   #'rgrep)
  (global-set-key (kbd "C-x C-r") #'recentf-open-files)
  (global-set-key (kbd "C-.")     #'execute-extended-command))

;;; ----------------------------------------
;;; Which-key
;;; ----------------------------------------
(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.5
                which-key-min-display-lines 5))

;;; ----------------------------------------
;;; Navigation
;;; ----------------------------------------
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line))
  :config (setq avy-timeout-seconds 0.3
                avy-all-windows nil))

;;; ----------------------------------------
;;; Window Management
;;; ----------------------------------------
(use-package ace-window
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window))
  :config (setq aw-ignore-current t
                aw-scope 'frame
                aw-dispatch-always t))

(winner-mode 1)
(global-set-key (kbd "C-c <left>")  'winner-undo)
(global-set-key (kbd "C-c <right>") 'winner-redo)
(windmove-default-keybindings)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; ----------------------------------------
;;; Undo System
;;; ----------------------------------------
(use-package undo-tree
  :init (global-undo-tree-mode)
  :config (setq undo-tree-auto-save-history nil)
  :bind ("C-x u" . undo-tree-visualize))

;;; ----------------------------------------
;;; Code Completion
;;; ----------------------------------------
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-align-annotations t))

(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

;;; ----------------------------------------
;;; Languages / Modes
;;; ----------------------------------------
(use-package cc-mode :ensure nil
  :config
  ;; Keep Linux style vars but DO NOT auto-indent
  (setq c-default-style "linux"
        c-basic-offset 8)
  ;; Make sure RET doesn’t reindent automatically
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq-local electric-indent-inhibit t)
              (electric-indent-local-mode -1))))

(use-package nasm-mode
  :mode "\\.\\(asm\\|nasm\\|s\\)\\'")

;; FASM (simple loader if present)
(let ((fasm-dir (expand-file-name "fasm-mode" user-emacs-directory)))
  (unless (file-directory-p fasm-dir)
    (ignore-errors
      (shell-command (format "git clone https://github.com/the-little-language-designer/fasm-mode.git %s" fasm-dir))))
  (add-to-list 'load-path fasm-dir)
  (autoload 'fasm-mode "fasm-mode" "Major mode for FASM" t)
  (add-to-list 'auto-mode-alist '("\\.\\(fasm\\|inc\\)\\'" . fasm-mode)))

(use-package python-mode
  :mode "\\.py\\'"
  :config (setq python-indent-offset 4))

(use-package pyvenv :config (pyvenv-mode 1))

;; Makefile defaults
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  show-trailing-whitespace t)))

;;; ----------------------------------------
;;; Tags / Xref
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
;;; Editing Aids (NO auto-indent)
;;; ----------------------------------------
(use-package smartparens :config (smartparens-global-mode 1))

;; Folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c h") 'hs-toggle-hiding)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;; NO aggressive-indent at all (removed)
;; (use-package aggressive-indent ...)  ; intentionally omitted

(use-package ws-butler :hook (prog-mode . ws-butler-mode))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c k" . crux-kill-other-buffers)))

;;; ----------------------------------------
;;; VCS
;;; ----------------------------------------
(use-package magit :bind ("C-x g" . magit-status))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (with-eval-after-load 'diff-hl
    (defvar vc-svn-diff-switches nil)
    (defvar vc-fossil-diff-switches nil)
    (defvar vc-jj-diff-switches nil)))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

;;; ----------------------------------------
;;; Help
;;; ----------------------------------------
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;;; ----------------------------------------
;;; Terminal
;;; ----------------------------------------
(global-set-key (kbd "C-c t") 'term)
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode 0)))

;;; ----------------------------------------
;;; Auto Insert Templates
;;; ----------------------------------------
(use-package autoinsert :ensure nil
  :init (auto-insert-mode 1)
  :config
  (setq auto-insert-directory "~/.emacs.d/templates/"
        auto-insert-query nil)

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

  (define-auto-insert "\\.c\\'"
    '("C source: "
      "/* " (file-name-nondirectory buffer-file-name) " - " (read-string "Description: ") " */\n\n"
      "#include \"" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".h\"\n\n"
      _))

  (define-auto-insert "\\.cpp\\'"
    '("C++ source: "
      "/* " (file-name-nondirectory buffer-file-name) " - " (read-string "Description: ") " */\n\n"
      "#include \"" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".h\"\n\n"
      _))

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
(setq compilation-scroll-output t
      compilation-window-height 12)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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
(defun my-set-compile-command () (set (make-local-variable 'compile-command) (my-compile-command)))
(dolist (hook '(c-mode-hook c++-mode-hook python-mode-hook nasm-mode-hook fasm-mode-hook))
  (add-hook hook 'my-set-compile-command))

;;; ----------------------------------------
;;; Debugging
;;; ----------------------------------------
(setq gdb-many-windows t)
(global-set-key (kbd "<f9>") 'gdb)

;;; ----------------------------------------
;;; Projects
;;; ----------------------------------------
(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-c f" . projectile-find-file)
         ("C-c s" . projectile-switch-project))
  :config
  (setq projectile-completion-system 'default
        projectile-enable-caching t
        projectile-indexing-method 'alien))

;;; ----------------------------------------
;;; Visual Polish (minimal)
;;; ----------------------------------------
(setq-default line-spacing 0.15)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(window-divider-mode 1)
(setq window-divider-default-right-width 2
      window-divider-default-bottom-width 2)

(setq scroll-margin 8
      scroll-conservatively 101
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      fast-but-imprecise-scrolling t
      redisplay-dont-pause t)

(global-hl-line-mode 1)
(setq display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook (lambda () (display-fill-column-indicator-mode 1)))

(setq lazy-highlight-initial-delay 0
      lazy-highlight-max-at-a-time nil)

(setq frame-title-format
      '("%b – " (:eval (if (and (featurep 'projectile) (projectile-project-p))
                           (abbreviate-file-name (projectile-project-root))
                         "Emacs"))))

;;; ----------------------------------------
;;; Buffer Cycling / Keys
;;; ----------------------------------------
(global-set-key (kbd "C-c b") 'next-buffer)
(global-set-key (kbd "C-c B") 'previous-buffer)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c m") 'man)

;;; ----------------------------------------
;;; Expand Region
;;; ----------------------------------------
(use-package expand-region :bind ("C-=" . er/expand-region))

;;; ----------------------------------------
;;; Cheatsheet
;;; ----------------------------------------
(defun my-config-cheatsheet ()
  "Show a curated cheatsheet of keybindings from this config."
  (interactive)
  (let* ((items
          `(("Navigation & Windows"
             (("M-o / C-x o" "Switch window (ace-window)")
              ("C-x 2 / 3" "Split below/right")
              ("C-x 0 / 1" "Delete window/others")
              ("C-c arrows" "Windmove or winner undo/redo")
              ("C-x C-b" "ibuffer")))
            ("Search & Jump"
             (("C-s" "Isearch (or M-s l)")
              ("C-: / C-'" "Avy jump char/2-char")
              ("M-g w / l" "Avy jump word/line")
              ("M-s r" "Ripgrep (or rgrep)")))
            ("Editing"
             (("C-a" "Smart bol")
              ("C-=" "Expand region")
              ("C-c h" "Toggle fold")
              ("C-c d" "Duplicate line/region")
              ("C-c D" "Delete file+buffer")
              ("C-c r" "Rename file+buffer")
              ("C-> / C-<" "Multiple cursors")
              ("C-x u" "Undo-tree graph")))
            ("Completion & Minibuffer"
             (("C-x b" "Switch buffer")
              ("C-x C-r" "Recent files")
              ("M-y" "Yank ring")
              ("C-." "Actions (Embark or M-x)")
              ("M-g g" "Goto line")))
            ("Project & VCS"
             (("C-c p" "Projectile")
              ("C-c f" "Find file in project")
              ("C-c s" "Switch project")
              ("C-x g" "Magit status")))
            ("Tags/Xref"
             (("M-." "Jump to def")
              ("M-," "Pop mark")
              ("C-c g s/h/r/f/c" "ggtags")))
            ("Build & Run"
             (("<f5>/C-c c" "Compile")
              ("<f6>" "Recompile")
              ("<f9>" "GDB many windows")))
            ("Terminal & Utilities"
             (("C-c t" "term")
              ("C-c m" "man")
              ("C-c b/B" "Next/prev buffer")
              ("C-c k" "Kill other buffers")))))
         (buf (get-buffer-create "*Emacs Cheatsheet*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "Emacs Cheatsheet (Linux Style, no auto-indent)\n")
      (insert "===========================================\n\n")
      (dolist (group items)
        (let ((title (car group))
              (pairs (cdr group)))
          (insert (format "%s\n" title))
          (insert (make-string (length title) ?-))
          (insert "\n")
          (dolist (p pairs)
            (when (and (listp p) (= (length p) 2))
              (insert (format "  %-18s  %s\n" (car p) (cadr p)))))
          (insert "\n")))
      (insert "Style: Tabs=8, 80 columns. Auto-indent disabled.\n")
      (goto-char (point-min))
      (view-mode 1))
    (pop-to-buffer buf)))

(global-set-key (kbd "C-c ?") #'my-config-cheatsheet)

;;; ----------------------------------------
;;; Custom (generated)
;;; ----------------------------------------
(custom-set-variables
 '(package-selected-packages
   '(expand-region crux ws-butler diff-hl projectile helpful magit multiple-cursors
		   smartparens ggtags dired-subtree company-c-headers company
		   which-key avy undo-tree modus-themes
		   ;; The following will only be present on Emacs 29+: vertico orderless consult embark embark-consult
		   )))
(custom-set-faces)
