;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Antonio Juan Querol Giner"
      user-mail-address "antoniojuan.querolginer@telefonica.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Iosevka" :size 32 :weight 'thin)
      doom-variable-pitch-font (font-spec :family "Iosevka") ; inherits `doom-font''s :size
      doom-unicode-font (font-spec :family "Symbola" :size 32)
      doom-big-font (font-spec :family "Iosevka" :size 34))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(use-package! god-mode
  :init
  (require 'god-mode-isearch)
  :bind
  (("<escape>" . god-local-mode)
   :map god-local-mode-map
   ("o" . insert-one-character)
   ("O" . spacer)
   ("z" . repeat)
   ("i" . god-local-mode)
   :map isearch-mode-map
   ("<escape>" . god-mode-isearch-activate)
   :map god-mode-isearch-map
   ("<escape>" . god-mode-isearch-disable))
   :config
   (defun insert-one-character (times) (interactive "p") (dotimes (x times) (quoted-insert 1)))
   (defun spacer (times)  (interactive "p") (dotimes (x times) (insert " ")))
   (setq god-exempt-major-modes '(dired-mode
                                  grep-mode
                                  vc-annotate-mode
                                  eshell-mode
                                  shell-mode
                                  term-mode
                                  neotree-mode
                                  kite-mini-console-mode
                                  inf-ruby
                                  browse-kill-ring-mode
                                  undo-tree-visualizer
                                 info-mode))
   (setq god-exempt-predicates nil))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Abbrew math symbols completion
(setq abbrev-file-name "~/.doom.d/abbrev_defs")

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(after! undo-tree
  (setq undo-tree-auto-save-history nil))

(use-package! undo-tree
  :init
  (add-hook 'prog-mode-hook 'undo-tree-mode))

(setq eval-expression-print-maximum-character #x10fffff)
(setq inf-ruby-default-implementation "pry")


(use-package! elmacro
  :config
  (elmacro-mode))

(map! :leader
      (:prefix ("¡" . "Eval things")
       :desc "Execute shell command" "s" #'shell-command
       :desc "Eval Expression" "e" #'eval-expression
       (:prefix ("a" . "async")
        :desc "Execute async shell command" "s" #'async-shell-command)))


;; to ~/.doom.d/config.el
(use-package! kubernetes-el
  :commands (kubernetes-overview)
  :init
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package! bm
         :ensure t
         :demand t
         )

(add-hook 'emacs-lisp-mode-hook #'nameless-mode)

(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(setq feature-default-language "python")

(after! rustic
  (setq lsp-rust-server 'rust-analyzer))

;;(load! "my-doctor")
(load! "my-java-lsp")
(load! "ox-jira.el")
