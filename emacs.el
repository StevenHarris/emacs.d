(tool-bar-mode -1) 
(menu-bar-mode -1)
(scroll-bar-mode -1)

(when window-system (global-hl-line-mode t))

(line-number-mode 1)
(column-number-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/emacs.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun kill-whole-word ()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c w w") 'kill-whole-word)

(setq display-time-24hr-format t)
(display-time-mode 1)

(use-package diminish
 :ensure t)
(diminish 'eldoc-mode)

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode 1)))

(use-package sudo-edit
  :ensure t
  :bind ("s-e" . sudo-edit))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "s-<return>") 'ansi-term)

(use-package hungry-delete
 :ensure t
 :diminish hungry-delete-mode
 :config(global-hungry-delete-mode))

(setq electric-pair-pairs '(
			    (?\( .?\))
			    (?\[ .?\])
			    (?\{ .?\})
			    ))
(electric-pair-mode t)

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(use-package smex
      :ensure t
      :init (smex-initialize)
      :bind 
	 (("M-x" . smex )  ;; use smex every time we M-x
	  ("M-X" . smex-major-mode-commands) ;; lists only commands relevant to the major mode we are using
	  ("C-c C-c M-x" . execute-extended-command)));; this last one is supposed to supply the standard M-x command but thtas not working

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'ibuffer)

(use-package avy
  :ensure t
  :bind ("M-s". avy-goto-char))

(defun copy-whole-line ()
  (interactive)
  (save-excursion
    ;; saves point and restores it at end of body
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "C-c w l") 'copy-whole-line)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
;;(global-set-key (kbd "C-M-s-k" 'kill-all-buffers))

(use-package spacemacs-theme
    :defer t 
    :init (load-theme 'spacemacs-dark t))

(use-package spaceline
  :ensure t
  :config
   (require 'spaceline-config)
   (setq spaceline-buffer-encoding-abbrev-p nil)
   (setq spaceline-line-column-p 1)
   (setq spaceline-line-p nil)
   (setq powerline-default-separator (quote arrow))
   (spaceline-spacemacs-theme))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 10)
			(bookmarks . 5)
			(agenda . 5)
			(registers . 5)))
  (setq dashboard-banner-logo-title "Steven's Tailored Emacs Config"))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq org-src-window-setup 'current-window)

(use-package org-roam
  :hook 
  (after-init . org-roam-mode)
  :straight (:host github :repo "jethrokuan/org-roam")
  :diminish org-roam-mode
  :custom
  (org-roam-directory "~/org-roam/")
  :bind (:map org-roam-mode-map
	      (("C-c m l" . org-roam)
	       ("C-c m f" . org-roam-find-file)
	       ("C-c m g" . org-roam-show-graph))
	      :map org-mode-map
	      (("C-c m i" . org-roam-insert))))
(setq org-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
	"%?"
	:file-name "%<%Y%m%d%H%M%S>-${slug}"
	:head "#+TITLE: ${title}\n"
	:unnarrowed t)))

(use-package org-journal
  :bind
  ("C-c m j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/org-roam/")
  (org-journal-date-format "%y-%m-%d %a"))

(use-package el-patch
  :straight (:host github
		   :repo "raxod502/el-patch"
		   :branch "develop"))
(eval-when-compile
  (require 'el-patch))
(use-package deft
  :after org
  :bind
  ("C-c m d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org-roam/")
  ;; same as above...
  :config/el-patch
  (defun deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (el-patch-swap (if deft-use-filename-as-title
		       (deft-base-filename file)
		     (let ((begin (string-match "^.+$" contents)))
		       (if begin
			   (funcall deft-parse-title-function
				    (substring contents begin (match-end 0))))))
		   (org-roam--get-title-or-slug file))))

(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

(use-package yasnippet)
  :ensure t
  :config (use-package yasnippet-snippets
	    :ensure t)
(yas-reload-all)

(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(use-package gnu-apl-mode)
