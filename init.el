;; straight package manager bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Install org-mode (and compatibility hacks to work with Straight)
;; See: https://github.com/raxod502/straight.el/#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(straight-use-package 'org)

;;Load spacemacs early
;;(straight-use-package 'spacemacs-theme
;;    :ensure t )

;; Load configuration
;; TODO: Figure out how to make byte compilation work?
(let ((config-el (expand-file-name "emacs.el" user-emacs-directory))
      (config-org (expand-file-name "emacs.org" user-emacs-directory)))
  (if (and (file-exists-p config-el)
           (time-less-p (file-attribute-modification-time (file-attributes config-org))
                        (file-attribute-modification-time (file-attributes config-el))))
        (load-file config-el)
   (org-babel-load-file config-org)))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("7f225bdf56b4ae88ec391b741bd6c98982692595bf579876576370bdc4c38caf" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(deft-default-extension "org" t)
 '(deft-directory "~/org-roam/" t)
 '(deft-recursive t t)
 '(deft-use-filter-string-for-filename t t)
 '(org-journal-date-format "%y-%m-%d %a")
 '(org-journal-date-prefix "#+TITLE: ")
 '(org-journal-dir "~/org-roam/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-roam-directory "~/org-roam/")
 '(safe-local-variable-values (quote ((eval visual-line-mode t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "misc" :family "fixed")))))
