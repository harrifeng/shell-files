(provide 'el-setting)

(package-initialize)
;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
 '(
   ag
   ;; auto-complete
   bm
   column-marker
   dash-at-point
   dockerfile-mode
   go-mode
   grep-a-lot
   groovy-mode
   git-gutter
   helm
   helm-ag
   helm-projectile
   highlight-indentation
   highlight-symbol
   htmlize
   inf-ruby
   js2-mode
   magit
   markdown-mode
   multi-term
   multiple-cursors
   projectile
   restclient
   rvm
   rust-mode
   scss-mode
   slime
   smart-mode-line-powerline-theme
   solarized-theme
   sublime-themes
   toml-mode
   web-mode
   yasnippet dropdown-list
   zen-and-art-theme
   ))

;; [A]g------------------------------------------------------------------->>
(setq ag-highlight-search t)

;; [A]uto-complete-------------------------------------------------------->>
;; (require 'auto-complete-config)
;; (ac-config-default)

;; [B]m-toogle------------------------------------------------------------>>
(require 'bm)
(global-set-key (kbd "C-x m")        'bm-toggle)

;; [G]rep-a-lot----------------------------------------------------------->>
(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;; [G]it-gutter----------------------------------------------------------->>
(require 'git-gutter)
(global-git-gutter-mode +1)

;; [H]elm-alike-plugins--------------------------------------------------->>
(require 'helm)
(require 'helm-projectile)
(helm-projectile-on)

(global-set-key (kbd "C-;")          'helm-toggle-visible-mark)
(global-set-key (kbd "C-x C-r")      'helm-recentf)
(global-set-key (kbd "C-x b")        'helm-mini)
(global-set-key (kbd "C-M-;")        'helm-projectile)
(global-set-key (kbd "C-M-y")        'helm-show-kill-ring)
(global-set-key (kbd "C-c C-m")      'helm-M-x)
(global-set-key (kbd "C-x C-m")      'helm-M-x)

;; Only works for helm-ag, not for helm-do-ag
(setq helm-ag-command-option "--all-text")
(setq helm-ag-source-type 'file-line)

;; [H]ighlight-indentation------------------------------------------------>>
(require 'highlight-indentation)

(add-hook 'ruby-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

(add-hook 'python-mode-hook
          (lambda () (highlight-indentation-current-column-mode)))

;; [H]ighlight-symbol----------------------------------------------------->>
(setq highlight-symbol-mode t)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; To customize the background color
;(set-face-background 'highline-face "#333")

;; [I]nf-ruby------------------------------------------------------------->>
;; ruby shell mode

;; [M]agit---------------------------------------------------------------->>
(global-set-key (kbd "C-M-i")        'magit-status)

;; [M]ulti-term----------------------------------------------------------->>
(require 'multi-term)

;; [P]rojectile----------------------------------------------------------->>
(projectile-global-mode)

;; to enable caching unconditionally
;; (setq projectile-enable-caching t)

;; To disable remote file exists cache that use this snippet of code:
;; (setq projectile-file-exists-remote-cache-expire nil)

;; [R]estclient]---------------------------------------------------------->>
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http?\\'" . restclient-mode))

;; [S]mart-mode-line-powerline-theme-------------------------------------->>
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)
;; (sml/apply-theme 'powerline)

;; [S]olarized-thems------------------------------------------------------>>
;; (load-theme 'solarized-dark t)
;; [S]ublime-themes------------------------------------------------------->>
;; (load-theme 'hickey t)
;; (load-theme 'wheatgrass t)

;; [W]eb-mode------------------------------------------------------------->>
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; [Y]asnippet------------------------------------------------------------>>
(require 'yasnippet)
(yas-global-mode 1)

(setq yas-snippet-dirs
      '(
        (concat my-emacs-path "snippets")
        ))
;; dropdown-list is needed by yasnippet
(require 'dropdown-list)
(setq yas-prompt-functions
      '(yas/dropdown-prompt
        yas/ido-prompt
        yas/x-prompt
        yas/completing-prompt
        yas/no-prompt))
;; [Z]en-and-art-theme---------------------------------------------------->>
(load-theme 'zen-and-art t)

(require 'scss-mode)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))



(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; From now on, we use Ctrl + ; to expand the yasnippet
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-;") 'yas-expand)

;; slime && slime-helper
;; installation: sbcl --load quicklisp.lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; sbcl should be in the path
(setq inferior-lisp-program "sbcl")
