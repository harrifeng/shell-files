(provide 'sub-mac-path)

;;For Macos build the emacs by
;; > brew install emacs --cocoa --srge
(setenv "PATH"
	(concat
     (concat (getenv "HOME") "/.rbenv/shims") ":"
	 "/usr/local/bin" ":"
	 "/bin"           ":"
	 "/sbin"          ":"
	 "/usr"           ":"
	 "/usr/bin"       ":"
	 "/usr/sbin"      ":"
	 (getenv "PATH")
	 ))
