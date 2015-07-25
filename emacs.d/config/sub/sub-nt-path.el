(provide 'sub-nt-path)

(if (file-exists-p "c:/Devbox/")
    (defconst my-dev "c:/Devbox/")
  (defconst my-dev "d:/Devbox/"))

(defconst my-util-path
  (concat my-dev "Util/"))

(defconst my-rust-path
  (concat my-dev "Rust/bin/"))

(defconst my-go-path
  (concat my-dev "Go/bin/"))

(defconst my-python-path
  (concat my-dev "Python275/App/"))

(defconst my-python-script-path
  (concat my-python-path "Scripts/"))

(defconst my-git-path
  (concat my-dev "Git/bin/"))

(defconst my-mingw-path
  (concat my-dev "MinGW/bin/"))

(defconst my-java-home
  (concat my-dev "Java/"))

(defconst my-go-home
  (concat my-dev "Go/"))

(defconst my-java-path
  (concat my-dev "Java/bin/"))

(defconst my-graphviz-path
  (concat my-dev "Graphviz/bin/"))

(setenv "PATH"
        (concat
         my-git-path ";"
         my-python-path ";"
         my-rust-path ";"
         my-go-path ";"
         my-python-script-path ";"
         my-java-path ";"
         my-mingw-path ";"
         my-graphviz-path ";"
         my-util-path ";"
         (getenv "PATH")))

(setenv "JAVA_HOME"
        my-java-home)

(setenv "GOROOT"
        my-go-home)
