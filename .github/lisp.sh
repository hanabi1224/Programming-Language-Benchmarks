#!/usr/bin/sh
sudo apt install sbcl
sbcl --version
git clone https://github.com/sbcl/sbcl $HOME/sbcl
cd $HOME/sbcl
git checkout 77bd1bf6f8c81f9e53410c54ae659995e0c25f08
sh ./make.sh --prefix=/usr --fancy
sudo sh ./install.sh
cd $HOME
sbcl --version
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" --eval "(exit)"
git clone https://github.com/marcoheisig/sb-simd.git $HOME/quicklisp/local-projects/sb-simd
cd $HOME/quicklisp/local-projects/sb-simd
git checkout b19964199af483fe43e6dbf03313aea89f050b31 #7c432699affd21516940009bab9e05e1695eee8b
cd $HOME
sbcl --noinform --eval "(ql:quickload :sb-simd)" --eval "(exit)"
sbcl --version
