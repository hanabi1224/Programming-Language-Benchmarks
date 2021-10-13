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
git checkout f2abc3f5c88322333a6bb648427b498a922ce54e
cd $HOME
sbcl --noinform --eval "(ql:quickload :sb-simd)" --eval "(exit)"
sbcl --version
