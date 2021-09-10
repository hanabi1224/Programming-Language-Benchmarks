#!/usr/bin/sh
sudo apt install sbcl
sbcl --version
git clone https://github.com/bpecsek/sbcl.git $HOME/sbcl
cd $HOME/sbcl
sh ./make.sh --prefix=/usr --fancy
sudo sh ./install.sh
cd $HOME
sbcl --version
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" --eval "(exit)"
git clone https://github.com/marcoheisig/sb-simd.git $HOME/quicklisp/local-projects/sb-simd
sbcl --noinform --eval "(ql:quickload :sb-simd)" --eval "(exit)"
sbcl --version
