#!/usr/bin/sh
sudo apt install -y sbcl
sbcl --version
git clone https://github.com/sbcl/sbcl $HOME/sbcl
cd $HOME/sbcl
git checkout sbcl-2.1.10
sh ./make.sh --prefix=/usr --fancy
sudo sh ./install.sh
cd $HOME
sbcl --version
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" --eval "(exit)"
git clone https://github.com/marcoheisig/sb-simd $HOME/quicklisp/local-projects/sb-simd
cd $HOME/quicklisp/local-projects/sb-simd
git checkout a784fe831792764e88368656fa7fd643e33b712b
cd $HOME
sbcl --noinform --eval "(ql:quickload :sb-simd)" --eval "(exit)"
sbcl --version
