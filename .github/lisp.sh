#!/usr/bin/sh
sudo apt install sbcl
sbcl --version
git clone https://github.com/sbcl/sbcl $HOME/sbcl
cd $HOME/sbcl
git checkout 14b60aa20c07481b64756b4a250fe8e6eed307b2
sh ./make.sh --prefix=/usr --fancy
sudo sh ./install.sh
cd $HOME
sbcl --version
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" --eval "(exit)"
git clone https://github.com/marcoheisig/sb-simd.git $HOME/quicklisp/local-projects/sb-simd
cd $HOME/quicklisp/local-projects/sb-simd
git checkout 409111d2025e57888444fffb54d58aba198bdfe6
cd $HOME
sbcl --noinform --eval "(ql:quickload :sb-simd)" --eval "(exit)"
sbcl --version
