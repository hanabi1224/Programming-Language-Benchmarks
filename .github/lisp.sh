#!/usr/bin/sh
sudo apt install -y sbcl
sbcl --version
git clone https://github.com/sbcl/sbcl $HOME/sbcl
cd $HOME/sbcl
git checkout sbcl-2.1.10 #14b60aa20c07481b64756b4a250fe8e6eed307b2
sh ./make.sh --prefix=/usr --fancy
sudo sh ./install.sh
cd $HOME
sbcl --version
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" --eval "(exit)"
git clone https://github.com/marcoheisig/sb-simd $HOME/quicklisp/local-projects/sb-simd
cd $HOME/quicklisp/local-projects/sb-simd
#git checkout 384c19896acf46a3db80ea3a3785b344fdf1c069
cd $HOME
sbcl --noinform --eval "(ql:quickload :sb-simd)" --eval "(exit)"
sbcl --version
