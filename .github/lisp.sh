#!/usr/bin/sh

export LISP=sbcl-bin
echo '#!/usr/bin/sh' >./sbcl
echo 'ros run -- $@' >>./sbcl
cat ./sbcl
sudo chmod +x ./sbcl
curl -L https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh
sudo ros install sbcl-bin
sudo ros use sbcl-bin
ros version
sbcl --version
sudo mv ./sbcl /usr/bin
git clone git://git.code.sf.net/p/sbcl/sbcl $HOME/sbcl-2.1.8
cd $HOME/sbcl-2.1.8
sh ./make.sh --prefix=$HOME --fancy
sh ./install.sh
cd $HOME
sudo rm /usr/bin/sbcl
export $PATH=$HOME/bin:$PATH
sudo cp $HOME/bin/sbcl /usr/bin
sbcl --version
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --noinform --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" --eval "(exit)"
git clone https://github.com/marcoheisig/sb-simd.git $HOME/quicklisp/local-projects/sb-simd
git clone https://github.com/marcoheisig/sb-simd.git $HOME/.roswell/local-projects/sb-simd
sbcl --noinform --eval "(ql:quickload :sb-simd)" --eval "(exit)"
