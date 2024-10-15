#!/usr/bin/env bash
# Script to install cardano-node dependencies

case $(uname) in
    Darwin)
        BREW=$(which brew)
        if [[ -x $BREW ]]; then
            $BREW install libtool autoconf automake
        fi
        ;;
    *) ;;
esac

# install forked libsodium
# https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/install.md#installing-libsodium
git clone https://github.com/intersectmbo/libsodium
pushd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
make check
sudo make install
popd

cat >> ~/.zshrc <<EOF
export LD_LIBRARY_PATH="/usr/local/lib:\$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:\$PKG_CONFIG_PATH"
EOF

# install libsecpk256
# https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/install.md#installing-secp256k1
git clone https://github.com/bitcoin-core/secp256k1
pushd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
popd

# install libblst
# needed for recent versions of the node
# https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/install.md#installing-blst

git clone https://github.com/supranational/blst
pushd blst
git checkout v0.3.10
./build.sh
cat > libblst.pc << EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: 0.3.10
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod u=rw,go=r /usr/local/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}

popd
