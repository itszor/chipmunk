#!/bin/sh

set -e

PREFIX=$(readlink -f `pwd`/prefix)

# Uncomment to enable distcc power
WITH_DISTCC=`which distcc`
#WITH_DISTCC=

if [ -n "$WITH_DISTCC" ]; then
  echo distcc located.
  DISTCC=distcc
  FASTER="-j 5"
else
  echo No distcc/distcc disabled.
  DISTCC=
  FASTER=
fi

echo Prefix is $PREFIX

# build basic tools
for x in libjtype asm ld sim test; do
  pushd "$x"
  make
  popd
done

maybe_mkdir ()
{
  if ! [ -e $1 ]; then
    mkdir -p $1
  fi
}

maybe_mkdir $PREFIX/bin

cp asm/chipmunk-as $PREFIX/bin
cp ld/chipmunk-ld $PREFIX/bin
cp sim/sim $PREFIX/bin/chipmunk-sim
if ! [ -e $PREFIX/bin/chipmunk-ar ]; then
  ln -s `which ar` $PREFIX/bin/chipmunk-ar
fi
if ! [ -e $PREFIX/bin/chipmunk-ranlib ]; then
  ln -s `which true` $PREFIX/bin/chipmunk-ranlib
fi

export PATH=$PREFIX/bin:$PATH

maybe_mkdir gcc-build
maybe_mkdir newlib-build

pushd gcc-build
CC="$DISTCC gcc" ../gcc/configure --target=chipmunk --prefix=$PREFIX --with-newlib \
  --enable-languages=c --with-as=$PREFIX/bin/chipmunk-as \
  --with-ld=$PREFIX/bin/chipmunk-ld && make $FASTER && make install
popd

pushd newlib-build
../newlib/configure --target=chipmunk --prefix=$PREFIX && make && make install
popd

#pushd gcc-build
#../gcc/configure --target=chipmunk --prefix=$PREFIX --with-newlib \
#  --enable-languages=c --with-as=$PREFIX/bin/chipmunk-as \
#  --with-ld=$PREFIX/bin/chipmunk-ld && make && make install
#popd
