#!/bin/sh
packages="gl-pal
bullet-mini
vr-pal
glfw-pal
halive
animation-pal
linear-extra
lens-extra
openvr-hs
pd-hs
bindings-GLFW
freetype-gl-mini
text-gl
extensible-ecs"

cd ..
for i in $packages; do
    git clone --recursive git@github.com:lukexi/$i
done

cd rumpus

# Copy necessary DLLs to /usr/local/bin
(cd ../openvr-hs && ./copyLibWin.sh)
(cd ../pd-hs && ./copyLibWin.sh)
