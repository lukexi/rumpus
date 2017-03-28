# Building Rumpus on Windows

Be sure to clone with `--recursive`
`git clone --recursive https://github.com/lukexi/rumpus`

Then run
`scripts/copyLibsWin.sh` to place the OpenVR and Pd DLLs in `/usr/local/bin`

Rumpus requires GHC 8.
If you have trouble running Rumpus after building,
please use this preview build of GHC 8.0.2 which resolves a linker issue.
https://github.com/lukexi/ghc/releases/download/8.0.1.1/ghc-8.0.1-x86_64-unknown-mingw32.tar.xz

I build Rumpus in an MSYS2 environment. Here's a description of my setup:
https://gist.github.com/lukexi/e634067f1d7e3a629988

In particular you'll need SDL2, pkg-config, FreeType and GLEW which `pacman` can install:
```
pacman -S mingw64/mingw-w64-x86_64-SDL2 mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-freetype mingw64/mingw-w64-x86_64-glew
```

Now you should be able to run
```
stack build && stack exec rumpus
```

If not, please [open an issue](https://github.com/lukexi/rumpus/issues) or get in touch at luke@rumpus.land : )

# Building on Linux
```
(cd submodules/openvr-hs && git checkout linux)
sudo apt install libfreetype6-dev libsdl2-dev
sudo scripts/copyLibsLinux.sh
sudo ldconfig
```
