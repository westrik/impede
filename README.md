# impede

[![Build Status](https://travis-ci.com/westrik/impede.svg?token=zgPt6dWpztphELfxtxey&branch=master)](https://travis-ci.com/westrik/impede)

A distributed<sup>\*</sup> functional physically-based<sup>\*</sup> renderer.

<sub><sup>\*</sup> [workin' on it](TODO.md)</sub>

## do the thing
```
brew install llvm@5
# GHC wants LLVM stuff to be in /usr/bin/
sudo ln -s /usr/local/Cellar/llvm@5/5.0.2/bin/llc /usr/bin/llc
sudo ln -s /usr/local/Cellar/llvm@5/5.0.2/bin/opt /usr/bin/opt 
stack build
stack exec impede test.png
```

## project goals

- Practice writing fast numerical Haskell (performance optimization, parallelism, SIMD primitives)
- Learn stuff about math and physically-based rendering
- Play around with Kubernetes/Docker, gRPC, and Elm (distributed rendering with web preview)