# Building a bulletin board using Haskell, scotty and friends

Check out the [blog post](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board) for the tutorial.

## Run with


```sh
stack run
```

## Static executable

Compile a static executable using the following command:

```sh
stack build \
  --ghc-options ' -static -optl-static -optl-pthread -fPIC' \
  --docker --docker-image "utdemir/ghc-musl:v16-ghc884" \
  --no-nix
```