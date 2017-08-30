This package provides a Reflex wrapper around the ACE editor.  It is very
incomplete and was derived from [code written for
hsnippet](https://github.com/mightybyte/hsnippet/blob/64cc17d2bf2bcce219f3ab8e96b7fd6071d5b56b/frontend/src/ACE.hs).
This is also intended to serve as an example of how to structure FFI packages
that rely on external JS packages.

running
-------
You can run the test app with the following (assuming you've got
`reflex-platform` at `..` and `wai-app-static` aka the `warp` binary installed
in your path):

Using one terminal, from this project's directory do:
```shell
warp -d lib
```

Then in another terminal do:
```shell
../reflex-platform/work-on ghc ./.
cabal configure
cabal repl reflex-dom-ace-exe
```

That will drop you into the repl from which you can simply:
```haskell
runDef
```

Then visit [http://localhost:8888]().
