# colourista

![logo](https://user-images.githubusercontent.com/4276606/74611761-e7d69c80-50f6-11ea-8065-c9da0371b9bf.png)

[![GitHub CI](https://github.com/kowainik/colourista/workflows/CI/badge.svg)](https://github.com/kowainik/colourista/actions)
[![Build status](https://img.shields.io/travis/kowainik/colourista.svg?logo=travis)](https://travis-ci.org/kowainik/colourista)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/kowainik/colourista?branch=master&svg=true)](https://ci.appveyor.com/project/kowainik/colourista)
[![Hackage](https://img.shields.io/hackage/v/colourista.svg?logo=haskell)](https://hackage.haskell.org/package/colourista)
[![Stackage Lts](http://stackage.org/package/colourista/badge/lts)](http://stackage.org/lts/package/colourista)
[![Stackage Nightly](http://stackage.org/package/colourista/badge/nightly)](http://stackage.org/nightly/package/colourista)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

> «The best colour in the whole world is the one that looks good on you.»
>
> — Coco Chanel

`colourista` is the library that provides a convenient interface for printing
colourful messages to the terminal. It is based on
[`ansi-terminal`](https://hackage.haskell.org/package/ansi-terminal), however,
in contradistinction to this Haskell library, `colourista` is a high-level
wrapper focused on easily achieved output modification without low-level setup.

## Interface

The two main functions that `colourista` provides are:

 * `formatWith` — the function that formats pure output by applying provided
   formatting codes. It works with polymorphic strings.
 * `formattedMessage` — the function that outputs the formatted output directly
   into the terminal (working in `IO` with `Text`).

The library also provides a set of different pure and impure helpers for the
colouring and emphasis.

## Example

![output](https://user-images.githubusercontent.com/8126674/74609327-0a5dbb00-50e1-11ea-8c4b-2db4ab5b42a2.png)

## How to use

`colourista` is compatible with the latest GHC compiler versions starting from `8.2.2`.

In order to start using `colourista` in your project, you will need to set it up with the three easy steps:

1. Add the dependency on `colorista` in your project's `.cabal` file. For this,
   you should modify the `build-depends` section by adding the name of this
   library. After the adjustment, this section could look like this:

   ```haskell
   build-depends: base ^>= 4.14
                , colourista ^>= 0.0
   ```
2. In the module where you wish to use the colourful output with `colourista`,
   you should add the import:

   ```haskell
   import Colourista (successMessage)
   ```
3. Now you can use the functions from the library:

   ```haskell
   main :: IO ()
   main = successMessage "All set up!"
   ```
### Usage with Stack

If `colourista` is not available on your current Stackage resolver yet, fear not! You can still use
it from Hackage by adding the following to the `extra-deps` section of your `stack.yaml`
file:

```yaml
extra-deps:
  - colourista-0.0.0.0
  - ansi-terminal-0.10
```

Then you can add it as a dependency in your `package.yaml` file as usual:

```yaml
library:
  dependencies:
    - colourista
```

Great!

## Acknowledgement

Icons made by [Freepik](http://www.freepik.com) from [www.flaticon.com](https://www.flaticon.com/) is licensed by [CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
