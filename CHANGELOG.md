# Changelog

`colourista` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.2.0.0

* [#26](https://github.com/kowainik/colourista/issues/26):
  Support enabling and disable of colouring with implicit parameters.

  __Migration guide:__ You can continue using `colourista` without
  changing anything and you still get colourful formatting.

  However, if you want to enable or disable colouring, you need to add
  the `HasColourMode =>` constraint to all functions that format data
  or call such formatting functions, and set the value of the
  `?colourMode` variable in the beginning of your application.

## 0.1.0.0 — May 2, 2020 🌈

* [#22](https://github.com/kowainik/colourista/issues/22):
  Support GHC-8.10.
* [#27](https://github.com/kowainik/colourista/issues/27):
  Add unicode indicators to `*Message` functions.
* [#17](https://github.com/kowainik/colourista/issues/17):
  Add `indent` formatting function.
* [#7](https://github.com/kowainik/colourista/issues/7):
  Add `underline`, `doubleUnderline`, `noUnderline` formatting functions.
* [#28](https://github.com/kowainik/colourista/issues/28):
  Add `Colourista.Short` with short aliases for `bold`, `italic` and
  `underline`.

## 0.0.0.0 — Feb 16, 2020 🌈

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/colourista/releases
