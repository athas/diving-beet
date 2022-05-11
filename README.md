# Diving Beet

Diving Beet is a port of [Falling
Turnip](https://github.com/tranma/falling-turnip) from Haskell to
[Futhark](https://futhark-lang.org) and Go.  It is a physics imitation
that uses cellular automata to implement fascimiles of gravity and
(al)chemical interactions between various elements.  For example,
water plus fire gives steam, lava melts sand and metal (the latter at
a slower rate), plants grow into water, etc.

You can look at a [video of it in
action](http://sigkill.dk/junk/diving-beet.webm) - note that the
choppiness (especially near the end) is due to the screen recorder not
being able to keep up (it runs out of memory and starts writing to
disk).

## Usage

```
$ make
$ ./diving-beet
```

### Controls

| Action | Consequence |
| --- | --- |
| Left-click | Insert particles |
| Right-click | Delete particles |
| Scroll | Change modification radius |
| Ctrl+Scroll | Zoom |
| Ctrl+drag | Move |
| PageUp/PageDown | Select particle to insert |
| Space | Single-step |
| Return | Unpause |

## Using the GPU

Set the environment variable `FUTHARK_BACKEND` to `opencl` or `cuda`
before compiling.
