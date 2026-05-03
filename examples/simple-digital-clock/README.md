About
=====

Despite the name this is a complete Wayland client for displaying date
and time.

**Key Features**:

- **Protocol Coverage**: Implements `wl_shm`, `wl_shm_pool`, and
    `wl_buffer` for graphics, alongside `wl_callback` for
    frame-synchronized updates.

- **Window Management**: Utilizes `xdg-shell` for toplevel surfaces
    and `xdg-decoration` for negotiating Server-Side Decorations
    (SSD), ensuring compatibility across `KDE Plasma`, `labwc`, and
    `sway`.

- **Efficient Event Loop**: Uses conditional blocking with GHC’s
    `threadWaitRead`. This ensures the client remains dormant (0% CPU)
    when idle and responsive during resizes.

- **Flicker-Free Rendering**: Uses a double-buffering strategy with
    `gi-cairo-render` to provide smooth, tear-free updates.

The code is thoroughly commented. It also exposes a library to
generate `Haddock` documentation.

Building
========

Use `hws -c hs-wayland-scanner.cfg` to generate the bindings:

``` shell
cd examples/simple-digital-clock
/path/to/hws -c hs-wayland-scanner.cfg
```

Alternatively run:

``` shell
/path/to/hws -p ./  protocol/wayland.xml protocol/xdg-shell.xml protocol/xdg-decoration-unstable-v1.xml
```

Now you can run, build or install it with `cabal`:

``` shell
cabal [run|build|install]
```

Run with:

``` shell
/path/to/simple-digital-clock
```

or, if `WAYLAND_DISPLAY` is not set:

``` shell
WAYLAND_DISPLAY="wayland-0" /path/to/simple-digital-clock
```

Documentation
=============

To produce the documentation run:

``` shell
cabal haddock
```
