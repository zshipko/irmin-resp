irmin-server — A lightweight server for Irmin
-------------------------------------------------------------------------------
%%VERSION%%

irmin-server is a Redis protocol server for [Irmin](https://github.com/mirage/irmin) using [resp-server](https://github.com/zshipko/resp-server)

irmin-server is distributed under the ISC license.

Homepage: https://github.com/zshipko/irmin-server

## Installation

irmin-server can be installed with `opam`:

    opam install irmin-server

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
irmin-server`.

[doc]: https://github.com/zshipko/irmin-server

## Tests

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    jbuilder runtest
