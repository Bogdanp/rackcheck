# rackcheck

A property-based testing library for Racket with support for shrinking.

## Setup

    raco pkg install rackcheck

## Usage

See the `examples` directory.

## What about [quickcheck]?

I initially started out by forking the existing [quickcheck] library
to add support for shrinking, but that required making many breaking
changes so I figured I'd start from scratch instead.

## License

    rackcheck is licensed under the 3-Clause BSD license.

[quickcheck]: https://docs.racket-lang.org/quickcheck/index.html?q=quickcheck
