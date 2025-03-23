A little toy [roguelike](https://en.wikipedia.org/wiki/Roguelike) to play around with Common Lisp some more:

- Explore
  [CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System) and
  generic functions
- Explore use of LLMs to generate many descriptive phrases of the same
  "terrain" in advance
- `curses`-like behavior[^1] without using the actual library (just
  use ANSI escape codes directly)

[^1]: https://en.wikipedia.org/wiki/Curses_(programming_library)

# Building

Prerequisites:

- [SBCL](https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp)
- `make`

To build the project, simply run `make` in the root directory. This will compile the project and create an executable called `curseless`.

To install in $BINDIR, run `make install`.

![curseless in action](https://github.com/eigenhombre/gifs/blob/master/gifs/curseless0.gif)
