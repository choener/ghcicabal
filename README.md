# ghcicabal

Provides a ghci shell with options parsed from multiple cabal files,
simplifying work on multiple packages concurrently.

# Workflow

This tool is mostly designed to be used together with ```NixOs``` and
```shellFor```. It accepts a list of directories at the command line, parses
all cabal files found in sub-directories, and provides a ghci with the correct
extensions and search paths set.

Let's assume you have two directories with Haskell packages ```./Lib``` and
```./Work``` and you want to develop both simultaneously. ```ghci``` can do
this in principle, just load it up with
    ghci -i./Lib -i./Work
and everything should work...

unless the ```cabal``` file has all the extensions used, as well as the real
library paths.

    ghcicabal ./Lib ./Work

will then parse the ```cabal``` files and provide an environment for working
with all modules simultaneously.



### connection to ```snack``` by ```nmattia```.

This is a poor-man's version of snack ghci for a very narrow use-case.
