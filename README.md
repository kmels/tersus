tersus
======

a virtual filesystem to use with js/coffeescript + open source js libraries hub.

Coding style
======
 * Comment every top level function (particularly exported funtions), and provide a type signature.
 * Use Haddock syntax in the comments (see below).
 * Code should be compilable with ghc-options: -Wall -Werror set in the tersus.cabal file. There should be no warnings.
 * Code should be free of any warnings or errors from the Hlint tool; use your best judgement on some warnings like eta-reduction or bracket removal, though.
 * Partial functions should be avoided
 * Any pure function added to the core must have QuickCheck properties precisely defining its behaviour. Tests for everything else are encouraged. 

For examples of Haddock documentation syntax, have a look at other extensions. Important points are:

 * Every exported function (or even better, every function) should have a Haddock comment explaining what it does, and providing examples.
 * Literal chunks of code can be written in comments using "birdtrack" notation (a greater-than symbol at the beginning of each line). Be sure to leave a blank line before and after each birdtrack-quoted section.
 * Link to functions by surrounding the names in single quotes, modules in double quotes.
 * Literal quote marks and slashes should be escaped with a backslash. 

To generate and view the Haddock documentation for your extension, run `runhaskell Setup haddock`, for more information, see the [Haddock documentation](http://www.haskell.org/haddock/doc/html/index.html)