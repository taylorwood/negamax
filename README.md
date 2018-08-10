# negamax

A demonstration of the [negamax algorithm with alpha beta pruning](https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning).

There are a lot of examples of using minimax/negamax to play Tic-tac-toe,
but I couldn't find any Clojure implementation with alpha beta pruning.
This implementation goes one step further in that it can be used for problems
other than Tic-tac-toe.

## Usage

Run with `lein run` to play an unwinnable game of Tic-tac-toe against this Clojure program.

Or load `negamax.core` namespace in a REPL and have at it. 

## License

Copyright © 2018 Taylor Wood

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
