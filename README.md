# Reagent Turing Machine

This is a Turing Machine simulation written using ClojureScript and Reagent. Assuming you have Leiningen installed you can run it with `lein figwheel`.

The default program shown is shown in the instruction table and will perform unary multiplication on its ':' separated arguments 3 (111) and 2 (11) to give 6 (111111).

To write a new default program edit the map `initial-description` in `src/cljs/turing_machine/core.cljs`. Support for editing, writing and running programs in the browser is coming soon.
