# Atlas of Consonance

Tools to explore ideas from Norman Sohl's [Atlas of Consonance](http://www.sohl.com/mt/maptone.html) with Overtone and Quil.

## Installation

Clone this repo.

## Usage

```clj
lein run 1
```

Then click in the scale to hear either the Consonance-inspired tones or the standard diatonic tuning notes.

![Screenshot](https://github.com/rogerallen/atlas_of_consonance/raw/master/doc/atlas1.png)

```clj
lein run 2
```

Then click in the grid to hear either the Consonance-inspired chords or the standard diatonic tuning chords.
Use the shift key to select the chord-type.

![Screenshot](https://github.com/rogerallen/atlas_of_consonance/raw/master/doc/atlas2.png)

or use your favorite repl for more control like this

```clj
user> (ns atlas-of-consonance.atlas1)            ; or atlas2
nil
atlas-of-consonance.atlas1> (run)                ; to bring up the window
#'atlas-of-consonance.atlas1/doodle
atlas-of-consonance.atlas1> (set-num-octaves 50) ; to see more overtones
nil
atlas-of-consonance.atlas1> (o/stop)             ; if a note goes wonky
```

## Options

None yet.

## License

Copyright © 2013 Roger Allen

Distributed under the Eclipse Public License, the same as Clojure.
