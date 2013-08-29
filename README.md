# Atlas of Consonance

Tools to explore ideas from Norman Sohl's [Atlas of Consonance](http://www.sohl.com/mt/maptone.html) with Overtone and Quil.

I read Mr. Sohl's wiki and was intrigued by the diagrams and the ideas.  Perhaps you will be, too?  In order to make sense of the atlases below, please read the document.

I think it turns out to be quite interesting to hear the subtle differences between notes and chords derived in this method vs. the diatonic scale.  Warning: you may not be able to "un-hear" this.  :-)

The code is a work-in-progress, so send feedback.  I've found subtle differences in my graphs vs. his, so perhaps there are bugs.  At this point, I believe my code is correct, but there may be something wrong as I cannot see the Major Triad (C-E-G) and I would certainly expect that to be found via this method.

## Installation

Clone this repo.

## Usage

### Atlas1 - Scale Exploration

```clj
lein run 1
```

Then click in the scale to hear either the Consonance-inspired tones or the standard diatonic tuning notes.

![Screenshot](https://github.com/rogerallen/atlas_of_consonance/raw/master/doc/atlas1.png)

### Atlas2 - Chord Exploration

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
