#!/bin/sh

run() {
    echo -e "$1_heatmap_indexes = [" >> partial.hs
    convert $1.png txt:- | sed -r -e '1d' -e 's/^[0-9]+,[0-9]+: \(([0-9]+),.*$/    \1,/g' - >> partial.hs
    echo -e "    ]\n\n\n" >> partial.hs
}

echo -e "-------------------\n-- Generated heatmap tables\n\n" > partial.hs
run "bishop"
run "king"
run "horse"
run "rock"
run "pawn"
