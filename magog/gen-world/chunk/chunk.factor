! Copyright (C) 2011 Risto Saarelma

USING: arrays assocs combinators combinators.short-circuit fry kernel literals
magog.gen-world.chunks math math.vectors memoize sequences ;

IN: magog.gen-world.chunk

CONSTANT: north     { -1 -1  0 }
CONSTANT: northeast {  0 -1  0 }
CONSTANT: southeast {  1  0  0 }
CONSTANT: south     {  1  1  0 }
CONSTANT: southwest {  0  1  0 }
CONSTANT: northwest { -1  0  0 }
CONSTANT: up        {  0  0  1 }
CONSTANT: down      {  0  0 -1 }

CONSTANT: horizontal-6-dirs { $ north $ northeast $ southeast $ south
                              $ southwest $ northwest }

CONSTANT: dirs { $ northeast $ southeast $ southwest
                 $ northwest $ up $ down }

CONSTANT: starting-loc { 0 0 0 }

: opposite ( dir -- opposite ) -1 v*n ;

: north-edge ( chunk -- edge ) { 0 0 } swap at ;

: ne-edge ( chunk -- edge ) chunk-dim first iota
    [ 1 + 0 2array over at [ " " ] unless* ]
    map concat nip ;

: se-edge ( chunk -- edge ) chunk-dim second iota
    [ 1 + chunk-dim first 1 + swap 2array over at [ " " ] unless* ]
    map concat nip ;

: south-edge ( chunk -- edge ) chunk-dim { 1 1 } v+ swap at ;

: sw-edge ( chunk -- edge ) chunk-dim second iota
    [ 1 + chunk-dim second 1 + 2array over at [ " " ] unless* ]
    map concat nip ;

: nw-edge ( chunk -- edge ) chunk-dim second iota
    [ 1 + 0 swap 2array over at [ " " ] unless* ]
    map concat nip ;

: char-locs ( chunk char -- loc/f )
    '[ nip _ = ] assoc-filter keys ;

! Generate a loc array of the down-stairs or up-stairs locations. Matching
! chunks must have the corresponding stairs in the exact same pattern.

! XXX: Munging the patterns to same for now, since we actually want the up and
! down stairs to have an off-by-one relation, and can't think of a simple way
! to spec the equality relation right now.

: munge ( vert-edge -- vert-edge' ) dup empty? [ drop { 1 } ] unless ;

: up-edge ( chunk -- edge ) "y" char-locs munge ;

: down-edge ( chunk -- edge ) "z" char-locs munge ;

! Generating the edge is a bit complex and this gets called a lot, so use
! memoization here.
MEMO: edge ( chunk dir -- edge ) {
    { north [ north-edge ] }
    { northeast [ ne-edge ] }
    { southeast [ se-edge ] }
    { south [ south-edge ] }
    { southwest [ sw-edge ] }
    { northwest [ nw-edge ] }
    { up [ up-edge ] }
    { down [ down-edge ] }
  } case ;

: in-chunk? ( loc -- ? ) {
    [ { 0 0 } v>= [ ] all? ]
    [ chunk-dim v< [ ] all? ] } 1&& ;

! Remove the edge connectivity annotations from the chunk data.
: strip-annotations ( chunk -- chunk' )
    [ [ { 1 1 } v- ] dip ] assoc-map
    [ drop in-chunk? ] assoc-filter ;

! Vertical edges are represented by arrays of stair points instead of strings.
: open-vertical-edge? ( edge -- ? ) [ array? ] [ empty? not ] bi and ;

: open-horizontal-edge? ( edge -- ? ) [ CHAR: 1 = ] any? ;

: open? ( edge -- ? )
    { [ open-vertical-edge? ]
      [ open-horizontal-edge? ] } 1|| ;

: vertical-edge-open? ( chunk -- ? )
    { [ up edge open? ] [ down edge open? ] } 1|| ;
