! Copyright (C) 2011 Risto Saarelma

USING: accessors arrays colors.constants combinators.short-circuit
dust.delaunay dust.quadedge kernel locals math math.ranges math.rectangles
math.vectors namespaces opengl random sequences sets ui.gadgets
ui.gadgets.panes ui.render ;

IN: dust.delaunay.vis

TUPLE: graph-gadget < gadget edges vertices ;

M: graph-gadget pref-dim* drop { 256 256 } ;

: <graph-gadget> ( edges vertices -- graph-gadget )
    graph-gadget new swap >>vertices swap >>edges t >>clipped? ;

CONSTANT: node-radius { 2 2 }

: node-color ( -- ) COLOR: blue gl-color ;

: arc-color ( -- ) COLOR: magenta gl-color ;

: vertex-color ( -- ) COLOR: DarkGreen gl-color ;

: vertex-point ( p -- p1 p2 )
    node-radius v- node-radius 2 v*n ;

: draw-arc ( p1 p2 -- )
    2dup [ vertex-point node-color gl-fill-rect ] bi@
    arc-color gl-line ;

: draw-vertex ( p -- )
    vertex-point vertex-color gl-fill-rect ;

M:: graph-gadget draw-gadget* ( gadget -- )
    COLOR: black gl-color
    gadget dim>> { 0 0 } swap gl-rect
    gadget edges>> [ first2 draw-arc ] each
    gadget vertices>> [ draw-vertex ] each ;

: delaunay. ( delaunay -- )
    edges [ [ orig ] [ dest ] bi 2array ] map f <graph-gadget> gadget. ;

:: voronoi. ( delaunay -- )
    delaunay generate-dual-vertices
    delaunay edges [ turn [ orig ] [ dest ] bi 2array ] map
    [ [ ] all? ] filter
    delaunay vertices
    <graph-gadget> gadget. ;

: random-point ( -- p )
    1 255 [a,b] random >float 1 255 [a,b] random >float 2array ;

SYMBOL: demo-point-n

: empty-delaunay ( -- delaunay )
    { 0 0 } { 256 256 } <rect> enclosing-delaunay ;

: demo-delaunay ( -- delaunay )
    { 0 0 } { 256 256 } <rect> enclosing-delaunay
    demo-point-n get [ 100 ] unless* [ random-point insert ] times ;

: valid-point? ( p -- ? )
    { [ { 1 1 } v>= [ ] all? ] [ { 255 255 } v< [ ] all? ] } 1&& ;

! XXX: Fractional points lead to breakage in the lib.
: relax ( delaunay -- delaunay )
    relaxed-vertices [ valid-point? ] filter [ [ >integer ] map ] map members
    empty-delaunay swap [ insert ] each ;

: demo ( -- )
    demo-delaunay delaunay. ;