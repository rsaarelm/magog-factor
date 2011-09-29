! Copyright (C) 2011 Risto Saarelma

USING: accessors arrays colors.constants dust.delaunay kernel locals math
math.ranges math.rectangles math.vectors opengl random sequences ui.gadgets
ui.gadgets.panes ui.render ;

IN: dust.delaunay.vis

TUPLE: delaunay-gadget < gadget delaunay bounding-rect ;

M: delaunay-gadget pref-dim* drop { 256 256 } ;

CONSTANT: node-radius { 2 2 }

: node-color ( -- ) COLOR: blue gl-color ;

: arc-color ( -- ) COLOR: magenta gl-color ;

: draw-arc ( p1 p2 dim -- )
    [ v* ] curry bi@
    2dup [ node-radius v- node-radius 2 v*n node-color gl-fill-rect ] bi@
    arc-color gl-line ;

M:: delaunay-gadget draw-gadget* ( gadget -- )
    COLOR: black gl-color
    gadget dim>> { 0 0 } swap gl-rect
    gadget delaunay>> edges
    [ first2 { 1 1 } draw-arc ] each
    ;

: delaunay. ( delaunay -- )
    delaunay-gadget new swap >>delaunay gadget. ;

: demo-delaunay ( -- delaunay )
    { 0 0 } { 256 256 } <rect> enclosing-delaunay ;

: random-point ( -- p )
    1 255 [a,b] random 1 255 [a,b] random 2array ;

: demo ( -- )
    demo-delaunay 1000 [ random-point insert ] times delaunay. ;