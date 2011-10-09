! Copyright (C) 2011 Risto Saarelma

USING: assocs combinators.short-circuit dust.hex fry kernel locals math
math.vectors sequences ;

IN: dust.hexgraph

! Operating on the faces, edges and vertices of a graph where the faces are
! hexagons in a regular lattice. The faces, edges and vertices are encoded as
! points on a plane, with any point with integer coordinates being a valid
! face whose neighboring faces are at { +1 0 } { +1 +1 } { 0 +1 } { -1 0 }
! { -1 -1 } and { 0 -1 }.

! The edge and vertex values are encoded in lattices around the face points.
!              x = 0 .25 .5 .75 1
!
!              v1
!           e0    e1
!  y =   v0          v2
!  0        e5    f0----e2----> x=1
!  .25         v5  |       v3
!  .5             e4    e3
!  .75             | v4
!  1               V y=1

: face? ( x -- ? ) [ integer? ] all? ;

: edge? ( x -- ? ) [ 4 * 4 rem ] map
    { [ [ 0 = ] all? not ] [ [ [ 0 = ] [ 2 = ] bi or ] all? ] } 1&& ;

: vertex? ( x -- ? ) [ 4 * 4 rem ] map
    { [ { 3 1 } = ] [ { 1 3 } = ] } 1|| ;

<PRIVATE

: assert-loc ( x -- )
    { [ length 2 = ] [ [ number? ] all? ] } 1&&
    [ "Not a loc" throw ] unless ;

: assert-face ( x -- )
    dup assert-loc face? [ "Not a face" throw ] unless ;

: assert-edge ( x -- )
    dup assert-loc edge? [ "Not an edge" throw ] unless ;

: assert-vertex ( x -- )
    dup assert-loc vertex? [ "Not a vertex" throw ] unless ;

: assert-contains ( item graph -- )
    key? [ "Item not in graph" throw ] unless ;

: neighbor-keys ( face -- seq ) hex-dirs swap '[ _ v+ ] map ;

CONSTANT: edge-dirs { { -1/2 -1/2 }
                      { 0 -1/2 }
                      { 1/2 0 }
                      { 1/2 1/2 }
                      { 0 1/2 }
                      { -1/2 0 } }

CONSTANT: vertex-dirs { { -3/4 -1/4 }
                        { -1/4 -3/4 }
                        { 1/4 -1/4 }
                        { 3/4 1/4 }
                        { 1/4 3/4 }
                        { -1/4 1/4 } }

: edge-keys ( face -- seq ) dup assert-face
    edge-dirs swap '[ _ v+ ] map ;

: vertex-keys ( face -- seq ) dup assert-face
    vertex-dirs swap '[ _ v+ ] map ;

: touch ( pt hexgraph -- )
    2dup key? [ 2drop ] [ [ f ] 2dip set-at ] if ;

PRIVATE>

: <hexgraph> ( -- hexgraph ) H{ } clone ;

:: add-hex ( pt hexgraph -- )
    pt assert-face
    pt hexgraph touch
    pt edge-keys [ hexgraph touch ] each
    pt vertex-keys [ hexgraph touch ] each ;

: faces ( hexgraph -- seq ) keys [ face? ] filter ;

: edges ( hexgraph -- seq ) keys [ edge? ] filter ;

: vertices ( hexgraph -- seq ) keys [ vertex? ] filter ;

: neighbors ( face hexgraph -- seq )
    2dup assert-contains
    swap neighbor-keys [ over key? ] filter nip ;

: borders ( face hexgraph -- seq )
    ! Must have all borders if there's a face.
    2dup assert-contains drop edge-keys ;

: corners ( face hexgraph -- seq )
    ! Must have all corners if there's a face.
    2dup assert-contains drop vertex-keys ;

: on-edge? ( face hexgraph -- ? )
    neighbors length 6 < ;