! Copyright (C) 2011 Risto Saarelma

USING: arrays assocs combinators dust.hex dust.hexgraph kernel literals locals
math math.functions math.matrices math.vectors math.vectors.simd noise
sequences ;

IN: magog.overworld

! In regions
CONSTANT: overworld-radius 7

! In chunks, evaluated at parse time so that we can use it in constants later.
<<
CONSTANT: region-radius 5
>>

CONSTANT: ocean-region "ocean"
CONSTANT: beach-region "beach"
CONSTANT: grass-region "grass"
CONSTANT: forest-region "forest"
CONSTANT: mountain-region "mountain"

<PRIVATE

CONSTANT: region-to-plane-transform
{ { 1    0 }
  { -1/2 1 } }

CONSTANT: region-to-chunk-transform
{ { $ region-radius $[ region-radius neg ] }
  { $ region-radius $[ region-radius 2 * ] } }

: loc>perlin ( region-loc -- point )
    region-to-plane-transform v.m { 0 0 } append 10 v/n >float-4 ;

: noise-adjust ( region-loc -- n )
    hex-dist overworld-radius / 2 * 1 swap - ;

:: generate-terrain ( hexgraph -- )
    <perlin-noise-table> :> table
    hexgraph faces [ hexgraph on-edge? ] partition :> ( edge inner )
    edge [ [ ocean-region ] dip hexgraph set-at ] each
    inner [| loc |
        table loc loc>perlin perlin-noise loc noise-adjust + 0 >
          [ grass-region ] [ ocean-region ] if
        loc hexgraph set-at ] each ;

! XXX: Just picking an arbitrary face to fill the edge as, make a more
! detailed rule for this later.
:: fill-borders ( hexgraph -- )
    hexgraph edges [| edge |
        edge hexgraph sides first hexgraph at
        edge hexgraph set-at ] each ;

:: fill-corners ( hexgraph -- )
    hexgraph vertices [| vertex |
        vertex hexgraph touching first hexgraph at
        vertex hexgraph set-at ] each ;

PRIVATE>

: <overworld-graph> ( -- hexgraph )
    <hexgraph>
    overworld-radius iota [ hex-circle ] map concat
    [ over add-hex ] each ;

: generate-overworld ( hexgraph -- )
    { [ generate-terrain ]
      [ fill-borders ]
      [ fill-corners ]
    } cleave ;

<PRIVATE

: chunk-translate ( region-loc -- chunk-loc )
    region-to-chunk-transform v.m ;

CONSTANT: face-offset { 0 0 }
CONSTANT: e-edge-offset { 2 0 }
CONSTANT: se-edge-offset { 2 2 }
CONSTANT: sw-edge-offset { 0 2 }
CONSTANT: se-vertex-offset { 3 1 }
CONSTANT: s-vertex-offset { 1 3 }

: face-locs ( -- seq ) region-radius iota [ hex-circle ] map concat ;

: e-edge-locs ( -- seq )
    region-radius 1 - iota [ { 1 1 } n*v 1 1 region-radius - 2array v+ ] map ;

: se-edge-locs ( -- seq )
    region-radius 1 - iota [ { 0 1 } n*v region-radius 1 2array v+ ] map ;

: sw-edge-locs ( -- seq )
    region-radius 1 - iota [ { 1 0 } n*v 1 region-radius 2array v+ ] map ;

: se-vertex-loc ( -- seq ) region-radius 0 2array 1array ;

: s-vertex-loc ( -- seq ) region-radius dup 2array 1array ;

PRIVATE>

: chunk-locs ( hexgraph-key -- seq )
    [ [ floor ] map chunk-translate ] [ [ 4 * 4 rem ] map ] bi
    { { face-offset [ face-locs ] }
      { e-edge-offset [ e-edge-locs ] }
      { se-edge-offset [ se-edge-locs ] }
      { sw-edge-offset [ sw-edge-locs ] }
      { se-vertex-offset [ se-vertex-loc ] }
      { s-vertex-offset [ s-vertex-loc ] }
      [ "Invalid hexgraph key" throw ]
    } case
    swap [ v+ ] curry map ;

! The ring of chunk locs surrounding the central face of the region. Should be
! the union of the borders and corners of the face.
: perimeter ( region-loc -- chunk-loc-seq )
    chunk-translate region-radius hex-circle [ over v+ ] map nip ;

: inside-perimeter ( region-loc -- chunk-loc-seq )
    chunk-translate region-radius 1 - hex-circle [ over v+ ] map nip ;