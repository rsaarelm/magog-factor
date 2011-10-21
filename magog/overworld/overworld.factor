! Copyright (C) 2011 Risto Saarelma

QUALIFIED-WITH: magog.gen-world.biome biome

USING: arrays assocs combinators dust.hex dust.hexgraph kernel literals locals
math math.functions math.matrices math.vectors math.vectors.simd noise
sequences ;

IN: magog.overworld

! In regions
CONSTANT: overworld-radius 7

! Region radius in chunks, evaluated at parse time so that we can use it in
! constants later.
<<
CONSTANT: region-radius 5
>>

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
    edge [ [ biome:ocean ] dip hexgraph set-at ] each
    inner [| loc |
        table loc loc>perlin perlin-noise loc noise-adjust +
        { { [ dup .6 > ] [ biome:mountain ] }
          { [ dup .3 > ] [ biome:forest ] }
          { [ dup 0 > ] [ biome:grassland ] }
          [ biome:ocean ] } cond nip
        loc hexgraph set-at ] each ;

: choose-perimeter-biome ( connected-region-biomes -- biome ) supremum ;

:: fill-borders ( hexgraph -- )
    hexgraph edges [| edge |
        edge hexgraph sides choose-perimeter-biome hexgraph at
        edge hexgraph set-at ] each ;

:: fill-corners ( hexgraph -- )
    hexgraph vertices [| vertex |
        vertex hexgraph touching choose-perimeter-biome hexgraph at
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

<PRIVATE
: (chunk-locs) ( hexgraph-key -- seq )
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
PRIVATE>

: chunk-locs ( z hexgraph-key -- seq )
    (chunk-locs) [ over suffix ] map nip ;