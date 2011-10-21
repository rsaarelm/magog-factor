! Copyright (C) 2011 Risto Saarelma

QUALIFIED-WITH: magog.tile tile

USING: classes.parser classes.singleton classes.union combinators kernel lexer
magog.gen-world.chunks ;

IN: magog.gen-world.biome

<PRIVATE

: default-tileset ( char -- tile )
    { { "." [ tile:floor ] }
      { "#" [ tile:wall ] }
      { "*" [ tile:rock ] }
      { "~" [ tile:water ] }
      { "<" [ tile:slope0 ] }
      { "P" [ tile:slope1 ] }
      { "A" [ tile:slope2 ] }
      { ">" [ tile:slope3 ] }
      { "J" [ tile:slope4 ] }
      { "V" [ tile:slope5 ] }
      [ drop f ]
    } case ;

PRIVATE>

GENERIC: default-edge ( biome -- edge )

GENERIC: biome-chunks ( biome -- chunks )

! Chunks that won't be chosen for unconstrained random terrain but can be used
! as transition tiles from a neighboring different terrain.
GENERIC: biome-extra-chunks ( biome -- chunks )

GENERIC: biome-tile ( char biome -- tile )

<<
! Make each biome into a singleton class and make the union-class "biome"
! encompass them all.
SYNTAX: BIOMES:
    ";" [ create-class-in dup define-singleton-class ] map-tokens
    [ "biome" create-class-in ] dip define-union-class ;
>>

BIOMES: ocean grassland forest mountain ;

M: ocean default-edge drop "~~~~~" ;

M: ocean biome-chunks drop water-chunks ;

M: ocean biome-extra-chunks drop water-ground-chunks ;

M: ocean biome-tile drop
    { { "." [ tile:sand ] }
      [ default-tileset ] } case ;

M: grassland default-edge drop "11111" ;

M: grassland biome-chunks drop natural-chunks ;

M: grassland biome-extra-chunks drop water-ground-chunks ;

M: grassland biome-tile drop
    { { "." [ tile:grass ] }
      { "#" [ tile:sand ] }
      [ default-tileset ] } case ;

M: forest default-edge drop "11111" ;

M: forest biome-chunks drop natural-chunks ;

M: forest biome-extra-chunks drop water-ground-chunks ;

M: forest biome-tile drop
    { { "." [ tile:grass ] }
      { "#" [ tile:tree ] }
      [ default-tileset ] } case ;

M: mountain default-edge drop "11111" ;

M: mountain biome-chunks drop natural-chunks ;

M: mountain biome-extra-chunks drop f ;

M: mountain biome-tile drop
    { { "." [ tile:sand ] }
      { "#" [ tile:rock ] }
      [ default-tileset ] } case ;