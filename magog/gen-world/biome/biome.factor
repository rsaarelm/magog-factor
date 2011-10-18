! Copyright (C) 2011 Risto Saarelma

QUALIFIED-WITH: magog.tile tile

USING: combinators kernel ;

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
    } case ;

PRIVATE>

GENERIC: default-edge ( biome -- edge )

GENERIC: biome-chunks ( biome -- chunks )

GENERIC: biome-tile ( char biome -- tile )

SINGLETON: ocean

SINGLETON: grassland

SINGLETON: mountain

M: ocean default-edge drop "~~~~~" ;

M: grassland default-edge drop "11111" ;

M: mountain default-edge drop "00000" ;
