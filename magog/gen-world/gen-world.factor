! Copyright (C) 2011 Risto Saarelma

QUALIFIED-WITH: magog.tile tile

QUALIFIED: sets

USING: accessors arrays assocs combinators combinators.short-circuit dust.geom
dust.hexgraph hash-sets kernel locals magog.areautil magog.gen-world.biome
magog.gen-world.chunk magog.gen-world.chunks magog.gen-world.spawn
magog.overworld magog.rules math math.ranges math.vectors namespaces random
sequences ;

IN: magog.gen-world

<PRIVATE

TUPLE: slot edges ;

: <slot> ( -- slot ) H{ } clone slot boa ;

: <placed-chunks> ( -- placed-chunks ) H{ } clone ;

TUPLE: mapgen shape biome slots ;

: <mapgen> ( shape biome -- mapgen )
    H{ } clone mapgen boa ;

SYMBOL: +mapgen+
SYMBOL: +placed-chunks+

: get-mapgen ( -- mapgen ) +mapgen+ get ;

: current-biome ( -- biome ) get-mapgen biome>> ;

: get-placed ( -- placed-chunks ) +placed-chunks+ get ;

: within-shape? ( loc -- ? ) get-mapgen shape>> member? ;

: clear? ( loc -- ? ) get-placed key? not ;

:: slot-edge ( loc slot-edge -- edge/f )
    loc slot-edge v+ :> neighbor-loc
    neighbor-loc get-placed at
    [ slot-edge opposite edge ]
    [ neighbor-loc within-shape? slot-edge horizontal? not or [ f ] [ current-biome default-edge ] if
    ] if* ;

:: set-initial-edges ( slot loc -- )
    dirs [| dir |
        loc dir slot-edge [ dir slot edges>> set-at ] when*
    ] each ;

! If there is a slot at loc, return it. If loc is empty, generate one and
! return it. If loc has already a chunk in it or is outside the current
! shape, return f.
:: gen-slot ( loc -- slot/f )
    loc clear? loc within-shape? and
    [ loc get-mapgen slots>> at
      [ <slot> dup loc get-mapgen slots>> set-at
        dup loc set-initial-edges ] unless*
    ] [ f ] if ;

:: add-slots ( chunk loc -- )
    dirs [| dir |
        chunk dir edge :> edge
        loc dir v+ gen-slot :> slot
        slot [ edge dir opposite slot edges>> set-at ] when
    ] each ;

: slots ( -- slots ) get-mapgen slots>> ;

: level-slots ( z -- slots ) slots [ drop third over = ] assoc-filter nip ;

PRIVATE>

: with-mapgen ( placed-chunks mapgen quot -- )
    [ [ +mapgen+ set +placed-chunks+ set ] dip call ] with-scope ; inline

: new-chunk-sites ( -- locs ) slots keys ;

:: fits-in-slot? ( chunk slot -- ? )
    slot [
        slot edges>> >alist [
            first2 :> ( dir e )
            chunk dir edge e edges-match? ] all?
    ] [ f ] if ;

:: paint-chunk ( chunk loc -- )
    loc third :> z
    loc [ chunk strip-annotations [| loc char |
        char {
            { "z" [ tile:floor loc terrain
                    { 0 0 } area-below loc portal ] }
            { "y" [ tile:floor loc terrain
                    { 0 0 } area-above loc portal ] }
            [ current-biome biome-tile [ loc terrain ] when* ]
        } case
    ] assoc-each
    chunk-dim first current-rect-edge-portals
    ] make-area ;

: place-chunk ( chunk loc -- ) {
    [ get-placed set-at ]
    [ get-mapgen slots>> delete-at drop ]
    [ add-slots ]
    [ paint-chunk ] } 2cleave ;

: open-cells ( loc -- seq )
    [ { 0 0 } chunk-dim rect-iota [ terrain-at can-walk-terrain? ] filter ]
    make-area ;

: 1-chance-in ( n -- ? ) random 0 = ;

: on-cells ( loc n quot -- ) [ open-cells ] 2dip [ sample ] dip each ; inline

:: spawn-mobs ( loc -- )
    loc [
        loc third :> z
        {
            { [ z -6 < 8 1-chance-in and ]
              [ loc 2 random [ totem-guardian swap spawn ] on-cells ] }
            { [ z -4 < 6 1-chance-in and ]
              [ loc 3 random [ golem swap spawn ] on-cells ] }
            { [ z -1 < 5 1-chance-in and ]
              [ loc 5 random [ thrall swap spawn ] on-cells ] }
            { [ 4 1-chance-in ]
              [ loc 6 random [ dreg swap spawn ] on-cells ] }
            [ ]
        } cond
    ] make-area ;

CONSTANT: chunks-per-level 32

: perimeter ( shape -- seq )
    dup [ { [ northeast v+ over member? ] [ southeast v+ over member? ]
            [ southwest v+ over member? ] [ northwest v+ over member? ] } 1&& not ]
            filter nip ;

:: generate-region ( placed-chunks shape biome -- placed-chunks )
    placed-chunks clone :> old-placed
    f :> new-placed!
    shape length :> num-chunks
    placed-chunks shape biome <mapgen> [
        shape perimeter [ gen-slot drop ] each
        num-chunks [
            slots keys random :> slot-loc
            slot-loc [
                slot-loc slots at :> slot
                ! Try to find a fitting chunk.
                ! First try regular chunks.
                current-biome biome-chunks [ slot fits-in-slot? ] filter
                ! Then try extra chunks for connectivity.
                [ current-biome biome-extra-chunks [ slot fits-in-slot? ] filter ] when-empty
                ! Finally revert to an universal fallback chunk.
                [ fallback-chunk 1array ] when-empty
                random slot-loc place-chunk ] when
        ] times
        get-placed new-placed!
    ] with-mapgen
    ! new-placed keys old-placed keys sets:diff :> new-locs
    ! TODO: Paint terrain for new-locs here.
    new-placed ;

:: init-world ( -- )
    <overworld-graph> :> overworld
    overworld generate-overworld
    overworld edges overworld vertices append :> perimeter
    overworld faces :> regions
    perimeter <placed-chunks>
    [ [ 0 swap chunk-locs ] [ overworld at ] bi generate-region ] reduce
    regions swap
    [ [ 0 swap chunk-locs ] [ overworld at ] bi generate-region ] reduce
    drop
    ! Player in center
    starting-loc [ pc starting-loc open-cells random spawn ] make-area ;