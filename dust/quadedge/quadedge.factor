! Copyright (C) 2011 Risto Saarelma

USING: accessors kernel locals sequences ;

IN: dust.quadedge

TUPLE: edge next turn sym data ;

! XXX: Cannot use the default equal? since it recurses into the structure and
! quadedges often have reference cycles. Instead of doing something very
! clever with graph comparisons, just overriding this to look at the memory
! address.
M: edge equal? eq? ;

! Using "turn" instead of the "rot" found in literature in order not to clash
! with Factor combinator conventions.

! Create the interlinked quad-edge complex and return the initial edge.
:: <edge> ( -- edge )
    edge new edge new edge new edge new :> ( e e-turn e-sym e-invturn )
    e-turn e-invturn >>next e-sym >>turn e-invturn >>sym drop
    e-sym e-sym >>next e-invturn >>turn e >>sym drop
    e-invturn e-turn >>next e >>turn e-turn >>sym drop
    e e >>next e-turn >>turn e-sym >>sym ;

: turn ( edge -- edge ) turn>> ;

: sym ( edge -- edge ) sym>> ;

: -turn ( edge -- edge ) sym turn ;

: orig-next ( edge -- edge ) next>> ;

: orig-prev ( edge -- edge ) turn orig-next turn ;

: dest-next ( edge -- edge ) sym orig-next sym ;

: dest-prev ( edge -- edge ) -turn orig-next -turn ;

: left-next ( edge -- edge ) -turn orig-next turn ;

: left-prev ( edge -- edge ) orig-next sym ;

: right-next ( edge -- edge ) turn orig-next -turn ;

: right-prev ( edge -- edge ) sym orig-next ;

: orig ( edge -- data ) data>> ;

: dest ( edge -- data ) sym orig ;

: set-points ( edge orig-data dest-data -- )
    [ over sym ] dip [ >>data drop ] 2bi@ ;

:: swap-nexts ( edge1 edge2 -- )
    edge1 orig-next edge2 orig-next :> ( next1 next2 )
    edge2 next1 >>next drop
    edge1 next2 >>next drop ;

! Connects disconnected edges and disconnects connected edges.
:: splice ( edge-a edge-b -- )
    edge-a orig-next turn edge-b orig-next turn swap-nexts
    edge-a edge-b swap-nexts ;

:: remove-edge ( edge -- )
    edge edge orig-prev splice
    edge sym edge sym orig-prev splice ;

:: edge-loop ( start-edge quot -- seq )
    start-edge quot call
    [ dup start-edge eq? not ] [ dup quot call swap ] produce
    nip start-edge prefix ; inline

: left-face-edges ( start-edge -- seq ) [ left-next ] edge-loop ;

: right-face-edges ( start-edge -- seq ) sym left-face-edges ;
