! Copyright (C) 2011 Risto Saarelma

USING: accessors arrays combinators dust.quadedge kernel locals math
math.constants math.order math.rectangles math.vectors sequences sets sorting
;

IN: dust.delaunay

TUPLE: delaunay starting-edge edges ;

:: <delaunay> ( a b c -- delaunay )
    <edge> <edge> <edge> :> ( ab bc ca )
    ab a b set-points
    bc b c set-points
    ca c a set-points
    ab sym bc splice
    bc sym ca splice
    ca sym ab splice
    ab { } clone delaunay boa ;

:: enclosing-delaunay ( rect -- delaunay )
    rect rect-bounds :> ( loc dim )
    loc { 1 1 } v-
    loc { 1 1 } v- dim { 2 0 } v* { 2 0 } v+ v+
    loc { 1 1 } v- dim { 0 2 } v* { 0 2 } v+ v+ <delaunay> ;

! Add a new edge going from the destination of a to the origin of b.
:: connect ( edge-a edge-b -- edge )
    <edge> :> e
    e edge-a left-next splice
    e sym edge-b splice
    e edge-a dest edge-b orig set-points
    e ;

! Turn the edge counterclockwise in its enclosing quadrilateral.
:: flip ( edge -- )
    edge orig-prev edge sym orig-prev :> ( a b )
    edge a splice
    edge sym b splice
    edge a left-next splice
    edge sym b left-next splice
    edge a dest b dest set-points ;

! Positive if the perimeter is oriented counterclockwise.
:: tri-area ( a b c -- area )
    b first a first - c second a second - *
    b second a second - c first a first - * - ;

:: in-circle? ( a b c p -- ? )
    a norm-sq b c p tri-area *
    b norm-sq a c p tri-area * -
    c norm-sq a b p tri-area * +
    p norm-sq a b c tri-area * - 0 > ;

: ccw? ( a b c -- ? ) tri-area 0 > ;

:: right-of? ( edge p -- ? ) p edge dest edge orig ccw? ;

:: left-of? ( edge p -- ? ) p edge orig edge dest ccw? ;

:: segment-dist ( a b p -- x )
    b a v- norm-sq :> l2
    l2 epsilon < [ p a v- norm ] ! Degenerate line
    [
        p a v- b a v- v. l2 / :> u
        {
            { [ u 0 < ] [ p a v- norm ] } ! p beyond end a
            { [ u 1 > ] [ p b v- norm ] } ! p beyond end b
            [ a u b a v- n*v v+ :> proj
              p proj v- norm ]
        } cond
    ] if ;

:: on-edge? ( edge p -- ? )
    p edge orig v- norm :> t1
    p edge dest v- norm :> t2
    t1 epsilon > t2 epsilon > and [
        edge orig edge dest v- norm :> t3
        t1 t3 > t2 t3 > or [ f ]
        [ edge orig edge dest p segment-dist epsilon < ] if
    ] [ t ] if ;

! Find an edge on a triangle that contains point p.
:: (find-containing) ( edge p -- edge )
   {
       { [ p edge orig = p edge dest = or ] [ edge ] }
       { [ edge p right-of? ] [ edge sym p (find-containing) ] }
       { [ edge orig-next p right-of? not ]
         [ edge orig-next p (find-containing) ] }
       { [ edge dest-prev p right-of? not ]
         [ edge dest-prev p (find-containing) ] }
       [ edge ]
   } cond ;

: find-containing ( delaunay p -- edge )
    [ starting-edge>> ] dip (find-containing) ;

:: insert-vertex ( delaunay parent-edge p -- edge )
    delaunay edges>> length 3 + :> iter-limit!

    <edge> :> base!
    delaunay [ base prefix ] change-edges drop
    parent-edge :> e!
    parent-edge orig :> start-point
    base e orig p set-points
    base e splice
    [ e dest start-point = ]
    [ iter-limit 1 - iter-limit!
      iter-limit 0 < [ "insert-vertex fails to converge" throw ] when

      e base sym connect base!
      delaunay [ base prefix ] change-edges drop
      base orig-prev e!
    ] do until
    base orig-prev ;

:: check-edges ( delaunay parent-edge p edge -- )
    edge :> e!
    t :> running!
    [ running ]
    [ e orig-prev :> u
      { { [ e u dest right-of?
            e orig u dest e dest p in-circle? and ]
          [
              e flip
              u e!
          ] }
        { [ e orig parent-edge orig = ] [ f running! ] }
        [ e orig-next left-prev e! ]
      } cond ] while ;

:: insert ( delaunay p -- delaunay )
    delaunay p find-containing :> edge!
    p edge orig = p edge dest = or [
        edge p on-edge? [
            edge orig-prev edge!
            edge orig-next remove-edge
            delaunay [ edge orig-next swap remove-eq ] change-edges drop
        ] when
        delaunay edge p insert-vertex :> e
        delaunay edge p e check-edges
    ] unless
    delaunay ;

: vertices ( delaunay -- seq )
    edges>> [ [ orig ] [ dest ] bi 2array ] map concat members ;

: edges ( delaunay -- seq )
    edges>> [ [ orig ] [ dest ] bi 2array [ <=> ] sort ] map members ;

: bounding-rect ( delaunay -- rect )
    vertices [ f ]
    [ [ unclip [ vmin ] reduce ] [ unclip [ vmax ] reduce ] bi
      over v- <rect> ] if-empty ;