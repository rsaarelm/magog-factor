! Copyright (C) 2011 Risto Saarelma

USING: accessors arrays assocs circular combinators dust.quadedge fry kernel
locals math math.constants math.order math.rectangles math.vectors sequences
sets sorting ;

IN: dust.delaunay

TUPLE: delaunay starting-edge edges support-edges ;

:: polygon-area ( points -- a )
    points <circular> :> points
    points length iota [| i |
        i points nth first2 :> ( x_i y_i )
        i 1 + points nth first2 :> ( x_i+1 y_i+1 )
        x_i y_i+1 * x_i+1 y_i * -
    ] map sum 1/2 * ;

:: (centroid) ( points -- center )
    1 points polygon-area 6 * / :> c
    points <circular> :> points
    points length iota [| i |
        i points nth first2 :> ( x_i y_i )
        i 1 + points nth first2 :> ( x_i+1 y_i+1 )
        x_i x_i+1 + x_i y_i+1 * x_i+1 y_i * - *
        y_i y_i+1 + x_i y_i+1 * x_i+1 y_i * - *
        2array c v*n
    ] map { 0 0 } [ v+ ] binary-reduce ;

: centroid ( points -- center )
    {
        { [ dup empty? ] [ drop f ] }
        { [ dup length 1 = ] [ first ] }
        { [ dup length 2 = ] [ first2 v+ 1/2 v*n ] }
        [ (centroid) ]
    } cond ;

:: <delaunay> ( a b c -- delaunay )
    <edge> <edge> <edge> :> ( ab bc ca )
    ab a b set-points
    bc b c set-points
    ca c a set-points
    ab sym bc splice
    bc sym ca splice
    ca sym ab splice
    ab { } ab bc ca 3array delaunay boa ;

:: enclosing-delaunay ( rect -- delaunay )
    rect rect-bounds :> ( loc dim )
    loc { 1 1 } v-
    loc { 1 1 } v- dim { 2 0 } v* { 2 0 } v+ v+
    loc { 1 1 } v- dim { 0 2 } v* { 0 2 } v+ v+ <delaunay> ;

: support-edge? ( edge delaunay -- ? )
    [ [ orig ] [ dest ] bi 2array ]
    [ support-edges>> [ orig ] map ] bi* intersects? ;

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

:: circumcircle-center ( a b c -- p )
    b a v- first2 :> ( bx by )
    c a v- first2 :> ( cx cy )
    bx cy * by cx * - 2 * :> d
    bx bx * by by * + cy * cx cx * cy cy * + by * - d /
    cx cx * cy cy * + bx * bx bx * by by * + cx * - d /
    2array a v+ ;

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

:: insert-vertex ( delaunay parent-edge p -- )
    delaunay edges>> length 3 + :> iter-limit!

    <edge> :> base!
    delaunay [ base prefix ] change-edges drop
    parent-edge :> e!
    parent-edge orig :> start-point
    base e orig p set-points
    base e splice
    base :> starting-edge
    [ e left-next starting-edge eq? ]
    [ iter-limit 1 - iter-limit!
      iter-limit 0 < [ "insert-vertex fails to converge" throw ] when

      e base sym connect base!
      delaunay [ base prefix ] change-edges drop
      base orig-prev e!
    ] do until

    ! Check if we need to flip edges.
    delaunay edges>> length 3 + iter-limit!
    t :> running!
    [ running ]
    [ iter-limit 1 - iter-limit!
      iter-limit 0 < [ "edge flip check fails to converge" throw ] when
      e orig-prev :> u
      { { [ e u dest right-of?
            e orig u dest e dest p in-circle? and ]
          [
              e flip
              e orig-prev e!
          ] }
        { [ e orig-next starting-edge = ] [ f running! ] }
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
        delaunay edge p insert-vertex
    ] unless
    delaunay ;

: dual-vertices ( edge -- left-p right-p )
    [ left-face-edges ] [ right-face-edges ] bi
    [ dup length 3 >=
      [ [ orig ] map first3 circumcircle-center ] [ drop f ] if ]
    bi@ ;

: edges ( delaunay -- seq )
    dup edges>> [ over support-edge? not ] filter nip ;

: vertices ( delaunay -- seq )
    edges [ [ orig ] [ dest ] bi 2array ] map concat members ;

: generate-dual-vertices ( delaunay -- )
    edges [ [ turn ] [ ] bi dual-vertices set-points ] each ;

: voronoi-cell ( edge -- seq ) turn left-face-edges
    dup [ orig ] all? [ drop f ] unless ;

:: relaxed-vertices ( delaunay -- seq )
    delaunay generate-dual-vertices
    delaunay edges [| edge |
        edge voronoi-cell [ orig ] map [ edge orig ] [ centroid ] if-empty
    ] map ;

<PRIVATE

: rotate ( seq -- seq' ) unclip suffix ;

: (normalize-cycle) ( elt-idx-ord-seq -- smallest-idx )
    [ ] [ 2array [ [ third ] bi@ <=> ] sort first ] map-reduce second ;

: normalize-cycle ( seq ord-quot -- seq' )
    '[ over _ call 3array ] dupd map-index
    (normalize-cycle) [ rotate ] times ; inline

PRIVATE>

: faces ( delaunay -- seq )
    edges>> [ left-face-edges ] map
    ! At this point we have many duplicates since the iteration made a
    ! separate face for each edge, normalize and prune to get the result.
    [ [ orig ] normalize-cycle ] map members ;

:: validate ( delaunay -- )
    delaunay faces [ length 3 >= ] filter :> faces
    delaunay vertices :> vertices
    faces [| face | vertices [| vertex |
        face [ orig ] map first3 vertex
        in-circle? [ "Vertex in circle" throw ] when
    ] each ] each ;