module ForceDirected exposing
    ( State
    , simulation
    , tick
    , isCompleted
    , Entity
    , Force
    , links
    , manyBodyStrength
    , center
    )

{-| Simple force-directed layout simulation
Vendored to avoid Force module naming conflict between elm-units and elm-visualization
-}


type State id
    = State
        { forces : List (Force id)
        , alpha : Float
        , alphaDecay : Float
        , iterations : Int
        }


type alias Entity a =
    { a
        | x : Float
        , y : Float
        , vx : Float
        , vy : Float
        , id : Int
    }


type Force id
    = Links (List { source : Int, target : Int, distance : Float, strength : Maybe Float })
    | ManyBody Float (List Int)
    | Center Float Float


simulation : List (Force id) -> State id
simulation forces =
    State
        { forces = forces
        , alpha = 1.0
        , alphaDecay = 0.01
        , iterations = 300
        }


isCompleted : State id -> Bool
isCompleted (State state) =
    state.alpha < 0.001


tick : State id -> List (Entity a) -> ( State id, List (Entity a) )
tick (State state) entities =
    if state.alpha < 0.001 then
        ( State state, entities )

    else
        let
            -- Apply all forces
            updatedEntities =
                List.foldl applyForce entities state.forces
                    |> List.map
                        (\entity ->
                            { entity
                                | x = entity.x + entity.vx * state.alpha
                                , y = entity.y + entity.vy * state.alpha
                                , vx = entity.vx * 0.4
                                , vy = entity.vy * 0.4
                            }
                        )

            newAlpha =
                state.alpha * (1 - state.alphaDecay)
        in
        ( State { state | alpha = newAlpha }, updatedEntities )


applyForce : Force id -> List (Entity a) -> List (Entity a)
applyForce force entities =
    case force of
        Links linkList ->
            applyLinks linkList entities

        ManyBody strength _ ->
            applyManyBody strength entities

        Center cx cy ->
            applyCenter cx cy entities


applyLinks : List { source : Int, target : Int, distance : Float, strength : Maybe Float } -> List (Entity a) -> List (Entity a)
applyLinks linkList entities =
    let
        entityById =
            List.foldl (\e dict -> ( e.id, e ) :: dict) [] entities

        findEntity id =
            List.filter (\( eid, _ ) -> eid == id) entityById
                |> List.head
                |> Maybe.map Tuple.second

        applyLink link =
            case ( findEntity link.source, findEntity link.target ) of
                ( Just source, Just target ) ->
                    let
                        dx =
                            target.x - source.x

                        dy =
                            target.y - source.y

                        distance =
                            sqrt (dx * dx + dy * dy)

                        strength =
                            Maybe.withDefault 0.5 link.strength

                        force =
                            (distance - link.distance) * strength / 2

                        fx =
                            if distance > 0 then
                                dx / distance * force

                            else
                                0

                        fy =
                            if distance > 0 then
                                dy / distance * force

                            else
                                0
                    in
                    [ ( link.source, \e -> { e | vx = e.vx + fx, vy = e.vy + fy } )
                    , ( link.target, \e -> { e | vx = e.vx - fx, vy = e.vy - fy } )
                    ]

                _ ->
                    []

        updates =
            List.concatMap applyLink linkList
    in
    List.map
        (\entity ->
            List.foldl
                (\( id, update ) e ->
                    if e.id == id then
                        update e

                    else
                        e
                )
                entity
                updates
        )
        entities


applyManyBody : Float -> List (Entity a) -> List (Entity a)
applyManyBody strength entities =
    let
        applyToEntity entity otherEntities =
            let
                ( fx, fy ) =
                    List.foldl
                        (\other ( accX, accY ) ->
                            if other.id == entity.id then
                                ( accX, accY )

                            else
                                let
                                    dx =
                                        other.x - entity.x

                                    dy =
                                        other.y - entity.y

                                    distance =
                                        sqrt (dx * dx + dy * dy) + 1

                                    force =
                                        strength / (distance * distance)

                                    forceX =
                                        if distance > 1 then
                                            dx / distance * force

                                        else
                                            0

                                    forceY =
                                        if distance > 1 then
                                            dy / distance * force

                                        else
                                            0
                                in
                                ( accX + forceX, accY + forceY )
                        )
                        ( 0, 0 )
                        otherEntities
            in
            { entity | vx = entity.vx + fx, vy = entity.vy + fy }
    in
    List.map (\e -> applyToEntity e entities) entities


applyCenter : Float -> Float -> List (Entity a) -> List (Entity a)
applyCenter cx cy entities =
    let
        count =
            toFloat (List.length entities)

        ( sumX, sumY ) =
            List.foldl
                (\e ( x, y ) -> ( x + e.x, y + e.y ))
                ( 0, 0 )
                entities

        avgX =
            sumX / count

        avgY =
            sumY / count

        dx =
            cx - avgX

        dy =
            cy - avgY
    in
    List.map
        (\e ->
            { e
                | vx = e.vx + dx * 0.1
                , vy = e.vy + dy * 0.1
            }
        )
        entities


links : List { source : Int, target : Int, distance : Float, strength : Maybe Float } -> Force id
links =
    Links


manyBodyStrength : Float -> List Int -> Force id
manyBodyStrength =
    ManyBody


center : Float -> Float -> Force id
center =
    Center
