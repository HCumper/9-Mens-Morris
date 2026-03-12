namespace MorrisGui

type Player =
    | White
    | Black

    member this.Other =
        match this with
        | White -> Black
        | Black -> White

type Piece =
    | Empty
    | Occupied of Player

type Controller =
    | Human
    | Computer

type Action =
    | Place of int
    | Move of int * int
    | Remove of int

type Phase =
    | Placing
    | Moving
    | Removing
    | GameOver

type GameState =
    { Board: Piece array
      NextToMove: Player
      Phase: Phase
      WhitePlaced: int
      BlackPlaced: int
      Selected: int option
      PendingMill: bool
      Winner: Player option
      Status: string }

module MorrisBoard =

    // Point numbering:
    //
    //  0-----------1-----------2
    //  |           |           |
    //  |   3-------4-------5   |
    //  |   |       |       |   |
    //  |   |   6---7---8   |   |
    //  |   |   |       |   |   |
    //  9--10--11      12--13--14
    //  |   |   |       |   |   |
    //  |   |  15--16--17  |   |
    //  |   |       |       |   |
    //  |  18------19------20   |
    //  |           |           |
    // 21----------22----------23

    let pointCount = 24
    let stonesPerSide = 9

    let neighbors: Map<int, int list> =
        Map.ofList [
            0, [ 1; 9 ]
            1, [ 0; 2; 4 ]
            2, [ 1; 14 ]

            3, [ 4; 10 ]
            4, [ 1; 3; 5; 7 ]
            5, [ 4; 13 ]

            6, [ 7; 11 ]
            7, [ 4; 6; 8 ]
            8, [ 7; 12 ]

            9, [ 0; 10; 21 ]
            10, [ 3; 9; 11; 18 ]
            11, [ 6; 10; 15 ]

            12, [ 8; 13; 17 ]
            13, [ 5; 12; 14; 20 ]
            14, [ 2; 13; 23 ]

            15, [ 11; 16 ]
            16, [ 15; 17; 19 ]
            17, [ 12; 16 ]

            18, [ 10; 19 ]
            19, [ 16; 18; 20; 22 ]
            20, [ 13; 19 ]

            21, [ 9; 22 ]
            22, [ 19; 21; 23 ]
            23, [ 14; 22 ]
        ]

    let mills: int list list =
        [
            [ 0; 1; 2 ]
            [ 3; 4; 5 ]
            [ 6; 7; 8 ]
            [ 15; 16; 17 ]
            [ 18; 19; 20 ]
            [ 21; 22; 23 ]

            [ 0; 9; 21 ]
            [ 3; 10; 18 ]
            [ 6; 11; 15 ]
            [ 1; 4; 7 ]
            [ 16; 19; 22 ]
            [ 8; 12; 17 ]
            [ 5; 13; 20 ]
            [ 2; 14; 23 ]

            [ 9; 10; 11 ]
            [ 12; 13; 14 ]
        ]

    let initialState =
        { Board = Array.create pointCount Empty
          NextToMove = White
          Phase = Placing
          WhitePlaced = 0
          BlackPlaced = 0
          Selected = None
          PendingMill = false
          Winner = None
          Status = "White to move: place a stone" }

    let pieceCount player (state: GameState) =
        state.Board
        |> Array.sumBy (function
            | Occupied p when p = player -> 1
            | _ -> 0)

    let isEmpty index (state: GameState) = state.Board[index] = Empty

    let isOccupiedBy player index (state: GameState) =
        match state.Board[index] with
        | Occupied p when p = player -> true
        | _ -> false

    let private setPiece idx piece (board: Piece array) =
        let copy = Array.copy board
        copy[idx] <- piece
        copy

    let private boardHasMillAt point player (board: Piece array) =
        mills
        |> List.exists (fun mill ->
            List.contains point mill
            && (mill |> List.forall (fun p -> board[p] = Occupied player)))

    let private stateHasMillAt point player (state: GameState) =
        boardHasMillAt point player state.Board

    let private allPiecesInMills player (state: GameState) =
        [ 0 .. pointCount - 1 ]
        |> List.filter (fun p -> isOccupiedBy player p state)
        |> List.forall (fun p -> stateHasMillAt p player state)

    let removablePoints player (state: GameState) =
        let allInMills = allPiecesInMills player state

        [ 0 .. pointCount - 1 ]
        |> List.filter (fun p ->
            isOccupiedBy player p state
            && (allInMills || not (stateHasMillAt p player state)))

    let private hasAnyLegalMove player (state: GameState) =
        let count = pieceCount player state

        if count <= 3 then
            [ 0 .. pointCount - 1 ]
            |> List.exists (fun fromPoint ->
                isOccupiedBy player fromPoint state
                && ([ 0 .. pointCount - 1 ] |> List.exists (fun toPoint -> isEmpty toPoint state)))
        else
            [ 0 .. pointCount - 1 ]
            |> List.exists (fun fromPoint ->
                isOccupiedBy player fromPoint state
                && (neighbors[fromPoint] |> List.exists (fun toPoint -> isEmpty toPoint state)))

    let private gameOverState winner message (state: GameState) =
        { state with
            Phase = GameOver
            Winner = Some winner
            Selected = None
            PendingMill = false
            Status = message }

    let private checkForWinnerAfterTurn (state: GameState) =
        let playerToMove = state.NextToMove
        let enemy = playerToMove.Other

        let enemyCount = pieceCount enemy state
        let playerCount = pieceCount playerToMove state

        if enemyCount < 3 && state.WhitePlaced >= stonesPerSide && state.BlackPlaced >= stonesPerSide then
            gameOverState playerToMove (sprintf "%A wins: %A has fewer than 3 stones" playerToMove enemy) state
        elif playerCount < 3 && state.WhitePlaced >= stonesPerSide && state.BlackPlaced >= stonesPerSide then
            gameOverState enemy (sprintf "%A wins: %A has fewer than 3 stones" enemy playerToMove) state
        elif
            state.WhitePlaced >= stonesPerSide
            && state.BlackPlaced >= stonesPerSide
            && not (hasAnyLegalMove playerToMove state)
        then
            gameOverState enemy (sprintf "%A wins: %A has no legal move" enemy playerToMove) state
        else
            state

    let private advancePhaseAfterPlacement whitePlaced blackPlaced =
        if whitePlaced >= stonesPerSide && blackPlaced >= stonesPerSide then
            Moving
        else
            Placing

    let private statusForNormalTurn (state: GameState) =
        match state.Phase with
        | Placing -> sprintf "%A to move: place a stone" state.NextToMove
        | Moving ->
            let count = pieceCount state.NextToMove state

            if count = 3 then
                sprintf "%A to move: select a stone to fly" state.NextToMove
            else
                sprintf "%A to move: select a stone" state.NextToMove
        | Removing -> sprintf "%A formed a mill: remove an opposing stone" state.NextToMove
        | GameOver ->
            match state.Winner with
            | Some winner -> sprintf "Game over. Winner: %A" winner
            | None -> "Game over"

    let private completeTurnWithoutMill (nextPhase: Phase) (nextToMove: Player) (state: GameState) =
        let s =
            { state with
                NextToMove = nextToMove
                Phase = nextPhase
                Selected = None
                PendingMill = false
                Status = "" }

        let s2 = { s with Status = statusForNormalTurn s }
        checkForWinnerAfterTurn s2

    let private completeTurnWithMill (samePlayer: Player) (state: GameState) =
        { state with
            NextToMove = samePlayer
            Phase = Removing
            Selected = None
            PendingMill = true
            Status = sprintf "%A formed a mill: remove an opposing stone" samePlayer }

    let private canMove fromPoint toPoint (state: GameState) =
        let count = pieceCount state.NextToMove state

        if count = 3 then
            isEmpty toPoint state
        else
            neighbors[fromPoint] |> List.contains toPoint

    let private handlePlacement index (state: GameState) =
        if not (isEmpty index state) then
            { state with Status = "That point is occupied" }
        else
            let newBoard = setPiece index (Occupied state.NextToMove) state.Board

            let whitePlaced =
                if state.NextToMove = White then
                    state.WhitePlaced + 1
                else
                    state.WhitePlaced

            let blackPlaced =
                if state.NextToMove = Black then
                    state.BlackPlaced + 1
                else
                    state.BlackPlaced

            let nextPhase = advancePhaseAfterPlacement whitePlaced blackPlaced
            let formedMill = boardHasMillAt index state.NextToMove newBoard

            let newState =
                { state with
                    Board = newBoard
                    WhitePlaced = whitePlaced
                    BlackPlaced = blackPlaced }

            if formedMill then
                completeTurnWithMill state.NextToMove newState
            else
                completeTurnWithoutMill nextPhase state.NextToMove.Other newState

    let private handleMoveSelection index (state: GameState) =
        match state.Board[index] with
        | Occupied p when p = state.NextToMove ->
            let count = pieceCount state.NextToMove state

            let moveKind =
                if count = 3 then
                    "choose destination to fly"
                else
                    "choose destination"

            { state with
                Selected = Some index
                Status = sprintf "%A selected point %d; %s" state.NextToMove index moveKind }
        | _ -> { state with Status = "Select one of your own stones" }

    let private handleMoveDestination index fromPoint (state: GameState) =
        if fromPoint = index then
            { state with
                Selected = None
                Status = "Selection cleared" }
        elif not (isEmpty index state) then
            { state with Status = "Destination is occupied" }
        elif not (canMove fromPoint index state) then
            let count = pieceCount state.NextToMove state

            if count = 3 then
                { state with Status = "When flying, choose any empty point" }
            else
                { state with Status = "That move is not along a line" }
        else
            let board1 = setPiece fromPoint Empty state.Board
            let board2 = setPiece index (Occupied state.NextToMove) board1
            let formedMill = boardHasMillAt index state.NextToMove board2

            let newState =
                { state with
                    Board = board2
                    Selected = None }

            if formedMill then
                completeTurnWithMill state.NextToMove newState
            else
                completeTurnWithoutMill Moving state.NextToMove.Other newState

    let private handleMoving index (state: GameState) =
        match state.Selected with
        | None -> handleMoveSelection index state
        | Some fromPoint -> handleMoveDestination index fromPoint state

    let private handleRemoving index (state: GameState) =
        let opponent = state.NextToMove.Other
        let legalRemoves = removablePoints opponent state

        if not (List.contains index legalRemoves) then
            { state with Status = "That stone cannot be removed" }
        else
            let newBoard = setPiece index Empty state.Board

            let nextPhase =
                if state.WhitePlaced >= stonesPerSide && state.BlackPlaced >= stonesPerSide then
                    Moving
                else
                    Placing

            let s =
                { state with
                    Board = newBoard
                    PendingMill = false
                    Selected = None }

            completeTurnWithoutMill nextPhase state.NextToMove.Other s

    let clickPoint index (state: GameState) =
        if index < 0 || index >= pointCount then
            { state with Status = "Invalid point" }
        else
            match state.Phase with
            | GameOver -> state
            | Placing -> handlePlacement index state
            | Moving -> handleMoving index state
            | Removing -> handleRemoving index state

    let private wouldFormMillAfterPlacement player point (state: GameState) =
        if not (isEmpty point state) then
            false
        else
            let board = setPiece point (Occupied player) state.Board
            boardHasMillAt point player board

    let private wouldFormMillAfterMove player fromPoint toPoint (state: GameState) =
        if not (isOccupiedBy player fromPoint state) || not (isEmpty toPoint state) then
            false
        else
            let board1 = setPiece fromPoint Empty state.Board
            let board2 = setPiece toPoint (Occupied player) board1
            boardHasMillAt toPoint player board2

    let legalActions (state: GameState) : Action list =
        match state.Phase with
        | GameOver -> []
        | Placing ->
            [ 0 .. pointCount - 1 ]
            |> List.filter (fun p -> isEmpty p state)
            |> List.map Place
        | Removing ->
            removablePoints state.NextToMove.Other state |> List.map Remove
        | Moving ->
            let mine =
                [ 0 .. pointCount - 1 ]
                |> List.filter (fun p -> isOccupiedBy state.NextToMove p state)

            let destinations fromPoint =
                let count = pieceCount state.NextToMove state

                if count = 3 then
                    [ 0 .. pointCount - 1 ] |> List.filter (fun p -> isEmpty p state)
                else
                    neighbors[fromPoint] |> List.filter (fun p -> isEmpty p state)

            mine
            |> List.collect (fun fromPoint ->
                destinations fromPoint |> List.map (fun toPoint -> Move(fromPoint, toPoint)))

    let applyAction (action: Action) (state: GameState) : GameState =
        match action with
        | Place p ->
            match state.Phase with
            | Placing -> handlePlacement p state
            | _ -> { state with Status = "Invalid action" }
        | Move(fromPoint, toPoint) ->
            match state.Phase with
            | Moving ->
                state
                |> handleMoveSelection fromPoint
                |> handleMoveDestination toPoint fromPoint
            | _ -> { state with Status = "Invalid action" }
        | Remove p ->
            match state.Phase with
            | Removing -> handleRemoving p state
            | _ -> { state with Status = "Invalid action" }

    module NegamaxAi =

        let private rng = System.Random()

        [<Literal>]
        let private WinScore = 1_000_000

        [<Literal>]
        let private PieceWeightPlacing = 80

        [<Literal>]
        let private PieceWeightMoving = 130

        [<Literal>]
        let private MillWeightPlacing = 140

        [<Literal>]
        let private MillWeightMoving = 110

        [<Literal>]
        let private PotentialMillWeightPlacing = 60

        [<Literal>]
        let private PotentialMillWeightMoving = 35

        [<Literal>]
        let private MobilityWeight = 18

        [<Literal>]
        let private BlockedWeight = 40

        [<Literal>]
        let private DoubleThreatWeight = 140

        [<Literal>]
        let private ImmediateThreatWeight = 220

        [<Literal>]
        let private RemoveBonus = 140

        [<Literal>]
        let private DefaultDepth = 5

        let private isPlacementPhase (state: GameState) =
            state.WhitePlaced < stonesPerSide || state.BlackPlaced < stonesPerSide

        let private countCompletedMills player (state: GameState) =
            mills
            |> List.sumBy (fun mill ->
                if mill |> List.forall (fun p -> state.Board[p] = Occupied player) then 1 else 0)

        let private countPotentialMills player (state: GameState) =
            mills
            |> List.sumBy (fun mill ->
                let mine =
                    mill
                    |> List.sumBy (fun p ->
                        match state.Board[p] with
                        | Occupied q when q = player -> 1
                        | _ -> 0)

                let enemy =
                    mill
                    |> List.sumBy (fun p ->
                        match state.Board[p] with
                        | Occupied q when q = player.Other -> 1
                        | _ -> 0)

                let empty = 3 - mine - enemy

                if mine = 2 && empty = 1 && enemy = 0 then 1 else 0)

        let private blockedPieces player (state: GameState) =
            let count = pieceCount player state

            if count <= 3 || isPlacementPhase state then
                0
            else
                [ 0 .. pointCount - 1 ]
                |> List.sumBy (fun p ->
                    if isOccupiedBy player p state then
                        let hasMove =
                            neighbors[p] |> List.exists (fun n -> isEmpty n state)
                        if hasMove then 0 else 1
                    else
                        0)

        let private countDoubleThreats player (state: GameState) =
            [ 0 .. pointCount - 1 ]
            |> List.sumBy (fun p ->
                if isEmpty p state then
                    let board = setPiece p (Occupied player) state.Board

                    let millsCompleted =
                        mills
                        |> List.sumBy (fun mill ->
                            if List.contains p mill && (mill |> List.forall (fun q -> board[q] = Occupied player)) then
                                1
                            else
                                0)

                    if millsCompleted >= 2 then 1 else 0
                else
                    0)

        let private mobilityFor player (state: GameState) =
            if isPlacementPhase state then
                0
            elif pieceCount player state < 3 then
                0
            else
                let restored =
                    { state with
                        NextToMove = player
                        Phase = Moving
                        Selected = None
                        PendingMill = false }

                legalActions restored
                |> List.sumBy (function
                    | Move _ -> 1
                    | _ -> 0)

        let private immediateMillThreatCount player (state: GameState) =
            if state.Phase = Removing || state.Phase = GameOver then
                0
            else
                match state.Phase with
                | Placing ->
                    [ 0 .. pointCount - 1 ]
                    |> List.sumBy (fun p ->
                        if isEmpty p state && wouldFormMillAfterPlacement player p state then 1 else 0)

                | Moving ->
                    let mine =
                        [ 0 .. pointCount - 1 ]
                        |> List.filter (fun p -> isOccupiedBy player p state)

                    mine
                    |> List.sumBy (fun fromPoint ->
                        let destinations =
                            if pieceCount player state = 3 then
                                [ 0 .. pointCount - 1 ] |> List.filter (fun p -> isEmpty p state)
                            else
                                neighbors[fromPoint] |> List.filter (fun p -> isEmpty p state)

                        destinations
                        |> List.sumBy (fun toPoint ->
                            if wouldFormMillAfterMove player fromPoint toPoint state then 1 else 0))

                | _ ->
                    0

        let private evaluateFor (player: Player) (state: GameState) =
            let opp = player.Other

            match state.Winner with
            | Some winner when winner = player -> WinScore
            | Some _ -> -WinScore
            | None ->
                let myPieces = pieceCount player state
                let oppPieces = pieceCount opp state

                let myMills = countCompletedMills player state
                let oppMills = countCompletedMills opp state

                let myPotential = countPotentialMills player state
                let oppPotential = countPotentialMills opp state

                let myDoubleThreats = countDoubleThreats player state
                let oppDoubleThreats = countDoubleThreats opp state

                let myImmediateThreats = immediateMillThreatCount player state
                let oppImmediateThreats = immediateMillThreatCount opp state

                let removeScore =
                    if state.Phase = Removing && state.NextToMove = player then
                        RemoveBonus
                    elif state.Phase = Removing && state.NextToMove = opp then
                        -RemoveBonus
                    else
                        0

                if isPlacementPhase state then
                    (myPieces - oppPieces) * PieceWeightPlacing
                    + (myMills - oppMills) * MillWeightPlacing
                    + (myPotential - oppPotential) * PotentialMillWeightPlacing
                    + (myDoubleThreats - oppDoubleThreats) * DoubleThreatWeight
                    + (myImmediateThreats - oppImmediateThreats) * ImmediateThreatWeight
                    + removeScore
                else
                    let myMobility = mobilityFor player state
                    let oppMobility = mobilityFor opp state

                    let myBlockedEnemy = blockedPieces opp state
                    let oppBlockedMe = blockedPieces player state

                    (myPieces - oppPieces) * PieceWeightMoving
                    + (myMills - oppMills) * MillWeightMoving
                    + (myPotential - oppPotential) * PotentialMillWeightMoving
                    + (myMobility - oppMobility) * MobilityWeight
                    + (myBlockedEnemy - oppBlockedMe) * BlockedWeight
                    + (myDoubleThreats - oppDoubleThreats) * DoubleThreatWeight
                    + (myImmediateThreats - oppImmediateThreats) * ImmediateThreatWeight
                    + removeScore

        let private moveOrderingScore (state: GameState) (action: Action) =
            let me = state.NextToMove
            let opp = me.Other

            let formsMill =
                match action with
                | Place p -> if wouldFormMillAfterPlacement me p state then 1 else 0
                | Move (fromPoint, toPoint) -> if wouldFormMillAfterMove me fromPoint toPoint state then 1 else 0
                | Remove _ -> 0

            let nextState = applyAction action state

            let blocksOppImmediateMill =
                let before = immediateMillThreatCount opp state
                let after = immediateMillThreatCount opp nextState
                if before > 0 && after < before then 1 else 0

            let removeScore =
                match action with
                | Remove p ->
                    let inMill = stateHasMillAt p opp state
                    if inMill then 40 else 220
                | _ -> 0

            let centralityScore =
                match action with
                | Place p -> neighbors[p].Length * 5
                | Move (_, toPoint) -> neighbors[toPoint].Length * 5
                | Remove _ -> 0

            formsMill * 1000 + blocksOppImmediateMill * 900 + removeScore + centralityScore

        let private orderActions (state: GameState) (actions: Action list) =
            actions |> List.sortByDescending (moveOrderingScore state)

        let private boardSignature (state: GameState) =
            let boardPart =
                state.Board
                |> Array.map (function
                    | Empty -> 'E'
                    | Occupied White -> 'W'
                    | Occupied Black -> 'B')
                |> System.String

            sprintf "%s|%A|%A|%d|%d"
                boardPart
                state.NextToMove
                state.Phase
                state.WhitePlaced
                state.BlackPlaced

        let rec private negamax
            (rootPlayer: Player)
            (state: GameState)
            (depth: int)
            (ply: int)
            (alpha: int)
            (beta: int)
            (history: Set<string>) =

            let sigKey = boardSignature state

            if history.Contains sigKey then
                0, None
            elif state.Phase = GameOver then
                let score =
                    match state.Winner with
                    | Some winner when winner = rootPlayer -> WinScore - ply
                    | Some _ -> -WinScore + ply
                    | None -> 0
                score, None
            elif depth = 0 then
                evaluateFor rootPlayer state, None
            else
                let actions = legalActions state |> orderActions state

                match actions with
                | [] ->
                    evaluateFor rootPlayer state, None
                | _ ->
                    let mutable bestScore = System.Int32.MinValue / 4
                    let mutable bestActions : Action list = []
                    let mutable a = alpha
                    let mutable cutoff = false
                    let nextHistory = history.Add sigKey

                    for action in actions do
                        if not cutoff then
                            let nextState = applyAction action state

                            let extension =
                                match nextState.Phase with
                                | Removing -> 1
                                | _ -> 0

                            let childScore =
                                if nextState.NextToMove = state.NextToMove then
                                    fst (negamax rootPlayer nextState (depth - 1 + extension) (ply + 1) a beta nextHistory)
                                else
                                    -fst (negamax rootPlayer nextState (depth - 1 + extension) (ply + 1) (-beta) (-a) nextHistory)

                            if childScore > bestScore then
                                bestScore <- childScore
                                bestActions <- [ action ]
                            elif childScore = bestScore then
                                bestActions <- action :: bestActions

                            if childScore > a then
                                a <- childScore

                            if a >= beta then
                                cutoff <- true

                    match bestActions with
                    | [] -> bestScore, None
                    | xs -> bestScore, Some xs[rng.Next(xs.Length)]

        let chooseActionWithDepth depth (state: GameState) : Action option =
            let _, action =
                negamax
                    state.NextToMove
                    state
                    depth
                    0
                    (System.Int32.MinValue / 4)
                    (System.Int32.MaxValue / 4)
                    Set.empty

            action

        let chooseActionIterative maxDepth (state: GameState) : Action option =
            let mutable bestAction = None

            for depth in 1 .. maxDepth do
                match chooseActionWithDepth depth state with
                | Some action -> bestAction <- Some action
                | None -> ()

            bestAction

        let chooseAction (state: GameState) : Action option =
            chooseActionIterative DefaultDepth state