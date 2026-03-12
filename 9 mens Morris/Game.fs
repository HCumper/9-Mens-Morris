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

type Phase =
    | Placing
    | Moving
    | Removing
    | GameOver

type GameState =
    { Board : Piece array
      NextToMove : Player
      Phase : Phase
      WhitePlaced : int
      BlackPlaced : int
      Selected : int option
      PendingMill : bool
      Winner : Player option
      Status : string }

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

    let neighbors : Map<int, int list> =
        Map.ofList [
            0,  [1; 9]
            1,  [0; 2; 4]
            2,  [1; 14]

            3,  [4; 10]
            4,  [1; 3; 5; 7]
            5,  [4; 13]

            6,  [7; 11]
            7,  [4; 6; 8]
            8,  [7; 12]

            9,  [0; 10; 21]
            10, [3; 9; 11; 18]
            11, [6; 10; 15]

            12, [8; 13; 17]
            13, [5; 12; 14; 20]
            14, [2; 13; 23]

            15, [11; 16]
            16, [15; 17; 19]
            17, [12; 16]

            18, [10; 19]
            19, [16; 18; 20; 22]
            20, [13; 19]

            21, [9; 22]
            22, [19; 21; 23]
            23, [14; 22]
        ]

    let mills : int list list =
        [
            [0; 1; 2]
            [3; 4; 5]
            [6; 7; 8]
            [15; 16; 17]
            [18; 19; 20]
            [21; 22; 23]

            [0; 9; 21]
            [3; 10; 18]
            [6; 11; 15]
            [1; 4; 7]
            [16; 19; 22]
            [8; 12; 17]
            [5; 13; 20]
            [2; 14; 23]

            [9; 10; 11]
            [12; 13; 14]
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

    let pieceCount player (state : GameState) =
        state.Board
        |> Array.sumBy (function
            | Occupied p when p = player -> 1
            | _ -> 0)

    let isEmpty index (state : GameState) =
        state.Board[index] = Empty

    let isOccupiedBy player index (state : GameState) =
        match state.Board[index] with
        | Occupied p when p = player -> true
        | _ -> false

    let private setPiece idx piece (board : Piece array) =
        let copy = Array.copy board
        copy[idx] <- piece
        copy

    let private boardHasMillAt point player (board : Piece array) =
        mills
        |> List.exists (fun mill ->
            List.contains point mill
            && (mill |> List.forall (fun p -> board[p] = Occupied player)))

    let private stateHasMillAt point player (state : GameState) =
        boardHasMillAt point player state.Board

    let private allPiecesInMills player (state : GameState) =
        [0 .. pointCount - 1]
        |> List.filter (fun p -> isOccupiedBy player p state)
        |> List.forall (fun p -> stateHasMillAt p player state)

    let removablePoints player (state : GameState) =
        let allInMills = allPiecesInMills player state
        [0 .. pointCount - 1]
        |> List.filter (fun p ->
            isOccupiedBy player p state
            && (allInMills || not (stateHasMillAt p player state)))

    let private hasAnyLegalMove player (state : GameState) =
        let count = pieceCount player state

        if count <= 3 then
            // Flying
            [0 .. pointCount - 1]
            |> List.exists (fun fromPoint ->
                isOccupiedBy player fromPoint state
                && ([0 .. pointCount - 1] |> List.exists (fun toPoint -> isEmpty toPoint state)))
        else
            [0 .. pointCount - 1]
            |> List.exists (fun fromPoint ->
                isOccupiedBy player fromPoint state
                && (neighbors[fromPoint] |> List.exists (fun toPoint -> isEmpty toPoint state)))

    let private gameOverState winner message (state : GameState) =
        { state with
            Phase = GameOver
            Winner = Some winner
            Selected = None
            PendingMill = false
            Status = message }

    let private checkForWinnerAfterTurn (state : GameState) =
        let playerToMove = state.NextToMove
        let enemy = playerToMove.Other

        let enemyCount = pieceCount enemy state
        let playerCount = pieceCount playerToMove state

        if enemyCount < 3 && state.WhitePlaced >= stonesPerSide && state.BlackPlaced >= stonesPerSide then
            gameOverState playerToMove (sprintf "%A wins: %A has fewer than 3 stones" playerToMove enemy) state
        elif playerCount < 3 && state.WhitePlaced >= stonesPerSide && state.BlackPlaced >= stonesPerSide then
            gameOverState enemy (sprintf "%A wins: %A has fewer than 3 stones" enemy playerToMove) state
        elif state.WhitePlaced >= stonesPerSide && state.BlackPlaced >= stonesPerSide && not (hasAnyLegalMove playerToMove state) then
            gameOverState enemy (sprintf "%A wins: %A has no legal move" enemy playerToMove) state
        else
            state

    let private advancePhaseAfterPlacement whitePlaced blackPlaced =
        if whitePlaced >= stonesPerSide && blackPlaced >= stonesPerSide then
            Moving
        else
            Placing

    let private statusForNormalTurn (state : GameState) =
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

    let private completeTurnWithoutMill (nextPhase : Phase) (nextToMove : Player) (state : GameState) =
        let s =
            { state with
                NextToMove = nextToMove
                Phase = nextPhase
                Selected = None
                PendingMill = false
                Status = "" }

        let s2 = { s with Status = statusForNormalTurn s }
        checkForWinnerAfterTurn s2

    let private completeTurnWithMill (samePlayer : Player) (state : GameState) =
        let s =
            { state with
                NextToMove = samePlayer
                Phase = Removing
                Selected = None
                PendingMill = true
                Status = sprintf "%A formed a mill: remove an opposing stone" samePlayer }

        s

    let private canMove fromPoint toPoint (state : GameState) =
        let count = pieceCount state.NextToMove state
        if count = 3 then
            // Flying
            isEmpty toPoint state
        else
            neighbors[fromPoint] |> List.contains toPoint

    let private handlePlacement index (state : GameState) =
        if not (isEmpty index state) then
            { state with Status = "That point is occupied" }
        else
            let newBoard = setPiece index (Occupied state.NextToMove) state.Board

            let whitePlaced =
                if state.NextToMove = White then state.WhitePlaced + 1 else state.WhitePlaced

            let blackPlaced =
                if state.NextToMove = Black then state.BlackPlaced + 1 else state.BlackPlaced

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

    let private handleMoveSelection index (state : GameState) =
        match state.Board[index] with
        | Occupied p when p = state.NextToMove ->
            let count = pieceCount state.NextToMove state
            let moveKind =
                if count = 3 then "choose destination to fly"
                else "choose destination"

            { state with
                Selected = Some index
                Status = sprintf "%A selected point %d; %s" state.NextToMove index moveKind }

        | _ ->
            { state with Status = "Select one of your own stones" }

    let private handleMoveDestination index fromPoint (state : GameState) =
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

    let private handleMoving index (state : GameState) =
        match state.Selected with
        | None -> handleMoveSelection index state
        | Some fromPoint -> handleMoveDestination index fromPoint state

    let private handleRemoving index (state : GameState) =
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

            let nextState = completeTurnWithoutMill nextPhase state.NextToMove.Other s
            nextState

    let clickPoint index (state : GameState) =
        if index < 0 || index >= pointCount then
            { state with Status = "Invalid point" }
        else
            match state.Phase with
            | GameOver ->
                state

            | Placing ->
                handlePlacement index state

            | Moving ->
                handleMoving index state

            | Removing ->
                handleRemoving index state