namespace MorrisGui

open System
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading

type TurnRecord =
    { Player : Player
      MainAction : MorrisGui.Action
      Capture : MorrisGui.Action option }
    
type MainWindow() as this =
    inherit Window()

    let mutable state = MorrisBoard.initialState
    let mutable lastMoveDescription = "None"
    let mutable engineThinking = false
    let mutable moveHistory : TurnRecord list = []

    let whiteController = Human
    let blackController = Computer

    let controllerFor player =
        match player with
        | White -> whiteController
        | Black -> blackController

    let playerName player =
        match player with
        | White -> "Red"
        | Black -> "Blue"

    let describeAction (player: Player) (action: MorrisGui.Action) =
        match action with
        | Place p ->
            sprintf "%s placed at %d" (playerName player) p
        | Move (fromPoint, toPoint) ->
            sprintf "%s moved %d -> %d" (playerName player) fromPoint toPoint
        | Remove p ->
            sprintf "%s removed at %d" (playerName player) p

    let compactAction (_player: Player) (action: MorrisGui.Action) =
        match action with
        | Place p -> sprintf "P%d" p
        | Move (fromPoint, toPoint) -> sprintf "M%d-%d" fromPoint toPoint
        | Remove p -> sprintf "x%d" p

    let compactTurn (turn: TurnRecord) =
        match turn.Capture with
        | Some capture ->
            sprintf "%s %s"
                (compactAction turn.Player turn.MainAction)
                (compactAction turn.Player capture)
        | None ->
            compactAction turn.Player turn.MainAction
    
    let recordAction (player: Player) (action: MorrisGui.Action) =
        match action with
        | Remove _ ->
            match List.rev moveHistory with
            | last :: rest when last.Player = player && last.Capture.IsNone ->
                let updated =
                    { last with Capture = Some action }

                moveHistory <- List.rev rest @ [ updated ]
            | _ ->
                moveHistory <-
                    moveHistory @
                        [ { Player = player
                            MainAction = action
                            Capture = None } ]

        | _ ->
            moveHistory <-
                moveHistory @
                    [ { Player = player
                        MainAction = action
                        Capture = None } ]

        lastMoveDescription <- describeAction player action

    let renderMoveHistory () =
        if List.isEmpty moveHistory then
            "Game record:\n(none yet)"
        else
            let indexed = moveHistory |> List.indexed

            let lines =
                indexed
                |> List.chunkBySize 2
                |> List.map (fun chunk ->
                    match chunk with
                    | [ (i1, t1); (_, t2) ] ->
                        sprintf "%d. %-14s %-14s"
                            (i1 / 2 + 1)
                            (compactTurn t1)
                            (compactTurn t2)
                    | [ (i1, t1) ] ->
                        sprintf "%d. %-14s"
                            (i1 / 2 + 1)
                            (compactTurn t1)
                    | _ ->
                        "")

            "Game record:\n" + String.concat "\n" lines
    
    let board =
        BoardControl(
            Width = 700.0,
            Height = 700.0,
            HorizontalAlignment = HorizontalAlignment.Center,
            VerticalAlignment = VerticalAlignment.Center
        )

    let statusText =
        TextBlock(
            Text = state.Status,
            FontSize = 18.0,
            Margin = Thickness(10.0),
            TextWrapping = TextWrapping.Wrap
        )

    let turnText =
        TextBlock(
            Text = sprintf "Turn: %A" state.NextToMove,
            FontSize = 16.0,
            Margin = Thickness(10.0)
        )

    let lastMoveText =
        TextBlock(
            Text = "Last move: None",
            FontSize = 16.0,
            Margin = Thickness(10.0),
            TextWrapping = TextWrapping.Wrap
        )

    let thinkingText =
        TextBlock(
            Text = "",
            FontSize = 16.0,
            Margin = Thickness(10.0),
            Foreground = Brushes.DarkRed,
            TextWrapping = TextWrapping.Wrap
        )

    let gameRecordText =
        TextBlock(
            Text = "Game record:\n(none yet)",
            FontSize = 14.0,
            Margin = Thickness(10.0),
            TextWrapping = TextWrapping.Wrap,
            FontFamily = FontFamily("Consolas")
        )

    let gameRecordScroll =
        ScrollViewer(
            Content = gameRecordText,
            Height = 180.0,
            HorizontalScrollBarVisibility = ScrollBarVisibility.Auto,
            VerticalScrollBarVisibility = ScrollBarVisibility.Auto,
            Margin = Thickness(90.0, 10.0, 10.0, 10.0)
        )

    let resetButton =
        Button(
            Content = "Reset",
            Margin = Thickness(10.0),
            HorizontalAlignment = HorizontalAlignment.Left
        )

    let setThinking isThinking =
        engineThinking <- isThinking
        thinkingText.Text <-
            if isThinking then
                sprintf "%s is thinking..." (playerName state.NextToMove)
            else
                ""

    let refreshUi () =
        board.State <- state
        board.InvalidateVisual()

        let shownStatus =
            if engineThinking && state.Phase <> GameOver then
                sprintf "%s is thinking..." (playerName state.NextToMove)
            else
                state.Status

        statusText.Text <- shownStatus
        lastMoveText.Text <- sprintf "Last move: %s" lastMoveDescription
        gameRecordText.Text <- renderMoveHistory ()

        let winnerText =
            match state.Winner with
            | Some p -> sprintf "   Winner: %A" p
            | None -> ""

        turnText.Text <- sprintf "Turn: %A   Phase: %A%s" state.NextToMove state.Phase winnerText

        gameRecordScroll.ScrollToEnd()

    let rec runOneComputerMove () =
        if state.Phase <> GameOver && controllerFor state.NextToMove = Computer then
            let thinkingPlayer = state.NextToMove
            let position = state

            setThinking true
            refreshUi ()

            async {
                let! actionOpt =
                    Task.Run(fun () -> MorrisBoard.NegamaxAi.chooseAction position)
                    |> Async.AwaitTask

                let uiTask =
                    Dispatcher.UIThread.InvokeAsync(fun () ->
                        match actionOpt with
                        | Some action ->
                            recordAction thinkingPlayer action
                            state <- MorrisBoard.applyAction action state
                            setThinking false
                            refreshUi ()

                            if state.Phase <> GameOver && controllerFor state.NextToMove = Computer then
                                runOneComputerMove ()

                        | None ->
                            setThinking false
                            refreshUi ()
                    )

                do! (uiTask.GetTask() |> Async.AwaitTask)
            }
            |> Async.StartImmediate

    do
        this.Title <- "Nine Men's Morris"
        this.Width <- 1400.0
        this.Height <- 950.0
        this.MinWidth <- 1200.0
        this.MinHeight <- 850.0
        board.State <- state

        board.OnPointClicked <-
            fun idx ->
                if controllerFor state.NextToMove = Human then
                    let before = state
                    let after = MorrisBoard.clickPoint idx state

                    if not (obj.ReferenceEquals(before, after)) then
                        match before.Phase, before.Selected, after.Phase with
                        | Placing, _, _ ->
                            recordAction before.NextToMove (Place idx)

                        | Moving, None, _ ->
                            ()

                        | Moving, Some fromPoint, _ when fromPoint <> idx ->
                            match after.Status with
                            | "Destination is occupied"
                            | "That move is not along a line"
                            | "When flying, choose any empty point" ->
                                ()
                            | _ ->
                                recordAction before.NextToMove (Move(fromPoint, idx))

                        | Removing, _, _ ->
                            recordAction before.NextToMove (Remove idx)

                        | _ ->
                            ()

                    state <- after
                    setThinking false
                    refreshUi ()

                    if controllerFor state.NextToMove = Computer && state.Phase <> GameOver then
                        runOneComputerMove ()

        resetButton.Click.Add(fun _ ->
            state <- MorrisBoard.initialState
            lastMoveDescription <- "None"
            moveHistory <- []
            setThinking false
            refreshUi ()

            if controllerFor state.NextToMove = Computer then
                runOneComputerMove ()
        )

        let root =
            Grid(
                ColumnDefinitions = ColumnDefinitions("4*,2*"),
                RowDefinitions = RowDefinitions("*")
            )

        let leftPanel =
            Grid(
                RowDefinitions = RowDefinitions("Auto,*")
            )

        let topPanel =
            StackPanel(
                Orientation = Orientation.Vertical,
                Margin = Thickness(10.0)
            )

        topPanel.Children.Add(turnText)
        topPanel.Children.Add(statusText)
        topPanel.Children.Add(lastMoveText)
        topPanel.Children.Add(thinkingText)
        topPanel.Children.Add(resetButton)

        Grid.SetRow(topPanel, 0)
        leftPanel.Children.Add(topPanel)

        let boardHost =
            Grid(
                HorizontalAlignment = HorizontalAlignment.Stretch,
                VerticalAlignment = VerticalAlignment.Stretch
            )

        boardHost.Children.Add(board)

        Grid.SetRow(boardHost, 1)
        leftPanel.Children.Add(boardHost)

        Grid.SetColumn(leftPanel, 0)
        root.Children.Add(leftPanel)

        let rightPanel =
            StackPanel(
                Orientation = Orientation.Vertical,
                Margin = Thickness(30.0, 10.0, 10.0, 10.0)
            )

        rightPanel.Children.Add(gameRecordScroll)

        Grid.SetColumn(rightPanel, 1)
        root.Children.Add(rightPanel)

        this.Content <- root
        refreshUi ()

        if controllerFor state.NextToMove = Computer then
            runOneComputerMove ()