namespace MorrisGui

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading

type MainWindow() as this =
    inherit Window()

    let mutable state = MorrisBoard.initialState
    let mutable lastMoveDescription = "None"

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

    let describeAction player action =
        match action with
        | Place p ->
            sprintf "%s placed at %d" (playerName player) p
        | Move (fromPoint, toPoint) ->
            sprintf "%s moved %d -> %d" (playerName player) fromPoint toPoint
        | Remove p ->
            sprintf "%s removed at %d" (playerName player) p

    let board =
        BoardControl(
            Width = 700.0,
            Height = 700.0,
            HorizontalAlignment = HorizontalAlignment.Center,
            VerticalAlignment = VerticalAlignment.Center)

    let statusText =
        TextBlock(
            Text = state.Status,
            FontSize = 18.0,
            Margin = Thickness(10.0),
            TextWrapping = TextWrapping.Wrap)

    let turnText =
        TextBlock(
            Text = sprintf "Turn: %A" state.NextToMove,
            FontSize = 16.0,
            Margin = Thickness(10.0))

    let lastMoveText =
        TextBlock(
            Text = "Last move: None",
            FontSize = 16.0,
            Margin = Thickness(10.0),
            TextWrapping = TextWrapping.Wrap)

    let thinkingText =
        TextBlock(
            Text = "",
            FontSize = 16.0,
            Margin = Thickness(10.0),
            Foreground = Brushes.DarkRed,
            TextWrapping = TextWrapping.Wrap)

    let resetButton =
        Button(
            Content = "Reset",
            Margin = Thickness(10.0),
            HorizontalAlignment = HorizontalAlignment.Left)

    let setThinking isThinking =
        thinkingText.Text <-
            if isThinking then
                sprintf "%s is thinking..." (playerName state.NextToMove)
            else
                ""

    let refreshUi () =
        board.State <- state
        board.InvalidateVisual()

        let baseStatus =
            if state.Phase <> GameOver && controllerFor state.NextToMove = Computer then
                sprintf "%s is thinking..." (playerName state.NextToMove)
            else
                state.Status

        statusText.Text <- baseStatus
        lastMoveText.Text <- sprintf "Last move: %s" lastMoveDescription

        let winnerText =
            match state.Winner with
            | Some p -> sprintf "   Winner: %A" p
            | None -> ""

        turnText.Text <- sprintf "Turn: %A   Phase: %A%s" state.NextToMove state.Phase winnerText

    let rec runOneComputerMove () =
        if state.Phase <> GameOver && controllerFor state.NextToMove = Computer then
            let thinkingPlayer = state.NextToMove
            setThinking true
            refreshUi ()

            Dispatcher.UIThread.Post(
                (fun () ->
                    match MorrisBoard.NegamaxAi.chooseAction state with
                    | Some action ->
                        lastMoveDescription <- describeAction thinkingPlayer action
                        state <- MorrisBoard.applyAction action state
                        setThinking false
                        refreshUi ()

                        if state.Phase <> GameOver && controllerFor state.NextToMove = Computer then
                            Dispatcher.UIThread.Post(
                                (fun () -> runOneComputerMove ()),
                                DispatcherPriority.Background
                            )
                    | None ->
                        setThinking false
                        refreshUi ()
                ),
                DispatcherPriority.Background
            )

    do
        this.Title <- "Nine Men's Morris"
        this.Width <- 1000.0
        this.Height <- 900.0
        this.MinWidth <- 800.0
        this.MinHeight <- 800.0

        board.State <- state

        board.OnPointClicked <-
            fun idx ->
                if controllerFor state.NextToMove = Human then
                    let before = state
                    let after = MorrisBoard.clickPoint idx state

                    if not (obj.ReferenceEquals(before, after)) then
                        match before.Phase, before.Selected, after.Phase with
                        | Placing, _, _ ->
                            lastMoveDescription <- describeAction before.NextToMove (Place idx)

                        | Moving, None, _ ->
                            // First click only selects a stone; do not treat as a move yet.
                            ()

                        | Moving, Some fromPoint, _ when fromPoint <> idx ->
                            match after.Status with
                            | "Destination is occupied"
                            | "That move is not along a line"
                            | "When flying, choose any empty point" ->
                                ()
                            | _ ->
                                lastMoveDescription <- describeAction before.NextToMove (Move(fromPoint, idx))

                        | Removing, _, _ ->
                            lastMoveDescription <- describeAction before.NextToMove (Remove idx)

                        | _ ->
                            ()

                    state <- after
                    setThinking false
                    refreshUi ()

                    if controllerFor state.NextToMove = Computer && state.Phase <> GameOver then
                        Dispatcher.UIThread.Post(
                            (fun () -> runOneComputerMove ()),
                            DispatcherPriority.Background
                        )

        resetButton.Click.Add(fun _ ->
            state <- MorrisBoard.initialState
            lastMoveDescription <- "None"
            setThinking false
            refreshUi ()

            if controllerFor state.NextToMove = Computer then
                Dispatcher.UIThread.Post(
                    (fun () -> runOneComputerMove ()),
                    DispatcherPriority.Background
                ))

        let root =
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
        root.Children.Add(topPanel)

        let boardHost =
            Grid(
                HorizontalAlignment = HorizontalAlignment.Stretch,
                VerticalAlignment = VerticalAlignment.Stretch
            )

        boardHost.Children.Add(board)

        Grid.SetRow(boardHost, 1)
        root.Children.Add(boardHost)

        this.Content <- root
        refreshUi ()

        if controllerFor state.NextToMove = Computer then
            runOneComputerMove ()