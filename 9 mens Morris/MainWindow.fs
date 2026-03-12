namespace MorrisGui

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media

type MainWindow() as this =
    inherit Window()

    let mutable state = MorrisBoard.initialState

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

    let resetButton =
        Button(
            Content = "Reset",
            Margin = Thickness(10.0),
            HorizontalAlignment = HorizontalAlignment.Left)

    let refreshUi () =
        board.State <- state
        board.InvalidateVisual()
        statusText.Text <- state.Status

        let winnerText =
            match state.Winner with
            | Some p -> sprintf "   Winner: %A" p
            | None -> ""

        turnText.Text <- sprintf "Turn: %A   Phase: %A%s" state.NextToMove state.Phase winnerText

    do
        this.Title <- "Nine Men's Morris"
        this.Width <- 1000.0
        this.Height <- 900.0
        this.MinWidth <- 800.0
        this.MinHeight <- 800.0

        board.State <- state
        board.OnPointClicked <-
            fun idx ->
                state <- MorrisBoard.clickPoint idx state
                refreshUi ()

        resetButton.Click.Add(fun _ ->
            state <- MorrisBoard.initialState
            refreshUi ())

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