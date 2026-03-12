namespace MorrisGui

open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media

type BoardControl() as this =
    inherit Control()

    static let stateProperty : StyledProperty<GameState> =
        AvaloniaProperty.Register<BoardControl, GameState>(
            "State",
            MorrisBoard.initialState
        )

    let pointPositions (bounds: Rect) =
        let margin = 40.0
        let left = bounds.Left + margin
        let top = bounds.Top + margin
        let right = bounds.Right - margin
        let bottom = bounds.Bottom - margin
        let midX = (left + right) / 2.0
        let midY = (top + bottom) / 2.0

        let outerW = right - left
        let outerH = bottom - top

        let middleLeft = left + outerW / 6.0
        let middleRight = right - outerW / 6.0
        let middleTop = top + outerH / 6.0
        let middleBottom = bottom - outerH / 6.0

        let innerLeft = left + outerW / 3.0
        let innerRight = right - outerW / 3.0
        let innerTop = top + outerH / 3.0
        let innerBottom = bottom - outerH / 3.0

        [|
            Point(left, top)                   // 0
            Point(midX, top)                   // 1
            Point(right, top)                  // 2

            Point(middleLeft, middleTop)       // 3
            Point(midX, middleTop)             // 4
            Point(middleRight, middleTop)      // 5

            Point(innerLeft, innerTop)         // 6
            Point(midX, innerTop)              // 7
            Point(innerRight, innerTop)        // 8

            Point(left, midY)                  // 9
            Point(middleLeft, midY)            // 10
            Point(innerLeft, midY)             // 11

            Point(innerRight, midY)            // 12
            Point(middleRight, midY)           // 13
            Point(right, midY)                 // 14

            Point(innerLeft, innerBottom)      // 15
            Point(midX, innerBottom)           // 16
            Point(innerRight, innerBottom)     // 17

            Point(middleLeft, middleBottom)    // 18
            Point(midX, middleBottom)          // 19
            Point(middleRight, middleBottom)   // 20

            Point(left, bottom)                // 21
            Point(midX, bottom)                // 22
            Point(right, bottom)               // 23
        |]

    let lines =
        [|
            0, 1; 1, 2
            3, 4; 4, 5
            6, 7; 7, 8
            9, 10; 10, 11
            12, 13; 13, 14
            15, 16; 16, 17
            18, 19; 19, 20
            21, 22; 22, 23

            0, 9; 9, 21
            3, 10; 10, 18
            6, 11; 11, 15
            1, 4; 4, 7
            16, 19; 19, 22
            8, 12; 12, 17
            5, 13; 13, 20
            2, 14; 14, 23
        |]

    member _.State
        with get() = this.GetValue(stateProperty)
        and set(value) =
            this.SetValue(stateProperty, value) |> ignore
            this.InvalidateVisual()

    member val OnPointClicked : (int -> unit) = ignore with get, set

    static member StateProperty = stateProperty

    override _.Render(context) =
        base.Render(context)

        let bounds = this.Bounds
        let points = pointPositions bounds

        let boardPen = Pen(Brushes.Black, 2.0)
        let selectedPen = Pen(Brushes.DarkRed, 3.0)
        let removablePen = Pen(Brushes.ForestGreen, 3.0)

        let removableTargets =
            if this.State.Phase = Removing then
                MorrisBoard.removablePoints this.State.NextToMove.Other this.State
                |> Set.ofList
            else
                Set.empty

        for (a, b) in lines do
            context.DrawLine(boardPen, points[a], points[b])

        for i = 0 to points.Length - 1 do
            let p = points[i]
            let radius = 14.0

            let fill =
                match this.State.Board[i] with
                | Empty -> Brushes.Beige
                | Occupied White -> Brushes.Red
                | Occupied Black -> Brushes.Blue

            let border =
                match this.State.Selected with
                | Some s when s = i -> selectedPen
                | _ when removableTargets.Contains i -> removablePen
                | _ -> boardPen

            context.DrawEllipse(fill, border, p, radius, radius)

            let labelBrush =
                match this.State.Board[i] with
                | Occupied _ -> Brushes.White
                | Empty -> Brushes.DarkBlue            

            let ft =
                FormattedText(
                    i.ToString(),
                    CultureInfo.InvariantCulture,
                    FlowDirection.LeftToRight,
                    Typeface.Default,
                    12.0,
                    labelBrush
                )

            context.DrawText(ft, Point(p.X - 6.0, p.Y - 7.0))

    override _.OnPointerPressed(e: PointerPressedEventArgs) =
        base.OnPointerPressed(e)

        let pos = e.GetPosition(this)
        let points = pointPositions this.Bounds
        let hitRadius = 28.0

        let mutable found = None

        for i = 0 to points.Length - 1 do
            let dx = points[i].X - pos.X
            let dy = points[i].Y - pos.Y
            let dist2 = dx * dx + dy * dy
            if dist2 <= hitRadius * hitRadius then
                found <- Some i

        match found with
        | Some idx -> this.OnPointClicked idx
        | None -> ()