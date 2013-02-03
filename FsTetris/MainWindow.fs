namespace FsTetris

open System
open System.Collections.ObjectModel
open System.ComponentModel
open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Threading

type MainWindow () as x =

    let _wpf = Application.LoadComponent(new Uri("MainWindow.xaml", System.UriKind.Relative)) :?> Window
    let _display = _wpf.FindName("gridDisplay") :?> Grid
    let _items =
        [
            for y in 1..30 ->
                [
                    for x in 1..20 ->
                        _display.FindName(sprintf "P%02d%02d" x y) :?> Rectangle
                ]
        ]

    do
        _wpf.DataContext <- x

    /// Windowを取得する。
    member x.Window = _wpf
    /// 描画領域コントロールを取得する。
    member x.Display = _display
    /// セルリスト
    member x.Items = _items

    member x.DoEvents() =
        let frame = new DispatcherFrame()
        let exitFrameCallback =
            new DispatcherOperationCallback(fun o -> (o :?> DispatcherFrame).Continue <- false; null)
        let exitOperation =
            Dispatcher.CurrentDispatcher.BeginInvoke(
                DispatcherPriority.Background, exitFrameCallback, frame)
        Dispatcher.PushFrame(frame)

        if (exitOperation.Status <> DispatcherOperationStatus.Completed) then
            ignore <| exitOperation.Abort()

