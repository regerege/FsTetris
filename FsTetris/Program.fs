namespace FsTetris
open System
open System.Diagnostics
open System.Windows

module Program =
    let fps = 60L
    let gameFps = 2000L
    let fpsTimer = new Stopwatch()
    let gameTimer = new Stopwatch()
    let mw = new MainWindow()
    let items = mw.Items
    let maxY = items.Length
    let maxX = items.[0].Length

///////////////////////////////////////////////////////////////////
    let rec fpsLoop (x,y) =
        let (x2,y2) =
            if fps <= fpsTimer.ElapsedMilliseconds then
                fpsTimer.Restart()
                if gameFps <= gameTimer.ElapsedMilliseconds then
                    // TODO: 処理
                    if 0 <= y then
                        items.[y].[x].Tag <- 0
                    if y < maxY then
                        items.[y].[x].Tag <- 2
                    gameTimer.Restart()
                    x,((y+1) % 30)
                else x,y
            else x,y
        // UIイベント
        mw.DoEvents()
        // 次のループ
        fpsLoop (x2, y2)

///////////////////////////////////////////////////////////////////

    [<STAThread>]
    [<EntryPoint>]
    let Main (_) =
        mw.Window.Show()
        fpsTimer.Start()
        gameTimer.Start()
        fpsLoop (10,0)
        0
