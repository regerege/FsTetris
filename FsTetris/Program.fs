namespace FsTetris
open System
open System.Diagnostics
open System.Threading.Tasks

module Program =
    let config = new TetrisConfig(null, 60L)
    let game = new Tetris(config)

    /// キー取得用非同期タスク
    let getAsyncKeyInput () =
        Async.StartAsTask (async { return System.Console.ReadKey().Key; })

    let mapKey (key : ConsoleKey) =
        match key with
        | ConsoleKey.LeftArrow -> -1,0
        | ConsoleKey.RightArrow -> 1,0
        | ConsoleKey.DownArrow -> 0,1
        | _ -> 0,0

    [<STAThread>]
    [<EntryPoint>]
    let Main (_) =
        Console.CursorVisible <- false
        Console.Title <- "テトリス？"
        Console.WriteLine "Game Tetris !!"
        Console.WriteLine "If you start the game Tetris Please enter any key."
//        game.ScreenUpdate.Add(fun bits ->
//            Console.Clear()
//            bits |> Seq.iter(fun bit ->
//                Convert.ToString(bit, 2).PadLeft(20, '0')
//                |> Seq.map(fun x -> if x = '1' then "■" else "□")
//                |> Seq.reduce(+)
//                |> Console.WriteLine))
//        game.Run()
        
        GameTetris.run (ConsoleKey.Escape) getAsyncKeyInput mapKey
        |> Seq.iter (fun bits ->
            Console.Clear()
            bits |> Seq.iter(fun bit ->
                Convert.ToString(bit, 2).PadLeft(20, '0')
                |> Seq.map(fun x -> if x = '1' then "■" else "□")
                |> Seq.reduce(+)
                |> Console.WriteLine))
        0
