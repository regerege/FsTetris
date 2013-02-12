namespace FsTetris
open System
open System.Diagnostics
open System.Threading.Tasks

module Program =
    /// create new key input task
    let getAsyncKeyInput () =
        Async.StartAsTask (async { return System.Console.ReadKey().Key; })
    let convertBehavior (key : ConsoleKey) =
        match key with
        | ConsoleKey.LeftArrow -> TetrisInputBehavior.Left
        | ConsoleKey.RightArrow -> TetrisInputBehavior.Right
        | _ -> TetrisInputBehavior.None
    /// create new tetris config
    let getTetrisConfig() : TetrisConfig<ConsoleKey> =
        {
            Width = 20
            Height = 30
            Region = 0
            BlockBit = []
            ScreenBit = []
            Score = 0L
            IntervalBlockFallTime = fun _ _ -> 500L
            InputBehavior = TetrisInputBehavior.None
            InputBehaviorTask = getAsyncKeyInput()
            CreateInputTask = getAsyncKeyInput
            ConvertToTetrisBehavior = convertBehavior
        }

    [<STAThread>]
    [<EntryPoint>]
    let Main (_) =
        Console.CursorVisible <- false
        Console.Title <- "テトリス？"
        Console.WriteLine "Game Tetris !!"
        Console.WriteLine "If you start the game Tetris Please enter any key."

        let conf = getTetrisConfig()
        GameTetris.run conf
        |> Seq.iter (fun c ->
            let bits = Seq.zip c.BlockBit c.ScreenBit |> Seq.map (fun (a,b) -> a ||| b) |> Seq.toArray
            Console.Clear()
            bits |> Seq.iter(fun bit ->
                Convert.ToString(bit, 2).PadLeft(conf.Width, '0')
                |> Seq.map(fun x -> if x = '1' then "■" else "□")
                |> Seq.reduce(+)
                |> Console.WriteLine))

        0
