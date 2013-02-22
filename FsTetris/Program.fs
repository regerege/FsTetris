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
        | ConsoleKey.LeftArrow -> TetrisInputBehavior.LeftTurn
        | ConsoleKey.RightArrow -> TetrisInputBehavior.RightTurn
        | ConsoleKey.S -> TetrisInputBehavior.Left
        | ConsoleKey.F -> TetrisInputBehavior.Right
        | ConsoleKey.Spacebar -> TetrisInputBehavior.Fall
        | ConsoleKey.P -> TetrisInputBehavior.Pause
        | _ -> TetrisInputBehavior.None

//    /// create new tetris config
//    let getTetrisConfig() : TetrisConfig<ConsoleKey> =
//        {
//            Width = 20
//            Height = 30
//            CenterPos = 0
//            Region = 0
//            BlockBit = []
//            ScreenBit = []
//            Score = 0L
//            IntervalBlockFallTime = fun _ _ time -> time % 500L = 0L
//            InputBehavior = TetrisInputBehavior.None
//            InputBehaviorTask = getAsyncKeyInput()
//            CreateInputTask = getAsyncKeyInput
//            ConvertToTetrisBehavior = convertBehavior
//        }
    /// create new tetris config
    let getTetrisConfig() : TetrisRunConfig<ConsoleKey> =
        {
            Width = 20
            Height = 30
            IntervalBlockFallTime = fun _ _ time -> time % 500L = 0L
            CreateInputTask = getAsyncKeyInput
            ConvertToTetrisBehavior = convertBehavior
        }

    [<STAThread>]
    [<EntryPoint>]
    let Main (_) =
        Console.CursorVisible <- false
        Console.Title <- "Retris!!"
        Console.WriteLine "Game Tetris !!"
        Console.WriteLine "If you start the game Tetris Please enter any key."

        let conf = getTetrisConfig()
        GameTetris.run conf
            |> Seq.iter (fun c ->
                let bits = Seq.zip c.BlockBit c.ScreenBit |> Seq.map (fun (a,b) -> a ||| b) |> Seq.toArray
                Console.Clear()

                // Output of the Score
                Console.WriteLine(String.Format("Score: {0:#,##0}", c.Score))
                Console.WriteLine()

                // Output of the Block
                bits |> Seq.iter(fun bit ->
                    Convert.ToString(bit, 2).PadLeft(conf.Width, '0')
                    |> Seq.map(fun x -> if x = '1' then "■" else "□")
                    |> Seq.reduce(+)
                    |> Console.WriteLine))
        0

