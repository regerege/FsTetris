namespace FsTetris
open System

module Program =
    let convertBehavior (key : ConsoleKey) =
        match key with
        | ConsoleKey.Z          -> TetrisInputBehavior.TurnLeft
        | ConsoleKey.X          -> TetrisInputBehavior.TurnRight
        | ConsoleKey.LeftArrow  -> TetrisInputBehavior.Left
        | ConsoleKey.RightArrow -> TetrisInputBehavior.Right
        | ConsoleKey.Spacebar   -> TetrisInputBehavior.Fall
        | ConsoleKey.C          -> TetrisInputBehavior.Pause
        | _                     -> TetrisInputBehavior.None

    /// create new key input task
    let getAsyncKeyInput () =
        Async.StartAsTask (async {
            let x = System.Console.ReadKey()
            return convertBehavior x.Key })

    /// create new tetris config
    let getTetrisConfig() : TetrisRunConfig =
        {
            Width = 20
            Height = 30
            IntervalBlockFallTime = fun _ _ time -> time % 500L = 0L
            CreateInputTask = getAsyncKeyInput
        }
