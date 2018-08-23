namespace FsTetris
open System
open System.Diagnostics
open System.Threading.Tasks

module Program =
    /// create new key input task
    let getAsyncKeyInput () =
        Async.StartAsTask (async { let x = System.Console.ReadKey() in return x.Key })
    let convertBehavior (key : ConsoleKey) =
        match key with
        | ConsoleKey.LeftArrow -> TetrisInputBehavior.LeftTurn
        | ConsoleKey.RightArrow -> TetrisInputBehavior.RightTurn
        | ConsoleKey.S -> TetrisInputBehavior.Left
        | ConsoleKey.F -> TetrisInputBehavior.Right
        | ConsoleKey.Spacebar -> TetrisInputBehavior.Fall
        | ConsoleKey.P -> TetrisInputBehavior.Pause
        | _ -> TetrisInputBehavior.None

    /// create new tetris config
    let getTetrisConfig() : TetrisRunConfig<ConsoleKey> =
        {
            Width = 20
            Height = 30
            IntervalBlockFallTime = fun _ _ time -> time % 500L = 0L
            CreateInputTask = getAsyncKeyInput
            ConvertToTetrisBehavior = convertBehavior
        }
