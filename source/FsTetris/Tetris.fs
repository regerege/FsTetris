﻿namespace FsTetris
open System.Diagnostics

/// Tetris
module GameTetris =
    /// Frame Per Second Loop
    /// 1. Accepts input
    /// 2. Fall to the deepest
    /// 3. Rotation of the block
    /// 4. Regular movement of the block
    /// 5. Calculation of the Score
    /// 6. Repaint
    ///
    /// ブロックの消滅計算も含める。
    /// Stopwatch が停止時はゲームも停止する。
    /// ゲームの終了判定を定める。
    /// 
    let rec private fspLoop (sw : Stopwatch)  (conf : TetrisConfig) =
        seq {
            let conf2 = TetrisBehavior.getProcess conf              // input
            if conf2.InputBehavior <> TetrisInputBehavior.Pause then
                // Disappearance of the calculation screen block
                let confDisappearance,extinguishedBlock = TetrisBehavior.extinctionBlock conf2
                // exec of the input behavior
                let confProcess = TetrisBehavior.calcProcess confDisappearance
                // Drawn immediately after the behavior
                if sw.ElapsedMilliseconds % 60L = 0L then yield confProcess
                // Calculation and drawing fall
                if conf.IntervalBlockFallTime extinguishedBlock confProcess.Score sw.ElapsedMilliseconds then
                    let confMoved = TetrisBehavior.moveBlock confProcess
                    yield confMoved
                    yield! fspLoop sw confMoved
                yield! fspLoop sw confProcess       // recursion loop
            else
                if sw.ElapsedMilliseconds % 60L = 0L then yield conf2
                yield! fspLoop sw conf2                 // recursion loop
        }

    let public convertConfig (config : TetrisRunConfig) : TetrisConfig =
        {
            Width = config.Width
            Height = config.Height
//            Region = config.Height + 5
            BlockConfig = { BlockIndex = 0; TopPos = 0; RightPos = 0; BlockGroup = []; }
            BlockBit = [ for y = 1 to (config.Height + 5) do yield 0 ]
            ScreenBit = [ for y = 1 to (config.Height + 4) do yield 0 ]@[0xFFFFFFFF]
            Score = 0L
            IntervalBlockFallTime = fun _ _ time -> time % 500L = 0L
            InputBehavior = TetrisInputBehavior.None
            InputBehaviorTask = config.CreateInputTask()
            CreateInputTask = config.CreateInputTask
        }

    /// Running of Tetris Game
    let run (config : TetrisRunConfig) =
        let stopWatch = new Stopwatch()
        stopWatch.Start()
        // game start
        fspLoop stopWatch <| convertConfig config
        // Output up to boundary
        |> Seq.map (fun conf ->
            let f (l:int list) =
                let arr = Array.ofList l
                List.ofArray <| arr.[4..(conf.Height + 3)]
            { conf with
                BlockBit = f conf.BlockBit
                ScreenBit = f conf.ScreenBit })

