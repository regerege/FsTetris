namespace FsTetris
open System.Diagnostics
open TetrisBehavior

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
    let rec private fspLoop (sw : Stopwatch)  (conf : TetrisConfig<'a>) =
        seq {
            let conf2 = TetrisBehavior.getProcess conf              // input
            if conf2.InputBehavior <> TetrisInputBehavior.Pause then
                // exec of the input behavior
                let confProcess = TetrisBehavior.calcProcess conf2
                // Drawn immediately after the behavior
                if sw.ElapsedMilliseconds % 60L = 0L then yield confProcess
                // Calculation and drawing fall
                // TODO: -1の数値を消滅ブロックの段差に変える
                if conf.IntervalBlockFallTime -1 sw.ElapsedMilliseconds then
                    let confMoved = TetrisBehavior.moveBlock confProcess
                    yield confMoved
                    yield! fspLoop sw confMoved
                yield! fspLoop sw confProcess       // recursion loop
            else
                if sw.ElapsedMilliseconds % 60L = 0L then yield conf2
                yield! fspLoop sw conf2                 // recursion loop
        }
    let run2 (config : TetrisConfig<'a>) =
        let stopWatch = new Stopwatch()
        stopWatch.Start()
        // game start
        fspLoop stopWatch
            <| { config with
                    Region = config.Height + 5
                    BlockBit = [ for y = 1 to (config.Height + 5) do yield 0 ]
                    ScreenBit = [ for y = 1 to (config.Height + 4) do yield 0 ]@[0xFFFFFFFF] }
        // Output up to boundary
        |> Seq.map (fun conf ->
            let f (l:int list) =
                let arr = Array.ofList l
                List.ofArray <| arr.[4..(conf.Height + 3)]
            { conf with
                BlockBit = f conf.BlockBit
                ScreenBit = f conf.ScreenBit })

    /// Running of Tetris Game
    let run (config : TetrisConfig<'a>) =
        let stopWatch = new Stopwatch()
        stopWatch.Start()
        // fps loop
        let rec loop (conf : TetrisConfig<'a>) (calc_flag:bool) =
            seq {
                let behavior_conf = getProcess conf
                // calculation
                let conf2 =
                    if calc_flag then
                        // Calculation of the Move or Turn of the Block
                        calcBehaviorOnBlockAndScreen behavior_conf
                    else
                        // Only calculate the input of the state
                        behavior_conf
                // output
                if calc_flag then yield conf2
                // recursive loop
                // Wait until the end of the computation time block after the other calculation is finished
                yield! loop conf2 <| config.IntervalBlockFallTime 4 conf2.Score
            }
        // game start
        loop
            <| { config with
                    Region = config.Height + 5
                    BlockBit = [ for y = 1 to (config.Height + 5) do yield 0 ]
                    ScreenBit = [ for y = 1 to (config.Height + 4) do yield 0 ]@[0xFFFFFFFF] }
            <| true
        // Output up to boundary
        |> Seq.map (fun conf ->
            let f (l:int list) =
                let arr = Array.ofList l
                List.ofArray <| arr.[4..(conf.Height + 3)]
            { conf with
                BlockBit = f conf.BlockBit
                ScreenBit = f conf.ScreenBit })
