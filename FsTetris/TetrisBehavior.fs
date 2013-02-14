namespace FsTetris
open System.Diagnostics

module TetrisBehavior =
    // Converts the instruction Tetris instruction is input.
    let getProcess (conf : TetrisConfig<'a>) =
        let t = conf.InputBehaviorTask
        let b = if t.IsCompleted then conf.ConvertToTetrisBehavior t.Result else conf.InputBehavior
        let bt = if t.IsCompleted then conf.CreateInputTask() else conf.InputBehaviorTask
        { conf with
            InputBehavior = b
            InputBehaviorTask = bt }

//    /// Navigate to the deepest part of determining the contents of conf
//    let moveDeepest (conf : TetrisConfig<'a>) =
//        if conf.InputBehavior <> TetrisInputBehavior.Fall then conf else
//        /// 1.ブロックを移動する。
//        /// 2.スクリーン上のブロックとぶつかるかを判定
//        /// 3.true なら None (処理を終了し、前回の移動結果を返す)
//        /// 4.false なら移動結果を返す。
//        /// アルゴリズムの計算量：　O( distance * height )
//        Seq.unfold (fun bl ->
//            [0]@bl |> Seq.tak
//
//        let distance =
//            Seq.zip conf.BlockBit conf.ScreenBit
//            |> Seq.findIndex (fun (b,s) -> 0 < (b &&& s))
//            |> fun n -> n - 1
//        let b =
//            [ for i = 0 to distance do yield 0 ] @ conf.BlockBit
//            |> Seq.take distance
//            |> Seq.toList
//        let s = Seq.zip b conf.ScreenBit |> Seq.map (fun (b,s) -> b ||| s) |> Seq.toList
//        { conf with BlockBit = b; ScreenBit = s }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 書き直し
    /// calculation of input behavior
    let behaviorBlock (block : int list) (behavior : TetrisInputBehavior) =
        let op_lr =
            match behavior with
            | TetrisInputBehavior.Left -> fun a b -> b <<< a
            | TetrisInputBehavior.Right -> fun a b -> b >>> a
            | _ -> (*)
        List.map (op_lr 1) block

    /// calculation of block and screen
    let calcBehaviorOnBlockAndScreen (conf : TetrisConfig<'a>) =
        let block = TetrisBlock.getFallBlock conf.Height
        // block of state after the movement
        let mb =
            if conf.BlockBit |> Seq.forall ((=)0) then
                block
            else
                let block = behaviorBlock conf.BlockBit conf.InputBehavior
                [0]@block |> Seq.take (conf.Region) |> Seq.toList
        // Whether movement
        let wm =
            Seq.zip mb conf.ScreenBit
            |> Seq.map (fun (a,b) -> a &&& b)
            |> Seq.forall ((=)0)
        // if non-movement to the screen
        let sb =
            if wm then conf.ScreenBit
            else
                Seq.zip conf.BlockBit conf.ScreenBit
                |> Seq.map (fun (a,b) -> a ||| b)
                |> Seq.toList
        { conf with
            BlockBit = (if wm then mb else block)
            ScreenBit = sb
            Score = conf.Score
            InputBehavior = TetrisInputBehavior.None
        }
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

