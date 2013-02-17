namespace FsTetris
open System.Diagnostics

module TetrisBehavior =
    let inline private unionBlock (b1, b2) = b1 ||| b2
    let inline private collisionBlock (b1, b2) = 0 < (b1 &&& b2)

    /// Moving block
    let moveBlock (conf : TetrisConfig<'a>) =
        if TetrisBlock.isNullOrEmpty conf.BlockBit then
            { conf with BlockBit = TetrisBlock.getFallBlock conf.Height }
        else
            let s = conf.ScreenBit
            let b = conf.BlockBit
            if 0 < b.[b.Length - 2] then
                { conf with
                    BlockBit = [ for i = 1 to b.Length do yield 0]
                    ScreenBit = Seq.zip b s |> Seq.map unionBlock |> Seq.toList }
            else
                let b2 = [0]@(b |> Seq.take (b.Length - 1) |> Seq.toList)
                let checkCollision = Seq.zip b2 s |> Seq.map collisionBlock |> Seq.reduce (||)
                if checkCollision then
                    { conf with
                        BlockBit = [ for i = 1 to b.Length do yield 0 ]
                        ScreenBit = Seq.zip b s |> Seq.map unionBlock |> Seq.toList }
                else { conf with BlockBit = b2 }

    /// Navigate to the deepest part of determining the contents of conf
    let private moveDeepest (conf : TetrisConfig<'a>) =
        let b = conf.BlockBit
        let s = conf.ScreenBit
        let n =
            b |> Seq.mapi (fun i n -> i,n)
            |> Seq.filter (snd >> (<)0)
            |> Seq.map (fun (bi,bn) ->
                s |> Seq.mapi (fun si sn -> si, bn &&& sn)
                |> Seq.filter (snd >> (<)0)
                |> Seq.head
                |> fun (si,sn) -> si)
            |> Seq.max
        let height = b |> Seq.filter ((<)0) |> Seq.length
        let blank = b |> Seq.findIndex ((<)0)
        let b2 = b |> Seq.take (b.Length - (n - height - blank)) |> Seq.toList
        { conf with
            BlockBit = [ for i = 1 to s.Length do yield 0 ]
            ScreenBit =
                [ for i = 1 to (b.Length - b2.Length) do yield 0 ]@b2
                |> Seq.zip s |> Seq.map unionBlock |> Seq.toList
        }

// //思いついたら修正
//    /// Rotation of the block
//    let rotate (conf : TetrisConfig<'a>) =
//        let s = conf.ScreenBit
//        // Remove the blank line, get the Y coordinate of the block
//        let b,p =
//            conf.BlockBit
//            |> Seq.fold (fun (block, pos, inc_flg) line ->
//                if 0 < line then block@[line] else block
//                , pos + if inc_flg then 1 else 0
//                , inc_flg && line = 0) ([],0,true)
//            |> fun (a,b,_) -> a,b
//        let hight = b.Length
//        let width = b |> List.reduce(|||) |> TetrisCommon.bitcount
//        let diff = max(

    /// Moving block to right
    let private moveRight (conf : TetrisConfig<'a>) =
        let s = conf.ScreenBit
        let b = List.map (fun n -> n >>> 1) conf.BlockBit
        let notMove =
            (conf.BlockBit |> Seq.reduce(|||)) &&& 1 = 1
            || Seq.zip b s |> Seq.map collisionBlock |> Seq.reduce (||)
        let b2 = if notMove then conf.BlockBit else b
        { conf with BlockBit = b2 }

    /// Moving block to left
    let private moveLeft (conf : TetrisConfig<'a>) =
        let s = conf.ScreenBit
        let b = List.map (fun n -> n <<< 1) conf.BlockBit
        let notMove =
            (conf.BlockBit |> Seq.reduce(|||) >>> (conf.Width - 1)) &&& 1 = 1
            || Seq.zip b s |> Seq.map collisionBlock |> Seq.reduce (||)
        let b2 = if notMove then conf.BlockBit else b
        { conf with BlockBit = b2 }

    /// Mapping of the operation and function
    let private mapProcessFunc =
        [
            TetrisInputBehavior.None, id
            TetrisInputBehavior.Pause, id
            TetrisInputBehavior.LeftTurn, id
            TetrisInputBehavior.RightTurn, id

            TetrisInputBehavior.Fall, moveDeepest
            TetrisInputBehavior.Right, moveRight
            TetrisInputBehavior.Left, moveLeft
        ]

    // Converts the instruction Tetris instruction is input.
    let getProcess (conf : TetrisConfig<'a>) =
        let t = conf.InputBehaviorTask
        let b = if t.IsCompleted then conf.ConvertToTetrisBehavior t.Result else conf.InputBehavior
        let bt = if t.IsCompleted then conf.CreateInputTask() else conf.InputBehaviorTask
        { conf with
            InputBehavior = b
            InputBehaviorTask = bt }

    /// Calculate the input state
    let calcProcess (conf : TetrisConfig<'a>) =
        if TetrisBlock.isNullOrEmpty conf.BlockBit then conf
        else
            let conf2 =
                snd <| List.find (fst >> (=)conf.InputBehavior) mapProcessFunc
                    <| conf
            { conf2 with InputBehavior = TetrisInputBehavior.None }

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

