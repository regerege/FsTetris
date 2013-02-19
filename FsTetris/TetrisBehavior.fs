namespace FsTetris
open System.Diagnostics

module TetrisBehavior =
    /// or for the tuple
    let inline private union (b1, b2) = b1 ||| b2
    /// and for the tuple
    let inline private collision (b1, b2) = b1 &&& b2
    /// Disjunction between the list
    let private unionBlock (a : int list) (b : int list) =
        Seq.zip a b |> Seq.map union |> Seq.toList
    /// Disjunction AND between the list
    let private collisionBlock (a : int list) (b : int list) =
        Seq.zip a b |> Seq.map collision |> Seq.toList

    /// Create an empty block
    let private createBlankBlock (height : int) = [ for i = 1 to height do yield 0 ]
    
    /// Check collision
    let private checkCollisionBit (a : int list) (b : int list) =
        Seq.zip a b
        |> Seq.map collision
        |> Seq.exists ((<)0)
    /// Check not collision
    let private checkCollision (conf : TetrisConfig<'a>) =
        checkCollisionBit conf.BlockBit conf.ScreenBit

    /// Calculation Move after block
    let private calcMoveBlock (n : int) (block : int list) =
        if block.Length < (abs n) then block
        elif 0 < n then
            [ for i = 1 to n do yield 0]@(block |> Seq.take (block.Length - n) |> Seq.toList)
        elif n < 0 then
            let n = abs n
            (block |> Seq.skip n |> Seq.toList)@[ for i = 1 to n do yield 0]
        else block
        
    /// Moving Block Sequence
    let private seqMoveBlock (conf : TetrisConfig<'a>) =
        Seq.unfold (fun b -> Some (b,calcMoveBlock 1 b)) conf.BlockBit

    /// Moving block
    let moveBlock (conf : TetrisConfig<'a>) =
        if TetrisBlock.isNullOrEmpty conf.BlockBit then
            { conf with BlockBit = TetrisBlock.getFallBlock conf.Height }
        else
            let b = seqMoveBlock conf |> Seq.nth 1
            let s = conf.ScreenBit
            if collisionBlock b s |> Seq.forall ((=)0) then
                { conf with BlockBit = b }
            else
                { conf with
                    BlockBit = createBlankBlock s.Length
                    ScreenBit = unionBlock conf.BlockBit s }

    /// Navigate to the deepest part of determining the contents of conf
    let private moveDeepest (conf : TetrisConfig<'a>) =
        let s = conf.ScreenBit
        let b =
            seqMoveBlock conf
            |> Seq.scan (fun (a,b) bit -> b,bit) ([],[])
            |> Seq.skipWhile (snd >> (=)[])
            |> Seq.find (snd >> checkCollisionBit s)
            |> fst
        { conf with
            BlockBit = createBlankBlock s.Length
            ScreenBit = unionBlock b s }
        
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
            || Seq.zip b s |> Seq.map (collision >> (<)0) |> Seq.reduce (||)
        let b2 = if notMove then conf.BlockBit else b
        { conf with BlockBit = b2 }

    /// Moving block to left
    let private moveLeft (conf : TetrisConfig<'a>) =
        let s = conf.ScreenBit
        let b = List.map (fun n -> n <<< 1) conf.BlockBit
        let notMove =
            (conf.BlockBit |> Seq.reduce(|||) >>> (conf.Width - 1)) &&& 1 = 1
            || Seq.zip b s |> Seq.map (collision >> (<)0) |> Seq.reduce (||)
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

