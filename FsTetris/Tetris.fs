namespace FsTetris
open System
open System.Collections
open System.Diagnostics
open System.Threading.Tasks

(*
テトリスに必要な情報
・画面の横幅
・画面の縦幅
・ブロックの位置
・積み上がったブロック全体の情報（スクリーン情報）
・得点
・経過時間　（入力動作の監視、描画タイミング、ブロックの移動タイミング、ブロックの回転タイミング）
・ブロックの落下計算間隔計算関数ポインタ（点数から速度を計算する）
・現在の入力動作　（入力情報）
・動作処理後の初期入力動作（入力情報がない場合の初期値）
・入力を受け取るための関数ポインタ (とりあえず Task<'a> で受け取れる形と限定する。)
・独自入力値をTetrisInputBehaviorに変換する関数ポインタ
*)

/// Behavior definition of Tetris
type TetrisInputBehavior =
    /// No Action
    | None
    /// Moved to the left block
    | Left
    /// Moved to the right block
    | Right
    /// Turning it counterclockwise block
    | LeftTurn
    /// Turning it clockwise block
    | RightTurn
    /// Fall to the block
    | Fall

/// All the configuration information of Tetris
type TetrisConfig<'a> = {
        /// Screen Width
        Width : int
        /// Screen Height
        Height : int
        /// Screen region
        Region : int
        /// Fall Block
        BlockBit : int list
        /// Screen
        ScreenBit : int list
        /// Score
        Score : int64
        /// Interval calculation of the block fall
        /// int : The height of the block wich was extinguished
        /// int64 : current score
        /// int64 : calculation interval time (ms)
        IntervalBlockFallTime : int -> int64 -> int64
        /// State value of block behavior
        InputBehavior : TetrisInputBehavior
        /// Current Behavior Task
        InputBehaviorTask : Task<'a>
        /// Create new Task for custom behavior
        CreateInputTask : unit -> Task<'a>
        /// Tetris behavior converted for a custom behavior
        ConvertToTetrisBehavior : 'a -> TetrisInputBehavior
    }

/// Tetris
module GameTetris =
    /// dropping the new block
    let getFallBlock (conf : TetrisConfig<'a>) =
        let b = TetrisCommon.getFallBlock()
        let l = b.Length
        List.append
            <| [ for y = 1 to (4-l) do yield 0 ]@b
            <| [ for y = 0 to conf.Height do yield 0]
    /// calculation of input behavior
    let behaviorBlock (block : int list) (behavior : TetrisInputBehavior) =
        let op_lr =
            match behavior with
            | TetrisInputBehavior.Left -> fun a b -> b <<< a
            | TetrisInputBehavior.Right -> fun a b -> b >>> a
            | _ -> (*)
        List.map (op_lr 1) block
    /// calculation of block and screen
    let calcBlockAndScreen (conf : TetrisConfig<'a>) =
        // block of state after the movement
        let mb =
            if conf.BlockBit |> Seq.forall ((=)0) then
                getFallBlock conf
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
        (if wm then mb else getFallBlock conf), sb

    let run (config : TetrisConfig<'a>) =
        let stopWatch = new Stopwatch()
        stopWatch.Start()
        let rec loop (conf : TetrisConfig<'a>) (calc_flag:bool) =
            seq {
                // calculation
                let conf2 =
                    if calc_flag then
                        let (block_bit, screen_bit) = calcBlockAndScreen conf
                        { conf with
                            BlockBit = block_bit
                            ScreenBit = screen_bit
                            Score = conf.Score
                            InputBehavior = TetrisInputBehavior.None
                        }
                    else
                        // Only calculate the input of the state
                        let t = conf.InputBehaviorTask
                        let b = if t.IsCompleted then conf.ConvertToTetrisBehavior t.Result else conf.InputBehavior
                        let bt = if t.IsCompleted then conf.CreateInputTask() else conf.InputBehaviorTask
                        { conf with
                            InputBehavior = b
                            InputBehaviorTask = bt }
                // Wait until the end of the computation time block after the other calculation is finished
                let interval = config.IntervalBlockFallTime 4 conf2.Score
                // output
                if calc_flag then yield conf2
                // recursive loop
                yield! loop conf2 ((stopWatch.ElapsedMilliseconds % interval) = 0L)
            }
        loop
            <| { config with
                    Region = config.Height + 5
                    BlockBit = [ for y = 1 to (config.Height + 5) do yield 0 ]
                    ScreenBit = [ for y = 1 to (config.Height + 4) do yield 0 ]@[0x7FFFFFFF] }
            <| true
        |> Seq.map (fun conf ->
            let f (l:int list) =
                let arr = Array.ofList l
                List.ofArray <| arr.[4..(conf.Height + 3)]
            { conf with
                BlockBit = f conf.BlockBit
                ScreenBit = f conf.ScreenBit })
