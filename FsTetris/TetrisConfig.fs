﻿namespace FsTetris
open System
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