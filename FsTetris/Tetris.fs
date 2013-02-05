namespace FsTetris
open System
open System.Collections
open System.Diagnostics

/// キー入力ハンドラ
type KeyInputHandler = delegate of char -> char

/// テトリス設定情報
type TetrisConfig (input : KeyInputHandler, fps_time : int64) =
    /// キーイベントの取得
    member x.KeyInputEvent = input
    /// FPSタイマーの既定値を取得
    member x.FpsTime = fps_time

/// テトリスゲーム クラス
type Tetris (config : TetrisConfig) =
    /// FPS用タイマー
    let _fpsTimer = new Stopwatch()
    /// ブロックタイマー
    let _blockTimer = new Stopwatch()
    /// 描画イベント
    let _screenEvent = new Event<int list>()
    
//#region Game Main Logic
    
    /// FPS処理計算
    let _fpsLogic (lastkey:char option) (bits : int list) (blockbit : int list) : bool * char option =
        if config.FpsTime <= _fpsTimer.ElapsedMilliseconds then
            /// 画面描画イベント
            _screenEvent.Trigger(
                Seq.zip bits blockbit |> Seq.toArray |> (fun arr -> arr.[5..])
                |> Seq.map (fun (a,b) -> a ||| b)
                |> Seq.toList)
            true, lastkey
        else false, lastkey

    let _initFallBlock () =
        [
            0b00000001000000000000
            0b00000001000000000000
            0b00000001000000000000
            0b00000001000000000000
        ]

    /// ブロックの落下計算
    let _moveBlock (bits : int list) (blockbit : int list) =
            if 25L <= _blockTimer.ElapsedMilliseconds then
                if blockbit |> Seq.forall ((=)0) then
                    let b = TetrisCommon.getFallBlock()
                    let l = b.Length
                    List.append
                        <| [ for y = 1 to(4 - l) do yield 0 ]@b         // 画面外の領域 (ブロック生成部)
                        <| [ for y = 1 to 30 do yield 0 ]
                else
                    [0]@blockbit |> Seq.take (blockbit.Length) |> Seq.toList
                , true
            else blockbit, false

    /// <summary>ゲームのメインコード</summary>
    /// <param name="bits">ブロックの配置情報</param>
    let rec gameStart (lastkey:char option) (bits : int list) (blockbit : int list) : unit =
        /// その他FPSの計算
        let (reset_fps, key) = _fpsLogic lastkey bits blockbit

        /// 落下ブロックの計算
        let (blockbit2, reset_block) = _moveBlock bits blockbit

        /// タイマーを初期化後に再帰ループ
        if reset_fps then _fpsTimer.Restart()
        if reset_block then _blockTimer.Restart()
        gameStart key bits blockbit2

//#endregion

    /// ゲームの実行
    member x.Run() =
        _fpsTimer.Start()
        _blockTimer.Start()
        gameStart
            <| None
            <| [for y in 1..34 -> 0 ]
            <| [for y in 1..34 -> 0 ]
        
    /// 画面描画イベント
    [<CLIEvent>]
    member x.ScreenUpdate = _screenEvent.Publish
