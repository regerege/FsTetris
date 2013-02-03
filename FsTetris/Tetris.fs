namespace FsTetris
open System
open System.Collections
open System.Diagnostics

/// 画面描画用イベントハンドラ
type TetrisUpdateHandler = delegate of bits : int list -> unit

/// テトリスゲーム クラス
type Tetris (config : TetrisConfig) =
    /// FPS用タイマー
    let _fpsTimer = new Stopwatch()
    /// ブロックタイマー
    let _blockTimer = new Stopwatch()
    /// 描画イベント
    let _screenEvent = new Event<int list>()

    /// <summary>ゲームのメインコード</summary>
    /// <param name="bits">ブロックの配置情報</param>
    let rec gameStart (bits : int list) (blockbit : int list) : unit =
        if config.FpsTime <= _fpsTimer.ElapsedMilliseconds then
            _screenEvent.Trigger(
                Seq.zip bits blockbit
                |> Seq.map (fun (a,b) -> a ||| b)
                |> Seq.toList)
            _fpsTimer.Restart()

        let blockbit2 =
            if 1000L <= _blockTimer.ElapsedMilliseconds then
                _blockTimer.Restart()
                if blockbit |> Seq.forall ((=)0) then
                    [
                        for y in 1..30 ->
                            if y = 1 then 0b00000001111000000000
                            else 0
                    ]
                else
                    [0]@blockbit |> Seq.take (blockbit.Length) |> Seq.toList
            else blockbit

        gameStart bits blockbit2

    /// ゲームの実行
    member x.Run() =
        _fpsTimer.Start()
        _blockTimer.Start()
        gameStart
            <| [for y in 1..30 -> 0 ]
            <| [for y in 1..30 -> 0 ]
        
    /// 画面描画イベント
    member x.ScreenUpdate = _screenEvent.Publish

