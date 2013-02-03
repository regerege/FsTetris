namespace FsTetris
open System

/// テトリス設定情報
type TetrisConfig (input : KeyInputHandler, fps_time : int64) =
    /// キーイベントの取得
    member x.KeyInputEvent = input
    /// FPSタイマーの既定値を取得
    member x.FpsTime = fps_time

