namespace FsTetris.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsTetris
open FsTetris.TetrisCommon

[<TestClass>]
type TetrisBlockTest () =
    let Blocks = [
        // ■■ 
        [ [2;] ]
        // 　■
        // ■■ 
        [ [1;3;] ]
        // ■■
        // ■　
        [ [3;1;] ]
        // ■■
        // ■■
        [ [2;2;] ]
        // ■■■■
        [ [15]; [1;1;1;1;] ]
        // 　■■
        // ■■　
        [ [3;6;]; [2;3;1;] ]
        // ■■
        // 　■■　
        [ [6;3;]; [1;3;2;] ]
        // ■     100
        // ■■■ 111
        [ [4;7;]
          [3;2;2;]
          [7;1;]
          [1;1;3;] ]
        // 　　■
        // ■■■
        [ [1;7;]
          [2;2;3;]
          [7;4;]
          [3;1;1;] ]
        // 　■
        // ■■■
        [ [2;7;]
          [2;3;2;]
          [7;2;]
          [1;3;1;] ]
        // ■　■
        // ■■■
        [ [5;7;]
          [3;2;3;]
          [7;5;]
          [3;1;3;] ]
    ]

    [<TestMethod>]
    member x.TetrisBlockTest_CalcTurnTest () =
        Blocks
        |> Seq.iter x.CalcTurnTest

    /// ブロックの回転は漸化式で表せられ、特定の周期ごとに同じ数値が現れるため、自身以降の計算結果の集合を期待値と付き合わせる。
    /// Rotation of a block can be expressed by a recurrence formula,
    /// and since the same numerical value appears every specific period,
    /// a set of calculation results after itself is associated with an expected value.
    member x.CalcTurnTest l =
        match l with
        | [] | [_] -> ()
        | _ ->
            let h = List.head l
            let w = List.length l;
            let s b t = Seq.unfold (fun a -> Some (a, x.CalcTurn t a)) b
            let expected = l

            // 引数 l の先頭の値を含めない形で次の周期からのリストを取得する。
            // Skip to the next cycle to avoid using the value received from the argument.

            let actualLeft = s h BlockBehavior.TurnLeft |> Seq.skip 1 |> Seq.take w |> Seq.toList |> List.rev;
            Assert.AreEqual(expected, actualLeft)

            let actualRight = s h BlockBehavior.TurnRight |> Seq.skip w |> Seq.take w |> Seq.toList;
            Assert.AreEqual(expected, actualRight)

    // This is Test Target Expression
    member x.CalcTurn bb block =
        let f mask (shift,bit) =  (bit >>> mask &&& 1) <<< shift
        let w = block |> List.reduce (|||) |> bitcount |> fun x -> x - 1
        let h = List.length block - 1
        let rotate, masks =
            match bb with
            | BlockBehavior.TurnLeft -> [h..(-1)..0], [0..w]
            | BlockBehavior.TurnRight -> [0..h], [w..(-1)..0]
        [ for m in masks -> List.zip rotate block |> List.map (f m) |> List.reduce(|||) ]
