open FsTetris
open System

let writeScore (score:int64) =
    Console.SetCursorPosition(7,0)
    Console.Write("{0:#,##0}", score)

let writeBlock (h:TetrisConfig<'a>) (c:TetrisConfig<'a>) =
    let b1 = Seq.zip h.BlockBit h.ScreenBit |> Seq.map (fun (a,b) -> a ||| b) |> Seq.toArray
    let b2 = Seq.zip c.BlockBit c.ScreenBit |> Seq.map (fun (a,b) -> a ||| b) |> Seq.toArray
    let ch = Seq.zip b1 b2 |> Seq.map (fun (a,b) -> a ^^^ b) |> Seq.toArray
    let f (a,b) = Some ((a,b),(a>>>1,b>>>1))
    let f2 (a,b) = a &&& 1, b &&& 1
    let f3 a = if a = 1 then "■" else "□"
    let fvw a = (c.Width * 2) - (a*2+2)

    Seq.zip ch b2
    |> Seq.mapi (fun n a -> n,a)        // 連番,(変更箇所,実際の値)
    |> Seq.filter (snd >> fst >> (<)0)  // 変更履歴が 0 より大きい行のみを選択
    |> Seq.map (fun (nh,b) -> Console.CursorTop <- 2 + nh; b)
    //|> Seq.collect (Seq.unfold f)       // calc row block
    //|> Seq.takeWhile (fst >> (<)0)
    //|> Seq.mapi (fun nw b -> fvw nw,f2 b)
    //|> Seq.filter (snd >> fst >> (=)1)         // Only the points to be changed are targeted.
    //|> Seq.map (fun (nw,(_,b)) -> Console.CursorTop <- nw; f3 b)
    //|> Seq.iter (printf "%s")
    |> Seq.map(fun (a,b) -> Console.CursorLeft <- 0; b)
    |> Seq.iter(fun bit ->
        Convert.ToString(bit, 2).PadLeft(c.Width, '0')
        |> Seq.map(fun x -> if x = '1' then "■" else "□")
        |> Seq.reduce(+)
        |> Console.WriteLine)
    Console.SetCursorPosition(0, c.Height + 2)

[<STAThread>]
[<EntryPoint>]
let main _ =
    Console.CursorVisible <- false
    Console.Title <- "Retris!!"
    Console.WriteLine "Game Tetris !!"
    Console.WriteLine "If you start the game Tetris Please enter any key."
    Console.ReadLine() |> ignore
    Console.Clear()

    let conf = Program.getTetrisConfig()
    let empty = GameTetris.convertConfig conf

    // Output of the Score
    Console.SetWindowSize(empty.Width*2+5,empty.Height+5)
    Console.SetWindowPosition(0,0)

    Console.WriteLine("Score: ")
    Console.WriteLine()

    [ for y = 1 to (empty.Height) do yield 0 ]
    |> Seq.iter(fun bit ->
    System.Convert.ToString(bit, 2).PadLeft(conf.Width, '0')
    |> Seq.map(fun x -> if x = '1' then "■" else "□")
    |> Seq.reduce(+)
    |> Console.WriteLine)

    GameTetris.run conf
        |> Seq.scan (fun (h,c) a -> c,a) (empty,empty)
        |> Seq.skip 1
        |> Seq.iteri (fun n (h,c) ->
            writeScore c.Score
            writeBlock h c)
    0
