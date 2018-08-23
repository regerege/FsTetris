[<System.STAThread>]
[<EntryPoint>]
let main _ =
    System.Console.CursorVisible <- false
    System.Console.Title <- "Retris!!"
    System.Console.WriteLine "Game Tetris !!"
    System.Console.WriteLine "If you start the game Tetris Please enter any key."

    let conf = FsTetris.Program.getTetrisConfig()
    FsTetris.GameTetris.run conf
        |> Seq.iter (fun c ->
            let bits = Seq.zip c.BlockBit c.ScreenBit |> Seq.map (fun (a,b) -> a ||| b) |> Seq.toArray
            System.Console.Clear()

            // Output of the Score
            System.Console.WriteLine(System.String.Format("Score: {0:#,##0}", c.Score))
            System.Console.WriteLine()

            // Output of the Block
            bits |> Seq.iter(fun bit ->
                System.Convert.ToString(bit, 2).PadLeft(conf.Width, '0')
                |> Seq.map(fun x -> if x = '1' then "■" else "□")
                |> Seq.reduce(+)
                |> System.Console.WriteLine))
    0
