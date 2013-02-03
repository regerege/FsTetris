namespace FsTetris
open System
open System.Diagnostics

module Program =
    let config = new TetrisConfig(null, 60L)
    let game = new Tetris(config)

    [<STAThread>]
    [<EntryPoint>]
    let Main (_) =
        Console.CursorVisible <- false
        Console.Title <- "テトリス？"
        game.ScreenUpdate.Add(fun bits ->
            Console.Clear()
            bits |> Seq.iter(fun bit ->
                Convert.ToString(bit, 2).PadLeft(20, '0')
                |> Seq.map(fun x -> if x = '1' then "■" else "□")
                |> Seq.reduce(+)
                |> Console.WriteLine))
        game.Run()
        0
