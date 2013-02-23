namespace FsTetris

module TetrisBlock =
    /// Fall Blocks
    let private FallBlocks = [
        // ■■ 
        [ [0b11;] ]                 // 回転できないブロック！！！
        // 　■
        // ■■ 
        [ [0b01;0b11;] ]        // 回転できないブロック！！！
        // ■■
        // ■　
        [ [0b11;0b01;] ]        // 回転できないブロック！！！
        // ■■
        // ■■
        [ [ 0b11; 0b11; ] ]
        // ■■■■
        [ [ 0b1111; ]; [ 0b0100; 0b0100; 0b0100; 0b0100; ] ]
        // 　■■
        // ■■　
        [ [ 0b011; 0b110; ]; [ 0b10; 0b11; 0b01; ] ]
        // ■■
        // 　■■　
        [ [ 0b110; 0b011; ]; [ 0b01; 0b11; 0b10; ] ]
        // ■       100
        // ■■■ 111
        [ [ 0b100; 0b111; ]
          [ 0b11; 0b10; 0b10; ]
          [ 0b111; 0b001; ]
          [ 0b01; 0b01; 0b11; ] ]
        // 　　■
        // ■■■
        [ [ 0b001; 0b111; ]
          [ 0b10; 0b10; 0b11; ]
          [ 0b111; 0b100; ]
          [ 0b11; 0b01; 0b01; ] ]
        // 　■
        // ■■■
        [ [ 0b010; 0b111; ]
          [ 0b10; 0b11; 0b10; ]
          [ 0b111; 0b010; ]
          [ 0b01; 0b11; 0b01; ] ]
        // ■　■
        // ■■■
        [ [ 0b101; 0b111; ]
          [ 0b11; 0b10; 0b11; ]
          [ 0b111; 0b101; ]
          [ 0b11; 0b01; 0b11; ] ]
    ]

(*
[1] let f x = x
[2] let f x = x ^^^ 1
[4] let f x op = ((op x 1)%4) &&& 0b11
	           op = (+)or(-)
*)
    /// ブロックのリスト内要素数が1または2または4の場合は以下の計算式に基づき回転後のブロックのインデックスを計算する。
    /// If the number of elements in the list of blocks of 1 or 2 or 4 to calculate the index of the block after the rotation based on the following equation.
    /// 1: Blocks have the same sharpe even if the rotation
    /// 2: The shape of the block after the roation because only two, that it can be represented as the exclusive
    /// 4: 
    ///     There are four types of shape of the rotated block.
    ///     Because the clockwise order of the blocks that are defined in FallBlocks must be negative if the current index counterclockwise.
    ///     Alose, if you want to do the calculation right around I must plus the current index.
    ///     So, if 0 or 3 The problem is a boundary value, the number of the current index.
    ///     For the case of 3 and right around the remainder of the four may be obtained simply.
    ///     In the case of the left-handed and is 0 if it is -1, as a way to obtain a numerical value of 3 is you do not get a numeric value of 3 if not 3 Anded.
    let calcTurnFuncs =
        [
            fun x _ -> x
            fun x _ -> x
            fun x _ -> x ^^^ 1
            fun x _ -> x
            fun x op -> ((op x 1)%4) &&& 0b11
        ]

    let getFallBlock (conf : TetrisConfig<'a>) =
        let rnd = new System.Random()
        let bgi = rnd.Next(0, FallBlocks.Length)        // BlockGroup Index
        let b1 = FallBlocks.[bgi]
        let bi = rnd.Next(0, b1.Length)                     // Block Index
        let b2 = b1.[bi]
        let bw = b2 |> Seq.map TetrisCommon.bitcount |> Seq.max
        let b = b2 |> List.map (fun n -> n <<< ((conf.Width - bw) / 2))
        let blankint = 4 - b.Length
        let block =
            List.append
                <| [ for y = 1 to blankint do yield 0 ]@b
                <| [ for y = 0 to conf.Height do yield 0]
        { conf with
            BlockConfig =
                {
                    BlockIndex = bi
                    BlockGroup = b1
                    TopPos = block |> Seq.takeWhile ((=)0) |> Seq.length
                    RightPos =
                        if block |> Seq.forall ((=)0) then 0
                        else
                            block
                            |> Seq.filter ((<)0)
                            |> Seq.map TetrisCommon.getRightZeroCount
                            |> Seq.min
                }
            BlockBit = block }


