namespace FsTetris

module TetrisCommon =
    /// bit count
    let bitcount (bit:int) =
        let a = (bit &&& 0x55555555) + (bit >>> 1 &&& 0x55555555) in
        let b = (a &&& 0x33333333) + (a >>> 2 &&& 0x33333333) in
        let c = (b &&& 0x0F0F0F0F) + (b >>> 4 &&& 0x0F0F0F0F) in
        let d = (c &&& 0x00FF00FF) + (c >>> 8 &&& 0x00FF00FF) in
        (d &&& 0x0000FFFF) + (d >>> 16 &&& 0x0000FFFF)

    /// Count the number of Zeros from the end.
    let getRightZeroCount (bit:int) =
        if bit = 0 then 0
        else
            let rec find n c =
                if n &&& 1 = 1 then c
                else find (n >>> 1) (c+1)
            find bit 0
