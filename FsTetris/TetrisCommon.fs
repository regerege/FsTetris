namespace FsTetris

module TetrisCommon =
    /// bit count
    let bitcount (bit:int) =
        let a = (bit &&& 0x55555555) + (bit >>> 1 &&& 0x55555555) in
        let b = (a &&& 0x33333333) + (a >>> 2 &&& 0x33333333) in
        let c = (b &&& 0x0F0F0F0F) + (b >>> 4 &&& 0x0F0F0F0F) in
        let d = (c &&& 0x00FF00FF) + (c >>> 8 &&& 0x00FF00FF) in
        (d &&& 0x0000FFFF) + (d >>> 16 &&& 0x0000FFFF)

