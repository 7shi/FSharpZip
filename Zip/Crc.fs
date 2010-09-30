// public domain

module Crc

open System
open System.IO

let crc32_table =
    [| for i in 0..255 ->
        let mutable crc = uint32 i
        for j = 0 to 7 do
            let crc' = crc >>> 1
            crc <- if crc &&& 1u = 0u
                   then crc' else crc' ^^^ 0xedb88320u
        crc |]

let crc32 (buf:byte[]) =
    let mutable crc = ~~~0u
    for b in buf do
        let b' = int(crc ^^^ (uint32 b)) &&& 0xff
        crc <- (crc >>> 8) ^^^ crc32_table.[b']
    ~~~crc

let copyStream (src:Stream) (dst:Stream) =
    let buf = Array.zeroCreate<byte>(16 * 1024)
    let mutable crc = ~~~0u
    let mutable f = true
    while f do
        let len = src.Read(buf, 0, buf.Length)
        if len = 0 then f <- false else
            dst.Write(buf, 0, len)
            for i = 0 to len - 1 do
                let b = int(crc ^^^ (uint32 buf.[i])) &&& 0xff
                crc <- (crc >>> 8) ^^^ crc32_table.[b]
    ~~~crc
