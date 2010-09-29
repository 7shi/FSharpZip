// public domain

module Deflate

open System
open System.Collections.Generic
open System.IO
open System.Linq

let bufmax = 32768

let getBit (b:byte) (bit:int) =
    if b &&& (1uy <<< bit) = 0uy then 0 else 1

type BitReader(sin:Stream) =
    let mutable bit = 8
    let mutable cur = 0uy
    
    member x.Skip() =
        bit <- 8
    
    member x.ReadBit() =
        if bit = 8 then
            bit <- 0
            let b = sin.ReadByte()
            if b = -1 then
                failwith "バッファを超過しました"
            cur <- byte b
        let ret = if cur &&& (1uy <<< bit) = 0uy then 0 else 1
        bit <- bit + 1
        ret
    
    member x.ReadLE n =
        let mutable ret = 0
        for i = 0 to n - 1 do
            if x.ReadBit() = 1 then ret <- ret ||| (1 <<< i)
        ret
    
    member x.ReadBE n =
        let mutable ret = 0
        for i = 0 to n - 1 do
            ret <- (ret <<< 1) ||| x.ReadBit()
        ret
    
    member x.ReadBytes len =
        if bit <> 8 then bit <- 8
        let buf = Array.zeroCreate<byte> len
        ignore <| sin.Read(buf, 0, len)
        buf

type WriteBuffer(sout:Stream) =
    let mutable prev:byte[] = null
    let mutable buf = Array.zeroCreate<byte> bufmax
    let mutable p = 0
    
    let next newbuf =
        prev <- buf
        buf <- if newbuf then Array.zeroCreate<byte> bufmax else null
        p <- 0
    
    member x.Close() =
        next false
        next false
    
    interface IDisposable with
        member x.Dispose() = x.Close()
    
    member x.WriteByte (b:byte) =
        buf.[p] <- b
        sout.WriteByte b
        p <- p + 1
        if p = bufmax then next true

    member x.Write (src:byte[]) start len =
        let maxlen = bufmax - p
        if len <= maxlen then
            Array.Copy(src, start, buf, p, len)
            sout.Write(src, start, len)
            p <- p + len
            if p = bufmax then next true
        else
            x.Write src start maxlen
            x.Write src (start + maxlen) (len - maxlen)
    
    member x.Copy len dist =
        if dist < 1 then
            failwith <| sprintf "dist too small: %d < 1" dist
        elif dist > bufmax then
            failwith <| sprintf "dist too big: %d > %d" dist bufmax
        let pp = p - dist
        if pp < 0 then
            if prev = null then
                failwith <| sprintf "dist too big: %d > %d" dist p
            let pp = pp + bufmax
            let maxlen = bufmax - pp
            if len <= maxlen then
                x.Write prev pp len
            else
                x.Write prev pp maxlen
                x.Copy (len - maxlen) dist
        else
            let maxlen = p - pp
            if len <= maxlen then
                x.Write buf pp len
            else
                if dist = 1 then
                    let b = buf.[pp]
                    for i = 1 to len do
                        x.WriteByte b
                else
                    let buf' = buf
                    let mutable len' = len
                    while len' > 0 do
                        let len'' = Math.Min(len', maxlen)
                        x.Write buf' pp len''
                        len' <- len' - len''

type Huffman(lens:int[]) =
    let vals = Array.zeroCreate<int> lens.Length
    let min = lens.Where(fun x -> x > 0).Min()
    let max = lens.Max()
    let counts = Array.zeroCreate<int>  (max + 1)
    let firsts = Array.zeroCreate<int>  (max + 1)
    let nexts  = Array.zeroCreate<int>  (max + 1)
    let tables = Array.zeroCreate<int[]>(max + 1)
    
    do
        for len in lens do
            if len > 0 then counts.[len] <- counts.[len] + 1
        for i = 1 to max do
            firsts.[i] <- (firsts.[i - 1] + counts.[i - 1]) <<< 1
        Array.Copy(firsts, 0, nexts, 0, max + 1)
        for i = 0 to vals.Length - 1 do
            let len = lens.[i]
            if len > 0 then
                vals.[i] <- nexts.[len]
                nexts.[len] <- nexts.[len] + 1
        
        for i = 0 to vals.Length - 1 do
            let len = lens.[i]
            if len > 0 then
                let start = firsts.[len]
                if tables.[len] = null then
                    let count = nexts.[len] - start
                    tables.[len] <- Array.zeroCreate<int> count
                tables.[len].[vals.[i] - start] <- i
    
    member x.GetValue h =
        let rec getv i =
            if i > max then -1 else
                if h < nexts.[i] then
                    tables.[i].[h - firsts.[i]]
                else
                    getv (i + 1)
        getv min
    
    member x.Read(br:BitReader) =
        let rec read h i =
            if h < nexts.[i] then
                tables.[i].[h - firsts.[i]]
            else
                read ((h <<< 1) ||| br.ReadBit()) (i + 1)
        read (br.ReadBE min) min

type [<AbstractClass>] HuffmanDecoder() =
    abstract GetValue: unit->int
    abstract GetDistance: unit->int

type FixedHuffman(br:BitReader) =
    inherit HuffmanDecoder()
    
    override x.GetValue() =
        let v = br.ReadBE 7
        if v < 24 then v + 256 else
            let v = (v <<< 1) ||| br.ReadBit()
            if v < 192 then v - 48
            elif v < 200 then v + 88
            else ((v <<< 1) ||| br.ReadBit()) - 256
    
    override x.GetDistance() = br.ReadBE 5

type DynamicHuffman(br:BitReader) =
    inherit HuffmanDecoder()
    
    let lit, dist =
        let hlit =
            let hlit = (br.ReadLE 5) + 257
            if hlit > 286 then failwith <| sprintf "hlit: %d > 286" hlit
            hlit
        
        let hdist =
            let hdist = (br.ReadLE 5) + 1
            if hdist > 32 then failwith <| sprintf "hdist: %d > 32" hdist
            hdist
        
        let hclen =
            let hclen = (br.ReadLE 4) + 4
            if hclen > 19 then failwith <| sprintf "hclen: %d > 19" hclen
            hclen
        
        let clen =
            let hclens = Array.zeroCreate<int> 19
            let order = [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5;
                           11; 4; 12; 3; 13; 2; 14; 1; 15 |]
            for i = 0 to hclen - 1 do
                hclens.[order.[i]] <- br.ReadLE 3
            new Huffman(hclens)
        
        let ld = Array.zeroCreate<int>(hlit + hdist)
        let mutable i = 0
        while i < ld.Length do
            let v = clen.Read(br)
            if v < 16 then
                ld.[i] <- v
                i <- i + 1
            else
                let r, v =
                    match v with
                    | 16 -> (br.ReadLE 2) + 3, ld.[i - 1]
                    | 17 -> (br.ReadLE 3) + 3, 0
                    | 18 -> (br.ReadLE 7) + 11, 0
                    | _  -> failwith "不正な値です。"
                for j = 0 to r - 1 do
                    ld.[i + j] <- v
                i <- i + r
        
        new Huffman(ld.[0 .. hlit - 1]),
        new Huffman(ld.[hlit .. hlit + hdist - 1])
    
    override x.GetValue() = lit.Read br
    override x.GetDistance() = dist.Read br

let getLitExLen v = if v < 265 || v = 285 then 0 else (v - 261) >>> 2
let getDistExLen d = if d < 4 then 0 else (d - 2) >>> 1

let litlens =
    let litlens = Array.zeroCreate<int> 286
    let mutable v = 3
    for i = 257 to 284 do
        litlens.[i] <- v
        v <- v + (1 <<< (getLitExLen i))
    litlens.[285] <- 258
    litlens.[257..285]

let distlens =
    let distlens = Array.zeroCreate<int> 30
    let mutable v = 1
    for i = 0 to 29 do
        distlens.[i] <- v
        v <- v + (1 <<< (getDistExLen i))
    distlens

type Reader(sin:Stream) =
    inherit Stream()
    
    let br = new BitReader(sin)
    let fh = new FixedHuffman(br)
    
    let sout = new MemoryStream()
    let dbuf = new WriteBuffer(sout)
    
    let mutable cache:byte[] = null
    let mutable canRead = true

    let rec read (h:HuffmanDecoder) =
        let v = h.GetValue()
        if v > 285 then failwith <| sprintf "不正な値: %d" v
        if v < 256 then
            dbuf.WriteByte(byte v)
        elif v > 256 then
            let len =
                if v < 265 then v - 254 else
                    litlens.[v - 257] + (br.ReadLE (getLitExLen v))
            let dist =
                let d = h.GetDistance()
                if d > 29 then failwith <| sprintf "不正な距離: %d" d
                if d < 4 then d + 1 else
                    distlens.[d] + (br.ReadLE (getDistExLen d))
            dbuf.Copy len dist
        if v <> 256 then read h
    
    override x.CanRead  = canRead
    override x.CanWrite = false
    override x.CanSeek  = false
    override x.Flush()  = ()
    
    override x.Close() =
        dbuf.Close()
        canRead <- false
    
    override x.Read(buffer, offset, count) =
        let offset =
            if cache = null then 0 else
                let clen = cache.Length
                let len = Math.Min(clen, count)
                Array.Copy(cache, 0, buffer, offset, len)
                cache <- if len = clen then null
                         else cache.[len .. clen - 1]
                len
        let req = int64 <| count - offset
        while canRead && sout.Length < req do
            x.readBlock()
        let len =
            if sout.Length = 0L then 0 else
                let data = sout.ToArray()
                sout.SetLength(0L)
                let dlen = data.Length
                let len = Math.Min(int req, dlen)
                Array.Copy(data, 0, buffer, offset, len)
                if dlen > len then
                    cache <- data.[len..]
                len
        offset + len
    
    override x.Position
        with get() = raise <| new NotImplementedException()
        and set(v) = raise <| new NotImplementedException()
    
    override x.Length         = raise <| new NotImplementedException()
    override x.Seek(_, _)     = raise <| new NotImplementedException()
    override x.Write(_, _, _) = raise <| new NotImplementedException()
    override x.SetLength(_)   = raise <| new NotImplementedException()
    
    member private x.readBlock() =
        let bfinal = br.ReadBit()
        match br.ReadLE 2 with
        | 0 -> br.Skip()
               let len = br.ReadLE 16
               let nlen = br.ReadLE 16
               if len + nlen <> 0x10000 then
                   failwith "不正な非圧縮長"
               dbuf.Write (br.ReadBytes len) 0 len
        | 1 -> read fh
        | 2 -> read (new DynamicHuffman(br))
        | _ -> failwith "不正なブロックタイプ"
        if bfinal = 1 then
            canRead <- false
            x.Close()

type BitWriter(sout:Stream) =
    let mutable bit = 0
    let mutable cur = 0uy
    
    member x.Skip() =
        if bit > 0 then
            sout.WriteByte(cur)
            bit <- 0
            cur <- 0uy
    
    interface IDisposable with
        member x.Dispose() =
            x.Skip()
            sout.Flush()
    
    member x.WriteBit(b:int) =
        cur <- cur ||| ((byte b) <<< bit)
        bit <- bit + 1
        if bit = 8 then
            sout.WriteByte(cur)
            bit <- 0
            cur <- 0uy
    
    member x.WriteLE (len:int) (b:int) =
        for i = 0 to len - 1 do
            x.WriteBit <| if (b &&& (1 <<< i)) = 0 then 0 else 1
    
    member x.WriteBE (len:int) (b:int) =
        for i = len - 1 downto 0 do
            x.WriteBit <| if (b &&& (1 <<< i)) = 0 then 0 else 1
    
    member x.WriteBytes(data:byte[]) =
        x.Skip()
        sout.Write(data, 0, data.Length)

type FixedHuffmanWriter(bw:BitWriter) =
    member x.Write (b:int) =
        if b < 144 then
            bw.WriteBE 8 (b + 0b110000)
        elif b < 256 then
            bw.WriteBE 9 (b - 144 + 0b110010000)
        elif b < 280 then
            bw.WriteBE 7 (b - 256)
        elif b < 288 then
            bw.WriteBE 8 (b - 280 + 0b11000000)
    
    member x.WriteLen (len:int) =
        if len < 3 || len > 258 then
            failwith <| sprintf "不正な長さ: %d" len
        let mutable ll = 285
        while len < litlens.[ll - 257] do
            ll <- ll - 1
        x.Write ll
        bw.WriteLE (getLitExLen ll) (len - litlens.[ll - 257])
    
    member x.WriteDist (d:int) =
        if d < 1 || d > bufmax then
            failwith <| sprintf "不正な距離: %d" d
        let mutable dl = 29
        while d < distlens.[dl] do
            dl <- dl - 1
        bw.WriteBE 5 dl
        bw.WriteLE (getDistExLen dl) (d - distlens.[dl])

let search (data:byte[]) (pos:int) =
    let mutable p = pos - 1
    let mutable maxp = -1
    let mutable maxl = 0
    let mlen = Math.Min(258, data.Length - pos)
    let last = Math.Max(0, pos - bufmax)
    while p >= last do
        let mutable len = 0
        while len < mlen && data.[p + len] = data.[pos + len] do
            len <- len + 1
        if len > maxl then
            maxp <- p
            maxl <- len
        p <- p - 1
    maxp, maxl

let Compress (sout:Stream) (data:byte[]) =
    use bw = new BitWriter(sout)
    bw.WriteBit 1
    bw.WriteLE 2 1
    let hw = new FixedHuffmanWriter(bw)
    let len = data.Length
    let mutable p = 0
    while p < len do
        let maxp, maxl = search data p
        if maxl < 3 then
            hw.Write(int data.[p])
            p <- p + 1
        else
            hw.WriteLen maxl
            hw.WriteDist (p - maxp)
            p <- p + maxl
    hw.Write 256

let GetCompressBytes (data:byte[]) =
    let ms = new MemoryStream()
    Compress ms data
    ms.ToArray()
