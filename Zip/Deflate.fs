module Deflate

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text

let getBit (b:byte) (bit:int) =
    if b &&& (1uy <<< bit) = 0uy then 0 else 1

let getBitChar (b:byte) (bit:int) =
    if b &&& (1uy <<< bit) = 0uy then '0' else '1'

type BitReader(buf:byte[]) =
    let mutable bytepos = 0
    let mutable bitpos = 0
    
    member x.Skip() =
        bitpos  <- 0
        bytepos <- bytepos + 1
    
    member x.ReadBit() =
        if bytepos >= buf.Length then
            failwith "バッファを超過しました"
        else
            let ret = getBit buf.[bytepos] bitpos
            bitpos <- bitpos + 1
            if bitpos = 8 then x.Skip()
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

type DeflateBuffer() =
    let max = 32768
    let mutable last:byte[] = null
    let mutable buf = Array.zeroCreate<byte> max
    let mutable p = 0
    let list = new List<byte[]>()
    
    let newbuf() =
        list.Add buf
        last <- buf
        buf <- Array.zeroCreate<byte> max
        p <- 0
    
    member x.WriteByte (b:byte) =
        buf.[p] <- b
        p <- p + 1
        if p = max then newbuf()

    member x.Write (src:byte[]) start len =
        let maxlen = max - p
        if len <= maxlen then
            Array.Copy(src, start, buf, p, len)
            p <- p + len
            if p = max then newbuf()
        else
            x.Write src start maxlen
            x.Write src (start + maxlen) (len - maxlen)
    
    member x.Copy len dist =
        if dist < 1 then
            failwith <| sprintf "dist too small: %d < 1" dist
        elif dist > max then
            failwith <| sprintf "dist too big: %d > %d" dist max
        let pp = p - dist
        if pp < 0 then
            if last = null then
                failwith "dist too big: %d > %d" dist p
            let pp = pp + max
            let maxlen = max - pp
            if len <= maxlen then
                x.Write last pp len
            else
                x.Write last pp maxlen
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
    
    member x.WriteTo(s:Stream) =
        for b in list do
            s.Write(b, 0, b.Length)
        s.Write(buf, 0, p)
    
    member x.ToArray() =
        let ms = new MemoryStream()
        x.WriteTo ms
        ms.ToArray()

let rbin (b:byte) =
    let sb = new StringBuilder()
    for i = 0 to 7 do
        ignore <| sb.Append(getBitChar b i)
    sb.ToString()

type [<AbstractClass>] Huffman(br:BitReader) =
    abstract GetValue: unit->int
    abstract GetLength: unit->int

type FixedHuffman(br) =
    inherit Huffman(br)
    
    override x.GetValue() =
        let v = br.ReadBE 7
        if v < 24 then v + 256 else
            let v = (v <<< 1) ||| br.ReadBit()
            if v < 192 then v - 48
            elif v < 200 then v + 88
            else ((v <<< 1) ||| br.ReadBit()) - 256
    
    override x.GetLength() =
        let d = br.ReadBE 5
        if d > 29 then failwith <| sprintf "不正な距離: %d" d
        if d < 4 then d + 1 else
            let exlen = (d - 2) >>> 1
            ((d + 1) <<< exlen) ||| (br.ReadBE exlen)

let rec deflateHuffman (br:BitReader) (dbuf:DeflateBuffer) (h:Huffman) =
    let v = h.GetValue()
    if v > 285 then failwith <| sprintf "不正な値: %d" v
    if v < 256 then
        dbuf.WriteByte(byte v)
    elif v > 256 then
        let len =
            if v < 265 then v - 254 else
                let exlen = (v - 261) >>> 2
                ((v - 254) <<< exlen) ||| (br.ReadBE exlen)
        let dist = h.GetLength()
        Debug.WriteLine <| sprintf "len: %d, dist: %d" len dist
        dbuf.Copy len dist
    if v <> 256 then
        deflateHuffman br dbuf h

let Check (path:string) =
    let buf = File.ReadAllBytes(path)
    let br = new BitReader(buf)
    let dbuf = new DeflateBuffer()
    let mutable bfinal = 0
    while bfinal = 0 do
        bfinal <- br.ReadBit()
        match br.ReadLE 2 with
        | 0 -> failwith "非圧縮"
        | 1 -> deflateHuffman br dbuf (new FixedHuffman(br))
        | 2 -> failwith "動的ハフマン符号"
        | _ -> failwith "不正な値"
    Debug.Write(Encoding.Default.GetString(dbuf.ToArray()))
    Debug.WriteLine("")
