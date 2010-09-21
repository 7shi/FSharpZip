// public domain

module Zip

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text

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

let getDosDate (dt:DateTime) =
    uint16(((dt.Year - 1980) <<< 9) ||| (dt.Month <<< 5) ||| dt.Day)

let getDosTime (dt:DateTime) =
    uint16((dt.Hour <<< 11) ||| (dt.Minute <<< 5) ||| (dt.Second >>> 1))

let getDateTime (dd:uint16) (dt:uint16) =
    new DateTime(int(dd >>> 9) + 1980,
                 Math.Max(1, Math.Min(12, int(dd >>> 5) &&& 15)),
                 Math.Max(1, int(dd) &&& 31),
                 Math.Min(23, int(dt >>> 11)),
                 Math.Min(59, int(dt >>> 5) &&& 63),
                 Math.Min(59, (int(dt) &&& 31) * 2))

type ZipHeader =
    { version:uint16
      flags:uint16
      compression:uint16
      dos_time:uint16
      dos_date:uint16
      crc32:uint32
      compressed_size:uint32
      uncompressed_size:uint32
      filename_length:uint16
      extra_field_length:uint16 }

    static member Create path (relb:byte[]) size1 crc (data:byte[]) pos =
        let dt, size2 =
            if data = null then
                Directory.GetLastWriteTime path, 0u
            else
                File.GetLastWriteTime path, uint32 data.Length
        { version            = 20us
          flags              = 0us
          compression        = 8us
          dos_time           = getDosTime(dt)
          dos_date           = getDosDate(dt)
          crc32              = crc
          compressed_size    = size2
          uncompressed_size  = size1
          filename_length    = uint16 relb.Length
          extra_field_length = 0us }
    
    static member Read (br:BinaryReader) =
        let pos = uint32 br.BaseStream.Position
        { version            = br.ReadUInt16()
          flags              = br.ReadUInt16()
          compression        = br.ReadUInt16()
          dos_time           = br.ReadUInt16()
          dos_date           = br.ReadUInt16()
          crc32              = br.ReadUInt32()
          compressed_size    = br.ReadUInt32()
          uncompressed_size  = br.ReadUInt32()
          filename_length    = br.ReadUInt16()
          extra_field_length = br.ReadUInt16() }
    
    member x.Write (bw:BinaryWriter) =
        bw.Write x.version
        bw.Write x.flags
        bw.Write x.compression
        bw.Write x.dos_time
        bw.Write x.dos_date
        bw.Write x.crc32
        bw.Write x.compressed_size
        bw.Write x.uncompressed_size
        bw.Write x.filename_length
        bw.Write x.extra_field_length
    
    member x.DateTime = getDateTime x.dos_date x.dos_time

type ZipDirHeader =
    { version:uint16
      header:ZipHeader
      _1:uint16 // file comment length
      _2:uint16 // disk number start
      _3:uint16 // internal file attributes
      attrs:uint32
      pos:uint32
      fname:byte[] }

    static member Create path (rel:string) size1 crc (data:byte[]) pos =
        let relb = Encoding.Default.GetBytes rel
        { version = 20us
          header  = ZipHeader.Create path relb size1 crc data pos
          _1      = 0us
          _2      = 0us
          _3      = 0us
          attrs   = uint32(File.GetAttributes path)
          pos     = pos
          fname   = relb }
    
    static member Read (br:BinaryReader) =
        let v = br.ReadUInt16()
        let h = ZipHeader.Read br
        let r = { version = v
                  header  = h
                  _1      = br.ReadUInt16()
                  _2      = br.ReadUInt16()
                  _3      = br.ReadUInt16()
                  attrs   = br.ReadUInt32()
                  pos     = br.ReadUInt32()
                  fname   = br.ReadBytes(int h.filename_length) }
        let exlen = (int64 h.extra_field_length) + (int64 r._1)
        ignore <| br.BaseStream.Seek(exlen, SeekOrigin.Current)
        r
    
    member x.Write (bw:BinaryWriter) =
        bw.Write x.version
        x.header.Write bw
        bw.Write x._1
        bw.Write x._2
        bw.Write x._3
        bw.Write x.attrs
        bw.Write x.pos
        bw.Write x.fname

let mkrel reldir (name:string) =
    if reldir = "" then name else reldir + "/" + name

let writeFile (list:List<ZipDirHeader>) (bw:BinaryWriter) path rel =
    let data1 = File.ReadAllBytes path
    let data2 = Deflate.GetCompressBytes data1
    let p = uint32 bw.BaseStream.Position
    let ziph = ZipDirHeader.Create path rel (uint32 data1.Length) (crc32 data1) data2 p
    bw.Write [| byte 'P'; byte 'K'; 3uy; 4uy |]
    ziph.header.Write bw
    bw.Write ziph.fname
    bw.Write data2
    list.Add(ziph)

let rec writeDir (list:List<ZipDirHeader>) (bw:BinaryWriter) path rel =
    let p = uint32 bw.BaseStream.Position
    let ziph = ZipDirHeader.Create path (rel + "/") 0u 0u null p
    bw.Write [| byte 'P'; byte 'K'; 3uy; 4uy |]
    ziph.header.Write bw
    bw.Write ziph.fname
    list.Add(ziph)
    
    let di = new DirectoryInfo(path)
    for fi in di.GetFiles() do
        writeFile list bw fi.FullName (mkrel rel fi.Name)
    for di2 in di.GetDirectories() do
        writeDir list bw di2.FullName (mkrel rel di2.Name)

let write (list:List<ZipDirHeader>) (bw:BinaryWriter) path rel =
    if File.Exists path then writeFile list bw path rel
                        else writeDir  list bw path rel

let writeZip (bw:BinaryWriter) (files:string[]) =
    let list = new List<ZipDirHeader>()
    
    for f in files do write list bw f (Path.GetFileName f)

    let dir_start = bw.BaseStream.Position
    for ziph in list do
        bw.Write [| byte 'P'; byte 'K'; 1uy; 2uy |]
        ziph.Write bw
    let dir_len = bw.BaseStream.Position - dir_start
    
    bw.Write [| byte 'P'; byte 'K'; 5uy; 6uy |]
    bw.Write 0us // number of this disk
    bw.Write 0us // number of the disk with the start of the central directory
    bw.Write (uint16 list.Count)
    bw.Write (uint16 list.Count)
    bw.Write (uint32 dir_len)
    bw.Write (uint32 dir_start)
    bw.Write 0us // zipfile comment length

let Create (files:string[]) =
    let dir = Path.GetDirectoryName files.[0]
    let fn = if File.Exists(files.[0])
             then Path.GetFileNameWithoutExtension(files.[0]) + ".zip"
             else files.[0] + ".zip"
    use fs1 = new FileStream(Path.Combine(dir, fn), FileMode.Create)
    use bw = new BinaryWriter(fs1)
    writeZip bw files

let mkDir (path:string) =
    if not(Directory.Exists path) then
        ignore <| Directory.CreateDirectory(path)

let isPathSep (ch:char) = ch = '/' || ch = '\\'

let hasDrive (path:string) =
    path.Length >= 3 &&
    Char.IsLetter path.[0] &&
    path.[1] = ':' &&
    isPathSep path.[2]

let isAbsPath (path:string) =
    (path.Length >= 1 && isPathSep path.[0]) || hasDrive path

let mkRelPath (dir:string) (path:string) =
    if isAbsPath path then
        Path.Combine(dir, Path.GetFileName(path))
    else
        let sb = new StringBuilder()
        let mutable ret = dir
        for ch in path do
            if isPathSep ch then
                if sb.Length > 0 then
                    ret <- Path.Combine(ret, sb.ToString())
                    mkDir ret
                    sb.Length <- 0
            else
                ignore <| sb.Append ch
        if sb.Length > 0 then ret <- Path.Combine(ret, sb.ToString())
        ret

type SubStream(s:Stream, length:int64) =
    inherit Stream()
    
    let start = s.Position
    let mutable pos = 0L
    
    override x.Length   = length
    override x.CanRead  = pos < length
    override x.CanWrite = false
    override x.CanSeek  = true
    override x.Flush()  = ()
    
    override x.Position
        with get() = pos
        and set(v) = pos <- v
                     s.Position <- start + pos
    
    override x.Read(buffer, offset, count) =
        if not x.CanRead then 0 else
            let count' = int <| Math.Min(length - pos, int64 count)
            let ret = s.Read(buffer, offset, count')
            pos <- pos + (int64 ret)
            ret
    
    override x.Seek(offset, origin) =
        match origin with
        | SeekOrigin.Begin   -> x.Position <- offset
        | SeekOrigin.Current -> x.Position <- pos + offset
        | SeekOrigin.End     -> x.Position <- length + offset
        | _ -> ()
        pos
    
    override x.Write(_, _, _) = raise <| new NotImplementedException()
    override x.SetLength(_)   = raise <| new NotImplementedException()

let listup (br:BinaryReader) =
    let list = new List<ZipDirHeader>()
    
    let fs = br.BaseStream
    if fs.Length < 22L then
        failwith "ファイルが小さ過ぎます。"
    
    fs.Position <- fs.Length - 22L
    if br.ReadInt32() <> 0x06054b50 then
        failwith "ヘッダが見付かりません。"
    
    fs.Position <- fs.Position + 6L
    let count = int <| br.ReadUInt16()
    let dir_len = br.ReadUInt32()
    let dir_start = br.ReadUInt32()
    
    fs.Position <- int64 dir_start
    for i = 1 to count do
        if br.ReadInt32() <> 0x02014b50 then
            failwith "ファイルが壊れています。"
        let zipdh = ZipDirHeader.Read br
        list.Add zipdh
    
    list

let getTopDir (list:List<ZipDirHeader>) =
    let mutable ret = ""
    let mutable en = list.GetEnumerator()
    let mutable f = true
    while f && en.MoveNext() do
        let zipdh = en.Current
        let fn = Encoding.Default.GetString zipdh.fname
        if not(isAbsPath fn) then
            let p = fn.Replace('\\', '/').IndexOf('/')
            if p > 0 then
                let topdir = fn.Substring(0, p)
                if ret = "" then
                    ret <- topdir
                elif ret <> topdir then
                    ret <- ""
                    f <- false
    ret

let Extract (zip:string) =
    use fs = new FileStream(zip, FileMode.Open)
    use br = new BinaryReader(fs)
    let list = listup br
    
    let topdir = getTopDir list
    let dir = if topdir = ""
              then Path.ChangeExtension(zip, "")
              else Path.GetDirectoryName(zip)
    mkDir dir
    
    for zipdh in list do
        let dt = zipdh.header.DateTime
        let fn = Encoding.Default.GetString zipdh.fname
        let path = mkRelPath dir fn
        let attrs = enum<FileAttributes>(int zipdh.attrs)
        if int(attrs &&& FileAttributes.Directory) <> 0 then
            mkDir path
            File.SetAttributes(path, attrs)
            Directory.SetLastWriteTime(path, dt)
        else
            fs.Position <- int64 zipdh.pos
            if br.ReadInt32() <> 0x04034b50 then
                failwith "ファイルが壊れています。"
            let ziph = ZipHeader.Read br
            fs.Position <- fs.Position +
                           int64(ziph.filename_length + ziph.extra_field_length)
            do
                use ss = new SubStream(fs, int64 zipdh.header.compressed_size)
                use file = new FileStream(path, FileMode.Create)
                let crc =
                    match ziph.compression with
                    | 0us -> copyStream ss file
                    | 8us -> use dfs = new Deflate.Reader(ss)
                             copyStream dfs file
                    | _   -> failwith "サポートされていない圧縮形式です。"
                if crc <> zipdh.header.crc32 then
                    failwith("CRC が一致しません: \n\n" + fn)
            File.SetAttributes(path, attrs)
            File.SetLastWriteTime(path, dt)
