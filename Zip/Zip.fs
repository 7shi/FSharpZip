// public domain

module Zip

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text

let crc32_table =
    [| for i in 0..255 ->
        let mutable reg = uint32 i
        for j = 0 to 7 do
            let reg' = reg >>> 1
            reg <- if reg &&& 1u = 0u
                   then reg' else reg' ^^^ 0xedb88320u
        reg |]

let crc32 (buf:byte[]) =
    let mutable reg = ~~~0u
    for b in buf do
        reg <- (reg >>> 8) ^^^
               crc32_table.[int(reg ^^^ (uint32 b)) &&& 0xff]
    ~~~reg

let getDosDate (dt:DateTime) =
    uint16(((dt.Year - 1980) <<< 9) ||| (dt.Month <<< 5) ||| dt.Day)

let getDosTime (dt:DateTime) =
    uint16((dt.Hour <<< 11) ||| (dt.Minute <<< 5) ||| (dt.Second >>> 1))

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

    static member Create path (relb:byte[]) (data:byte[]) pos =
        let dt, crc, size =
            if data = null then
                Directory.GetLastWriteTime path, 0u, 0u
            else
                File.GetLastWriteTime path, crc32 data, uint32 data.Length
        { version            = 10us
          flags              = 0us
          compression        = 0us
          dos_time           = getDosTime(dt)
          dos_date           = getDosDate(dt)
          crc32              = crc
          compressed_size    = size
          uncompressed_size  = size
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

type ZipDirHeader =
    { version:uint16
      header:ZipHeader
      _1:uint16 // file comment length
      _2:uint16 // disk number start
      _3:uint16 // internal file attributes
      attrs:uint32
      pos:uint32
      fname:byte[] }

    static member Create path (rel:string) (data:byte[]) pos =
        let relb = Encoding.Default.GetBytes rel
        { version = 10us
          header  = ZipHeader.Create path relb data pos
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
    let data = File.ReadAllBytes path
    let ziph = ZipDirHeader.Create path rel data (uint32 bw.BaseStream.Position)
    bw.Write [| byte 'P'; byte 'K'; 3uy; 4uy |]
    ziph.header.Write bw
    bw.Write ziph.fname
    bw.Write data
    list.Add(ziph)

let rec writeDir (list:List<ZipDirHeader>) (bw:BinaryWriter) path rel =
    let ziph = ZipDirHeader.Create path (rel + "/") null (uint32 bw.BaseStream.Position)
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

let createZip (files:string[]) =
    let dir = Path.GetDirectoryName files.[0]
    let fn = Path.GetFileNameWithoutExtension(files.[0]) + ".zip"
    use fs1 = new FileStream(Path.Combine(dir, fn), FileMode.Create)
    use bw = new BinaryWriter(fs1)
    writeZip bw files

let extractZip (zip:string) =
    let dir = Path.ChangeExtension(zip, "")
    if not(Directory.Exists dir) then
        ignore <| Directory.CreateDirectory(dir)
    
    use fs = new FileStream(zip, FileMode.Open)
    if fs.Length < 22L then
        failwith "ファイルが小さ過ぎます。"
    
    fs.Position <- fs.Length - 22L
    use br = new BinaryReader(fs)
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
        let path = Path.Combine(dir, Encoding.Default.GetString zipdh.fname)
        if zipdh.attrs &&& (uint32 FileAttributes.Directory) = 0u then
            let pos = fs.Position
            fs.Position <- int64 zipdh.pos
            if br.ReadInt32() <> 0x04034b50 then
                failwith "ファイルが壊れています。"
            let ziph = ZipHeader.Read br
            fs.Position <- fs.Position +
                           int64(ziph.filename_length + ziph.extra_field_length)
            let data = br.ReadBytes(int zipdh.header.compressed_size)
            let data = match ziph.compression with
                       | 0us -> data // 無圧縮
                       | 8us -> let buf = Array.zeroCreate<byte> 4096
                                use mso = new MemoryStream()
                                use msi = new MemoryStream(data)
                                use dfs = new DeflateStream(msi, CompressionMode.Decompress)
                                let rec decomp() =
                                    let num = dfs.Read(buf, 0, buf.Length)
                                    if num > 0 then
                                        mso.Write(buf, 0, num)
                                        decomp()
                                decomp()
                                mso.ToArray()
                       | _   -> failwith "サポートされていない圧縮形式です。"
            use fso = new FileStream(path, FileMode.Create)
            fso.Write(data, 0, data.Length)
            fs.Position <- pos
        File.SetAttributes(path, enum<FileAttributes>(int32 zipdh.attrs))
