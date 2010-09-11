// public domain

module Zip

open System
open System.Collections.Generic
open System.IO
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
    extra_field_length:uint16
    _fname:byte[]
    _attrs:uint32
    _pos:uint32 }

    static member Create path (rel:string) (data:byte[]) pos =
        let dt, crc, size =
            if data = null then
                Directory.GetLastWriteTime path, 0u, 0u
            else
                File.GetLastWriteTime path, crc32 data, uint32 data.Length
        let fn = Encoding.Default.GetBytes rel
        { version            = 10us
          flags              = 0us
          compression        = 0us
          dos_time           = getDosTime(dt)
          dos_date           = getDosDate(dt)
          crc32              = crc
          compressed_size    = size
          uncompressed_size  = size
          filename_length    = uint16 fn.Length
          extra_field_length = 0us
          _fname             = fn
          _attrs             = uint32(File.GetAttributes path)
          _pos               = pos }
    
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

let mkrel reldir (name:string) =
    if reldir = "" then name else reldir + "/" + name

let writeFile (list:List<ZipHeader>) (bw:BinaryWriter) path rel =
    let data = File.ReadAllBytes path
    let ziph = ZipHeader.Create path rel data (uint32 bw.BaseStream.Position)
    bw.Write [| byte 'P'; byte 'K'; 3uy; 4uy |]
    ziph.Write bw
    bw.Write ziph._fname
    bw.Write data
    list.Add(ziph)

let rec writeDir (list:List<ZipHeader>) (bw:BinaryWriter) path rel =
    let ziph = ZipHeader.Create path (rel + "/") null (uint32 bw.BaseStream.Position)
    bw.Write [| byte 'P'; byte 'K'; 3uy; 4uy |]
    ziph.Write bw
    bw.Write ziph._fname
    list.Add(ziph)
    
    let di = new DirectoryInfo(path)
    for fi in di.GetFiles() do
        writeFile list bw fi.FullName (mkrel rel fi.Name)
    for di2 in di.GetDirectories() do
        writeDir list bw di2.FullName (mkrel rel di2.Name)

let write (list:List<ZipHeader>) (bw:BinaryWriter) path rel =
    if File.Exists path then writeFile list bw path rel
                        else writeDir  list bw path rel

let writeZip (bw:BinaryWriter) (files:string[]) =
    let list = new List<ZipHeader>()
    
    for f in files do write list bw f (Path.GetFileName f)

    let dir_start = bw.BaseStream.Position
    for ziph in list do
        bw.Write [| byte 'P'; byte 'K'; 1uy; 2uy |]
        bw.Write 10us
        ziph.Write bw
        bw.Write 0s
        bw.Write 0s
        bw.Write 0s
        bw.Write ziph._attrs
        bw.Write ziph._pos
        bw.Write ziph._fname
    let dir_len = bw.BaseStream.Position - dir_start
    
    bw.Write [| byte 'P'; byte 'K'; 5uy; 6uy |]
    bw.Write 0us
    bw.Write 0us
    bw.Write (uint16 list.Count)
    bw.Write (uint16 list.Count)
    bw.Write (uint32 dir_len)
    bw.Write (uint32 dir_start)
    bw.Write 0us

let createZip (files:string[]) =
    let dir = Path.GetDirectoryName files.[0]
    let fn = Path.GetFileNameWithoutExtension(files.[0]) + ".zip"
    use fs1 = new FileStream(Path.Combine(dir, fn), FileMode.Create)
    use bw = new BinaryWriter(fs1)
    writeZip bw files
