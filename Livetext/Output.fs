
#light
namespace Livetext

open System
open System.Numerics
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Text
open System.Runtime.InteropServices
open System.IO
open Poly2Tri
open Squish
open Mesh
open Lua
open Import

module Output =
    open Poly2Tri.Triangulation.Delaunay

    //[<Flags>]
    type DDSFlags =
    | Caps = 1
    | Height = 2
    | Width = 4
    | Pitch = 8
    | PixelFormat = 4096
    | MipMapCount = 131072
    | LinearSize = 524288
    | DepthTexture = 8388608
    
    let generateMaterial texturePath outputPath = 
      let mtl = 
        F("data",
          P [
            ("params", P [
              ("map_color_alpha", P [
                ("compressionAllowed", B true);
                ("fileName", S (texturePath + ".dds"));
                ("magFilter", S "LINEAR");
                ("minFilter", S "LINEAR_MIPMAP_LINEAR");
                ("mipmapAlphaScale", V 0);
                ("type", S "TWOD");
                ("wrapS", S "CLAMP_TO_EDGE");
                ("wrapT", S "CLAMP_TO_EDGE")
              ]);
              ("polygon_offset", P [
                ("factor", V 0);
                ("units", V 0)
              ]);
              ("two_sided", P [("twoSided", B true)])
            ]);
            ("type", S "TRANSPARENT")
            ]
          )
        |> printLua 0 in

      File.WriteAllText(outputPath + ".mtl", mtl)

    let dds outputPath = function
      | [] -> ()
      | ((mipmap, size) :: downsampes as mipmaps : (byte[] * Size) list) -> 
        using (new BinaryWriter(new FileStream(outputPath + ".dds", FileMode.Create))) (
          fun bw ->
            let flags = DDSFlags.Caps ||| DDSFlags.Height ||| DDSFlags.Width ||| DDSFlags.PixelFormat ||| DDSFlags.MipMapCount ||| DDSFlags.LinearSize

            bw.Write(Array.map (byte) [|0x44; 0x44; 0x53; 0x20|]) // 'DDS '
            bw.Write(124) // dwSize
            bw.Write((int) flags) // dwFlags
            bw.Write(size.Height) // dwHeight
            bw.Write(size.Width) // dwWidth
            bw.Write(mipmap.Length) // dwPitchOrLinearSize
            bw.Write(0) // dwDepth
            bw.Write(mipmaps.Length) // dwMipMapCount

            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0) // dwReserved1

            bw.Write(32) // ddpfPixelFormat // dwSize
            bw.Write(0x4) // dwFlags

            bw.Write(Array.map (byte) [|0x44; 0x58; 0x54; 0x35|]);
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)
            bw.Write(0)

            bw.Write(0x1000 ||| 0x400000 ||| 0x8);
            bw.Write(0); // Caps 2
            
            bw.Write(0);
            bw.Write(0);
            bw.Write(0); // dwReserved2

            mipmaps |> List.iter(fun (data, _) -> bw.Write(data))
        )

    let drawColorTexture midFix texturesPath materialPath (color : Color) = 
      let mipmap =
        [16; 8; 4]
        |> List.map(fun s ->
          let size = new Size(s, s)
          let bmp = new Bitmap(size.Width, size.Height)
          let rect = new Rectangle(new Point(0, 0), size)
          let g = Graphics.FromImage(bmp)
          g.SmoothingMode <- SmoothingMode.AntiAlias;
          g.InterpolationMode <- InterpolationMode.HighQualityBicubic;
          g.PixelOffsetMode <- PixelOffsetMode.HighQuality;
          g.TextRenderingHint <- TextRenderingHint.AntiAliasGridFit;

          g.FillRectangle(new SolidBrush(color), rect);
          g.Flush()

          let data : byte [] = Array.zeroCreate(size.Width * size.Height * 4)
          let bmpData = bmp.LockBits(rect, System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
          Marshal.Copy(bmpData.Scan0, data, 0, bmpData.Stride * bmpData.Height);
          bmp.UnlockBits(bmpData);

          let dest : byte [] = Array.zeroCreate(Squish.GetStorageRequirements(size.Width, size.Height, SquishFlags.kDxt5))
          Squish.Squish.CompressImage(data, size.Width, size.Height, ref dest, SquishFlags.kDxt5);
          (dest, size)
        ) in
        let colorName = sprintf "C%02X%02X%02X%02X" color.A color.R color.G color.B
        dds (texturesPath + "livetext/" + colorName) mipmap;
        generateMaterial ("models/livetext/" + colorName) (materialPath + "livetext/" + colorName);
        "livetext/" + colorName

    let drawGlyph (font : Font) midFix texturesPath materialPath (gl : uint32) =
      let mipmap =
        [100; 50; 25]
        |> List.map(fun s ->
          let size = new Size(s, s)
          let bmp = new Bitmap(size.Width, size.Height)
          let rect = new Rectangle(new Point(0, 0), size)
          let g = Graphics.FromImage(bmp)
          g.SmoothingMode <- SmoothingMode.AntiAlias;
          g.InterpolationMode <- InterpolationMode.HighQualityBicubic;
          g.PixelOffsetMode <- PixelOffsetMode.HighQuality;
          g.TextRenderingHint <- TextRenderingHint.AntiAliasGridFit;

          g.FillRectangle(new SolidBrush(Color.FromArgb(0, 255, 255, 255)), rect);
          g.DrawString (
            (char gl).ToString(),
            new Font(font.FontFamily, 0.6f * float32 s, font.Style),
            new SolidBrush(Color.FromArgb(255, 255, 255, 255)),
            new PointF(0.0f, 0.0f)
            )
          g.Flush()

          let data : byte [] = Array.zeroCreate(size.Width * size.Height * 4)
          let bmpData = bmp.LockBits(rect, System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
          Marshal.Copy(bmpData.Scan0, data, 0, bmpData.Stride * bmpData.Height);
          bmp.UnlockBits(bmpData);

          let dest : byte [] = Array.zeroCreate(Squish.GetStorageRequirements(size.Width, size.Height, SquishFlags.kDxt5))
          Squish.Squish.CompressImage(data, size.Width, size.Height, ref dest, SquishFlags.kDxt5);
          (dest, size)
        ) in
        dds (texturesPath + midFix + gl.ToString()) mipmap;
        generateMaterial (midFix + gl.ToString()) (materialPath + midFix + gl.ToString());
        midFix + gl.ToString()
        
    let squareMesh =
      let polyBase = [(0, 1); (0, 0); (1, 0); (0, 1); (1, 0); (1, 1)]
      let uvBase = [(0, 0); (0, 1); (1, 1); (0, 0); (1, 1); (1, 0)]
      let mesh : MeshData = {
        vertices = List.map (fun (x, y) -> Vector3(float32 x, 0.0f, float32 y)) polyBase;
        normals = List.init 6 (fun _ -> Vector3(0.0f, -1.0f, 0.0f));
        tangents = List.init 6 (fun _ -> Vector3(1.0f, 0.0f, 0.0f));
        uv0 = List.map (fun (x, y) -> Vector2(float32 x, float32 y)) polyBase;
        uv1 = [];
        indices = (0, 1, 2) :: (3, 4, 5) :: []
        }
      mesh

    let generateMesh materialPath outputPath (glyph : uint32) =
      let (blob, mesh) = Mesh.generate squareMesh (materialPath + glyph.ToString())

      let mshPath = outputPath + glyph.ToString() + ".msh"
      let blobPath = mshPath + ".blob"
      File.WriteAllBytes(blobPath, blob)
      File.WriteAllText(mshPath, mesh)

    let generateModel meshPath outputPath (glyph : uint32) =
      let mdl = 
        F ("data",
          P [
            ("collider", P [
              ("params", P [
                ("center", A [V 0; V 0; V 0]);
                ("halfExtents", A [V 0; V 0; V 0])
              ]);
              ("type", S "BOX")
            ]);
            ("lods", A [
              P [
                ("animations", A []);
                ("children", A [
                  P [
                    ("id", S (meshPath + glyph.ToString() + ".msh"));
                    ("transf", A (List.map (V) [1;0;0;0;0;1;0;0;0;0;1;0;0;0;0;1]));
                    ("type", S "MESH")
                  ]
                ]);
                ("events", A []);
                ("matConfigs", A [A [V 0]]);
                ("static", B true);
                ("visibleFrom", V 0);
                ("visibleTo", V 2000)
              ]
            ]);
            ("metadata", A [])
          ]
        )
      |> printLua 0 in

      File.WriteAllText(outputPath + glyph.ToString() + ".mdl", mdl)
      
    let GetFontParams (font : Font) cp =
      use graphics = Graphics.FromHwnd IntPtr.Zero
      graphics.PageUnit <- GraphicsUnit.Pixel;
      let hdc = graphics.GetHdc()
      let hfont = font.ToHfont()
      let hObject = SelectObject(hdc, hfont)
      
      try
        let mutable kerningPairs = GetKerningPairsW(hdc, uint32 0, null);
        let kern : KERNINGPAIR[] = Array.zeroCreate(if kerningPairs <= (uint32 0) then 0 else int kerningPairs)
        //if (kerningPairs <= (uint32 0)) then
        GetKerningPairsW(hdc, kerningPairs, kern) |> ignore
        //else 
          //()
      
        let abcWidths = cp |> List.map (fun c -> 
          let mutable w : ABCFLOAT[] = Array.zeroCreate(1)
          GetCharABCWidthsFloatW(hdc, c, c, w) |> ignore
          (c, Array.head w)
        )

        (kern |> Array.sortBy (fun k -> k.wFirst), abcWidths)
      finally
        SelectObject(hdc, hObject) |> ignore
    

    let generateDescription font cp scriptPath = 
      let (k, w) = GetFontParams font (List.ofSeq cp) in
      let param = w |> List.map(fun (glyph, abc) ->
            glyph,
            abc.abcfA,
            abc.abcfB,
            abc.abcfC,
            k |> Array.filter (fun k -> k.wSecond = uint16 glyph) |> Array.map (fun k -> k.wFirst, k.iKernelAmount)
        ) in

      let lua = 
        R [
          L ("abc" ,
            X (param |> List.map (fun (g, a, b, c, _) ->
              (int g, P [
                ("a", N (float (a / font.Size)));
                ("b", N (float (b / font.Size)));
                ("c", N (float (c / font.Size)));
              ])
            ))
          );
          L ("kern", 
            X (param 
              |> List.map (fun (g, _, _, _, kern) -> (int g, X (kern |> Array.filter (fun (c, _) -> Seq.contains (uint32 c) cp) |> Array.map (fun (c, k) -> (int c, N (float (float32 k /font.Size)))) |> List.ofArray))) 
              |> List.filter (function (_, X []) -> false | _ -> true)
            )
          );
          (RT (A [VA "abc"; VA "kern"]))
        ] 
       |> printLua 0
      System.IO.File.WriteAllText(scriptPath, lua);
            
    let extractPolygon materialPath transMaterialPath outputPath (font : Font) =
      let transform = 
        use path = new GraphicsPath()
        path.AddString("M", font.FontFamily, (int)font.Style, font.Size, new PointF(0.0f, 0.0f), StringFormat.GenericTypographic)
        path.Flatten()
        path.PathPoints 
        |> Seq.map (fun p -> p.Y) 
        |> (fun y -> fun pt -> pt - new Vector2(0.0f, Seq.max y))
      
      fun (glyph : uint32) ->
        //bmp check
        let check (triangles : DelaunayTriangle list) = 
          let size = new Size(1000, 1000)
          let bmp = new Bitmap(size.Width, size.Height)
          let rect = new Rectangle(new Point(0, 0), size)
          let brush = new Pen(new SolidBrush(Color.Black))
          let g = Graphics.FromImage(bmp)
          g.SmoothingMode <- SmoothingMode.AntiAlias;
          g.InterpolationMode <- InterpolationMode.HighQualityBicubic;
          g.PixelOffsetMode <- PixelOffsetMode.HighQuality;
          g.TextRenderingHint <- TextRenderingHint.AntiAliasGridFit;
          g.FillRectangle(new SolidBrush(Color.White), rect);
          g.DrawLine(brush, 0, 800, 1000, 800)
          g.DrawRectangle(brush, rect)
          triangles |> List.iter (fun t ->
            g.DrawPolygon(brush, t.Points |> Seq.map (fun p -> new Point(int (p.X * 10.0), int (-p.Y * 10.0))) |> Seq.toArray)
          )
          g.Flush()
          Directory.CreateDirectory(outputPath + "error/") |> ignore
          bmp.Save(outputPath + "error/" + glyph.ToString() + ".bmp")

        let pts  = 
          use path = new GraphicsPath()
          path.AddString((char glyph).ToString(), font.FontFamily, (int)font.Style, font.Size, new PointF(0.0f, 0.0f), StringFormat.GenericTypographic)
          path.Flatten()
          match path.PointCount with
            | 0 -> [||]
            | _ -> Array.zip path.PathPoints path.PathTypes
      
        let poly = 
          pts 
          |> Array.fold (fun result (pt, t) ->
              match t &&& 0x07uy, t &&& 0xF8uy, result with
                | 0x00uy, _, _ -> [pt] :: result
                | _, 0x80uy, current :: rest -> if List.last current = pt then result else (pt :: current) :: rest
                | _, _, current :: rest -> (pt :: current) :: rest  
                | _ -> result
          ) []
          |> List.map List.rev
          |> List.rev
          |> List.map (List.map (fun p -> new Vector2(p.X, p.Y) |> transform))
          |> List.filter (fun p -> p |> List.length > 2)
      
        let leftMost =
          match poly with
          | [] -> 0.0f
          | _ -> poly |> List.concat |> List.map (fun v -> v.X) |> List.min

        let triangles = 
          match poly.Length with
          | 0 -> []
          | _ ->
            let polygonSet = Poly.generatePolygonSet(poly)
            try 
              P2T.Triangulate(polygonSet)
              polygonSet
              |> List.ofSeq
              |> List.collect(fun p -> List.ofSeq p.Triangles)
            with 
            | _ -> printfn "Error parsing %x" glyph; []
        check triangles;
        let vertices = 
          triangles
          |> List.collect(fun t -> List.ofSeq t.Points)
          |> List.map(fun p -> new Vector3(p.Xf - leftMost, 0.0f, -p.Yf) / font.Size)

        let mesh : MeshData = {
          normals = vertices |> List.map (fun _ -> new Vector3(0.0f, -1.0f, 0.0f));
          vertices = vertices;
          tangents = vertices |> List.map (fun _ -> new Vector3(1.0f, 0.0f, 0.0f));
          uv0 = vertices |> List.map (fun v -> new Vector2(v.X, v.Z));
          uv1 = [];
          indices = List.init (List.length vertices / 3) (fun i -> new Tuple<int, int, int>(i * 3, i * 3 + 1, i * 3 + 2));
        }

        let (blob, msh) = if vertices.IsEmpty then Mesh.generate squareMesh transMaterialPath else Mesh.generate mesh materialPath
      
        let mshPath = outputPath + glyph.ToString() + ".msh"
        let blobPath = mshPath + ".blob"
        File.WriteAllBytes(blobPath, blob)
        File.WriteAllText(mshPath, msh)