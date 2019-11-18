
#light
namespace Livetext

open System
open System.Numerics
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Text
open System.Runtime.InteropServices
open System.IO
open Squish
open Mesh
open Lua
open Import
open LibTessDotNet

module Output =

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

          let rec exchangeRB (data : byte list) = 
            match data with
            | a :: r :: g :: b :: rest -> g :: r :: a :: b :: (exchangeRB rest)
            | [] -> []
          
          let dest : byte [] = Array.zeroCreate(Squish.GetStorageRequirements(size.Width, size.Height, SquishFlags.kDxt5))
          Squish.Squish.CompressImage(data |> Array.toList |> exchangeRB |> List.toArray, size.Width, size.Height, ref dest, SquishFlags.kDxt5);
          (dest, size)
        ) in
        let colorName = sprintf "C%02X%02X%02X%02X" color.A color.R color.G color.B
        dds (texturesPath + "livetext/" + colorName) mipmap;
        generateMaterial ("models/livetext/" + colorName) (materialPath + "livetext/" + colorName);
        "livetext/" + colorName
 
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
      
    let generateModel meshPath outputPath colors materials (glyph : uint32) =
      let mdl meshPath material = 
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
                ("node", P[
                  ("children", A [
                      P [
                      ("materials", A [S material]);
                      ("mesh", S meshPath);
                      ("name", S (sprintf "%d" glyph))
                      ("transf", A (List.map (V) [1;0;0;0;0;1;0;0;0;0;1;0;0;0;0;1]));
                      ]
                  ]);
                  ("name", S "RootNode");
                  ("transf", A (List.map (V) [1;0;0;0;0;1;0;0;0;0;1;0;0;0;0;1]))
                ]);
                ("static", B true);
                ("visibleFrom", V 0);
                ("visibleTo", V 2000)
              ]
            ]);
            ("metadata", A [])
          ]
        )
      |> printLua 0 in

      colors 
      |> List.map (fun (color : Color) -> 
        sprintf "%sC%02X%02X%02X/%s.mdl" outputPath color.R color.G color.B (glyph.ToString()),
        meshPath + glyph.ToString() + ".msh"
      )
      |> List.zip materials
      |> List.iter (fun (material, (mdlPath, meshPath)) -> File.WriteAllText(mdlPath, mdl meshPath (material  + ".mtl")))
                  
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
            
    let extractPolygon outputPath (font : Font) =
      let transform = 
        use path = new GraphicsPath()
        path.AddString("M", font.FontFamily, (int)font.Style, font.Size, new PointF(0.0f, 0.0f), StringFormat.GenericTypographic)
        path.Flatten()
        path.PathPoints 
        |> Seq.map (fun p -> p.Y) 
        |> (fun y -> fun pt -> pt - new Vector2(0.0f, Seq.max y))
      
      fun (glyph : uint32) ->
        //bmp check
        let check (poly : Vector2 list list) (vertices : Vector2 list) = 
          let triangles = 
            vertices 
            |> List.fold (fun t v -> 
                match t with 
                | [v1] :: r -> [v; v1] :: r 
                | [v2; v1] :: r -> [v; v2; v1] :: r 
                | _ -> [v] :: t 
              ) []

          let size = new Size(1000, 1300)
          let bmp = new Bitmap(size.Width, size.Height)
          let rect = new Rectangle(new Point(0, 0), size)
          let brushOutline = new Pen(new SolidBrush(Color.Black), 2.0f)
          let brush = new Pen(new SolidBrush(Color.Red))
          let g = Graphics.FromImage(bmp)
          g.SmoothingMode <- SmoothingMode.AntiAlias;
          g.InterpolationMode <- InterpolationMode.HighQualityBicubic;
          g.PixelOffsetMode <- PixelOffsetMode.HighQuality;
          g.TextRenderingHint <- TextRenderingHint.AntiAliasGridFit;
          g.FillRectangle(new SolidBrush(Color.White), rect);
          g.DrawLine(brush, 0, 300, 1000, 300)
          g.DrawRectangle(brush, rect)
          triangles |> List.iter (fun t ->
            g.DrawPolygon(brush, t |> List.map (fun p -> new Point(int (p.X * 10.0f), int (-p.Y * 10.0f  + 300.0f))) |> List.toArray)
          )
          poly 
          |> List.map (fun (hp :: rp) -> hp::rp@[hp])
          |> List.map (List.map (fun p -> new PointF(p.X * 10.0f, -p.Y * 10.0f + 300.0f)) >> List.toArray)
          |> List.iteri (fun i p -> 
            g.DrawLines(brushOutline, p); 
            g.DrawString(i.ToString(), new Font("Arial",10.0f), new SolidBrush(Color.Black), p.[0]);
            p |> Array.rev |> Array.tail |> Array.rev |> Array.iteri (fun i p -> g.DrawString("  " + i.ToString(), new Font("Arial",10.0f), new SolidBrush(Color.Blue), p));
          )
          g.Flush()
          Directory.CreateDirectory("check/" + outputPath) |> ignore
          bmp.Save("check/" + outputPath + glyph.ToString() + ".bmp")

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
        
        let tess = new Tess()

        match poly.Length with
        | 0 -> ()
        | _ -> 
          poly |> List.iter (fun poly -> 
            let ps = poly |> List.map (fun v -> new ContourVertex( Position = new Vec3(X = v.X, Y = v.Y, Z = 0.0f) ))
            tess.AddContour(List.toArray ps)
          )
        
        tess.Tessellate(WindingRule.NonZero, ElementType.Polygons, 3)
        
        let preVertices = match poly.Length with
          | 0 -> []
          | _ -> tess.Elements
                 |> Array.map (fun e -> new Vector2(tess.Vertices.[e].Position.X, tess.Vertices.[e].Position.Y))
                 |> Array.toList

        let vertices = preVertices |> List.map(fun p -> new Vector3(p.X - leftMost, 0.0f, -p.Y) / font.Size)
        
        check poly preVertices

        let mesh : MeshData = {
          normals = vertices |> List.map (fun _ -> new Vector3(0.0f, -1.0f, 0.0f));
          vertices = vertices;
          tangents = vertices |> List.map (fun _ -> new Vector3(1.0f, 0.0f, 0.0f));
          uv0 = vertices |> List.map (fun v -> new Vector2(v.X, v.Z));
          uv1 = [];
          indices = List.init (List.length vertices / 3) (fun i -> new Tuple<int, int, int>(i * 3, i * 3 + 1, i * 3 + 2));
        }

        //let (blob, msh) = if vertices.IsEmpty then Mesh.generate squareMesh (List.init (List.length materials) (fun _ -> transMaterial)) else Mesh.generate mesh materials
        let (blob, msh) = if vertices.IsEmpty then Mesh.generate squareMesh else Mesh.generate mesh
          
        let mshPath = outputPath + glyph.ToString() + ".msh"
        let blobPath = mshPath + ".blob"
        File.WriteAllBytes(blobPath, blob)
        File.WriteAllText(mshPath, msh)
        vertices.IsEmpty