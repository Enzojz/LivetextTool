namespace Livetext
open System.Drawing

module Task = 
  open System.IO

  let task cp (facename : string) (style : FontStyle) ((r, g, b) : (int * int * int)) = 
      
      let scriptsPath = "res/scripts/"
      let texturesPath = "res/textures/models/"
      let meshPath = "res/models/mesh/"
      let materialPath = "res/models/material/"
      let modelPath = "res/models/model/"
      
      let font = new Font(new FontFamily(facename), 80.0f, style, GraphicsUnit.Pixel)
      let key = (font.FontFamily.Name + (if font.Style = FontStyle.Regular then "" else ("_" + font.Style.ToString()))).ToLower().Replace(' ', '_')

      let midFix = "livetext/" + key + "/" + sprintf "C%02X%02X%02X" r g b + "/"
            
      Directory.CreateDirectory(materialPath + "livetext/")    |> ignore
      Directory.CreateDirectory(texturesPath + "livetext/")    |> ignore
      Directory.CreateDirectory(meshPath + midFix)        |> ignore
      Directory.CreateDirectory(modelPath + midFix)       |> ignore
      Directory.CreateDirectory(scriptsPath + "livetext/")|> ignore
      
      //cp.ToList().AsParallel()
      //.ForAll(c =>
      //{
      //    Output.drawGlyph(font, midFix, texturesPath, materialPath, c);
      //    Output.generateMesh(midFix, meshPath + midFix, c);
      //    Output.generateModel(midFix, modelPath + midFix, c);
      //}
      //);

      let mat = Output.drawColorTexture midFix texturesPath materialPath (Color.FromArgb(255, r, g, b))
      let trans = Output.drawColorTexture midFix texturesPath materialPath (Color.FromArgb(0, 255, 255, 255))

      let extractPolygon= Output.extractPolygon mat trans (meshPath + midFix) font
      let generateModel = Output.generateModel midFix (modelPath + midFix)
      cp
      |> List.toArray
      |> Array.Parallel.iter
        (fun c ->
            extractPolygon c
            generateModel c
        )

      Output.generateDescription font cp (scriptsPath + "livetext/" + key + ".lua")

