namespace Livetext
open System.Drawing

module Task = 
  open System.IO

  let task cp (facename : string) (style : FontStyle) (colors : Color list) = 
      
      let scriptsPath = "res/scripts/"
      let texturesPath = "res/textures/models/"
      let meshPath = "res/models/mesh/"
      let materialPath = "res/models/material/"
      let modelPath = "res/models/model/"
      
      let font = new Font(new FontFamily(facename), 80.0f, style, GraphicsUnit.Pixel)
      let key = (font.FontFamily.Name + (if font.Style = FontStyle.Regular then "" else ("_" + font.Style.ToString()))).ToLower().Replace(' ', '_')

      let midFix = "livetext/" + key + "/"
            
      Directory.CreateDirectory(materialPath + "livetext/")    |> ignore
      Directory.CreateDirectory(texturesPath + "livetext/")    |> ignore
      Directory.CreateDirectory(meshPath + midFix)        |> ignore
      colors |> List.map (fun color -> Directory.CreateDirectory(modelPath + midFix + (sprintf "C%02X%02X%02X" color.R color.G color.B) + "/")) |> ignore
      Directory.CreateDirectory(scriptsPath + "livetext/")|> ignore
      
      //cp.ToList().AsParallel()
      //.ForAll(c =>
      //{
      //    Output.drawGlyph(font, midFix, texturesPath, materialPath, c);
      //    Output.generateMesh(midFix, meshPath + midFix, c);
      //    Output.generateModel(midFix, modelPath + midFix, c);
      //}
      //);

      let trans = colors |> List.map (fun _ -> Output.drawColorTexture midFix texturesPath materialPath (Color.FromArgb(0, 255, 255, 255)))
      let materials = 
        colors
        |> List.map (Output.drawColorTexture midFix texturesPath materialPath)

      let extractPolygon= Output.extractPolygon materials trans (meshPath + midFix) font
      let generateModel = Output.generateModel midFix (modelPath + midFix) colors
      cp
      |> List.toArray
      |> Array.Parallel.iter
        (fun c ->
            extractPolygon c
            generateModel c
        )

      Output.generateDescription font cp (scriptsPath + "livetext/" + key + ".lua")

