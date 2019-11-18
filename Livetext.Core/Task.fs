namespace Livetext
open System.Drawing

module Task = 
  open System.IO

  let task cp (facename : string) (style : FontStyle) (w :: b :: pw :: colors  as allColors : Color list) = 
      let basicColors = [w; b; pw]

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

      allColors 
      |> List.map (fun color -> sprintf "C%02X%02X%02X" color.R color.G color.B)
      |> List.map (sprintf "%s%s%s/" modelPath midFix)
      |> List.map (Directory.CreateDirectory)
      |> ignore
      Directory.CreateDirectory(scriptsPath + "livetext/")|> ignore
      
      let trans = Output.drawColorTexture midFix texturesPath materialPath (Color.FromArgb(0, 255, 255, 255))
      let materials = List.map (Output.drawColorTexture midFix texturesPath materialPath) allColors
        
      let extractPolygon= Output.extractPolygon (meshPath + midFix) font
      let generateModel = Output.generateModel midFix (modelPath + midFix) allColors
      cp
      //|> List.iter
      |> List.toArray
      |> Array.Parallel.iter
        (fun c ->
            let isEmpty = extractPolygon c in
            generateModel materials c
        )

      Output.generateDescription font cp (scriptsPath + "livetext/" + key + ".lua")

