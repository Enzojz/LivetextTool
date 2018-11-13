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
      let colorMeshPath = 
        colors
        |> List.map (fun color -> sprintf "%02X%02X%02X" color.R color.G color.B)
        |> List.fold (+) ""
        |> sprintf "%s%s%s/" meshPath midFix
      colorMeshPath
      |> Directory.CreateDirectory
      |> ignore
      allColors 
      |> List.map (fun color -> sprintf "C%02X%02X%02X" color.R color.G color.B)
      |> List.map (sprintf "%s%s%s/" modelPath midFix)
      |> List.map (Directory.CreateDirectory)
      |> ignore
      Directory.CreateDirectory(scriptsPath + "livetext/")|> ignore
      
      let trans = colors |> List.map (fun _ -> Output.drawColorTexture midFix texturesPath materialPath (Color.FromArgb(0, 255, 255, 255)))
      let [basicMaterials; colorMaterials] = 
        [basicColors;colors]
        |> List.map (List.map (Output.drawColorTexture midFix texturesPath materialPath))
        
      let extractBasicPolygon= Output.extractPolygon basicMaterials trans (meshPath + midFix) font
      let extractColorPolygon= Output.extractPolygon colorMaterials trans colorMeshPath font
      let generateModel = Output.generateModel midFix (modelPath + midFix) basicColors colors
      cp
      |> List.toArray
      |> Array.Parallel.iter
        (fun c ->
            extractBasicPolygon c
            extractColorPolygon c
            generateModel c
        )

      Output.generateDescription font cp (scriptsPath + "livetext/" + key + ".lua")

