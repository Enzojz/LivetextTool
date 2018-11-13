#light
open Livetext
open System.Drawing
open System.IO
open System
open FSharp.Data
open System.Diagnostics

[<EntryPoint>]
let main argv = 
    //[0x20..0x7F];    // Latin
    //[0x0A1..0x024F]; // Latin
    //[0x400..0x52F];  // Cyrilic
    //[0x0370..0x03FF];// Greek
    //[0x2030..0x2031];// Per mille
    //[0x2200..0x22FF];// Math
    //[0x2190..0x21FF];// Arrows
    //[0X2900..0x297F];// Arrows
    //[0x4E00..0x9FEF]; // 基本汉字
    //[0x3040..0x309F]; // 平仮名
    //[0x30A0..0x30FF]; // 片仮名
    //[0x1100..0x11FF]; // 한글
    
    
    printfn "!NOT ALL FONTS ARE FREEWARE!"
    printfn "Press Y to confirm that you have the correct license for the intended usage, "
    printfn "otherwise please press any key to close..."

    match Console.ReadKey().KeyChar with
    | 'Y' | 'y' ->
      printfn "\nGenerating the meshes, please wait..."
      if Directory.Exists("res") then
        let res = new DirectoryInfo("res")
        res.EnumerateFiles() |> Seq.iter (fun f -> f.Delete())
        res.EnumerateDirectories() |> Seq.iter (fun d -> d.Delete(true))
      else
        ()
    
      let value = JsonValue.Load("livetext.json")

      let basicColors = 
        Color.FromArgb(255, 242, 242, 242) ::
        Color.FromArgb(255, 0, 0, 0) ::
        Color.FromArgb(255, 255, 255, 255)
        :: []

      let cp, tasks = 
        match value with
        | JsonValue.Record [|("charset", JsonValue.Array charset); ("tasks", JsonValue.Array tasks)|] ->
          let cp = 
            charset 
            |> Array.map (function JsonValue.Array [|JsonValue.Number f; JsonValue.Number t; _|] -> [f .. t])
            |> Array.toList
            |> List.concat
            |> List.map (uint32)
          let task = 
            tasks
            |> Array.map (function 
                  JsonValue.Record 
                  [|
                  ("font", JsonValue.String font);
                  ("isItalic", JsonValue.Boolean isItalic);
                  ("isBold", JsonValue.Boolean isBold);
                  ("colors", JsonValue.Array colors)
                  |] ->
                    font, 
                    colors |> Array.toList |> List.map (function JsonValue.String color -> System.Drawing.ColorTranslator.FromHtml(color)),
                    match isItalic, isBold with 
                    | (false, false) -> FontStyle.Regular
                    | (false, true) -> FontStyle.Bold
                    | (true, false) -> FontStyle.Italic
                    | (true, true) -> FontStyle.Bold ||| FontStyle.Italic
            )
          (cp, task)
        
      tasks |> Array.iter (fun (font, colors, style) -> Task.task cp font style (basicColors @ (colors |> List.except basicColors)) )
    
      //Task.task cp "Source Han Sans CN Normal" FontStyle.Regular
      printfn "Total converted glyphs: %d" (List.length cp)
      printfn "Press space to open the output folder, any other key to close..."
      match Console.ReadKey().Key with
      | ConsoleKey.Spacebar -> 
        Process.Start "res" |> ignore;
        0
      | _ -> 0
    | _ -> 0
