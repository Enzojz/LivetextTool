namespace Livetext
open System

module Lua = 
    let aindent = "  "
    type Lua = 
        | V of int
        | B of bool
        | N of float
        | S of string
        | L of (string * Lua)      // local x = 
        | F of (string * Lua)      // function
        | P of (string * Lua) list // { x = .. }
        | X of (int * Lua) list    // [x] = ..
        | A of Lua list            // a table
        | R of Lua list            // Root
        | RT of Lua                // Return
        | VA of string             // Ref
    
    let isSimple nodes = nodes |> List.forall (function V _ | B _ | N _ -> true | _ -> false)

    let rec printLua indent (node : Lua) = 
        let sindent = String.replicate indent aindent
        let ind str = 
            function 
            | true -> (Environment.NewLine + sindent + str)
            | false -> str
        match node with
        | R ls -> ls |> List.map (printLua indent) |> String.concat Environment.NewLine
        | V num -> sprintf "%d" num
        | B bln -> sprintf "%s" (if bln then "true" else "false")
        | N flt -> sprintf "%f" flt
        | S str -> @"""" + str + @""""
        | P ls -> 
          match ls |> List.map (fun (_, n) -> n) |> isSimple with
          | true when ls |> List.length < 5 -> 
            sprintf "{ %s }" 
              (ls 
                |> List.map (
                  fun (name, x) -> 
                    let children = (printLua (indent + 1) x)
                    sprintf "%s = %s" name children
                )
                |> String.concat (", ")
              )
          | _ ->
              sprintf "{\n%s\n%s}" 
                (ls 
                  |> List.map (
                    fun (name, x) -> 
                      let children = (printLua (indent + 1) x)
                      match x with
                      | P z when z |> List.map (fun (_, n) -> n) |> isSimple -> 
                        let maxName = match ls with [] -> 0 | _ -> ls |> List.map (fun (n, _) -> n.Length) |> List.max
                        sprintf "%s%s = %s" sindent (name.PadRight(maxName)) children
                      | _ -> sprintf "%s%s = %s" sindent name children
                  )
                  |> String.concat ("," + Environment.NewLine)
                )
                (String.replicate (indent - 1) aindent)
        | X ls -> printLua indent (P (List.map (fun (name, x) -> sprintf "[%d]" name, x) ls))
        | A ls -> 
          match isSimple ls with
            | true -> 
              sprintf "{ %s }"
                (ls |> List.map (printLua (indent + 1)) |> String.concat ", ")
            | false ->
              sprintf "{\n%s\n%s}" 
                (ls 
                  |> List.map (
                    fun x -> 
                      let children = (printLua (indent + 1) x)
                      sprintf "%s%s" sindent children
                  )
                  |> String.concat ("," + Environment.NewLine)
                )
                (String.replicate (indent - 1) aindent)
        | L (v, x) -> (sprintf "local %s = " v) + (printLua (indent + 1) x)
        | F (f, x) -> (sprintf "function %s() return " f) + (printLua (indent + 1) x) + " end"
        | VA v -> v
        | RT l -> (sprintf "return %s" (printLua (indent + 1) l))