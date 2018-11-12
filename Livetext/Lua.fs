namespace Livetext
open System

module Lua = 
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
        | C of (bool * Lua)
        | RT of Lua                // Return
        | VA of string             // Ref
    

    let rec printLua indent (node : Lua) = 
        let sindent = String.replicate indent "  "
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
          sprintf "{\n%s\n%s}" 
            (ls 
              |> List.map (
                fun (name, x) -> 
                  let children = (printLua (indent + 1) x)
                  sprintf "%s%s = %s" sindent name children
              )
              |> String.concat ("," + Environment.NewLine)
            )
            (String.replicate (indent - 1) "    ")
        | X ls -> printLua indent (P (List.map (fun (name, x) -> sprintf "[%d]" name, x) ls))
        | A ls -> 
          match ls |> List.forall (function V _ | B _ | N _ -> true | _ -> false) with
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
                (String.replicate (indent - 1) "    ")
        | L (v, x) -> (sprintf "local %s = " v) + (printLua (indent + 1) x)
        | F (f, x) -> (sprintf "function %s() return " f) + (printLua (indent + 1) x) + " end"
        | VA v -> v
        | RT l -> (sprintf "return %s" (printLua (indent + 1) l))
        | C (c, x) -> 
            if c then (printLua indent x)
            else ""