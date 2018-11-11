namespace Livetext
open System

module Lua = 
    type Lua = 
        | V of int
        | B of bool
        | N of float
        | S of string
        | F of (string * Lua)
        | C of (bool * Lua)
        | P of (string * Lua) list
        | X of (int * Lua) list
        | L of (string * Lua)
        | A of Lua list
        | R of Lua list
        | RT of Lua
        | VA of string
    
    let rec printLua indent (node : Lua) = 
        let ind str = 
            function 
            | true -> (Environment.NewLine + (String.replicate indent "  ") + str)
            | false -> str
        match node with
        | R ls -> ls |> List.map (printLua indent) |> String.concat Environment.NewLine
        | V num -> sprintf "%d" num
        | B bln -> sprintf "%s" (if bln then "true" else "false")
        | N flt -> sprintf "%f" flt
        | S str -> @"""" + str + @""""
        | P ls -> 
          "{" + Environment.NewLine + (ls 
                    |> List.map (fun (name, x) -> name + " = " + (printLua (indent + 1) x)) 
                    |> String.concat ("," + Environment.NewLine))
            |> fun str -> str + (ind " }" (str.Contains(Environment.NewLine)))
        | X ls -> 
          "{" + Environment.NewLine + (ls 
                    |> List.map (fun (name, x) -> "[" + name.ToString() + "]" + " = " + (printLua (indent + 1) x)) 
                    |> String.concat ("," + Environment.NewLine))
            |> fun str -> str + (ind " }" (str.Contains(Environment.NewLine)))
        | A ls -> 
            "{ " + (ls
                    |> List.map (printLua (indent + 1))
                    |> String.concat ", ")
            |> fun str -> str + (ind " }" (str.Contains(Environment.NewLine)))
        | L (v, x) -> (sprintf "local %s = " v) + (printLua indent x)
        | F (f, x) -> (sprintf "function %s() return " f) + (printLua (indent + 1) x) + " end"
        | VA v -> v
        | RT l -> (sprintf "return %s" (printLua indent l))
        | C (c, x) -> 
            if c then (printLua indent x)
            else ""
        |> fun rs -> 
            match node with
            | C _ | F _ | V _ -> rs
            | _ -> ind rs (rs.Length > 30)