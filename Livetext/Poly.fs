namespace Livetext
open System.Numerics
open Poly2Tri.Triangulation.Polygon

  module Poly =
    
    type PolyTree = 
      {
        node : Vector2 list;
        children : PolyTree list
      }
    
    let isInside (rhs : Vector2 list) (lhs : Vector2 list) =
      match rhs with
      | [] -> false
      | _ ->
        let rhsSegs = 
          List.zip rhs (((List.head rhs) :: (List.tail rhs |> List.rev)) |> List.rev)
        let isInPolygon (point : Vector2) =
          let mRot = // Rotation to avoid parallel segments and interestion on the segement extremities
            List.map (fun (p1, p2) -> p2 - p1) rhsSegs @ List.map (fun p -> p - point) rhs
            |> List.filter (fun v -> v.Length() > 0.0f)
            |> List.map (fun v -> atan2 v.Y v.X)
            |> List.distinct
            |> List.sortBy (fun r -> abs r)
            |> List.except [0.0f]
            |> function fst :: _ -> -0.5f * fst | _ -> 0.0f
            |> Matrix3x2.CreateRotation
          rhsSegs 
            |> List.map (fun (p1, p2) ->
              Vector2.Transform(p1 - point, mRot),
              Vector2.Transform(p2 - point, mRot)
            )
            |> List.filter (fun (p1, p2) -> p1.Y * p2.Y < 0.0f)
            |> List.map (fun (p1, p2) ->  p1 + (p2 - p1) * (-p1.Y) / (p2.Y - p1.Y))
            |> List.partition (fun p -> p.X < 0.0f)
            |> fun (l, r) -> l.Length % 2 = 1 && r.Length % 2 = 1
        lhs |> List.fold (fun r p -> r && isInPolygon p) true
    
    let rec tryInsertInto (node : PolyTree) (poly : PolyTree) =
      match node.node, isInside node.node poly.node, isInside poly.node node.node with
      | [], _, _
      | _, true, false -> 
        match List.fold (fun (poly :: result, state) c -> 
            match tryInsertInto c poly with
            | r, true -> r :: result, true 
            | _, false -> poly :: c :: result, state) ([poly], false) node.children with
        | children, true -> { node with children = children}, true
        | _, false -> { node with children = poly :: node.children }, true
      | _, false, true -> { poly with children = node :: poly.children }, true
      | _, false, false -> node, false
      | _, true, true -> assert false; node, false // Only node = poly in this case which is not vaild
      
    let generatePolygonSet (polys : Vector2 list list) =
      let toPoly (pts : Vector2 list) = new Polygon(pts |> List.map (fun p -> new PolygonPoint(float p.X, float p.Y)))

      let rec flattenPoly isHole (node : PolyTree) = 
        match isHole with 
        | false ->
            let p = toPoly node.node
            node.children |> List.iter (fun c -> p.AddHole(toPoly c.node))
            p :: (node.children |> List.map (flattenPoly true) |> List.concat)
        | true ->
            node.children |> List.map (flattenPoly false) |> List.concat
      
      polys 
      |> List.fold (fun r p -> match tryInsertInto r { node = p; children = []} with r, _ -> r) { node = []; children = [] }
      |> flattenPoly true
      |> Seq.ofList
            
            
        



