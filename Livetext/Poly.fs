namespace Livetext
open System.Numerics
open Poly2Tri.Triangulation.Polygon

  module Poly =
    
    type PolyTree = 
      {
        node : Vector2 list;
        children : PolyTree list
      }
    
    let segements poly = List.zip poly (((List.head poly) :: (List.tail poly |> List.rev)) |> List.rev)

    let isInside (rhs : Vector2 list) (lhs : Vector2 list) =
      match rhs with
      | [] -> false
      | _ ->
        let rhsSegs = segements rhs
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
        let r = List.map isInPolygon lhs 
        match List.forall (fun x -> x) r, List.exists (fun x -> x) r with
        | true, true -> true
        | false, false -> false
        | false, true -> (r |> List.filter (fun r -> r) |> List.length) * 10 > (r |> List.length) * 8
        | true, false -> false
    
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
    
    let hypot v (v1, v2) =
      let vecN = Vector2.Normalize(v2 - v1)
      let pl = Vector2.Dot(vecN, v - v1)
      vecN * pl + v1

    let selfIntersection (poly : Vector2 list) =
      let intersects (fl, tl) (fr, tr) = 
        let vr : Vector2 = tr - fr
        let vf : Vector2 = fl - fr
        let vt : Vector2 = tl - fr
        let cf = vr.X * vf.Y - vr.Y * vf.X
        let ct = vr.X * vt.Y - vr.Y * vt.X
        cf * ct < 0.0f
      let intersection (fl, tl) (fr, tr) =
        let p1 = hypot fl (fr, tr)
        let p2 = hypot tl (fr, tr)
        let d1 = (p1 - fl).Length()
        let d2 = (p2 - tl).Length()
        (d1 * p2 + d2 * p1) / (d1 + d2)

      let rec subDivide poly = 
        let segs = segements poly
        segs 
        |> List.mapi (fun i l -> segs |> List.mapi (fun j r -> (intersects l r && intersects r l, i, j)))
        |> List.concat
        |> List.filter (fun (b, _, _) -> b)
        |> List.map (fun (_, i, j) -> intersection segs.[i] segs.[j], i, j)
        |> function
          | [] -> poly :: []
          | (intersection, i, j) :: _ ->
            let ((ll, r), lr) = poly |> List.splitAt (j + 1) |> (fun (h, t) -> h |> List.splitAt (i + 1), t)
            let left = ll @ intersection :: lr
            let right = intersection :: r
            subDivide left @ subDivide right
            //match j - i < i + poly.Length - j with
            //| true -> subDivide left
            //| false -> subDivide right
            ////| _ ->  
      subDivide poly
      //|> List.fold (fun r p -> 
      //  let anyInside = r |> List.map (fun r -> r, isInside p r, isInside r p)
      //  match anyInside |> List.exists (fun (_, x, _) -> x), anyInside |> List.exists (fun (_, _, x) -> x) with
      //  | true, false -> p :: (anyInside |> List.filter (fun (_, x, _) -> not x) |> List.map (fun (x, _, p) -> x))
      //  | false, true -> r
      //  | false, false -> p :: r
      //  | true, true -> p :: r
      //) []

    let generatePolygonSet (polys : Vector2 list list) =
      let toPoly (pts : Vector2 list) = new Polygon(pts |> List.map (fun p -> new PolygonPoint(float p.X, float p.Y)))

      let rec flattenPoly isHole (node : PolyTree) = 
        match isHole with 
        | false ->
            let subPolys = selfIntersection node.node
            let children = node.children |> List.map (fun n -> n.node) 
            let polys = 
              subPolys 
              |> List.map (fun p ->
                  let py = toPoly p
                  children
                  |> List.filter (isInside p)
                  |> List.map (toPoly)
                  |> List.iter (fun p -> py.AddHole(p))
                  py
                )
            polys @ (node.children |> List.map (flattenPoly true) |> List.concat)
        | true ->
            node.children |> List.map (flattenPoly false) |> List.concat
      polys 
      |> List.fold (fun r p -> match tryInsertInto r { node = p; children = []} with r, _ -> r) { node = []; children = [] }
      |> flattenPoly true
      |> Seq.ofList
            
            
        



