namespace Livetext

open System
open Lua
open System.Numerics

module Mesh =

  type MeshData = {
        normals : Vector3 list;
        vertices : Vector3 list;
        tangents : Vector3 list;
        uv0 : Vector2 list;
        uv1 : Vector2 list;
        indices : (int * int * int) list
  }

  type ByteData = {
        normals : byte list;
        vertices : byte list;
        tangents : byte list;
        uv0 : byte list;
        uv1 : byte list;
        indices : (int * int * int) list
  }

  type NumData = {
    normals : int;
    vertices : int;
    tangents : int;
    uv0 : int;
    uv1 : int
  }

  let toBytes f vectors =
      let toByte(values : float32 list) = values |> List.collect(BitConverter.GetBytes >> Array.toList)
      vectors |> List.collect(f >> toByte)

  let convertToBytes(mesh : MeshData) = {
    normals = mesh.normals |> toBytes(fun n -> [n.X; n.Y; n.Z]);
    vertices = mesh.vertices |> toBytes(fun n -> [n.X; n.Y; n.Z]);
    tangents = mesh.tangents |> toBytes(fun n -> [n.X; n.Y; n.Z; 1.0f]);
    uv0 = mesh.uv0 |> toBytes(fun n -> [n.X; n.Y]);
    uv1 = mesh.uv1 |> toBytes(fun n -> [n.X; n.Y]);
    indices = mesh.indices
  }

  let blobGen(mesh : ByteData) =
      let indices =
          mesh.indices
          |> List.collect(fun (a, b, c) -> [a; b; c])
          |> List.collect(BitConverter.GetBytes >> Array.toList)
      mesh.vertices @ mesh.uv0 @ mesh.normals @ mesh.tangents @ mesh.uv1 @ indices |> List.toArray

  let meshGen (mesh : ByteData) material =
    let d = sprintf "%d"

    let counts = {
      normals = mesh.normals.Length;
      vertices = mesh.vertices.Length;
      tangents = mesh.tangents.Length;
      uv0 = mesh.uv0.Length;
      uv1 = mesh.uv1.Length
      }

    let offsets = {
      vertices = 0;
      uv0 = counts.vertices;
      normals = counts.vertices + counts.uv0;
      tangents = counts.vertices + counts.uv0 + counts.normals;
      uv1 = counts.vertices + counts.uv0 + counts.normals + counts.tangents
      }

    let subMeshIndices data material offset =
      let indiceLength = data.indices.Length * 3 * sizeof<int>
      let indiceAttr count offset =
          P [
            ("count", V count);
            ("offset", V offset)
          ]
      P [
        ("indices",
          P [
            ("normal", indiceAttr indiceLength offset);
            ("position", indiceAttr indiceLength offset);
            ("tangent", indiceAttr indiceLength offset);
            ("uv0", indiceAttr indiceLength offset);
            ]);
        ("materials", A [S(material + @".mtl")])]

    let vertexAttr count numComp offset = 
      P [
        ("count", V count);
        ("numComp", V numComp);
        ("offset", V offset)
      ]

    F("data",
      P [
        ("animations", A []);
        ("matConfigs", A [A [V 0]]);
        ("subMeshes", A [subMeshIndices mesh material (offsets.uv1 + counts.uv1)]);
        ("vertexAttr",
            P [
              ("normal", vertexAttr counts.normals 3 offsets.normals);
              ("position", vertexAttr counts.vertices 3 offsets.vertices);
              ("tangent", vertexAttr counts.tangents 4 offsets.tangents);
              ("uv0", vertexAttr counts.uv0 2 offsets.uv0);
            ]
          )
        ]
      )
    |> printLua 0

  let generate mesh material =
    let b = convertToBytes mesh
    let blob = blobGen b
    let desc = meshGen b material
    (blob, desc)