type 'a sets = Empty | Set of 'a list list
(* let q = *)

type ('a,'b) triple = ('a * 'a * 'b)

type color = Red | Black

type suit = Hearts | Clubs | Spades | Diamonds

let suit_to_color suit =
  match suit with
  | Hearts -> Red
  | Clubs -> Black
  | Spades -> Black
  | Diamonds -> Red

(* type ('k,'v) tree = Leaf | Node of ('k,'v) tree * ('k,'v) * ('k,'v) tree

let rec contains (t:('k,'v) tree) (n:('k,'v)) : bool =
  match t with
  | Leaf -> false
  | Node(lt,x,rt) -> x = n || (contains lt n) || (contains rt n)

let rec insert (t:'a tree) (n:'a) : 'a tree =
  match t with
  | Leaf -> Node(Leaf, n, Leaf)
  | Node(lt,x,rt) -> if x = n then t else
                        if n < x then Node(insert lt n, x, rt) else
                            Node(lt,x,insert rt n)*)

type 'a vertex = Empty | Vertex of 'a * 'a vertex list

let extract_room_ids (t : Yojson.Basic.json) : string list =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "rooms"
  |> flatten
  |> filter_member "id"
  |> filter_string

let extract_room_descriptions (t : Yojson.Basic.json) : string list =
  let open Yojson.Basic.Util in
  [t]
  |> filter_member "rooms"
  |> flatten
  |> filter_member "description"
  |> filter_string

let t = Yojson.Basic.from_file "minimal.json"
let ids = extract_room_ids t
let desciprtions = extract_room_descriptions t
