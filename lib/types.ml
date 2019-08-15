open Core_kernel
open Fn

module Tag = struct
  module T = struct
    type t = string
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make(T)
end

module Field = struct
  module T = struct
    type t = Ident of string
           | Bracketed of t list
           | Quoted of t list
           | Command of string * t list option
           | Text of string
           | Sep of string
    [@@deriving compare, sexp]
  end

  include T

  let is_text = function Text _ -> true | _ -> false

  let is_simple_command = function Command _ | _ -> false

  let rec is_word = function
    | Text _ -> true
    | Command (_, Some vs)
    | Quoted vs
    | Bracketed vs -> List.exists vs ~f:(fun v -> is_word v)
    | _ -> false

  let is_lower =
    let rec aux p br = function
      | Text v ->
        if br && not p then
          false
        else if not @@ String.is_empty v then
          Char.is_lowercase v.[0]
        else
          false
      | Command (_, Some vs) | Quoted vs ->
        List.fold_until vs ~init:p ~finish:(const false) ~f:(fun c v ->
            if is_word v then
              Stop (aux c br v)
            else if is_simple_command v then
              Continue true
            else
              Continue c
          )
      | Bracketed vs ->
        List.fold_until vs ~init:false ~finish:(const false) ~f:(fun c v ->
            if is_word v then
              Stop (aux c true v)
            else if is_simple_command v then
              Continue true
            else
              Continue c
          )
      | _ -> false
    in aux false false

  let first_lower_pos ts =
    List.findi ts ~f:(const is_lower) |> Option.map ~f:fst

  let last_lower_pos ts =
    List.rev ts |> List.findi ~f:(const is_lower) |> Option.map ~f:fst

  let first_word_pos ts =
    List.findi ts ~f:(const is_lower) |> Option.map ~f:fst

  let last_word_pos ts =
    List.rev ts |> List.findi ~f:(const is_lower) |> Option.map ~f:fst

  let rec word_count = function
    | Text _ -> 1
    | Bracketed vs ->
      if List.exists vs ~f:is_word then 1 else 0
    | Quoted vs | Command (_, Some vs) ->
      List.fold vs ~init:0 ~f:(fun acc n -> acc + word_count n)
    | _ -> 0

  let inner_string = function
    | Text t | Ident t | Sep t -> Some t
    | _ -> None


  let unicode_compose m vs = Option.bind vs ~f:(fun vs' ->
      if List.is_empty vs' || not @@ is_text @@ List.hd_exn vs' then
        None
      else match m, Option.value_exn (inner_string @@ List.hd_exn vs') with
        | "'", "a" -> Some "á"
        | "'", "e" -> Some "é"
        | "'", "o" -> Some "ó"
        | "'", "u" -> Some "ú"
        | "\"", "a" -> Some "ä"
        | "\"", "e" -> Some "ë"
        | "\"", "i" -> Some "ï"
        | "\"", "o" -> Some "ö"
        | "\"", "u" -> Some "ü"
        | "/", "l" -> Some "ł"
        | ",", "c" -> Some "ç"
        | _ -> None
    )

  let normalise_spaces s =
    String.split s ~on:' '
    |> List.filter ~f:(Fn.compose not String.is_empty)
    |> String.concat ~sep:" "

  let rec to_string m = function
    | Ident v -> begin match Tag.Map.find m v with
        | Some v' -> list_to_string m v'
        | None -> ""
      end
    | Bracketed vs | Quoted vs ->
      List.map vs ~f:(to_string m) |> String.concat
    | Command (c, r) -> begin match unicode_compose c r with
        | Some c' -> c'
        | None when c = "&" && Option.is_none r -> c
        | _ ->
          Option.map r ~f:(fun v -> String.concat @@ List.map v ~f:(to_string m))
          |> Option.value ~default:""
      end
    | Text t -> t
    | Sep s -> normalise_spaces s

  and list_to_string m =
    compose String.concat (List.map ~f:(to_string m))

  let rec fold_strings m = function
    | Ident k -> begin match Tag.Map.find m k with
        | Some v -> Quoted v
        | None -> Text ""
      end
    | Quoted vs -> Quoted (List.map vs ~f:(fold_strings m))
    | Bracketed vs -> Bracketed (List.map vs ~f:(fold_strings m))
    | Command (c, Some vs) -> Command (c, Some (List.map vs ~f:(fold_strings m)))
    | v -> v

  let list_fold_strings m = List.map ~f:(fold_strings m)

  include Comparable.Make(T)

end

module Entry = struct
  module T = struct
    type t = Comment of Field.t list
           | Preamble of Field.t list
           | String of {
               tag   : Tag.t;
               value : Field.t list;
             }
           | Entry of {
               kind   : string;
               key    : string;
               fields : Field.t list Tag.Map.t;
             }
    [@@deriving compare, sexp]
  end

  include T

  let list_fold_strings m =
    compose snd @@ List.fold_map ~init:m ~f:(fun m' v -> match v with
        | Comment _ | Preamble _ -> (m', v)
        | String { tag; value } ->
          let value' = Field.list_fold_strings m' value in
          (Map.add_exn m' ~key:tag ~data:value', String { tag; value = value' })
        | Entry { kind; key; fields } ->
          (m', Entry { kind; key; fields = Map.map ~f:(Field.list_fold_strings m) fields })
      )

  include Comparable.Make(T)
end
