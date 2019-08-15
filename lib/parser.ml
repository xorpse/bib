open Core_kernel
open Fn

open Types
open Angstrom

let is_alpha = Char.is_alpha
let is_alphanum = Char.is_alphanum
let is_digit = Char.is_digit
let is_whitespace = Char.is_whitespace

let comment =
  char '%' *> skip_many (not_char '\n') *> (end_of_line <|> end_of_input)
let spaces = skip_while is_whitespace
let skip_wsc = spaces *> skip_many (comment *> spaces)

(* Identifiers *)

let is_ident_start = is_alpha
let is_ident_rest = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | ':' | '-' | '_' -> true
  | _ -> false

let identifier =
  satisfy is_ident_start   >>= fun c ->
  take_while is_ident_rest >>| fun r ->
  String.of_char c ^ r

(* Field values *)

let is_sep =
  function ' ' | '\t' | '\n' | '\r' | '-' | '~' -> true | _ -> false

(* Needs to be conjunction not disjuntion *)
let is_text =
  compose not (String.mem ",{}\\\" \t\n\r-~")


let field_value_inner = fix (fun field_value_inner ->
  (char '{' *> many field_value_inner <* char '}' >>| fun v -> Field.Bracketed v)
  <|>
  (char '\\' *>
    ((char '{' <|>  char '}' >>| String.of_char) <|>
     take_while1 (function '\'' | '"' | '/' | ',' | '&' | '~' | '%' | '_' -> true | _ -> false) <|>
     take_while1 is_alphanum)
    >>= fun name -> option None (char '{' *> many1 field_value_inner |> lift Option.some <* char '}')
    >>| fun arg  -> Field.Command (name, arg))
  <|>
  (take_while1 is_text >>| fun t -> Field.Text t)
  <|>
  (take_while1 is_sep >>| fun s -> Field.Sep s)
  <|>
  (char ',' >>| fun c -> Field.Text (String.of_char c)))

let field_value_elt = fix (fun field_value_elt ->
  (char '{' *> many field_value_inner <* char '}' >>| fun v -> Field.Bracketed v)
  <|>
  (char '"' *> many field_value_inner <* char '"' >>| fun v -> Field.Quoted v)
  <|>
  (take_while1 is_digit >>| fun v -> Field.Text v)
  <|>
  (identifier >>| fun v -> Field.Ident v)
  <|>
  (comment *> field_value_elt))

let field_value =
  skip_wsc *> field_value_elt >>= fun elt ->
  skip_wsc *> many (skip_wsc *> char '#' *> skip_wsc *> field_value_elt) >>| fun elts ->
  elt :: elts

let field = fix (fun field ->
  skip_wsc *>
  ((comment *> field) <|>
   lift2 (fun x y -> (x, y)) identifier (skip_wsc *> char '=' *> skip_wsc *> field_value)))

let fields =
  sep_by1 (char ',') field >>= fun fields ->
  skip_wsc *> option () (char ',' >>| ignore) >>| fun () ->
  Tag.Map.of_alist_exn fields

let entry =
  (char '@' *> skip_wsc *>
   ((string_ci "comment" >>| fun _ v -> Entry.Comment v) <|>
    (string_ci "preamble" >>| fun _ v -> Entry.Preamble v)) >>= fun f ->
   commit *> skip_wsc *> char '{' *> many field_value_inner <* char '}' >>| f)
  <|>
  (char '@' *> skip_wsc *>
   (string_ci "string" >>| fun _ (tag, value) -> Entry.String { tag; value }) >>= fun f ->
   commit *> skip_wsc *> char '{' *> field <* char '}' >>| f)
  <|>
  (char '@' *> skip_wsc *>
   take_while1 is_alpha >>= fun kind ->
   commit *> skip_wsc *> char '{' *> skip_wsc *> identifier >>= fun key ->
   skip_wsc *> char ',' *> skip_wsc *> fields <* skip_wsc <* char '}' >>| fun fields ->
   Entry.Entry { kind; key; fields })

let entries =
  many (skip_wsc *> entry)

let entry_of_string = parse_string entry
let entries_of_string = parse_string entries
