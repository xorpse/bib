open Core_kernel

module Tag : sig
  type t = string
    [@@deriving compare, sexp]

  include Comparable.S with type t := t
end

module Field : sig
  type t = Ident of string
         | Bracketed of t list
         | Quoted of t list
         | Command of string * t list option
         | Text of string
         | Sep of string
         [@@deriving compare, sexp]

  include Comparable.S with type t := t

  val is_text           : t -> bool
  val is_simple_command : t -> bool
  val is_word           : t -> bool
  val is_lower          : t -> bool

  val first_lower_pos : t list -> int option
  val last_lower_pos  : t list -> int option

  val first_word_pos  : t list -> int option
  val last_word_pos   : t list -> int option

  val word_count      : t -> int

  val inner_string    : t -> string option
  val to_string       : t list Tag.Map.t -> t -> string

  val fold_strings    : t list Tag.Map.t -> t -> t

  val list_to_string    : t list Tag.Map.t -> t list -> string
  val list_fold_strings : t list Tag.Map.t -> t list -> t list
end

module Entry : sig
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

   include Comparable.S with type t := t

   val list_fold_strings : Field.t list Tag.Map.t -> t list -> t list
end
