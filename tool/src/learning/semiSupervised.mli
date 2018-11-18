type result = {
  model        : string;
  iterations   : int;
  unclassified : int
}

val learn : ?delta:float -> learner:(string -> string * string array * string) -> classifier:(string -> (int * string * float) list) -> string -> result
