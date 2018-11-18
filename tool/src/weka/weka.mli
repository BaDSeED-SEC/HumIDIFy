module J48 : sig

  val classify : class_path:string -> test_file:string -> model:string -> (int * string * float) list
  val learn    : class_path:string -> test_file:string -> model:string -> unit

end

val infer_class_path : unit -> string
