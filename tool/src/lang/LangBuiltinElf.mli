type state

val load         : in_channel -> state option

val architecture : state -> name:string -> LangBuiltinBase.t
val endianness   : state -> name:string -> LangBuiltinBase.t
val exports      : state -> name:string -> LangBuiltinBase.t
val imports      : state -> name:string -> LangBuiltinBase.t
