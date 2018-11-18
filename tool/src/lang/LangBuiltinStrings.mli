type state

val load     : in_channel -> state option

val contains : state -> name:string -> LangBuiltinBase.t
