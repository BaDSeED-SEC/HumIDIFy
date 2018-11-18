open LangBuiltinBase
open LangUtil
open LangUtil.Option

type state = {
  strings : S.t option Lazy.t;
}

(* Load state from ELF input channel *)
let load ic = Option.return @@ {
  strings = Lazy.from_fun (fun () ->
    Strings.of_channel ~specialise:true ic >>| S.of_list);
}

(* Queries on state *)
let contains t ~name = match Lazy.force t.strings with
  | None -> BOTTOM
  | Some strings -> BOOL (S.mem name strings)
