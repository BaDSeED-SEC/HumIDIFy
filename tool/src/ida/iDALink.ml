open Core_kernel.Std
open Bap.Std
open Option

let load_py = sprintf "
from idaapi import *
from idautils import *
import re

## !!! BEGIN TAKEN FROM PYTHON 2.7.9 JSON LIBRARY !!!

ESCAPE = re.compile(r'[\\x00-\\x1f\\\\\"\\b\\f\\n\\r\\t]')
ESCAPE_DCT = {
    '\\\\': '\\\\\\\\',
    '\\\"': '\\\\\\\"',
    '\\b': '\\\\b',
    '\\f': '\\\\f',
    '\\n': '\\\\n',
    '\\r': '\\\\r',
    '\\t': '\\\\t',
}
for i in range(0x20):
    ESCAPE_DCT.setdefault(chr(i), '\\\\x{0:02x}'.format(i))

def esc(s):
    def replace(match):
        return ESCAPE_DCT[match.group(0)]
    return '\"' + ESCAPE.sub(replace, str(s)) + '\"'

## !!!   END TAKEN FROM PYTHON 2.7.9 JSON LIBRARY !!!


def demangle(name):
    dm_name = Demangle(name, GetLongPrm(INF_SHORT_DN))

    if dm_name == None:
        return name
    else:
        return dm_name


autoWait()


strrefs = dict()
strs = Strings()

for s in strs:
    try:
      for ref in XrefsTo(s.ea, False):
        name = esc(demangle(GetFunctionName(ref.frm)))
        if name:
            if name not in strrefs:
                strrefs[name] = set([esc(str(s))])
            else:
                strrefs[name].add(esc(str(s)))
    except StopIteration:
      pass

output = '('

for entry_address in Segments():
    for fn_entry_address in Functions(SegStart(entry_address), SegEnd(entry_address)):
        fn_name = esc(demangle(GetFunctionName(fn_entry_address)))

        fn = get_func(fn_entry_address)

        output += '({:s} 0x{:x} 0x{:x} ('.format(fn_name, fn.startEA, fn.endEA)

        for fn_block in FlowChart(fn):
          succs = []

          for block_succ in fn_block.succs():
              succs.append('0x{:x}'.format(block_succ.startEA))

          output += '(0x{:x} 0x{:x} ({:s}))'.format(fn_block.startEA, fn_block.endEA, ' '.join(succs))

        output += ')'

        if fn_name in strrefs:
            output += ' ({:s}))'.format(' '.join(strrefs[fn_name]))
        else:
            output += ' ())'

output += ')'


with open('%s', 'w+') as f:
    f.write(output)
Exit(0)
" 


module rec Function : sig
 
  type func = {
    fn_name    : string;
    fn_entry   : addr;
    fn_exit    : addr;

    fn_strings : string list;
    fn_memory  : mem;

    fn_blocks  : Block.block Addr.Table.t;
  }

  include Opaque
    with type t := func

  val create  : image -> string -> addr -> addr -> (addr * addr * addr list) list -> string list -> func option
  val bound   : func -> (addr -> bool)
  val blocks  : func -> Block.block Addr.Table.t
  val memory  : func -> mem
  val name    : func -> string
  val entry   : func -> addr
  val strings : func -> string list
 
end = struct

  type func = {
    fn_name    : string;
    fn_entry   : addr;
    fn_exit    : addr;

    fn_strings : string list;
    fn_memory  : mem;

    fn_blocks  : Block.block Addr.Table.t;
  }

  let create image fn_name fn_entry fn_exit blocks strs =
    Table.find_addr (Image.segments image) fn_entry >>| fun (mem, _) ->
    let fns = Addr.Table.create () in
    List.iter blocks ~f:(fun (start_addr, end_addr, dests) ->
      match Block.create image mem fns start_addr end_addr dests with
        | None -> ()
        | Some block -> Addr.Table.add_exn fns ~key:start_addr ~data:block);
    {
      fn_name = fn_name;
      fn_entry = fn_entry;
      fn_exit = fn_exit;
      fn_memory = mem;
      fn_blocks = fns;
      fn_strings = strs;
    }

  let name { fn_name } = fn_name
  let entry { fn_entry } = fn_entry

  let bound t addr =
    Addr.between ~low:t.fn_entry ~high:t.fn_exit addr

  let blocks { fn_blocks } = fn_blocks

  let memory { fn_memory } = fn_memory
  let strings { fn_strings } = fn_strings

  include Opaque.Make (struct
    type t = func

    let hash { fn_entry } = Addr.hash fn_entry
    let compare fn1 fn2 = Addr.compare fn1.fn_entry fn2.fn_entry
  end)

end and Block : sig

  type block = {
    start_addr : addr;
    end_addr   : addr;

    _fn_blocks : block Addr.Table.t;

    memory     : mem;
    dests      : addr list;
    insns      : (mem * insn option) list lazy_t;
  }

  and dest = [ `Cond of addr * block | `Fall of addr * block | `Call of addr ]

  include Opaque
    with type t := block

  type t = block

  val create       : image -> mem -> block Addr.Table.t -> addr -> addr -> addr list -> block option

  val local_dests  : block -> dest list
  val calls        : image:image -> bound:(addr -> bool) -> block -> addr list
  val dests        : image:image -> bound:(addr -> bool) -> block -> dest list
  val insns        : block -> (mem * insn option) list
  val addr         : block -> addr

  val dependent_call : image:image -> bound:(addr -> bool) -> ftab:[ `Fixed of var list Addr.Table.t | `Heuristic of var list ] -> block -> (addr * (mem * exp) Var.Map.t) option

end = struct

  type block = {
    start_addr : addr;
    end_addr   : addr;

    _fn_blocks : block Addr.Table.t;

    memory     : mem;
    dests      : addr list;
    insns      : (mem * insn option) list lazy_t;
  }

  and dest = [ `Cond of addr * block | `Fall of addr * block | `Call of addr ]

  type t = block

  let create image mem fns start_addr end_addr dests =
    let word_size = (Image.addr_size image :> Size.t) in
    let word_bytes = Int64.of_int (Size.to_bytes word_size) in

    let words =
      Int64.(to_int_exn @@
              ((Or_error.ok_exn @@ Addr.to_int64 end_addr) -
               (Or_error.ok_exn @@ Addr.to_int64 start_addr)) / word_bytes)
    in

    let _fn_blocks = fns in

    Option.try_with (fun () ->
      Or_error.ok_exn @@
      Memory.view ~word_size
                  ~from:start_addr
                  ~words
                  mem)

    >>| fun memory ->
    (* FIXME: IDA's basic blocks are not equivalent to BAP's *)
    let insns = lazy (linear_sweep_exn (Image.arch image) memory) in  
    {
      start_addr;
      end_addr;
      _fn_blocks;
      memory;
      dests;
      insns;
    }

  let addr { start_addr } = start_addr

  let insns { insns } = Lazy.force insns

  let local_dests { _fn_blocks; dests } =
    let len = List.length dests in
    List.filter_map dests ~f:(fun addr ->
      match Hashtbl.find _fn_blocks addr with
        | None -> None
        | Some block -> Some (if len > 1 then `Cond (addr, block) else `Fall (addr, block)))

  let calls ~image ~bound t =
    let memory = Image.memory image in
    let insns = List.filter_map ~f:(function (mem, Some insn) -> Some (mem, insn) | _ -> None) (insns t) in
    IDALinkArch.Arm.resolve_calls ~memory ~bound insns

  let dests ~image ~bound t =
    List.map ~f:(fun addr -> `Call addr) (calls ~image ~bound t) @ local_dests t

  let dependent_call ~image ~bound ~ftab t =
    let memory = Image.memory image in
    let insns = List.filter_map ~f:(function (mem, Some insn) -> Some (mem, insn) | _ -> None) (insns t) in
    IDALinkArch.Arm.find_dependent_call ~memory ~bound ~ftab insns

  include Opaque.Make (struct
    type t = block

    let hash { start_addr } = Addr.hash start_addr
    let compare b1 b2 = Addr.compare b1.start_addr b2.start_addr
  end)

end

let load ?(path = "idal") image input =
  IDA.create ~path >>= fun t ->
  let output = Filename.temp_file "ida_" ".sexp" in
  if IDA.run ~script_type:`Python ~remove_database:true ~t ~script:(load_py output) input then
    In_channel.with_file output ~f:(fun ic ->
      try
        let data = Sexp.input_sexp ic in
        let width = Size.to_bits (Image.addr_size image :> Size.t) in

        let parse_addr addr = Addr.of_int64 ~width (Int64.of_string addr) in

        let parse_string = function Sexp.Atom str -> str | _ -> assert false in

        let parse_dests =
          List.map ~f:(function Sexp.Atom addr -> parse_addr addr | _ -> assert false)
        in

        let rec parse_block = function
          | Sexp.List [Sexp.Atom start_addr; Sexp.Atom end_addr; Sexp.List dests] ->
              let start_addr = parse_addr start_addr in
              let end_addr = parse_addr end_addr in
              let dests = parse_dests dests in
              (start_addr, end_addr, dests)
          | _ -> assert false
        in

        let rec parse_function = function
          | Sexp.List [Sexp.Atom name; Sexp.Atom start_addr; Sexp.Atom end_addr; Sexp.List blocks; Sexp.List strs] ->
              let start_addr = parse_addr start_addr in
              let end_addr = parse_addr end_addr in
              let blocks = List.map ~f:parse_block blocks in
              let strs = List.map ~f:parse_string strs in
              Function.create image name start_addr end_addr blocks strs
          | _ -> assert false
        in

        match data with
          | Sexp.List functions ->
              Some (List.fold ~init:Seq.empty ~f:(fun acc func ->
                match parse_function func with
                  | None -> acc
                  | Some fn -> Seq.cons fn acc) functions)
          | _ -> None
      with
        | _ -> None)
  else
    None

let load_syms ?(path = "idal") input =
  IDA.create ~path >>= fun t ->
  let output = Filename.temp_file "ida_" ".sexp" in
  if IDA.run ~script_type:`Python ~remove_database:true ~t ~script:(load_py output) input then
    In_channel.with_file output ~f:(fun ic ->
      try
        let data = Sexp.input_sexp ic in
        let rec parse_function = function
          | Sexp.List [Sexp.Atom name; Sexp.Atom start_addr; Sexp.Atom end_addr; Sexp.List blocks; Sexp.List strs] ->
              Some name
          | _ -> assert false
        in

        match data with
          | Sexp.List functions ->
              Some (List.fold ~init:Seq.empty ~f:(fun acc func ->
                match parse_function func with
                  | None -> acc
                  | Some fn -> Seq.cons fn acc) functions)
          | _ -> None
      with
        | _ -> None)
  else
    None
