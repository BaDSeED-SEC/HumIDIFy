open LangBuiltinBase
open LangUtil
open LangUtil.Option

type state = {
  architecture : Elf.Header.machine;
  endianness   : Elf.Header.endian;
  symbols      : (S.t * S.t) Lazy.t;
}

(* Code duplication from LearningFeatures *)

let extract_symbols symtab strtab =
  Array.fold_left (fun acc se ->
    let pos = Int32.to_int se.ElfSymbolTable.st_name in
    match Elf.StringTable.extract_string_at ~pos strtab with
      | None -> acc
      | Some str -> S.add str acc)
    S.empty
    symtab

let load_table eh shs ~file ~name ~strings =
  Elf.Utilities.with_section_with_name eh shs ~name ~file ~f:begin fun symtab_sh ->
    Elf.Utilities.with_section_with_name eh shs ~name:strings ~file ~f:begin fun strtab_sh ->
      Elf.SymbolTable.read_from eh symtab_sh ~file >>= fun symtab ->
      Elf.StringTable.extract eh strtab_sh ~file >>| fun strtab ->
      extract_symbols symtab strtab
    end
  end

(* Explanation:
   .dynsym and .dynstr contain dynamic symbols and *should* generally be
   present. If present, .symtab and .strtab contain additional symbols;
   similarly, .rela.plt and .rela.dyn also contain dynamic symbols that
   refer to those symbols within .dynsym.

   Imported symbols are `Weak and have a st_value of 0 (i.e. UND); exported
   symbols are `Global.
 *)
let add_symbol ~table ~strings syms sym =
  let pos = Int32.to_int sym.ElfSymbolTable.st_name in
  match Elf.StringTable.extract_string_at strings ~pos with
    | None | Some "" -> syms
    | Some v -> print_endline v; S.add v syms

let symbols_from ~table ~strings syms =
  Array.fold_left (fun ((imports, exports) as acc) sym ->
    match snd sym.ElfSymbolTable.st_info with
      | `Weak when sym.ElfSymbolTable.st_value = 0L ->
          (add_symbol ~table ~strings imports sym, exports)
      | `Global  ->
          (imports, add_symbol ~table ~strings exports sym)
      | _ -> acc)
    syms
    table

let symbols_from_sections ~file ~table ~strings eh shs syms =
  Elf.Utilities.with_section_with_name ~file ~name:table eh shs ~f:(fun symtsh ->
  Elf.SymbolTable.read_from ~file eh symtsh >>= fun symt ->

  Elf.Utilities.with_section_with_name ~file ~name:strings eh shs ~f:(fun strtsh ->
  Elf.StringTable.extract ~file eh strtsh >>| fun strt ->

  symbols_from ~table:symt ~strings:strt syms))

let load_symbols ~file eh shs =
  let empty = (S.empty, S.empty) in
  let syms = Option.value ~default:empty @@
    symbols_from_sections ~file ~table:".dynsym" ~strings:".dynstr" eh shs empty
  in
  Option.value ~default:syms @@
    symbols_from_sections ~file ~table:".symtab" ~strings:".strtab" eh shs syms

(* Load state from ELF input channel *)
let load ic =
  Elf.Header.read_from ~file:ic >>= fun eh ->
  Elf.SectionHeader.read_from ~file:ic eh >>| fun shs -> {
    architecture = eh.ElfHeader.e_machine;
    endianness   = eh.ElfHeader.ei_data;
    symbols      = Lazy.from_fun (fun () -> load_symbols ~file:ic eh shs);
  }

(* Queries on state *)
let imports t ~name = BOOL (S.mem name @@ fst @@ Lazy.force t.symbols)
let exports t ~name = BOOL (S.mem name @@ snd @@ Lazy.force t.symbols)

let endianness t ~name = match String.lowercase name with
  | "le" | "little" | "little_endian" | "littleendian" ->
      BOOL (t.endianness = `LittleEndian)
  | "be" | "big" | "big_endian" | "bigendian" ->
      BOOL (t.endianness = `BigEndian)
  | _ ->
      BOTTOM

let architecture t ~name = match String.lowercase name with
  | "arm" -> BOOL (t.architecture = `ARM)
  | "arm64" | "aarch64" -> BOOL (t.architecture = `AArch64)
  | "mips" -> BOOL (t.architecture = `MIPS)
  | "ppc" | "powerpc" -> BOOL (t.architecture = `PowerPC)
  | "intel" -> BOOL (t.architecture = `x86 || t.architecture = `x86_64)
  | "x86" -> BOOL (t.architecture = `x86)
  | "amd64" | "x86-64" | "x86_64" | "x64" -> BOOL (t.architecture = `x86_64)
  | _ -> BOTTOM
