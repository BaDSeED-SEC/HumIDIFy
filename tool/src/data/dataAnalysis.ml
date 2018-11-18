open Core_kernel.Std
module E = Elf
open Bap.Std
open Option


type t = {
  loc_section_name   : string option;
  loc_section_header : E.SectionHeader.t option;
  loc_type           : [ `Global | `GlobalRO | `Other | `Stack ];
  loc_addr           : int64;
  loc_offset         : int64 option;
}


let maybe_pps f = function
  | None -> "?"
  | Some v -> f v

let pps_type = function
  | `Global -> "GLOBAL"
  | `GlobalRO -> "GLOBAL-RO"
  | `Other -> "UNKNOWN"
  | `Stack -> "STACK"

let pps_section_name s = s

let pps t =
  sprintf "0x%LX: %s %s"
    t.loc_addr
    (maybe_pps pps_section_name t.loc_section_name)
    (pps_type t.loc_type)


let resolve_file_offset sh addr =
  (* We assume that [addr] is contained within the section defined by [sh] *)
  let open Int64 in
  sh.ElfSectionHeader.sh_offset + (addr - sh.ElfSectionHeader.sh_addr)


let default_section_map = function
  | _ -> `Other


let elf_section_map sh t =
  let module SH = E.SectionHeader in match value_exn t.loc_section_name with
    | ".bss" when sh.SH.sh_type = `NoBits && SH.is_alloc sh && SH.is_write sh ->
        Some { t with
          loc_type = `Global
        }
    | ".data" when sh.SH.sh_type = `ProgBits && SH.is_alloc sh && SH.is_write sh ->
        Some { t with
          loc_type = `Global;
          loc_offset = Some (resolve_file_offset sh t.loc_addr)
        }
    | ".text" when sh.SH.sh_type = `ProgBits && SH.is_alloc sh && SH.is_exec_instr sh ->
        Some { t with
          loc_type = `Global;
          loc_offset = Some (resolve_file_offset sh t.loc_addr)
        }
    | ".rodata" when sh.SH.sh_type = `ProgBits && SH.is_alloc sh && not (SH.is_write sh) ->
        Some { t with
          loc_type = `GlobalRO;
          loc_offset = Some (resolve_file_offset sh t.loc_addr)
        }
    | _ -> None


let resolve_by_section_attrs sh t =
  let module SH = E.SectionHeader in
  
  if sh.SH.sh_type = `ProgBits then
    { t with
      loc_type = if not (SH.is_write sh) then `GlobalRO else `Global;
      loc_offset = Some (resolve_file_offset sh t.loc_addr);
    }
  else
    t


let resolve_by_section_name ?(section_map = elf_section_map) shstrs sh addr =
  let t = {
    loc_section_name = None;
    loc_section_header = Some sh;
    loc_addr = addr;
    loc_type = `Other;
    loc_offset = None;
  } in

  let pos = Int.of_int32_exn sh.ElfSectionHeader.sh_name in
  match E.StringTable.extract_string_at ~pos shstrs with
    | Some name ->
        let t' = { t with loc_section_name = Some name } in
        begin match section_map sh t' with
          | None -> resolve_by_section_attrs sh t'
          | Some v -> v
        end
    | None -> resolve_by_section_attrs sh t


let is_data { loc_type } =
  loc_type = `Global || loc_type = `GlobalRO


module Make (Target : Target) = struct

  module CPU = Target.CPU

  let of_exp syms shs addr =
    match addr with
      | Bil.Int i ->
          let addr' = Or_error.ok_exn @@ Word.to_int64 i in

          (* We want to obtain the section, find it's name and properties *)
          E.Utilities.with_section shs ~f:(fun sh -> Some (resolve_by_section_name syms sh addr')) ~matching:(fun sh ->
            let saddr = sh.ElfSectionHeader.sh_addr in
            let eaddr = Int64.(saddr + sh.ElfSectionHeader.sh_size) in

            E.SectionHeader.is_alloc sh && saddr <= addr' && addr' < eaddr)

      (* TODO: Handle the case of a stack allocated variable *)
      | _ -> None

end

module Extraction : module type of DataExtraction = DataExtraction
