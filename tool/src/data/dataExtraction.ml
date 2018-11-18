open Core_kernel.Std
open Bap.Std
open Option

type data = [ `String of string
            | `Binary of string
            | `Int8 of int
            | `Int16 of int
            | `Int32 of int32
            | `Int64 of int64 ]

type selector = [ `S_String of [ `ASCII_Z ]
                | `S_Binary of int64 (* maximum length *)
                | `S_Int8
                | `S_Int16
                | `S_Int32
                | `S_Int64 ]

type source = [ `File of in_channel | `String of string ]


let default_selector = `S_Binary 1024L


let extract ?(with_type = default_selector) ~from ~at =
  let data = match from with
    | `File ic -> In_channel.seek ic at; Stream.of_channel ic
    | `String data -> String.suffix data (Int.of_int64_exn at) |> Stream.of_string
  in

  match with_type with
    | `S_Binary len ->
        let len = Int.of_int64_exn len in
        let buf = Buffer.create len in
        let rec aux n =
          if n < len then
            match Stream.peek data with
              | None -> `Binary (Buffer.to_bytes buf |> Bytes.unsafe_to_string)
              | Some v -> Stream.junk data; Buffer.add_char buf v; aux (succ n)
          else
            `Binary (Buffer.to_bytes buf |> Bytes.unsafe_to_string)
        in Some (aux 0)

    | `S_String string_type ->
        (match string_type with
          | `ASCII_Z ->
              (* Assume we are looking for printable characters that end with a terminating NULL *)
              let buf = Buffer.create 80 in
              let rec aux () =
                Stream.peek data >>= fun c ->
                  if Char.is_print c || Char.is_whitespace c then (
                    Stream.junk data; Buffer.add_char buf c; aux ()
                  ) else if c = '\x00' then
                    Some (`String (Buffer.to_bytes buf |> Bytes.unsafe_to_string))
                  else
                    None
              in aux ())

    | `S_Int8 ->
        Stream.peek data >>| fun c -> `Int8 (Char.to_int c)

    | `S_Int16 ->
        Stream.peek data >>= fun a ->
        Stream.junk data; Stream.peek data >>| fun b ->

        let i0 = Char.to_int a in
        let i1 = Char.to_int b in

        `Int16 (if Sys.big_endian then
          (i0 lsl 8) lor i1
        else
          (i1 lsl 8) lor i0)

    | `S_Int32 ->
        Stream.peek data >>= fun a ->
        Stream.junk data; Stream.peek data >>= fun b ->
        Stream.junk data; Stream.peek data >>= fun c ->
        Stream.junk data; Stream.peek data >>| fun d ->

        let i0 = Int32.of_int_exn @@ Char.to_int a in
        let i1 = Int32.of_int_exn @@ Char.to_int b in
        let i2 = Int32.of_int_exn @@ Char.to_int c in
        let i3 = Int32.of_int_exn @@ Char.to_int d in

        let (lsl) = Int32.shift_left in
        let (lor) = Int32.bit_or in

        `Int32 (if Sys.big_endian then
          (i0 lsl 24) lor (i1 lsl 16) lor (i2 lsl 8) lor i3
        else
          (i3 lsl 24) lor (i2 lsl 16) lor (i1 lsl 8) lor i0)

    | `S_Int64 ->
        Stream.peek data >>= fun a ->
        Stream.junk data; Stream.peek data >>= fun b ->
        Stream.junk data; Stream.peek data >>= fun c ->
        Stream.junk data; Stream.peek data >>= fun d ->
        Stream.junk data; Stream.peek data >>= fun e ->
        Stream.junk data; Stream.peek data >>= fun f ->
        Stream.junk data; Stream.peek data >>= fun g ->
        Stream.junk data; Stream.peek data >>| fun h ->

        let i0 = Int64.of_int_exn @@ Char.to_int a in
        let i1 = Int64.of_int_exn @@ Char.to_int b in
        let i2 = Int64.of_int_exn @@ Char.to_int c in
        let i3 = Int64.of_int_exn @@ Char.to_int d in
        let i4 = Int64.of_int_exn @@ Char.to_int e in
        let i5 = Int64.of_int_exn @@ Char.to_int f in
        let i6 = Int64.of_int_exn @@ Char.to_int g in
        let i7 = Int64.of_int_exn @@ Char.to_int h in

        let (lsl) = Int64.shift_left in
        let (lor) = Int64.bit_or in

        `Int64 (if Sys.big_endian then
          (i0 lsl 56) lor (i1 lsl 48) lor (i2 lsl 40) lor (i3 lsl 32) lor
          (i4 lsl 24) lor (i5 lsl 16) lor (i6 lsl 8) lor i7
        else
          (i7 lsl 56) lor (i6 lsl 48) lor (i5 lsl 40) lor (i4 lsl 32) lor
          (i3 lsl 24) lor (i2 lsl 16) lor (i1 lsl 8) lor i0)
