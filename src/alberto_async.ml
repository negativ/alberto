open Async.Std

module BE = EndianString.BigEndian

exception Scheduler_not_running

let (>>@) d f = 
  match Deferred.peek d with
  | Some x -> f x
  | None -> raise Scheduler_not_running

module Make(A: (module type of Alberto)) = struct
   type t = A.t Deferred.t

   let create term = 
     return term

   let encode term = 
     term >>| A.encode

   let decode str = 
     str >>= (fun s -> match A.decode s with
                       | None -> return None
                       | Some x -> return (Some (create x)))

   let encode_exn term =
     term >>| A.encode_exn

   let decode_exn str =
     str >>= (fun str -> create (A.decode_exn str))

   let to_string term =
     term >>| A.to_string

   let read_term r =
     let len =
       let buf = String.create 4 in
       Reader.really_read r ~pos:0 ~len:4 buf
       >>| (function | `Eof _ -> raise End_of_file
                     | `Ok -> BE.get_int32 buf 0 
                              |> Int32.to_int)
       >>@ (fun len -> len) in
     let buf = String.create len in
     Reader.really_read r ~pos:0 ~len:len buf
     >>= (function | `Eof _ -> raise End_of_file
                   | `Ok -> decode_exn (return buf))

   let write_term w term =
     encode_exn term 
     >>| (fun str -> 
              let hdr = 
                  let buf = Bytes.create 4 in
                  let () = BE.set_int32 buf 0 (Int32.of_int (String.length str)) in Bytes.to_string buf in
              Writer.write w hdr;
              encode_exn term >>> (fun str -> Writer.write w str);
              Writer.flushed w >>> fun () -> ())

end



















