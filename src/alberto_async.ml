open Async.Std

module BE = EndianString.BigEndian

exception Scheduler_not_running

let (>>@) d f = 
  match Deferred.peek d with
  | Some x -> f x
  | None -> raise Scheduler_not_running

let forward x = x

let take_int r =
  let buf = String.create 4 in
  Reader.really_read r ~pos:0 ~len:4 buf
  >>| (function | `Eof _ ->  raise End_of_file
                | `Ok -> BE.get_int32 buf 0 |> Int32.to_int)

let put_int w i =
  let hdr =
    let buf = Bytes.create 4 in
    let () = BE.set_int32 buf 0 @@ Int32.of_int i in Bytes.to_string buf in
    return @@ Writer.write w hdr;

module Make(A: (module type of Alberto)) = struct
   type t = A.t Deferred.t

   let create term = 
     return term

   let encode term = 
     term >>| A.encode

   let decode str = 
     match A.decode str with
     | None -> return None
     | Some x -> return @@ Some (create x)

   let encode_exn term =
     term >>| A.encode_exn

   let decode_exn str =
     create (A.decode_exn str)

   let to_string term =
     term >>| A.to_string

   let read_term r =
     take_int r
     >>| (fun len -> String.create len)
     >>= (fun buf -> Reader.really_read r ~pos:0 ~len:(String.length buf) buf
                     >>= (function | `Eof _ -> raise End_of_file
                                   | `Ok -> decode_exn buf))

   let write_term w term =
     encode_exn term 
     >>| (fun str -> 
              String.length str |> put_int w >>> forward;
              encode_exn term >>> (fun str -> Writer.write w str);
              Writer.flushed w >>> forward)

   let interact (f: A.t -> t) =
     let r = Lazy.force Reader.stdin and
         w = Lazy.force Writer.stdout in
     let rec loop () =
       read_term r 
       >>= (function | `Atom "stop" -> exit 0
                     | `Atom "ping" -> create (`Atom "pong")
                     | term -> f term)
       >>= (fun x -> write_term w (create x))
       >>= loop
     in begin
       try_with (fun () -> loop ()) >>= (fun _ -> exit 0)
     end
end










