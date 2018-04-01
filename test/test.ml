open Opentracing_lwt

let () =
  Lwt_main.run @@
  let spans_stream, push_span = Lwt_stream.create () in
  let () = Lwt.async (fun () ->
      Tracer.noop_tracer spans_stream
    )
  in
  let tracer = Tracer.{ push_span } in
  let open Lwt.Infix in
  Tracer.trace tracer "hello"
    (fun () ->
       Lwt_unix.sleep 0.5 >>= fun () ->
       Tracer.trace tracer "world"
         (fun () -> Lwt_unix.sleep 0.1)
    )
