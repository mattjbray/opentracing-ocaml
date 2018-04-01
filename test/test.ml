open Opentracing_lwt

let () =
  Lwt_main.run @@
  let tracer = Tracer.init
      (* Tracer.noop_tracer *)
      Opentracing_datadog.Tracer.tracer
  in
  let open Lwt.Infix in
  Tracer.trace tracer "hello" (fun () ->
      Lwt_unix.sleep 0.5 >>= fun () ->
      Tracer.trace tracer "world"
        (fun () -> Lwt_unix.sleep 0.1))
