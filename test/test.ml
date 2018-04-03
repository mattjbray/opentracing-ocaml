(* module T = Opentracing_lwt.Noop_tracer *)
module T = Opentracing_datadog.Tracer

let tags = Opentracing.Tags.of_list
    [ ( Opentracing_datadog.Tags.span_type, `String "web" )
    ; ( Opentracing_datadog.Tags.service_name, `String "opentracing-ocaml" )
    ; ( Opentracing_datadog.Tags.resource_name, `String "example_tracer" )
    ]

let () =
  Lwt_main.run @@
  let () = T.init in
  let open Lwt.Infix in
  T.trace "hello" ~tags
    (fun () ->
      Lwt_unix.sleep (0.1 +. Random.float 0.5) >>= fun () ->
      T.trace "world" ~tags:(Opentracing.Tags.of_list [ ("my.metric", `Int (Random.int 10))])
        (fun () -> Lwt_unix.sleep (0.1 +. Random.float 0.1)))
    >>= fun () -> Lwt_unix.sleep 1.0
