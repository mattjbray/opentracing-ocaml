module Make(T : Opentracing_lwt.Tracer.S) : module type of Cohttp_lwt_unix.Client = struct
  module C = Cohttp_lwt_unix.Client

  type ctx = C.ctx
  let sexp_of_ctx = C.sexp_of_ctx
  let default_ctx = C.default_ctx
  let custom_ctx = C.custom_ctx

  let trace_call meth uri get_response call =
    T.trace "cohttp-client"
      ~tags:Opentracing.(
          Tags.of_list
            [ ( Tags.Http.url, `String (Uri.to_string uri) )
            ; ( Tags.Http.meth, `String (Cohttp.Code.string_of_method meth) )
            ])
      (fun () ->
         let open Lwt.Infix in
         call >>= fun result ->
         T.modify_span
           (T.Span.set_tag
              ~key:Opentracing.Tags.Http.status_code
              ~value:(`String
                        (result
                         |> get_response
                         |> Cohttp.Response.status
                         |> Cohttp.Code.code_of_status
                         |> string_of_int
                        )))
         >>= fun () ->
         Lwt.return result
      )


  let call ?ctx ?headers ?body ?chunked meth uri =
    trace_call meth uri fst (C.call ?ctx ?headers ?body ?chunked meth uri)

  let head ?ctx ?headers uri =
    trace_call `HEAD uri CCFun.id (C.head ?ctx ?headers uri)

  let get ?ctx ?headers uri =
    trace_call `GET uri fst (C.get ?ctx ?headers uri)

  let delete ?ctx ?body ?chunked ?headers uri =
    trace_call `DELETE uri fst (C.delete ?ctx ?body ?chunked ?headers uri)

  let post ?ctx ?body ?chunked ?headers uri =
    trace_call `POST uri fst (C.post ?ctx ?body ?chunked ?headers uri)

  let put ?ctx ?body ?chunked ?headers uri =
    trace_call `PUT uri fst (C.put ?ctx ?body ?chunked ?headers uri)

  let patch ?ctx ?body ?chunked ?headers uri =
    trace_call `PATCH uri fst (C.patch ?ctx ?body ?chunked ?headers uri)

  let post_form ?ctx ?headers ~params uri =
    trace_call `POST uri fst (C.post_form ?ctx ?headers ~params uri)

  (* TODO *)
  let callv = C.callv
end
