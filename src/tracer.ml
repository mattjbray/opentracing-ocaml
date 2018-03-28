let span_key : Span.t Lwt_mvar.t Lwt.key =
  Lwt.new_key ()

let start_span
    ?(references : Span.reference list = [])
    ?(start_ts : float option)
    ?(tags : Span.tag list = [])
    (operation_name : string)
    (f : unit -> 'b Lwt.t)
  =
  let open Span in
  let span_context =
    { trace_id =
        references
        |> CCList.head_opt
        |> CCOpt.map (fun r -> r.reference_context.trace_id)
        |> CCOpt.get_lazy (fun () -> Uuidm.(v `V4 |> to_string))
    ; span_id =
        Uuidm.(v `V4 |> to_string)
    }
  in
  let span =
    { operation_name
    ; start_ts =
        CCOpt.get_lazy Unix.gettimeofday start_ts
    ; finish_ts = None
    ; tags
    ; logs = []
    ; span_context
    ; references
    }
  in
  let span_mvar = Lwt_mvar.create span in
  Lwt.with_value span_key (Some span_mvar) f

let modify_span (f : Span.t -> Span.t) : unit Lwt.t =
  match Lwt.get span_key with
  | None -> Lwt.return_unit
  | Some span_mvar ->
    Lwt.Infix.(
      Lwt_mvar.take span_mvar >>= fun span ->
      Lwt_mvar.put span_mvar (f span)
    )
