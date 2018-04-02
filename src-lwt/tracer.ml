open Opentracing

module Make(Span_context : Span.Span_context) = struct
  module Span = Span.Make(Span_context)

  type t =
    { push_span : Span.t option -> unit }

  let span_key : Span.t Lwt_mvar.t Lwt.key =
    Lwt.new_key ()

  let start_span
      ?(references : Span.reference list = [])
      ?(start_ts : float option)
      ?(tags : Tags.t = Tags.empty)
      (operation_name : string)
    : Span.t
    =
    let open Span in
    let span_context =
      Span.Context.{
        trace_id =
          references
          |> CCList.head_opt
          |> CCOpt.map (fun r -> r.reference_context.trace_id)
          |> CCOpt.get_lazy Span_context.new_trace_id
      ; span_id =
          Span.Context.new_span_id ()
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
    span

  let modify_span (f : Span.t -> Span.t) : unit Lwt.t =
    match Lwt.get span_key with
    | None -> Lwt.return_unit
    | Some span_mvar ->
      Lwt.Infix.(
        Lwt_mvar.take span_mvar >>= fun span ->
        Lwt_mvar.put span_mvar (f span)
      )

  type tracer = Span.t Lwt_stream.t -> unit Lwt.t

  let init (tracer : tracer) : t =
    let spans_stream, push_span = Lwt_stream.create () in
    let () = Lwt.async (fun () -> tracer spans_stream) in
    { push_span }

  let trace (t : t) (operation_name : string)
      ?(tags : Tags.t = Tags.empty)
      (f : unit -> 'a Lwt.t) =
    let open Lwt.Infix in
    let references =
      match Lwt.get span_key with
      | None -> Lwt.return []
      | Some parent_span_mvar ->
        Lwt_mvar.take parent_span_mvar >>= fun parent_span ->
        Lwt_mvar.put parent_span_mvar parent_span >|= fun () ->
        [ Span.{ reference_type = Child_of
               ; reference_context = parent_span.span_context
               }
        ]
    in
    references >>= fun references ->
    let span = start_span operation_name ~references ~tags in
    let span_mvar = Lwt_mvar.create span in
    Lwt.finalize
      (fun () -> Lwt.with_value span_key (Some span_mvar) f)
      (fun () ->
         Lwt_mvar.take span_mvar >>= fun span ->
         let span = Span.finish span in
         t.push_span (Some span)
         |> Lwt.return
      )

end
