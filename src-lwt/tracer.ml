open Opentracing

module type Implementation = sig
  module Span : Span.Span

  val span_receiver : Span.t Lwt_stream.t -> unit Lwt.t

  (** Tags that child spans will inherit from the parent span. *)
  val inherit_tags : string list
end

module Make(Impl : Implementation) = struct
  module Span = Impl.Span

  (* Thread-local variable holding the current span. *)
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
          |> CCOpt.get_lazy Span.Context.new_trace_id
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

  let spans_stream, push_span = Lwt_stream.create ()

  let init : unit =
    let () = Lwt.async (fun () -> Impl.span_receiver spans_stream) in
    ()

  let trace (operation_name : string)
      ?(tags : Tags.t = Tags.empty)
      (f : unit -> 'a Lwt.t) =
    let open Lwt.Infix in
    let references_tags =
      match Lwt.get span_key with
      | None -> Lwt.return ([], Tags.empty)
      | Some parent_span_mvar ->
        Lwt_mvar.take parent_span_mvar >>= fun parent_span ->
        Lwt_mvar.put parent_span_mvar parent_span >|= fun () ->
        ( Span.[{ reference_type = Child_of
                ; reference_context = parent_span.span_context
                }]
        , parent_span.tags
          |> Tags.filter (fun key _ -> List.mem key Impl.inherit_tags)
        )
    in
    references_tags >>= fun (references, parent_tags) ->
    let tags =
      Tags.StringMap.union
        (fun key parent_tag tag -> Some tag)
        parent_tags
        tags
    in
    let span = start_span operation_name ~references ~tags in
    let span_mvar = Lwt_mvar.create span in
    Lwt.finalize
      (fun () -> Lwt.with_value span_key (Some span_mvar) f)
      (fun () ->
         Lwt_mvar.take span_mvar >>= fun span ->
         let span = Span.finish span in
         push_span (Some span)
         |> Lwt.return
      )

end
