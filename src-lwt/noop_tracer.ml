open Opentracing

module Span_context : Opentracing.Span.Span_context = struct
  type trace_id = Uuidm.t
  type span_id = Uuidm.t

  let pp_trace_id = Uuidm.pp
  let pp_span_id = Uuidm.pp

  let new_trace_id () = Uuidm.create `V4
  let new_span_id () = Uuidm.create `V4
end

include Tracer.Make(Span_context)

let noop_tracer : tracer =
  fun spans_stream ->
    let section = Lwt_log.Section.make "tracer.noop" in
    spans_stream
    |> Lwt_stream.iter_s Span.(fun span ->
        Lwt_log.notice ~section
          (CCFormat.to_string Span.pp span)
      )

let init = init noop_tracer
