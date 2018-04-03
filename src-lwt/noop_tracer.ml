open Opentracing

module Span_context : Opentracing.Span.Span_context = struct
  type trace_id = Uuidm.t
  type span_id = Uuidm.t

  let pp_trace_id = Uuidm.pp
  let pp_span_id = Uuidm.pp

  let new_trace_id () = Uuidm.create `V4
  let new_span_id () = Uuidm.create `V4
end

module Noop_implementation : Tracer.Implementation = struct
  module Span = Opentracing.Span.Make(Span_context)

  let span_receiver (spans_stream : Span.t Lwt_stream.t) : unit Lwt.t =
    let section = Lwt_log.Section.make "tracer.noop" in
    spans_stream
    |> Lwt_stream.iter_s Span.(fun span ->
        Lwt_log.notice ~section
          (CCFormat.to_string Span.pp span)
      )

  let inherit_tags = []
end

include Tracer.Make(Noop_implementation)
