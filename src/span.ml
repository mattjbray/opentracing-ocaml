module type Span_context = sig
  type trace_id
  type span_id

  val new_trace_id : unit -> trace_id
  val new_span_id : unit -> span_id

  val pp_trace_id : trace_id CCFormat.printer
  val pp_span_id : span_id CCFormat.printer
end

module type Span = sig
  type timestamp = float

  module Context : sig
    include Span_context
    type t =
      { trace_id : trace_id
      ; span_id : span_id
      }
  end

  type span_log =
    { log_ts : timestamp
    ; log_tags : Tags.t
    }

  type reference_type =
    | Child_of
    | Follows_from

  type reference =
    { reference_type : reference_type
    ; reference_context : Context.t
    }

  type t =
    { operation_name : string
    ; start_ts : timestamp
    ; finish_ts : timestamp option
    ; tags : Tags.t
    ; logs : span_log list
    ; span_context : Context.t
    ; references : reference list
    }

  val finish : ?finish_ts:timestamp -> t -> t
  val pp : t CCFormat.printer
end

module Make(M : Span_context) : Span
  with type Context.trace_id = M.trace_id
  with type Context.span_id = M.span_id
= struct
  type timestamp = float

  type span_log =
    { log_ts : timestamp
    ; log_tags : Tags.t
    }

  module Context = struct
    include M

    type t =
      { trace_id : trace_id
      ; span_id : span_id
      }
  end

  type reference_type =
    | Child_of
    | Follows_from

  type reference =
    { reference_type : reference_type
    ; reference_context : Context.t
    }

  type t =
    { operation_name : string
    ; start_ts : timestamp
    ; finish_ts : timestamp option
    ; tags : Tags.t
    ; logs : span_log list
    ; span_context : Context.t
    ; references : reference list
    }

  let get_span_context (span : t) : Context.t =
    span.span_context

  let set_operation_name (operation_name : string) (span : t) : t =
    { span with operation_name }

  let finish ?(finish_ts : timestamp option) (span : t) : t =
    { span with
      finish_ts =
        begin match span.finish_ts with
          | Some ts -> Some ts
          | None -> Some (CCOpt.get_lazy Unix.gettimeofday finish_ts)
        end
    }

  let set_tag ~(key : string) ~(value : Tags.value) (span : t) : t =
    { span with
      tags = Tags.add key value span.tags
    }

  let log ~(tags : Tags.t) ?(log_ts : timestamp option) (span : t) : t =
    { span with
      logs =
        { log_ts = CCOpt.get_lazy Unix.gettimeofday log_ts
        ; log_tags = tags
        }
        :: span.logs
    }

  let pp : t CCFormat.printer =
    fun fmt t ->
      CCFormat.fprintf fmt
        "[Trace=%a Span=%a Op=%S Ts=%fs]"
        Context.pp_trace_id t.span_context.trace_id
        Context.pp_span_id t.span_context.span_id
        t.operation_name
        ((t.finish_ts |> CCOpt.get_or ~default:0.0) -. t.start_ts)
end
