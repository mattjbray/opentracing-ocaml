type tag_value =
  [ `String of string
  | `Bool of bool
  | `Int of int
  | `Float of float
  ]

type tag =
  { key : string
  ; value : tag_value
  }

type timestamp = float

type span_log =
  { log_ts : timestamp
  ; log_tags : tag list
  }

type span_context =
  { trace_id : string
  ; span_id : string
  }

type reference_type =
  | Child_of
  | Follows_from

type reference =
  { reference_type : reference_type
  ; reference_context : span_context
  }

type t =
  { operation_name : string
  ; start_ts : timestamp
  ; finish_ts : timestamp option
  ; tags : tag list
  ; logs : span_log list
  ; span_context : span_context
  ; references : reference list
  }

let get_span_context (span : t) : span_context =
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

let set_tag ~(key : string) ~(value : tag_value) (span : t) : t =
  { span with
    tags = { key; value } :: span.tags
  }

let log ~(tags : tag list) ?(log_ts : timestamp option) (span : t) : t =
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
      "[Trace=%S Span=%S Op=%S Ts=%fs]"
      t.span_context.trace_id
      t.span_context.span_id
      t.operation_name
      ((t.finish_ts |> CCOpt.get_or ~default:0.0) -. t.start_ts)
