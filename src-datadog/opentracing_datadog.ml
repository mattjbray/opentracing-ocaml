module StringMap = CCMap.Make(CCString)

module Span_context = struct
  type trace_id = Int64.t
  type span_id = Int64.t

  let pp_trace_id = CCFormat.of_to_string Int64.to_string
  let pp_span_id = CCFormat.of_to_string Int64.to_string

  let () = Random.self_init ()
  let new_trace_id () = Random.int64 Int64.max_int
  let new_span_id () = Random.int64 Int64.max_int
end

module OT_Span = Opentracing.Span.Make(Span_context)

module Tags = struct
  (* SpanType defines the Span type (web, db, cache) *)
  let span_type = "span.type"

  (* ServiceName defines the Service name for this Span *)
  let service_name = "service.name"

  (* ResourceName defines the Resource name for the Span *)
  let resource_name = "resource.name"

  (* Error defines an error. *)
  let error = "error.error"

  let reserved_tags =
    [ span_type; service_name; resource_name; error ]
end

module DD_span = struct
  type t =
    { name : string
    (** Name is the name of the operation being measured. Some examples
        might be "http.handler", "fileserver.upload" or "video.decompress".
        Name should be set on every span.
    *)
    ; service : string
    (** Service is the name of the process doing a particular job. Some
        examples might be "user-database" or "datadog-web-app". Services
        will be inherited from parents, so only set this in your app's
        top level span.
    *)
    ; resource : string
    (** Resource is a query to a service. A web application might use
        resources like "/user/{user_id}". A sql database might use resources
        like "select * from user where id = ?".

        You can track thousands of resources (not millions or billions) so
        prefer normalized resources like "/user/{id}" to "/user/123".

        Resources should only be set on an app's top level spans.
    *)
    ; span_type : string
    (** protocol associated with the span*)
    ; start : Int64.t
    (** span start time expressed in nanoseconds since epoch *)
    ; duration : Int64.t
    (** duration of the span expressed in nanoseconds *)
    ; meta : string StringMap.t
    (** arbitrary map of metadata *)
    ; metrics : float StringMap.t
    (** arbitrary map of numeric metrics *)
    ; span_id : Int64.t
    (** identifier of this span *)
    ; trace_id : Int64.t
    (** identifier of the root span *)
    ; parent_id : Int64.t option
    (** identifier of the span's direct parent *)
    ; error : Int32.t
    (** error status of the span; 0 means no errors *)
    ; sampled : bool
    (** if this span is sampled (and should be kept/recorded) or not *)
    }

  let json_of_int64 (i64 : Int64.t) : Yojson.json =
    `Intlit (Int64.to_string i64)

  let json_of_meta (meta : string StringMap.t) : Yojson.json =
    `Assoc
      (StringMap.bindings meta
       |> List.map (fun (k, v) -> (k, `String v)))

  let json_of_metrics (metrics : float StringMap.t) : Yojson.json =
    `Assoc
      (StringMap.bindings metrics
       |> List.map (fun (k, v) -> (k, `Float v)))

  let json_of_t (t : t) : Yojson.json =
    `Assoc
      [ ( "trace_id", json_of_int64 t.trace_id )
      ; ( "span_id", json_of_int64 t.span_id )
      ; ( "name", `String t.name )
      ; ( "resource", `String t.resource )
      ; ( "service", `String t.service )
      ; ( "type", `String t.span_type )
      ; ( "start", json_of_int64 t.start )
      ; ( "duration", json_of_int64 t.duration )
      ; ( "parent_id", t.parent_id |> CCOpt.map_or ~default:`Null json_of_int64 )
      ; ( "error", `Intlit (Int32.to_string t.error) )
      ; ( "meta", json_of_meta t.meta )
      ; ( "metrics", json_of_metrics t.metrics )
      ]

  let get_string_tag (tag_key : string) (tags : Opentracing.Tags.t) : string =
    match Opentracing.Tags.find_opt tag_key tags with
    | Some (`String value) -> value
    | _ -> ""

  let seconds_to_nanoseconds (s : float) : Int64.t =
    s *. 1000000000.0
    |> Int64.of_float

  let t_of_opentracing_span (ot_span : OT_Span.t) : t =
    { name = ot_span.OT_Span.operation_name
    ; service =
        ot_span.tags
        |> get_string_tag Tags.service_name
    ; resource =
        ot_span.tags
        |> get_string_tag Tags.resource_name
    ; span_type =
        ot_span.tags
        |> get_string_tag Tags.span_type
    ; start =
        seconds_to_nanoseconds ot_span.start_ts
    ; duration =
        (ot_span.finish_ts |> CCOpt.get_or ~default:0.0) -. ot_span.start_ts
        |> seconds_to_nanoseconds
    ; meta =
        Opentracing.Tags.bindings ot_span.tags
        |> CCList.filter_map (fun (k, v) ->
            if List.mem k Tags.reserved_tags then
              None
            else
              match v with
              | `String v -> Some (k, v)
              | _ -> None
          )
        |> StringMap.of_list
    ; metrics =
        Opentracing.Tags.bindings ot_span.tags
        |> CCList.filter_map (fun (k, v) ->
            match v with
            | `String _ -> None
            | `Bool b -> Some (k, if b then 0. else 1.)
            | `Int i -> Some (k, float_of_int i)
            | `Float f -> Some (k, f)
          )
        |> StringMap.of_list
    ; span_id =
        ot_span.span_context.span_id
    ; trace_id =
        ot_span.span_context.trace_id
    ; parent_id =
        ot_span.references
        |> CCList.find_map
          (fun r ->
             match r.OT_Span.reference_type with
             | Child_of -> Some r.reference_context.span_id
             | Follows_from -> None
          )
    ; error =
        begin match Opentracing.Tags.find_opt Tags.error ot_span.tags with
          | Some (`Int value) -> Int32.of_int value
          | _ -> Int32.zero
        end
    ; sampled = true
    }

end


module Service = struct
  type t =
    { app : string
    ; app_type : string
    }

  let equal t1 t2 =
    String.equal t1.app t2.app &&
    String.equal t1.app_type t1.app_type

  let json_of_t (t : t) : Yojson.json =
    `Assoc
      [ ( "app", `String t.app )
      ; ( "app_type", `String t.app_type )
      ]
end

type services = Service.t StringMap.t

module Transport = struct
  let section = Lwt_log.Section.make "ot.datadog.transport"

  let trace_uri = Uri.of_string "http://localhost:8126/v0.3/traces"
  let service_uri = Uri.of_string "http://localhost:8126/v0.3/services"

  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")]

  let send_traces (traces : DD_span.t list list) =
    let traces_json =
      `List
        (traces |> List.map (fun spans ->
             `List (spans |> List.map DD_span.json_of_t)
           ))
    in
    let body_str =
      traces_json
      |> Yojson.to_string
    in
    let body = Cohttp_lwt.Body.of_string body_str in
    let open Lwt.Infix in
    Lwt_log.notice_f ~section "Sending traces: %s" body_str >>= fun () ->
    Cohttp_lwt_unix.Client.put ~headers ~body trace_uri

  let send_services (services : services) =
    let body_str =
      `Assoc
        (services
         |> StringMap.map Service.json_of_t
         |> StringMap.bindings)
      |> Yojson.to_string
    in
    let body =
      Cohttp_lwt.Body.of_string body_str
    in
    let open Lwt.Infix in
    Lwt_log.notice_f ~section "Sending services: %s" body_str >>= fun () ->
    Cohttp_lwt_unix.Client.put ~headers ~body service_uri
end

module Implementation : Opentracing_lwt.Tracer.Implementation = struct
  let section = Lwt_log.Section.make "tracer.datadog"

  let inherit_tags = Tags.reserved_tags

  module Int64Map = CCMap.Make(Int64)

  type t =
    { services : services
    ; last_services : services (** Services at last flush. *)
    ; traces : DD_span.t list Int64Map.t
    }

  let init : t =
    { services = StringMap.empty
    ; last_services = StringMap.empty
    ; traces = Int64Map.empty
    }

  let t_mvar : t Lwt_mvar.t =
    Lwt_mvar.create init

  let spans_collected : t -> int =
    fun t ->
      t.traces
      |> Int64Map.bindings
      |> List.map (fun (k, spans) -> List.length spans)
      |> List.fold_left (+) 0

  module Span = Opentracing.Span.Make(Span_context)

  let span_receiver (spans_stream : Span.t Lwt_stream.t) : unit Lwt.t =
    let open Lwt.Infix in
    spans_stream
    |> Lwt_stream.iter_s
      (fun ot_span ->
         let span = DD_span.t_of_opentracing_span ot_span in
         Lwt_log.notice_f ~section
           "Received span: %s"
           (CCFormat.to_string Span.pp ot_span) >>= fun () ->

         Lwt_mvar.take t_mvar >>= fun t ->

         let t' =
           { t with
             services =
               if span.service = "" then
                 t.services
               else
                 StringMap.add span.service
                   Service.{ app = span.service; app_type = span.span_type }
                   t.services
           ; traces =
               t.traces
               |> Int64Map.update span.trace_id
                 (function
                   | None -> Some [span]
                   | Some spans -> Some (span :: spans)
                 )
           }
         in

         Lwt_mvar.put t_mvar t'
      )

  let unless (cond : bool) (f : unit -> unit Lwt.t) : unit Lwt.t =
    if cond then Lwt.return_unit else f ()

  let flush () : unit Lwt.t =
    let open Lwt.Infix in
    Lwt_log.notice ~section "flush" >>= fun () ->
    Lwt_mvar.take t_mvar >>= fun t ->
    unless (Int64Map.is_empty t.traces)
      (fun () ->
         Transport.send_traces (Int64Map.bindings t.traces |> List.map snd) >>= fun (resp, body) ->
         Cohttp_lwt.Body.to_string body >>= fun body_str ->
         Lwt_log.notice_f ~section
           "Received response %i: %S"
           (Cohttp.Response.status resp |> Cohttp.Code.code_of_status)
           body_str
      ) >>= fun () ->

    unless (StringMap.equal Service.equal t.services t.last_services)
      (fun () ->
         Transport.send_services t.services >>= fun (resp, body) ->
         Cohttp_lwt.Body.to_string body >>= fun body_str ->
         Lwt_log.notice_f ~section
           "Received response %i: %S"
           (Cohttp.Response.status resp |> Cohttp.Code.code_of_status)
           body_str
      ) >>= fun () ->

    Lwt_mvar.put t_mvar
      { t with
        traces = Int64Map.empty
      ; last_services = t.services
      }

end

module Tracer = Opentracing_lwt.Tracer.Make(Implementation)
