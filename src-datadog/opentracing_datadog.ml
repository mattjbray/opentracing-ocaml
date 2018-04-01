module StringMap = CCMap.Make(CCString)

module Span = struct
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
      ]

  module Tags = struct
    (* SpanType defines the Span type (web, db, cache) *)
    let span_type = "span.type"

    (* ServiceName defines the Service name for this Span *)
    let service_name = "service.name"

    (* ResourceName defines the Resource name for the Span *)
    let resource_name = "resource.name"

    (* Error defines an error. *)
    let error = "error.error"
  end

  let get_string_tag (tag_key : string) (tags : Opentracing.Span.tag list) : string option =
    tags
    |> CCList.find_map (fun (tag : Opentracing.Span.tag) ->
        match tag.value with
        | `String value when tag.key = tag_key -> Some value
        | _ -> None
      )

  let seconds_to_nanoseconds (s : float) : Int64.t =
    s *. 1000000000.0
    |> Int64.of_float

  let t_of_opentracing_span (ot_span : Opentracing.Span.t) : t =
    { name = ot_span.Opentracing.Span.operation_name
    ; service =
        ot_span.tags
        |> get_string_tag Tags.service_name
        |> CCOpt.get_or ~default:""

    ; resource =
        ot_span.tags
        |> get_string_tag Tags.resource_name
        |> CCOpt.get_or ~default:""
    ; span_type =
        ot_span.tags
        |> get_string_tag Tags.span_type
        |> CCOpt.get_or ~default:""
    ; start = seconds_to_nanoseconds ot_span.start_ts
    ; duration =
        (ot_span.finish_ts |> CCOpt.get_or ~default:0.0) -. ot_span.start_ts
        |> seconds_to_nanoseconds
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

end

module Service = struct
  type t =
    { app : string
    ; app_type : string
    }

  let json_of_t (t : t) : Yojson.json =
    `Assoc
      [ ( "app", `String t.app )
      ; ( "app_type", `String t.app_type )
      ]
end

module Transport = struct
  let trace_uri = Uri.of_string "http://localhost:8126/v0.3/traces"
  let service_uri = Uri.of_string "http://localhost:8126/v0.3/services"

  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")]

  let send_traces (traces : Span.t list list) =
    let traces_json =
      `List
        (traces |> List.map (fun spans ->
             `List (spans |> List.map Span.json_of_t)
           ))
    in
    let body =
      traces_json
      |> Yojson.to_string
      |> Cohttp_lwt.Body.of_string
    in
    Cohttp_lwt_unix.Client.put ~headers ~body trace_uri

  let send_service (service : Service.t) =
    let body =
      service
      |> Service.json_of_t
      |> Yojson.to_string
      |> Cohttp_lwt.Body.of_string
    in
    Cohttp_lwt_unix.Client.put ~headers ~body service_uri
end

module Tracer = struct
  let section = Lwt_log.Section.make "tracer.datadog"

  module StringMap = CCMap.Make(CCString)

  type services = Service.t StringMap.t

  type t =
    { services : services }

  let init : t =
    { services = StringMap.empty }

  let tracer (spans_stream : Opentracing.Span.t Lwt_stream.t) : unit Lwt.t =
    let open Lwt.Infix in
    Lwt_stream.fold_s
      (fun ot_span t ->
        Lwt_log.notice ~section
          (CCFormat.to_string Opentracing.Span.pp ot_span) >>= fun () ->
         Lwt.return t
      )
      spans_stream
      init
    >>= fun _ ->
    Lwt.return_unit
end
