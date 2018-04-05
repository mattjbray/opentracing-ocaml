open Opium.Std

module Make(T : Opentracing_lwt.Tracer.S) = struct
  let middleware : Rock.Middleware.t =
    let filter : (Request.t, Response.t) Rock.Filter.simple =
      fun (handler : Rock.Handler.t) (req : Request.t) ->
        let url = req |> Request.uri |> Uri.to_string in
        T.trace "opium-server"
          ~tags:Opentracing.(
              Tags.of_list
                [ ( Tags.Http.url, `String url )
                ; ( Tags.Http.meth, `String (req |> Request.meth |> Cohttp.Code.string_of_method) )
                ; ( Tags.Resource.name, `String url ) (* resource.name should be overridden in the handler when the route is known. *)
                ])
          (fun () ->
             let open Lwt.Infix in
             handler req >>= fun resp ->
             T.modify_span
               (T.Span.set_tag
                  ~key:Opentracing.Tags.Http.status_code
                  ~value:(`String (resp.code |> Cohttp.Code.code_of_status |> string_of_int)))
             >>= fun () ->
             Lwt.return resp
          )
    in
    Rock.Middleware.create ~filter ~name:"opentracing-opium"

  module App = struct
    open Opium.Std

    (* Modify the handler to set the resource.name tag to the route name. *)
    let tag_route (route : App.route) : App.route =
      fun route_str handler ->
        let handler' (req : Request.t) : Response.t Lwt.t =
          let open Lwt.Infix in
          T.modify_span @@ T.Span.set_tag ~key:Opentracing.Tags.Resource.name ~value:(`String route_str) >>= fun () ->
          handler req
        in
        route route_str handler'

    let get : App.route = tag_route Opium.App.get
    let post : App.route = tag_route Opium.App.post
    let delete : App.route = tag_route Opium.App.delete
    let put : App.route = tag_route Opium.App.put
    let patch : App.route = tag_route Opium.App.patch
    let options : App.route = tag_route Opium.App.options
    let head : App.route = tag_route Opium.App.head
    let any (methods : Cohttp.Code.meth list) : App.route = tag_route (Opium.App.any methods)
    let all : App.route = tag_route Opium.App.all
    let action  (meth : Cohttp.Code.meth) : App.route = tag_route (Opium.App.action meth)
  end
end
