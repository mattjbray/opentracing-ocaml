type value =
  [ `String of string
  | `Bool of bool
  | `Int of int
  | `Float of float
  ]

module StringMap = CCMap.Make(CCString)

type t = value StringMap.t

let empty = StringMap.empty
let add = StringMap.add
let of_list = StringMap.of_list
let find_opt = StringMap.find_opt
let bindings = StringMap.bindings
let filter = StringMap.filter

module Resource = struct
  let name = "resource.name"
end

module Http = struct
  let url = "http.url"
  let meth = "http.method"
  let status_code = "http.status_code"
end
