type stream_dependency = {dependency_id: int; weight: int; is_exclusive: bool}
[@@deriving fields]

type t = {stream_id: int; dependency: stream_dependency} [@@deriving fields]
