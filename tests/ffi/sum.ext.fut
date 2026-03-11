-- tags { external }

type sum = #a i32 i64 | #b f32

entry foreign (v: sum): f32 =
  match v
  case #a v1 v2 -> f32.i32 v1 + f32.i64 v2
  case #b v1 -> v1 ** 2
