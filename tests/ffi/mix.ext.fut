-- tags { external }

type sum = #a i32 i64 | #b bool

type rec = {
  a: sum,
  b: i32,
  c: bool
}

entry foreign (r: rec): (i32, i64) =
  match r.a
  case #a s1 s2 -> (r.b + s1, s2)
  case #b s3 -> if r.c && s3 then (2i32, 5i64) else (r.b, 7i64)
