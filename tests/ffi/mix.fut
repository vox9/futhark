-- ==
-- externals { mix.ext }

type sum = #a i32 i64 | #b bool

type rec = {
  a: sum,
  b: i32,
  c: bool
}

extern foreign (r: rec): (i32, i64)

-- ==
-- input { true 3i32 7i64 true 2i32 false } output { 5i32 7i64 }
-- input { false 3i32 7i64 true 2i32 false } output { 2i32 7i64 }
entry main b s1 s2 s3 r2 r3 =
  if b then foreign { a = #a s1 s2, b = r2, c = r3 }
       else foreign { a = #b s3, b = r2, c = r3 }
