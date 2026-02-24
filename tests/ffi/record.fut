-- ==
-- externals { record.ext }

type rec = {
  a: i32,
  b: i64,
  c: bool
}

extern foreign (a: rec) (b: rec): rec

-- ==
-- input { 3i32 2i64 true   2i32 7i64 false } output { 6i32 9i64 false }
entry main aa ab ac ba bb bc =
  let o = foreign { a=aa, b=ab, c=ac } { a=ba, b=bb, c=bc }
  in (o.a, o.b, o.c)
