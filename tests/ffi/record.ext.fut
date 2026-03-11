-- tags { external }

type rec = {
  a: i32,
  b: i64,
  c: bool
}

entry foreign (a: rec) (b: rec): rec = { a = a.a * b.a, b = a.b + b.b, c = a.c && b.c }
