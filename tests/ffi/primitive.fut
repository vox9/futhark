-- ==
-- externals { primitive.ext }

extern foreign (x: i32) (y: i32): i32

-- ==
-- interpreted input { 1i32 2i32 } output { 9i32 }
entry main = foreign
