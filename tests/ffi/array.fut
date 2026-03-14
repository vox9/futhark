-- ==
-- externals { array.ext }

extern foreign (x: []i32) (y: []i32): []i32

-- ==
-- entry: main
-- interpreted input { [1i32, 2i32, 3i32] [2i32, 3i32, 2i32] } output { [1i32, 8i32, 9i32] }
entry main = foreign
