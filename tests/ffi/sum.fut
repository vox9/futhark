-- ==
-- externals { sum.ext }

type sum = #a i32 i64 | #b f32

extern foreign (v: sum): f32

-- ==
-- input { true 3i32 6i64 0f32 } output { 9f32 }
-- input { false 0i32 0i64 4f32 } output { 16f32 }
entry main b v1 v2 v3 =
  if b then foreign (#a v1 v2)
       else foreign (#b v3)
