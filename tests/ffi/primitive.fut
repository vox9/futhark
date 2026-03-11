-- ==
-- externals { primitive.ext }

extern foreign_i8  (x: i8)  (y: i8):  i8 
extern foreign_i16 (x: i16) (y: i16): i16
extern foreign_i32 (x: i32) (y: i32): i32
extern foreign_i64 (x: i64) (y: i64): i64

extern foreign_u8  (x: u8)  (y: u8):  u8 
extern foreign_u16 (x: u16) (y: u16): u16
extern foreign_u32 (x: u32) (y: u32): u32
extern foreign_u64 (x: u64) (y: u64): u64

extern foreign_f16 (x: f16) (y: f16): f16
extern foreign_f32 (x: f32) (y: f32): f32
extern foreign_f64 (x: f64) (y: f64): f64

extern foreign_bool (x: bool) (y: bool): bool

-- ==
-- entry: main_i8
-- interpreted input { 3i8 2i8 } output { 1i8 }
entry main_i8 = foreign_i8
-- ==
-- entry: main_i16
-- interpreted input { 3i16 2i16 } output { 1i16 }
entry main_i16 = foreign_i16
-- ==
-- entry: main_i32
-- interpreted input { 3i32 2i32 } output { 1i32 }
entry main_i32 = foreign_i32
-- ==
-- entry: main_i64
-- interpreted input { 3i64 2i64 } output { 1i64 }
entry main_i64 = foreign_i64

-- ==
-- entry: main_u8
-- interpreted input { 3u8 2u8 } output { 1u8 }
entry main_u8 = foreign_u8
-- ==
-- entry: main_u16
-- interpreted input { 3u16 2u16 } output { 1u16 }
entry main_u16 = foreign_u16
-- ==
-- entry: main_u32
-- interpreted input { 3u32 2u32 } output { 1u32 }
entry main_u32 = foreign_u32
-- ==
-- entry: main_u64
-- interpreted input { 3u64 2u64 } output { 1u64 }
entry main_u64 = foreign_u64

-- ==
-- entry: main_f16
-- interpreted input { 3f16 2f16 } output { 1f16 }
entry main_f16 = foreign_f16
-- ==
-- entry: main_f32
-- interpreted input { 3f32 2f32 } output { 1f32 }
entry main_f32 = foreign_f32
-- ==
-- entry: main_f64
-- interpreted input { 3f64 2f64 } output { 1f64 }
entry main_f64 = foreign_f64

-- ==
-- entry: main_bool
-- interpreted input { true false } output { false }
-- interpreted input { true true } output { true }
entry main_bool = foreign_bool
