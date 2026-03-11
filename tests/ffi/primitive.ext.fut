-- tags { external }

entry foreign_i8  (x: i8)  (y: i8):  i8   = x - y
entry foreign_i16 (x: i16) (y: i16): i16  = x - y
entry foreign_i32 (x: i32) (y: i32): i32  = x - y
entry foreign_i64 (x: i64) (y: i64): i64  = x - y

entry foreign_u8  (x: u8)  (y: u8):  u8   = x - y
entry foreign_u16 (x: u16) (y: u16): u16  = x - y
entry foreign_u32 (x: u32) (y: u32): u32  = x - y
entry foreign_u64 (x: u64) (y: u64): u64  = x - y

entry foreign_f16 (x: f16) (y: f16): f16  = x - y
entry foreign_f32 (x: f32) (y: f32): f32  = x - y
entry foreign_f64 (x: f64) (y: f64): f64  = x - y

entry foreign_bool (x: bool) (y: bool): bool = x && y
