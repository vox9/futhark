-- Scan with 4x4 matrix multiplication.
-- ==
-- entry: fwd rev
-- input {
-- [[1f32,2f32,3f32,4f32,5f32,6f32,7f32,8f32,9f32,10f32,11f32,12f32,13f32,14f32,15f32,16f32],
--  [16f32,15f32,14f32,13f32,12f32,11f32,10f32,9f32,8f32,7f32,6f32,5f32,4f32,3f32,2f32,1f32],
--  [1f32,2f32,3f32,4f32,5f32,6f32,7f32,8f32,9f32,10f32,11f32,12f32,13f32,14f32,15f32,16f32],
--  [16f32,15f32,14f32,13f32,12f32,11f32,10f32,9f32,8f32,7f32,6f32,5f32,4f32,3f32,2f32,1f32]]
-- }
-- output { [[[[1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]],
-- [[0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32,
-- 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32,
-- 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32,
-- 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32,
-- 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
-- [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
-- [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 1f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]],
-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 1f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 1f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 1f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 1f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 1f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
-- [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32]]], [[[16f32, 12f32, 8f32, 4f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
-- [1f32, 0f32, 0f32, 0f32, 2f32, 0f32, 0f32, 0f32, 3f32, 0f32, 0f32,
-- 0f32, 4f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32]], [[15f32, 11f32, 7f32, 3f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32,
-- 1f32, 0f32, 0f32, 0f32, 2f32, 0f32, 0f32, 0f32, 3f32, 0f32, 0f32,
-- 0f32, 4f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32]], [[14f32, 10f32, 6f32, 2f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32,
-- 1f32, 0f32, 0f32, 0f32, 2f32, 0f32, 0f32, 0f32, 3f32, 0f32, 0f32,
-- 0f32, 4f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32]], [[13f32, 9f32, 5f32, 1f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32,
-- 1f32, 0f32, 0f32, 0f32, 2f32, 0f32, 0f32, 0f32, 3f32, 0f32, 0f32,
-- 0f32, 4f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32]], [[0f32, 0f32, 0f32, 0f32, 16f32, 12f32, 8f32, 4f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [5f32, 0f32, 0f32, 0f32,
-- 6f32, 0f32, 0f32, 0f32, 7f32, 0f32, 0f32, 0f32, 8f32, 0f32, 0f32,
-- 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]],
-- [[0f32, 0f32, 0f32, 0f32, 15f32, 11f32, 7f32, 3f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 5f32, 0f32, 0f32, 0f32,
-- 6f32, 0f32, 0f32, 0f32, 7f32, 0f32, 0f32, 0f32, 8f32, 0f32, 0f32],
-- [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]],
-- [[0f32, 0f32, 0f32, 0f32, 14f32, 10f32, 6f32, 2f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 5f32, 0f32, 0f32,
-- 0f32, 6f32, 0f32, 0f32, 0f32, 7f32, 0f32, 0f32, 0f32, 8f32, 0f32],
-- [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]],
-- [[0f32, 0f32, 0f32, 0f32, 13f32, 9f32, 5f32, 1f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 5f32, 0f32,
-- 0f32, 0f32, 6f32, 0f32, 0f32, 0f32, 7f32, 0f32, 0f32, 0f32, 8f32],
-- [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]],
-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 16f32, 12f32,
-- 8f32, 4f32, 0f32, 0f32, 0f32, 0f32], [9f32, 0f32, 0f32, 0f32,
-- 10f32, 0f32, 0f32, 0f32, 11f32, 0f32, 0f32, 0f32, 12f32, 0f32,
-- 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 15f32,
-- 11f32, 7f32, 3f32, 0f32, 0f32, 0f32, 0f32], [0f32, 9f32, 0f32,
-- 0f32, 0f32, 10f32, 0f32, 0f32, 0f32, 11f32, 0f32, 0f32, 0f32,
-- 12f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 14f32, 10f32, 6f32, 2f32, 0f32, 0f32, 0f32, 0f32], [0f32,
-- 0f32, 9f32, 0f32, 0f32, 0f32, 10f32, 0f32, 0f32, 0f32, 11f32, 0f32,
-- 0f32, 0f32, 12f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 13f32, 9f32, 5f32, 1f32, 0f32, 0f32, 0f32, 0f32],
-- [0f32, 0f32, 0f32, 9f32, 0f32, 0f32, 0f32, 10f32, 0f32, 0f32, 0f32,
-- 11f32, 0f32, 0f32, 0f32, 12f32], [0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
-- [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 16f32, 12f32, 8f32,
-- 4f32], [13f32, 0f32, 0f32, 0f32, 14f32, 0f32, 0f32, 0f32, 15f32,
-- 0f32, 0f32, 0f32, 16f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 15f32, 11f32,
-- 7f32, 3f32], [0f32, 13f32, 0f32, 0f32, 0f32, 14f32, 0f32, 0f32,
-- 0f32, 15f32, 0f32, 0f32, 0f32, 16f32, 0f32, 0f32], [0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 14f32,
-- 10f32, 6f32, 2f32], [0f32, 0f32, 13f32, 0f32, 0f32, 0f32, 14f32,
-- 0f32, 0f32, 0f32, 15f32, 0f32, 0f32, 0f32, 16f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 13f32, 9f32, 5f32, 1f32], [0f32, 0f32, 0f32, 13f32, 0f32, 0f32,
-- 0f32, 14f32, 0f32, 0f32, 0f32, 15f32, 0f32, 0f32, 0f32, 16f32],
-- [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]]],
-- [[[386f32, 274f32, 162f32, 50f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [1f32, 5f32, 9f32,
-- 13f32, 2f32, 10f32, 18f32, 26f32, 3f32, 15f32, 27f32, 39f32, 4f32,
-- 20f32, 36f32, 52f32], [80f32, 0f32, 0f32, 0f32, 70f32, 0f32, 0f32,
-- 0f32, 60f32, 0f32, 0f32, 0f32, 50f32, 0f32, 0f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32]], [[444f32, 316f32, 188f32, 60f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32],
-- [2f32, 6f32, 10f32, 14f32, 4f32, 12f32, 20f32, 28f32, 6f32, 18f32,
-- 30f32, 42f32, 8f32, 24f32, 40f32, 56f32], [0f32, 80f32, 0f32, 0f32,
-- 0f32, 70f32, 0f32, 0f32, 0f32, 60f32, 0f32, 0f32, 0f32, 50f32,
-- 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[502f32, 358f32,
-- 214f32, 70f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32], [3f32, 7f32, 11f32, 15f32, 6f32, 14f32,
-- 22f32, 30f32, 9f32, 21f32, 33f32, 45f32, 12f32, 28f32, 44f32,
-- 60f32], [0f32, 0f32, 80f32, 0f32, 0f32, 0f32, 70f32, 0f32, 0f32,
-- 0f32, 60f32, 0f32, 0f32, 0f32, 50f32, 0f32], [0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32]], [[560f32, 400f32, 240f32, 80f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [4f32, 8f32,
-- 12f32, 16f32, 8f32, 16f32, 24f32, 32f32, 12f32, 24f32, 36f32,
-- 48f32, 16f32, 32f32, 48f32, 64f32], [0f32, 0f32, 0f32, 80f32, 0f32,
-- 0f32, 0f32, 70f32, 0f32, 0f32, 0f32, 60f32, 0f32, 0f32, 0f32,
-- 50f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32,
-- 0f32, 386f32, 274f32, 162f32, 50f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32], [5f32, 25f32, 45f32, 65f32, 6f32, 30f32, 54f32,
-- 78f32, 7f32, 35f32, 63f32, 91f32, 8f32, 40f32, 72f32, 104f32],
-- [240f32, 0f32, 0f32, 0f32, 214f32, 0f32, 0f32, 0f32, 188f32, 0f32,
-- 0f32, 0f32, 162f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32]], [[0f32, 0f32, 0f32, 0f32, 444f32, 316f32, 188f32, 60f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [10f32, 30f32,
-- 50f32, 70f32, 12f32, 36f32, 60f32, 84f32, 14f32, 42f32, 70f32,
-- 98f32, 16f32, 48f32, 80f32, 112f32], [0f32, 240f32, 0f32, 0f32,
-- 0f32, 214f32, 0f32, 0f32, 0f32, 188f32, 0f32, 0f32, 0f32, 162f32,
-- 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32,
-- 0f32, 502f32, 358f32, 214f32, 70f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32], [15f32, 35f32, 55f32, 75f32, 18f32, 42f32,
-- 66f32, 90f32, 21f32, 49f32, 77f32, 105f32, 24f32, 56f32, 88f32,
-- 120f32], [0f32, 0f32, 240f32, 0f32, 0f32, 0f32, 214f32, 0f32, 0f32,
-- 0f32, 188f32, 0f32, 0f32, 0f32, 162f32, 0f32], [0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 560f32, 400f32, 240f32,
-- 80f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [20f32,
-- 40f32, 60f32, 80f32, 24f32, 48f32, 72f32, 96f32, 28f32, 56f32,
-- 84f32, 112f32, 32f32, 64f32, 96f32, 128f32], [0f32, 0f32, 0f32,
-- 240f32, 0f32, 0f32, 0f32, 214f32, 0f32, 0f32, 0f32, 188f32, 0f32,
-- 0f32, 0f32, 162f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 386f32, 274f32, 162f32,
-- 50f32, 0f32, 0f32, 0f32, 0f32], [9f32, 45f32, 81f32, 117f32, 10f32,
-- 50f32, 90f32, 130f32, 11f32, 55f32, 99f32, 143f32, 12f32, 60f32,
-- 108f32, 156f32], [400f32, 0f32, 0f32, 0f32, 358f32, 0f32, 0f32,
-- 0f32, 316f32, 0f32, 0f32, 0f32, 274f32, 0f32, 0f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 444f32, 316f32, 188f32, 60f32, 0f32, 0f32, 0f32, 0f32],
-- [18f32, 54f32, 90f32, 126f32, 20f32, 60f32, 100f32, 140f32, 22f32,
-- 66f32, 110f32, 154f32, 24f32, 72f32, 120f32, 168f32], [0f32,
-- 400f32, 0f32, 0f32, 0f32, 358f32, 0f32, 0f32, 0f32, 316f32, 0f32,
-- 0f32, 0f32, 274f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]],
-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 502f32, 358f32,
-- 214f32, 70f32, 0f32, 0f32, 0f32, 0f32], [27f32, 63f32, 99f32,
-- 135f32, 30f32, 70f32, 110f32, 150f32, 33f32, 77f32, 121f32, 165f32,
-- 36f32, 84f32, 132f32, 180f32], [0f32, 0f32, 400f32, 0f32, 0f32,
-- 0f32, 358f32, 0f32, 0f32, 0f32, 316f32, 0f32, 0f32, 0f32, 274f32,
-- 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 560f32, 400f32, 240f32, 80f32, 0f32, 0f32,
-- 0f32, 0f32], [36f32, 72f32, 108f32, 144f32, 40f32, 80f32, 120f32,
-- 160f32, 44f32, 88f32, 132f32, 176f32, 48f32, 96f32, 144f32,
-- 192f32], [0f32, 0f32, 0f32, 400f32, 0f32, 0f32, 0f32, 358f32, 0f32,
-- 0f32, 0f32, 316f32, 0f32, 0f32, 0f32, 274f32], [0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 386f32, 274f32, 162f32, 50f32], [13f32,
-- 65f32, 117f32, 169f32, 14f32, 70f32, 126f32, 182f32, 15f32, 75f32,
-- 135f32, 195f32, 16f32, 80f32, 144f32, 208f32], [560f32, 0f32, 0f32,
-- 0f32, 502f32, 0f32, 0f32, 0f32, 444f32, 0f32, 0f32, 0f32, 386f32,
-- 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 444f32,
-- 316f32, 188f32, 60f32], [26f32, 78f32, 130f32, 182f32, 28f32,
-- 84f32, 140f32, 196f32, 30f32, 90f32, 150f32, 210f32, 32f32, 96f32,
-- 160f32, 224f32], [0f32, 560f32, 0f32, 0f32, 0f32, 502f32, 0f32,
-- 0f32, 0f32, 444f32, 0f32, 0f32, 0f32, 386f32, 0f32, 0f32], [0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 502f32, 358f32, 214f32, 70f32],
-- [39f32, 91f32, 143f32, 195f32, 42f32, 98f32, 154f32, 210f32, 45f32,
-- 105f32, 165f32, 225f32, 48f32, 112f32, 176f32, 240f32], [0f32,
-- 0f32, 560f32, 0f32, 0f32, 0f32, 502f32, 0f32, 0f32, 0f32, 444f32,
-- 0f32, 0f32, 0f32, 386f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]],
-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 560f32, 400f32, 240f32, 80f32], [52f32, 104f32, 156f32,
-- 208f32, 56f32, 112f32, 168f32, 224f32, 60f32, 120f32, 180f32,
-- 240f32, 64f32, 128f32, 192f32, 256f32], [0f32, 0f32, 0f32, 560f32,
-- 0f32, 0f32, 0f32, 502f32, 0f32, 0f32, 0f32, 444f32, 0f32, 0f32,
-- 0f32, 386f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32]]], [[[17760f32,
-- 12640f32, 7520f32, 2400f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [80f32, 240f32, 400f32,
-- 560f32, 160f32, 480f32, 800f32, 1120f32, 240f32, 720f32, 1200f32,
-- 1680f32, 320f32, 960f32, 1600f32, 2240f32], [1280f32, 960f32,
-- 640f32, 320f32, 1120f32, 840f32, 560f32, 280f32, 960f32, 720f32,
-- 480f32, 240f32, 800f32, 600f32, 400f32, 200f32], [1620f32, 0f32,
-- 0f32, 0f32, 1880f32, 0f32, 0f32, 0f32, 2140f32, 0f32, 0f32, 0f32,
-- 2400f32, 0f32, 0f32, 0f32]], [[15868f32, 11292f32, 6716f32,
-- 2140f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32], [70f32, 214f32, 358f32, 502f32, 140f32, 428f32,
-- 716f32, 1004f32, 210f32, 642f32, 1074f32, 1506f32, 280f32, 856f32,
-- 1432f32, 2008f32], [1200f32, 880f32, 560f32, 240f32, 1050f32,
-- 770f32, 490f32, 210f32, 900f32, 660f32, 420f32, 180f32, 750f32,
-- 550f32, 350f32, 150f32], [0f32, 1620f32, 0f32, 0f32, 0f32, 1880f32,
-- 0f32, 0f32, 0f32, 2140f32, 0f32, 0f32, 0f32, 2400f32, 0f32, 0f32]],
-- [[13976f32, 9944f32, 5912f32, 1880f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [60f32, 188f32,
-- 316f32, 444f32, 120f32, 376f32, 632f32, 888f32, 180f32, 564f32,
-- 948f32, 1332f32, 240f32, 752f32, 1264f32, 1776f32], [1120f32,
-- 800f32, 480f32, 160f32, 980f32, 700f32, 420f32, 140f32, 840f32,
-- 600f32, 360f32, 120f32, 700f32, 500f32, 300f32, 100f32], [0f32,
-- 0f32, 1620f32, 0f32, 0f32, 0f32, 1880f32, 0f32, 0f32, 0f32,
-- 2140f32, 0f32, 0f32, 0f32, 2400f32, 0f32]], [[12084f32, 8596f32,
-- 5108f32, 1620f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32], [50f32, 162f32, 274f32, 386f32, 100f32,
-- 324f32, 548f32, 772f32, 150f32, 486f32, 822f32, 1158f32, 200f32,
-- 648f32, 1096f32, 1544f32], [1040f32, 720f32, 400f32, 80f32, 910f32,
-- 630f32, 350f32, 70f32, 780f32, 540f32, 300f32, 60f32, 650f32,
-- 450f32, 250f32, 50f32], [0f32, 0f32, 0f32, 1620f32, 0f32, 0f32,
-- 0f32, 1880f32, 0f32, 0f32, 0f32, 2140f32, 0f32, 0f32, 0f32,
-- 2400f32]], [[0f32, 0f32, 0f32, 0f32, 17760f32, 12640f32, 7520f32,
-- 2400f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [400f32,
-- 1200f32, 2000f32, 2800f32, 480f32, 1440f32, 2400f32, 3360f32,
-- 560f32, 1680f32, 2800f32, 3920f32, 640f32, 1920f32, 3200f32,
-- 4480f32], [3840f32, 2880f32, 1920f32, 960f32, 3424f32, 2568f32,
-- 1712f32, 856f32, 3008f32, 2256f32, 1504f32, 752f32, 2592f32,
-- 1944f32, 1296f32, 648f32], [5108f32, 0f32, 0f32, 0f32, 5912f32,
-- 0f32, 0f32, 0f32, 6716f32, 0f32, 0f32, 0f32, 7520f32, 0f32, 0f32,
-- 0f32]], [[0f32, 0f32, 0f32, 0f32, 15868f32, 11292f32, 6716f32,
-- 2140f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [350f32,
-- 1070f32, 1790f32, 2510f32, 420f32, 1284f32, 2148f32, 3012f32,
-- 490f32, 1498f32, 2506f32, 3514f32, 560f32, 1712f32, 2864f32,
-- 4016f32], [3600f32, 2640f32, 1680f32, 720f32, 3210f32, 2354f32,
-- 1498f32, 642f32, 2820f32, 2068f32, 1316f32, 564f32, 2430f32,
-- 1782f32, 1134f32, 486f32], [0f32, 5108f32, 0f32, 0f32, 0f32,
-- 5912f32, 0f32, 0f32, 0f32, 6716f32, 0f32, 0f32, 0f32, 7520f32,
-- 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 13976f32, 9944f32, 5912f32,
-- 1880f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [300f32,
-- 940f32, 1580f32, 2220f32, 360f32, 1128f32, 1896f32, 2664f32,
-- 420f32, 1316f32, 2212f32, 3108f32, 480f32, 1504f32, 2528f32,
-- 3552f32], [3360f32, 2400f32, 1440f32, 480f32, 2996f32, 2140f32,
-- 1284f32, 428f32, 2632f32, 1880f32, 1128f32, 376f32, 2268f32,
-- 1620f32, 972f32, 324f32], [0f32, 0f32, 5108f32, 0f32, 0f32, 0f32,
-- 5912f32, 0f32, 0f32, 0f32, 6716f32, 0f32, 0f32, 0f32, 7520f32,
-- 0f32]], [[0f32, 0f32, 0f32, 0f32, 12084f32, 8596f32, 5108f32,
-- 1620f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [250f32,
-- 810f32, 1370f32, 1930f32, 300f32, 972f32, 1644f32, 2316f32, 350f32,
-- 1134f32, 1918f32, 2702f32, 400f32, 1296f32, 2192f32, 3088f32],
-- [3120f32, 2160f32, 1200f32, 240f32, 2782f32, 1926f32, 1070f32,
-- 214f32, 2444f32, 1692f32, 940f32, 188f32, 2106f32, 1458f32, 810f32,
-- 162f32], [0f32, 0f32, 0f32, 5108f32, 0f32, 0f32, 0f32, 5912f32,
-- 0f32, 0f32, 0f32, 6716f32, 0f32, 0f32, 0f32, 7520f32]], [[0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 17760f32, 12640f32,
-- 7520f32, 2400f32, 0f32, 0f32, 0f32, 0f32], [720f32, 2160f32,
-- 3600f32, 5040f32, 800f32, 2400f32, 4000f32, 5600f32, 880f32,
-- 2640f32, 4400f32, 6160f32, 960f32, 2880f32, 4800f32, 6720f32],
-- [6400f32, 4800f32, 3200f32, 1600f32, 5728f32, 4296f32, 2864f32,
-- 1432f32, 5056f32, 3792f32, 2528f32, 1264f32, 4384f32, 3288f32,
-- 2192f32, 1096f32], [8596f32, 0f32, 0f32, 0f32, 9944f32, 0f32, 0f32,
-- 0f32, 11292f32, 0f32, 0f32, 0f32, 12640f32, 0f32, 0f32, 0f32]],
-- [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 15868f32,
-- 11292f32, 6716f32, 2140f32, 0f32, 0f32, 0f32, 0f32], [630f32,
-- 1926f32, 3222f32, 4518f32, 700f32, 2140f32, 3580f32, 5020f32,
-- 770f32, 2354f32, 3938f32, 5522f32, 840f32, 2568f32, 4296f32,
-- 6024f32], [6000f32, 4400f32, 2800f32, 1200f32, 5370f32, 3938f32,
-- 2506f32, 1074f32, 4740f32, 3476f32, 2212f32, 948f32, 4110f32,
-- 3014f32, 1918f32, 822f32], [0f32, 8596f32, 0f32, 0f32, 0f32,
-- 9944f32, 0f32, 0f32, 0f32, 11292f32, 0f32, 0f32, 0f32, 12640f32,
-- 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 13976f32, 9944f32, 5912f32, 1880f32, 0f32, 0f32, 0f32, 0f32],
-- [540f32, 1692f32, 2844f32, 3996f32, 600f32, 1880f32, 3160f32,
-- 4440f32, 660f32, 2068f32, 3476f32, 4884f32, 720f32, 2256f32,
-- 3792f32, 5328f32], [5600f32, 4000f32, 2400f32, 800f32, 5012f32,
-- 3580f32, 2148f32, 716f32, 4424f32, 3160f32, 1896f32, 632f32,
-- 3836f32, 2740f32, 1644f32, 548f32], [0f32, 0f32, 8596f32, 0f32,
-- 0f32, 0f32, 9944f32, 0f32, 0f32, 0f32, 11292f32, 0f32, 0f32, 0f32,
-- 12640f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 12084f32, 8596f32, 5108f32, 1620f32, 0f32, 0f32, 0f32, 0f32],
-- [450f32, 1458f32, 2466f32, 3474f32, 500f32, 1620f32, 2740f32,
-- 3860f32, 550f32, 1782f32, 3014f32, 4246f32, 600f32, 1944f32,
-- 3288f32, 4632f32], [5200f32, 3600f32, 2000f32, 400f32, 4654f32,
-- 3222f32, 1790f32, 358f32, 4108f32, 2844f32, 1580f32, 316f32,
-- 3562f32, 2466f32, 1370f32, 274f32], [0f32, 0f32, 0f32, 8596f32,
-- 0f32, 0f32, 0f32, 9944f32, 0f32, 0f32, 0f32, 11292f32, 0f32, 0f32,
-- 0f32, 12640f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 17760f32, 12640f32, 7520f32, 2400f32],
-- [1040f32, 3120f32, 5200f32, 7280f32, 1120f32, 3360f32, 5600f32,
-- 7840f32, 1200f32, 3600f32, 6000f32, 8400f32, 1280f32, 3840f32,
-- 6400f32, 8960f32], [8960f32, 6720f32, 4480f32, 2240f32, 8032f32,
-- 6024f32, 4016f32, 2008f32, 7104f32, 5328f32, 3552f32, 1776f32,
-- 6176f32, 4632f32, 3088f32, 1544f32], [12084f32, 0f32, 0f32, 0f32,
-- 13976f32, 0f32, 0f32, 0f32, 15868f32, 0f32, 0f32, 0f32, 17760f32,
-- 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 15868f32, 11292f32, 6716f32,
-- 2140f32], [910f32, 2782f32, 4654f32, 6526f32, 980f32, 2996f32,
-- 5012f32, 7028f32, 1050f32, 3210f32, 5370f32, 7530f32, 1120f32,
-- 3424f32, 5728f32, 8032f32], [8400f32, 6160f32, 3920f32, 1680f32,
-- 7530f32, 5522f32, 3514f32, 1506f32, 6660f32, 4884f32, 3108f32,
-- 1332f32, 5790f32, 4246f32, 2702f32, 1158f32], [0f32, 12084f32,
-- 0f32, 0f32, 0f32, 13976f32, 0f32, 0f32, 0f32, 15868f32, 0f32, 0f32,
-- 0f32, 17760f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 13976f32, 9944f32, 5912f32,
-- 1880f32], [780f32, 2444f32, 4108f32, 5772f32, 840f32, 2632f32,
-- 4424f32, 6216f32, 900f32, 2820f32, 4740f32, 6660f32, 960f32,
-- 3008f32, 5056f32, 7104f32], [7840f32, 5600f32, 3360f32, 1120f32,
-- 7028f32, 5020f32, 3012f32, 1004f32, 6216f32, 4440f32, 2664f32,
-- 888f32, 5404f32, 3860f32, 2316f32, 772f32], [0f32, 0f32, 12084f32,
-- 0f32, 0f32, 0f32, 13976f32, 0f32, 0f32, 0f32, 15868f32, 0f32, 0f32,
-- 0f32, 17760f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 0f32, 0f32,
-- 0f32, 0f32, 0f32, 0f32, 0f32, 12084f32, 8596f32, 5108f32, 1620f32],
-- [650f32, 2106f32, 3562f32, 5018f32, 700f32, 2268f32, 3836f32,
-- 5404f32, 750f32, 2430f32, 4110f32, 5790f32, 800f32, 2592f32,
-- 4384f32, 6176f32], [7280f32, 5040f32, 2800f32, 560f32, 6526f32,
-- 4518f32, 2510f32, 502f32, 5772f32, 3996f32, 2220f32, 444f32,
-- 5018f32, 3474f32, 1930f32, 386f32], [0f32, 0f32, 0f32, 12084f32,
-- 0f32, 0f32, 0f32, 13976f32, 0f32, 0f32, 0f32, 15868f32, 0f32, 0f32,
-- 0f32, 17760f32]]]] }

def mm4by4  (a0,b0,c0,d0,e0,f0,g0,h0,i0,j0,k0,l0,m0,n0,o0,p0)
            (a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1:f32) =
  ( a0*a1 + b0*e1 + c0*i1 + d0*m1
  , a0*b1 + b0*f1 + c0*j1 + d0*n1
  , a0*c1 + b0*g1 + c0*k1 + d0*o1
  , a0*d1 + b0*h1 + c0*l1 + d0*p1

  , e0*a1 + f0*e1 + g0*i1 + h0*m1
  , e0*b1 + f0*f1 + g0*j1 + h0*n1
  , e0*c1 + f0*g1 + g0*k1 + h0*o1
  , e0*d1 + f0*h1 + g0*l1 + h0*p1

  , i0*a1 + j0*e1 + k0*i1 + l0*m1
  , i0*b1 + j0*f1 + k0*j1 + l0*n1
  , i0*c1 + j0*g1 + k0*k1 + l0*o1
  , i0*d1 + j0*h1 + k0*l1 + l0*p1

  , m0*a1 + n0*e1 + o0*i1 + p0*m1
  , m0*b1 + n0*f1 + o0*j1 + p0*n1
  , m0*c1 + n0*g1 + o0*k1 + p0*o1
  , m0*d1 + n0*h1 + o0*l1 + p0*p1
  )

def primal2 [n] (xs: [n](f32,f32,f32,f32,f32,f32,f32,f32,f32,f32,f32,f32,f32,f32,f32,f32)) =
  scan mm4by4 (1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1) xs

def fromarrs2 = map (\(x: [16]f32) -> (x[0],x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10],x[11],x[12],x[13],x[14],x[15]))
def toarrs2 = map (\(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) -> [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p])

def onehot_2d n m x y =
  tabulate_2d n m (\i j -> f32.bool((i,j) == (x,y)))

entry fwd [n] (input: [n][16]f32) : [n][16][n][16]f32 =
  let input = fromarrs2 input
  in tabulate (n*16) (\i -> jvp primal2 input (fromarrs2 (onehot_2d n 16 (i/16) (i%16))))
     |> map toarrs2 |> transpose |> map transpose |> map (map unflatten)

entry rev [n] (input: [n][16]f32) : [n][16][n][16]f32 =
  let input = fromarrs2 input
  in tabulate (n*16) (\i -> vjp primal2 input (fromarrs2 (onehot_2d n 16 (i/16) (i%16))))
     |> unflatten |> map (map toarrs2)
