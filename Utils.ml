let valOrder (x, y) = if x < y then (x, y) else (y, x)
let keyOrder ((k1, v1), (k2, v2)) = if v1 < v2 then (k1, k2) else (k2, k1)
