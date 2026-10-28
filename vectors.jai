Vec2 :: struct {
    x, y: float;

    #overlay (x) e: [2] float;
}

Vec3 :: struct {
    x, y, z: float;

    #overlay (x) xy: Vec2;
    #overlay (x) e: [3] float;
}

Vec4 :: struct {
    x, y, z, w: float;

    #overlay (x) xy: Vec2;
    #overlay (x) xyz: Vec3;
    #overlay (x) e: [4] float;

    #overlay (x) r: float;
    #overlay (y) g: float;
    #overlay (z) b: float;
    #overlay (w) a: float;
    #overlay (x) rgb: Vec3;
}

valid_vector :: inline (v: Vec2) -> bool {
    return is_finite(v.x) && is_finite(v.y);
}

valid_vector :: inline (v: Vec3) -> bool {
    return is_finite(v.x) && is_finite(v.y) && is_finite(v.z);
}

valid_vector :: inline (v: Vec4) -> bool {
    return is_finite(v.x) && is_finite(v.y) && is_finite(v.z) && is_finite(v.w);
}

// Make operations
xy :: (x: float, y: float) -> Vec2 { return .{x, y}; }
xy :: (s: float) -> Vec2 { return .{s, s}; }

xyz :: (x: float, y: float, z: float) -> Vec3 { return .{x, y, z}; }
xyz :: (s: float) -> Vec3 { return .{s, s, s}; }

xyzw :: (x: float, y: float, z: float, w: float) -> Vec4 { return .{x, y, z, w}; }
xyzw :: (using xy: Vec2, z: float, w: float) -> Vec4 { return .{x, y, z, w}; }
xyzw :: (using xyz: Vec3, w: float) -> Vec4 { return .{x, y, z, w}; }
xyzw :: (s: float) -> Vec4 { return .{s, s, s, s}; }

// Dot Product
dot :: inline (a: Vec2, b: Vec2) -> float { return a.x*b.x + a.y*b.y; }
dot :: inline (a: Vec3, b: Vec3) -> float { return a.x*b.x + a.y*b.y + a.z*b.z; }
dot :: inline (a: Vec4, b: Vec4) -> float { return a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w; }

// Length
length :: inline (a: $T/MyVectors) -> float { return sqrt(dot(a, a)); }
length_squared :: inline (a: $T/MyVectors) -> float { return dot(a, a); }

// Distance
distance :: inline (a: $T/MyVectors, b: T) -> float { return length(a - b); }
distance_squared :: inline (a: $T/MyVectors, b: T) -> float { return length_squared(a - b); }

// Abs
abs :: inline (a: Vec2) -> Vec2 { return .{abs(a.x), abs(a.y)}; }
abs :: inline (a: Vec3) -> Vec3 { return .{abs(a.x), abs(a.y), abs(a.z)}; }
abs :: inline (a: Vec4) -> Vec4 { return .{abs(a.x), abs(a.y), abs(a.z), abs(a.w)}; }

min :: inline (a: Vec2, b: Vec2) -> Vec2 { return .{ Basic.min(a.x, b.x), Basic.min(a.y, b.y) }; }
min :: inline (a: Vec3, b: Vec3) -> Vec3 { return .{ Basic.min(a.x, b.x), Basic.min(a.y, b.y), Basic.min(a.z, b.z) }; }
min :: inline (a: Vec4, b: Vec4) -> Vec4 { return .{ Basic.min(a.x, b.x), Basic.min(a.y, b.y), Basic.min(a.z, b.z), Basic.min(a.w, b.w) }; }

max :: inline (a: Vec2, b: Vec2) -> Vec2 { return .{ Basic.max(a.x, b.x), Basic.max(a.y, b.y) }; }
max :: inline (a: Vec3, b: Vec3) -> Vec3 { return .{ Basic.max(a.x, b.x), Basic.max(a.y, b.y), Basic.max(a.z, b.z) }; }
max :: inline (a: Vec4, b: Vec4) -> Vec4 { return .{ Basic.max(a.x, b.x), Basic.max(a.y, b.y), Basic.max(a.z, b.z), Basic.max(a.w, b.w) }; }

// Vec2 operators
operator + :: inline (a: Vec2, b: Vec2)  -> Vec2 { return .{a.x+b.x, a.y+b.y}; }
operator - :: inline (a: Vec2, b: Vec2)  -> Vec2 { return .{a.x-b.x, a.y-b.y}; }
operator - :: inline (a: Vec2)           -> Vec2 { return .{-a.x, -a.y}; }
operator * :: inline (a: Vec2, b: Vec2)  -> Vec2 { return .{a.x*b.x, a.y*b.y}; }
operator * :: inline (a: Vec2, s: float) -> Vec2 #symmetric { return .{a.x*s, a.y*s}; }
operator / :: inline (a: Vec2, s: float) -> Vec2 { return a * (1.0 / s); }
operator / :: inline (s: float, a: Vec2) -> Vec2 { return .{s/a.x, s/a.y}; }

// Vec3 operators
operator + :: inline (a: Vec3, b: Vec3)  -> Vec3 { return .{a.x+b.x, a.y+b.y, a.z+b.z}; }
operator - :: inline (a: Vec3, b: Vec3)  -> Vec3 { return .{a.x-b.x, a.y-b.y, a.z-b.z}; }
operator - :: inline (a: Vec3)           -> Vec3 { return .{-a.x, -a.y, -a.z}; }
operator * :: inline (a: Vec3, b: Vec3)  -> Vec3 { return .{a.x*b.x, a.y*b.y, a.z*b.z}; }
operator * :: inline (a: Vec3, s: float) -> Vec3 #symmetric { return .{a.x*s, a.y*s, a.z*s}; }
operator / :: inline (a: Vec3, s: float) -> Vec3 { return a * (1.0 / s); }
operator / :: inline (s: float, a: Vec3) -> Vec3 { return .{s/a.x, s/a.y, s/a.z}; }

// Vec4 operators
operator + :: inline (a: Vec4, b: Vec4)  -> Vec4 { return .{a.x+b.x, a.y+b.y, a.z+b.z, a.w+b.w}; }
operator - :: inline (a: Vec4, b: Vec4)  -> Vec4 { return .{a.x-b.x, a.y-b.y, a.z-b.z, a.w-b.w}; }
operator - :: inline (a: Vec4)           -> Vec4 { return .{-a.x, -a.y, -a.z, -a.w}; }
operator * :: inline (a: Vec4, b: Vec4)  -> Vec4 { return .{a.x*b.x, a.y*b.y, a.z*b.z, a.w*b.w}; }
operator * :: inline (a: Vec4, s: float) -> Vec4 #symmetric { return .{a.x*s, a.y*s, a.z*s, a.w*s}; }
operator / :: inline (a: Vec4, s: float) -> Vec4 { return a * (1.0 / s); }
operator / :: inline (s: float, a: Vec4) -> Vec4 { return .{s/a.x, s/a.y, s/a.z, s/a.w}; }

// Lerp operations
lerp :: inline (a: $R/.[Vec2, Vec3, Vec4], b: R, t: float) -> R { return a + (b - a) * t; }

// Clamp operations
clamp :: inline (a: Vec2, min: Vec2, max: Vec2) -> Vec2 {
    return .{
        Basic.clamp(a.x, min.x, max.x),
        Basic.clamp(a.y, min.y, max.y)};
}

clamp :: inline (a: Vec3, min: Vec3, max: Vec3) -> Vec3 {
    return .{
        Basic.clamp(a.x, min.x, max.x),
        Basic.clamp(a.y, min.y, max.y),
        Basic.clamp(a.z, min.z, max.z)};
}

clamp :: inline (a: Vec4, min: Vec4, max: Vec4) -> Vec4 {
    return .{
        Basic.clamp(a.x, min.x, max.x),
        Basic.clamp(a.y, min.y, max.y),
        Basic.clamp(a.z, min.z, max.z),
        Basic.clamp(a.w, min.w, max.w)};
}

// Saturate operations
clamp01 :: inline (a: Vec2) -> Vec2 { return .{clamp01(a.x), clamp01(a.y)}; }
clamp01 :: inline (a: Vec3) -> Vec3 { return .{clamp01(a.x), clamp01(a.y), clamp01(a.z)}; }
clamp01 :: inline (a: Vec4) -> Vec4 { return .{clamp01(a.x), clamp01(a.y), clamp01(a.z), clamp01(a.w)}; }

// Normalize operations
normalize :: inline (v: $T/MyVectors) -> T { return v / length(v); }

normalize_safe :: inline (v: $T/MyVectors, tolerance := DEFAULT_EPSILON, fallback := T.{}) -> T {
    v_len := length(v);

    if v_len <= tolerance return fallback;

    return v / v_len;
}

normalize_safe_2d :: inline (v: Vec3, tolerance := DEFAULT_EPSILON) -> Vec3 {
    v.y = 0;
    return normalize_safe(v, tolerance);
}

zero_y :: inline (v: Vec3) -> Vec3 {
    v.y = 0;
    return v;
}

normalize_and_get_length :: inline (v: $T/MyVectors) -> (T, float) { len := length(v); return v / len, len; }

normalize_safe_and_get_length :: inline (v: $T/MyVectors, tolerance := DEFAULT_EPSILON, fallback := T.{}) -> (T, float) {
    v_len := length(v);

    if v_len <= tolerance return fallback, v_len;

    return v / v_len, v_len;
}

// Perp
perp :: inline (a: Vec2) -> Vec2 { return .{-a.y, a.x}; }

// Cross
cross :: inline (a: Vec2, b: Vec2) -> float {
    return a.x*b.y - a.y*b.x;
}

cross :: inline (a: Vec3, b: Vec3) -> Vec3 {
    r: Vec3 = ---;

    r.x = a.y * b.z - a.z * b.y;
    r.y = a.z * b.x - a.x * b.z;
    r.z = a.x * b.y - a.y * b.x;

    return r;
}

// Angles (radians). a and b must be nonzero; unsafe variants require unit vectors.
unsigned_angle :: inline (a: Vec3, b: Vec3) -> float {
    return unsigned_angle_unsafe(normalize(a), normalize(b));
}

unsigned_angle_unsafe :: inline (a: Vec3, b: Vec3) -> float {
    return acos(Basic.clamp(dot(a, b), -1.0, 1.0));
}

signed_angle :: inline (a: Vec3, b: Vec3, up: Vec3) -> float {
    return signed_angle_unsafe(normalize(a), normalize(b), up);
}

signed_angle_unsafe :: inline (a: Vec3, b: Vec3, up: Vec3) -> float {
    return unsigned_angle_unsafe(a, b) * sign(dot(up, cross(a, b)));
}

// Project & Reject
project :: (a: Vec3, b: Vec3) -> Vec3 {
    return (dot(a, b) / dot(b, b)) * b;
}

project_unit :: (a: Vec3, b: Vec3) -> Vec3 {
    return dot(a, b) * b;
}

reject :: (a: Vec3, b: Vec3) -> Vec3 {
    return a - (dot(a, b) / dot(b, b)) * b;
}

reject_unit :: (a: Vec3, b: Vec3) -> Vec3 {
    return a - dot(a, b) * b;
}

// Weights correspond to p0, p1, p2: point = u*p0 + v*p1 + w*p2, u+v+w = 1.
// Signed weights are not clamped; points outside the triangle may have negative weights.
// Triangles must be nondegenerate. The 3D overload orthogonally projects onto the plane.
barycentric_coordinates :: (p0: Vec2, p1: Vec2, p2: Vec2, test_point: Vec2) -> (float, float, float) {
    edge1 := p1 - p0;
    edge2 := p2 - p0;
    offset := test_point - p0;
    determinant := cross(edge1, edge2);

    Basic.assert(is_finite(determinant) && determinant != 0.0, "Degenerate barycentric triangle!");

    v := cross(offset, edge2) / determinant;
    w := cross(edge1, offset) / determinant;
    return 1.0 - v - w, v, w;
}

barycentric_coordinates :: (p0: Vec3, p1: Vec3, p2: Vec3, test_point: Vec3) -> (float, float, float) {
    normal := cross(p1 - p0, p2 - p0);
    normal_length_squared := length_squared(normal);

    Basic.assert(is_finite(normal_length_squared) && normal_length_squared > 0.0, "Degenerate barycentric triangle!");

    projected := p0 + reject(test_point - p0, normal);
    u, v, w := barycentric_coordinates_unsafe(p0, p1, p2, projected);
    return u, v, w;
}

// test_point must already lie on the triangle plane; no orthogonal projection is performed.
barycentric_coordinates_unsafe :: (p0: Vec3, p1: Vec3, p2: Vec3, test_point: Vec3) -> (float, float, float) {
    normal := cross(p1 - p0, p2 - p0);

    // Drop the dominant normal axis to obtain the largest projected triangle area.
    if abs(normal.x) >= abs(normal.y) && abs(normal.x) >= abs(normal.z) {
        u, v, w := barycentric_coordinates(Vec2.{p0.y, p0.z},
                                          Vec2.{p1.y, p1.z},
                                          Vec2.{p2.y, p2.z},
                                          Vec2.{test_point.y, test_point.z});
        return u, v, w;
    }

    if abs(normal.y) >= abs(normal.z) {
        u, v, w := barycentric_coordinates(Vec2.{p0.x, p0.z},
                                          Vec2.{p1.x, p1.z},
                                          Vec2.{p2.x, p2.z},
                                          Vec2.{test_point.x, test_point.z});
        return u, v, w;
    }

    u, v, w := barycentric_coordinates(p0.xy, p1.xy, p2.xy, test_point.xy);
    return u, v, w;
}

// Scale inverse
scale_inverse_safe :: inline (scale: float) -> float {
    return ifx abs(scale) > 0.0 then 1.0 / scale else 0.0;
}

scale_inverse_safe :: inline (scale: Vec2) -> Vec2 {
    result_scale: Vec2 = ---;

    result_scale.x = scale_inverse_safe(scale.x);
    result_scale.y = scale_inverse_safe(scale.y);

    return result_scale;
}

scale_inverse_safe :: inline (scale: Vec3) -> Vec3 {
    result_scale: Vec3 = ---;

    result_scale.x = scale_inverse_safe(scale.x);
    result_scale.y = scale_inverse_safe(scale.y);
    result_scale.z = scale_inverse_safe(scale.z);

    return result_scale;
}

#scope_module
MyVectors :: Type.[Vec2, Vec3, Vec4];
