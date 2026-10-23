Complex :: struct {
    re: float = 1.0;
    im: float = 0.0;
}

Quat :: struct {
    x, y, z: float;
    w: float = 1.0;

    #overlay (x) xyz: Vec3;
    #overlay (x) im: Vec3;
    #overlay (w) re: float;
    #overlay (x) e: [4] float;
}

IDENTITY_QUAT :: Quat.{};
IDENTITY_COMPLEX :: Complex.{};

#scope_module
ComplexTypes :: Type.[Complex, Quat];

#scope_export
dot :: inline (a: Complex, b: Complex) -> float { return a.re*b.re + a.im*b.im; }
dot :: inline (a: Quat, b: Quat) -> float { return a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w; }

length :: inline (a: $T/ComplexTypes) -> float { return sqrt(dot(a, a)); }
length_squared :: inline (a: $T/ComplexTypes) -> float { return dot(a, a); }
normalize :: inline (a: $T/ComplexTypes) -> T { return a / length(a); }

normalize_safe :: inline (a: $T/ComplexTypes) -> T {
    length_squared := dot(a, a);
    if length_squared <= DEFAULT_EPSILON_SQUARED return T.{};
    return a / sqrt(length_squared);
}

// Complex operators
operator + :: inline (a: Complex, b: Complex) -> Complex { return .{a.re+b.re, a.im+b.im}; }
operator - :: inline (a: Complex, b: Complex) -> Complex { return .{a.re-b.re, a.im-b.im}; }
operator - :: inline (a: Complex)             -> Complex { return .{-a.re, -a.im}; }
operator * :: inline (a: Complex, s: float)   -> Complex { return .{a.re*s, a.im*s}; }
operator / :: inline (a: Complex, s: float)   -> Complex { return a * (1.0 / s); }

operator * :: inline (a: Complex, b: Complex) -> Complex {
    r: Complex = ---;

    r.re = a.re * b.re - a.im * b.im;
    r.im = a.re * b.im + a.im * b.re;

    return r;
}

// Quat operators
operator + :: inline (a: Quat, b: Quat)  -> Quat { return .{a.x+b.x, a.y+b.y, a.z+b.z, a.w+b.w}; }
operator - :: inline (a: Quat, b: Quat)  -> Quat { return .{a.x-b.x, a.y-b.y, a.z-b.z, a.w-b.w}; }
operator - :: inline (a: Quat)           -> Quat { return .{-a.x, -a.y, -a.z, -a.w}; }
operator * :: inline (a: Quat, s: float) -> Quat #symmetric { return .{a.x*s, a.y*s, a.z*s, a.w*s}; }
operator / :: inline (a: Quat, s: float) -> Quat { return a * (1.0 / s); }

operator * :: inline (a: Quat, b: Quat) -> Quat {
    r: Quat = ---;

    r.w = a.w * b.w - dot(a.xyz, b.xyz);
    r.xyz = a.w * b.xyz + b.w * a.xyz + cross(a.xyz, b.xyz);

    return r;
}

// Conjugate operation
conjugate :: (q: Complex) -> Complex {
    r: Complex = ---;

    r.im = -q.im;
    r.re = q.re;

    return r;
}

conjugate :: (q: Quat) -> Quat {
    r: Quat = ---;

    r.im = -q.im;
    r.w = q.w;

    return r;
}

// Inverse operation
inverse :: (q: Complex) -> Complex { return (inline conjugate(q)) / dot(q, q); }
inverse :: (q: Quat) -> Quat { return (inline conjugate(q)) / dot(q, q); }

// Lerp
lerp :: inline (a: Complex, b: Complex, t: float) -> Complex { return a + (b - a) * t; }
lerp :: inline (a: Quat, b: Quat, t: float) -> Quat { return a + (b - a) * t; }

// Slerp 2D
slerp_unclamped :: (a: Complex, b: Complex, t: float) -> Complex {
    return slerp_unclamped_unsafe(normalize_safe(a), normalize_safe(b), t);
}

slerp_unclamped_unsafe :: (a: Complex, b: Complex, t: float) -> Complex {
    delta := b * conjugate(a);
    angle := atan2(delta.im, delta.re);
    return normalize_safe(complex_from_angle(angle * t) * a);
}

// Slerp 3D
slerp_unclamped :: (a: Quat, b: Quat, t: float) -> Quat {
    return slerp_unclamped_unsafe(normalize_safe(a), normalize_safe(b), t);
}

slerp_unclamped_unsafe :: (a: Quat, b: Quat, t: float) -> Quat {
    finish := b;
    cosine := dot(a, finish);
    if cosine < 0 {
        finish = -finish;
        cosine = -cosine;
    }

    // NLERP avoids dividing by a small sin(angle). Its linear blend must be normalized.
    if cosine > 0.9995 return normalize_safe(lerp(a, finish, t));

    angle := acos(clamp01(cosine));
    inverse_sine := 1 / sin(angle);

    // Exact SLERP is unit length; normalization removes floating-point length error.
    return normalize_safe(a * (sin((1 - t) * angle) * inverse_sine) + finish * (sin(t * angle) * inverse_sine));
}

// Rotate vector with complex number
rotate :: inline (v: Vec2, c: Complex) -> Vec2 {
    result := c * Complex.{v.x, v.y};
    return .{result.re, result.im};
}

operator * :: inline (c: Complex, v: Vec2) -> Vec2 { return rotate(v, c); }

// Rotate vector with quaternion
rotate :: inline (v: Vec3, q: Quat) -> Vec3 {
    // https://fgiesen.wordpress.com/2019/02/09/rotating-a-single-vector-using-a-quaternion/

    t := cross(q.xyz, v) * 2.0;

    return v + t * q.w + cross(q.xyz, t);
}

operator * :: inline (q: Quat, v: Vec3) -> Vec3 {
    return rotate(v, q);
}

rotated_x_axis :: inline (c: Complex) -> Vec2 { return .{c.re, c.im}; }
rotated_y_axis :: inline (c: Complex) -> Vec2 { return .{-c.im, c.re}; }

rotated_x_axis :: inline (q: Quat) -> Vec3 { return rotate(Vec3.{1, 0, 0}, q); }
rotated_y_axis :: inline (q: Quat) -> Vec3 { return rotate(Vec3.{0, 1, 0}, q); }
rotated_z_axis :: inline (q: Quat) -> Vec3 { return rotate(Vec3.{0, 0, 1}, q); }

to_vector :: inline (c: Complex) -> Vec2 { return rotated_x_axis(c); }
to_vector :: inline (q: Quat) -> Vec3 { return rotated_z_axis(q); }

complex_from_angle :: (angle: float) -> Complex { return .{cos(angle), sin(angle)}; }
complex_to_angle :: (using complex: Complex) -> float { return atan2(im, re); }

// Rotation matrix with rotated basis axes in its columns. Quaternion must be normalized.
to_matrix3 :: (q: Quat) -> Mat3 {
    Basic.assert(values_are_close(length(q), 1.0));

    m: Mat3 = ---;

    x2 := q.x * q.x;
    y2 := q.y * q.y;
    z2 := q.z * q.z;
    xy := q.x * q.y;
    xz := q.x * q.z;
    yz := q.y * q.z;
    wx := q.w * q.x;
    wy := q.w * q.y;
    wz := q.w * q.z;

    m._00 = 1.0 - 2.0*(y2 + z2);
    m._01 = 2.0*(xy - wz);
    m._02 = 2.0*(xz + wy);

    m._10 = 2.0*(xy + wz);
    m._11 = 1.0 - 2.0*(x2 + z2);
    m._12 = 2.0*(yz - wx);

    m._20 = 2.0*(xz - wy);
    m._21 = 2.0*(yz + wx);
    m._22 = 1.0 - 2.0*(x2 + y2);

    return m;
}

// Homogeneous rotation matrix (quaternion must be normalized).
to_matrix :: (q: Quat) -> Mat4 {
    basis := to_matrix3(q);
    return .{_00 = basis._00, _01 = basis._01, _02 = basis._02,
             _10 = basis._10, _11 = basis._11, _12 = basis._12,
             _20 = basis._20, _21 = basis._21, _22 = basis._22,
             _33 = 1};
}
