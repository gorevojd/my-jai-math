/*
    In my implementation - matrices are stored in memory in Row Major format.
    However, the transformation vectors that build up a transformation matrix are written in columns.
    The reason for this is to do calculations in mathematics kinda style way.
*/

#scope_module
MatricesTypes :: Type.[Mat2, Mat3, Mat4];

#scope_export

Mat2 :: struct {
    _00, _01 : float; // Row 1
    _10, _11 : float; // Row 2

    #overlay (_00) rows: [2] Vec2;
    #overlay (_00) e: [4] float;
    #overlay (_00) e2: [2][2] float;

    Row_Type :: Vec2;
    Column_Type :: Vec2;

    IsMyMatrix :: true;
}

Mat2_Identity :: Mat2.{_00=1, _11=1};

Mat3 :: struct {
    _00, _01, _02 : float; // Row 1
    _10, _11, _12 : float; // Row 2
    _20, _21, _22 : float; // Row 3

    #overlay (_00) rows: [3] Vec3;
    #overlay (_00) e: [9] float;
    #overlay (_00) e2: [3][3] float;

    Row_Type :: Vec3;
    Column_Type :: Vec3;

    IsMyMatrix :: true;
}

Mat3_Identity :: Mat3.{_00=1, _11=1, _22=1};

Mat4 :: struct {
    _00, _01, _02, _03 : float; // Row 1
    _10, _11, _12, _13 : float; // Row 2
    _20, _21, _22, _23 : float; // Row 3
    _30, _31, _32, _33 : float; // Row 4

    #overlay (_00) rows: [4] Vec4;
    #overlay (_00) e: [16] float;
    #overlay (_00) e2: [4][4] float;

    Row_Type :: Vec4;
    Column_Type :: Vec4;

    IsMyMatrix :: true;
}

Mat4_Identity :: Mat4.{_00=1, _11=1, _22=1, _33=1};

#scope_module

AnyMat3 :: struct {
    _00, _01, _02 : float; // Row 1
    _10, _11, _12 : float; // Row 2
    _20, _21, _22 : float; // Row 3
};

AnyMat :: struct {
    IsMyMatrix :: true;
};

#scope_export

// Make operations
make_from_rows :: inline (r0: Vec2, r1: Vec2) -> Mat2 {
    r: Mat2 = ---;

    r.rows[0] = r0;
    r.rows[1] = r1;

    return r;
}

make_from_columns :: inline (c0: Vec2, c1: Vec2) -> Mat2 {
    r: Mat2 = ---;

    r.rows[0] = .{c0.x, c1.x};
    r.rows[1] = .{c0.y, c1.y};

    return r;
}

make_from_rows :: inline (r0: Vec3, r1: Vec3, r2: Vec3) -> Mat3 {
    r: Mat3 = ---;

    r.rows[0] = r0;
    r.rows[1] = r1;
    r.rows[2] = r2;

    return r;
}

make_from_columns :: inline (c0: Vec3, c1: Vec3, c2: Vec3) -> Mat3 {
    r: Mat3 = ---;

    r.rows[0] = .{c0.x, c1.x, c2.x};
    r.rows[1] = .{c0.y, c1.y, c2.y};
    r.rows[2] = .{c0.z, c1.z, c2.z};

    return r;
}

make_from_rows :: inline (r0: Vec4, r1: Vec4, r2: Vec4, r3: Vec4) -> Mat4 {
    r: Mat4 = ---;

    r.rows[0] = r0;
    r.rows[1] = r1;
    r.rows[2] = r2;
    r.rows[3] = r3;

    return r;
}

make_from_columns :: inline (c0: Vec4, c1: Vec4, c2: Vec4, c3: Vec4) -> Mat4 {
    r: Mat4 = ---;

    r.rows[0] = .{c0.x, c1.x, c2.x, c3.x};
    r.rows[1] = .{c0.y, c1.y, c2.y, c3.y};
    r.rows[2] = .{c0.z, c1.z, c2.z, c3.z};
    r.rows[3] = .{c0.w, c1.w, c2.w, c3.w};

    return r;
}

// Matrix by vector multiply functions
multiply :: (m: Mat2, v: Vec2) -> Vec2 {
    r: Vec2 = ---;

    r.x = m._00*v.x + m._01*v.y;
    r.y = m._10*v.x + m._11*v.y;

    return r;
}

multiply :: (m: Mat3, v: Vec3) -> Vec3 {
    r: Vec3 = ---;

    r.x = m._00*v.x + m._01*v.y + m._02*v.z;
    r.y = m._10*v.x + m._11*v.y + m._12*v.z;
    r.z = m._20*v.x + m._21*v.y + m._22*v.z;

    return r;
}

multiply :: (m: Mat4, v: Vec4) -> Vec4 {
    r: Vec4 = ---;

    r.x = m._00*v.x + m._01*v.y + m._02*v.z + m._03*v.w;
    r.y = m._10*v.x + m._11*v.y + m._12*v.z + m._13*v.w;
    r.z = m._20*v.x + m._21*v.y + m._22*v.z + m._23*v.w;
    r.w = m._30*v.x + m._31*v.y + m._32*v.z + m._33*v.w;

    return r;
}

multiply_direction :: (m: Mat4, v: Vec3) -> Vec3 {
    r: Vec3 = ---;

    r.x = m._00*v.x + m._01*v.y + m._02*v.z;
    r.y = m._10*v.x + m._11*v.y + m._12*v.z;
    r.z = m._20*v.x + m._21*v.y + m._22*v.z;

    return r;
}

// Matrix by vector multiply operators
operator * :: (m: Mat2, v: Vec2) -> Vec2 { return inline multiply(m, v); }
operator * :: (m: Mat3, v: Vec3) -> Vec3 { return inline multiply(m, v); }
operator * :: (m: Mat4, v: Vec4) -> Vec4 { return inline multiply(m, v); }

// Matrix multiplication
multiply :: (a: Mat2, b: Mat2) -> Mat2 #no_alias {
    r: Mat2 = ---;

    r._00 = a._00*b._00 + a._01*b._10;
    r._01 = a._00*b._01 + a._01*b._11;

    r._10 = a._10*b._00 + a._11*b._10;
    r._11 = a._10*b._01 + a._11*b._11;

    return r;
}

multiply :: (a: Mat3, b: Mat3) -> Mat3 #no_alias {
    r: Mat3 = ---;

    r._00 = a._00*b._00 + a._01*b._10 + a._02*b._20;
    r._01 = a._00*b._01 + a._01*b._11 + a._02*b._21;
    r._02 = a._00*b._02 + a._01*b._12 + a._02*b._22;

    r._10 = a._10*b._00 + a._11*b._10 + a._12*b._20;
    r._11 = a._10*b._01 + a._11*b._11 + a._12*b._21;
    r._12 = a._10*b._02 + a._11*b._12 + a._12*b._22;

    r._20 = a._20*b._00 + a._21*b._10 + a._22*b._20;
    r._21 = a._20*b._01 + a._21*b._11 + a._22*b._21;
    r._22 = a._20*b._02 + a._21*b._12 + a._22*b._22;

    return r;
}

multiply :: (a: Mat4, b: Mat4) -> Mat4 #no_alias {
    r: Mat4 = ---;

    r._00 = a._00*b._00 + a._01*b._10 + a._02*b._20 + a._03*b._30;
    r._01 = a._00*b._01 + a._01*b._11 + a._02*b._21 + a._03*b._31;
    r._02 = a._00*b._02 + a._01*b._12 + a._02*b._22 + a._03*b._32;
    r._03 = a._00*b._03 + a._01*b._13 + a._02*b._23 + a._03*b._33;

    r._10 = a._10*b._00 + a._11*b._10 + a._12*b._20 + a._13*b._30;
    r._11 = a._10*b._01 + a._11*b._11 + a._12*b._21 + a._13*b._31;
    r._12 = a._10*b._02 + a._11*b._12 + a._12*b._22 + a._13*b._32;
    r._13 = a._10*b._03 + a._11*b._13 + a._12*b._23 + a._13*b._33;

    r._20 = a._20*b._00 + a._21*b._10 + a._22*b._20 + a._23*b._30;
    r._21 = a._20*b._01 + a._21*b._11 + a._22*b._21 + a._23*b._31;
    r._22 = a._20*b._02 + a._21*b._12 + a._22*b._22 + a._23*b._32;
    r._23 = a._20*b._03 + a._21*b._13 + a._22*b._23 + a._23*b._33;

    r._30 = a._30*b._00 + a._31*b._10 + a._32*b._20 + a._33*b._30;
    r._31 = a._30*b._01 + a._31*b._11 + a._32*b._21 + a._33*b._31;
    r._32 = a._30*b._02 + a._31*b._12 + a._32*b._22 + a._33*b._32;
    r._33 = a._30*b._03 + a._31*b._13 + a._32*b._23 + a._33*b._33;

    return r;
}

operator * :: (a: $T/MatricesTypes, b: T) -> T { return inline multiply(a, b); }

operator * :: (a: $T/interface AnyMat, s: float) -> T #symmetric  {
    r: T = ---;

    for 0..a.e.count-1 {
        r.e[it] = a.e[it] * s;
    }

    return r;
}

// Transpose operations
transpose :: (m: Mat2) -> Mat2 {
    r: Mat2 = ---;

    r._00 = m._00;
    r._11 = m._11;
    r._01 = m._10;
    r._10 = m._01;

    return r;
}

transpose :: (m: Mat3) -> Mat3 {
    r: Mat3 = ---;

    for x: 0..2 for y: 0..2 {
        r.e2[x][y] = m.e2[y][x];
    }

    return r;
}

transpose :: (m: Mat4) -> Mat4 {
    r: Mat4 = ---;

    for x: 0..3 for y: 0..3 {
        r.e2[x][y] = m.e2[y][x];
    }

    return r;
}

// Determinant
determinant :: inline (m: Mat3) -> float {
    return + m._00 * (m._11*m._22 - m._21*m._12)
           - m._01 * (m._10*m._22 - m._20*m._12)
           + m._02 * (m._10*m._21 - m._20*m._11);
}

// Inverse operations
inverse :: inline (m: Mat2, epsilon := DEFAULT_EPSILON) -> (success: bool, result: Mat2) {
    determinant := m._00*m._11 - m._01*m._10;
    if abs(determinant) <= epsilon {
        return false, Mat2_Identity;
    }

    inv_det := 1.0 / determinant;

    return true, Mat2.{inv_det * m._11, inv_det * -m._01,
                       inv_det * -m._10, inv_det * m._00};
}

inverse :: (m: Mat3, epsilon := DEFAULT_EPSILON) -> (success: bool, result: Mat3) {
    a := Vec3.{m._00, m._10, m._20};
    b := Vec3.{m._01, m._11, m._21};
    c := Vec3.{m._02, m._12, m._22};

    r0 := cross(b, c);
    r1 := cross(c, a);
    r2 := cross(a, b);

    determinant := dot(r2, c);
    if abs(determinant) <= epsilon {
        return false, Mat3_Identity;
    }

    inv_det := 1.0 / determinant;

    return true, Mat3.{inv_det * r0.x, inv_det * r0.y, inv_det * r0.z,
                       inv_det * r1.x, inv_det * r1.y, inv_det * r1.z,
                       inv_det * r2.x, inv_det * r2.y, inv_det * r2.z};
}

inverse :: (m: Mat4, epsilon := DEFAULT_EPSILON) -> (success: bool, result: Mat4) {
    a := Vec3.{m._00, m._10, m._20};
    b := Vec3.{m._01, m._11, m._21};
    c := Vec3.{m._02, m._12, m._22};
    d := Vec3.{m._03, m._13, m._23};

    x := m._30;
    y := m._31;
    z := m._32;
    w := m._33;

    s := cross(a, b);
    t := cross(c, d);
    u := a*y - b*x;
    v := c*w - d*z;

    determinant := dot(s,v) + dot(t, u);
    if abs(determinant) <= epsilon {
        return false, Mat4_Identity;
    }

    inv_det := 1.0 / determinant;
    s *= inv_det;
    t *= inv_det;
    u *= inv_det;
    v *= inv_det;

    r0 := cross(b, v) + t * y;
    r1 := cross(v, a) - t * x;
    r2 := cross(d, u) + s * w;
    r3 := cross(u, c) - s * z;

    return true, .{r0.x, r0.y, r0.z, -dot(b, t),
                   r1.x, r1.y, r1.z, dot(a, t),
                   r2.x, r2.y, r2.z, -dot(d, s),
                   r3.x, r3.y, r3.z, dot(c, s)};
}

// .....................
// XYZ Rotation matrices
// .....................
#scope_module
rotation_x_internal :: inline (m: *$T/interface AnyMat3, angle: float) {
    m._00 = 1.0;
    m._01 = 0.0;
    m._02 = 0.0;

    m._10 = 0.0;
    m._11 = cos(angle);
    m._12 = -sin(angle);

    m._20 = 0.0;
    m._21 = sin(angle);
    m._22 = cos(angle);
}

rotation_y_internal :: inline (m: *$T/interface AnyMat3, angle: float) {
    m._00 = cos(angle);
    m._01 = 0.0;
    m._02 = sin(angle);

    m._10 = 0.0;
    m._11 = 1.0;
    m._12 = 0.0;

    m._20 = -sin(angle);
    m._21 = 0.0;
    m._22 = cos(angle);
}

rotation_z_internal :: inline (m: *$T/interface AnyMat3, angle: float) {
    m._00 = cos(angle);
    m._01 = -sin(angle);
    m._02 = 0.0;

    m._10 = sin(angle);
    m._11 = cos(angle);
    m._12 = 0.0;

    m._20 = 0.0;
    m._21 = 0.0;
    m._22 = 1.0;
}

#scope_export
rotation_mat2 :: (angle: float) -> Mat2 {
    r: Mat2 = ---;

    s := sin(angle);
    c := cos(angle);

    r._00 = c;
    r._01 = -s;
    r._10 = s;
    r._11 = c;

    return r;
}

rotation_x_mat3 :: (angle: float) -> Mat3 {
    r: Mat3 = ---;

    rotation_x_internal(*r, angle);

    return r;
}

rotation_x_mat4 :: (angle: float) -> Mat4 {
    r: Mat4;

    rotation_x_internal(*r, angle);

    r._33 = 1.0;

    return r;
}

rotation_y_mat3 :: (angle: float) -> Mat3 {
    r: Mat3 = ---;

    rotation_y_internal(*r, angle);

    return r;
}

rotation_y_mat4 :: (angle: float) -> Mat4 {
    r: Mat4;

    rotation_y_internal(*r, angle);

    r._33 = 1.0;

    return r;
}

rotation_z_mat3 :: (angle: float) -> Mat3 {
    r: Mat3 = ---;

    rotation_z_internal(*r, angle);

    return r;
}

rotation_z_mat4 :: (angle: float) -> Mat4 {
    r: Mat4;

    rotation_z_internal(*r, angle);

    r._33 = 1.0;

    return r;
}

rotation_x :: rotation_x_mat4;
rotation_y :: rotation_y_mat4;
rotation_z :: rotation_z_mat4;

// ...................
// Axis-Angle rotation
// ...................
#scope_module
axis_angle_rotation_internal :: inline (m: *$T/interface AnyMat3, axis: Vec3, angle: float) {
    c := cos(angle);
    s := sin(angle);
    d := 1.0 - c;

    x := axis.x * d;
    y := axis.y * d;
    z := axis.z * d;

    axay := x * axis.y;
    axaz := x * axis.z;
    ayaz := y * axis.z;

    m._00 = c + x * axis.x;
    m._01 = axay - s * axis.z;
    m._02 = axaz + s * axis.y;

    m._10 = axay + s * axis.z;
    m._11 = c + y * axis.y;
    m._12 = ayaz - s * axis.x;

    m._20 = axaz - s * axis.y;
    m._21 = ayaz + s * axis.x;
    m._22 = c + z * axis.z;
}

#scope_export
axis_angle_rotation_mat3 :: (axis: Vec3, angle: float) -> Mat3 {
    r: Mat3 = ---;

    axis_angle_rotation_internal(*r, axis, angle);

    return r;
}

axis_angle_rotation_mat4 :: (axis: Vec3, angle: float) -> Mat4 {
    r: Mat4;

    axis_angle_rotation_internal(*r, axis, angle);

    r._33 = 1.0;

    return r;
}

axis_angle_rotation :: axis_angle_rotation_mat4;

// .................
// Reflection matrix
// .................
#scope_module
reflection_internal :: inline (using m: *$T/interface AnyMat3, plane_normal: Vec3) {
    x := plane_normal.x * -2.0;
    y := plane_normal.y * -2.0;
    z := plane_normal.z * -2.0;

    axay := x * plane_normal.y;
    axaz := x * plane_normal.z;
    ayaz := y * plane_normal.z;

    _00 = x * plane_normal.x + 1.0;
    _01 = axay;
    _02 = axaz;

    _10 = axay;
    _11 = y * plane_normal.y + 1.0;
    _12 = ayaz;

    _20 = axaz;
    _21 = ayaz;
    _22 = z * plane_normal.z + 1.0;
}

#scope_export

reflection_mat2 :: (plane_normal: Vec2) -> Mat2 {
    r: Mat2 = ---;

    xy2 := -2.0 * plane_normal.x * plane_normal.y;

    r._00 = 1.0 - 2.0 * plane_normal.x * plane_normal.x;
    r._01 = xy2;
    r._10 = xy2;
    r._11 = 1.0 - 2.0 * plane_normal.y * plane_normal.y;

    return r;
}

reflection_mat3 :: (plane_normal: Vec3) -> Mat3 {
    r: Mat3 = ---;

    reflection_internal(*r, plane_normal);

    return r;
}

reflection_mat4 :: (plane_normal: Vec3) -> Mat4 {
    r: Mat4;

    reflection_internal(*r, plane_normal);

    r._33 = 1.0;

    return r;
}

// ............
// Scale matrix
// ............
scale_mat2 :: (scale: Vec2) -> Mat2 {
    r: Mat2;

    r._00 = scale.x;
    r._11 = scale.y;

    return r;
}

scale_mat3 :: (scale: Vec3) -> Mat3 {
    r: Mat3;

    r._00 = scale.x;
    r._11 = scale.y;
    r._22 = scale.z;

    return r;
}

scale_mat4 :: (scale: Vec3) -> Mat4 {
    r: Mat4;

    r._00 = scale.x;
    r._11 = scale.y;
    r._22 = scale.z;
    r._33 = 1.0;

    return r;
}

#scope_module
scale_along_axis_internal :: inline (m: *$T/interface AnyMat3, a: Vec3, scale: float) {
    s := scale - 1.0;

    x := a.x * s;
    y := a.y * s;
    z := a.z * s;

    axay := x * a.y;
    axaz := x * a.z;
    ayaz := y * a.z;

    m._00 = x * a.x + 1.0;
    m._01 = axay;
    m._02 = axaz;

    m._10 = axay;
    m._11 = y * a.y + 1.0;
    m._12 = ayaz;

    m._20 = axaz;
    m._21 = ayaz;
    m._22 = z * a.z + 1.0;
}

#scope_export
scale_along_axis_mat2 :: (axis: Vec2, scale: float) -> Mat2 {
    m: Mat2;

    s1 := scale - 1.0;
    s1xy = s1 * axis.x * axis.y;

    m._00 = s1 * axis.x * axis.x + 1.0;
    m._01 = s1xy;
    m._10 = s1xy;
    m._11 = s1 * axis.y * axis.y + 1.0;

    return m;
}

scale_along_axis_mat3 :: (axis: Vec3, scale: float) -> Mat3 {
    m: Mat3 = ---;
    scale_along_axis_internal(*m, axis, scale);
    return m;
}

scale_along_axis_mat4 :: (axis: Vec3, scale: float) -> Mat4 {
    m: Mat4;
    scale_along_axis_internal(*m, axis, scale);
    m._33 = 1.0;
    return m;
}

// Projection matrices
orthographic_projection_fullscreen :: (width: u32, height: u32) -> Mat4 {
    if (width == 0 || height == 0) then return Mat4_Identity;

    m: Mat4;

    m._00 = 2.0 / cast(float)width;
    m._03 = -1.0;

    m._11 = -2.0 / cast(float)height;
    m._13 = 1.0;

    m._22 = 1.0;
    m._33 = 1.0;

    return m;
}

orthographic_projection_infinite :: (num_units_horz: float, num_units_vert: float) -> Mat4 {
    if is_nearly_zero(num_units_horz) || is_nearly_zero(num_units_vert) then  return Mat4_Identity;

    m: Mat4;

    m._00 = 2.0 / num_units_horz;
    m._11 = 2.0 / num_units_vert;

    m._22 = 1.0;
    m._33 = 1.0;

    return m;
}

orthographic_projection_infinite :: (w: float, h: float, num_pixels_per_unit: float) -> Mat4 {
    if is_nearly_zero(num_pixels_per_unit) then return Mat4_Identity;

    num_units_horz := w / num_pixels_per_unit;
    num_units_vert := h / num_pixels_per_unit;

    return orthographic_projection_infinite(num_units_horz, num_units_vert);
}