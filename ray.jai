#scope_export

// p(t) = origin + direction * t. Direction is unit length, so t measures distance in this ray's coordinate space.
// Use make_ray to normalize a nonzero direction once. Direct initialization/field writes must preserve unit length.
// Queries assume finite geometry and 0.0 <= t_min <= t_max; t_max may be positive infinity.
Ray :: struct {
    origin: Vec3;
    direction: Vec3 = .{0.0, 0.0, 1.0};
}

make_ray :: inline (origin: Vec3, direction: Vec3) -> Ray {
    return .{origin = origin, direction = normalize(direction)};
}

ray_point :: inline (r: Ray, t: float) -> Vec3 {
    return r.origin + r.direction * t;
}

get_closest_point_on_ray_to_point :: inline (r: Ray, p: Vec3) -> Vec3 {
    t := Basic.max(0.0, dot(p - r.origin, r.direction));
    return ray_point(r, t);
}

get_distance_of_point_to_ray_squared :: inline (r: Ray, p: Vec3) -> float {
    return distance_squared(p, get_closest_point_on_ray_to_point(r, p));
}

// Nonzero uniform scale and a unit rotation are required. Negative scale also reverses the direction.
// result_distance = input_distance * distance_scale. Convert query intervals as well as hit distances.
transform_ray :: inline (t: Transform, r: Ray) -> (result: Ray, distance_scale: float) {
    transformed := make_ray(transform_point(t, r.origin), transform_direction(t, r.direction) * sign(t.scale));
    return transformed, abs(t.scale);
}

untransform_ray :: inline (t: Transform, r: Ray) -> (result: Ray, distance_scale: float) {
    Basic.assert(t.scale != 0.0, "Cannot invert a ray transform with zero scale");
    transformed := make_ray(untransform_point(t, r.origin), untransform_direction(t, r.direction) * sign(t.scale));
    return transformed, 1.0 / abs(t.scale);
}

RayIntersectionResult :: struct {
    p: Vec3;
    // Distance in the input ray's coordinate space.
    t: float;
    // Triangle-only weights for p0, p1, p2: p = p0*x + p1*y + p2*z. Zero for other surfaces.
    // These are not texture UVs. Interpolate vertex UVs/normals/colors using these same three weights.
    barycentric: Vec3;
}

ray_sphere :: (r: Ray,
               sphere_origin: Vec3,
               sphere_radius: float,
               t_min: float = 0.0,
               t_max: float = FLOAT32_INFINITY) -> (hit: bool, result: RayIntersectionResult)
{
    offset := r.origin - sphere_origin;
    half_b := dot(offset, r.direction);
    radius_squared := sphere_radius * sphere_radius;
    c := dot(offset, offset) - radius_squared;

    // Compute the discriminant through perpendicular distance, avoiding cancellation in half_b*half_b - c.
    perpendicular := offset - r.direction * half_b;
    radial_squared := radius_squared - dot(perpendicular, perpendicular);
    if radial_squared < 0.0 {
        return false, .{};
    }

    root := sqrt(radial_squared);
    q := -half_b - root;
    if half_b < 0.0 {
        q = -half_b + root;
    }

    first := -half_b;
    last := first;
    if q != 0.0 {
        first = q;
        last = c / q;
        if first > last {
            temporary := first;
            first = last;
            last = temporary;
        }
    }

    first_hit, first_result := make_ray_hit(r, first, t_min, t_max);
    if first_hit {
        return first_hit, first_result;
    }

    last_hit, last_result := make_ray_hit(r, last, t_min, t_max);
    return last_hit, last_result;
}

ray_plane :: (r: Ray, plane: Plane, t_min: float = 0.0, t_max: float = FLOAT32_INFINITY) -> (hit: bool, result: RayIntersectionResult) {
    numerator := dot(plane.normal, r.origin) + plane.d;
    denominator := dot(plane.normal, r.direction);
    if denominator == 0.0 {
        return false, .{};
    }

    found, intersection := make_ray_hit(r, -numerator / denominator, t_min, t_max);
    return found, intersection;
}

// Intersect the solid box and clip its overlap to [t_min, t_max]. Touching counts as overlap.
// A query starting inside returns t_min as t_enter; a miss returns false, 0.0, 0.0.
ray_aabb :: (r: Ray,
             box: Rect3D,
             t_min: float = 0.0,
             t_max: float = FLOAT32_INFINITY) -> (hit: bool, t_enter: float, t_exit: float)
{
    minimum := rect_min(box);
    maximum := rect_max(box);

    enter := t_min;
    exit := t_max;
    for axis: 0..2 {
        origin := r.origin.e[axis];
        direction := r.direction.e[axis];
        if direction == 0.0 {
            if origin < minimum.e[axis] || origin > maximum.e[axis] {
                return false, 0.0, 0.0;
            }
            continue;
        }

        near := (minimum.e[axis] - origin) / direction;
        far := (maximum.e[axis] - origin) / direction;
        if near > far {
            temporary := near;
            near = far;
            far = temporary;
        }

        enter = Basic.max(enter, near);
        exit = Basic.min(exit, far);
        if enter > exit {
            return false, 0.0, 0.0;
        }
    }

    return true, enter, exit;
}

// box is in local space; transform places it in the input ray's space. Returns the same clipped overlap as ray_aabb.
// Returned distances are in the input ray's space, including for negative/non-unit transform scale.
ray_obb :: (r: Ray,
            box: Rect3D,
            transform: Transform,
            t_min: float = 0.0,
            t_max: float = FLOAT32_INFINITY) -> (hit: bool, t_enter: float, t_exit: float)
{
    local_ray, distance_scale := untransform_ray(transform, r);
    found, enter, exit := ray_aabb(local_ray, box, t_min * distance_scale, t_max * distance_scale);
    if !found {
        return false, 0.0, 0.0;
    }

    return true, enter / distance_scale, exit / distance_scale;
}

// Nearest surface hit, including the exit when starting inside. Radius must be positive.
// capsule.a/b are hemisphere centers; coincident endpoints produce a sphere.
ray_capsule :: (r: Ray,
                capsule: Segment,
                capsule_radius: float,
                t_min: float = 0.0,
                t_max: float = FLOAT32_INFINITY) -> (hit: bool, result: RayIntersectionResult)
{
    axis := capsule.b - capsule.a;
    axis_length := length(axis);
    if axis_length == 0.0 {
        found, intersection := ray_sphere(r, capsule.a, capsule_radius, t_min, t_max);
        return found, intersection;
    }
    axis /= axis_length;
    offset := r.origin - capsule.a;
    axial_origin := dot(offset, axis);
    axial_direction := dot(r.direction, axis);
    perpendicular_origin := offset - axis * axial_origin;
    perpendicular_direction := r.direction - axis * axial_direction;
    a := dot(perpendicular_direction, perpendicular_direction);
    half_b := dot(perpendicular_origin, perpendicular_direction);
    c := dot(perpendicular_origin, perpendicular_origin) - capsule_radius * capsule_radius;
    found := false;
    nearest: RayIntersectionResult;
    nearest_t := t_max;

    // Cylinder wall: test both roots, since t_min may exclude the entry.
    if a > 0.0 {
        closest := perpendicular_origin - perpendicular_direction * (half_b / a);
        radial_squared := capsule_radius * capsule_radius - dot(closest, closest);
        if radial_squared >= 0.0 {
            root := sqrt(a * radial_squared);
            q := -half_b - root;
            if half_b < 0.0 {
                q = -half_b + root;
            }
            first := -half_b / a;
            last := first;
            if q != 0.0 {
                first = q / a;
                last = c / q;
            }
            for distance: float.[first, last] {
                axial := axial_origin + axial_direction * distance;
                if distance >= t_min && distance <= nearest_t && axial >= 0.0 && axial <= axis_length {
                    found = true;
                    nearest_t = distance;
                    nearest = .{p = ray_point(r, distance), t = distance};
                }
            }
        }
    }
    else if c == 0.0 {
        // A ray along the wall already touches the surface while it is within the finite cylinder.
        axial := axial_origin + axial_direction * t_min;
        if axial >= 0.0 && axial <= axis_length {
            found = true;
            nearest_t = t_min;
            nearest = .{p = ray_point(r, t_min), t = t_min};
        }
    }

    // Restrict each sphere query to its outward hemisphere; internal sphere surfaces are not capsule surfaces.
    for center, index: Vec3.[capsule.a, capsule.b] {
        outward := ifx index == 0 then -axis else axis;
        projection := dot(r.origin - center, outward);
        slope := dot(r.direction, outward);
        cap_min := t_min;
        cap_max := nearest_t;
        if slope > 0.0 {
            cap_min = Basic.max(cap_min, -projection / slope);
        }
        else if slope < 0.0 {
            cap_max = Basic.min(cap_max, -projection / slope);
        }
        else if projection < 0.0 {
            continue;
        }
        if cap_min > cap_max {
            continue;
        }
        cap_hit, cap_result := ray_sphere(r, center, capsule_radius, cap_min, cap_max);
        if cap_hit {
            found = true;
            nearest_t = cap_result.t;
            nearest = cap_result;
        }
    }
    return found, nearest;
}

// Ray triangle intersection. With position and barycentric reconstruction
ray_triangle :: (r: Ray,
                 p0: Vec3,
                 p1: Vec3,
                 p2: Vec3,
                 t_min: float = 0.0,
                 t_max: float = FLOAT32_INFINITY) -> (hit: bool, result: RayIntersectionResult)
{
    found, intersection := ray_triangle_internal(r, p0, p1, p2, t_min, t_max, any_hit = false);
    return found, intersection;
}

// Ray triangle intersection. WITHOUT position and barycentric reconstruction
ray_triangle_any :: (r: Ray,
                     p0: Vec3,
                     p1: Vec3,
                     p2: Vec3,
                     t_min: float = 0.0,
                     t_max: float = FLOAT32_INFINITY) -> bool
{
    found, _ := ray_triangle_internal(r, p0, p1, p2, t_min, t_max, any_hit = true);
    return found;
}

// Weights x/y/z correspond to v0/v1/v2, in the same vertex order as ray_triangle.
// Use Vec2 for UVs, Vec3 for positions/RGB and Vec4 (Linear_Color) for linear RGBA.
// Decode packed colors or sRGB before interpolation. No clamping or perspective correction is applied.
// These helpers also work with barycentric weights obtained without a ray query.
barycentric_interpolate :: inline (v0: $T/.[float, Vec2, Vec3, Vec4], v1: T, v2: T, barycentric: Vec3) -> T {
    return v0 * barycentric.x + v1 * barycentric.y + v2 * barycentric.z;
}

// Return a smooth shading normal in the input normals' coordinate space; the weighted sum must be nonzero.
// Surface orientation, transforms and normal mapping remain the caller's responsibility.
barycentric_interpolate_normal :: inline (n0: Vec3, n1: Vec3, n2: Vec3, barycentric: Vec3) -> Vec3 {
    return normalize(barycentric_interpolate(n0, n1, n2, barycentric));
}

// normal must be unit length and in the same space as the tangents; the projected tangent must be nonzero.
// xyz is the tangent, w is handedness (+1.0 or -1.0). All three corners must have the same w.
// Split vertices/triangles at mirrored tangent-space seams. Handedness is preserved rather than blended.
// Reconstruct the bitangent as cross(normal, result.xyz) * result.w.
barycentric_interpolate_tangent :: inline (t0: Vec4, t1: Vec4, t2: Vec4, barycentric: Vec3, normal: Vec3) -> Vec4 {
    tangent := barycentric_interpolate(t0.xyz, t1.xyz, t2.xyz, barycentric);
    tangent = normalize(tangent - normal * dot(normal, tangent));
    return xyzw(tangent, t0.w);
}

#scope_module

ray_triangle_internal :: (r: Ray,
                          p0: Vec3,
                          p1: Vec3,
                          p2: Vec3,
                          t_min: float,
                          t_max: float,
                          $any_hit: bool) -> bool, RayIntersectionResult
{
    // Shear the ray onto its dominant axis, reducing the inside test to an origin/2D-triangle test.
    z_axis := 0;
    if abs(r.direction.y) > abs(r.direction.x) {
        z_axis = 1;
    }
    if abs(r.direction.z) > abs(r.direction.e[z_axis]) {
        z_axis = 2;
    }
    x_axis := (z_axis + 1) % 3;
    y_axis := (x_axis + 1) % 3;
    direction_z := r.direction.e[z_axis];
    shear_x := r.direction.e[x_axis] / direction_z;
    shear_y := r.direction.e[y_axis] / direction_z;
    x, y, z: [3]float;
    for p, index: Vec3.[p0, p1, p2] {
        relative := p - r.origin;
        relative_z := relative.e[z_axis];
        x[index] = relative.e[x_axis] - shear_x * relative_z;
        y[index] = relative.e[y_axis] - shear_y * relative_z;
        z[index] = relative_z / direction_z;
    }

    e0 := x[1] * y[2] - y[1] * x[2];
    e1 := x[2] * y[0] - y[2] * x[0];
    e2 := x[0] * y[1] - y[0] * x[1];
    if ((e0 < 0.0 || e1 < 0.0 || e2 < 0.0) &&
        (e0 > 0.0 || e1 > 0.0 || e2 > 0.0)) {
        return false, .{};
    }

    determinant := e0 + e1 + e2;
    if determinant == 0.0 {
        return false, .{};
    }

    scaled_t := e0 * z[0] + e1 * z[1] + e2 * z[2];
    if determinant > 0.0 {
        if scaled_t < t_min * determinant || scaled_t > t_max * determinant {
            return false, .{};
        }
    }
    else {
        if scaled_t > t_min * determinant || scaled_t < t_max * determinant {
            return false, .{};
        }
    }

    #if any_hit {
        return true, .{};
    }

    t := scaled_t / determinant;
    barycentric := Vec3.{e0 / determinant, e1 / determinant, e2 / determinant};
    return true, .{p = ray_point(r, t), t = t, barycentric = barycentric};
}

make_ray_hit :: (r: Ray, t: float, t_min: float, t_max: float, barycentric: Vec3 = .{}) -> bool, RayIntersectionResult {
    if t < t_min || t > t_max {
        return false, .{};
    }

    return true, .{p = ray_point(r, t), t = t, barycentric = barycentric};
}
