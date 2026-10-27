// Plane
Plane :: struct {
    a, b, c, d : float;

    #overlay (a) #as vec4: Vec4;

    #overlay (a) normal: Vec3;
}

Plane2D :: struct {
    a, b, c: float;

    #overlay (a) #as vec3: Vec3;

    #overlay (a) normal: Vec2;
}

// Plane 3d constructor
make_plane :: (a: float, b: float, c: float, d: float) -> Plane {
    p: Plane = ---;
    p.a = a;
    p.b = b;
    p.c = c;
    p.d = d;
    return p;
}

make_plane :: (v: Vec3, d: float) -> Plane {
    p: Plane = ---;
    p.normal = v;
    p.d = d;
    return p;
}

make_plane :: (v: Vec4) -> Plane {
    p: Plane = ---;
    p.a = v.x;
    p.b = v.y;
    p.c = v.z;
    p.d = v.w;
    return p;
}

plane_from_point_and_normal :: (point: Vec3, normal: Vec3) -> Plane {
    p: Plane = ---;
    p.normal = normal;
    p.c = -dot(point, normal);
    return p;
}

plane_from_points :: (p0: Vec3, p1: Vec3, p2: Vec3) -> Plane {
    cross := normalize_safe(cross(p2 - p1, p0 - p1));

    result: Plane = ---;
    result.a = cross.x;
    result.b = cross.y;
    result.c = cross.z;
    result.d = -dot_product(cross, p0);

    return result;
}

// Plane2D constructors
make_plane_2d :: (a: float, b: float, c: float) -> Plane2D {
    p: Plane2D = ---;
    p.a = a;
    p.b = b;
    p.c = c;
    return p;
}

make_plane_2d :: (v: Vec2, c: float) -> Plane2D {
    p: Plane2D = ---;
    p.normal = v;
    p.c = c;
    return p;
}

make_plane_2d :: (v: Vec3) -> Plane2D {
    p: Plane2D = ---;
    p.a = v.x;
    p.b = v.y;
    p.c = v.z;
    return p;
}

plane_2d_from_point_and_normal :: (point: Vec2, normal: Vec2) -> Plane2D {
    p: Plane2D = ---;
    p.normal = normal;
    p.c = -dot(point, normal);
    return p;
}

// Get Normal operation
get_normal :: (p: Plane, tolerance: float = DEFAULT_EPSILON, fallback: Vec3 = .{z=1}) -> Vec3 {
    result := p.normal;
    normalize(*result, tolerance, fallback);
    return result;
}

get_normal :: (p: Plane2D, tolerance: float = DEFAULT_EPSILON, fallback: Vec2 = .{y=1}) -> Vec2 {
    result := p.normal;
    normalize(*result, tolerance, fallback);
    return result;
}

// Normalize operation
normalize :: (using p: Plane, tolerance: float = DEFAULT_EPSILON, fallback_normal: Vec3 = .{z=1}) -> Plane {
    len := length(normal);

    if len <= tolerance {
        return .{normal = fallback_normal};
    }
    else {
        result := p;
        ilen := 1 / len;
        result.a *= ilen;
        result.b *= ilen;
        result.c *= ilen;
        result.d *= ilen;
        return result;
    }
}

normalize :: (using p: Plane2D, tolerance: float = DEFAULT_EPSILON, fallback_normal: Vec2 = .{y=1}) -> Plane2D {
    len := length(normal);

    if len <= tolerance {
        return .{normal = fallback_normal};
    }
    else {
        result := p;
        ilen := 1 / len;
        result.a *= ilen;
        result.b *= ilen;
        result.c *= ilen;
        return result;
    }
}

// Dot operation
plane_dot :: (plane: Plane, v: Vec3) -> float #symmetric {
    return dot(plane.normal, v) + plane.d;
}

plane_dot :: (plane: Plane2D, v: Vec2) -> float #symmetric {
    return dot(plane.normal, v) + plane.c;
}

// Negate operator
operator- :: (p: Plane) -> Plane {
    r: Plane = ---;
    r.a = -p.a;
    r.b = -p.b;
    r.c = -p.c;
    r.d = -p.d;
    return r;
}

operator- :: (p: Plane2D) -> Plane2D {
    r: Plane2D = ---;
    r.a = -p.a;
    r.b = -p.b;
    r.c = -p.c;
    return r;
}

