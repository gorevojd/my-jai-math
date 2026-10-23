#scope_export

Rect :: struct {
    center: Vec2;
    radius: Vec2; // Half dimensions along each axis.
}

Rect3D :: struct {
    center: Vec3;
    radius: Vec3; // Half dimensions along each axis.
}

rect_min :: #bake_constants rect_min_internal(T = Rect, R = Vec2);
rect_min :: #bake_constants rect_min_internal(T = Rect3D, R = Vec3);

rect_max :: #bake_constants rect_max_internal(T = Rect, R = Vec2);
rect_max :: #bake_constants rect_max_internal(T = Rect3D, R = Vec3);

rect_dim :: #bake_constants rect_dim_internal(T = Rect, R = Vec2);
rect_dim :: #bake_constants rect_dim_internal(T = Rect3D, R = Vec3);

rect_center :: #bake_constants rect_center_internal(T = Rect, R = Vec2);
rect_center :: #bake_constants rect_center_internal(T = Rect3D, R = Vec3);

rect_min_max :: #bake_constants rect_min_max_internal(T = Vec2, R = Rect);
rect_min_max :: #bake_constants rect_min_max_internal(T = Vec3, R = Rect3D);

rect_min_dim :: #bake_constants rect_min_dim_internal(T = Vec2, R = Rect);
rect_min_dim :: #bake_constants rect_min_dim_internal(T = Vec3, R = Rect3D);

rect_center_dim :: #bake_constants rect_center_dim_internal(T = Vec2, R = Rect);
rect_center_dim :: #bake_constants rect_center_dim_internal(T = Vec3, R = Rect3D);

rect_align_dim :: #bake_constants rect_align_dim_internal(T = Vec2, R = Rect);
rect_align_dim :: #bake_constants rect_align_dim_internal(T = Vec3, R = Rect3D);

rect_get_points :: #bake_constants rect_get_points_internal(T = Rect, R = [4] Vec2);
rect_get_points :: #bake_constants rect_get_points_internal(T = Rect3D, R = [8] Vec3);

rect_union :: (a: $T, b: T) -> T {
    minimum := min(rect_min(a), rect_min(b));
    maximum := max(rect_max(a), rect_max(b));

    return rect_min_max(minimum, maximum);
}

// Perimeter
get_perimeter :: (rect: Rect) -> float {
    dim := rect_dim(rect);

    return 2.0 * (dim.x + dim.y);
}

get_surface_area :: (rect: Rect3D) -> float {
    dim := rect_dim(rect);

    return 2.0 * (dim.x*dim.y + dim.x*dim.z + dim.y*dim.z);
}

// Contains check; touching boundaries count as contained
contains :: (outer: Rect, inner: Rect) -> bool {
    extent := abs(inner.center - outer.center) + inner.radius;

    return extent.x <= outer.radius.x &&
           extent.y <= outer.radius.y;
}

contains :: (outer: Rect3D, inner: Rect3D) -> bool {
    extent := abs(inner.center - outer.center) + inner.radius;

    return extent.x <= outer.radius.x &&
           extent.y <= outer.radius.y &&
           extent.z <= outer.radius.z;
}

// Overlap check; touching boundaries count as overlap
overlap :: (a: Rect, b: Rect) -> bool {
    distance := abs(b.center - a.center);
    radii_sum := a.radius + b.radius;

    return distance.x <= radii_sum.x &&
           distance.y <= radii_sum.y;
}

overlap :: (a: Rect3D, b: Rect3D) -> bool {
    distance := abs(b.center - a.center);
    radii_sum := a.radius + b.radius;

    return distance.x <= radii_sum.x &&
           distance.y <= radii_sum.y &&
           distance.z <= radii_sum.z;
}

#scope_module

rect_min_internal :: inline (rect: $T) -> $R { return rect.center - rect.radius; }
rect_max_internal :: inline (rect: $T) -> $R { return rect.center + rect.radius; }
rect_dim_internal :: inline (rect: $T) -> $R { return rect.radius * 2; }
rect_center_internal :: inline (rect: $T) -> $R { return rect.center; }

// Min Max
rect_min_max_internal :: (min: $T, max: T) -> $R {
    #if T == Vec2 {
        Basic.assert(max.x >= min.x && max.y >= min.y);
    }
    else #if T == Vec3 {
        Basic.assert(max.x >= min.x && max.y >= min.y && max.z >= min.z);
    }
    else #assert false, "Unsupported vector type!";

    radius := (max - min) * 0.5;
    return .{center = min + radius, radius = radius};
}

// Min Dim
rect_min_dim_internal :: (min: $T, dim: T) -> $R {
    #if T == Vec2 {
        Basic.assert(dim.x >= 0.0 && dim.y >= 0.0);
    }
    else #if T == Vec3 {
        Basic.assert(dim.x >= 0.0 && dim.y >= 0.0 && dim.z >= 0.0);
    }
    else #assert false, "Unsupported vector type!";

    radius := dim * 0.5;
    return .{center = min + radius, radius = radius};
}

// Center Dim
rect_center_dim_internal :: (center: $T, dim: T) -> $R {
    #if T == Vec2 {
        Basic.assert(dim.x >= 0.0 && dim.y >= 0.0);
    }
    else #if T == Vec3 {
        Basic.assert(dim.x >= 0.0 && dim.y >= 0.0 && dim.z >= 0.0);
    }
    else #assert false, "Unsupported vector type!";

    return .{center = center, radius = dim * 0.5};
}

// Align Dim
rect_align_dim_internal :: (origin: $T, dim: T, align: T) -> $R {
    #if T == Vec2 {
        Basic.assert(dim.x >= 0.0 && dim.y >= 0.0);
    }
    else #if T == Vec3 {
        Basic.assert(dim.x >= 0.0 && dim.y >= 0.0 && dim.z >= 0.0);
    }
    else #assert false, "Unsupported vector type!";

    radius := dim * 0.5;
    return .{center = origin - dim * align + radius, radius = radius};
}

// Points
rect_get_points_internal :: (rect: $T) -> $R {
    min := rect_min(rect);
    max := rect_max(rect);
    result: R;

    #if T == Rect {
        result[0] = Vec2.{min.x, max.y};
        result[1] = max;
        result[2] = Vec2.{max.x, min.y};
        result[3] = min;
    }
    else #if T == Rect3D {
        // Same XY corner order as Rect, first at min.z, then at max.z.
        result[0] = Vec3.{min.x, max.y, min.z};
        result[1] = Vec3.{max.x, max.y, min.z};
        result[2] = Vec3.{max.x, min.y, min.z};
        result[3] = min;
        result[4] = Vec3.{min.x, max.y, max.z};
        result[5] = max;
        result[6] = Vec3.{max.x, min.y, max.z};
        result[7] = Vec3.{min.x, min.y, max.z};
    }
    else #assert false, "Unsupported rectangle type!";

    return result;
}
