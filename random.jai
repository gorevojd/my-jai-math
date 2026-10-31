// Points centered at the origin. Radius must be finite and nonnegative.
// Random samplers use context.random_state; seed it through the Random module.

/**
 * @brief Generate points uniformly by area inside a disk.
 * @param[out] dst Caller-owned output positions; empty arrays are supported.
 * @param[in] radius Finite nonnegative radius; zero produces origin points.
 * @details
 * - Positions are centered at the origin. No allocations are performed.
 * - Uses radius * sqrt(u), giving P(distance <= r) = (r / radius)^2.
 * - Uses context.random_state; seed it with random_seed from the Random module for reproducibility.
 * @pre Radius must be finite and nonnegative; checked by an assertion.
 */
random_points_in_circle :: (dst: [] Vec2, radius: float) {
    random_points_on_circle(dst, radius);

    for *point: dst {
        point.* *= sqrt(random_get_zero_to_one_open());
    }
}

/**
 * @brief Generate points uniformly by arc length on a circumference.
 * @param[out] dst Caller-owned output positions; empty arrays are supported.
 * @param[in] radius Finite nonnegative radius; zero produces origin points.
 * @details
 * - Positions are centered at the origin. No allocations are performed.
 * - Every point has the supplied radius, within floating-point rounding error.
 * - Uses context.random_state; seed it with random_seed from the Random module for reproducibility.
 * @pre Radius must be finite and nonnegative; checked by an assertion.
 */
random_points_on_circle :: (dst: [] Vec2, radius: float) {
    assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    for *point: dst {
        angle := TAU64 * cast(float64) random_get_zero_to_one_open();
        point.x = radius * cast(float) cos(angle);
        point.y = radius * cast(float) sin(angle);
    }
}

/**
 * @brief Generate points uniformly by volume inside a ball.
 * @param[out] dst Caller-owned output positions; empty arrays are supported.
 * @param[in] radius Finite nonnegative radius; zero produces origin points.
 * @details
 * - Positions are centered at the origin. No allocations are performed.
 * - Uses radius * cbrt(u), giving P(distance <= r) = (r / radius)^3.
 * - Uses context.random_state; seed it with random_seed from the Random module for reproducibility.
 * @pre Radius must be finite and nonnegative; checked by an assertion.
 */
random_points_in_sphere :: (dst: [] Vec3, radius: float) {
    random_points_on_sphere(dst, radius);

    for *point: dst {
        point.* *= pow(random_get_zero_to_one_open(), 1.0 / 3.0);
    }
}

/**
 * @brief Generate points uniformly by volume inside a half-ball facing normal.
 * @param[out] dst Caller-owned output positions; empty arrays are supported.
 * @param[in] radius Finite nonnegative radius; zero produces origin points.
 * @param[in] normal Finite nonzero direction of the hemisphere; need not have unit length.
 * @details
 * - Positions are centered at the origin. No allocations are performed.
 * - Scales uniform hemisphere directions by radius * cbrt(u).
 * - Uses context.random_state; seed it with random_seed from the Random module for reproducibility.
 * @pre Radius must be finite and nonnegative; checked by an assertion.
 * @pre For nonempty dst, normal must be finite and nonzero; checked by assertions.
 */
random_points_in_hemisphere :: (dst: [] Vec3, radius: float, normal: Vec3) {
    random_points_on_hemisphere(dst, radius, normal);

    for *point: dst {
        point.* *= pow(random_get_zero_to_one_open(), 1.0 / 3.0);
    }
}

/**
 * @brief Generate points uniformly by area on a sphere surface.
 * @param[out] dst Caller-owned output positions; empty arrays are supported.
 * @param[in] radius Finite nonnegative radius; zero produces origin points.
 * @details
 * - Positions are centered at the origin. No allocations are performed.
 * - Samples height and azimuth uniformly, avoiding the polar bias of uniform polar angles.
 * - Uses context.random_state; seed it with random_seed from the Random module for reproducibility.
 * @pre Radius must be finite and nonnegative; checked by an assertion.
 */
random_points_on_sphere :: (dst: [] Vec3, radius: float) {
    assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    for *point: dst {
        height := 1.0 - 2.0 * random_get_zero_to_one_open();
        angle := TAU64 * cast(float64) random_get_zero_to_one_open();
        point.* = surface_sample(height, angle) * radius;
    }
}

/**
 * @brief Generate points uniformly by area on the curved hemisphere surface facing normal.
 * @param[out] dst Caller-owned output positions; empty arrays are supported.
 * @param[in] radius Finite nonnegative radius; zero produces origin points.
 * @param[in] normal Finite nonzero direction of the hemisphere; need not have unit length.
 * @details
 * - Positions are centered at the origin. No allocations are performed.
 * - Samples only the curved surface, not the flat base disk.
 * - Uses context.random_state; seed it with random_seed from the Random module for reproducibility.
 * @pre Radius must be finite and nonnegative; checked by an assertion.
 * @pre For nonempty dst, normal must be finite and nonzero; checked by assertions.
 */
random_points_on_hemisphere :: (dst: [] Vec3, radius: float, normal: Vec3) {
    assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    if dst.count == 0 {
        return;
    }

    axis, tangent, bitangent := hemisphere_basis(normal);
    for *point: dst {
        height := random_get_zero_to_one_open();
        angle := TAU64 * cast(float64) random_get_zero_to_one_open();
        local := surface_sample(height, angle);
        point.* = (tangent * local.x + axis * local.y + bitangent * local.z) * radius;
    }
}

/**
 * @brief Generate deterministic, approximately uniform Fibonacci points on a sphere surface.
 * @param[out] dst Caller-owned output positions; empty arrays are supported.
 * @param[in] radius Finite nonnegative radius; zero produces origin points.
 * @details
 * - Positions are centered at the origin. No allocations are performed.
 * - Uses the golden angle pi * (3 - sqrt(5)) and equally spaced midpoint heights, avoiding the poles.
 * - Does not consume random state. The layout depends on dst.count; changing the count moves existing samples.
 * @pre Radius must be finite and nonnegative; checked by an assertion.
 */
golden_points_on_sphere :: (dst: [] Vec3, radius: float) {
    assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    if dst.count == 0 {
        return;
    }

    angle: float64 = 0.0;
    for *point, index: dst {
        height := cast(float) (1.0 - 2.0 * (cast(float64) index + 0.5) / cast(float64) dst.count);
        point.* = surface_sample(height, angle) * radius;
        angle = next_golden_angle(angle);
    }
}

/**
 * @brief Generate deterministic Fibonacci points on the curved hemisphere surface facing normal.
 * @param[out] dst Caller-owned output positions; empty arrays are supported.
 * @param[in] radius Finite nonnegative radius; zero produces origin points.
 * @param[in] normal Finite nonzero direction of the hemisphere; need not have unit length.
 * @details
 * - Positions are centered at the origin. No allocations are performed.
 * - Uses the golden angle and equally spaced midpoint heights; excludes the flat base disk.
 * - Does not consume random state. The layout depends on dst.count; changing the count moves existing samples.
 * @pre Radius must be finite and nonnegative; checked by an assertion.
 * @pre For nonempty dst, normal must be finite and nonzero; checked by assertions.
 */
golden_points_on_hemisphere :: (dst: [] Vec3, radius: float, normal: Vec3) {
    assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    if dst.count == 0 {
        return;
    }

    axis, tangent, bitangent := hemisphere_basis(normal);
    angle: float64 = 0.0;
    for *point, index: dst {
        height := cast(float) ((cast(float64) index + 0.5) / cast(float64) dst.count);
        local := surface_sample(height, angle);
        point.* = (tangent * local.x + axis * local.y + bitangent * local.z) * radius;
        angle = next_golden_angle(angle);
    }
}

#scope_file
#import "Basic";
#import "Random";

/**
 * @brief Construct a unit sphere point from height along Y and azimuth.
 * @param[in] height Normalized Y coordinate in [-1, 1].
 * @param[in] angle Azimuth in radians in the XZ plane.
 * @return Unit-length position within floating-point rounding error.
 */
surface_sample :: (height: float, angle: float64) -> Vec3 {
    ring_radius := sqrt(max(0.0, 1.0 - height * height));
    return .{ring_radius * cast(float) cos(angle), height, ring_radius * cast(float) sin(angle)};
}

/**
 * @brief Advance an azimuth by the golden angle without accumulating large angles.
 * @param[in] angle Current azimuth in [0, TAU64).
 * @return Next azimuth wrapped to [0, TAU64).
 */
next_golden_angle :: (angle: float64) -> float64 {
    // 2*pi / phi^2 = pi * (3 - sqrt(5)). Bound angles to preserve precision for large buffers.
    GOLDEN_ANGLE :: #run PI64 * (3.0 - sqrt(cast(float64) 5.0));
    result := angle + GOLDEN_ANGLE;
    if result >= TAU64 {
        result -= TAU64;
    }
    return result;
}

/**
 * @brief Build an orthonormal frame around a hemisphere direction.
 * @param[in] normal Finite nonzero direction, not necessarily normalized.
 * @return Unit axis along normal, perpendicular tangent and cross(axis, tangent) bitangent.
 * @details
 * - Scales by the largest component before normalization to avoid overflow and underflow.
 * - Chooses a reference axis that is not parallel to the direction.
 * @pre Normal must be finite and nonzero; checked by assertions.
 */
hemisphere_basis :: (normal: Vec3) -> (Vec3, Vec3, Vec3) {
    assert(valid_vector(normal), "Hemisphere normal must be finite.");

    scale := max(abs(normal.x), max(abs(normal.y), abs(normal.z)));
    assert(scale > 0.0, "Hemisphere normal must be nonzero.");

    axis := normalize(normal / scale);
    reference: Vec3 = .{0.0, 1.0, 0.0};
    if abs(axis.y) > 0.9 {
        reference = .{1.0, 0.0, 0.0};
    }

    tangent := normalize(cross(reference, axis));
    return axis, tangent, cross(axis, tangent);
}


