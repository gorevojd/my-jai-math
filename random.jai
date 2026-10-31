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
        point.* *= sqrt(Random.random_get_zero_to_one_open());
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
    Basic.assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    for *point: dst {
        angle := TAU * Random.random_get_zero_to_one_open();
        point.x = radius * cos(angle);
        point.y = radius * sin(angle);
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
        point.* *= pow(Random.random_get_zero_to_one_open(), 1.0 / 3.0);
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
        point.* *= pow(Random.random_get_zero_to_one_open(), 1.0 / 3.0);
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
    Basic.assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    for *point: dst {
        height := 1.0 - 2.0 * Random.random_get_zero_to_one_open();
        angle := TAU * Random.random_get_zero_to_one_open();
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
    Basic.assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    if dst.count == 0 {
        return;
    }

    axis, tangent, bitangent := hemisphere_basis(normal);
    for *point: dst {
        height := Random.random_get_zero_to_one_open();
        angle := TAU * Random.random_get_zero_to_one_open();
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
    Basic.assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    if dst.count == 0 {
        return;
    }

    angle: float = 0.0;
    for *point, index: dst {
        height := 1.0 - 2.0 * (cast(float) index + 0.5) / cast(float) dst.count;
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
    Basic.assert(is_finite(radius) && radius >= 0.0, "Radius must be finite and nonnegative.");

    if dst.count == 0{
        return;
    }

    axis, tangent, bitangent := hemisphere_basis(normal);
    angle: float = 0.0;
    for *point, index: dst {
        height := (cast(float) index + 0.5) / cast(float) dst.count;
        local := surface_sample(height, angle);
        point.* = (tangent * local.x + axis * local.y + bitangent * local.z) * radius;
        angle = next_golden_angle(angle);
    }
}

#scope_file

surface_sample :: (height: float, angle: float) -> Vec3 {
    ring_radius := sqrt(Basic.max(0.0, 1.0 - height * height));
    return .{ring_radius * cos(angle), height, ring_radius * sin(angle)};
}

next_golden_angle :: (angle: float) -> float {
    // 2*pi / phi^2 = pi * (3 - sqrt(5)). Bound angles to preserve precision for large buffers.
    GOLDEN_ANGLE :: #run PI * (3.0 - sqrt(cast(float) 5.0));
    result := angle + GOLDEN_ANGLE;
    if result >= TAU {
        result -= TAU;
    }
    return result;
}

hemisphere_basis :: (normal: Vec3) -> (Vec3, Vec3, Vec3) {
    Basic.assert(valid_vector(normal), "Hemisphere normal must be finite.");

    axis: Vec3 = .{};
    scale := Basic.max(abs(normal.x), Basic.max(abs(normal.y), abs(normal.z)));
    if scale > 0.0 {
        axis = normalize(normal / scale);
    }
    else {
        Basic.assert(false, "Hemisphere normal must be nonzero. Fallback to UP");
        axis = .{0, 1, 0};
    }

    reference: Vec3 = .{0.0, 1.0, 0.0};
    if abs(axis.y) > 0.9 {
        reference = .{1.0, 0.0, 0.0};
    }

    tangent := normalize(cross(reference, axis));
    return axis, tangent, cross(axis, tangent);
}
