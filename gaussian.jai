/**
 * @brief Allocate a normalized, centered one-dimensional Gaussian kernel.
 * @param[in] radius Number of samples on each side of the center; zero produces [1].
 * @param[in] sigma Finite positive standard deviation in sample spacings.
 * @return Owned dynamic array of 2 * radius + 1 weights; empty on size overflow or nonfinite sigma if execution continues.
 * @details
 * - Samples exp(-0.5 * (x / sigma)^2) at integer offsets from -radius to radius.
 * - Byte counts above U32_MAX trigger an assertion and return an empty array before allocation if execution continues.
 * - Computes and accumulates float weights, then normalizes their sum to one within rounding error.
 * - Nonfinite sigma triggers an assertion and returns an empty array if execution continues.
 * - Nonpositive sigma triggers an assertion; if execution continues, clamps sigma to DEFAULT_EPSILON.
 * - Allocates through the current context allocator. The caller must release the result with array_free.
 * - Allocation failure follows Basic.array_resize behavior (assertion), not the empty-array error result.
 */
gaussian_kernel_1d :: (radius: u32, sigma: float) -> [..] float {
    weights: [..] float;
    if !is_finite(sigma) {
        Basic.assert(false, "Sigma is not finite!");
        return weights;
    }

    if sigma <= 0.0 {
        Basic.assert(false, "Sigma is not valid: %!", sigma);
        sigma = max(DEFAULT_EPSILON, sigma);
    }

    center := cast(s64) radius;
    count := 2 * center + 1;
    MAX_WEIGHT_COUNT :: U32_MAX / size_of(float);
    if count > MAX_WEIGHT_COUNT {
        Basic.assert(false, "Gaussian kernel size exceeds U32_MAX bytes: radius=%!", radius);
        return weights;
    }

    array_resize(*weights, count, false);
    total: float = 0.0;
    for *weight, index: weights {
        offset := cast(float) (index - center) / sigma;
        value := exp(-0.5 * offset * offset);
        weight.* = value;
        total += value;
    }

    for *weight: weights {
        weight.* /= total;
    }

    return weights;
}

/**
 * @brief Allocate a normalized square Gaussian kernel with equal sigma on both axes.
 * @param[in] radius Number of samples on each side of the center along both axes; zero produces [1].
 * @param[in] sigma Finite positive standard deviation in sample spacings on both axes.
 * @return Owned dynamic array of (2 * radius + 1)^2 weights; empty on size overflow or nonfinite sigma if execution continues.
 * @details
 * - Stores weights in row-major order: result[y * side + x], where side = 2 * radius + 1.
 * - Normalizes to unit sum within float rounding error.
 * - Byte counts above U32_MAX trigger an assertion and return an empty array if execution continues.
 * - Nonfinite sigma triggers an assertion and returns an empty array if execution continues.
 * - Nonpositive sigma triggers an assertion; if execution continues, clamps sigma to DEFAULT_EPSILON.
 * - Allocates through the current context allocator; the caller must release the result with array_free.
 * - Allocation failure follows Basic.array_resize behavior (assertion).
 * @see gaussian_kernel_1d
 */
gaussian_kernel_2d :: (radius: u32, sigma: float) -> [..] float {
    return gaussian_kernel_2d(radius, sigma, sigma);
}

/**
 * @brief Allocate a normalized square Gaussian kernel with independent X and Y sigma.
 * @param[in] radius Number of samples on each side of the center along both axes; zero produces [1].
 * @param[in] sigma_x Finite positive standard deviation along X in sample spacings.
 * @param[in] sigma_y Finite positive standard deviation along Y in sample spacings.
 * @return Owned dynamic array of (2 * radius + 1)^2 weights; empty on size overflow or nonfinite sigma if execution continues.
 * @details
 * - Stores weights in row-major order: result[y * side + x], where side = 2 * radius + 1.
 * - Samples exp(-0.5 * ((x / sigma_x)^2 + (y / sigma_y)^2)) at centered integer offsets.
 * - Computes and accumulates float weights, then normalizes their sum to one within rounding error.
 * - Nonfinite sigma triggers an assertion and returns an empty array if execution continues.
 * - Nonpositive sigma triggers an assertion; if execution continues, clamps both sigma values to at least DEFAULT_EPSILON.
 * - The kernel is separable: two 1D kernels with the same radius give the same weights within rounding error.
 * - Byte counts above U32_MAX trigger an assertion and return an empty array before allocation if execution continues.
 * - Allocates through the current context allocator.
 * - The caller must release the result with array_free. Allocation failure follows Basic.array_resize behavior (assertion).
 * @see gaussian_kernel_1d
 */
gaussian_kernel_2d :: (radius: u32, sigma_x: float, sigma_y: float) -> [..] float {
    weights: [..] float;
    if (!is_finite(sigma_x) || !is_finite(sigma_y)) {
        Basic.assert(false, "Sigma values are not finite!");
        return weights;
    }

    if (sigma_x <= 0.0 || sigma_y <= 0.0) {
        Basic.assert(false, "Sigma values are not valid: x=%, y=%!", sigma_x, sigma_y);
        sigma_x = max(DEFAULT_EPSILON, sigma_x);
        sigma_y = max(DEFAULT_EPSILON, sigma_y);
    }

    center := cast(s64) radius;
    side := 2 * center + 1;
    MAX_WEIGHT_COUNT :: U32_MAX / size_of(float);
    if side > MAX_WEIGHT_COUNT / side {
        Basic.assert(false, "Gaussian kernel size exceeds U32_MAX bytes: radius=%!", radius);
        return weights;
    }

    array_resize(*weights, side * side, false);
    total: float = 0.0;
    for *weight, index: weights {
        x := cast(float) (index % side - center) / sigma_x;
        y := cast(float) (index / side - center) / sigma_y;
        value := exp(-0.5 * (x * x + y * y));
        weight.* = value;
        total += value;
    }

    for *weight: weights {
        weight.* /= total;
    }

    return weights;
}
