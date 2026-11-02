ProjectionDepthRange :: enum {
    NegativeOneToOne;
    ZeroToOne;
}

ProjectionYDirection :: enum {
    Up;
    Down;
}

ProjectionDepthDirection :: enum {
    Normal;
    Reversed;
}

ProjectionFlip :: enum {
    None;
    Horizontal;
    Vertical;
    Both;
}

// Unit normals point inward; a point is inside or on the frustum when all six plane_dot values are >= 0.
FrustumPlanes :: struct {
    left, right: Plane;
    bottom, top: Plane;
    near, far: Plane;
}

is_volume_in_frustum :: (planes: FrustumPlanes, point: Vec3, radius: float) -> bool {
    Basic.assert(is_finite(radius) && radius >= 0.0, "is_volume_in_frustum requires a finite nonnegative sphere radius.");
    
    return (plane_dot(planes.left, point) >= -radius &&
            plane_dot(planes.right, point) >= -radius &&
            plane_dot(planes.bottom, point) >= -radius &&
            plane_dot(planes.top, point) >= -radius &&
            plane_dot(planes.near, point) >= -radius &&
            plane_dot(planes.far, point) >= -radius);
}

/**
 * @brief Extracts six frustum planes with unit normals pointing inward.
 * @param[in] matrix Transform from the desired plane coordinate space to homogeneous clip space.
 * @param[in] depth_range Clip-space depth convention used to construct the projection.
 * @param[in] y_direction Viewport Y convention, used to label the bottom and top planes.
 * @param[in] depth_direction Depth ordering, used to label the near and far planes.
 * @return Six normalized planes: plane_dot(plane, point) is positive on the interior side,
 *         zero on the plane, and negative on the exterior side. Inside/on the frustum requires all six values >= 0.
 * @pre The matrix must define six finite, non-degenerate planes; an infinite far plane is unsupported.
 * @details
 * - Works with perspective and orthographic projections, including asymmetric and oblique projections.
 * - P returns camera-space planes; P * V returns world-space planes; P * V * M returns model-space planes.
 * - A reflected view matrix is supported. The inward-normal convention is preserved.
 */
extract_frustum_planes :: (matrix: Mat4,
                          depth_range: ProjectionDepthRange,
                          y_direction: ProjectionYDirection = .Up,
                          depth_direction: ProjectionDepthDirection = .Normal) -> FrustumPlanes
{
    result: FrustumPlanes;

    result.left = normalized_frustum_plane(matrix.rows[3] + matrix.rows[0]);
    result.right = normalized_frustum_plane(matrix.rows[3] - matrix.rows[0]);
    lower_y := normalized_frustum_plane(matrix.rows[3] + matrix.rows[1]);
    upper_y := normalized_frustum_plane(matrix.rows[3] - matrix.rows[1]);

    if y_direction == .Up {
        result.bottom = lower_y;
        result.top = upper_y;
    }
    else {
        result.bottom = upper_y;
        result.top = lower_y;
    }

    lower_z: Plane;
    if depth_range == .NegativeOneToOne {
        lower_z = normalized_frustum_plane(matrix.rows[3] + matrix.rows[2]);
    }
    else {
        lower_z = normalized_frustum_plane(matrix.rows[2]);
    }

    upper_z := normalized_frustum_plane(matrix.rows[3] - matrix.rows[2]);
    if depth_direction == .Normal {
        result.near = lower_z;
        result.far = upper_z;
    }
    else {
        result.near = upper_z;
        result.far = lower_z;
    }

    return result;
}

extract_frustum_planes_ogl :: #bake_arguments extract_frustum_planes(depth_range = ProjectionDepthRange.NegativeOneToOne,
                                                                  y_direction = ProjectionYDirection.Up,
                                                                  depth_direction = ProjectionDepthDirection.Normal);
extract_frustum_planes_vk :: #bake_arguments extract_frustum_planes(depth_range = ProjectionDepthRange.ZeroToOne,
                                                                 y_direction = ProjectionYDirection.Down,
                                                                 depth_direction = ProjectionDepthDirection.Normal);
extract_frustum_planes_dx :: #bake_arguments extract_frustum_planes(depth_range = ProjectionDepthRange.ZeroToOne,
                                                                 y_direction = ProjectionYDirection.Up,
                                                                 depth_direction = ProjectionDepthDirection.Normal);

/**
 * @brief Flips a projected image horizontally, vertically, or along both axes.
 * @param[in] projection Projection matrix acting on column vectors, including any API-specific Y adjustment.
 * @param[in] flip None preserves the image; Horizontal reverses X; Vertical reverses Y; Both reverses X and Y.
 * @return A modified copy of projection. Clip-space Z and W, and therefore NDC depth, are unchanged.
 * @details
 * - Computes F * projection, where F is diag(sx, sy, 1, 1) and each selected axis has sign -1.
 *   The operation negates complete rows, including offsets. Negating only _00 or _11 would fail
 *   for asymmetric, fullscreen or jittered projections. Multiplying projection * F is a different operation.
 * - Works with perspective and orthographic projections, either depth range, and normal or reversed Z.
 *   Horizontal swaps left/right and Vertical swaps top/bottom about the viewport center.
 *   Both is equivalent to rotating the image by 180 degrees. Applying the same flip twice restores the matrix.
 * - Composes with the API convention already baked into projection. A second Y inversion cancels the first;
 *   do not separately change the viewport convention to compensate for this intentional image flip.
 * - This is an image flip, not reflection of a camera or scene about a world-space plane.
 *   Planar reflections require a separate plane-reflection transform; there is no universally required X or Y flip.
 * - A single-axis flip reverses projected triangle winding. Both preserves winding.
 *   The renderer must account for the combined parity of camera/scene reflections, projection flips and viewport
 *   orientation when choosing front-face/culling state. This function does not modify graphics API state.
 * - Update any cached inverse projection or inverse view-projection after changing the projection.
 * @note Example: projection = flip_projection(perspective_projection_vk(fov_y, aspect, near, far), .Vertical);
 */
flip_projection :: (projection: Mat4, flip: ProjectionFlip) -> Mat4 {
    result := projection;
    if flip == .Horizontal || flip == .Both {
        result.rows[0] = -result.rows[0];
    }
    if flip == .Vertical || flip == .Both {
        result.rows[1] = -result.rows[1];
    }
    return result;
}

perspective_projection :: (fov_y: float,
                          aspect: float,
                          near: float,
                          far: float,
                          depth_range: ProjectionDepthRange,
                          y_direction: ProjectionYDirection = .Up,
                          depth_direction: ProjectionDepthDirection = .Normal) -> Mat4
{
    Basic.assert(is_finite(fov_y) &&
                 fov_y > 0.0 &&
                 fov_y < PI, "perspective_projection requires a vertical FOV in radians between zero and PI.");
    Basic.assert(is_finite(aspect) && aspect > 0.0, "perspective_projection requires a positive finite aspect ratio.");
    Basic.assert(is_finite(near) &&
                 is_finite(far) &&
                 near > 0.0 &&
                 far > near, "perspective_projection requires finite distances with 0 < near < far.");

    near_ndc, far_ndc := projection_depth_endpoints(depth_range, depth_direction);
    inverse_depth := 1.0 / (far - near);
    f := 1.0 / tan(fov_y * 0.5);
    result: Mat4;
    result._00 = f / aspect;
    result._11 = ifx y_direction == .Up then f else -f;
    result._22 = (near_ndc * near - far_ndc * far) * inverse_depth;
    result._23 = (near_ndc - far_ndc) * near * (far * inverse_depth);
    result._32 = -1.0;
    return result;
}

orthographic_projection :: (left: float,
                           right: float,
                           bottom: float,
                           top: float,
                           near: float,
                           far: float,
                           depth_range: ProjectionDepthRange,
                           y_direction: ProjectionYDirection = .Up,
                           depth_direction: ProjectionDepthDirection = .Normal) -> Mat4
{
    Basic.assert(is_finite(left) &&
                 is_finite(right) &&
                 left != right, "orthographic_projection requires distinct finite horizontal bounds.");
    Basic.assert(is_finite(bottom) &&
                 is_finite(top) &&
                 bottom != top, "orthographic_projection requires distinct finite vertical bounds.");
    Basic.assert(is_finite(near) &&
                 is_finite(far) &&
                 far > near, "orthographic_projection requires finite depth bounds with near < far.");

    near_ndc, far_ndc := projection_depth_endpoints(depth_range, depth_direction);
    inverse_width := 1.0 / (right - left);
    inverse_height := 1.0 / (top - bottom);
    inverse_depth := 1.0 / (far - near);
    y_sign: float = ifx y_direction == .Up then 1.0 else -1.0;
    result: Mat4;
    result._00 = 2.0 * inverse_width;
    result._11 = y_sign * 2.0 * inverse_height;
    result._03 = -(right + left) * inverse_width;
    result._13 = -y_sign * (top + bottom) * inverse_height;
    result._22 = (near_ndc - far_ndc) * inverse_depth;
    result._23 = (near_ndc * far - far_ndc * near) * inverse_depth;
    result._33 = 1.0;
    return result;
}

orthographic_projection_fullscreen :: (width: u32,
                                      height: u32,
                                      depth_range: ProjectionDepthRange,
                                      y_direction: ProjectionYDirection = .Up,
                                      depth_direction: ProjectionDepthDirection = .Normal,
                                      near: float = 0.0,
                                      far: float = 1.0) -> Mat4
{
    if width == 0 || height == 0 {
        return Mat4_Identity;
    }
    return inline orthographic_projection(0.0,
                                          cast(float) width,
                                          cast(float) height,
                                          0.0,
                                          near,
                                          far,
                                          depth_range,
                                          y_direction,
                                          depth_direction);
}

perspective_projection_ogl :: #bake_arguments perspective_projection(depth_range = ProjectionDepthRange.NegativeOneToOne,
                                                                   y_direction = ProjectionYDirection.Up,
                                                                   depth_direction = ProjectionDepthDirection.Normal);
perspective_projection_vk :: #bake_arguments perspective_projection(depth_range = ProjectionDepthRange.ZeroToOne,
                                                                  y_direction = ProjectionYDirection.Down,
                                                                  depth_direction = ProjectionDepthDirection.Normal);
perspective_projection_dx :: #bake_arguments perspective_projection(depth_range = ProjectionDepthRange.ZeroToOne,
                                                                  y_direction = ProjectionYDirection.Up,
                                                                  depth_direction = ProjectionDepthDirection.Normal);

orthographic_projection_ogl :: #bake_arguments orthographic_projection(depth_range = ProjectionDepthRange.NegativeOneToOne,
                                                                     y_direction = ProjectionYDirection.Up,
                                                                     depth_direction = ProjectionDepthDirection.Normal);
orthographic_projection_vk :: #bake_arguments orthographic_projection(depth_range = ProjectionDepthRange.ZeroToOne,
                                                                    y_direction = ProjectionYDirection.Down,
                                                                    depth_direction = ProjectionDepthDirection.Normal);
orthographic_projection_dx :: #bake_arguments orthographic_projection(depth_range = ProjectionDepthRange.ZeroToOne,
                                                                    y_direction = ProjectionYDirection.Up,
                                                                    depth_direction = ProjectionDepthDirection.Normal);

orthographic_projection_fullscreen_ogl :: #bake_arguments orthographic_projection_fullscreen(depth_range = ProjectionDepthRange.NegativeOneToOne,
                                                                                           y_direction = ProjectionYDirection.Up,
                                                                                           depth_direction = ProjectionDepthDirection.Normal);
orthographic_projection_fullscreen_vk :: #bake_arguments orthographic_projection_fullscreen(depth_range = ProjectionDepthRange.ZeroToOne,
                                                                                          y_direction = ProjectionYDirection.Down,
                                                                                          depth_direction = ProjectionDepthDirection.Normal);
orthographic_projection_fullscreen_dx :: #bake_arguments orthographic_projection_fullscreen(depth_range = ProjectionDepthRange.ZeroToOne,
                                                                                          y_direction = ProjectionYDirection.Up,
                                                                                          depth_direction = ProjectionDepthDirection.Normal);

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

#scope_file

normalized_frustum_plane :: (coefficients: Vec4) -> Plane {
    normal_length := length(coefficients.xyz);
    Basic.assert(is_finite(normal_length) &&
                 normal_length > 0.0 &&
                 is_finite(coefficients.w), "extract_frustum_planes requires six finite, non-degenerate planes.");
    return make_plane(coefficients / normal_length);
}

projection_depth_endpoints :: inline (depth_range: ProjectionDepthRange, depth_direction: ProjectionDepthDirection) -> (float, float) {
    low: float = ifx depth_range == .NegativeOneToOne then -1.0 else 0.0;
    if depth_direction == .Reversed {
        return 1.0, low;
    }
    return low, 1.0;
}
