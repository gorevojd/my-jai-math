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

projection_depth_endpoints :: inline (depth_range: ProjectionDepthRange, depth_direction: ProjectionDepthDirection) -> (float, float) {
    low: float = ifx depth_range == .NegativeOneToOne then -1.0 else 0.0;
    if depth_direction == .Reversed {
        return 1.0, low;
    }
    return low, 1.0;
}
