Transform :: struct {
    position: Vec3;
    rotation: Quat;
    scale: float = 1;
}

Transform2D :: struct {
    position: Vec2;
    rotation: Complex;
    scale: float = 1;
}

#scope_module
TransformTypes :: Type.[Transform, Transform2D];
TransformVectorTypes :: Type.[Vec3, Vec2];

assert_transform_vector_types :: ($T: Type, $V: Type) #expand {
    #assert((T == Transform && V == Vec3) || (T == Transform2D && V == Vec2));
}

#scope_export

transform_point :: inline (t: $T/TransformTypes, p: $V/TransformVectorTypes) -> V {
    assert_transform_vector_types(T, V);

    return rotate(t.scale * p, t.rotation) + t.position;
}

untransform_point :: inline (t: $T/TransformTypes, p: $V/TransformVectorTypes) -> V {
    assert_transform_vector_types(T, V);

    return rotate(p - t.position, conjugate(t.rotation)) * scale_inverse_safe(t.scale);
}

transform_vector :: inline (t: $T/TransformTypes, v: $V/TransformVectorTypes) -> V {
    assert_transform_vector_types(T, V);

    return rotate(t.scale * v, t.rotation);
}

untransform_vector :: inline (t: $T/TransformTypes, v: $V/TransformVectorTypes) -> V {
    assert_transform_vector_types(T, V);

    return rotate(v, conjugate(t.rotation)) * scale_inverse_safe(t.scale);
}

transform_direction :: inline (t: $T/TransformTypes, dir: $V/TransformVectorTypes) -> V {
    assert_transform_vector_types(T, V);

    return rotate(dir, t.rotation);
}

untransform_direction :: inline (t: $T/TransformTypes, dir: $V/TransformVectorTypes) -> V {
    assert_transform_vector_types(T, V);

    return rotate(dir, conjugate(t.rotation));
}

transform_scalar :: inline (t: $T/TransformTypes, s: float) -> float {
    return s * t.scale;
}

untransform_scalar :: inline (t: $T/TransformTypes, s: float) -> float {
    return s * scale_inverse_safe(t.scale);
}

to_matrix :: (t: Transform) -> Mat4 {
    result := to_matrix(t.rotation);
    for row: 0..2 {
        for column: 0..2 {
            result.rows[row].e[column] *= t.scale;
        }
    }
    result._03 = t.position.x;
    result._13 = t.position.y;
    result._23 = t.position.z;
    return result;
}

multiply :: inline (a: $T/TransformTypes, b: T) -> T {
    t: T = ---;

    t.rotation = a.rotation * b.rotation;
    t.scale = a.scale * b.scale;
    t.position = rotate(b.position * a.scale, a.rotation) + a.position;

    return t;
}

/**
 * @brief Decomposes an affine TRS matrix into position, rotation and per-axis scale.
 * @param[in] matrix Matrix using column vectors, composed as translation * rotation * scale.
 * @return Position, a unit quaternion and signed X/Y/Z scale.
 * @pre All entries are finite, the last row is (0, 0, 0, 1), and there is no shear.
 * @pre Column lengths must be representable as finite floats.
 * @details
 * - Nonsingular reflections produce three negative scales; otherwise scales are nonnegative.
 *   Individual scale signs and the original quaternion cannot be uniquely recovered.
 * - Zero scale axes receive a deterministic orthonormal completion. An entirely zero
 *   basis returns identity rotation; its original rotation is unrecoverable.
 * - Perspective and shear are unsupported and checked with assertions.
 */
decompose_into_components :: (matrix: Mat4) -> (position: Vec3, rotation: Quat, scale: Vec3) {
    for value: matrix.e {
        Basic.assert(is_finite(value), "decompose requires finite matrix entries.");
    }
    Basic.assert(matrix._30 == 0.0 &&
                 matrix._31 == 0.0 &&
                 matrix._32 == 0.0 &&
                 matrix._33 == 1.0, "decompose requires an affine matrix without perspective.");

    position := Vec3.{matrix._03, matrix._13, matrix._23};
    scale: Vec3;
    axes: [3]Vec3;
    for column: 0..2 {
        axis := Vec3.{matrix.rows[0].e[column], matrix.rows[1].e[column], matrix.rows[2].e[column]};
        // Double precision avoids squared-length overflow/underflow for finite float columns.
        magnitude := sqrt(square(cast(float64) axis.x) + square(cast(float64) axis.y) + square(cast(float64) axis.z));
        scale.e[column] = cast(float) magnitude;
        Basic.assert(is_finite(scale.e[column]), "decompose requires representable column lengths.");
        if magnitude > 0.0 {
            axes[column] = .{cast(float) (axis.x / magnitude), cast(float) (axis.y / magnitude), cast(float) (axis.z / magnitude)};
        }
    }

    Basic.assert(abs(dot(axes[0], axes[1])) <= DECOMPOSE_TOLERANCE &&
                 abs(dot(axes[0], axes[2])) <= DECOMPOSE_TOLERANCE &&
                 abs(dot(axes[1], axes[2])) <= DECOMPOSE_TOLERANCE, "decompose does not support shear.");

    if dot(cross(axes[0], axes[1]), axes[2]) < 0.0 {
        scale = -scale;
        for *axis: axes {
            axis.* = -axis.*;
        }
    }

    if scale.x == 0.0 {
        if scale.y != 0.0 && scale.z != 0.0 {
            axes[0] = normalize(cross(axes[1], axes[2]));
        }
        else if scale.y != 0.0 {
            axes[0] = decompose_perpendicular(axes[1]);
        }
        else if scale.z != 0.0 {
            axes[0] = decompose_perpendicular(axes[2]);
        }
        else {
            axes[0] = .{1.0, 0.0, 0.0};
        }
    }

    if scale.y == 0.0 {
        if scale.z != 0.0 {
            axes[1] = normalize(cross(axes[2], axes[0]));
        }
        else {
            axes[1] = decompose_perpendicular(axes[0]);
        }
    }
    
    if scale.z == 0.0 {
        axes[2] = normalize(cross(axes[0], axes[1]));
    }

    rotation := decompose_rotation(make_from_columns(axes[0], axes[1], axes[2]));
    return position, rotation, scale;
}

/**
 * @brief Decomposes a uniformly scaled affine TRS matrix into a Transform.
 * @param[in] matrix Matrix satisfying the requirements of decompose_into_components.
 * @return Transform with a signed uniform scale and unit rotation.
 * @pre All three column lengths are equal within relative floating-point tolerance.
 * @see decompose_into_components
 */
decompose_into_transform :: (matrix: Mat4) -> Transform {
    position, rotation, scale := decompose_into_components(matrix);
    tolerance := DECOMPOSE_TOLERANCE * Basic.max(abs(scale.x), abs(scale.y), abs(scale.z));
    Basic.assert(abs(scale.x - scale.y) <= tolerance && abs(scale.x - scale.z) <= tolerance,
                 "Transform requires uniform scale; use decompose_into_components.");
    result: Transform;
    result.position = position;
    result.rotation = rotation;
    result.scale = scale.x;
    return result;
}

#scope_file

DECOMPOSE_TOLERANCE :: 0.00001;

decompose_perpendicular :: (axis: Vec3) -> Vec3 {
    reference: Vec3;
    if abs(axis.x) <= abs(axis.y) && abs(axis.x) <= abs(axis.z) {
        reference = .{1.0, 0.0, 0.0};
    }
    else if abs(axis.y) <= abs(axis.z) {
        reference = .{0.0, 1.0, 0.0};
    }
    else {
        reference = .{0.0, 0.0, 1.0};
    }
    return normalize(reference - axis * dot(axis, reference));
}

decompose_rotation :: (matrix: Mat3) -> Quat {
    result: Quat;
    trace := matrix._00 + matrix._11 + matrix._22;
    if trace > 0.0 {
        factor := 2.0 * sqrt(1.0 + trace);
        result.w = 0.25 * factor;
        result.x = (matrix._21 - matrix._12) / factor;
        result.y = (matrix._02 - matrix._20) / factor;
        result.z = (matrix._10 - matrix._01) / factor;
    }
    else if matrix._00 >= matrix._11 && matrix._00 >= matrix._22 {
        factor := 2.0 * sqrt(1.0 + matrix._00 - matrix._11 - matrix._22);
        result.x = 0.25 * factor;
        result.y = (matrix._01 + matrix._10) / factor;
        result.z = (matrix._02 + matrix._20) / factor;
        result.w = (matrix._21 - matrix._12) / factor;
    }
    else if matrix._11 >= matrix._22 {
        factor := 2.0 * sqrt(1.0 + matrix._11 - matrix._00 - matrix._22);
        result.x = (matrix._01 + matrix._10) / factor;
        result.y = 0.25 * factor;
        result.z = (matrix._12 + matrix._21) / factor;
        result.w = (matrix._02 - matrix._20) / factor;
    }
    else {
        factor := 2.0 * sqrt(1.0 + matrix._22 - matrix._00 - matrix._11);
        result.x = (matrix._02 + matrix._20) / factor;
        result.y = (matrix._12 + matrix._21) / factor;
        result.z = 0.25 * factor;
        result.w = (matrix._10 - matrix._01) / factor;
    }
    return normalize(result);
}
