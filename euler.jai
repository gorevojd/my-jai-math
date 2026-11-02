// Radians about X (pitch), Y (yaw), Z (roll); composition is Ry(yaw) * Rx(pitch) * Rz(roll).
EulerAngles :: struct {
    pitch, yaw, roll: float;

    #overlay (pitch) using xyz: Vec3;
}

/**
 * @brief Converts Euler angles in radians to a unit quaternion.
 * @param[in] angles Finite X pitch, Y yaw and Z roll angles.
 * @return Rotation composed as Ry(yaw) * Rx(pitch) * Rz(roll), acting on column vectors.
 * @details
 * - Positive angles follow the corresponding MyMath rotation matrices.
 * - With fixed axes, roll acts first, then pitch, then yaw.
 */
euler_to_quat :: (angles: EulerAngles) -> Quat {
    Basic.assert(is_finite(angles.pitch) &&
                 is_finite(angles.yaw) &&
                 is_finite(angles.roll), "euler_to_quat requires finite angles in radians.");
    sp := sin(angles.pitch * 0.5);
    cp := cos(angles.pitch * 0.5);
    sy := sin(angles.yaw * 0.5);
    cy := cos(angles.yaw * 0.5);
    sr := sin(angles.roll * 0.5);
    cr := cos(angles.roll * 0.5);

    result: Quat;
    result.x = cy * sp * cr + sy * cp * sr;
    result.y = sy * cp * cr - cy * sp * sr;
    result.z = cy * cp * sr - sy * sp * cr;
    result.w = cy * cp * cr + sy * sp * sr;
    return normalize(result);
}

/**
 * @brief Extracts Euler angles in radians using Ry(yaw) * Rx(pitch) * Rz(roll).
 * @param[in] rotation Finite unit quaternion; q and -q describe the same rotation.
 * @return Pitch in [-PI/2, PI/2], yaw and roll in [-PI, PI).
 * @pre The input quaternion is normalized.
 * @details
 * - Euler angles are not unique or continuous across their wrapping boundaries.
 * - At gimbal lock, roll is zero and yaw contains the combined Y/Z rotation.
 * - Within DEFAULT_EPSILON of either singular half-angle pair, the same convention
 *   is used; the reconstructed rotation differs only within floating-point tolerance.
 */
quat_to_euler :: (rotation: Quat) -> EulerAngles {
    Basic.assert(values_are_close(length(rotation), 1.0), "quat_to_euler requires a finite unit quaternion.");

    // These pairs encode (yaw - roll)/2 and (yaw + roll)/2. Unlike matrix extraction,
    // they avoid subtracting nearly equal squared terms when pitch approaches +/-PI/2.
    minus_y := rotation.y - rotation.z;
    minus_x := rotation.w + rotation.x;
    plus_y := rotation.y + rotation.z;
    plus_x := rotation.w - rotation.x;
    minus_squared := square(minus_x) + square(minus_y);
    plus_squared := square(plus_x) + square(plus_y);

    result: EulerAngles;
    result.pitch = atan2(minus_squared - plus_squared, 2.0 * sqrt(minus_squared * plus_squared));
    if (minus_squared <= DEFAULT_EPSILON_SQUARED ||
        plus_squared <= DEFAULT_EPSILON_SQUARED)
    {
        result.yaw = wrap_euler_angle(2.0 * atan2(rotation.y, rotation.w));
        result.roll = 0.0;
    }
    else {
        half_difference := atan2(minus_y, minus_x);
        half_sum := atan2(plus_y, plus_x);
        result.yaw = wrap_euler_angle(half_difference + half_sum);
        result.roll = wrap_euler_angle(half_sum - half_difference);
    }
    return result;
}

get_rotation_yaw :: (rotation: Quat) -> float {
    Basic.assert(values_are_close(length(rotation), 1.0), "get_rotation_yaw requires a finite unit quaternion.");
    minus_y := rotation.y - rotation.z;
    minus_x := rotation.w + rotation.x;
    plus_y := rotation.y + rotation.z;
    plus_x := rotation.w - rotation.x;
    if (square(minus_x) + square(minus_y) <= DEFAULT_EPSILON_SQUARED ||
        square(plus_x) + square(plus_y) <= DEFAULT_EPSILON_SQUARED)
    {
        return wrap_euler_angle(2.0 * atan2(rotation.y, rotation.w));
    }
    // Add the two half-angles algebraically, requiring only one atan2.
    return wrap_euler_angle(atan2(minus_y * plus_x + plus_y * minus_x, minus_x * plus_x - minus_y * plus_y));
}

get_rotation_pitch :: (rotation: Quat) -> float {
    Basic.assert(values_are_close(length(rotation), 1.0), "get_rotation_pitch requires a finite unit quaternion.");
    minus_squared := square(rotation.w + rotation.x) + square(rotation.y - rotation.z);
    plus_squared := square(rotation.w - rotation.x) + square(rotation.y + rotation.z);
    return atan2(minus_squared - plus_squared, 2.0 * sqrt(minus_squared * plus_squared));
}

get_rotation_roll :: (rotation: Quat) -> float {
    Basic.assert(values_are_close(length(rotation), 1.0), "get_rotation_roll requires a finite unit quaternion.");
    minus_y := rotation.y - rotation.z;
    minus_x := rotation.w + rotation.x;
    plus_y := rotation.y + rotation.z;
    plus_x := rotation.w - rotation.x;
    if (square(minus_x) + square(minus_y) <= DEFAULT_EPSILON_SQUARED ||
        square(plus_x) + square(plus_y) <= DEFAULT_EPSILON_SQUARED)
    {
        return 0.0;
    }
    // Subtract the two half-angles algebraically, requiring only one atan2.
    return wrap_euler_angle(atan2(plus_y * minus_x - minus_y * plus_x, plus_x * minus_x + plus_y * minus_y));
}

/**
 * @brief Converts Euler angles directly to a homogeneous rotation matrix.
 * @param[in] angles Finite X pitch, Y yaw and Z roll angles in radians.
 * @return Mat4 equal to Ry(yaw) * Rx(pitch) * Rz(roll), with zero translation and unit scale.
 * @see euler_to_quat
 */
euler_to_matrix :: (angles: EulerAngles) -> Mat4 {
    Basic.assert(is_finite(angles.pitch) && is_finite(angles.yaw) && is_finite(angles.roll),
                 "euler_to_matrix requires finite angles in radians.");
    sp := sin(angles.pitch);
    cp := cos(angles.pitch);
    sy := sin(angles.yaw);
    cy := cos(angles.yaw);
    sr := sin(angles.roll);
    cr := cos(angles.roll);

    result: Mat4;
    result._00 = cy * cr + sy * sp * sr;
    result._01 = -cy * sr + sy * sp * cr;
    result._02 = sy * cp;
    result._10 = cp * sr;
    result._11 = cp * cr;
    result._12 = -sp;
    result._20 = -sy * cr + cy * sp * sr;
    result._21 = sy * sr + cy * sp * cr;
    result._22 = cy * cp;
    result._33 = 1.0;
    return result;
}

/**
 * @brief Extracts Euler angles directly from the rotation block of a Mat3 or Mat4.
 * @param[in] matrix Matrix whose upper-left 3x3 block is a finite proper orthonormal rotation.
 * @return Radians in the same ranges and Y/X/Z composition order as quat_to_euler.
 * @pre The rotation block has no scale, shear or reflection; other entries are ignored.
 * @details
 * - When abs(cos(pitch)) <= DEFAULT_EPSILON, roll is zero and yaw holds the combined rotation.
 * - Remove scale first with decompose_into_components when working with a general TRS matrix.
 * @see quat_to_euler
 */
matrix_to_euler :: (matrix: $T/.[Mat3, Mat4]) -> EulerAngles {
    Basic.assert(is_euler_rotation_matrix(matrix), "matrix_to_euler requires a finite proper orthonormal rotation block.");

    cosine_pitch := length(Vec2.{matrix._02, matrix._22});
    result: EulerAngles;
    result.pitch = atan2(-matrix._12, cosine_pitch);

    // Extract the well-conditioned yaw +/- roll combination. Computing yaw and roll
    // independently near a pole would amplify unrelated rounding errors.
    combined: float;
    if matrix._12 <= 0.0 {
        combined = atan2(matrix._01 - matrix._20, matrix._00 + matrix._21);
    }
    else {
        combined = atan2(-matrix._01 - matrix._20, matrix._00 - matrix._21);
    }
    if cosine_pitch <= DEFAULT_EPSILON {
        result.yaw = wrap_euler_angle(combined);
        result.roll = 0.0;
    }
    else {
        result.yaw = wrap_euler_angle(atan2(matrix._02, matrix._22));
        if matrix._12 <= 0.0 {
            result.roll = wrap_euler_angle(result.yaw - combined);
        }
        else {
            result.roll = wrap_euler_angle(combined - result.yaw);
        }
    }

    return result;
}

#scope_file

is_euler_rotation_matrix :: (matrix: $T/.[Mat3, Mat4]) -> bool {
    x := Vec3.{matrix._00, matrix._10, matrix._20};
    y := Vec3.{matrix._01, matrix._11, matrix._21};
    z := Vec3.{matrix._02, matrix._12, matrix._22};
    return (values_are_close(length_squared(x), 1.0, 0.00001) &&
            values_are_close(length_squared(y), 1.0, 0.00001) &&
            values_are_close(length_squared(z), 1.0, 0.00001) &&
            abs(dot(x, y)) <= 0.00001 &&
            abs(dot(x, z)) <= 0.00001 &&
            abs(dot(y, z)) <= 0.00001 &&
            dot(cross(x, y), z) > 0.0);
}

wrap_euler_angle :: (angle: float) -> float {
    // Quaternion extraction bounds this input to [-2*PI, 2*PI].
    if angle >= PI {
        return angle - 2.0 * PI;
    }
    if angle < -PI {
        return angle + 2.0 * PI;
    }
    return angle;
}
