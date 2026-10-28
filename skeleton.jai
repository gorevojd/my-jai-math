// Skin weight validation is the responsibility of mesh/skeleton loaders, before runtime use:
// 1) All four weights of every vertex must be finite (no NaN or infinity).
// 2) Every weight must be nonnegative.
// 3) For a vertex with any nonzero weight, abs(sum(weights) - 1.0) must be <= 0.0001.
//    Four zero weights are allowed: skinning preserves that vertex's source position.
// apply_pose_to_mesh assumes these conditions and does not validate or normalize weights.

HashTable :: #import "Hash_Table";

INVALID_BONE_INDEX :: 255;
MAX_BONE_COUNT :: 128;
VertexBoneIndices :: [4]u8;

AnimatedGeometryInterface :: struct {
    vertices: [..]Vec3;
    indices: [..]u32;
    bones: [..]VertexBoneIndices;
    weights: [..]Vec4;
}

Bone :: struct {
    local: Transform;
    inverse_bind: Transform;

    name: string;
    id: string;

    parent_index: u8 = INVALID_BONE_INDEX;
}

Socket :: struct {
    position: Vec3;
    rotation: Quat;

    name: string;
    joint: string;
}

Pose :: struct {
    bone_transforms: [MAX_BONE_COUNT] Transform;
    num_bones: u32;
}

Skeleton :: struct {
    // Loaders must store parents before children and remap all bone references accordingly.
    // Each parent_index is INVALID_BONE_INDEX or strictly less than the child's index.
    bones: [MAX_BONE_COUNT] Bone;
    sockets: [MAX_BONE_COUNT] Socket;

    num_bones: u32;
    num_sockets: u32;

    default_pose: Pose;

    root_bone_index: u8 = INVALID_BONE_INDEX;
    bone_mapping: HashTable.Table(string, u8);
}

set_bone_transform :: (pose: *Pose, bone_index: u32, transform: Transform) #no_abc {
    Basic.assert(bone_index < pose.num_bones, "Invalid bone_index=%, when num bones is: %!", bone_index, pose.num_bones);
    pose.bone_transforms[bone_index] = transform;
}

get_bone_transform :: (pose: Pose, bone_index: u32) -> Transform #no_abc {
    Basic.assert(bone_index < pose.num_bones, "Invalid bone_index=%, when num bones is: %!", bone_index, pose.num_bones);
    return pose.bone_transforms[bone_index];
}

set_bone_position :: (pose: *Pose, bone_index: u32, position: Vec3) #no_abc {
    Basic.assert(bone_index < pose.num_bones, "Invalid bone_index=%, when num bones is: %!", bone_index, pose.num_bones);
    pose.bone_transforms[bone_index].position = position;
}

get_bone_position :: (pose: Pose, bone_index: u32) -> Vec3 #no_abc {
    Basic.assert(bone_index < pose.num_bones, "Invalid bone_index=%, when num bones is: %!", bone_index, pose.num_bones);
    return pose.bone_transforms[bone_index].position;
}

set_bone_rotation :: (pose: *Pose, bone_index: u32, rotation: Quat) #no_abc {
    Basic.assert(bone_index < pose.num_bones, "Invalid bone_index=%, when num bones is: %!", bone_index, pose.num_bones);
    pose.bone_transforms[bone_index].rotation = rotation;
}

get_bone_rotation :: (pose: Pose, bone_index: u32) -> Quat #no_abc {
    Basic.assert(bone_index < pose.num_bones, "Invalid bone_index=%, when num bones is: %!", bone_index, pose.num_bones);
    return pose.bone_transforms[bone_index].rotation;
}

set_bone_scale :: (pose: *Pose, bone_index: u32, scale: float) #no_abc {
    Basic.assert(bone_index < pose.num_bones, "Invalid bone_index=%, when num bones is: %!", bone_index, pose.num_bones);
    pose.bone_transforms[bone_index].scale = scale;
}

get_bone_scale :: (pose: Pose, bone_index: u32) -> float #no_abc {
    Basic.assert(bone_index < pose.num_bones, "Invalid bone_index=%, when num bones is: %!", bone_index, pose.num_bones);
    return pose.bone_transforms[bone_index].scale;
}

// Applies the delta to the base pose; rotation composes as delta * base.
add_poses :: (base: Pose, delta: Pose) -> Pose {
    result := base;
    add_poses_internal(*result, delta);
    return result;
}

add_poses_internal :: (base: *Pose, delta: Pose) #no_abc {
    Basic.assert(base.num_bones == delta.num_bones && base.num_bones <= MAX_BONE_COUNT);

    for i: 0..cast(int)base.num_bones - 1 {
        base_transform := *base.bone_transforms[i];
        delta_transform := delta.bone_transforms[i];
        base_transform.position += delta_transform.position;
        base_transform.rotation = normalize_safe(delta_transform.rotation * base_transform.rotation);
        base_transform.scale *= delta_transform.scale;
    }
}

sub_poses :: (a: Pose, b: Pose) -> Pose {
    result := a;
    sub_poses_internal(*result, b);
    return result;
}

// Produces a left delta: delta = pose * inverse(other).
// add_poses(other, delta) reconstructs the input pose.
sub_poses_internal :: (pose: *Pose, other: Pose) {
    Basic.assert(pose.num_bones == other.num_bones && pose.num_bones <= MAX_BONE_COUNT);

    for i: 0..cast(int)pose.num_bones - 1 {
        a := *pose.bone_transforms[i];
        b := other.bone_transforms[i];

        Basic.assert(b.scale != 0.0, "Cannot subtract a pose with zero scale at bone %!", i);

        a.position -= b.position;
        a.rotation = normalize_safe(normalize_safe(a.rotation) * conjugate(normalize_safe(b.rotation)));
        a.scale /= b.scale;
    }
}

lerp_poses :: (a: Pose, b: Pose, t: float) -> Pose #no_abc {
    result := a;
    lerp_poses_internal(*result, b, t);
    return result;
}

// The blend parameter is unclamped, matching the scalar/vector lerp helpers.
lerp_poses_internal :: (pose: *Pose, other: Pose, t: float) #no_abc {
    Basic.assert(pose.num_bones == other.num_bones && pose.num_bones <= MAX_BONE_COUNT);
    Basic.assert(is_finite(t));

    for i: 0..cast(int)pose.num_bones - 1 {
        a := *pose.bone_transforms[i];
        b := other.bone_transforms[i];
        a.position = lerp(a.position, b.position, t);
        a.rotation = slerp_unclamped(a.rotation, b.rotation, t);
        a.scale = lerp(a.scale, b.scale, t);
    }
}

lerp_barycentric_poses :: (p0: Pose, p1: Pose, p2: Pose, t0: float, t1: float, t2: float) -> Pose #no_abc {
    result := p0;
    lerp_barycentric_poses_internal(*result, p1, p2, t0, t1, t2);
    return result;
}

lerp_barycentric_poses_internal :: (p0: *Pose, p1: Pose, p2: Pose, t0: float, t1: float, t2: float) #no_abc {
    Basic.assert(p0.num_bones == p1.num_bones && p0.num_bones == p2.num_bones && p0.num_bones <= MAX_BONE_COUNT);
    Basic.assert(is_finite(t0) && is_finite(t1) && is_finite(t2));
    Basic.assert(t0 >= 0.0 && t1 >= 0.0 && t2 >= 0.0);

    largest := Basic.max(t0, Basic.max(t1, t2));
    Basic.assert(largest > 0.0);

    w0 := t0 / largest;
    w1 := t1 / largest;
    w2 := t2 / largest;
    total := w0 + w1 + w2;
    w0 /= total;
    w1 /= total;
    w2 /= total;

    for i: 0..cast(int)p0.num_bones - 1 {
        a := *p0.bone_transforms[i];
        b := p1.bone_transforms[i];
        c := p2.bone_transforms[i];
        q0 := normalize_safe(a.rotation);
        q1 := normalize_safe(b.rotation);
        q2 := normalize_safe(c.rotation);
        reference := q0;
        if t1 > t0 && t1 >= t2 {
            reference = q1;
        }
        else if t2 > t0 && t2 > t1 {
            reference = q2;
        }
        if dot(reference, q0) < 0.0 {
            q0 = -q0;
        }
        if dot(reference, q1) < 0.0 {
            q1 = -q1;
        }
        if dot(reference, q2) < 0.0 {
            q2 = -q2;
        }
        a.position = a.position * w0 + b.position * w1 + c.position * w2;
        a.rotation = normalize_safe(q0 * w0 + q1 * w1 + q2 * w2);
        a.scale = a.scale * w0 + b.scale * w1 + c.scale * w2;
    }
}

move_poses :: (dst: *Pose, src: Pose) #no_abc {
    Basic.assert(src.num_bones <= MAX_BONE_COUNT);
    dst.* = src;
}

apply_pose_to_mesh :: (dst: *$T/interface AnimatedGeometryInterface, src: T, skeleton: Skeleton, pose: Pose) {
    Basic.assert(pose.num_bones == skeleton.num_bones && skeleton.num_bones <= MAX_BONE_COUNT);
    Basic.assert(dst.vertices.count == src.vertices.count);
    Basic.assert(src.bones.count == src.vertices.count && src.weights.count == src.vertices.count);

    global_transforms: [MAX_BONE_COUNT] Transform;
    skin_transforms: [MAX_BONE_COUNT] Transform;

    // Parent-before-child storage makes a single forward pass sufficient.
    for i: 0..cast(int)skeleton.num_bones - 1 {
        parent_index := skeleton.bones[i].parent_index;
        global := pose.bone_transforms[i];
        if parent_index != INVALID_BONE_INDEX {
            Basic.assert(parent_index < i, "Parent bone % must precede child bone %!", parent_index, i);
            global = multiply(global_transforms[parent_index], global);
        }
        global_transforms[i] = global;
        skin_transforms[i] = multiply(global, skeleton.bones[i].inverse_bind);
    }

    for source_position, i: src.vertices {
        weights := src.weights[i];
        position: Vec3;
        total_weight := 0.0;
        for influence: 0..3 {
            weight := weights.e[influence];
            if weight == 0.0 {
                continue;
            }
            bone_index := src.bones[i][influence];
            Basic.assert(bone_index < skeleton.num_bones, "Invalid skin bone index % at vertex %!", bone_index, i);
            position += transform_point(skin_transforms[bone_index], source_position) * weight;
            total_weight += weight;
        }

        if total_weight == 0.0 {
            dst.vertices[i] = source_position;
        }
        else {
            dst.vertices[i] = position;
        }
    }
}
