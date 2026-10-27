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

multiply :: inline (a: $T/TransformTypes, b: T) -> T {
    t: T = ---;

    t.rotation = a.rotation * b.rotation;
    t.scale = a.scale * b.scale;
    t.position = rotate(b.position * a.scale, a.rotation) + a.position;

    return t;
}