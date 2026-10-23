Segment2D :: struct {
    a, b: Vec2;
}

Segment :: struct {
    a, b: Vec3;
}

#scope_module

SegmentTypes :: Type.[Segment, Segment2D];
SegmentVectorTypes :: Type.[Vec3, Vec2];

assert_segments_types :: ($S: Type, $V: Type) #expand {
    #assert((S == Segment2D && V == Vec2) || (S == Segment && V == Vec3));
}

#scope_export

get_closest_point_on_segment_to_point :: (s: $S/SegmentTypes, p: $V/SegmentVectorTypes) -> V {
    assert_segments_types(S, V);

    ab := s.b - s.a;

    t := clamp01(dot(p - s.a, ab) / dot(ab, ab));

    return s.a + ab * t;
}

get_distance_of_point_to_segment_squared :: (s: $S/SegmentTypes, p: $V/SegmentVectorTypes) -> float {
    assert_segments_types(S, V);

    ab := s.b - s.a;
    ac := p - s.a;
    bc := p - s.b;

    // p is outside a on projection
    e := dot(ac, ab);
    if e <= 0 {
        return dot(ac, ac);
    }

    // p is outside b on projection
    f := dot(ab, ab);
    if e >= f {
        return dot(bc, bc);
    }

    // p projects onto ab
    return dot(ac, ac) - e * e / f;
}

get_segments_intersection :: (a: Segment2D, b: Segment2D) -> bool, Vec2 {
    a_offset := a.b - a.a;
    b_offset := b.b - b.a;

    // a1 + a * t = b1 + b * u;

    denominator := cross(a_offset, b_offset);

    if is_nearly_zero(denominator) {
        // parallel or collinear
        return false, .{};
    }

    delta = b.a - a.a;

    t := cross(delta, b_offset) / denominator;
    u := cross(delta, a_offset) / denominator;

    if (t < 0 || t > 1 || u < 0 || u > 1) {
        return false, .{};
    }

    point := a.a + a_offset * t;

    return true, point;
}

get_closest_points_between_segments :: (a: Segment2D, b: Segment2D) -> Vec2, Vec2 {
    a_offset := a.b - a.a;
    b_offset := b.b - b.a;
    origins_offset := a.a - b.a;

    // Vector between result points: R(s, t) = origins_offset + s*a_offset - t*b_offset;
    // We want to minimize dot(R, R)

    a_len_squared := dot(a_offset, a_offset);
    b_len_squared := dot(b_offset, b_offset);

    // Both segments are points
    if a_len_squared <= DEFAULT_EPSILON_SQUARED && b_len_squared <= DEFAULT_EPSILON_SQUARED {
        return a.a, b.a;
    }

    // A segment is a point
    if a_len_squared <= DEFAULT_EPSILON_SQUARED {
        t := clamp01(dot(b_offset, a.a - b.a) / b_len_squared);

        return a.a, b.a + b_offset * t;
    }

    // B segment is a point
    if b_len_squared <= DEFAULT_EPSILON_SQUARED {
        s := clamp01(dot(a_offset, b.a - a.a) / a_len_squared);

        return a.a + a_offset * s, b.a;
    }

    ab_dot := dot(a_offset, b_offset);
    ao_dot := dot(a_offset, origins_offset);
    bo_dot := dot(b_offset, origins_offset);

    denominator := a_len_squared * b_len_squared - ab_dot * ab_dot;

    s, t: float;

    if denominator > DEFAULT_EPSILON * a_len_squared * b_len_squared {
        s = clamp01((ab_dot * bo_dot - b_len_squared * ao_dot) / denominator);
    }
    else {
        s = 0.0;
    }

    t = (bo_dot + ab_dot * s) / b_len_squared;
    if t < 0.0 {
        t = 0.0;

        s = clamp01(-ao_dot / a_len_squared);
    }
    else if t > 1.0{
        t = 1.0;

        s = clamp01((ab_dot - ao_dot) / a_len_squared);
    }

    point_a := a.a + a_offset * s;
    point_b := b.a + b_offset * t;

    return point_a, point_b;
}