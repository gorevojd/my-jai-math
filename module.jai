#load "cephes.jai";
#load "vectors.jai";
#load "mesh.jai";
#load "colors.jai";
#load "matrix.jai";
#load "complex.jai";
#load "fourier.jai";
#load "transform.jai";
#load "rectangle.jai";
#load "segments.jai";
#load "plane.jai";
#load "euler.jai";
#load "ray.jai";
#load "random.jai";
#load "gaussian.jai";

TAU   :: cast(float32) 6.283185;
TAU64 :: 6.28318530717958648;

PI    :: cast(float32) 3.1415927;
PI64  :: 3.141592653589793;

PI_05 :: PI * 0.5;
PI64_05 :: PI64 * 0.5;

FLOAT16_MAX : float :   65504.0;

FLOAT32_MIN        :: 0h00800000;
FLOAT32_MAX        :: 0h7F7FFFFF;
FLOAT32_INFINITY   :: 0h7F800000;
FLOAT32_NAN        :: 0h7FBFFFFF;

FLOAT64_MIN        :: 0h00100000_00000000;
FLOAT64_MAX        :: 0h7FEFFFFF_FFFFFFFF;
FLOAT64_INFINITY   :: 0h7FF00000_00000000;
FLOAT64_NAN        :: 0h7FF7FFFF_FFFFFFFF;

S8_MIN  :s8:    -128;
S8_MAX  :s8:    127;
U8_MAX  :u8:    255;
S16_MIN :s16:   -32768;
S16_MAX :s16:   32767;
U16_MAX :u16:   0xffff;
S32_MIN :s32:   0x8000_0000;
S32_MAX :s32:   0x7fff_ffff;
U32_MAX :u32:   0xffff_ffff;
S64_MIN :s64:   0x8000_0000_0000_0000;
S64_MAX :s64:   0x7fff_ffff_ffff_ffff;
U64_MAX :u64:   0xffff_ffff_ffff_ffff;

DEFAULT_EPSILON :: 0.000001;
DEFAULT_EPSILON_SQUARED :: #run square(cast(float64) DEFAULT_EPSILON);

DEG_TO_RAD_64 :: 0.0174532925;
RAD_TO_DEG_64 :: 57.2957795131;
DEG_TO_RAD :: cast(float32) DEG_TO_RAD_64;
RAD_TO_DEG :: cast(float32) RAD_TO_DEG_64;

deg_to_rad :: inline (angle_degrees: float) -> float { return angle_degrees * DEG_TO_RAD; }
rad_to_deg :: inline (angle_radians: float) -> float { return angle_radians * RAD_TO_DEG; }

square :: inline (x: $T) -> T
#modify { return Basic.IsScalar(T), "Argument is not of a scalar type."; } {
    return x * x;
}

abs :: inline (x: $T/.[s64, s32, s16, s8]) -> T {
    result := ifx x < 0 then -x else x;
    Basic.assert(result >= 0, "An argument was passed that has no absolute value in twos-complement representation.");  // If this assertion fails, it is probably because you passed a value like S64_MIN which does not have a positive version in two's-complement representation.
    return result;
}

abs :: inline (x: float64) -> float64 {
    union {
        _float64: float64;
        _u64:     u64;
    }

    _float64 = x;
    _u64 &= 0x7fff_ffff_ffff_ffff;
    return _float64;
}

abs :: inline (x: float32) -> float32 {
    union {
        _float32: float32;
        _u32:     u32;
    }

    _float32 = x;
    _u32 &= 0x7fff_ffff;
    return _float32;
}

sign :: inline (s: float) -> float { return ifx s >= 0.0 then 1.0 else -1.0; }

is_nearly_zero :: inline (f: float, epsilon := DEFAULT_EPSILON) -> bool {
    return abs(f) <= epsilon;
}

values_are_close :: inline (f0: float, f1: float, epsilon := DEFAULT_EPSILON) -> bool {
    return abs(f0-f1) <= epsilon;
}

values_are_close :: inline (a: Vec2, b: Vec2, epsilon := DEFAULT_EPSILON) -> bool {
    return values_are_close(a.x, b.x, epsilon) && values_are_close(a.y, b.y, epsilon);
}

values_are_close :: inline (a: Vec3, b: Vec3, epsilon := DEFAULT_EPSILON) -> bool {
    return values_are_close(a.x, b.x, epsilon) && values_are_close(a.y, b.y, epsilon) && values_are_close(a.z, b.z, epsilon);
}

equal :: values_are_close;

max_of_type :: inline ($T: Type) -> T  {
    using Math;
    #if      T == bool    return true;
    else #if T == u8      return U8_MAX;
    else #if T == s8      return S8_MAX;
    else #if T == u16     return U16_MAX;
    else #if T == s16     return S16_MAX;
    else #if T == u32     return U32_MAX;
    else #if T == s32     return S32_MAX;
    else #if T == u64     return U64_MAX;
    else #if T == s64     return S64_MAX;
    else #if T == float   return FLOAT32_MAX;
    else #if T == float64 return FLOAT64_MAX;
    else {
        #assert false "Unhandled type T in max_of_type.";
    }
}

min_of_type :: inline ($T: Type) -> T {
    using Math;
    #if      T == bool    return false;
    else #if T == float   return FLOAT32_MIN;
    else #if T == float64 return FLOAT64_MIN;
    else #if #run type_info(T).type != .INTEGER #assert false "Unhandled type T in min_of_type.";
    else #if #run type_info(T).signed == false return 0;
    else #if T == s8      return S8_MIN;
    else #if T == s16     return S16_MIN;
    else #if T == s32     return S32_MIN;
    else #if T == s64     return S64_MIN;
    else {
        #assert false "Unhandled type T in min_of_type.";
    }
}

is_nan_is_inf :: (x: float64) -> (isnan: bool, isinf: bool) {
    uf := (.*) cast(*u64)*x;

    unbiased_exponent := cast(s32) ((uf >> 52) & 0x7ff);  // @Feature: What we really mean here is "cast to whatever you think is a fast number, that is at least some size... [in this case it needs to be at least 11 bits!]

    if unbiased_exponent == 0x7ff {
        raw_mantissa := uf & 0xf_ffff_ffff_ffff;
        if raw_mantissa == 0 {
            return false, true;
        }
        else {
            return true, false;
        }
    }

    return false, false;
}

isnan :: (x: float64) -> bool {
    return is_nan_is_inf(x);
}

isinf :: (x: float64) -> bool {
    _, _isinf := is_nan_is_inf(x);
    return _isinf;
}

is_finite :: (x: float64) -> bool {
    uf := (.*) cast(*u64)*x;
    unbiased_exponent := cast(s32) ((uf >> 52) & 0x7ff);  // @Speed: Could remove shift by doing big mask.
    return unbiased_exponent != 0x7ff;
}

is_nan_is_inf :: (x: float32) -> (isnan: bool, isinf: bool) {
    uf := (.*) cast(*u32)*x;
    unbiased_exponent := (uf >> 23) & 0xff;
    if unbiased_exponent == 0xff {
        raw_mantissa := uf & 0x7f_ffff;
        if raw_mantissa == 0 {
            return false, true;
        }
        else {
            return true, false;
        }
    }

    return false, false;
}

isnan :: (x: float32) -> bool {
    return is_nan_is_inf(x);
}

isinf :: (x: float32) -> bool {
    _, _isinf := is_nan_is_inf(x);
    return _isinf;
}

is_finite :: (x: float) -> bool {
    uf := (.*) cast(*u32)*x;
    unbiased_exponent := (uf >> 23) & 0xff;
    return unbiased_exponent != 0xff;
}

lerp :: inline (a: float, b: float, t: float) -> float { return a + (b - a) * t; }
lerp :: inline (a: float64, b: float64, t: float64) -> float64 { return a + (b - a) * t; }

clamp01 :: inline (a: float)   -> float     { return Basic.clamp(a, 0, 1); }
clamp01 :: inline (a: float64) -> float64   { return Basic.clamp(a, 0, 1); }

#scope_module
Basic :: #import "Basic";
Random :: #import "Random";