
Color :: struct {
    r, g, b, a: u8;

    #overlay(r) rgba: u32;
}

Linear_Color :: Vec4;

srgba :: (r: u8, g: u8, b: u8, a: u8) -> Color { return .{r, g, b, a}; }
srgb :: (r: u8, g: u8, b: u8) -> Color { return .{r, g, b, 255}; }

int_from_hex_char_for_colors :: (char: u8) -> u8 {
    res: u8 = 0;

    if char >= #char "a" && char <= #char "f" {
        return char - #char "a" + 10;
    }

    if char >= #char "A" && char <= #char "F" {
        return char - #char "A" + 10;
    }

    if char >= #char "0" && char <= #char "9" {
        return char - #char "0";
    }

    return 0;
}

/*
    Function to convert string to color;
    String has to be of the following format:

    * Starts with a '#' sign
    * Followed by 6 hexadecimal characters

    Example #FF00CC. Alfa will be of value 255 (fully opaque)

*/
is_valid_hex_color :: (color_string: string) -> bool {
    if color_string.count != 7 {
        return false;
    }
    if color_string[0] != #char "#" {
        return false;
    }
    for 1..6 {
        char := color_string[it];
        if char >= #char "0" && char <= #char "9" {
            continue;
        }
        if char >= #char "A" && char <= #char "F" {
            continue;
        }
        if char >= #char "a" && char <= #char "f" {
            continue;
        }
        return false;
    }
    return true;
}

color_from_hex :: (color_string: string) -> Color {
    valid := is_valid_hex_color(color_string);
    Basic.assert(valid, "A color must contain # followed by six hexadecimal digits.");
    if !valid {
        return .{255, 0, 255, 255};
    }

    result: Color = ---;
    result.r = int_from_hex_char_for_colors(color_string[1]) * 16 + int_from_hex_char_for_colors(color_string[2]);
    result.g = int_from_hex_char_for_colors(color_string[3]) * 16 + int_from_hex_char_for_colors(color_string[4]);
    result.b = int_from_hex_char_for_colors(color_string[5]) * 16 + int_from_hex_char_for_colors(color_string[6]);
    result.a = 255;
    return result;
}

// Hex colors are authored in sRGB. Decode RGB with the accurate transfer;
// alpha describes coverage and is passed through unchanged.
linear_from_hex :: (color_string: string, alpha: float = 1.0) -> Linear_Color {
    encoded := get_float_representation(color_from_hex(color_string));
    encoded.a = alpha;
    return srgb_to_linear_accurate(encoded);
}

get_float_representation :: (color: Color) -> Vec4 {
    v: Vec4 = ---;

    one_over_255 :: 1.0 / 255.0;

    v.r = cast(float) (color.r) * one_over_255;
    v.g = cast(float) (color.g) * one_over_255;
    v.b = cast(float) (color.b) * one_over_255;
    v.a = cast(float) (color.a) * one_over_255;

    return v;
}

color_from_float_representation :: (using float_color: Vec4) -> Color {
    c: Color = ---;

    c.r = cast(u8)(clamp01(r) * 255.0 + 0.5);
    c.g = cast(u8)(clamp01(g) * 255.0 + 0.5);
    c.b = cast(u8)(clamp01(b) * 255.0 + 0.5);
    c.a = cast(u8)(clamp01(a) * 255.0 + 0.5);

    return c;
}

make_linear_color :: (r: float, g: float, b: float, a: float) -> Linear_Color { return .{r, g, b, a}; }
make_linear_color :: (r: float, g: float, b: float) -> Linear_Color { return .{r, g, b, 1.0}; }
make_linear_color :: (using rgb: Vec3)           -> Linear_Color { return .{x, y, z, 1.0}; }
make_linear_color :: (using rgb: Vec3, a: float) -> Linear_Color { return .{x, y, z, a}; }

srgb_channel_to_linear :: (c: float) -> float {
    if c <= 0.04045 {
        return c / 12.92;
    }

    return pow((c + 0.055) / 1.055, 2.4);
}

linear_channel_to_srgb :: (c: float) -> float {
    if c <= 0.0031308 {
        return c * 12.92;
    }

    return 1.055 * pow(c, 1.0 / 2.4) - 0.055;
}

srgb_to_linear_accurate :: (value: float) -> float { return srgb_channel_to_linear(value); }
srgb_to_linear_accurate :: (using color: Vec4) -> Linear_Color {
    return .{
        srgb_channel_to_linear(r),
        srgb_channel_to_linear(g),
        srgb_channel_to_linear(b),
        a
    };
}

linear_to_srgb_vec4_accurate :: (using color: Linear_Color) -> Vec4 {
    return .{
        linear_channel_to_srgb(r),
        linear_channel_to_srgb(g),
        linear_channel_to_srgb(b),
        a
    };
}

linear_to_srgb_color_accurate :: (color: Linear_Color) -> Color {
    return color_from_float_representation(linear_to_srgb_vec4_accurate(color));
}

srgb_to_linear_lazy :: (value: float) -> float { return value * value; }
srgb_to_linear_lazy :: (using srgb_color: Vec4) -> Linear_Color { return .{r * r, g * g, b * b, a}; }
srgb_to_linear_lazy :: (srgb_color: Color) -> Linear_Color { return srgb_to_linear_lazy(get_float_representation(srgb_color)); }

linear_to_srgb_vec4_lazy :: (using linear: Linear_Color) -> Vec4 { return .{sqrt(r), sqrt(g), sqrt(b), a}; }
linear_to_srgb_color_lazy :: (linear: Linear_Color) -> Color { return color_from_float_representation(linear_to_srgb_vec4_lazy(linear)); }

srgb_to_linear :: srgb_to_linear_lazy;
linear_to_srgb :: linear_to_srgb_color_lazy;
linear_to_srgb_color :: linear_to_srgb_color_lazy;
linear_to_srgb_vec4 :: linear_to_srgb_vec4_lazy;

premultiply_alpha_srgb :: (srgb_color: Vec4) -> Vec4 {
    linear_color := srgb_to_linear(srgb_color);

    linear_color.rgb *= linear_color.a;

    return linear_to_srgb_vec4(linear_color);
}

premultiply_alpha_srgb :: (srgb_color: Color) -> Color {
    linear_color := srgb_to_linear(color);

    linear_color.rgb *= linear_color.a;

    return linear_to_srgb_color(linear_color);
}

premultiply_alpha_linear :: (linear: Linear_Color) -> Linear_Color {
    return xyzw(linear.rgb * linear.a, linear.a);
}

// R in bits 0..4, G in 5..10, B in 11..15. No color-space conversion.
// Float channels are clamped to [0, 1] and rounded to the nearest stored value.
pack_rgb_to_u16 :: (color: Vec3) -> u16 {
    r := cast(u16)(clamp01(color.x) * 31.0 + 0.5);
    g := cast(u16)(clamp01(color.y) * 63.0 + 0.5);
    b := cast(u16)(clamp01(color.z) * 31.0 + 0.5);
    return r | (g << 5) | (b << 11);
}

pack_rgb_to_u16 :: (color: Vec4) -> u16 {
    return pack_rgb_to_u16(color.rgb);
}

pack_rgb_to_u16 :: (color: Color) -> u16 {
    return pack_rgb_to_u16(get_float_representation(color));
}

// Decode R5/G6/B5
unpack_rgb_u16_to_vec3 :: (packed: u16) -> Vec3 {
    r := packed & 31;
    g := (packed >> 5) & 63;
    b := (packed >> 11) & 31;
    return .{cast(float) r * (1.0 / 31.0),
             cast(float) g * (1.0 / 63.0),
             cast(float) b * (1.0 / 31.0)};
}

unpack_rgb_u16_to_vec4 :: (packed: u16) -> Vec4 { return xyzw(unpack_rgb_u16_to_vec3(packed), 1.0); }
unpack_rgb_u16_to_color :: (packed: u16) -> Color { return color_from_float_representation(unpack_rgb_u16_to_vec4(packed)); }

// RGB in bits 0..15, alpha in 16..23; bits 24..31 are zero.
pack_rgba_to_rgb16_a8 :: (color: Vec4) -> u32 {
    rgb := cast(u32) pack_rgb_to_u16(color);
    a := cast(u32)(clamp01(color.a) * 255.0 + 0.5);
    return rgb | (a << 16);
}

pack_rgba_to_rgb16_a8 :: (color: Color) -> u32 {
    rgb := cast(u32) pack_rgb_to_u16(color);
    a := cast(u32) color.a;
    return rgb | (a << 16);
}

// Decode RGB16 + A8; the unused upper byte is ignored.
unpack_rgb16_a8_to_vec4 :: (packed: u32) -> Vec4 {
    rgb := unpack_rgb_u16_to_vec3(cast(u16)(packed & 0xFFFF));
    alpha_u32 := (packed >> 16) & 255;
    a := cast(float)(alpha_u32) / 255.0;
    return xyzw(rgb, a);
}

unpack_rgb16_a8_to_color :: (packed: u32) -> Color {
    color := unpack_rgb_u16_to_color(cast(u16)(packed & 0xFFFF));
    color.a = cast(u8)((packed >> 16) & 255);
    return color;
}

pack_rgba_to_u32 :: (using color: $T/interface Vec4) -> u32 {
    r_int := cast(u32)(clamp01(r) * 255.0 + 0.5);
    g_int := cast(u32)(clamp01(g) * 255.0 + 0.5);
    b_int := cast(u32)(clamp01(b) * 255.0 + 0.5);
    a_int := cast(u32)(clamp01(a) * 255.0 + 0.5);

    result := r_int | (g_int << 8) | (b_int << 16) | (a_int << 24);

    return result;
}

unpack_rgba_from_u32 :: (packed: u32) -> Vec4 {
    r_int := packed & 255;
    g_int := (packed >> 8) & 255;
    b_int := (packed >> 16) & 255;
    a_int := (packed >> 24) & 255;

    one_over_255 :: 1.0 / 255.0;

    color: Vec4 = ---;

    color.r = cast(float)(r_int) * one_over_255;
    color.g = cast(float)(g_int) * one_over_255;
    color.b = cast(float)(b_int) * one_over_255;
    color.a = cast(float)(a_int) * one_over_255;

    return color;
}


#scope_module

RGBA_U64_RED_SHIFT :: 0;
RGBA_U64_GREEN_SHIFT :: 19;
RGBA_U64_BLUE_SHIFT :: 38;
RGBA_U64_ALPHA_SHIFT :: 56;
RGBA_U64_RED_FRACTION_BITS :: 14;
RGBA_U64_GREEN_FRACTION_BITS :: 14;
RGBA_U64_BLUE_FRACTION_BITS :: 13;
RGBA_U64_RGB_EXPONENT_BITS :: 5;
RGBA_U64_RGB_EXPONENT_BIAS :: 15;
RGBA_U64_RED_MAX: float : 65534.0;
RGBA_U64_GREEN_MAX: float : 65534.0;
RGBA_U64_BLUE_MAX: float : 65532.0;

#scope_export

// Linear HDR RGBA: R19/G19/B18/A8, from the least to the most significant bits.
// RGB uses unsigned floating point with five exponent bits and exponent bias 15.
// Exponents 1..30 are normal; exponent 0 has subnormals and no implicit leading bit.
// Exponent 31 is reserved and decodes to the channel's largest finite value.
pack_rgba_to_u64 :: (color: Linear_Color) -> u64 {
    r := cast(u64) pack_unsigned_linear_channel(color.r, RGBA_U64_RED_FRACTION_BITS);
    g := cast(u64) pack_unsigned_linear_channel(color.g, RGBA_U64_GREEN_FRACTION_BITS);
    b := cast(u64) pack_unsigned_linear_channel(color.b, RGBA_U64_BLUE_FRACTION_BITS);
    a := cast(u64) pack_linear_alpha(color.a);
    return (r << RGBA_U64_RED_SHIFT) | (g << RGBA_U64_GREEN_SHIFT)
        | (b << RGBA_U64_BLUE_SHIFT) | (a << RGBA_U64_ALPHA_SHIFT);
}

unpack_rgba_from_u64 :: (packed: u64) -> Linear_Color {
    r := cast(u32) ((packed >> RGBA_U64_RED_SHIFT) & 0x7ffff);
    g := cast(u32) ((packed >> RGBA_U64_GREEN_SHIFT) & 0x7ffff);
    b := cast(u32) ((packed >> RGBA_U64_BLUE_SHIFT) & 0x3ffff);
    a := cast(u32) (packed >> RGBA_U64_ALPHA_SHIFT);
    return .{
        unpack_unsigned_linear_channel(r, RGBA_U64_RED_FRACTION_BITS),
        unpack_unsigned_linear_channel(g, RGBA_U64_GREEN_FRACTION_BITS),
        unpack_unsigned_linear_channel(b, RGBA_U64_BLUE_FRACTION_BITS),
        cast(float) a * (1.0 / 255.0)
    };
}

#scope_module

round_color_bits_to_even :: (value: u32, shift: int) -> u32 {
    result := value >> shift;
    remainder := value & ((cast(u32) 1 << shift) - 1);
    half := cast(u32) 1 << (shift - 1);
    if remainder > half || (remainder == half && (result & 1) != 0) {
        result += 1;
    }
    return result;
}

pack_unsigned_linear_channel :: (value: float, $fraction_bits: int) -> u32 {
    #assert fraction_bits == 14 || fraction_bits == 13;
    bits := (.*) cast(*u32) *value;
    source_exponent := (bits >> 23) & 0xff;
    source_fraction := bits & 0x7fffff;
    maximum := (cast(u32) 31 << fraction_bits) - 1;

    if (bits & 0x80000000) != 0 || source_exponent == 0 {
        return 0;
    }
    if source_exponent == 0xff {
        if source_fraction != 0 {
            return 0;
        }
        return maximum;
    }

    target_exponent := cast(int) source_exponent - 112;
    if target_exponent >= 31 {
        return maximum;
    }

    significand := source_fraction | 0x800000;
    if target_exponent <= 0 {
        shift := 136 - fraction_bits - cast(int) source_exponent;
        if shift > 24 {
            return 0;
        }
        return round_color_bits_to_even(significand, shift);
    }

    // Rounding the significand also carries into the next exponent, if needed.
    rounded := round_color_bits_to_even(significand, 23 - fraction_bits);
    encoded := (cast(u32) (target_exponent - 1) << fraction_bits) + rounded;
    if encoded > maximum {
        return maximum;
    }
    return encoded;
}

unpack_unsigned_linear_channel :: (encoded: u32, $fraction_bits: int) -> float {
    #assert fraction_bits == 14 || fraction_bits == 13;
    maximum := (cast(u32) 31 << fraction_bits) - 1;
    if encoded > maximum {
        encoded = maximum;
    }

    exponent := encoded >> fraction_bits;
    fraction := encoded & ((cast(u32) 1 << fraction_bits) - 1);
    bits: u32 = ---;
    if exponent == 0 {
        // The subnormal quantum is 2^(-14 - fraction_bits), exactly representable in float32.
        bits = cast(u32) (113 - fraction_bits) << 23;
        quantum := (.*) cast(*float) *bits;
        return cast(float) fraction * quantum;
    }

    bits = ((exponent + 112) << 23) | (fraction << (23 - fraction_bits));
    return (.*) cast(*float) *bits;
}

pack_linear_alpha :: (value: float) -> u32 {
    scaled := cast(float64) clamp01(value) * 255.0;
    return cast(u32) (scaled + 0.5);
}

#scope_export

Colors :: struct {
    // MAIN
    TRANSPARENT :: #run srgba(0, 0, 0, 0);
    WHITE :: #run srgb(255, 255, 255);
    RED :: #run srgb(255, 0, 0);
    GREEN :: #run srgb(0, 255, 0);
    BLUE :: #run srgb(0, 0, 255);
    BLACK :: #run srgb(0, 0, 0);
    ORANGE :: #run color_from_hex("#ED7014");
    ORANGE_RED :: #run color_from_hex("#FF4500");
    YELLOW :: #run color_from_hex("#FFFF00");
    CYAN :: #run color_from_hex("#00FFFF");
    MAGENTA :: #run color_from_hex("#FF00FF");
    PURPLE :: #run color_from_hex("#800080");
    PINK :: #run color_from_hex("#FFC0CB");
    BROWN :: #run color_from_hex("#A52A2A");

    // SECONDARY
    RED2 :: #run color_from_hex("#D30000");
    GREEN2 :: #run color_from_hex("#3CB043");
    BLUE2 :: #run color_from_hex("#3944BC");

    // OTHER red
    IMPERIAL :: #run color_from_hex("#ED2939");
    CRIMSON :: #run color_from_hex("#B80F0A");
    REDWOOD :: #run color_from_hex("#A45A52");
    MAROON :: #run color_from_hex("#800000");
    FIREBRICK :: #run color_from_hex("#B22222");
    TOMATO :: #run color_from_hex("#FF6347");
    SALMON :: #run color_from_hex("#FA8072");

    // OTHER orange
    BURLYWOOD :: #run color_from_hex("#DEB887");
    BURNT_ORANGE :: #run color_from_hex("#CC5500");
    RUST :: #run color_from_hex("#8D4004");
    HONEY :: #run color_from_hex("#EC9706");
    DARK_ORANGE :: #run color_from_hex("#FF8C00");
    CORAL :: #run color_from_hex("#FF7F50");
    PEACH_PUFF :: #run color_from_hex("#FFDAB9");

    // OTHER yellow
    GOLD :: #run color_from_hex("#FFD700");
    GOLDENROD :: #run color_from_hex("#DAA520");
    KHAKI :: #run color_from_hex("#F0E68C");
    LEMON_CHIFFON :: #run color_from_hex("#FFFACD");

    // OTHER blue
    SKY :: #run color_from_hex("#63C5DA");
    ARCTIC :: #run color_from_hex("#82EEFD");
    NAVY :: #run color_from_hex("#000080");
    ROYAL_BLUE :: #run color_from_hex("#4169E1");
    DODGER_BLUE :: #run color_from_hex("#1E90FF");
    STEEL_BLUE :: #run color_from_hex("#4682B4");
    CORNFLOWER_BLUE :: #run color_from_hex("#6495ED");
    LIGHT_BLUE :: #run color_from_hex("#ADD8E6");

    // OTHER cyan
    TEAL :: #run color_from_hex("#008080");
    TURQUOISE :: #run color_from_hex("#40E0D0");
    AQUAMARINE :: #run color_from_hex("#7FFFD4");

    // OTHER green
    PINE :: #run color_from_hex("#234F1E");
    PICKLE :: #run color_from_hex("#597D35");
    LIME :: #run color_from_hex("#AEF359");
    MINT :: #run color_from_hex("#99EDC3");
    OLIVE :: #run color_from_hex("#808000");
    FOREST_GREEN :: #run color_from_hex("#228B22");
    SEA_GREEN :: #run color_from_hex("#2E8B57");
    DARK_GREEN :: #run color_from_hex("#006400");

    // OTHER purple
    INDIGO :: #run color_from_hex("#4B0082");
    VIOLET :: #run color_from_hex("#EE82EE");
    ORCHID :: #run color_from_hex("#DA70D6");
    LAVENDER :: #run color_from_hex("#E6E6FA");
    PLUM :: #run color_from_hex("#DDA0DD");

    // OTHER pink
    HOT_PINK :: #run color_from_hex("#FF69B4");
    DEEP_PINK :: #run color_from_hex("#FF1493");
    LIGHT_PINK :: #run color_from_hex("#FFB6C1");

    // OTHER brown
    CHOCOLATE :: #run color_from_hex("#D2691E");
    SIENNA :: #run color_from_hex("#A0522D");
    SADDLE_BROWN :: #run color_from_hex("#8B4513");
    TAN :: #run color_from_hex("#D2B48C");

    // OTHER gray
    GRAY :: #run color_from_hex("#808080");
    SILVER :: #run color_from_hex("#C0C0C0");
    LIGHT_GRAY :: #run color_from_hex("#D3D3D3");
    DIM_GRAY :: #run color_from_hex("#696969");
    DARK_SLATE_GRAY :: #run color_from_hex("#2F4F4F");

    // OTHER white
    IVORY :: #run color_from_hex("#FFFFF0");
    SNOW :: #run color_from_hex("#FFFAFA");
    WHITE_SMOKE :: #run color_from_hex("#F5F5F5");
    BEIGE :: #run color_from_hex("#F5F5DC");
}

// Compile-time mirror of Colors. Adding a named sRGB swatch automatically adds its linear counterpart.
LinearColors :: struct {
    #insert #run make_linear_color_constants();
}

#scope_file
make_linear_color_constants :: () -> string {
    builder: Basic.String_Builder;
    for member: type_info(Colors).members {
        if member.flags & .CONSTANT && member.type == type_info(Color) {
            Basic.print_to_builder(*builder, "% :: #run srgb_to_linear_accurate(get_float_representation(Colors.%));\n", member.name, member.name);
        }
    }
    return Basic.builder_to_string(*builder);
}
