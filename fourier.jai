Fourier_Direction :: enum {
    Forward;
    Inverse;
}

/**
 * @brief Compute a discrete Fourier transform directly in O(N squared) time.
 *
 * @param[in] input Complex samples; any positive length N is supported.
 * @param[out] output Caller-owned buffer of length N, with no overlap with input.
 * @param[in] direction Forward (default) or Inverse transformation.
 * @return Empty string on success; a static error string on invalid arguments.
 *         Validation errors leave output untouched.
 *
 * @details
 * - Forward uses exp(-i * 2*pi*k*n/N) without normalization. Inverse uses the
 *   positive sign and divides by N, restoring input up to numerical error.
 * - Output uses natural bin order: DC first, then positive and negative frequencies.
 *   No windowing or fftshift is applied.
 * - Complex components are float32; phase angles and bin accumulation use float64
 *   to limit cancellation and round-trip error in the direct transform.
 * - No memory allocations occur; input is unchanged.
 *
 * @pre Samples must be finite and within the numerical range of the transform.
 *      Sample values are not validated.
 * @see fft_inplace
 */
dft :: (input: []Complex, output: []Complex, direction: Fourier_Direction = .Forward) -> string {
    error := validate_fourier_input(input, direction);
    if error.count > 0 {
        return error;
    }
    error = validate_fourier_output(input, output);
    if error.count > 0 {
        return error;
    }

    sign: float64 = -1.0;
    scale: float64 = 1.0;
    if direction == .Inverse {
        sign = 1.0;
        scale /= input.count;
    }
    for bin: 0..input.count - 1 {
        re, im: float64;
        phase_index: s64;
        for index: 0..input.count - 1 {
            // Modular addition avoids k*n overflow and keeps the angle within one turn.
            angle := sign * TAU64 * phase_index / input.count;
            cosine := cos(angle);
            sine := sin(angle);
            re += input[index].re * cosine - input[index].im * sine;
            im += input[index].re * sine + input[index].im * cosine;
            phase_index += bin;
            if phase_index >= input.count {
                phase_index -= input.count;
            }
        }
        output[bin].re = cast(float) (re * scale);
        output[bin].im = cast(float) (im * scale);
    }
    return "";
}

/**
 * @brief Transform a complex buffer in place using radix-2 FFT in O(N log N) time.
 *
 * @param[in,out] values Input samples replaced by the transform result. Length N
 *                      must be a positive power of two, including one.
 * @param[in] direction Forward (default) or Inverse transformation.
 * @return Empty string on success; a static error string on invalid arguments.
 *         Validation errors leave values untouched.
 *
 * @details
 * - Uses the same signs, normalization and natural bin order as dft.
 *   Only Inverse divides the result by N; no windowing or fftshift is applied.
 * - Requires O(1) extra storage and performs no memory allocations.
 * - Samples, phase angles and intermediate results use float32. Twiddle factors
 *   are computed directly without accumulating successive rotations.
 *
 * @pre Samples must be finite and within the numerical range of the transform.
 *      Sample values are not validated.
 * @see dft, fft
 */
fft_inplace :: (values: []Complex, direction: Fourier_Direction = .Forward) -> string {
    error := validate_fft_input(values, direction);
    if error.count > 0 {
        return error;
    }

    reversed: s64;
    for index: 0..values.count - 1 {
        if index < reversed {
            temporary := values[index];
            values[index] = values[reversed];
            values[reversed] = temporary;
        }

        bit := values.count >> 1;
        while bit > 0 && (reversed & bit) != 0 {
            reversed = reversed ^ bit;
            bit = bit >> 1;
        }
        reversed = reversed ^ bit;
    }

    sign: float = -1.0;
    if direction == .Inverse {
        sign = 1.0;
    }
    width: s64 = 2;
    while width <= values.count {
        half := width / 2;
        for offset: 0..half - 1 {
            angle := sign * TAU * offset / width;
            cosine := cos(angle);
            sine := sin(angle);
            index := offset;

            while index < values.count {
                even := values[index];
                odd := values[index + half];
                re := odd.re * cosine - odd.im * sine;
                im := odd.re * sine + odd.im * cosine;
                values[index].re = even.re + re;
                values[index].im = even.im + im;
                values[index + half].re = even.re - re;
                values[index + half].im = even.im - im;
                index += width;
            }
        }

        if width == values.count {
            break;
        }

        width *= 2;
    }

    if direction == .Inverse {
        scale: float = 1.0;
        scale /= values.count;
        for *value: values {
            value.re *= scale;
            value.im *= scale;
        }
    }

    return "";
}

/**
 * @brief Compute FFT into a separate buffer while preserving the input samples.
 *
 * @param[in] input Complex samples; length N must be a positive power of two.
 * @param[out] output Caller-owned buffer of length N, with no overlap with input.
 * @param[in] direction Forward (default) or Inverse transformation.
 * @return Empty string on success; a static error string on invalid arguments.
 *         Validation errors leave output untouched.
 *
 * @details
 * - Copies input to output and calls fft_inplace, sharing its implementation,
 *   normalization, precision and bin order. The copy costs O(N) time.
 * - Performs no memory allocations. Use fft_inplace when input may be overwritten.
 *
 * @pre Samples must be finite and within the numerical range of the transform.
 *      Sample values are not validated.
 * @see fft_inplace, dft
 */
fft :: (input: []Complex, output: []Complex, direction: Fourier_Direction = .Forward) -> string {
    error := validate_fft_input(input, direction);
    if error.count > 0 {
        return error;
    }
    error = validate_fourier_output(input, output);
    if error.count > 0 {
        return error;
    }
    memcpy(output.data, input.data, input.count * size_of(Complex));
    return fft_inplace(output, direction);
}

#scope_file

validate_fourier_output :: (input: []Complex, output: []Complex) -> string {
    if output.data == null || output.count != input.count {
        return "Fourier output must have the same positive length as input.";
    }
    bytes := (cast(u64) input.count) * size_of(Complex);
    input_address := cast(u64) input.data;
    output_address := cast(u64) output.data;
    if input_address <= output_address {
        if output_address - input_address < bytes {
            return "Fourier input and output must not overlap.";
        }
    }
    else if input_address - output_address < bytes {
        return "Fourier input and output must not overlap.";
    }
    return "";
}

validate_fft_input :: (values: []Complex, direction: Fourier_Direction) -> string {
    error := validate_fourier_input(values, direction);
    if error.count > 0 {
        return error;
    }
    if (values.count & (values.count - 1)) != 0 {
        return "FFT length must be a power of two.";
    }
    return "";
}

validate_fourier_input :: (values: []Complex, direction: Fourier_Direction) -> string {
    if values.data == null || values.count <= 0 {
        return "Fourier transform requires a nonempty buffer.";
    }
    if values.count > 0x7fffffffffffffff / size_of(Complex) {
        return "Fourier buffer exceeds the addressable size.";
    }
    if direction != .Forward && direction != .Inverse {
        return "Invalid Fourier transform direction.";
    }
    return "";
}
