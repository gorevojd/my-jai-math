#scope_file
StandardMath :: #import "Math";

#scope_export
// Keep scalar overloads from Jai without importing its vector, matrix or constant names.
using,only(ceil, floor, sin, cos, tan, cot, asin, acos, atan, atan2, sqrt, pow, exp, log, frexp, ldexp) StandardMath;
