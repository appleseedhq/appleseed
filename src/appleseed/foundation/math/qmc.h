
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

#pragma once

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/arch.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace foundation
{

//
// References:
//
//   http://www-stat.stanford.edu/~owen/reports/siggraph03.pdf
//   https://lirias.kuleuven.be/bitstream/123456789/131168/1/mcm2005_bartv.pdf
//
// todo:
//
//   implement specializations of Halton and Hammersley sequences generators for bases (2,3).
//   implement incremental radical inverse (for successive input values).
//   implement vectorized radical inverse functions with SSE2.
//   implement Sobol sequence generator.
//


//
// Base-2 radical inverse functions.
//
// All return values are in the interval [0, 1).
//

// Radical inverse in base 2.
template <typename T>
T radical_inverse_base2(
    const size_t        value);         // input digits

// Radical inverse in base 2, 32-bit version.
template <typename T>
T radical_inverse_base2_32(
    std::uint32_t       value);         // input digits

// Radical inverse in base 2, 64-bit version.
template <typename T>
T radical_inverse_base2_64(
    std::uint64_t       value);         // input digits

// Folded radical inverse in base 2.
template <typename T>
T folded_radical_inverse_base2(
    size_t              value);         // input digits


//
// Arbitrary-base radical inverse functions.
//
// All return values are in the interval [0, 1).
//

// Radical inverse in arbitrary base. No fast code path for base 2.
template <typename T>
T radical_inverse(
    const size_t        base,           // base (prime number)
    size_t              value);         // input digits

// Radical inverse in arbitrary base, with the base specified at compile-time.
template <typename T, size_t Base>
T static_radical_inverse(
    size_t              value);         // input digits

// Fast variant of radical_inverse() for the first 1000 prime bases.
template <typename T>
T fast_radical_inverse(
    const size_t        base_index,     // index of the prime base, starting at 0 (base 2)
    const size_t        value);         // input digits

// Folded radical inverse in arbitrary base. No fast code path for base 2.
template <typename T>
T folded_radical_inverse(
    const size_t        base,           // base (prime number)
    size_t              value);         // input digits

// Radical inverse in arbitrary base with digits permutation. No fast code path for base 2.
template <typename T>
T permuted_radical_inverse(
    const size_t        base,           // base (prime number)
    const size_t        perm[],         // digit permutation table (base entries)
    size_t              value);         // input digits

// Radical inverse in arbitrary base with digits permutation, with the base specified at compile-time.
template <typename T, size_t Base>
T static_permuted_radical_inverse(
    const size_t        perm[],         // digit permutation table (Base entries)
    size_t              value);         // input digits

// Fast variant of permuted_radical_inverse() for the first 1000 prime bases.
template <typename T>
T fast_permuted_radical_inverse(
    const size_t        base_index,     // index of the prime base, starting at 0 (base 2)
    const size_t        perm[],         // digit permutation table (Primes[base_index] entries)
    const size_t        value);         // input digits


//
// Halton sequences of arbitrary dimensions.
//
// All return values are in the interval [0, 1)^Dim.
//

// Return the i'th sample of a Halton sequence.
template <typename T, size_t Dim>
Vector<T, Dim> halton_sequence(
    const size_t        bases[],        // bases (Dim entries, prime numbers)
    const size_t        i);             // sample number

// Return the i'th sample of a Halton sequence with digits permutation.
template <typename T, size_t Dim>
Vector<T, Dim> halton_sequence(
    const size_t        bases[],        // bases (Dim entries, prime numbers)
    const size_t        perms[],        // permutation tables, one per dimension
    const size_t        i);             // sample number

// Return the i'th sample of a Halton-Zaremba sequence (using folded radical inverse).
template <typename T, size_t Dim>
Vector<T, Dim> halton_zaremba_sequence(
    const size_t        bases[],        // bases (Dim entries, prime numbers)
    const size_t        i);             // sample number


//
// The first N points of the 4D Halton sequence.
//

const size_t PrecomputedHaltonSequenceSize = 256;
extern const double PrecomputedHaltonSequence[4 * PrecomputedHaltonSequenceSize];


//
// Hammersley sequences of arbitrary dimensions.
//
// All return values are in the interval [0, 1)^Dim.
//
// todo: replace count argument by rcp_count?
//
//

// Return the i'th sample of a Hammersley sequence
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],        // bases (Dim-1 entries, prime numbers)
    const size_t        count,          // total number of samples in sequence
    const size_t        i);             // sample number

// Return the i'th sample of a Hammersley sequence with digits permutation.
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],        // bases (Dim-1 entries, prime numbers)
    const size_t        perms[],        // permutation tables, one per base
    const size_t        count,          // total number of samples in sequence
    const size_t        i);             // sample number

// Return the i'th sample of a Hammersley-Zaremba sequence (using folded radical inverse).
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_zaremba_sequence(
    const size_t        bases[],        // bases (Dim-1 entries, prime numbers)
    const size_t        count,          // total number of samples in sequence
    const size_t        i);             // sample number


//
// Base-2 radical inverse functions implementation.
//

template <typename T>
inline T radical_inverse_base2(
    const size_t        value)
{
#ifdef APPLESEED_ARCH64
    return radical_inverse_base2_64<T>(value);
#else
    return radical_inverse_base2_32<T>(value);
#endif
}

template <typename T>
inline T radical_inverse_base2_32(
    std::uint32_t       value)
{
    value = (value >> 16) | (value << 16);                                                      // 16-bit swap
    value = ((value & 0xFF00FF00u) >> 8) | ((value & 0x00FF00FFu) << 8);                        // 8-bit swap
    value = ((value & 0xF0F0F0F0u) >> 4) | ((value & 0x0F0F0F0Fu) << 4);                        // 4-bit swap
    value = ((value & 0xCCCCCCCCu) >> 2) | ((value & 0x33333333u) << 2);                        // 2-bit swap
    value = ((value & 0xAAAAAAAAu) >> 1) | ((value & 0x55555555u) << 1);                        // 1-bit swap

    const T result = value * Rcp2Pow32<T>();

    assert(result >= T(0.0));
    assert(result < T(1.0));

    return result;
}

template <typename T>
inline T radical_inverse_base2_64(
    std::uint64_t       value)
{
    value = (value >> 32) | (value << 32);                                                      // 32-bit swap
    value = ((value & 0xFFFF0000FFFF0000ull) >> 16) | ((value & 0x0000FFFF0000FFFFull) << 16);  // 16-bit swap
    value = ((value & 0xFF00FF00FF00FF00ull) >> 8)  | ((value & 0x00FF00FF00FF00FFull) << 8);   // 8-bit swap
    value = ((value & 0xF0F0F0F0F0F0F0F0ull) >> 4)  | ((value & 0x0F0F0F0F0F0F0F0Full) << 4);   // 4-bit swap
    value = ((value & 0xCCCCCCCCCCCCCCCCull) >> 2)  | ((value & 0x3333333333333333ull) << 2);   // 2-bit swap
    value = ((value & 0xAAAAAAAAAAAAAAAAull) >> 1)  | ((value & 0x5555555555555555ull) << 1);   // 1-bit swap

    const T result = value * Rcp2Pow64<T>();

    assert(result >= T(0.0));
    assert(result < T(1.0));

    return result;
}

template <typename T>
inline T folded_radical_inverse_base2(
    size_t              value)
{
    size_t offset = 0;
    T inverse = T(0.0);
    T b = T(0.5);

    while (inverse + b > inverse)
    {
        if ((value + offset) & 1)
            inverse += b;
        value >>= 1;
        b *= T(0.5);
        ++offset;
    }

    return inverse;
}


//
// Arbitrary-base radical inverse functions implementation.
//

template <typename T>
inline T radical_inverse(
    const size_t        base,
    size_t              value)
{
    assert(base >= 2);

    const T rcp_base = T(1.0) / base;

    T inverse = T(0.0);
    T b = rcp_base;

    while (value > 0)
    {
        const size_t digit = value % base;
        inverse += digit * b;
        value /= base;
        b *= rcp_base;
    }

    return inverse;
}

template <typename T, size_t Base>
inline T static_radical_inverse(
    size_t              value)
{
    static_assert(Base >= 2, "foundation::static_radical_inverse() expects Base >= 2");

    const T RcpBase = T(1.0) / Base;

    size_t x = 0;
    T b = T(1.0);

    while (value > 0)
    {
        const size_t digit = value % Base;
        x = x * Base + digit;
        value /= Base;
        b *= RcpBase;
    }

    return static_cast<T>(x) * b;
}

template <typename T>
T fast_radical_inverse(
    const size_t        base_index,
    const size_t        value)
{
    switch (base_index)
    {
      case   0: return radical_inverse_base2<T>(value);
      case   1: return static_radical_inverse<T, 3>(value);
      case   2: return static_radical_inverse<T, 5>(value);
      case   3: return static_radical_inverse<T, 7>(value);
      case   4: return static_radical_inverse<T, 11>(value);
      case   5: return static_radical_inverse<T, 13>(value);
      case   6: return static_radical_inverse<T, 17>(value);
      case   7: return static_radical_inverse<T, 19>(value);
      case   8: return static_radical_inverse<T, 23>(value);
      case   9: return static_radical_inverse<T, 29>(value);
      case  10: return static_radical_inverse<T, 31>(value);
      case  11: return static_radical_inverse<T, 37>(value);
      case  12: return static_radical_inverse<T, 41>(value);
      case  13: return static_radical_inverse<T, 43>(value);
      case  14: return static_radical_inverse<T, 47>(value);
      case  15: return static_radical_inverse<T, 53>(value);
      case  16: return static_radical_inverse<T, 59>(value);
      case  17: return static_radical_inverse<T, 61>(value);
      case  18: return static_radical_inverse<T, 67>(value);
      case  19: return static_radical_inverse<T, 71>(value);
      case  20: return static_radical_inverse<T, 73>(value);
      case  21: return static_radical_inverse<T, 79>(value);
      case  22: return static_radical_inverse<T, 83>(value);
      case  23: return static_radical_inverse<T, 89>(value);
      case  24: return static_radical_inverse<T, 97>(value);
      case  25: return static_radical_inverse<T, 101>(value);
      case  26: return static_radical_inverse<T, 103>(value);
      case  27: return static_radical_inverse<T, 107>(value);
      case  28: return static_radical_inverse<T, 109>(value);
      case  29: return static_radical_inverse<T, 113>(value);
      case  30: return static_radical_inverse<T, 127>(value);
      case  31: return static_radical_inverse<T, 131>(value);
      case  32: return static_radical_inverse<T, 137>(value);
      case  33: return static_radical_inverse<T, 139>(value);
      case  34: return static_radical_inverse<T, 149>(value);
      case  35: return static_radical_inverse<T, 151>(value);
      case  36: return static_radical_inverse<T, 157>(value);
      case  37: return static_radical_inverse<T, 163>(value);
      case  38: return static_radical_inverse<T, 167>(value);
      case  39: return static_radical_inverse<T, 173>(value);
      case  40: return static_radical_inverse<T, 179>(value);
      case  41: return static_radical_inverse<T, 181>(value);
      case  42: return static_radical_inverse<T, 191>(value);
      case  43: return static_radical_inverse<T, 193>(value);
      case  44: return static_radical_inverse<T, 197>(value);
      case  45: return static_radical_inverse<T, 199>(value);
      case  46: return static_radical_inverse<T, 211>(value);
      case  47: return static_radical_inverse<T, 223>(value);
      case  48: return static_radical_inverse<T, 227>(value);
      case  49: return static_radical_inverse<T, 229>(value);
      case  50: return static_radical_inverse<T, 233>(value);
      case  51: return static_radical_inverse<T, 239>(value);
      case  52: return static_radical_inverse<T, 241>(value);
      case  53: return static_radical_inverse<T, 251>(value);
      case  54: return static_radical_inverse<T, 257>(value);
      case  55: return static_radical_inverse<T, 263>(value);
      case  56: return static_radical_inverse<T, 269>(value);
      case  57: return static_radical_inverse<T, 271>(value);
      case  58: return static_radical_inverse<T, 277>(value);
      case  59: return static_radical_inverse<T, 281>(value);
      case  60: return static_radical_inverse<T, 283>(value);
      case  61: return static_radical_inverse<T, 293>(value);
      case  62: return static_radical_inverse<T, 307>(value);
      case  63: return static_radical_inverse<T, 311>(value);
      case  64: return static_radical_inverse<T, 313>(value);
      case  65: return static_radical_inverse<T, 317>(value);
      case  66: return static_radical_inverse<T, 331>(value);
      case  67: return static_radical_inverse<T, 337>(value);
      case  68: return static_radical_inverse<T, 347>(value);
      case  69: return static_radical_inverse<T, 349>(value);
      case  70: return static_radical_inverse<T, 353>(value);
      case  71: return static_radical_inverse<T, 359>(value);
      case  72: return static_radical_inverse<T, 367>(value);
      case  73: return static_radical_inverse<T, 373>(value);
      case  74: return static_radical_inverse<T, 379>(value);
      case  75: return static_radical_inverse<T, 383>(value);
      case  76: return static_radical_inverse<T, 389>(value);
      case  77: return static_radical_inverse<T, 397>(value);
      case  78: return static_radical_inverse<T, 401>(value);
      case  79: return static_radical_inverse<T, 409>(value);
      case  80: return static_radical_inverse<T, 419>(value);
      case  81: return static_radical_inverse<T, 421>(value);
      case  82: return static_radical_inverse<T, 431>(value);
      case  83: return static_radical_inverse<T, 433>(value);
      case  84: return static_radical_inverse<T, 439>(value);
      case  85: return static_radical_inverse<T, 443>(value);
      case  86: return static_radical_inverse<T, 449>(value);
      case  87: return static_radical_inverse<T, 457>(value);
      case  88: return static_radical_inverse<T, 461>(value);
      case  89: return static_radical_inverse<T, 463>(value);
      case  90: return static_radical_inverse<T, 467>(value);
      case  91: return static_radical_inverse<T, 479>(value);
      case  92: return static_radical_inverse<T, 487>(value);
      case  93: return static_radical_inverse<T, 491>(value);
      case  94: return static_radical_inverse<T, 499>(value);
      case  95: return static_radical_inverse<T, 503>(value);
      case  96: return static_radical_inverse<T, 509>(value);
      case  97: return static_radical_inverse<T, 521>(value);
      case  98: return static_radical_inverse<T, 523>(value);
      case  99: return static_radical_inverse<T, 541>(value);
      case 100: return static_radical_inverse<T, 547>(value);
      case 101: return static_radical_inverse<T, 557>(value);
      case 102: return static_radical_inverse<T, 563>(value);
      case 103: return static_radical_inverse<T, 569>(value);
      case 104: return static_radical_inverse<T, 571>(value);
      case 105: return static_radical_inverse<T, 577>(value);
      case 106: return static_radical_inverse<T, 587>(value);
      case 107: return static_radical_inverse<T, 593>(value);
      case 108: return static_radical_inverse<T, 599>(value);
      case 109: return static_radical_inverse<T, 601>(value);
      case 110: return static_radical_inverse<T, 607>(value);
      case 111: return static_radical_inverse<T, 613>(value);
      case 112: return static_radical_inverse<T, 617>(value);
      case 113: return static_radical_inverse<T, 619>(value);
      case 114: return static_radical_inverse<T, 631>(value);
      case 115: return static_radical_inverse<T, 641>(value);
      case 116: return static_radical_inverse<T, 643>(value);
      case 117: return static_radical_inverse<T, 647>(value);
      case 118: return static_radical_inverse<T, 653>(value);
      case 119: return static_radical_inverse<T, 659>(value);
      case 120: return static_radical_inverse<T, 661>(value);
      case 121: return static_radical_inverse<T, 673>(value);
      case 122: return static_radical_inverse<T, 677>(value);
      case 123: return static_radical_inverse<T, 683>(value);
      case 124: return static_radical_inverse<T, 691>(value);
      case 125: return static_radical_inverse<T, 701>(value);
      case 126: return static_radical_inverse<T, 709>(value);
      case 127: return static_radical_inverse<T, 719>(value);
      case 128: return static_radical_inverse<T, 727>(value);
      case 129: return static_radical_inverse<T, 733>(value);
      case 130: return static_radical_inverse<T, 739>(value);
      case 131: return static_radical_inverse<T, 743>(value);
      case 132: return static_radical_inverse<T, 751>(value);
      case 133: return static_radical_inverse<T, 757>(value);
      case 134: return static_radical_inverse<T, 761>(value);
      case 135: return static_radical_inverse<T, 769>(value);
      case 136: return static_radical_inverse<T, 773>(value);
      case 137: return static_radical_inverse<T, 787>(value);
      case 138: return static_radical_inverse<T, 797>(value);
      case 139: return static_radical_inverse<T, 809>(value);
      case 140: return static_radical_inverse<T, 811>(value);
      case 141: return static_radical_inverse<T, 821>(value);
      case 142: return static_radical_inverse<T, 823>(value);
      case 143: return static_radical_inverse<T, 827>(value);
      case 144: return static_radical_inverse<T, 829>(value);
      case 145: return static_radical_inverse<T, 839>(value);
      case 146: return static_radical_inverse<T, 853>(value);
      case 147: return static_radical_inverse<T, 857>(value);
      case 148: return static_radical_inverse<T, 859>(value);
      case 149: return static_radical_inverse<T, 863>(value);
      case 150: return static_radical_inverse<T, 877>(value);
      case 151: return static_radical_inverse<T, 881>(value);
      case 152: return static_radical_inverse<T, 883>(value);
      case 153: return static_radical_inverse<T, 887>(value);
      case 154: return static_radical_inverse<T, 907>(value);
      case 155: return static_radical_inverse<T, 911>(value);
      case 156: return static_radical_inverse<T, 919>(value);
      case 157: return static_radical_inverse<T, 929>(value);
      case 158: return static_radical_inverse<T, 937>(value);
      case 159: return static_radical_inverse<T, 941>(value);
      case 160: return static_radical_inverse<T, 947>(value);
      case 161: return static_radical_inverse<T, 953>(value);
      case 162: return static_radical_inverse<T, 967>(value);
      case 163: return static_radical_inverse<T, 971>(value);
      case 164: return static_radical_inverse<T, 977>(value);
      case 165: return static_radical_inverse<T, 983>(value);
      case 166: return static_radical_inverse<T, 991>(value);
      case 167: return static_radical_inverse<T, 997>(value);
      case 168: return static_radical_inverse<T, 1009>(value);
      case 169: return static_radical_inverse<T, 1013>(value);
      case 170: return static_radical_inverse<T, 1019>(value);
      case 171: return static_radical_inverse<T, 1021>(value);
      case 172: return static_radical_inverse<T, 1031>(value);
      case 173: return static_radical_inverse<T, 1033>(value);
      case 174: return static_radical_inverse<T, 1039>(value);
      case 175: return static_radical_inverse<T, 1049>(value);
      case 176: return static_radical_inverse<T, 1051>(value);
      case 177: return static_radical_inverse<T, 1061>(value);
      case 178: return static_radical_inverse<T, 1063>(value);
      case 179: return static_radical_inverse<T, 1069>(value);
      case 180: return static_radical_inverse<T, 1087>(value);
      case 181: return static_radical_inverse<T, 1091>(value);
      case 182: return static_radical_inverse<T, 1093>(value);
      case 183: return static_radical_inverse<T, 1097>(value);
      case 184: return static_radical_inverse<T, 1103>(value);
      case 185: return static_radical_inverse<T, 1109>(value);
      case 186: return static_radical_inverse<T, 1117>(value);
      case 187: return static_radical_inverse<T, 1123>(value);
      case 188: return static_radical_inverse<T, 1129>(value);
      case 189: return static_radical_inverse<T, 1151>(value);
      case 190: return static_radical_inverse<T, 1153>(value);
      case 191: return static_radical_inverse<T, 1163>(value);
      case 192: return static_radical_inverse<T, 1171>(value);
      case 193: return static_radical_inverse<T, 1181>(value);
      case 194: return static_radical_inverse<T, 1187>(value);
      case 195: return static_radical_inverse<T, 1193>(value);
      case 196: return static_radical_inverse<T, 1201>(value);
      case 197: return static_radical_inverse<T, 1213>(value);
      case 198: return static_radical_inverse<T, 1217>(value);
      case 199: return static_radical_inverse<T, 1223>(value);
      case 200: return static_radical_inverse<T, 1229>(value);
      case 201: return static_radical_inverse<T, 1231>(value);
      case 202: return static_radical_inverse<T, 1237>(value);
      case 203: return static_radical_inverse<T, 1249>(value);
      case 204: return static_radical_inverse<T, 1259>(value);
      case 205: return static_radical_inverse<T, 1277>(value);
      case 206: return static_radical_inverse<T, 1279>(value);
      case 207: return static_radical_inverse<T, 1283>(value);
      case 208: return static_radical_inverse<T, 1289>(value);
      case 209: return static_radical_inverse<T, 1291>(value);
      case 210: return static_radical_inverse<T, 1297>(value);
      case 211: return static_radical_inverse<T, 1301>(value);
      case 212: return static_radical_inverse<T, 1303>(value);
      case 213: return static_radical_inverse<T, 1307>(value);
      case 214: return static_radical_inverse<T, 1319>(value);
      case 215: return static_radical_inverse<T, 1321>(value);
      case 216: return static_radical_inverse<T, 1327>(value);
      case 217: return static_radical_inverse<T, 1361>(value);
      case 218: return static_radical_inverse<T, 1367>(value);
      case 219: return static_radical_inverse<T, 1373>(value);
      case 220: return static_radical_inverse<T, 1381>(value);
      case 221: return static_radical_inverse<T, 1399>(value);
      case 222: return static_radical_inverse<T, 1409>(value);
      case 223: return static_radical_inverse<T, 1423>(value);
      case 224: return static_radical_inverse<T, 1427>(value);
      case 225: return static_radical_inverse<T, 1429>(value);
      case 226: return static_radical_inverse<T, 1433>(value);
      case 227: return static_radical_inverse<T, 1439>(value);
      case 228: return static_radical_inverse<T, 1447>(value);
      case 229: return static_radical_inverse<T, 1451>(value);
      case 230: return static_radical_inverse<T, 1453>(value);
      case 231: return static_radical_inverse<T, 1459>(value);
      case 232: return static_radical_inverse<T, 1471>(value);
      case 233: return static_radical_inverse<T, 1481>(value);
      case 234: return static_radical_inverse<T, 1483>(value);
      case 235: return static_radical_inverse<T, 1487>(value);
      case 236: return static_radical_inverse<T, 1489>(value);
      case 237: return static_radical_inverse<T, 1493>(value);
      case 238: return static_radical_inverse<T, 1499>(value);
      case 239: return static_radical_inverse<T, 1511>(value);
      case 240: return static_radical_inverse<T, 1523>(value);
      case 241: return static_radical_inverse<T, 1531>(value);
      case 242: return static_radical_inverse<T, 1543>(value);
      case 243: return static_radical_inverse<T, 1549>(value);
      case 244: return static_radical_inverse<T, 1553>(value);
      case 245: return static_radical_inverse<T, 1559>(value);
      case 246: return static_radical_inverse<T, 1567>(value);
      case 247: return static_radical_inverse<T, 1571>(value);
      case 248: return static_radical_inverse<T, 1579>(value);
      case 249: return static_radical_inverse<T, 1583>(value);
      case 250: return static_radical_inverse<T, 1597>(value);
      case 251: return static_radical_inverse<T, 1601>(value);
      case 252: return static_radical_inverse<T, 1607>(value);
      case 253: return static_radical_inverse<T, 1609>(value);
      case 254: return static_radical_inverse<T, 1613>(value);
      case 255: return static_radical_inverse<T, 1619>(value);
      case 256: return static_radical_inverse<T, 1621>(value);
      case 257: return static_radical_inverse<T, 1627>(value);
      case 258: return static_radical_inverse<T, 1637>(value);
      case 259: return static_radical_inverse<T, 1657>(value);
      case 260: return static_radical_inverse<T, 1663>(value);
      case 261: return static_radical_inverse<T, 1667>(value);
      case 262: return static_radical_inverse<T, 1669>(value);
      case 263: return static_radical_inverse<T, 1693>(value);
      case 264: return static_radical_inverse<T, 1697>(value);
      case 265: return static_radical_inverse<T, 1699>(value);
      case 266: return static_radical_inverse<T, 1709>(value);
      case 267: return static_radical_inverse<T, 1721>(value);
      case 268: return static_radical_inverse<T, 1723>(value);
      case 269: return static_radical_inverse<T, 1733>(value);
      case 270: return static_radical_inverse<T, 1741>(value);
      case 271: return static_radical_inverse<T, 1747>(value);
      case 272: return static_radical_inverse<T, 1753>(value);
      case 273: return static_radical_inverse<T, 1759>(value);
      case 274: return static_radical_inverse<T, 1777>(value);
      case 275: return static_radical_inverse<T, 1783>(value);
      case 276: return static_radical_inverse<T, 1787>(value);
      case 277: return static_radical_inverse<T, 1789>(value);
      case 278: return static_radical_inverse<T, 1801>(value);
      case 279: return static_radical_inverse<T, 1811>(value);
      case 280: return static_radical_inverse<T, 1823>(value);
      case 281: return static_radical_inverse<T, 1831>(value);
      case 282: return static_radical_inverse<T, 1847>(value);
      case 283: return static_radical_inverse<T, 1861>(value);
      case 284: return static_radical_inverse<T, 1867>(value);
      case 285: return static_radical_inverse<T, 1871>(value);
      case 286: return static_radical_inverse<T, 1873>(value);
      case 287: return static_radical_inverse<T, 1877>(value);
      case 288: return static_radical_inverse<T, 1879>(value);
      case 289: return static_radical_inverse<T, 1889>(value);
      case 290: return static_radical_inverse<T, 1901>(value);
      case 291: return static_radical_inverse<T, 1907>(value);
      case 292: return static_radical_inverse<T, 1913>(value);
      case 293: return static_radical_inverse<T, 1931>(value);
      case 294: return static_radical_inverse<T, 1933>(value);
      case 295: return static_radical_inverse<T, 1949>(value);
      case 296: return static_radical_inverse<T, 1951>(value);
      case 297: return static_radical_inverse<T, 1973>(value);
      case 298: return static_radical_inverse<T, 1979>(value);
      case 299: return static_radical_inverse<T, 1987>(value);
      case 300: return static_radical_inverse<T, 1993>(value);
      case 301: return static_radical_inverse<T, 1997>(value);
      case 302: return static_radical_inverse<T, 1999>(value);
      case 303: return static_radical_inverse<T, 2003>(value);
      case 304: return static_radical_inverse<T, 2011>(value);
      case 305: return static_radical_inverse<T, 2017>(value);
      case 306: return static_radical_inverse<T, 2027>(value);
      case 307: return static_radical_inverse<T, 2029>(value);
      case 308: return static_radical_inverse<T, 2039>(value);
      case 309: return static_radical_inverse<T, 2053>(value);
      case 310: return static_radical_inverse<T, 2063>(value);
      case 311: return static_radical_inverse<T, 2069>(value);
      case 312: return static_radical_inverse<T, 2081>(value);
      case 313: return static_radical_inverse<T, 2083>(value);
      case 314: return static_radical_inverse<T, 2087>(value);
      case 315: return static_radical_inverse<T, 2089>(value);
      case 316: return static_radical_inverse<T, 2099>(value);
      case 317: return static_radical_inverse<T, 2111>(value);
      case 318: return static_radical_inverse<T, 2113>(value);
      case 319: return static_radical_inverse<T, 2129>(value);
      case 320: return static_radical_inverse<T, 2131>(value);
      case 321: return static_radical_inverse<T, 2137>(value);
      case 322: return static_radical_inverse<T, 2141>(value);
      case 323: return static_radical_inverse<T, 2143>(value);
      case 324: return static_radical_inverse<T, 2153>(value);
      case 325: return static_radical_inverse<T, 2161>(value);
      case 326: return static_radical_inverse<T, 2179>(value);
      case 327: return static_radical_inverse<T, 2203>(value);
      case 328: return static_radical_inverse<T, 2207>(value);
      case 329: return static_radical_inverse<T, 2213>(value);
      case 330: return static_radical_inverse<T, 2221>(value);
      case 331: return static_radical_inverse<T, 2237>(value);
      case 332: return static_radical_inverse<T, 2239>(value);
      case 333: return static_radical_inverse<T, 2243>(value);
      case 334: return static_radical_inverse<T, 2251>(value);
      case 335: return static_radical_inverse<T, 2267>(value);
      case 336: return static_radical_inverse<T, 2269>(value);
      case 337: return static_radical_inverse<T, 2273>(value);
      case 338: return static_radical_inverse<T, 2281>(value);
      case 339: return static_radical_inverse<T, 2287>(value);
      case 340: return static_radical_inverse<T, 2293>(value);
      case 341: return static_radical_inverse<T, 2297>(value);
      case 342: return static_radical_inverse<T, 2309>(value);
      case 343: return static_radical_inverse<T, 2311>(value);
      case 344: return static_radical_inverse<T, 2333>(value);
      case 345: return static_radical_inverse<T, 2339>(value);
      case 346: return static_radical_inverse<T, 2341>(value);
      case 347: return static_radical_inverse<T, 2347>(value);
      case 348: return static_radical_inverse<T, 2351>(value);
      case 349: return static_radical_inverse<T, 2357>(value);
      case 350: return static_radical_inverse<T, 2371>(value);
      case 351: return static_radical_inverse<T, 2377>(value);
      case 352: return static_radical_inverse<T, 2381>(value);
      case 353: return static_radical_inverse<T, 2383>(value);
      case 354: return static_radical_inverse<T, 2389>(value);
      case 355: return static_radical_inverse<T, 2393>(value);
      case 356: return static_radical_inverse<T, 2399>(value);
      case 357: return static_radical_inverse<T, 2411>(value);
      case 358: return static_radical_inverse<T, 2417>(value);
      case 359: return static_radical_inverse<T, 2423>(value);
      case 360: return static_radical_inverse<T, 2437>(value);
      case 361: return static_radical_inverse<T, 2441>(value);
      case 362: return static_radical_inverse<T, 2447>(value);
      case 363: return static_radical_inverse<T, 2459>(value);
      case 364: return static_radical_inverse<T, 2467>(value);
      case 365: return static_radical_inverse<T, 2473>(value);
      case 366: return static_radical_inverse<T, 2477>(value);
      case 367: return static_radical_inverse<T, 2503>(value);
      case 368: return static_radical_inverse<T, 2521>(value);
      case 369: return static_radical_inverse<T, 2531>(value);
      case 370: return static_radical_inverse<T, 2539>(value);
      case 371: return static_radical_inverse<T, 2543>(value);
      case 372: return static_radical_inverse<T, 2549>(value);
      case 373: return static_radical_inverse<T, 2551>(value);
      case 374: return static_radical_inverse<T, 2557>(value);
      case 375: return static_radical_inverse<T, 2579>(value);
      case 376: return static_radical_inverse<T, 2591>(value);
      case 377: return static_radical_inverse<T, 2593>(value);
      case 378: return static_radical_inverse<T, 2609>(value);
      case 379: return static_radical_inverse<T, 2617>(value);
      case 380: return static_radical_inverse<T, 2621>(value);
      case 381: return static_radical_inverse<T, 2633>(value);
      case 382: return static_radical_inverse<T, 2647>(value);
      case 383: return static_radical_inverse<T, 2657>(value);
      case 384: return static_radical_inverse<T, 2659>(value);
      case 385: return static_radical_inverse<T, 2663>(value);
      case 386: return static_radical_inverse<T, 2671>(value);
      case 387: return static_radical_inverse<T, 2677>(value);
      case 388: return static_radical_inverse<T, 2683>(value);
      case 389: return static_radical_inverse<T, 2687>(value);
      case 390: return static_radical_inverse<T, 2689>(value);
      case 391: return static_radical_inverse<T, 2693>(value);
      case 392: return static_radical_inverse<T, 2699>(value);
      case 393: return static_radical_inverse<T, 2707>(value);
      case 394: return static_radical_inverse<T, 2711>(value);
      case 395: return static_radical_inverse<T, 2713>(value);
      case 396: return static_radical_inverse<T, 2719>(value);
      case 397: return static_radical_inverse<T, 2729>(value);
      case 398: return static_radical_inverse<T, 2731>(value);
      case 399: return static_radical_inverse<T, 2741>(value);
      case 400: return static_radical_inverse<T, 2749>(value);
      case 401: return static_radical_inverse<T, 2753>(value);
      case 402: return static_radical_inverse<T, 2767>(value);
      case 403: return static_radical_inverse<T, 2777>(value);
      case 404: return static_radical_inverse<T, 2789>(value);
      case 405: return static_radical_inverse<T, 2791>(value);
      case 406: return static_radical_inverse<T, 2797>(value);
      case 407: return static_radical_inverse<T, 2801>(value);
      case 408: return static_radical_inverse<T, 2803>(value);
      case 409: return static_radical_inverse<T, 2819>(value);
      case 410: return static_radical_inverse<T, 2833>(value);
      case 411: return static_radical_inverse<T, 2837>(value);
      case 412: return static_radical_inverse<T, 2843>(value);
      case 413: return static_radical_inverse<T, 2851>(value);
      case 414: return static_radical_inverse<T, 2857>(value);
      case 415: return static_radical_inverse<T, 2861>(value);
      case 416: return static_radical_inverse<T, 2879>(value);
      case 417: return static_radical_inverse<T, 2887>(value);
      case 418: return static_radical_inverse<T, 2897>(value);
      case 419: return static_radical_inverse<T, 2903>(value);
      case 420: return static_radical_inverse<T, 2909>(value);
      case 421: return static_radical_inverse<T, 2917>(value);
      case 422: return static_radical_inverse<T, 2927>(value);
      case 423: return static_radical_inverse<T, 2939>(value);
      case 424: return static_radical_inverse<T, 2953>(value);
      case 425: return static_radical_inverse<T, 2957>(value);
      case 426: return static_radical_inverse<T, 2963>(value);
      case 427: return static_radical_inverse<T, 2969>(value);
      case 428: return static_radical_inverse<T, 2971>(value);
      case 429: return static_radical_inverse<T, 2999>(value);
      case 430: return static_radical_inverse<T, 3001>(value);
      case 431: return static_radical_inverse<T, 3011>(value);
      case 432: return static_radical_inverse<T, 3019>(value);
      case 433: return static_radical_inverse<T, 3023>(value);
      case 434: return static_radical_inverse<T, 3037>(value);
      case 435: return static_radical_inverse<T, 3041>(value);
      case 436: return static_radical_inverse<T, 3049>(value);
      case 437: return static_radical_inverse<T, 3061>(value);
      case 438: return static_radical_inverse<T, 3067>(value);
      case 439: return static_radical_inverse<T, 3079>(value);
      case 440: return static_radical_inverse<T, 3083>(value);
      case 441: return static_radical_inverse<T, 3089>(value);
      case 442: return static_radical_inverse<T, 3109>(value);
      case 443: return static_radical_inverse<T, 3119>(value);
      case 444: return static_radical_inverse<T, 3121>(value);
      case 445: return static_radical_inverse<T, 3137>(value);
      case 446: return static_radical_inverse<T, 3163>(value);
      case 447: return static_radical_inverse<T, 3167>(value);
      case 448: return static_radical_inverse<T, 3169>(value);
      case 449: return static_radical_inverse<T, 3181>(value);
      case 450: return static_radical_inverse<T, 3187>(value);
      case 451: return static_radical_inverse<T, 3191>(value);
      case 452: return static_radical_inverse<T, 3203>(value);
      case 453: return static_radical_inverse<T, 3209>(value);
      case 454: return static_radical_inverse<T, 3217>(value);
      case 455: return static_radical_inverse<T, 3221>(value);
      case 456: return static_radical_inverse<T, 3229>(value);
      case 457: return static_radical_inverse<T, 3251>(value);
      case 458: return static_radical_inverse<T, 3253>(value);
      case 459: return static_radical_inverse<T, 3257>(value);
      case 460: return static_radical_inverse<T, 3259>(value);
      case 461: return static_radical_inverse<T, 3271>(value);
      case 462: return static_radical_inverse<T, 3299>(value);
      case 463: return static_radical_inverse<T, 3301>(value);
      case 464: return static_radical_inverse<T, 3307>(value);
      case 465: return static_radical_inverse<T, 3313>(value);
      case 466: return static_radical_inverse<T, 3319>(value);
      case 467: return static_radical_inverse<T, 3323>(value);
      case 468: return static_radical_inverse<T, 3329>(value);
      case 469: return static_radical_inverse<T, 3331>(value);
      case 470: return static_radical_inverse<T, 3343>(value);
      case 471: return static_radical_inverse<T, 3347>(value);
      case 472: return static_radical_inverse<T, 3359>(value);
      case 473: return static_radical_inverse<T, 3361>(value);
      case 474: return static_radical_inverse<T, 3371>(value);
      case 475: return static_radical_inverse<T, 3373>(value);
      case 476: return static_radical_inverse<T, 3389>(value);
      case 477: return static_radical_inverse<T, 3391>(value);
      case 478: return static_radical_inverse<T, 3407>(value);
      case 479: return static_radical_inverse<T, 3413>(value);
      case 480: return static_radical_inverse<T, 3433>(value);
      case 481: return static_radical_inverse<T, 3449>(value);
      case 482: return static_radical_inverse<T, 3457>(value);
      case 483: return static_radical_inverse<T, 3461>(value);
      case 484: return static_radical_inverse<T, 3463>(value);
      case 485: return static_radical_inverse<T, 3467>(value);
      case 486: return static_radical_inverse<T, 3469>(value);
      case 487: return static_radical_inverse<T, 3491>(value);
      case 488: return static_radical_inverse<T, 3499>(value);
      case 489: return static_radical_inverse<T, 3511>(value);
      case 490: return static_radical_inverse<T, 3517>(value);
      case 491: return static_radical_inverse<T, 3527>(value);
      case 492: return static_radical_inverse<T, 3529>(value);
      case 493: return static_radical_inverse<T, 3533>(value);
      case 494: return static_radical_inverse<T, 3539>(value);
      case 495: return static_radical_inverse<T, 3541>(value);
      case 496: return static_radical_inverse<T, 3547>(value);
      case 497: return static_radical_inverse<T, 3557>(value);
      case 498: return static_radical_inverse<T, 3559>(value);
      case 499: return static_radical_inverse<T, 3571>(value);
      case 500: return static_radical_inverse<T, 3581>(value);
      case 501: return static_radical_inverse<T, 3583>(value);
      case 502: return static_radical_inverse<T, 3593>(value);
      case 503: return static_radical_inverse<T, 3607>(value);
      case 504: return static_radical_inverse<T, 3613>(value);
      case 505: return static_radical_inverse<T, 3617>(value);
      case 506: return static_radical_inverse<T, 3623>(value);
      case 507: return static_radical_inverse<T, 3631>(value);
      case 508: return static_radical_inverse<T, 3637>(value);
      case 509: return static_radical_inverse<T, 3643>(value);
      case 510: return static_radical_inverse<T, 3659>(value);
      case 511: return static_radical_inverse<T, 3671>(value);
      case 512: return static_radical_inverse<T, 3673>(value);
      case 513: return static_radical_inverse<T, 3677>(value);
      case 514: return static_radical_inverse<T, 3691>(value);
      case 515: return static_radical_inverse<T, 3697>(value);
      case 516: return static_radical_inverse<T, 3701>(value);
      case 517: return static_radical_inverse<T, 3709>(value);
      case 518: return static_radical_inverse<T, 3719>(value);
      case 519: return static_radical_inverse<T, 3727>(value);
      case 520: return static_radical_inverse<T, 3733>(value);
      case 521: return static_radical_inverse<T, 3739>(value);
      case 522: return static_radical_inverse<T, 3761>(value);
      case 523: return static_radical_inverse<T, 3767>(value);
      case 524: return static_radical_inverse<T, 3769>(value);
      case 525: return static_radical_inverse<T, 3779>(value);
      case 526: return static_radical_inverse<T, 3793>(value);
      case 527: return static_radical_inverse<T, 3797>(value);
      case 528: return static_radical_inverse<T, 3803>(value);
      case 529: return static_radical_inverse<T, 3821>(value);
      case 530: return static_radical_inverse<T, 3823>(value);
      case 531: return static_radical_inverse<T, 3833>(value);
      case 532: return static_radical_inverse<T, 3847>(value);
      case 533: return static_radical_inverse<T, 3851>(value);
      case 534: return static_radical_inverse<T, 3853>(value);
      case 535: return static_radical_inverse<T, 3863>(value);
      case 536: return static_radical_inverse<T, 3877>(value);
      case 537: return static_radical_inverse<T, 3881>(value);
      case 538: return static_radical_inverse<T, 3889>(value);
      case 539: return static_radical_inverse<T, 3907>(value);
      case 540: return static_radical_inverse<T, 3911>(value);
      case 541: return static_radical_inverse<T, 3917>(value);
      case 542: return static_radical_inverse<T, 3919>(value);
      case 543: return static_radical_inverse<T, 3923>(value);
      case 544: return static_radical_inverse<T, 3929>(value);
      case 545: return static_radical_inverse<T, 3931>(value);
      case 546: return static_radical_inverse<T, 3943>(value);
      case 547: return static_radical_inverse<T, 3947>(value);
      case 548: return static_radical_inverse<T, 3967>(value);
      case 549: return static_radical_inverse<T, 3989>(value);
      case 550: return static_radical_inverse<T, 4001>(value);
      case 551: return static_radical_inverse<T, 4003>(value);
      case 552: return static_radical_inverse<T, 4007>(value);
      case 553: return static_radical_inverse<T, 4013>(value);
      case 554: return static_radical_inverse<T, 4019>(value);
      case 555: return static_radical_inverse<T, 4021>(value);
      case 556: return static_radical_inverse<T, 4027>(value);
      case 557: return static_radical_inverse<T, 4049>(value);
      case 558: return static_radical_inverse<T, 4051>(value);
      case 559: return static_radical_inverse<T, 4057>(value);
      case 560: return static_radical_inverse<T, 4073>(value);
      case 561: return static_radical_inverse<T, 4079>(value);
      case 562: return static_radical_inverse<T, 4091>(value);
      case 563: return static_radical_inverse<T, 4093>(value);
      case 564: return static_radical_inverse<T, 4099>(value);
      case 565: return static_radical_inverse<T, 4111>(value);
      case 566: return static_radical_inverse<T, 4127>(value);
      case 567: return static_radical_inverse<T, 4129>(value);
      case 568: return static_radical_inverse<T, 4133>(value);
      case 569: return static_radical_inverse<T, 4139>(value);
      case 570: return static_radical_inverse<T, 4153>(value);
      case 571: return static_radical_inverse<T, 4157>(value);
      case 572: return static_radical_inverse<T, 4159>(value);
      case 573: return static_radical_inverse<T, 4177>(value);
      case 574: return static_radical_inverse<T, 4201>(value);
      case 575: return static_radical_inverse<T, 4211>(value);
      case 576: return static_radical_inverse<T, 4217>(value);
      case 577: return static_radical_inverse<T, 4219>(value);
      case 578: return static_radical_inverse<T, 4229>(value);
      case 579: return static_radical_inverse<T, 4231>(value);
      case 580: return static_radical_inverse<T, 4241>(value);
      case 581: return static_radical_inverse<T, 4243>(value);
      case 582: return static_radical_inverse<T, 4253>(value);
      case 583: return static_radical_inverse<T, 4259>(value);
      case 584: return static_radical_inverse<T, 4261>(value);
      case 585: return static_radical_inverse<T, 4271>(value);
      case 586: return static_radical_inverse<T, 4273>(value);
      case 587: return static_radical_inverse<T, 4283>(value);
      case 588: return static_radical_inverse<T, 4289>(value);
      case 589: return static_radical_inverse<T, 4297>(value);
      case 590: return static_radical_inverse<T, 4327>(value);
      case 591: return static_radical_inverse<T, 4337>(value);
      case 592: return static_radical_inverse<T, 4339>(value);
      case 593: return static_radical_inverse<T, 4349>(value);
      case 594: return static_radical_inverse<T, 4357>(value);
      case 595: return static_radical_inverse<T, 4363>(value);
      case 596: return static_radical_inverse<T, 4373>(value);
      case 597: return static_radical_inverse<T, 4391>(value);
      case 598: return static_radical_inverse<T, 4397>(value);
      case 599: return static_radical_inverse<T, 4409>(value);
      case 600: return static_radical_inverse<T, 4421>(value);
      case 601: return static_radical_inverse<T, 4423>(value);
      case 602: return static_radical_inverse<T, 4441>(value);
      case 603: return static_radical_inverse<T, 4447>(value);
      case 604: return static_radical_inverse<T, 4451>(value);
      case 605: return static_radical_inverse<T, 4457>(value);
      case 606: return static_radical_inverse<T, 4463>(value);
      case 607: return static_radical_inverse<T, 4481>(value);
      case 608: return static_radical_inverse<T, 4483>(value);
      case 609: return static_radical_inverse<T, 4493>(value);
      case 610: return static_radical_inverse<T, 4507>(value);
      case 611: return static_radical_inverse<T, 4513>(value);
      case 612: return static_radical_inverse<T, 4517>(value);
      case 613: return static_radical_inverse<T, 4519>(value);
      case 614: return static_radical_inverse<T, 4523>(value);
      case 615: return static_radical_inverse<T, 4547>(value);
      case 616: return static_radical_inverse<T, 4549>(value);
      case 617: return static_radical_inverse<T, 4561>(value);
      case 618: return static_radical_inverse<T, 4567>(value);
      case 619: return static_radical_inverse<T, 4583>(value);
      case 620: return static_radical_inverse<T, 4591>(value);
      case 621: return static_radical_inverse<T, 4597>(value);
      case 622: return static_radical_inverse<T, 4603>(value);
      case 623: return static_radical_inverse<T, 4621>(value);
      case 624: return static_radical_inverse<T, 4637>(value);
      case 625: return static_radical_inverse<T, 4639>(value);
      case 626: return static_radical_inverse<T, 4643>(value);
      case 627: return static_radical_inverse<T, 4649>(value);
      case 628: return static_radical_inverse<T, 4651>(value);
      case 629: return static_radical_inverse<T, 4657>(value);
      case 630: return static_radical_inverse<T, 4663>(value);
      case 631: return static_radical_inverse<T, 4673>(value);
      case 632: return static_radical_inverse<T, 4679>(value);
      case 633: return static_radical_inverse<T, 4691>(value);
      case 634: return static_radical_inverse<T, 4703>(value);
      case 635: return static_radical_inverse<T, 4721>(value);
      case 636: return static_radical_inverse<T, 4723>(value);
      case 637: return static_radical_inverse<T, 4729>(value);
      case 638: return static_radical_inverse<T, 4733>(value);
      case 639: return static_radical_inverse<T, 4751>(value);
      case 640: return static_radical_inverse<T, 4759>(value);
      case 641: return static_radical_inverse<T, 4783>(value);
      case 642: return static_radical_inverse<T, 4787>(value);
      case 643: return static_radical_inverse<T, 4789>(value);
      case 644: return static_radical_inverse<T, 4793>(value);
      case 645: return static_radical_inverse<T, 4799>(value);
      case 646: return static_radical_inverse<T, 4801>(value);
      case 647: return static_radical_inverse<T, 4813>(value);
      case 648: return static_radical_inverse<T, 4817>(value);
      case 649: return static_radical_inverse<T, 4831>(value);
      case 650: return static_radical_inverse<T, 4861>(value);
      case 651: return static_radical_inverse<T, 4871>(value);
      case 652: return static_radical_inverse<T, 4877>(value);
      case 653: return static_radical_inverse<T, 4889>(value);
      case 654: return static_radical_inverse<T, 4903>(value);
      case 655: return static_radical_inverse<T, 4909>(value);
      case 656: return static_radical_inverse<T, 4919>(value);
      case 657: return static_radical_inverse<T, 4931>(value);
      case 658: return static_radical_inverse<T, 4933>(value);
      case 659: return static_radical_inverse<T, 4937>(value);
      case 660: return static_radical_inverse<T, 4943>(value);
      case 661: return static_radical_inverse<T, 4951>(value);
      case 662: return static_radical_inverse<T, 4957>(value);
      case 663: return static_radical_inverse<T, 4967>(value);
      case 664: return static_radical_inverse<T, 4969>(value);
      case 665: return static_radical_inverse<T, 4973>(value);
      case 666: return static_radical_inverse<T, 4987>(value);
      case 667: return static_radical_inverse<T, 4993>(value);
      case 668: return static_radical_inverse<T, 4999>(value);
      case 669: return static_radical_inverse<T, 5003>(value);
      case 670: return static_radical_inverse<T, 5009>(value);
      case 671: return static_radical_inverse<T, 5011>(value);
      case 672: return static_radical_inverse<T, 5021>(value);
      case 673: return static_radical_inverse<T, 5023>(value);
      case 674: return static_radical_inverse<T, 5039>(value);
      case 675: return static_radical_inverse<T, 5051>(value);
      case 676: return static_radical_inverse<T, 5059>(value);
      case 677: return static_radical_inverse<T, 5077>(value);
      case 678: return static_radical_inverse<T, 5081>(value);
      case 679: return static_radical_inverse<T, 5087>(value);
      case 680: return static_radical_inverse<T, 5099>(value);
      case 681: return static_radical_inverse<T, 5101>(value);
      case 682: return static_radical_inverse<T, 5107>(value);
      case 683: return static_radical_inverse<T, 5113>(value);
      case 684: return static_radical_inverse<T, 5119>(value);
      case 685: return static_radical_inverse<T, 5147>(value);
      case 686: return static_radical_inverse<T, 5153>(value);
      case 687: return static_radical_inverse<T, 5167>(value);
      case 688: return static_radical_inverse<T, 5171>(value);
      case 689: return static_radical_inverse<T, 5179>(value);
      case 690: return static_radical_inverse<T, 5189>(value);
      case 691: return static_radical_inverse<T, 5197>(value);
      case 692: return static_radical_inverse<T, 5209>(value);
      case 693: return static_radical_inverse<T, 5227>(value);
      case 694: return static_radical_inverse<T, 5231>(value);
      case 695: return static_radical_inverse<T, 5233>(value);
      case 696: return static_radical_inverse<T, 5237>(value);
      case 697: return static_radical_inverse<T, 5261>(value);
      case 698: return static_radical_inverse<T, 5273>(value);
      case 699: return static_radical_inverse<T, 5279>(value);
      case 700: return static_radical_inverse<T, 5281>(value);
      case 701: return static_radical_inverse<T, 5297>(value);
      case 702: return static_radical_inverse<T, 5303>(value);
      case 703: return static_radical_inverse<T, 5309>(value);
      case 704: return static_radical_inverse<T, 5323>(value);
      case 705: return static_radical_inverse<T, 5333>(value);
      case 706: return static_radical_inverse<T, 5347>(value);
      case 707: return static_radical_inverse<T, 5351>(value);
      case 708: return static_radical_inverse<T, 5381>(value);
      case 709: return static_radical_inverse<T, 5387>(value);
      case 710: return static_radical_inverse<T, 5393>(value);
      case 711: return static_radical_inverse<T, 5399>(value);
      case 712: return static_radical_inverse<T, 5407>(value);
      case 713: return static_radical_inverse<T, 5413>(value);
      case 714: return static_radical_inverse<T, 5417>(value);
      case 715: return static_radical_inverse<T, 5419>(value);
      case 716: return static_radical_inverse<T, 5431>(value);
      case 717: return static_radical_inverse<T, 5437>(value);
      case 718: return static_radical_inverse<T, 5441>(value);
      case 719: return static_radical_inverse<T, 5443>(value);
      case 720: return static_radical_inverse<T, 5449>(value);
      case 721: return static_radical_inverse<T, 5471>(value);
      case 722: return static_radical_inverse<T, 5477>(value);
      case 723: return static_radical_inverse<T, 5479>(value);
      case 724: return static_radical_inverse<T, 5483>(value);
      case 725: return static_radical_inverse<T, 5501>(value);
      case 726: return static_radical_inverse<T, 5503>(value);
      case 727: return static_radical_inverse<T, 5507>(value);
      case 728: return static_radical_inverse<T, 5519>(value);
      case 729: return static_radical_inverse<T, 5521>(value);
      case 730: return static_radical_inverse<T, 5527>(value);
      case 731: return static_radical_inverse<T, 5531>(value);
      case 732: return static_radical_inverse<T, 5557>(value);
      case 733: return static_radical_inverse<T, 5563>(value);
      case 734: return static_radical_inverse<T, 5569>(value);
      case 735: return static_radical_inverse<T, 5573>(value);
      case 736: return static_radical_inverse<T, 5581>(value);
      case 737: return static_radical_inverse<T, 5591>(value);
      case 738: return static_radical_inverse<T, 5623>(value);
      case 739: return static_radical_inverse<T, 5639>(value);
      case 740: return static_radical_inverse<T, 5641>(value);
      case 741: return static_radical_inverse<T, 5647>(value);
      case 742: return static_radical_inverse<T, 5651>(value);
      case 743: return static_radical_inverse<T, 5653>(value);
      case 744: return static_radical_inverse<T, 5657>(value);
      case 745: return static_radical_inverse<T, 5659>(value);
      case 746: return static_radical_inverse<T, 5669>(value);
      case 747: return static_radical_inverse<T, 5683>(value);
      case 748: return static_radical_inverse<T, 5689>(value);
      case 749: return static_radical_inverse<T, 5693>(value);
      case 750: return static_radical_inverse<T, 5701>(value);
      case 751: return static_radical_inverse<T, 5711>(value);
      case 752: return static_radical_inverse<T, 5717>(value);
      case 753: return static_radical_inverse<T, 5737>(value);
      case 754: return static_radical_inverse<T, 5741>(value);
      case 755: return static_radical_inverse<T, 5743>(value);
      case 756: return static_radical_inverse<T, 5749>(value);
      case 757: return static_radical_inverse<T, 5779>(value);
      case 758: return static_radical_inverse<T, 5783>(value);
      case 759: return static_radical_inverse<T, 5791>(value);
      case 760: return static_radical_inverse<T, 5801>(value);
      case 761: return static_radical_inverse<T, 5807>(value);
      case 762: return static_radical_inverse<T, 5813>(value);
      case 763: return static_radical_inverse<T, 5821>(value);
      case 764: return static_radical_inverse<T, 5827>(value);
      case 765: return static_radical_inverse<T, 5839>(value);
      case 766: return static_radical_inverse<T, 5843>(value);
      case 767: return static_radical_inverse<T, 5849>(value);
      case 768: return static_radical_inverse<T, 5851>(value);
      case 769: return static_radical_inverse<T, 5857>(value);
      case 770: return static_radical_inverse<T, 5861>(value);
      case 771: return static_radical_inverse<T, 5867>(value);
      case 772: return static_radical_inverse<T, 5869>(value);
      case 773: return static_radical_inverse<T, 5879>(value);
      case 774: return static_radical_inverse<T, 5881>(value);
      case 775: return static_radical_inverse<T, 5897>(value);
      case 776: return static_radical_inverse<T, 5903>(value);
      case 777: return static_radical_inverse<T, 5923>(value);
      case 778: return static_radical_inverse<T, 5927>(value);
      case 779: return static_radical_inverse<T, 5939>(value);
      case 780: return static_radical_inverse<T, 5953>(value);
      case 781: return static_radical_inverse<T, 5981>(value);
      case 782: return static_radical_inverse<T, 5987>(value);
      case 783: return static_radical_inverse<T, 6007>(value);
      case 784: return static_radical_inverse<T, 6011>(value);
      case 785: return static_radical_inverse<T, 6029>(value);
      case 786: return static_radical_inverse<T, 6037>(value);
      case 787: return static_radical_inverse<T, 6043>(value);
      case 788: return static_radical_inverse<T, 6047>(value);
      case 789: return static_radical_inverse<T, 6053>(value);
      case 790: return static_radical_inverse<T, 6067>(value);
      case 791: return static_radical_inverse<T, 6073>(value);
      case 792: return static_radical_inverse<T, 6079>(value);
      case 793: return static_radical_inverse<T, 6089>(value);
      case 794: return static_radical_inverse<T, 6091>(value);
      case 795: return static_radical_inverse<T, 6101>(value);
      case 796: return static_radical_inverse<T, 6113>(value);
      case 797: return static_radical_inverse<T, 6121>(value);
      case 798: return static_radical_inverse<T, 6131>(value);
      case 799: return static_radical_inverse<T, 6133>(value);
      case 800: return static_radical_inverse<T, 6143>(value);
      case 801: return static_radical_inverse<T, 6151>(value);
      case 802: return static_radical_inverse<T, 6163>(value);
      case 803: return static_radical_inverse<T, 6173>(value);
      case 804: return static_radical_inverse<T, 6197>(value);
      case 805: return static_radical_inverse<T, 6199>(value);
      case 806: return static_radical_inverse<T, 6203>(value);
      case 807: return static_radical_inverse<T, 6211>(value);
      case 808: return static_radical_inverse<T, 6217>(value);
      case 809: return static_radical_inverse<T, 6221>(value);
      case 810: return static_radical_inverse<T, 6229>(value);
      case 811: return static_radical_inverse<T, 6247>(value);
      case 812: return static_radical_inverse<T, 6257>(value);
      case 813: return static_radical_inverse<T, 6263>(value);
      case 814: return static_radical_inverse<T, 6269>(value);
      case 815: return static_radical_inverse<T, 6271>(value);
      case 816: return static_radical_inverse<T, 6277>(value);
      case 817: return static_radical_inverse<T, 6287>(value);
      case 818: return static_radical_inverse<T, 6299>(value);
      case 819: return static_radical_inverse<T, 6301>(value);
      case 820: return static_radical_inverse<T, 6311>(value);
      case 821: return static_radical_inverse<T, 6317>(value);
      case 822: return static_radical_inverse<T, 6323>(value);
      case 823: return static_radical_inverse<T, 6329>(value);
      case 824: return static_radical_inverse<T, 6337>(value);
      case 825: return static_radical_inverse<T, 6343>(value);
      case 826: return static_radical_inverse<T, 6353>(value);
      case 827: return static_radical_inverse<T, 6359>(value);
      case 828: return static_radical_inverse<T, 6361>(value);
      case 829: return static_radical_inverse<T, 6367>(value);
      case 830: return static_radical_inverse<T, 6373>(value);
      case 831: return static_radical_inverse<T, 6379>(value);
      case 832: return static_radical_inverse<T, 6389>(value);
      case 833: return static_radical_inverse<T, 6397>(value);
      case 834: return static_radical_inverse<T, 6421>(value);
      case 835: return static_radical_inverse<T, 6427>(value);
      case 836: return static_radical_inverse<T, 6449>(value);
      case 837: return static_radical_inverse<T, 6451>(value);
      case 838: return static_radical_inverse<T, 6469>(value);
      case 839: return static_radical_inverse<T, 6473>(value);
      case 840: return static_radical_inverse<T, 6481>(value);
      case 841: return static_radical_inverse<T, 6491>(value);
      case 842: return static_radical_inverse<T, 6521>(value);
      case 843: return static_radical_inverse<T, 6529>(value);
      case 844: return static_radical_inverse<T, 6547>(value);
      case 845: return static_radical_inverse<T, 6551>(value);
      case 846: return static_radical_inverse<T, 6553>(value);
      case 847: return static_radical_inverse<T, 6563>(value);
      case 848: return static_radical_inverse<T, 6569>(value);
      case 849: return static_radical_inverse<T, 6571>(value);
      case 850: return static_radical_inverse<T, 6577>(value);
      case 851: return static_radical_inverse<T, 6581>(value);
      case 852: return static_radical_inverse<T, 6599>(value);
      case 853: return static_radical_inverse<T, 6607>(value);
      case 854: return static_radical_inverse<T, 6619>(value);
      case 855: return static_radical_inverse<T, 6637>(value);
      case 856: return static_radical_inverse<T, 6653>(value);
      case 857: return static_radical_inverse<T, 6659>(value);
      case 858: return static_radical_inverse<T, 6661>(value);
      case 859: return static_radical_inverse<T, 6673>(value);
      case 860: return static_radical_inverse<T, 6679>(value);
      case 861: return static_radical_inverse<T, 6689>(value);
      case 862: return static_radical_inverse<T, 6691>(value);
      case 863: return static_radical_inverse<T, 6701>(value);
      case 864: return static_radical_inverse<T, 6703>(value);
      case 865: return static_radical_inverse<T, 6709>(value);
      case 866: return static_radical_inverse<T, 6719>(value);
      case 867: return static_radical_inverse<T, 6733>(value);
      case 868: return static_radical_inverse<T, 6737>(value);
      case 869: return static_radical_inverse<T, 6761>(value);
      case 870: return static_radical_inverse<T, 6763>(value);
      case 871: return static_radical_inverse<T, 6779>(value);
      case 872: return static_radical_inverse<T, 6781>(value);
      case 873: return static_radical_inverse<T, 6791>(value);
      case 874: return static_radical_inverse<T, 6793>(value);
      case 875: return static_radical_inverse<T, 6803>(value);
      case 876: return static_radical_inverse<T, 6823>(value);
      case 877: return static_radical_inverse<T, 6827>(value);
      case 878: return static_radical_inverse<T, 6829>(value);
      case 879: return static_radical_inverse<T, 6833>(value);
      case 880: return static_radical_inverse<T, 6841>(value);
      case 881: return static_radical_inverse<T, 6857>(value);
      case 882: return static_radical_inverse<T, 6863>(value);
      case 883: return static_radical_inverse<T, 6869>(value);
      case 884: return static_radical_inverse<T, 6871>(value);
      case 885: return static_radical_inverse<T, 6883>(value);
      case 886: return static_radical_inverse<T, 6899>(value);
      case 887: return static_radical_inverse<T, 6907>(value);
      case 888: return static_radical_inverse<T, 6911>(value);
      case 889: return static_radical_inverse<T, 6917>(value);
      case 890: return static_radical_inverse<T, 6947>(value);
      case 891: return static_radical_inverse<T, 6949>(value);
      case 892: return static_radical_inverse<T, 6959>(value);
      case 893: return static_radical_inverse<T, 6961>(value);
      case 894: return static_radical_inverse<T, 6967>(value);
      case 895: return static_radical_inverse<T, 6971>(value);
      case 896: return static_radical_inverse<T, 6977>(value);
      case 897: return static_radical_inverse<T, 6983>(value);
      case 898: return static_radical_inverse<T, 6991>(value);
      case 899: return static_radical_inverse<T, 6997>(value);
      case 900: return static_radical_inverse<T, 7001>(value);
      case 901: return static_radical_inverse<T, 7013>(value);
      case 902: return static_radical_inverse<T, 7019>(value);
      case 903: return static_radical_inverse<T, 7027>(value);
      case 904: return static_radical_inverse<T, 7039>(value);
      case 905: return static_radical_inverse<T, 7043>(value);
      case 906: return static_radical_inverse<T, 7057>(value);
      case 907: return static_radical_inverse<T, 7069>(value);
      case 908: return static_radical_inverse<T, 7079>(value);
      case 909: return static_radical_inverse<T, 7103>(value);
      case 910: return static_radical_inverse<T, 7109>(value);
      case 911: return static_radical_inverse<T, 7121>(value);
      case 912: return static_radical_inverse<T, 7127>(value);
      case 913: return static_radical_inverse<T, 7129>(value);
      case 914: return static_radical_inverse<T, 7151>(value);
      case 915: return static_radical_inverse<T, 7159>(value);
      case 916: return static_radical_inverse<T, 7177>(value);
      case 917: return static_radical_inverse<T, 7187>(value);
      case 918: return static_radical_inverse<T, 7193>(value);
      case 919: return static_radical_inverse<T, 7207>(value);
      case 920: return static_radical_inverse<T, 7211>(value);
      case 921: return static_radical_inverse<T, 7213>(value);
      case 922: return static_radical_inverse<T, 7219>(value);
      case 923: return static_radical_inverse<T, 7229>(value);
      case 924: return static_radical_inverse<T, 7237>(value);
      case 925: return static_radical_inverse<T, 7243>(value);
      case 926: return static_radical_inverse<T, 7247>(value);
      case 927: return static_radical_inverse<T, 7253>(value);
      case 928: return static_radical_inverse<T, 7283>(value);
      case 929: return static_radical_inverse<T, 7297>(value);
      case 930: return static_radical_inverse<T, 7307>(value);
      case 931: return static_radical_inverse<T, 7309>(value);
      case 932: return static_radical_inverse<T, 7321>(value);
      case 933: return static_radical_inverse<T, 7331>(value);
      case 934: return static_radical_inverse<T, 7333>(value);
      case 935: return static_radical_inverse<T, 7349>(value);
      case 936: return static_radical_inverse<T, 7351>(value);
      case 937: return static_radical_inverse<T, 7369>(value);
      case 938: return static_radical_inverse<T, 7393>(value);
      case 939: return static_radical_inverse<T, 7411>(value);
      case 940: return static_radical_inverse<T, 7417>(value);
      case 941: return static_radical_inverse<T, 7433>(value);
      case 942: return static_radical_inverse<T, 7451>(value);
      case 943: return static_radical_inverse<T, 7457>(value);
      case 944: return static_radical_inverse<T, 7459>(value);
      case 945: return static_radical_inverse<T, 7477>(value);
      case 946: return static_radical_inverse<T, 7481>(value);
      case 947: return static_radical_inverse<T, 7487>(value);
      case 948: return static_radical_inverse<T, 7489>(value);
      case 949: return static_radical_inverse<T, 7499>(value);
      case 950: return static_radical_inverse<T, 7507>(value);
      case 951: return static_radical_inverse<T, 7517>(value);
      case 952: return static_radical_inverse<T, 7523>(value);
      case 953: return static_radical_inverse<T, 7529>(value);
      case 954: return static_radical_inverse<T, 7537>(value);
      case 955: return static_radical_inverse<T, 7541>(value);
      case 956: return static_radical_inverse<T, 7547>(value);
      case 957: return static_radical_inverse<T, 7549>(value);
      case 958: return static_radical_inverse<T, 7559>(value);
      case 959: return static_radical_inverse<T, 7561>(value);
      case 960: return static_radical_inverse<T, 7573>(value);
      case 961: return static_radical_inverse<T, 7577>(value);
      case 962: return static_radical_inverse<T, 7583>(value);
      case 963: return static_radical_inverse<T, 7589>(value);
      case 964: return static_radical_inverse<T, 7591>(value);
      case 965: return static_radical_inverse<T, 7603>(value);
      case 966: return static_radical_inverse<T, 7607>(value);
      case 967: return static_radical_inverse<T, 7621>(value);
      case 968: return static_radical_inverse<T, 7639>(value);
      case 969: return static_radical_inverse<T, 7643>(value);
      case 970: return static_radical_inverse<T, 7649>(value);
      case 971: return static_radical_inverse<T, 7669>(value);
      case 972: return static_radical_inverse<T, 7673>(value);
      case 973: return static_radical_inverse<T, 7681>(value);
      case 974: return static_radical_inverse<T, 7687>(value);
      case 975: return static_radical_inverse<T, 7691>(value);
      case 976: return static_radical_inverse<T, 7699>(value);
      case 977: return static_radical_inverse<T, 7703>(value);
      case 978: return static_radical_inverse<T, 7717>(value);
      case 979: return static_radical_inverse<T, 7723>(value);
      case 980: return static_radical_inverse<T, 7727>(value);
      case 981: return static_radical_inverse<T, 7741>(value);
      case 982: return static_radical_inverse<T, 7753>(value);
      case 983: return static_radical_inverse<T, 7757>(value);
      case 984: return static_radical_inverse<T, 7759>(value);
      case 985: return static_radical_inverse<T, 7789>(value);
      case 986: return static_radical_inverse<T, 7793>(value);
      case 987: return static_radical_inverse<T, 7817>(value);
      case 988: return static_radical_inverse<T, 7823>(value);
      case 989: return static_radical_inverse<T, 7829>(value);
      case 990: return static_radical_inverse<T, 7841>(value);
      case 991: return static_radical_inverse<T, 7853>(value);
      case 992: return static_radical_inverse<T, 7867>(value);
      case 993: return static_radical_inverse<T, 7873>(value);
      case 994: return static_radical_inverse<T, 7877>(value);
      case 995: return static_radical_inverse<T, 7879>(value);
      case 996: return static_radical_inverse<T, 7883>(value);
      case 997: return static_radical_inverse<T, 7901>(value);
      case 998: return static_radical_inverse<T, 7907>(value);
      case 999: return static_radical_inverse<T, 7919>(value);
      default:
#if defined NDEBUG && defined _MSC_VER
        __assume(0);
#else
        assert(false);
        return T(0.0);
#endif
    }
}

template <typename T>
inline T folded_radical_inverse(
    const size_t        base,
    size_t              value)
{
    assert(base >= 2);

    const T rcp_base = T(1.0) / base;

    size_t offset = 0;
    T inverse = T(0.0);
    T b = rcp_base;

    while (inverse + (base - 1) * b > inverse)
    {
        const size_t digit = (value + offset) % base;
        inverse += digit * b;
        value /= base;
        b *= rcp_base;
        ++offset;
    }

    return inverse;
}

template <typename T>
inline T permuted_radical_inverse(
    const size_t        base,
    const size_t        perm[],
    size_t              value)
{
    assert(base >= 2);

    const T rcp_base = T(1.0) / base;

    T inverse = T(0.0);
    T b = rcp_base;

    while (value > 0)
    {
        const size_t digit = value % base;
        inverse += perm[digit] * b;
        value /= base;
        b *= rcp_base;
    }

    //
    // Because the loop above stops when the value reaches 0,
    // a term will be missing if 0 is permuted to another value
    // than 0, i.e. if perm[0] is not 0. The expression for this
    // missing term T is:
    //
    //   T = perm[0] * b +
    //       perm[0] * b / base +
    //       perm[0] * b / base^2 +
    //       ...
    //     = perm[0] * b * (1 + 1/base + 1/base^2 + ...)
    //     = perm[0] * b * base / (base - 1)
    //

    if (perm[0] != 0)
        inverse += perm[0] * b * base / (base - 1);

    return inverse;
}

template <typename T, size_t Base>
inline T static_permuted_radical_inverse(
    const size_t        perm[],
    size_t              value)
{
    static_assert(Base >= 2, "foundation::static_permuted_radical_inverse() expects Base >= 2");

    const T RcpBase = T(1.0) / Base;

    size_t x = 0;
    T b = T(1.0);

    while (value > 0)
    {
        const size_t digit = value % Base;
        x = x * Base + perm[digit];
        value /= Base;
        b *= RcpBase;
    }

    T inverse = static_cast<T>(x) * b;

    //
    // Because the loop above stops when the value reaches 0,
    // a term will be missing if 0 is permuted to another value
    // than 0, i.e. if perm[0] is not 0. The expression for this
    // missing term T is:
    //
    //   T = perm[0] * b +
    //       perm[0] * b / base +
    //       perm[0] * b / base^2 +
    //       ...
    //     = perm[0] * b * (1 + 1/base + 1/base^2 + ...)
    //     = perm[0] * b * base / (base - 1)
    //

    if (perm[0] != 0)
        inverse += perm[0] * b * Base / (Base - 1);

    return inverse;
}

template <typename T>
T fast_permuted_radical_inverse(
    const size_t        base_index,
    const size_t        perm[],
    const size_t        value)
{
    switch (base_index)
    {
      case   0: return radical_inverse_base2<T>(value ^ (~perm[0] + 1));
      case   1: return static_permuted_radical_inverse<T, 3>(perm, value);
      case   2: return static_permuted_radical_inverse<T, 5>(perm, value);
      case   3: return static_permuted_radical_inverse<T, 7>(perm, value);
      case   4: return static_permuted_radical_inverse<T, 11>(perm, value);
      case   5: return static_permuted_radical_inverse<T, 13>(perm, value);
      case   6: return static_permuted_radical_inverse<T, 17>(perm, value);
      case   7: return static_permuted_radical_inverse<T, 19>(perm, value);
      case   8: return static_permuted_radical_inverse<T, 23>(perm, value);
      case   9: return static_permuted_radical_inverse<T, 29>(perm, value);
      case  10: return static_permuted_radical_inverse<T, 31>(perm, value);
      case  11: return static_permuted_radical_inverse<T, 37>(perm, value);
      case  12: return static_permuted_radical_inverse<T, 41>(perm, value);
      case  13: return static_permuted_radical_inverse<T, 43>(perm, value);
      case  14: return static_permuted_radical_inverse<T, 47>(perm, value);
      case  15: return static_permuted_radical_inverse<T, 53>(perm, value);
      case  16: return static_permuted_radical_inverse<T, 59>(perm, value);
      case  17: return static_permuted_radical_inverse<T, 61>(perm, value);
      case  18: return static_permuted_radical_inverse<T, 67>(perm, value);
      case  19: return static_permuted_radical_inverse<T, 71>(perm, value);
      case  20: return static_permuted_radical_inverse<T, 73>(perm, value);
      case  21: return static_permuted_radical_inverse<T, 79>(perm, value);
      case  22: return static_permuted_radical_inverse<T, 83>(perm, value);
      case  23: return static_permuted_radical_inverse<T, 89>(perm, value);
      case  24: return static_permuted_radical_inverse<T, 97>(perm, value);
      case  25: return static_permuted_radical_inverse<T, 101>(perm, value);
      case  26: return static_permuted_radical_inverse<T, 103>(perm, value);
      case  27: return static_permuted_radical_inverse<T, 107>(perm, value);
      case  28: return static_permuted_radical_inverse<T, 109>(perm, value);
      case  29: return static_permuted_radical_inverse<T, 113>(perm, value);
      case  30: return static_permuted_radical_inverse<T, 127>(perm, value);
      case  31: return static_permuted_radical_inverse<T, 131>(perm, value);
      case  32: return static_permuted_radical_inverse<T, 137>(perm, value);
      case  33: return static_permuted_radical_inverse<T, 139>(perm, value);
      case  34: return static_permuted_radical_inverse<T, 149>(perm, value);
      case  35: return static_permuted_radical_inverse<T, 151>(perm, value);
      case  36: return static_permuted_radical_inverse<T, 157>(perm, value);
      case  37: return static_permuted_radical_inverse<T, 163>(perm, value);
      case  38: return static_permuted_radical_inverse<T, 167>(perm, value);
      case  39: return static_permuted_radical_inverse<T, 173>(perm, value);
      case  40: return static_permuted_radical_inverse<T, 179>(perm, value);
      case  41: return static_permuted_radical_inverse<T, 181>(perm, value);
      case  42: return static_permuted_radical_inverse<T, 191>(perm, value);
      case  43: return static_permuted_radical_inverse<T, 193>(perm, value);
      case  44: return static_permuted_radical_inverse<T, 197>(perm, value);
      case  45: return static_permuted_radical_inverse<T, 199>(perm, value);
      case  46: return static_permuted_radical_inverse<T, 211>(perm, value);
      case  47: return static_permuted_radical_inverse<T, 223>(perm, value);
      case  48: return static_permuted_radical_inverse<T, 227>(perm, value);
      case  49: return static_permuted_radical_inverse<T, 229>(perm, value);
      case  50: return static_permuted_radical_inverse<T, 233>(perm, value);
      case  51: return static_permuted_radical_inverse<T, 239>(perm, value);
      case  52: return static_permuted_radical_inverse<T, 241>(perm, value);
      case  53: return static_permuted_radical_inverse<T, 251>(perm, value);
      case  54: return static_permuted_radical_inverse<T, 257>(perm, value);
      case  55: return static_permuted_radical_inverse<T, 263>(perm, value);
      case  56: return static_permuted_radical_inverse<T, 269>(perm, value);
      case  57: return static_permuted_radical_inverse<T, 271>(perm, value);
      case  58: return static_permuted_radical_inverse<T, 277>(perm, value);
      case  59: return static_permuted_radical_inverse<T, 281>(perm, value);
      case  60: return static_permuted_radical_inverse<T, 283>(perm, value);
      case  61: return static_permuted_radical_inverse<T, 293>(perm, value);
      case  62: return static_permuted_radical_inverse<T, 307>(perm, value);
      case  63: return static_permuted_radical_inverse<T, 311>(perm, value);
      case  64: return static_permuted_radical_inverse<T, 313>(perm, value);
      case  65: return static_permuted_radical_inverse<T, 317>(perm, value);
      case  66: return static_permuted_radical_inverse<T, 331>(perm, value);
      case  67: return static_permuted_radical_inverse<T, 337>(perm, value);
      case  68: return static_permuted_radical_inverse<T, 347>(perm, value);
      case  69: return static_permuted_radical_inverse<T, 349>(perm, value);
      case  70: return static_permuted_radical_inverse<T, 353>(perm, value);
      case  71: return static_permuted_radical_inverse<T, 359>(perm, value);
      case  72: return static_permuted_radical_inverse<T, 367>(perm, value);
      case  73: return static_permuted_radical_inverse<T, 373>(perm, value);
      case  74: return static_permuted_radical_inverse<T, 379>(perm, value);
      case  75: return static_permuted_radical_inverse<T, 383>(perm, value);
      case  76: return static_permuted_radical_inverse<T, 389>(perm, value);
      case  77: return static_permuted_radical_inverse<T, 397>(perm, value);
      case  78: return static_permuted_radical_inverse<T, 401>(perm, value);
      case  79: return static_permuted_radical_inverse<T, 409>(perm, value);
      case  80: return static_permuted_radical_inverse<T, 419>(perm, value);
      case  81: return static_permuted_radical_inverse<T, 421>(perm, value);
      case  82: return static_permuted_radical_inverse<T, 431>(perm, value);
      case  83: return static_permuted_radical_inverse<T, 433>(perm, value);
      case  84: return static_permuted_radical_inverse<T, 439>(perm, value);
      case  85: return static_permuted_radical_inverse<T, 443>(perm, value);
      case  86: return static_permuted_radical_inverse<T, 449>(perm, value);
      case  87: return static_permuted_radical_inverse<T, 457>(perm, value);
      case  88: return static_permuted_radical_inverse<T, 461>(perm, value);
      case  89: return static_permuted_radical_inverse<T, 463>(perm, value);
      case  90: return static_permuted_radical_inverse<T, 467>(perm, value);
      case  91: return static_permuted_radical_inverse<T, 479>(perm, value);
      case  92: return static_permuted_radical_inverse<T, 487>(perm, value);
      case  93: return static_permuted_radical_inverse<T, 491>(perm, value);
      case  94: return static_permuted_radical_inverse<T, 499>(perm, value);
      case  95: return static_permuted_radical_inverse<T, 503>(perm, value);
      case  96: return static_permuted_radical_inverse<T, 509>(perm, value);
      case  97: return static_permuted_radical_inverse<T, 521>(perm, value);
      case  98: return static_permuted_radical_inverse<T, 523>(perm, value);
      case  99: return static_permuted_radical_inverse<T, 541>(perm, value);
      case 100: return static_permuted_radical_inverse<T, 547>(perm, value);
      case 101: return static_permuted_radical_inverse<T, 557>(perm, value);
      case 102: return static_permuted_radical_inverse<T, 563>(perm, value);
      case 103: return static_permuted_radical_inverse<T, 569>(perm, value);
      case 104: return static_permuted_radical_inverse<T, 571>(perm, value);
      case 105: return static_permuted_radical_inverse<T, 577>(perm, value);
      case 106: return static_permuted_radical_inverse<T, 587>(perm, value);
      case 107: return static_permuted_radical_inverse<T, 593>(perm, value);
      case 108: return static_permuted_radical_inverse<T, 599>(perm, value);
      case 109: return static_permuted_radical_inverse<T, 601>(perm, value);
      case 110: return static_permuted_radical_inverse<T, 607>(perm, value);
      case 111: return static_permuted_radical_inverse<T, 613>(perm, value);
      case 112: return static_permuted_radical_inverse<T, 617>(perm, value);
      case 113: return static_permuted_radical_inverse<T, 619>(perm, value);
      case 114: return static_permuted_radical_inverse<T, 631>(perm, value);
      case 115: return static_permuted_radical_inverse<T, 641>(perm, value);
      case 116: return static_permuted_radical_inverse<T, 643>(perm, value);
      case 117: return static_permuted_radical_inverse<T, 647>(perm, value);
      case 118: return static_permuted_radical_inverse<T, 653>(perm, value);
      case 119: return static_permuted_radical_inverse<T, 659>(perm, value);
      case 120: return static_permuted_radical_inverse<T, 661>(perm, value);
      case 121: return static_permuted_radical_inverse<T, 673>(perm, value);
      case 122: return static_permuted_radical_inverse<T, 677>(perm, value);
      case 123: return static_permuted_radical_inverse<T, 683>(perm, value);
      case 124: return static_permuted_radical_inverse<T, 691>(perm, value);
      case 125: return static_permuted_radical_inverse<T, 701>(perm, value);
      case 126: return static_permuted_radical_inverse<T, 709>(perm, value);
      case 127: return static_permuted_radical_inverse<T, 719>(perm, value);
      case 128: return static_permuted_radical_inverse<T, 727>(perm, value);
      case 129: return static_permuted_radical_inverse<T, 733>(perm, value);
      case 130: return static_permuted_radical_inverse<T, 739>(perm, value);
      case 131: return static_permuted_radical_inverse<T, 743>(perm, value);
      case 132: return static_permuted_radical_inverse<T, 751>(perm, value);
      case 133: return static_permuted_radical_inverse<T, 757>(perm, value);
      case 134: return static_permuted_radical_inverse<T, 761>(perm, value);
      case 135: return static_permuted_radical_inverse<T, 769>(perm, value);
      case 136: return static_permuted_radical_inverse<T, 773>(perm, value);
      case 137: return static_permuted_radical_inverse<T, 787>(perm, value);
      case 138: return static_permuted_radical_inverse<T, 797>(perm, value);
      case 139: return static_permuted_radical_inverse<T, 809>(perm, value);
      case 140: return static_permuted_radical_inverse<T, 811>(perm, value);
      case 141: return static_permuted_radical_inverse<T, 821>(perm, value);
      case 142: return static_permuted_radical_inverse<T, 823>(perm, value);
      case 143: return static_permuted_radical_inverse<T, 827>(perm, value);
      case 144: return static_permuted_radical_inverse<T, 829>(perm, value);
      case 145: return static_permuted_radical_inverse<T, 839>(perm, value);
      case 146: return static_permuted_radical_inverse<T, 853>(perm, value);
      case 147: return static_permuted_radical_inverse<T, 857>(perm, value);
      case 148: return static_permuted_radical_inverse<T, 859>(perm, value);
      case 149: return static_permuted_radical_inverse<T, 863>(perm, value);
      case 150: return static_permuted_radical_inverse<T, 877>(perm, value);
      case 151: return static_permuted_radical_inverse<T, 881>(perm, value);
      case 152: return static_permuted_radical_inverse<T, 883>(perm, value);
      case 153: return static_permuted_radical_inverse<T, 887>(perm, value);
      case 154: return static_permuted_radical_inverse<T, 907>(perm, value);
      case 155: return static_permuted_radical_inverse<T, 911>(perm, value);
      case 156: return static_permuted_radical_inverse<T, 919>(perm, value);
      case 157: return static_permuted_radical_inverse<T, 929>(perm, value);
      case 158: return static_permuted_radical_inverse<T, 937>(perm, value);
      case 159: return static_permuted_radical_inverse<T, 941>(perm, value);
      case 160: return static_permuted_radical_inverse<T, 947>(perm, value);
      case 161: return static_permuted_radical_inverse<T, 953>(perm, value);
      case 162: return static_permuted_radical_inverse<T, 967>(perm, value);
      case 163: return static_permuted_radical_inverse<T, 971>(perm, value);
      case 164: return static_permuted_radical_inverse<T, 977>(perm, value);
      case 165: return static_permuted_radical_inverse<T, 983>(perm, value);
      case 166: return static_permuted_radical_inverse<T, 991>(perm, value);
      case 167: return static_permuted_radical_inverse<T, 997>(perm, value);
      case 168: return static_permuted_radical_inverse<T, 1009>(perm, value);
      case 169: return static_permuted_radical_inverse<T, 1013>(perm, value);
      case 170: return static_permuted_radical_inverse<T, 1019>(perm, value);
      case 171: return static_permuted_radical_inverse<T, 1021>(perm, value);
      case 172: return static_permuted_radical_inverse<T, 1031>(perm, value);
      case 173: return static_permuted_radical_inverse<T, 1033>(perm, value);
      case 174: return static_permuted_radical_inverse<T, 1039>(perm, value);
      case 175: return static_permuted_radical_inverse<T, 1049>(perm, value);
      case 176: return static_permuted_radical_inverse<T, 1051>(perm, value);
      case 177: return static_permuted_radical_inverse<T, 1061>(perm, value);
      case 178: return static_permuted_radical_inverse<T, 1063>(perm, value);
      case 179: return static_permuted_radical_inverse<T, 1069>(perm, value);
      case 180: return static_permuted_radical_inverse<T, 1087>(perm, value);
      case 181: return static_permuted_radical_inverse<T, 1091>(perm, value);
      case 182: return static_permuted_radical_inverse<T, 1093>(perm, value);
      case 183: return static_permuted_radical_inverse<T, 1097>(perm, value);
      case 184: return static_permuted_radical_inverse<T, 1103>(perm, value);
      case 185: return static_permuted_radical_inverse<T, 1109>(perm, value);
      case 186: return static_permuted_radical_inverse<T, 1117>(perm, value);
      case 187: return static_permuted_radical_inverse<T, 1123>(perm, value);
      case 188: return static_permuted_radical_inverse<T, 1129>(perm, value);
      case 189: return static_permuted_radical_inverse<T, 1151>(perm, value);
      case 190: return static_permuted_radical_inverse<T, 1153>(perm, value);
      case 191: return static_permuted_radical_inverse<T, 1163>(perm, value);
      case 192: return static_permuted_radical_inverse<T, 1171>(perm, value);
      case 193: return static_permuted_radical_inverse<T, 1181>(perm, value);
      case 194: return static_permuted_radical_inverse<T, 1187>(perm, value);
      case 195: return static_permuted_radical_inverse<T, 1193>(perm, value);
      case 196: return static_permuted_radical_inverse<T, 1201>(perm, value);
      case 197: return static_permuted_radical_inverse<T, 1213>(perm, value);
      case 198: return static_permuted_radical_inverse<T, 1217>(perm, value);
      case 199: return static_permuted_radical_inverse<T, 1223>(perm, value);
      case 200: return static_permuted_radical_inverse<T, 1229>(perm, value);
      case 201: return static_permuted_radical_inverse<T, 1231>(perm, value);
      case 202: return static_permuted_radical_inverse<T, 1237>(perm, value);
      case 203: return static_permuted_radical_inverse<T, 1249>(perm, value);
      case 204: return static_permuted_radical_inverse<T, 1259>(perm, value);
      case 205: return static_permuted_radical_inverse<T, 1277>(perm, value);
      case 206: return static_permuted_radical_inverse<T, 1279>(perm, value);
      case 207: return static_permuted_radical_inverse<T, 1283>(perm, value);
      case 208: return static_permuted_radical_inverse<T, 1289>(perm, value);
      case 209: return static_permuted_radical_inverse<T, 1291>(perm, value);
      case 210: return static_permuted_radical_inverse<T, 1297>(perm, value);
      case 211: return static_permuted_radical_inverse<T, 1301>(perm, value);
      case 212: return static_permuted_radical_inverse<T, 1303>(perm, value);
      case 213: return static_permuted_radical_inverse<T, 1307>(perm, value);
      case 214: return static_permuted_radical_inverse<T, 1319>(perm, value);
      case 215: return static_permuted_radical_inverse<T, 1321>(perm, value);
      case 216: return static_permuted_radical_inverse<T, 1327>(perm, value);
      case 217: return static_permuted_radical_inverse<T, 1361>(perm, value);
      case 218: return static_permuted_radical_inverse<T, 1367>(perm, value);
      case 219: return static_permuted_radical_inverse<T, 1373>(perm, value);
      case 220: return static_permuted_radical_inverse<T, 1381>(perm, value);
      case 221: return static_permuted_radical_inverse<T, 1399>(perm, value);
      case 222: return static_permuted_radical_inverse<T, 1409>(perm, value);
      case 223: return static_permuted_radical_inverse<T, 1423>(perm, value);
      case 224: return static_permuted_radical_inverse<T, 1427>(perm, value);
      case 225: return static_permuted_radical_inverse<T, 1429>(perm, value);
      case 226: return static_permuted_radical_inverse<T, 1433>(perm, value);
      case 227: return static_permuted_radical_inverse<T, 1439>(perm, value);
      case 228: return static_permuted_radical_inverse<T, 1447>(perm, value);
      case 229: return static_permuted_radical_inverse<T, 1451>(perm, value);
      case 230: return static_permuted_radical_inverse<T, 1453>(perm, value);
      case 231: return static_permuted_radical_inverse<T, 1459>(perm, value);
      case 232: return static_permuted_radical_inverse<T, 1471>(perm, value);
      case 233: return static_permuted_radical_inverse<T, 1481>(perm, value);
      case 234: return static_permuted_radical_inverse<T, 1483>(perm, value);
      case 235: return static_permuted_radical_inverse<T, 1487>(perm, value);
      case 236: return static_permuted_radical_inverse<T, 1489>(perm, value);
      case 237: return static_permuted_radical_inverse<T, 1493>(perm, value);
      case 238: return static_permuted_radical_inverse<T, 1499>(perm, value);
      case 239: return static_permuted_radical_inverse<T, 1511>(perm, value);
      case 240: return static_permuted_radical_inverse<T, 1523>(perm, value);
      case 241: return static_permuted_radical_inverse<T, 1531>(perm, value);
      case 242: return static_permuted_radical_inverse<T, 1543>(perm, value);
      case 243: return static_permuted_radical_inverse<T, 1549>(perm, value);
      case 244: return static_permuted_radical_inverse<T, 1553>(perm, value);
      case 245: return static_permuted_radical_inverse<T, 1559>(perm, value);
      case 246: return static_permuted_radical_inverse<T, 1567>(perm, value);
      case 247: return static_permuted_radical_inverse<T, 1571>(perm, value);
      case 248: return static_permuted_radical_inverse<T, 1579>(perm, value);
      case 249: return static_permuted_radical_inverse<T, 1583>(perm, value);
      case 250: return static_permuted_radical_inverse<T, 1597>(perm, value);
      case 251: return static_permuted_radical_inverse<T, 1601>(perm, value);
      case 252: return static_permuted_radical_inverse<T, 1607>(perm, value);
      case 253: return static_permuted_radical_inverse<T, 1609>(perm, value);
      case 254: return static_permuted_radical_inverse<T, 1613>(perm, value);
      case 255: return static_permuted_radical_inverse<T, 1619>(perm, value);
      case 256: return static_permuted_radical_inverse<T, 1621>(perm, value);
      case 257: return static_permuted_radical_inverse<T, 1627>(perm, value);
      case 258: return static_permuted_radical_inverse<T, 1637>(perm, value);
      case 259: return static_permuted_radical_inverse<T, 1657>(perm, value);
      case 260: return static_permuted_radical_inverse<T, 1663>(perm, value);
      case 261: return static_permuted_radical_inverse<T, 1667>(perm, value);
      case 262: return static_permuted_radical_inverse<T, 1669>(perm, value);
      case 263: return static_permuted_radical_inverse<T, 1693>(perm, value);
      case 264: return static_permuted_radical_inverse<T, 1697>(perm, value);
      case 265: return static_permuted_radical_inverse<T, 1699>(perm, value);
      case 266: return static_permuted_radical_inverse<T, 1709>(perm, value);
      case 267: return static_permuted_radical_inverse<T, 1721>(perm, value);
      case 268: return static_permuted_radical_inverse<T, 1723>(perm, value);
      case 269: return static_permuted_radical_inverse<T, 1733>(perm, value);
      case 270: return static_permuted_radical_inverse<T, 1741>(perm, value);
      case 271: return static_permuted_radical_inverse<T, 1747>(perm, value);
      case 272: return static_permuted_radical_inverse<T, 1753>(perm, value);
      case 273: return static_permuted_radical_inverse<T, 1759>(perm, value);
      case 274: return static_permuted_radical_inverse<T, 1777>(perm, value);
      case 275: return static_permuted_radical_inverse<T, 1783>(perm, value);
      case 276: return static_permuted_radical_inverse<T, 1787>(perm, value);
      case 277: return static_permuted_radical_inverse<T, 1789>(perm, value);
      case 278: return static_permuted_radical_inverse<T, 1801>(perm, value);
      case 279: return static_permuted_radical_inverse<T, 1811>(perm, value);
      case 280: return static_permuted_radical_inverse<T, 1823>(perm, value);
      case 281: return static_permuted_radical_inverse<T, 1831>(perm, value);
      case 282: return static_permuted_radical_inverse<T, 1847>(perm, value);
      case 283: return static_permuted_radical_inverse<T, 1861>(perm, value);
      case 284: return static_permuted_radical_inverse<T, 1867>(perm, value);
      case 285: return static_permuted_radical_inverse<T, 1871>(perm, value);
      case 286: return static_permuted_radical_inverse<T, 1873>(perm, value);
      case 287: return static_permuted_radical_inverse<T, 1877>(perm, value);
      case 288: return static_permuted_radical_inverse<T, 1879>(perm, value);
      case 289: return static_permuted_radical_inverse<T, 1889>(perm, value);
      case 290: return static_permuted_radical_inverse<T, 1901>(perm, value);
      case 291: return static_permuted_radical_inverse<T, 1907>(perm, value);
      case 292: return static_permuted_radical_inverse<T, 1913>(perm, value);
      case 293: return static_permuted_radical_inverse<T, 1931>(perm, value);
      case 294: return static_permuted_radical_inverse<T, 1933>(perm, value);
      case 295: return static_permuted_radical_inverse<T, 1949>(perm, value);
      case 296: return static_permuted_radical_inverse<T, 1951>(perm, value);
      case 297: return static_permuted_radical_inverse<T, 1973>(perm, value);
      case 298: return static_permuted_radical_inverse<T, 1979>(perm, value);
      case 299: return static_permuted_radical_inverse<T, 1987>(perm, value);
      case 300: return static_permuted_radical_inverse<T, 1993>(perm, value);
      case 301: return static_permuted_radical_inverse<T, 1997>(perm, value);
      case 302: return static_permuted_radical_inverse<T, 1999>(perm, value);
      case 303: return static_permuted_radical_inverse<T, 2003>(perm, value);
      case 304: return static_permuted_radical_inverse<T, 2011>(perm, value);
      case 305: return static_permuted_radical_inverse<T, 2017>(perm, value);
      case 306: return static_permuted_radical_inverse<T, 2027>(perm, value);
      case 307: return static_permuted_radical_inverse<T, 2029>(perm, value);
      case 308: return static_permuted_radical_inverse<T, 2039>(perm, value);
      case 309: return static_permuted_radical_inverse<T, 2053>(perm, value);
      case 310: return static_permuted_radical_inverse<T, 2063>(perm, value);
      case 311: return static_permuted_radical_inverse<T, 2069>(perm, value);
      case 312: return static_permuted_radical_inverse<T, 2081>(perm, value);
      case 313: return static_permuted_radical_inverse<T, 2083>(perm, value);
      case 314: return static_permuted_radical_inverse<T, 2087>(perm, value);
      case 315: return static_permuted_radical_inverse<T, 2089>(perm, value);
      case 316: return static_permuted_radical_inverse<T, 2099>(perm, value);
      case 317: return static_permuted_radical_inverse<T, 2111>(perm, value);
      case 318: return static_permuted_radical_inverse<T, 2113>(perm, value);
      case 319: return static_permuted_radical_inverse<T, 2129>(perm, value);
      case 320: return static_permuted_radical_inverse<T, 2131>(perm, value);
      case 321: return static_permuted_radical_inverse<T, 2137>(perm, value);
      case 322: return static_permuted_radical_inverse<T, 2141>(perm, value);
      case 323: return static_permuted_radical_inverse<T, 2143>(perm, value);
      case 324: return static_permuted_radical_inverse<T, 2153>(perm, value);
      case 325: return static_permuted_radical_inverse<T, 2161>(perm, value);
      case 326: return static_permuted_radical_inverse<T, 2179>(perm, value);
      case 327: return static_permuted_radical_inverse<T, 2203>(perm, value);
      case 328: return static_permuted_radical_inverse<T, 2207>(perm, value);
      case 329: return static_permuted_radical_inverse<T, 2213>(perm, value);
      case 330: return static_permuted_radical_inverse<T, 2221>(perm, value);
      case 331: return static_permuted_radical_inverse<T, 2237>(perm, value);
      case 332: return static_permuted_radical_inverse<T, 2239>(perm, value);
      case 333: return static_permuted_radical_inverse<T, 2243>(perm, value);
      case 334: return static_permuted_radical_inverse<T, 2251>(perm, value);
      case 335: return static_permuted_radical_inverse<T, 2267>(perm, value);
      case 336: return static_permuted_radical_inverse<T, 2269>(perm, value);
      case 337: return static_permuted_radical_inverse<T, 2273>(perm, value);
      case 338: return static_permuted_radical_inverse<T, 2281>(perm, value);
      case 339: return static_permuted_radical_inverse<T, 2287>(perm, value);
      case 340: return static_permuted_radical_inverse<T, 2293>(perm, value);
      case 341: return static_permuted_radical_inverse<T, 2297>(perm, value);
      case 342: return static_permuted_radical_inverse<T, 2309>(perm, value);
      case 343: return static_permuted_radical_inverse<T, 2311>(perm, value);
      case 344: return static_permuted_radical_inverse<T, 2333>(perm, value);
      case 345: return static_permuted_radical_inverse<T, 2339>(perm, value);
      case 346: return static_permuted_radical_inverse<T, 2341>(perm, value);
      case 347: return static_permuted_radical_inverse<T, 2347>(perm, value);
      case 348: return static_permuted_radical_inverse<T, 2351>(perm, value);
      case 349: return static_permuted_radical_inverse<T, 2357>(perm, value);
      case 350: return static_permuted_radical_inverse<T, 2371>(perm, value);
      case 351: return static_permuted_radical_inverse<T, 2377>(perm, value);
      case 352: return static_permuted_radical_inverse<T, 2381>(perm, value);
      case 353: return static_permuted_radical_inverse<T, 2383>(perm, value);
      case 354: return static_permuted_radical_inverse<T, 2389>(perm, value);
      case 355: return static_permuted_radical_inverse<T, 2393>(perm, value);
      case 356: return static_permuted_radical_inverse<T, 2399>(perm, value);
      case 357: return static_permuted_radical_inverse<T, 2411>(perm, value);
      case 358: return static_permuted_radical_inverse<T, 2417>(perm, value);
      case 359: return static_permuted_radical_inverse<T, 2423>(perm, value);
      case 360: return static_permuted_radical_inverse<T, 2437>(perm, value);
      case 361: return static_permuted_radical_inverse<T, 2441>(perm, value);
      case 362: return static_permuted_radical_inverse<T, 2447>(perm, value);
      case 363: return static_permuted_radical_inverse<T, 2459>(perm, value);
      case 364: return static_permuted_radical_inverse<T, 2467>(perm, value);
      case 365: return static_permuted_radical_inverse<T, 2473>(perm, value);
      case 366: return static_permuted_radical_inverse<T, 2477>(perm, value);
      case 367: return static_permuted_radical_inverse<T, 2503>(perm, value);
      case 368: return static_permuted_radical_inverse<T, 2521>(perm, value);
      case 369: return static_permuted_radical_inverse<T, 2531>(perm, value);
      case 370: return static_permuted_radical_inverse<T, 2539>(perm, value);
      case 371: return static_permuted_radical_inverse<T, 2543>(perm, value);
      case 372: return static_permuted_radical_inverse<T, 2549>(perm, value);
      case 373: return static_permuted_radical_inverse<T, 2551>(perm, value);
      case 374: return static_permuted_radical_inverse<T, 2557>(perm, value);
      case 375: return static_permuted_radical_inverse<T, 2579>(perm, value);
      case 376: return static_permuted_radical_inverse<T, 2591>(perm, value);
      case 377: return static_permuted_radical_inverse<T, 2593>(perm, value);
      case 378: return static_permuted_radical_inverse<T, 2609>(perm, value);
      case 379: return static_permuted_radical_inverse<T, 2617>(perm, value);
      case 380: return static_permuted_radical_inverse<T, 2621>(perm, value);
      case 381: return static_permuted_radical_inverse<T, 2633>(perm, value);
      case 382: return static_permuted_radical_inverse<T, 2647>(perm, value);
      case 383: return static_permuted_radical_inverse<T, 2657>(perm, value);
      case 384: return static_permuted_radical_inverse<T, 2659>(perm, value);
      case 385: return static_permuted_radical_inverse<T, 2663>(perm, value);
      case 386: return static_permuted_radical_inverse<T, 2671>(perm, value);
      case 387: return static_permuted_radical_inverse<T, 2677>(perm, value);
      case 388: return static_permuted_radical_inverse<T, 2683>(perm, value);
      case 389: return static_permuted_radical_inverse<T, 2687>(perm, value);
      case 390: return static_permuted_radical_inverse<T, 2689>(perm, value);
      case 391: return static_permuted_radical_inverse<T, 2693>(perm, value);
      case 392: return static_permuted_radical_inverse<T, 2699>(perm, value);
      case 393: return static_permuted_radical_inverse<T, 2707>(perm, value);
      case 394: return static_permuted_radical_inverse<T, 2711>(perm, value);
      case 395: return static_permuted_radical_inverse<T, 2713>(perm, value);
      case 396: return static_permuted_radical_inverse<T, 2719>(perm, value);
      case 397: return static_permuted_radical_inverse<T, 2729>(perm, value);
      case 398: return static_permuted_radical_inverse<T, 2731>(perm, value);
      case 399: return static_permuted_radical_inverse<T, 2741>(perm, value);
      case 400: return static_permuted_radical_inverse<T, 2749>(perm, value);
      case 401: return static_permuted_radical_inverse<T, 2753>(perm, value);
      case 402: return static_permuted_radical_inverse<T, 2767>(perm, value);
      case 403: return static_permuted_radical_inverse<T, 2777>(perm, value);
      case 404: return static_permuted_radical_inverse<T, 2789>(perm, value);
      case 405: return static_permuted_radical_inverse<T, 2791>(perm, value);
      case 406: return static_permuted_radical_inverse<T, 2797>(perm, value);
      case 407: return static_permuted_radical_inverse<T, 2801>(perm, value);
      case 408: return static_permuted_radical_inverse<T, 2803>(perm, value);
      case 409: return static_permuted_radical_inverse<T, 2819>(perm, value);
      case 410: return static_permuted_radical_inverse<T, 2833>(perm, value);
      case 411: return static_permuted_radical_inverse<T, 2837>(perm, value);
      case 412: return static_permuted_radical_inverse<T, 2843>(perm, value);
      case 413: return static_permuted_radical_inverse<T, 2851>(perm, value);
      case 414: return static_permuted_radical_inverse<T, 2857>(perm, value);
      case 415: return static_permuted_radical_inverse<T, 2861>(perm, value);
      case 416: return static_permuted_radical_inverse<T, 2879>(perm, value);
      case 417: return static_permuted_radical_inverse<T, 2887>(perm, value);
      case 418: return static_permuted_radical_inverse<T, 2897>(perm, value);
      case 419: return static_permuted_radical_inverse<T, 2903>(perm, value);
      case 420: return static_permuted_radical_inverse<T, 2909>(perm, value);
      case 421: return static_permuted_radical_inverse<T, 2917>(perm, value);
      case 422: return static_permuted_radical_inverse<T, 2927>(perm, value);
      case 423: return static_permuted_radical_inverse<T, 2939>(perm, value);
      case 424: return static_permuted_radical_inverse<T, 2953>(perm, value);
      case 425: return static_permuted_radical_inverse<T, 2957>(perm, value);
      case 426: return static_permuted_radical_inverse<T, 2963>(perm, value);
      case 427: return static_permuted_radical_inverse<T, 2969>(perm, value);
      case 428: return static_permuted_radical_inverse<T, 2971>(perm, value);
      case 429: return static_permuted_radical_inverse<T, 2999>(perm, value);
      case 430: return static_permuted_radical_inverse<T, 3001>(perm, value);
      case 431: return static_permuted_radical_inverse<T, 3011>(perm, value);
      case 432: return static_permuted_radical_inverse<T, 3019>(perm, value);
      case 433: return static_permuted_radical_inverse<T, 3023>(perm, value);
      case 434: return static_permuted_radical_inverse<T, 3037>(perm, value);
      case 435: return static_permuted_radical_inverse<T, 3041>(perm, value);
      case 436: return static_permuted_radical_inverse<T, 3049>(perm, value);
      case 437: return static_permuted_radical_inverse<T, 3061>(perm, value);
      case 438: return static_permuted_radical_inverse<T, 3067>(perm, value);
      case 439: return static_permuted_radical_inverse<T, 3079>(perm, value);
      case 440: return static_permuted_radical_inverse<T, 3083>(perm, value);
      case 441: return static_permuted_radical_inverse<T, 3089>(perm, value);
      case 442: return static_permuted_radical_inverse<T, 3109>(perm, value);
      case 443: return static_permuted_radical_inverse<T, 3119>(perm, value);
      case 444: return static_permuted_radical_inverse<T, 3121>(perm, value);
      case 445: return static_permuted_radical_inverse<T, 3137>(perm, value);
      case 446: return static_permuted_radical_inverse<T, 3163>(perm, value);
      case 447: return static_permuted_radical_inverse<T, 3167>(perm, value);
      case 448: return static_permuted_radical_inverse<T, 3169>(perm, value);
      case 449: return static_permuted_radical_inverse<T, 3181>(perm, value);
      case 450: return static_permuted_radical_inverse<T, 3187>(perm, value);
      case 451: return static_permuted_radical_inverse<T, 3191>(perm, value);
      case 452: return static_permuted_radical_inverse<T, 3203>(perm, value);
      case 453: return static_permuted_radical_inverse<T, 3209>(perm, value);
      case 454: return static_permuted_radical_inverse<T, 3217>(perm, value);
      case 455: return static_permuted_radical_inverse<T, 3221>(perm, value);
      case 456: return static_permuted_radical_inverse<T, 3229>(perm, value);
      case 457: return static_permuted_radical_inverse<T, 3251>(perm, value);
      case 458: return static_permuted_radical_inverse<T, 3253>(perm, value);
      case 459: return static_permuted_radical_inverse<T, 3257>(perm, value);
      case 460: return static_permuted_radical_inverse<T, 3259>(perm, value);
      case 461: return static_permuted_radical_inverse<T, 3271>(perm, value);
      case 462: return static_permuted_radical_inverse<T, 3299>(perm, value);
      case 463: return static_permuted_radical_inverse<T, 3301>(perm, value);
      case 464: return static_permuted_radical_inverse<T, 3307>(perm, value);
      case 465: return static_permuted_radical_inverse<T, 3313>(perm, value);
      case 466: return static_permuted_radical_inverse<T, 3319>(perm, value);
      case 467: return static_permuted_radical_inverse<T, 3323>(perm, value);
      case 468: return static_permuted_radical_inverse<T, 3329>(perm, value);
      case 469: return static_permuted_radical_inverse<T, 3331>(perm, value);
      case 470: return static_permuted_radical_inverse<T, 3343>(perm, value);
      case 471: return static_permuted_radical_inverse<T, 3347>(perm, value);
      case 472: return static_permuted_radical_inverse<T, 3359>(perm, value);
      case 473: return static_permuted_radical_inverse<T, 3361>(perm, value);
      case 474: return static_permuted_radical_inverse<T, 3371>(perm, value);
      case 475: return static_permuted_radical_inverse<T, 3373>(perm, value);
      case 476: return static_permuted_radical_inverse<T, 3389>(perm, value);
      case 477: return static_permuted_radical_inverse<T, 3391>(perm, value);
      case 478: return static_permuted_radical_inverse<T, 3407>(perm, value);
      case 479: return static_permuted_radical_inverse<T, 3413>(perm, value);
      case 480: return static_permuted_radical_inverse<T, 3433>(perm, value);
      case 481: return static_permuted_radical_inverse<T, 3449>(perm, value);
      case 482: return static_permuted_radical_inverse<T, 3457>(perm, value);
      case 483: return static_permuted_radical_inverse<T, 3461>(perm, value);
      case 484: return static_permuted_radical_inverse<T, 3463>(perm, value);
      case 485: return static_permuted_radical_inverse<T, 3467>(perm, value);
      case 486: return static_permuted_radical_inverse<T, 3469>(perm, value);
      case 487: return static_permuted_radical_inverse<T, 3491>(perm, value);
      case 488: return static_permuted_radical_inverse<T, 3499>(perm, value);
      case 489: return static_permuted_radical_inverse<T, 3511>(perm, value);
      case 490: return static_permuted_radical_inverse<T, 3517>(perm, value);
      case 491: return static_permuted_radical_inverse<T, 3527>(perm, value);
      case 492: return static_permuted_radical_inverse<T, 3529>(perm, value);
      case 493: return static_permuted_radical_inverse<T, 3533>(perm, value);
      case 494: return static_permuted_radical_inverse<T, 3539>(perm, value);
      case 495: return static_permuted_radical_inverse<T, 3541>(perm, value);
      case 496: return static_permuted_radical_inverse<T, 3547>(perm, value);
      case 497: return static_permuted_radical_inverse<T, 3557>(perm, value);
      case 498: return static_permuted_radical_inverse<T, 3559>(perm, value);
      case 499: return static_permuted_radical_inverse<T, 3571>(perm, value);
      case 500: return static_permuted_radical_inverse<T, 3581>(perm, value);
      case 501: return static_permuted_radical_inverse<T, 3583>(perm, value);
      case 502: return static_permuted_radical_inverse<T, 3593>(perm, value);
      case 503: return static_permuted_radical_inverse<T, 3607>(perm, value);
      case 504: return static_permuted_radical_inverse<T, 3613>(perm, value);
      case 505: return static_permuted_radical_inverse<T, 3617>(perm, value);
      case 506: return static_permuted_radical_inverse<T, 3623>(perm, value);
      case 507: return static_permuted_radical_inverse<T, 3631>(perm, value);
      case 508: return static_permuted_radical_inverse<T, 3637>(perm, value);
      case 509: return static_permuted_radical_inverse<T, 3643>(perm, value);
      case 510: return static_permuted_radical_inverse<T, 3659>(perm, value);
      case 511: return static_permuted_radical_inverse<T, 3671>(perm, value);
      case 512: return static_permuted_radical_inverse<T, 3673>(perm, value);
      case 513: return static_permuted_radical_inverse<T, 3677>(perm, value);
      case 514: return static_permuted_radical_inverse<T, 3691>(perm, value);
      case 515: return static_permuted_radical_inverse<T, 3697>(perm, value);
      case 516: return static_permuted_radical_inverse<T, 3701>(perm, value);
      case 517: return static_permuted_radical_inverse<T, 3709>(perm, value);
      case 518: return static_permuted_radical_inverse<T, 3719>(perm, value);
      case 519: return static_permuted_radical_inverse<T, 3727>(perm, value);
      case 520: return static_permuted_radical_inverse<T, 3733>(perm, value);
      case 521: return static_permuted_radical_inverse<T, 3739>(perm, value);
      case 522: return static_permuted_radical_inverse<T, 3761>(perm, value);
      case 523: return static_permuted_radical_inverse<T, 3767>(perm, value);
      case 524: return static_permuted_radical_inverse<T, 3769>(perm, value);
      case 525: return static_permuted_radical_inverse<T, 3779>(perm, value);
      case 526: return static_permuted_radical_inverse<T, 3793>(perm, value);
      case 527: return static_permuted_radical_inverse<T, 3797>(perm, value);
      case 528: return static_permuted_radical_inverse<T, 3803>(perm, value);
      case 529: return static_permuted_radical_inverse<T, 3821>(perm, value);
      case 530: return static_permuted_radical_inverse<T, 3823>(perm, value);
      case 531: return static_permuted_radical_inverse<T, 3833>(perm, value);
      case 532: return static_permuted_radical_inverse<T, 3847>(perm, value);
      case 533: return static_permuted_radical_inverse<T, 3851>(perm, value);
      case 534: return static_permuted_radical_inverse<T, 3853>(perm, value);
      case 535: return static_permuted_radical_inverse<T, 3863>(perm, value);
      case 536: return static_permuted_radical_inverse<T, 3877>(perm, value);
      case 537: return static_permuted_radical_inverse<T, 3881>(perm, value);
      case 538: return static_permuted_radical_inverse<T, 3889>(perm, value);
      case 539: return static_permuted_radical_inverse<T, 3907>(perm, value);
      case 540: return static_permuted_radical_inverse<T, 3911>(perm, value);
      case 541: return static_permuted_radical_inverse<T, 3917>(perm, value);
      case 542: return static_permuted_radical_inverse<T, 3919>(perm, value);
      case 543: return static_permuted_radical_inverse<T, 3923>(perm, value);
      case 544: return static_permuted_radical_inverse<T, 3929>(perm, value);
      case 545: return static_permuted_radical_inverse<T, 3931>(perm, value);
      case 546: return static_permuted_radical_inverse<T, 3943>(perm, value);
      case 547: return static_permuted_radical_inverse<T, 3947>(perm, value);
      case 548: return static_permuted_radical_inverse<T, 3967>(perm, value);
      case 549: return static_permuted_radical_inverse<T, 3989>(perm, value);
      case 550: return static_permuted_radical_inverse<T, 4001>(perm, value);
      case 551: return static_permuted_radical_inverse<T, 4003>(perm, value);
      case 552: return static_permuted_radical_inverse<T, 4007>(perm, value);
      case 553: return static_permuted_radical_inverse<T, 4013>(perm, value);
      case 554: return static_permuted_radical_inverse<T, 4019>(perm, value);
      case 555: return static_permuted_radical_inverse<T, 4021>(perm, value);
      case 556: return static_permuted_radical_inverse<T, 4027>(perm, value);
      case 557: return static_permuted_radical_inverse<T, 4049>(perm, value);
      case 558: return static_permuted_radical_inverse<T, 4051>(perm, value);
      case 559: return static_permuted_radical_inverse<T, 4057>(perm, value);
      case 560: return static_permuted_radical_inverse<T, 4073>(perm, value);
      case 561: return static_permuted_radical_inverse<T, 4079>(perm, value);
      case 562: return static_permuted_radical_inverse<T, 4091>(perm, value);
      case 563: return static_permuted_radical_inverse<T, 4093>(perm, value);
      case 564: return static_permuted_radical_inverse<T, 4099>(perm, value);
      case 565: return static_permuted_radical_inverse<T, 4111>(perm, value);
      case 566: return static_permuted_radical_inverse<T, 4127>(perm, value);
      case 567: return static_permuted_radical_inverse<T, 4129>(perm, value);
      case 568: return static_permuted_radical_inverse<T, 4133>(perm, value);
      case 569: return static_permuted_radical_inverse<T, 4139>(perm, value);
      case 570: return static_permuted_radical_inverse<T, 4153>(perm, value);
      case 571: return static_permuted_radical_inverse<T, 4157>(perm, value);
      case 572: return static_permuted_radical_inverse<T, 4159>(perm, value);
      case 573: return static_permuted_radical_inverse<T, 4177>(perm, value);
      case 574: return static_permuted_radical_inverse<T, 4201>(perm, value);
      case 575: return static_permuted_radical_inverse<T, 4211>(perm, value);
      case 576: return static_permuted_radical_inverse<T, 4217>(perm, value);
      case 577: return static_permuted_radical_inverse<T, 4219>(perm, value);
      case 578: return static_permuted_radical_inverse<T, 4229>(perm, value);
      case 579: return static_permuted_radical_inverse<T, 4231>(perm, value);
      case 580: return static_permuted_radical_inverse<T, 4241>(perm, value);
      case 581: return static_permuted_radical_inverse<T, 4243>(perm, value);
      case 582: return static_permuted_radical_inverse<T, 4253>(perm, value);
      case 583: return static_permuted_radical_inverse<T, 4259>(perm, value);
      case 584: return static_permuted_radical_inverse<T, 4261>(perm, value);
      case 585: return static_permuted_radical_inverse<T, 4271>(perm, value);
      case 586: return static_permuted_radical_inverse<T, 4273>(perm, value);
      case 587: return static_permuted_radical_inverse<T, 4283>(perm, value);
      case 588: return static_permuted_radical_inverse<T, 4289>(perm, value);
      case 589: return static_permuted_radical_inverse<T, 4297>(perm, value);
      case 590: return static_permuted_radical_inverse<T, 4327>(perm, value);
      case 591: return static_permuted_radical_inverse<T, 4337>(perm, value);
      case 592: return static_permuted_radical_inverse<T, 4339>(perm, value);
      case 593: return static_permuted_radical_inverse<T, 4349>(perm, value);
      case 594: return static_permuted_radical_inverse<T, 4357>(perm, value);
      case 595: return static_permuted_radical_inverse<T, 4363>(perm, value);
      case 596: return static_permuted_radical_inverse<T, 4373>(perm, value);
      case 597: return static_permuted_radical_inverse<T, 4391>(perm, value);
      case 598: return static_permuted_radical_inverse<T, 4397>(perm, value);
      case 599: return static_permuted_radical_inverse<T, 4409>(perm, value);
      case 600: return static_permuted_radical_inverse<T, 4421>(perm, value);
      case 601: return static_permuted_radical_inverse<T, 4423>(perm, value);
      case 602: return static_permuted_radical_inverse<T, 4441>(perm, value);
      case 603: return static_permuted_radical_inverse<T, 4447>(perm, value);
      case 604: return static_permuted_radical_inverse<T, 4451>(perm, value);
      case 605: return static_permuted_radical_inverse<T, 4457>(perm, value);
      case 606: return static_permuted_radical_inverse<T, 4463>(perm, value);
      case 607: return static_permuted_radical_inverse<T, 4481>(perm, value);
      case 608: return static_permuted_radical_inverse<T, 4483>(perm, value);
      case 609: return static_permuted_radical_inverse<T, 4493>(perm, value);
      case 610: return static_permuted_radical_inverse<T, 4507>(perm, value);
      case 611: return static_permuted_radical_inverse<T, 4513>(perm, value);
      case 612: return static_permuted_radical_inverse<T, 4517>(perm, value);
      case 613: return static_permuted_radical_inverse<T, 4519>(perm, value);
      case 614: return static_permuted_radical_inverse<T, 4523>(perm, value);
      case 615: return static_permuted_radical_inverse<T, 4547>(perm, value);
      case 616: return static_permuted_radical_inverse<T, 4549>(perm, value);
      case 617: return static_permuted_radical_inverse<T, 4561>(perm, value);
      case 618: return static_permuted_radical_inverse<T, 4567>(perm, value);
      case 619: return static_permuted_radical_inverse<T, 4583>(perm, value);
      case 620: return static_permuted_radical_inverse<T, 4591>(perm, value);
      case 621: return static_permuted_radical_inverse<T, 4597>(perm, value);
      case 622: return static_permuted_radical_inverse<T, 4603>(perm, value);
      case 623: return static_permuted_radical_inverse<T, 4621>(perm, value);
      case 624: return static_permuted_radical_inverse<T, 4637>(perm, value);
      case 625: return static_permuted_radical_inverse<T, 4639>(perm, value);
      case 626: return static_permuted_radical_inverse<T, 4643>(perm, value);
      case 627: return static_permuted_radical_inverse<T, 4649>(perm, value);
      case 628: return static_permuted_radical_inverse<T, 4651>(perm, value);
      case 629: return static_permuted_radical_inverse<T, 4657>(perm, value);
      case 630: return static_permuted_radical_inverse<T, 4663>(perm, value);
      case 631: return static_permuted_radical_inverse<T, 4673>(perm, value);
      case 632: return static_permuted_radical_inverse<T, 4679>(perm, value);
      case 633: return static_permuted_radical_inverse<T, 4691>(perm, value);
      case 634: return static_permuted_radical_inverse<T, 4703>(perm, value);
      case 635: return static_permuted_radical_inverse<T, 4721>(perm, value);
      case 636: return static_permuted_radical_inverse<T, 4723>(perm, value);
      case 637: return static_permuted_radical_inverse<T, 4729>(perm, value);
      case 638: return static_permuted_radical_inverse<T, 4733>(perm, value);
      case 639: return static_permuted_radical_inverse<T, 4751>(perm, value);
      case 640: return static_permuted_radical_inverse<T, 4759>(perm, value);
      case 641: return static_permuted_radical_inverse<T, 4783>(perm, value);
      case 642: return static_permuted_radical_inverse<T, 4787>(perm, value);
      case 643: return static_permuted_radical_inverse<T, 4789>(perm, value);
      case 644: return static_permuted_radical_inverse<T, 4793>(perm, value);
      case 645: return static_permuted_radical_inverse<T, 4799>(perm, value);
      case 646: return static_permuted_radical_inverse<T, 4801>(perm, value);
      case 647: return static_permuted_radical_inverse<T, 4813>(perm, value);
      case 648: return static_permuted_radical_inverse<T, 4817>(perm, value);
      case 649: return static_permuted_radical_inverse<T, 4831>(perm, value);
      case 650: return static_permuted_radical_inverse<T, 4861>(perm, value);
      case 651: return static_permuted_radical_inverse<T, 4871>(perm, value);
      case 652: return static_permuted_radical_inverse<T, 4877>(perm, value);
      case 653: return static_permuted_radical_inverse<T, 4889>(perm, value);
      case 654: return static_permuted_radical_inverse<T, 4903>(perm, value);
      case 655: return static_permuted_radical_inverse<T, 4909>(perm, value);
      case 656: return static_permuted_radical_inverse<T, 4919>(perm, value);
      case 657: return static_permuted_radical_inverse<T, 4931>(perm, value);
      case 658: return static_permuted_radical_inverse<T, 4933>(perm, value);
      case 659: return static_permuted_radical_inverse<T, 4937>(perm, value);
      case 660: return static_permuted_radical_inverse<T, 4943>(perm, value);
      case 661: return static_permuted_radical_inverse<T, 4951>(perm, value);
      case 662: return static_permuted_radical_inverse<T, 4957>(perm, value);
      case 663: return static_permuted_radical_inverse<T, 4967>(perm, value);
      case 664: return static_permuted_radical_inverse<T, 4969>(perm, value);
      case 665: return static_permuted_radical_inverse<T, 4973>(perm, value);
      case 666: return static_permuted_radical_inverse<T, 4987>(perm, value);
      case 667: return static_permuted_radical_inverse<T, 4993>(perm, value);
      case 668: return static_permuted_radical_inverse<T, 4999>(perm, value);
      case 669: return static_permuted_radical_inverse<T, 5003>(perm, value);
      case 670: return static_permuted_radical_inverse<T, 5009>(perm, value);
      case 671: return static_permuted_radical_inverse<T, 5011>(perm, value);
      case 672: return static_permuted_radical_inverse<T, 5021>(perm, value);
      case 673: return static_permuted_radical_inverse<T, 5023>(perm, value);
      case 674: return static_permuted_radical_inverse<T, 5039>(perm, value);
      case 675: return static_permuted_radical_inverse<T, 5051>(perm, value);
      case 676: return static_permuted_radical_inverse<T, 5059>(perm, value);
      case 677: return static_permuted_radical_inverse<T, 5077>(perm, value);
      case 678: return static_permuted_radical_inverse<T, 5081>(perm, value);
      case 679: return static_permuted_radical_inverse<T, 5087>(perm, value);
      case 680: return static_permuted_radical_inverse<T, 5099>(perm, value);
      case 681: return static_permuted_radical_inverse<T, 5101>(perm, value);
      case 682: return static_permuted_radical_inverse<T, 5107>(perm, value);
      case 683: return static_permuted_radical_inverse<T, 5113>(perm, value);
      case 684: return static_permuted_radical_inverse<T, 5119>(perm, value);
      case 685: return static_permuted_radical_inverse<T, 5147>(perm, value);
      case 686: return static_permuted_radical_inverse<T, 5153>(perm, value);
      case 687: return static_permuted_radical_inverse<T, 5167>(perm, value);
      case 688: return static_permuted_radical_inverse<T, 5171>(perm, value);
      case 689: return static_permuted_radical_inverse<T, 5179>(perm, value);
      case 690: return static_permuted_radical_inverse<T, 5189>(perm, value);
      case 691: return static_permuted_radical_inverse<T, 5197>(perm, value);
      case 692: return static_permuted_radical_inverse<T, 5209>(perm, value);
      case 693: return static_permuted_radical_inverse<T, 5227>(perm, value);
      case 694: return static_permuted_radical_inverse<T, 5231>(perm, value);
      case 695: return static_permuted_radical_inverse<T, 5233>(perm, value);
      case 696: return static_permuted_radical_inverse<T, 5237>(perm, value);
      case 697: return static_permuted_radical_inverse<T, 5261>(perm, value);
      case 698: return static_permuted_radical_inverse<T, 5273>(perm, value);
      case 699: return static_permuted_radical_inverse<T, 5279>(perm, value);
      case 700: return static_permuted_radical_inverse<T, 5281>(perm, value);
      case 701: return static_permuted_radical_inverse<T, 5297>(perm, value);
      case 702: return static_permuted_radical_inverse<T, 5303>(perm, value);
      case 703: return static_permuted_radical_inverse<T, 5309>(perm, value);
      case 704: return static_permuted_radical_inverse<T, 5323>(perm, value);
      case 705: return static_permuted_radical_inverse<T, 5333>(perm, value);
      case 706: return static_permuted_radical_inverse<T, 5347>(perm, value);
      case 707: return static_permuted_radical_inverse<T, 5351>(perm, value);
      case 708: return static_permuted_radical_inverse<T, 5381>(perm, value);
      case 709: return static_permuted_radical_inverse<T, 5387>(perm, value);
      case 710: return static_permuted_radical_inverse<T, 5393>(perm, value);
      case 711: return static_permuted_radical_inverse<T, 5399>(perm, value);
      case 712: return static_permuted_radical_inverse<T, 5407>(perm, value);
      case 713: return static_permuted_radical_inverse<T, 5413>(perm, value);
      case 714: return static_permuted_radical_inverse<T, 5417>(perm, value);
      case 715: return static_permuted_radical_inverse<T, 5419>(perm, value);
      case 716: return static_permuted_radical_inverse<T, 5431>(perm, value);
      case 717: return static_permuted_radical_inverse<T, 5437>(perm, value);
      case 718: return static_permuted_radical_inverse<T, 5441>(perm, value);
      case 719: return static_permuted_radical_inverse<T, 5443>(perm, value);
      case 720: return static_permuted_radical_inverse<T, 5449>(perm, value);
      case 721: return static_permuted_radical_inverse<T, 5471>(perm, value);
      case 722: return static_permuted_radical_inverse<T, 5477>(perm, value);
      case 723: return static_permuted_radical_inverse<T, 5479>(perm, value);
      case 724: return static_permuted_radical_inverse<T, 5483>(perm, value);
      case 725: return static_permuted_radical_inverse<T, 5501>(perm, value);
      case 726: return static_permuted_radical_inverse<T, 5503>(perm, value);
      case 727: return static_permuted_radical_inverse<T, 5507>(perm, value);
      case 728: return static_permuted_radical_inverse<T, 5519>(perm, value);
      case 729: return static_permuted_radical_inverse<T, 5521>(perm, value);
      case 730: return static_permuted_radical_inverse<T, 5527>(perm, value);
      case 731: return static_permuted_radical_inverse<T, 5531>(perm, value);
      case 732: return static_permuted_radical_inverse<T, 5557>(perm, value);
      case 733: return static_permuted_radical_inverse<T, 5563>(perm, value);
      case 734: return static_permuted_radical_inverse<T, 5569>(perm, value);
      case 735: return static_permuted_radical_inverse<T, 5573>(perm, value);
      case 736: return static_permuted_radical_inverse<T, 5581>(perm, value);
      case 737: return static_permuted_radical_inverse<T, 5591>(perm, value);
      case 738: return static_permuted_radical_inverse<T, 5623>(perm, value);
      case 739: return static_permuted_radical_inverse<T, 5639>(perm, value);
      case 740: return static_permuted_radical_inverse<T, 5641>(perm, value);
      case 741: return static_permuted_radical_inverse<T, 5647>(perm, value);
      case 742: return static_permuted_radical_inverse<T, 5651>(perm, value);
      case 743: return static_permuted_radical_inverse<T, 5653>(perm, value);
      case 744: return static_permuted_radical_inverse<T, 5657>(perm, value);
      case 745: return static_permuted_radical_inverse<T, 5659>(perm, value);
      case 746: return static_permuted_radical_inverse<T, 5669>(perm, value);
      case 747: return static_permuted_radical_inverse<T, 5683>(perm, value);
      case 748: return static_permuted_radical_inverse<T, 5689>(perm, value);
      case 749: return static_permuted_radical_inverse<T, 5693>(perm, value);
      case 750: return static_permuted_radical_inverse<T, 5701>(perm, value);
      case 751: return static_permuted_radical_inverse<T, 5711>(perm, value);
      case 752: return static_permuted_radical_inverse<T, 5717>(perm, value);
      case 753: return static_permuted_radical_inverse<T, 5737>(perm, value);
      case 754: return static_permuted_radical_inverse<T, 5741>(perm, value);
      case 755: return static_permuted_radical_inverse<T, 5743>(perm, value);
      case 756: return static_permuted_radical_inverse<T, 5749>(perm, value);
      case 757: return static_permuted_radical_inverse<T, 5779>(perm, value);
      case 758: return static_permuted_radical_inverse<T, 5783>(perm, value);
      case 759: return static_permuted_radical_inverse<T, 5791>(perm, value);
      case 760: return static_permuted_radical_inverse<T, 5801>(perm, value);
      case 761: return static_permuted_radical_inverse<T, 5807>(perm, value);
      case 762: return static_permuted_radical_inverse<T, 5813>(perm, value);
      case 763: return static_permuted_radical_inverse<T, 5821>(perm, value);
      case 764: return static_permuted_radical_inverse<T, 5827>(perm, value);
      case 765: return static_permuted_radical_inverse<T, 5839>(perm, value);
      case 766: return static_permuted_radical_inverse<T, 5843>(perm, value);
      case 767: return static_permuted_radical_inverse<T, 5849>(perm, value);
      case 768: return static_permuted_radical_inverse<T, 5851>(perm, value);
      case 769: return static_permuted_radical_inverse<T, 5857>(perm, value);
      case 770: return static_permuted_radical_inverse<T, 5861>(perm, value);
      case 771: return static_permuted_radical_inverse<T, 5867>(perm, value);
      case 772: return static_permuted_radical_inverse<T, 5869>(perm, value);
      case 773: return static_permuted_radical_inverse<T, 5879>(perm, value);
      case 774: return static_permuted_radical_inverse<T, 5881>(perm, value);
      case 775: return static_permuted_radical_inverse<T, 5897>(perm, value);
      case 776: return static_permuted_radical_inverse<T, 5903>(perm, value);
      case 777: return static_permuted_radical_inverse<T, 5923>(perm, value);
      case 778: return static_permuted_radical_inverse<T, 5927>(perm, value);
      case 779: return static_permuted_radical_inverse<T, 5939>(perm, value);
      case 780: return static_permuted_radical_inverse<T, 5953>(perm, value);
      case 781: return static_permuted_radical_inverse<T, 5981>(perm, value);
      case 782: return static_permuted_radical_inverse<T, 5987>(perm, value);
      case 783: return static_permuted_radical_inverse<T, 6007>(perm, value);
      case 784: return static_permuted_radical_inverse<T, 6011>(perm, value);
      case 785: return static_permuted_radical_inverse<T, 6029>(perm, value);
      case 786: return static_permuted_radical_inverse<T, 6037>(perm, value);
      case 787: return static_permuted_radical_inverse<T, 6043>(perm, value);
      case 788: return static_permuted_radical_inverse<T, 6047>(perm, value);
      case 789: return static_permuted_radical_inverse<T, 6053>(perm, value);
      case 790: return static_permuted_radical_inverse<T, 6067>(perm, value);
      case 791: return static_permuted_radical_inverse<T, 6073>(perm, value);
      case 792: return static_permuted_radical_inverse<T, 6079>(perm, value);
      case 793: return static_permuted_radical_inverse<T, 6089>(perm, value);
      case 794: return static_permuted_radical_inverse<T, 6091>(perm, value);
      case 795: return static_permuted_radical_inverse<T, 6101>(perm, value);
      case 796: return static_permuted_radical_inverse<T, 6113>(perm, value);
      case 797: return static_permuted_radical_inverse<T, 6121>(perm, value);
      case 798: return static_permuted_radical_inverse<T, 6131>(perm, value);
      case 799: return static_permuted_radical_inverse<T, 6133>(perm, value);
      case 800: return static_permuted_radical_inverse<T, 6143>(perm, value);
      case 801: return static_permuted_radical_inverse<T, 6151>(perm, value);
      case 802: return static_permuted_radical_inverse<T, 6163>(perm, value);
      case 803: return static_permuted_radical_inverse<T, 6173>(perm, value);
      case 804: return static_permuted_radical_inverse<T, 6197>(perm, value);
      case 805: return static_permuted_radical_inverse<T, 6199>(perm, value);
      case 806: return static_permuted_radical_inverse<T, 6203>(perm, value);
      case 807: return static_permuted_radical_inverse<T, 6211>(perm, value);
      case 808: return static_permuted_radical_inverse<T, 6217>(perm, value);
      case 809: return static_permuted_radical_inverse<T, 6221>(perm, value);
      case 810: return static_permuted_radical_inverse<T, 6229>(perm, value);
      case 811: return static_permuted_radical_inverse<T, 6247>(perm, value);
      case 812: return static_permuted_radical_inverse<T, 6257>(perm, value);
      case 813: return static_permuted_radical_inverse<T, 6263>(perm, value);
      case 814: return static_permuted_radical_inverse<T, 6269>(perm, value);
      case 815: return static_permuted_radical_inverse<T, 6271>(perm, value);
      case 816: return static_permuted_radical_inverse<T, 6277>(perm, value);
      case 817: return static_permuted_radical_inverse<T, 6287>(perm, value);
      case 818: return static_permuted_radical_inverse<T, 6299>(perm, value);
      case 819: return static_permuted_radical_inverse<T, 6301>(perm, value);
      case 820: return static_permuted_radical_inverse<T, 6311>(perm, value);
      case 821: return static_permuted_radical_inverse<T, 6317>(perm, value);
      case 822: return static_permuted_radical_inverse<T, 6323>(perm, value);
      case 823: return static_permuted_radical_inverse<T, 6329>(perm, value);
      case 824: return static_permuted_radical_inverse<T, 6337>(perm, value);
      case 825: return static_permuted_radical_inverse<T, 6343>(perm, value);
      case 826: return static_permuted_radical_inverse<T, 6353>(perm, value);
      case 827: return static_permuted_radical_inverse<T, 6359>(perm, value);
      case 828: return static_permuted_radical_inverse<T, 6361>(perm, value);
      case 829: return static_permuted_radical_inverse<T, 6367>(perm, value);
      case 830: return static_permuted_radical_inverse<T, 6373>(perm, value);
      case 831: return static_permuted_radical_inverse<T, 6379>(perm, value);
      case 832: return static_permuted_radical_inverse<T, 6389>(perm, value);
      case 833: return static_permuted_radical_inverse<T, 6397>(perm, value);
      case 834: return static_permuted_radical_inverse<T, 6421>(perm, value);
      case 835: return static_permuted_radical_inverse<T, 6427>(perm, value);
      case 836: return static_permuted_radical_inverse<T, 6449>(perm, value);
      case 837: return static_permuted_radical_inverse<T, 6451>(perm, value);
      case 838: return static_permuted_radical_inverse<T, 6469>(perm, value);
      case 839: return static_permuted_radical_inverse<T, 6473>(perm, value);
      case 840: return static_permuted_radical_inverse<T, 6481>(perm, value);
      case 841: return static_permuted_radical_inverse<T, 6491>(perm, value);
      case 842: return static_permuted_radical_inverse<T, 6521>(perm, value);
      case 843: return static_permuted_radical_inverse<T, 6529>(perm, value);
      case 844: return static_permuted_radical_inverse<T, 6547>(perm, value);
      case 845: return static_permuted_radical_inverse<T, 6551>(perm, value);
      case 846: return static_permuted_radical_inverse<T, 6553>(perm, value);
      case 847: return static_permuted_radical_inverse<T, 6563>(perm, value);
      case 848: return static_permuted_radical_inverse<T, 6569>(perm, value);
      case 849: return static_permuted_radical_inverse<T, 6571>(perm, value);
      case 850: return static_permuted_radical_inverse<T, 6577>(perm, value);
      case 851: return static_permuted_radical_inverse<T, 6581>(perm, value);
      case 852: return static_permuted_radical_inverse<T, 6599>(perm, value);
      case 853: return static_permuted_radical_inverse<T, 6607>(perm, value);
      case 854: return static_permuted_radical_inverse<T, 6619>(perm, value);
      case 855: return static_permuted_radical_inverse<T, 6637>(perm, value);
      case 856: return static_permuted_radical_inverse<T, 6653>(perm, value);
      case 857: return static_permuted_radical_inverse<T, 6659>(perm, value);
      case 858: return static_permuted_radical_inverse<T, 6661>(perm, value);
      case 859: return static_permuted_radical_inverse<T, 6673>(perm, value);
      case 860: return static_permuted_radical_inverse<T, 6679>(perm, value);
      case 861: return static_permuted_radical_inverse<T, 6689>(perm, value);
      case 862: return static_permuted_radical_inverse<T, 6691>(perm, value);
      case 863: return static_permuted_radical_inverse<T, 6701>(perm, value);
      case 864: return static_permuted_radical_inverse<T, 6703>(perm, value);
      case 865: return static_permuted_radical_inverse<T, 6709>(perm, value);
      case 866: return static_permuted_radical_inverse<T, 6719>(perm, value);
      case 867: return static_permuted_radical_inverse<T, 6733>(perm, value);
      case 868: return static_permuted_radical_inverse<T, 6737>(perm, value);
      case 869: return static_permuted_radical_inverse<T, 6761>(perm, value);
      case 870: return static_permuted_radical_inverse<T, 6763>(perm, value);
      case 871: return static_permuted_radical_inverse<T, 6779>(perm, value);
      case 872: return static_permuted_radical_inverse<T, 6781>(perm, value);
      case 873: return static_permuted_radical_inverse<T, 6791>(perm, value);
      case 874: return static_permuted_radical_inverse<T, 6793>(perm, value);
      case 875: return static_permuted_radical_inverse<T, 6803>(perm, value);
      case 876: return static_permuted_radical_inverse<T, 6823>(perm, value);
      case 877: return static_permuted_radical_inverse<T, 6827>(perm, value);
      case 878: return static_permuted_radical_inverse<T, 6829>(perm, value);
      case 879: return static_permuted_radical_inverse<T, 6833>(perm, value);
      case 880: return static_permuted_radical_inverse<T, 6841>(perm, value);
      case 881: return static_permuted_radical_inverse<T, 6857>(perm, value);
      case 882: return static_permuted_radical_inverse<T, 6863>(perm, value);
      case 883: return static_permuted_radical_inverse<T, 6869>(perm, value);
      case 884: return static_permuted_radical_inverse<T, 6871>(perm, value);
      case 885: return static_permuted_radical_inverse<T, 6883>(perm, value);
      case 886: return static_permuted_radical_inverse<T, 6899>(perm, value);
      case 887: return static_permuted_radical_inverse<T, 6907>(perm, value);
      case 888: return static_permuted_radical_inverse<T, 6911>(perm, value);
      case 889: return static_permuted_radical_inverse<T, 6917>(perm, value);
      case 890: return static_permuted_radical_inverse<T, 6947>(perm, value);
      case 891: return static_permuted_radical_inverse<T, 6949>(perm, value);
      case 892: return static_permuted_radical_inverse<T, 6959>(perm, value);
      case 893: return static_permuted_radical_inverse<T, 6961>(perm, value);
      case 894: return static_permuted_radical_inverse<T, 6967>(perm, value);
      case 895: return static_permuted_radical_inverse<T, 6971>(perm, value);
      case 896: return static_permuted_radical_inverse<T, 6977>(perm, value);
      case 897: return static_permuted_radical_inverse<T, 6983>(perm, value);
      case 898: return static_permuted_radical_inverse<T, 6991>(perm, value);
      case 899: return static_permuted_radical_inverse<T, 6997>(perm, value);
      case 900: return static_permuted_radical_inverse<T, 7001>(perm, value);
      case 901: return static_permuted_radical_inverse<T, 7013>(perm, value);
      case 902: return static_permuted_radical_inverse<T, 7019>(perm, value);
      case 903: return static_permuted_radical_inverse<T, 7027>(perm, value);
      case 904: return static_permuted_radical_inverse<T, 7039>(perm, value);
      case 905: return static_permuted_radical_inverse<T, 7043>(perm, value);
      case 906: return static_permuted_radical_inverse<T, 7057>(perm, value);
      case 907: return static_permuted_radical_inverse<T, 7069>(perm, value);
      case 908: return static_permuted_radical_inverse<T, 7079>(perm, value);
      case 909: return static_permuted_radical_inverse<T, 7103>(perm, value);
      case 910: return static_permuted_radical_inverse<T, 7109>(perm, value);
      case 911: return static_permuted_radical_inverse<T, 7121>(perm, value);
      case 912: return static_permuted_radical_inverse<T, 7127>(perm, value);
      case 913: return static_permuted_radical_inverse<T, 7129>(perm, value);
      case 914: return static_permuted_radical_inverse<T, 7151>(perm, value);
      case 915: return static_permuted_radical_inverse<T, 7159>(perm, value);
      case 916: return static_permuted_radical_inverse<T, 7177>(perm, value);
      case 917: return static_permuted_radical_inverse<T, 7187>(perm, value);
      case 918: return static_permuted_radical_inverse<T, 7193>(perm, value);
      case 919: return static_permuted_radical_inverse<T, 7207>(perm, value);
      case 920: return static_permuted_radical_inverse<T, 7211>(perm, value);
      case 921: return static_permuted_radical_inverse<T, 7213>(perm, value);
      case 922: return static_permuted_radical_inverse<T, 7219>(perm, value);
      case 923: return static_permuted_radical_inverse<T, 7229>(perm, value);
      case 924: return static_permuted_radical_inverse<T, 7237>(perm, value);
      case 925: return static_permuted_radical_inverse<T, 7243>(perm, value);
      case 926: return static_permuted_radical_inverse<T, 7247>(perm, value);
      case 927: return static_permuted_radical_inverse<T, 7253>(perm, value);
      case 928: return static_permuted_radical_inverse<T, 7283>(perm, value);
      case 929: return static_permuted_radical_inverse<T, 7297>(perm, value);
      case 930: return static_permuted_radical_inverse<T, 7307>(perm, value);
      case 931: return static_permuted_radical_inverse<T, 7309>(perm, value);
      case 932: return static_permuted_radical_inverse<T, 7321>(perm, value);
      case 933: return static_permuted_radical_inverse<T, 7331>(perm, value);
      case 934: return static_permuted_radical_inverse<T, 7333>(perm, value);
      case 935: return static_permuted_radical_inverse<T, 7349>(perm, value);
      case 936: return static_permuted_radical_inverse<T, 7351>(perm, value);
      case 937: return static_permuted_radical_inverse<T, 7369>(perm, value);
      case 938: return static_permuted_radical_inverse<T, 7393>(perm, value);
      case 939: return static_permuted_radical_inverse<T, 7411>(perm, value);
      case 940: return static_permuted_radical_inverse<T, 7417>(perm, value);
      case 941: return static_permuted_radical_inverse<T, 7433>(perm, value);
      case 942: return static_permuted_radical_inverse<T, 7451>(perm, value);
      case 943: return static_permuted_radical_inverse<T, 7457>(perm, value);
      case 944: return static_permuted_radical_inverse<T, 7459>(perm, value);
      case 945: return static_permuted_radical_inverse<T, 7477>(perm, value);
      case 946: return static_permuted_radical_inverse<T, 7481>(perm, value);
      case 947: return static_permuted_radical_inverse<T, 7487>(perm, value);
      case 948: return static_permuted_radical_inverse<T, 7489>(perm, value);
      case 949: return static_permuted_radical_inverse<T, 7499>(perm, value);
      case 950: return static_permuted_radical_inverse<T, 7507>(perm, value);
      case 951: return static_permuted_radical_inverse<T, 7517>(perm, value);
      case 952: return static_permuted_radical_inverse<T, 7523>(perm, value);
      case 953: return static_permuted_radical_inverse<T, 7529>(perm, value);
      case 954: return static_permuted_radical_inverse<T, 7537>(perm, value);
      case 955: return static_permuted_radical_inverse<T, 7541>(perm, value);
      case 956: return static_permuted_radical_inverse<T, 7547>(perm, value);
      case 957: return static_permuted_radical_inverse<T, 7549>(perm, value);
      case 958: return static_permuted_radical_inverse<T, 7559>(perm, value);
      case 959: return static_permuted_radical_inverse<T, 7561>(perm, value);
      case 960: return static_permuted_radical_inverse<T, 7573>(perm, value);
      case 961: return static_permuted_radical_inverse<T, 7577>(perm, value);
      case 962: return static_permuted_radical_inverse<T, 7583>(perm, value);
      case 963: return static_permuted_radical_inverse<T, 7589>(perm, value);
      case 964: return static_permuted_radical_inverse<T, 7591>(perm, value);
      case 965: return static_permuted_radical_inverse<T, 7603>(perm, value);
      case 966: return static_permuted_radical_inverse<T, 7607>(perm, value);
      case 967: return static_permuted_radical_inverse<T, 7621>(perm, value);
      case 968: return static_permuted_radical_inverse<T, 7639>(perm, value);
      case 969: return static_permuted_radical_inverse<T, 7643>(perm, value);
      case 970: return static_permuted_radical_inverse<T, 7649>(perm, value);
      case 971: return static_permuted_radical_inverse<T, 7669>(perm, value);
      case 972: return static_permuted_radical_inverse<T, 7673>(perm, value);
      case 973: return static_permuted_radical_inverse<T, 7681>(perm, value);
      case 974: return static_permuted_radical_inverse<T, 7687>(perm, value);
      case 975: return static_permuted_radical_inverse<T, 7691>(perm, value);
      case 976: return static_permuted_radical_inverse<T, 7699>(perm, value);
      case 977: return static_permuted_radical_inverse<T, 7703>(perm, value);
      case 978: return static_permuted_radical_inverse<T, 7717>(perm, value);
      case 979: return static_permuted_radical_inverse<T, 7723>(perm, value);
      case 980: return static_permuted_radical_inverse<T, 7727>(perm, value);
      case 981: return static_permuted_radical_inverse<T, 7741>(perm, value);
      case 982: return static_permuted_radical_inverse<T, 7753>(perm, value);
      case 983: return static_permuted_radical_inverse<T, 7757>(perm, value);
      case 984: return static_permuted_radical_inverse<T, 7759>(perm, value);
      case 985: return static_permuted_radical_inverse<T, 7789>(perm, value);
      case 986: return static_permuted_radical_inverse<T, 7793>(perm, value);
      case 987: return static_permuted_radical_inverse<T, 7817>(perm, value);
      case 988: return static_permuted_radical_inverse<T, 7823>(perm, value);
      case 989: return static_permuted_radical_inverse<T, 7829>(perm, value);
      case 990: return static_permuted_radical_inverse<T, 7841>(perm, value);
      case 991: return static_permuted_radical_inverse<T, 7853>(perm, value);
      case 992: return static_permuted_radical_inverse<T, 7867>(perm, value);
      case 993: return static_permuted_radical_inverse<T, 7873>(perm, value);
      case 994: return static_permuted_radical_inverse<T, 7877>(perm, value);
      case 995: return static_permuted_radical_inverse<T, 7879>(perm, value);
      case 996: return static_permuted_radical_inverse<T, 7883>(perm, value);
      case 997: return static_permuted_radical_inverse<T, 7901>(perm, value);
      case 998: return static_permuted_radical_inverse<T, 7907>(perm, value);
      case 999: return static_permuted_radical_inverse<T, 7919>(perm, value);
      default:
#if defined NDEBUG && defined _MSC_VER
        __assume(0);
#else
        assert(false);
        return T(0.0);
#endif
    }
}


//
// Halton sequences implementation.
//

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_sequence(
    const size_t        bases[],
    const size_t        i)
{
    Vector<T, Dim> p;

    for (size_t d = 0; d < Dim; ++d)
    {
        const size_t base = bases[d];

        p[d] =
            base == 2
                ? radical_inverse_base2<T>(i)
                : radical_inverse<T>(base, i);
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_sequence(
    const size_t        bases[],
    const size_t        perms[],
    const size_t        i)
{
    Vector<T, Dim> p;

    for (size_t d = 0; d < Dim; ++d)
    {
        const size_t base = bases[d];

        p[d] =
            base == 2
                ? radical_inverse_base2<T>(i)
                : permuted_radical_inverse<T>(base, perms, i);

        perms += base;
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_zaremba_sequence(
    const size_t        bases[],
    const size_t        i)
{
    Vector<T, Dim> p;

    for (size_t d = 0; d < Dim; ++d)
    {
        const size_t base = bases[d];

        p[d] =
            base == 2
                ? folded_radical_inverse_base2<T>(i)
                : folded_radical_inverse<T>(base, i);
    }

    return p;
}


//
// Hammersley sequences implementation.
//

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],
    const size_t        count,
    const size_t        i)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(i) / count;

    for (size_t d = 1; d < Dim; ++d)
    {
        const size_t base = bases[d - 1];

        p[d] =
            base == 2
                ? radical_inverse_base2<T>(i)
                : radical_inverse<T>(base, i);
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],
    const size_t        perms[],
    const size_t        count,
    const size_t        i)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(i) / count;

    for (size_t d = 1; d < Dim; ++d)
    {
        const size_t base = bases[d - 1];

        p[d] =
            base == 2
                ? radical_inverse_base2<T>(i)
                : permuted_radical_inverse<T>(base, perms, i);

        perms += base;
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_zaremba_sequence(
    const size_t        bases[],
    const size_t        count,
    const size_t        i)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(i) / count;

    for (size_t d = 1; d < Dim; ++d)
    {
        const size_t base = bases[d - 1];

        p[d] =
            base == 2
                ? folded_radical_inverse_base2<T>(i)
                : folded_radical_inverse<T>(bases[d - 1], i);
    }

    return p;
}

}   // namespace foundation
