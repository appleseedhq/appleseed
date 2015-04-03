
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_QMC_H
#define APPLESEED_FOUNDATION_MATH_QMC_H

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

// Boost headers.
#include "boost/static_assert.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>

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
    size_t              input);         // input digits

// Folded radical inverse in base 2.
template <typename T>
T folded_radical_inverse_base2(
    size_t              input);         // input digits


//
// Arbitrary-base radical inverse functions.
//
// All return values are in the interval [0, 1).
//

// Radical inverse in arbitrary base. No fast code path for base 2.
template <typename T>
T radical_inverse(
    const size_t        base,           // base (prime number)
    const size_t        input);         // input digits

// Radical inverse in arbitrary base, with the base specified at compile-time.
template <typename T, isize_t Base>
T static_radical_inverse(
    const size_t        input);         // input digits

// Fast variant of radical_inverse() for the first 1000 prime bases.
template <typename T>
T fast_radical_inverse(
    const size_t        base_index,     // index of the prime base, starting at 0 (base 2)
    const size_t        input);         // input digits

// Folded radical inverse in arbitrary base. No fast code path for base 2.
template <typename T>
T folded_radical_inverse(
    const size_t        base,           // base (prime number)
    const size_t        input);         // input digits

// Radical inverse in arbitrary base with digits permutation. No fast code path for base 2.
template <typename T>
T permuted_radical_inverse(
    const size_t        base,           // base (prime number)
    const size_t        perm[],         // digit permutation table (base entries)
    const size_t        input);         // input digits

// Radical inverse in arbitrary base with digits permutation, with the base specified at compile-time.
template <typename T, isize_t Base>
T static_permuted_radical_inverse(
    const size_t        perm[],         // digit permutation table (Base entries)
    const size_t        input);         // input digits

// Fast variant of permuted_radical_inverse() for the first 1000 prime bases.
template <typename T>
T fast_permuted_radical_inverse(
    const size_t        base_index,     // index of the prime base, starting at 0 (base 2)
    const size_t        perm[],         // digit permutation table (Primes[base_index] entries)
    const size_t        input);         // input digits


//
// Halton sequences of arbitrary dimensions.
//
// All return values are in the interval [0, 1)^Dim.
//

// Return the input'th sample of a Halton sequence.
template <typename T, size_t Dim>
Vector<T, Dim> halton_sequence(
    const size_t        bases[],        // bases (Dim entries, prime numbers)
    const size_t        input);         // input digits

// Return the input'th sample of a Halton sequence with digits permutation.
template <typename T, size_t Dim>
Vector<T, Dim> halton_sequence(
    const size_t        bases[],        // bases (Dim entries, prime numbers)
    const size_t        perms[],        // permutation tables, one per dimension
    const size_t        input);         // input digits

// Return the input'th sample of a Halton-Zaremba sequence (using folded radical inverse).
template <typename T, size_t Dim>
Vector<T, Dim> halton_zaremba_sequence(
    const size_t        bases[],        // bases (Dim entries, prime numbers)
    const size_t        input);         // input digits


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

// Return the input'th sample of a Hammersley sequence
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],        // bases (Dim-1 entries, prime numbers)
    const size_t        input,          // input digits
    const size_t        count);         // total number of samples in sequence

// Return the input'th sample of a Hammersley sequence with digits permutation.
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],        // bases (Dim-1 entries, prime numbers)
    const size_t        perms[],        // permutation tables, one per base
    const size_t        input,          // input digits
    const size_t        count);         // total number of samples in sequence

// Return the input'th sample of a Hammersley-Zaremba sequence (using folded radical inverse).
template <typename T, size_t Dim>
Vector<T, Dim> hammersley_zaremba_sequence(
    const size_t        bases[],        // bases (Dim-1 entries, prime numbers)
    const size_t        input,          // input digits
    const size_t        count);         // total number of samples in sequence



//
// Base-2 radical inverse functions implementation.
//

template <typename T>
inline T radical_inverse_base2(
    size_t              input)
{
    input = (input >> 16) | (input << 16);                                  // 16-bit swap
    input = ((input & 0xFF00FF00) >> 8) | ((input & 0x00FF00FF) << 8);      // 8-bit swap
    input = ((input & 0xF0F0F0F0) >> 4) | ((input & 0x0F0F0F0F) << 4);      // 4-bit swap
    input = ((input & 0xCCCCCCCC) >> 2) | ((input & 0x33333333) << 2);      // 2-bit swap
    input = ((input & 0xAAAAAAAA) >> 1) | ((input & 0x55555555) << 1);      // 1-bit swap

    return static_cast<T>(input) / static_cast<T>(0x100000000LL);
}

template <typename T>
inline T folded_radical_inverse_base2(
    size_t              input)
{
    T x = T(0.0);
    T b = T(0.5);
    size_t offset = 0;

    while (x + b > x)
    {
        if ((input + offset) & 1)
            x += b;
        b *= T(0.5);
        input >>= 1;
        ++offset;
    }

    return x;
}


//
// Arbitrary-base radical inverse functions implementation.
//

template <typename T>
inline T radical_inverse(
    const size_t        base,
    const size_t        input)
{
    assert(base >= 2);

    const isize_t signed_base = static_cast<isize_t>(base);
    const T rcp_base = T(1.0) / signed_base;

    isize_t i = static_cast<isize_t>(input);
    isize_t x = 0;
    T b = T(1.0);

    while (i)
    {
        x *= signed_base;
        x += i % signed_base;
        i /= signed_base;
        b *= rcp_base;
    }

    return static_cast<T>(x) * b;
}

template <typename T, isize_t Base>
inline T static_radical_inverse(
    const size_t        input)
{
    BOOST_STATIC_ASSERT(Base >= 2);

    const T rcp_base = T(1.0) / Base;

    isize_t i = static_cast<isize_t>(input);
    isize_t x = 0;
    T b = T(1.0);

    while (i)
    {
        x *= Base;
        x += i % Base;
        i /= Base;
        b *= rcp_base;
    }

    return static_cast<T>(x) * b;
}

template <typename T>
T fast_radical_inverse(
    const size_t        base_index,
    const size_t        input)
{
    switch (base_index)
    {
      case   0: return radical_inverse_base2<T>(input);
      case   1: return static_radical_inverse<T, 3>(input);
      case   2: return static_radical_inverse<T, 5>(input);
      case   3: return static_radical_inverse<T, 7>(input);
      case   4: return static_radical_inverse<T, 11>(input);
      case   5: return static_radical_inverse<T, 13>(input);
      case   6: return static_radical_inverse<T, 17>(input);
      case   7: return static_radical_inverse<T, 19>(input);
      case   8: return static_radical_inverse<T, 23>(input);
      case   9: return static_radical_inverse<T, 29>(input);
      case  10: return static_radical_inverse<T, 31>(input);
      case  11: return static_radical_inverse<T, 37>(input);
      case  12: return static_radical_inverse<T, 41>(input);
      case  13: return static_radical_inverse<T, 43>(input);
      case  14: return static_radical_inverse<T, 47>(input);
      case  15: return static_radical_inverse<T, 53>(input);
      case  16: return static_radical_inverse<T, 59>(input);
      case  17: return static_radical_inverse<T, 61>(input);
      case  18: return static_radical_inverse<T, 67>(input);
      case  19: return static_radical_inverse<T, 71>(input);
      case  20: return static_radical_inverse<T, 73>(input);
      case  21: return static_radical_inverse<T, 79>(input);
      case  22: return static_radical_inverse<T, 83>(input);
      case  23: return static_radical_inverse<T, 89>(input);
      case  24: return static_radical_inverse<T, 97>(input);
      case  25: return static_radical_inverse<T, 101>(input);
      case  26: return static_radical_inverse<T, 103>(input);
      case  27: return static_radical_inverse<T, 107>(input);
      case  28: return static_radical_inverse<T, 109>(input);
      case  29: return static_radical_inverse<T, 113>(input);
      case  30: return static_radical_inverse<T, 127>(input);
      case  31: return static_radical_inverse<T, 131>(input);
      case  32: return static_radical_inverse<T, 137>(input);
      case  33: return static_radical_inverse<T, 139>(input);
      case  34: return static_radical_inverse<T, 149>(input);
      case  35: return static_radical_inverse<T, 151>(input);
      case  36: return static_radical_inverse<T, 157>(input);
      case  37: return static_radical_inverse<T, 163>(input);
      case  38: return static_radical_inverse<T, 167>(input);
      case  39: return static_radical_inverse<T, 173>(input);
      case  40: return static_radical_inverse<T, 179>(input);
      case  41: return static_radical_inverse<T, 181>(input);
      case  42: return static_radical_inverse<T, 191>(input);
      case  43: return static_radical_inverse<T, 193>(input);
      case  44: return static_radical_inverse<T, 197>(input);
      case  45: return static_radical_inverse<T, 199>(input);
      case  46: return static_radical_inverse<T, 211>(input);
      case  47: return static_radical_inverse<T, 223>(input);
      case  48: return static_radical_inverse<T, 227>(input);
      case  49: return static_radical_inverse<T, 229>(input);
      case  50: return static_radical_inverse<T, 233>(input);
      case  51: return static_radical_inverse<T, 239>(input);
      case  52: return static_radical_inverse<T, 241>(input);
      case  53: return static_radical_inverse<T, 251>(input);
      case  54: return static_radical_inverse<T, 257>(input);
      case  55: return static_radical_inverse<T, 263>(input);
      case  56: return static_radical_inverse<T, 269>(input);
      case  57: return static_radical_inverse<T, 271>(input);
      case  58: return static_radical_inverse<T, 277>(input);
      case  59: return static_radical_inverse<T, 281>(input);
      case  60: return static_radical_inverse<T, 283>(input);
      case  61: return static_radical_inverse<T, 293>(input);
      case  62: return static_radical_inverse<T, 307>(input);
      case  63: return static_radical_inverse<T, 311>(input);
      case  64: return static_radical_inverse<T, 313>(input);
      case  65: return static_radical_inverse<T, 317>(input);
      case  66: return static_radical_inverse<T, 331>(input);
      case  67: return static_radical_inverse<T, 337>(input);
      case  68: return static_radical_inverse<T, 347>(input);
      case  69: return static_radical_inverse<T, 349>(input);
      case  70: return static_radical_inverse<T, 353>(input);
      case  71: return static_radical_inverse<T, 359>(input);
      case  72: return static_radical_inverse<T, 367>(input);
      case  73: return static_radical_inverse<T, 373>(input);
      case  74: return static_radical_inverse<T, 379>(input);
      case  75: return static_radical_inverse<T, 383>(input);
      case  76: return static_radical_inverse<T, 389>(input);
      case  77: return static_radical_inverse<T, 397>(input);
      case  78: return static_radical_inverse<T, 401>(input);
      case  79: return static_radical_inverse<T, 409>(input);
      case  80: return static_radical_inverse<T, 419>(input);
      case  81: return static_radical_inverse<T, 421>(input);
      case  82: return static_radical_inverse<T, 431>(input);
      case  83: return static_radical_inverse<T, 433>(input);
      case  84: return static_radical_inverse<T, 439>(input);
      case  85: return static_radical_inverse<T, 443>(input);
      case  86: return static_radical_inverse<T, 449>(input);
      case  87: return static_radical_inverse<T, 457>(input);
      case  88: return static_radical_inverse<T, 461>(input);
      case  89: return static_radical_inverse<T, 463>(input);
      case  90: return static_radical_inverse<T, 467>(input);
      case  91: return static_radical_inverse<T, 479>(input);
      case  92: return static_radical_inverse<T, 487>(input);
      case  93: return static_radical_inverse<T, 491>(input);
      case  94: return static_radical_inverse<T, 499>(input);
      case  95: return static_radical_inverse<T, 503>(input);
      case  96: return static_radical_inverse<T, 509>(input);
      case  97: return static_radical_inverse<T, 521>(input);
      case  98: return static_radical_inverse<T, 523>(input);
      case  99: return static_radical_inverse<T, 541>(input);
      case 100: return static_radical_inverse<T, 547>(input);
      case 101: return static_radical_inverse<T, 557>(input);
      case 102: return static_radical_inverse<T, 563>(input);
      case 103: return static_radical_inverse<T, 569>(input);
      case 104: return static_radical_inverse<T, 571>(input);
      case 105: return static_radical_inverse<T, 577>(input);
      case 106: return static_radical_inverse<T, 587>(input);
      case 107: return static_radical_inverse<T, 593>(input);
      case 108: return static_radical_inverse<T, 599>(input);
      case 109: return static_radical_inverse<T, 601>(input);
      case 110: return static_radical_inverse<T, 607>(input);
      case 111: return static_radical_inverse<T, 613>(input);
      case 112: return static_radical_inverse<T, 617>(input);
      case 113: return static_radical_inverse<T, 619>(input);
      case 114: return static_radical_inverse<T, 631>(input);
      case 115: return static_radical_inverse<T, 641>(input);
      case 116: return static_radical_inverse<T, 643>(input);
      case 117: return static_radical_inverse<T, 647>(input);
      case 118: return static_radical_inverse<T, 653>(input);
      case 119: return static_radical_inverse<T, 659>(input);
      case 120: return static_radical_inverse<T, 661>(input);
      case 121: return static_radical_inverse<T, 673>(input);
      case 122: return static_radical_inverse<T, 677>(input);
      case 123: return static_radical_inverse<T, 683>(input);
      case 124: return static_radical_inverse<T, 691>(input);
      case 125: return static_radical_inverse<T, 701>(input);
      case 126: return static_radical_inverse<T, 709>(input);
      case 127: return static_radical_inverse<T, 719>(input);
      case 128: return static_radical_inverse<T, 727>(input);
      case 129: return static_radical_inverse<T, 733>(input);
      case 130: return static_radical_inverse<T, 739>(input);
      case 131: return static_radical_inverse<T, 743>(input);
      case 132: return static_radical_inverse<T, 751>(input);
      case 133: return static_radical_inverse<T, 757>(input);
      case 134: return static_radical_inverse<T, 761>(input);
      case 135: return static_radical_inverse<T, 769>(input);
      case 136: return static_radical_inverse<T, 773>(input);
      case 137: return static_radical_inverse<T, 787>(input);
      case 138: return static_radical_inverse<T, 797>(input);
      case 139: return static_radical_inverse<T, 809>(input);
      case 140: return static_radical_inverse<T, 811>(input);
      case 141: return static_radical_inverse<T, 821>(input);
      case 142: return static_radical_inverse<T, 823>(input);
      case 143: return static_radical_inverse<T, 827>(input);
      case 144: return static_radical_inverse<T, 829>(input);
      case 145: return static_radical_inverse<T, 839>(input);
      case 146: return static_radical_inverse<T, 853>(input);
      case 147: return static_radical_inverse<T, 857>(input);
      case 148: return static_radical_inverse<T, 859>(input);
      case 149: return static_radical_inverse<T, 863>(input);
      case 150: return static_radical_inverse<T, 877>(input);
      case 151: return static_radical_inverse<T, 881>(input);
      case 152: return static_radical_inverse<T, 883>(input);
      case 153: return static_radical_inverse<T, 887>(input);
      case 154: return static_radical_inverse<T, 907>(input);
      case 155: return static_radical_inverse<T, 911>(input);
      case 156: return static_radical_inverse<T, 919>(input);
      case 157: return static_radical_inverse<T, 929>(input);
      case 158: return static_radical_inverse<T, 937>(input);
      case 159: return static_radical_inverse<T, 941>(input);
      case 160: return static_radical_inverse<T, 947>(input);
      case 161: return static_radical_inverse<T, 953>(input);
      case 162: return static_radical_inverse<T, 967>(input);
      case 163: return static_radical_inverse<T, 971>(input);
      case 164: return static_radical_inverse<T, 977>(input);
      case 165: return static_radical_inverse<T, 983>(input);
      case 166: return static_radical_inverse<T, 991>(input);
      case 167: return static_radical_inverse<T, 997>(input);
      case 168: return static_radical_inverse<T, 1009>(input);
      case 169: return static_radical_inverse<T, 1013>(input);
      case 170: return static_radical_inverse<T, 1019>(input);
      case 171: return static_radical_inverse<T, 1021>(input);
      case 172: return static_radical_inverse<T, 1031>(input);
      case 173: return static_radical_inverse<T, 1033>(input);
      case 174: return static_radical_inverse<T, 1039>(input);
      case 175: return static_radical_inverse<T, 1049>(input);
      case 176: return static_radical_inverse<T, 1051>(input);
      case 177: return static_radical_inverse<T, 1061>(input);
      case 178: return static_radical_inverse<T, 1063>(input);
      case 179: return static_radical_inverse<T, 1069>(input);
      case 180: return static_radical_inverse<T, 1087>(input);
      case 181: return static_radical_inverse<T, 1091>(input);
      case 182: return static_radical_inverse<T, 1093>(input);
      case 183: return static_radical_inverse<T, 1097>(input);
      case 184: return static_radical_inverse<T, 1103>(input);
      case 185: return static_radical_inverse<T, 1109>(input);
      case 186: return static_radical_inverse<T, 1117>(input);
      case 187: return static_radical_inverse<T, 1123>(input);
      case 188: return static_radical_inverse<T, 1129>(input);
      case 189: return static_radical_inverse<T, 1151>(input);
      case 190: return static_radical_inverse<T, 1153>(input);
      case 191: return static_radical_inverse<T, 1163>(input);
      case 192: return static_radical_inverse<T, 1171>(input);
      case 193: return static_radical_inverse<T, 1181>(input);
      case 194: return static_radical_inverse<T, 1187>(input);
      case 195: return static_radical_inverse<T, 1193>(input);
      case 196: return static_radical_inverse<T, 1201>(input);
      case 197: return static_radical_inverse<T, 1213>(input);
      case 198: return static_radical_inverse<T, 1217>(input);
      case 199: return static_radical_inverse<T, 1223>(input);
      case 200: return static_radical_inverse<T, 1229>(input);
      case 201: return static_radical_inverse<T, 1231>(input);
      case 202: return static_radical_inverse<T, 1237>(input);
      case 203: return static_radical_inverse<T, 1249>(input);
      case 204: return static_radical_inverse<T, 1259>(input);
      case 205: return static_radical_inverse<T, 1277>(input);
      case 206: return static_radical_inverse<T, 1279>(input);
      case 207: return static_radical_inverse<T, 1283>(input);
      case 208: return static_radical_inverse<T, 1289>(input);
      case 209: return static_radical_inverse<T, 1291>(input);
      case 210: return static_radical_inverse<T, 1297>(input);
      case 211: return static_radical_inverse<T, 1301>(input);
      case 212: return static_radical_inverse<T, 1303>(input);
      case 213: return static_radical_inverse<T, 1307>(input);
      case 214: return static_radical_inverse<T, 1319>(input);
      case 215: return static_radical_inverse<T, 1321>(input);
      case 216: return static_radical_inverse<T, 1327>(input);
      case 217: return static_radical_inverse<T, 1361>(input);
      case 218: return static_radical_inverse<T, 1367>(input);
      case 219: return static_radical_inverse<T, 1373>(input);
      case 220: return static_radical_inverse<T, 1381>(input);
      case 221: return static_radical_inverse<T, 1399>(input);
      case 222: return static_radical_inverse<T, 1409>(input);
      case 223: return static_radical_inverse<T, 1423>(input);
      case 224: return static_radical_inverse<T, 1427>(input);
      case 225: return static_radical_inverse<T, 1429>(input);
      case 226: return static_radical_inverse<T, 1433>(input);
      case 227: return static_radical_inverse<T, 1439>(input);
      case 228: return static_radical_inverse<T, 1447>(input);
      case 229: return static_radical_inverse<T, 1451>(input);
      case 230: return static_radical_inverse<T, 1453>(input);
      case 231: return static_radical_inverse<T, 1459>(input);
      case 232: return static_radical_inverse<T, 1471>(input);
      case 233: return static_radical_inverse<T, 1481>(input);
      case 234: return static_radical_inverse<T, 1483>(input);
      case 235: return static_radical_inverse<T, 1487>(input);
      case 236: return static_radical_inverse<T, 1489>(input);
      case 237: return static_radical_inverse<T, 1493>(input);
      case 238: return static_radical_inverse<T, 1499>(input);
      case 239: return static_radical_inverse<T, 1511>(input);
      case 240: return static_radical_inverse<T, 1523>(input);
      case 241: return static_radical_inverse<T, 1531>(input);
      case 242: return static_radical_inverse<T, 1543>(input);
      case 243: return static_radical_inverse<T, 1549>(input);
      case 244: return static_radical_inverse<T, 1553>(input);
      case 245: return static_radical_inverse<T, 1559>(input);
      case 246: return static_radical_inverse<T, 1567>(input);
      case 247: return static_radical_inverse<T, 1571>(input);
      case 248: return static_radical_inverse<T, 1579>(input);
      case 249: return static_radical_inverse<T, 1583>(input);
      case 250: return static_radical_inverse<T, 1597>(input);
      case 251: return static_radical_inverse<T, 1601>(input);
      case 252: return static_radical_inverse<T, 1607>(input);
      case 253: return static_radical_inverse<T, 1609>(input);
      case 254: return static_radical_inverse<T, 1613>(input);
      case 255: return static_radical_inverse<T, 1619>(input);
      case 256: return static_radical_inverse<T, 1621>(input);
      case 257: return static_radical_inverse<T, 1627>(input);
      case 258: return static_radical_inverse<T, 1637>(input);
      case 259: return static_radical_inverse<T, 1657>(input);
      case 260: return static_radical_inverse<T, 1663>(input);
      case 261: return static_radical_inverse<T, 1667>(input);
      case 262: return static_radical_inverse<T, 1669>(input);
      case 263: return static_radical_inverse<T, 1693>(input);
      case 264: return static_radical_inverse<T, 1697>(input);
      case 265: return static_radical_inverse<T, 1699>(input);
      case 266: return static_radical_inverse<T, 1709>(input);
      case 267: return static_radical_inverse<T, 1721>(input);
      case 268: return static_radical_inverse<T, 1723>(input);
      case 269: return static_radical_inverse<T, 1733>(input);
      case 270: return static_radical_inverse<T, 1741>(input);
      case 271: return static_radical_inverse<T, 1747>(input);
      case 272: return static_radical_inverse<T, 1753>(input);
      case 273: return static_radical_inverse<T, 1759>(input);
      case 274: return static_radical_inverse<T, 1777>(input);
      case 275: return static_radical_inverse<T, 1783>(input);
      case 276: return static_radical_inverse<T, 1787>(input);
      case 277: return static_radical_inverse<T, 1789>(input);
      case 278: return static_radical_inverse<T, 1801>(input);
      case 279: return static_radical_inverse<T, 1811>(input);
      case 280: return static_radical_inverse<T, 1823>(input);
      case 281: return static_radical_inverse<T, 1831>(input);
      case 282: return static_radical_inverse<T, 1847>(input);
      case 283: return static_radical_inverse<T, 1861>(input);
      case 284: return static_radical_inverse<T, 1867>(input);
      case 285: return static_radical_inverse<T, 1871>(input);
      case 286: return static_radical_inverse<T, 1873>(input);
      case 287: return static_radical_inverse<T, 1877>(input);
      case 288: return static_radical_inverse<T, 1879>(input);
      case 289: return static_radical_inverse<T, 1889>(input);
      case 290: return static_radical_inverse<T, 1901>(input);
      case 291: return static_radical_inverse<T, 1907>(input);
      case 292: return static_radical_inverse<T, 1913>(input);
      case 293: return static_radical_inverse<T, 1931>(input);
      case 294: return static_radical_inverse<T, 1933>(input);
      case 295: return static_radical_inverse<T, 1949>(input);
      case 296: return static_radical_inverse<T, 1951>(input);
      case 297: return static_radical_inverse<T, 1973>(input);
      case 298: return static_radical_inverse<T, 1979>(input);
      case 299: return static_radical_inverse<T, 1987>(input);
      case 300: return static_radical_inverse<T, 1993>(input);
      case 301: return static_radical_inverse<T, 1997>(input);
      case 302: return static_radical_inverse<T, 1999>(input);
      case 303: return static_radical_inverse<T, 2003>(input);
      case 304: return static_radical_inverse<T, 2011>(input);
      case 305: return static_radical_inverse<T, 2017>(input);
      case 306: return static_radical_inverse<T, 2027>(input);
      case 307: return static_radical_inverse<T, 2029>(input);
      case 308: return static_radical_inverse<T, 2039>(input);
      case 309: return static_radical_inverse<T, 2053>(input);
      case 310: return static_radical_inverse<T, 2063>(input);
      case 311: return static_radical_inverse<T, 2069>(input);
      case 312: return static_radical_inverse<T, 2081>(input);
      case 313: return static_radical_inverse<T, 2083>(input);
      case 314: return static_radical_inverse<T, 2087>(input);
      case 315: return static_radical_inverse<T, 2089>(input);
      case 316: return static_radical_inverse<T, 2099>(input);
      case 317: return static_radical_inverse<T, 2111>(input);
      case 318: return static_radical_inverse<T, 2113>(input);
      case 319: return static_radical_inverse<T, 2129>(input);
      case 320: return static_radical_inverse<T, 2131>(input);
      case 321: return static_radical_inverse<T, 2137>(input);
      case 322: return static_radical_inverse<T, 2141>(input);
      case 323: return static_radical_inverse<T, 2143>(input);
      case 324: return static_radical_inverse<T, 2153>(input);
      case 325: return static_radical_inverse<T, 2161>(input);
      case 326: return static_radical_inverse<T, 2179>(input);
      case 327: return static_radical_inverse<T, 2203>(input);
      case 328: return static_radical_inverse<T, 2207>(input);
      case 329: return static_radical_inverse<T, 2213>(input);
      case 330: return static_radical_inverse<T, 2221>(input);
      case 331: return static_radical_inverse<T, 2237>(input);
      case 332: return static_radical_inverse<T, 2239>(input);
      case 333: return static_radical_inverse<T, 2243>(input);
      case 334: return static_radical_inverse<T, 2251>(input);
      case 335: return static_radical_inverse<T, 2267>(input);
      case 336: return static_radical_inverse<T, 2269>(input);
      case 337: return static_radical_inverse<T, 2273>(input);
      case 338: return static_radical_inverse<T, 2281>(input);
      case 339: return static_radical_inverse<T, 2287>(input);
      case 340: return static_radical_inverse<T, 2293>(input);
      case 341: return static_radical_inverse<T, 2297>(input);
      case 342: return static_radical_inverse<T, 2309>(input);
      case 343: return static_radical_inverse<T, 2311>(input);
      case 344: return static_radical_inverse<T, 2333>(input);
      case 345: return static_radical_inverse<T, 2339>(input);
      case 346: return static_radical_inverse<T, 2341>(input);
      case 347: return static_radical_inverse<T, 2347>(input);
      case 348: return static_radical_inverse<T, 2351>(input);
      case 349: return static_radical_inverse<T, 2357>(input);
      case 350: return static_radical_inverse<T, 2371>(input);
      case 351: return static_radical_inverse<T, 2377>(input);
      case 352: return static_radical_inverse<T, 2381>(input);
      case 353: return static_radical_inverse<T, 2383>(input);
      case 354: return static_radical_inverse<T, 2389>(input);
      case 355: return static_radical_inverse<T, 2393>(input);
      case 356: return static_radical_inverse<T, 2399>(input);
      case 357: return static_radical_inverse<T, 2411>(input);
      case 358: return static_radical_inverse<T, 2417>(input);
      case 359: return static_radical_inverse<T, 2423>(input);
      case 360: return static_radical_inverse<T, 2437>(input);
      case 361: return static_radical_inverse<T, 2441>(input);
      case 362: return static_radical_inverse<T, 2447>(input);
      case 363: return static_radical_inverse<T, 2459>(input);
      case 364: return static_radical_inverse<T, 2467>(input);
      case 365: return static_radical_inverse<T, 2473>(input);
      case 366: return static_radical_inverse<T, 2477>(input);
      case 367: return static_radical_inverse<T, 2503>(input);
      case 368: return static_radical_inverse<T, 2521>(input);
      case 369: return static_radical_inverse<T, 2531>(input);
      case 370: return static_radical_inverse<T, 2539>(input);
      case 371: return static_radical_inverse<T, 2543>(input);
      case 372: return static_radical_inverse<T, 2549>(input);
      case 373: return static_radical_inverse<T, 2551>(input);
      case 374: return static_radical_inverse<T, 2557>(input);
      case 375: return static_radical_inverse<T, 2579>(input);
      case 376: return static_radical_inverse<T, 2591>(input);
      case 377: return static_radical_inverse<T, 2593>(input);
      case 378: return static_radical_inverse<T, 2609>(input);
      case 379: return static_radical_inverse<T, 2617>(input);
      case 380: return static_radical_inverse<T, 2621>(input);
      case 381: return static_radical_inverse<T, 2633>(input);
      case 382: return static_radical_inverse<T, 2647>(input);
      case 383: return static_radical_inverse<T, 2657>(input);
      case 384: return static_radical_inverse<T, 2659>(input);
      case 385: return static_radical_inverse<T, 2663>(input);
      case 386: return static_radical_inverse<T, 2671>(input);
      case 387: return static_radical_inverse<T, 2677>(input);
      case 388: return static_radical_inverse<T, 2683>(input);
      case 389: return static_radical_inverse<T, 2687>(input);
      case 390: return static_radical_inverse<T, 2689>(input);
      case 391: return static_radical_inverse<T, 2693>(input);
      case 392: return static_radical_inverse<T, 2699>(input);
      case 393: return static_radical_inverse<T, 2707>(input);
      case 394: return static_radical_inverse<T, 2711>(input);
      case 395: return static_radical_inverse<T, 2713>(input);
      case 396: return static_radical_inverse<T, 2719>(input);
      case 397: return static_radical_inverse<T, 2729>(input);
      case 398: return static_radical_inverse<T, 2731>(input);
      case 399: return static_radical_inverse<T, 2741>(input);
      case 400: return static_radical_inverse<T, 2749>(input);
      case 401: return static_radical_inverse<T, 2753>(input);
      case 402: return static_radical_inverse<T, 2767>(input);
      case 403: return static_radical_inverse<T, 2777>(input);
      case 404: return static_radical_inverse<T, 2789>(input);
      case 405: return static_radical_inverse<T, 2791>(input);
      case 406: return static_radical_inverse<T, 2797>(input);
      case 407: return static_radical_inverse<T, 2801>(input);
      case 408: return static_radical_inverse<T, 2803>(input);
      case 409: return static_radical_inverse<T, 2819>(input);
      case 410: return static_radical_inverse<T, 2833>(input);
      case 411: return static_radical_inverse<T, 2837>(input);
      case 412: return static_radical_inverse<T, 2843>(input);
      case 413: return static_radical_inverse<T, 2851>(input);
      case 414: return static_radical_inverse<T, 2857>(input);
      case 415: return static_radical_inverse<T, 2861>(input);
      case 416: return static_radical_inverse<T, 2879>(input);
      case 417: return static_radical_inverse<T, 2887>(input);
      case 418: return static_radical_inverse<T, 2897>(input);
      case 419: return static_radical_inverse<T, 2903>(input);
      case 420: return static_radical_inverse<T, 2909>(input);
      case 421: return static_radical_inverse<T, 2917>(input);
      case 422: return static_radical_inverse<T, 2927>(input);
      case 423: return static_radical_inverse<T, 2939>(input);
      case 424: return static_radical_inverse<T, 2953>(input);
      case 425: return static_radical_inverse<T, 2957>(input);
      case 426: return static_radical_inverse<T, 2963>(input);
      case 427: return static_radical_inverse<T, 2969>(input);
      case 428: return static_radical_inverse<T, 2971>(input);
      case 429: return static_radical_inverse<T, 2999>(input);
      case 430: return static_radical_inverse<T, 3001>(input);
      case 431: return static_radical_inverse<T, 3011>(input);
      case 432: return static_radical_inverse<T, 3019>(input);
      case 433: return static_radical_inverse<T, 3023>(input);
      case 434: return static_radical_inverse<T, 3037>(input);
      case 435: return static_radical_inverse<T, 3041>(input);
      case 436: return static_radical_inverse<T, 3049>(input);
      case 437: return static_radical_inverse<T, 3061>(input);
      case 438: return static_radical_inverse<T, 3067>(input);
      case 439: return static_radical_inverse<T, 3079>(input);
      case 440: return static_radical_inverse<T, 3083>(input);
      case 441: return static_radical_inverse<T, 3089>(input);
      case 442: return static_radical_inverse<T, 3109>(input);
      case 443: return static_radical_inverse<T, 3119>(input);
      case 444: return static_radical_inverse<T, 3121>(input);
      case 445: return static_radical_inverse<T, 3137>(input);
      case 446: return static_radical_inverse<T, 3163>(input);
      case 447: return static_radical_inverse<T, 3167>(input);
      case 448: return static_radical_inverse<T, 3169>(input);
      case 449: return static_radical_inverse<T, 3181>(input);
      case 450: return static_radical_inverse<T, 3187>(input);
      case 451: return static_radical_inverse<T, 3191>(input);
      case 452: return static_radical_inverse<T, 3203>(input);
      case 453: return static_radical_inverse<T, 3209>(input);
      case 454: return static_radical_inverse<T, 3217>(input);
      case 455: return static_radical_inverse<T, 3221>(input);
      case 456: return static_radical_inverse<T, 3229>(input);
      case 457: return static_radical_inverse<T, 3251>(input);
      case 458: return static_radical_inverse<T, 3253>(input);
      case 459: return static_radical_inverse<T, 3257>(input);
      case 460: return static_radical_inverse<T, 3259>(input);
      case 461: return static_radical_inverse<T, 3271>(input);
      case 462: return static_radical_inverse<T, 3299>(input);
      case 463: return static_radical_inverse<T, 3301>(input);
      case 464: return static_radical_inverse<T, 3307>(input);
      case 465: return static_radical_inverse<T, 3313>(input);
      case 466: return static_radical_inverse<T, 3319>(input);
      case 467: return static_radical_inverse<T, 3323>(input);
      case 468: return static_radical_inverse<T, 3329>(input);
      case 469: return static_radical_inverse<T, 3331>(input);
      case 470: return static_radical_inverse<T, 3343>(input);
      case 471: return static_radical_inverse<T, 3347>(input);
      case 472: return static_radical_inverse<T, 3359>(input);
      case 473: return static_radical_inverse<T, 3361>(input);
      case 474: return static_radical_inverse<T, 3371>(input);
      case 475: return static_radical_inverse<T, 3373>(input);
      case 476: return static_radical_inverse<T, 3389>(input);
      case 477: return static_radical_inverse<T, 3391>(input);
      case 478: return static_radical_inverse<T, 3407>(input);
      case 479: return static_radical_inverse<T, 3413>(input);
      case 480: return static_radical_inverse<T, 3433>(input);
      case 481: return static_radical_inverse<T, 3449>(input);
      case 482: return static_radical_inverse<T, 3457>(input);
      case 483: return static_radical_inverse<T, 3461>(input);
      case 484: return static_radical_inverse<T, 3463>(input);
      case 485: return static_radical_inverse<T, 3467>(input);
      case 486: return static_radical_inverse<T, 3469>(input);
      case 487: return static_radical_inverse<T, 3491>(input);
      case 488: return static_radical_inverse<T, 3499>(input);
      case 489: return static_radical_inverse<T, 3511>(input);
      case 490: return static_radical_inverse<T, 3517>(input);
      case 491: return static_radical_inverse<T, 3527>(input);
      case 492: return static_radical_inverse<T, 3529>(input);
      case 493: return static_radical_inverse<T, 3533>(input);
      case 494: return static_radical_inverse<T, 3539>(input);
      case 495: return static_radical_inverse<T, 3541>(input);
      case 496: return static_radical_inverse<T, 3547>(input);
      case 497: return static_radical_inverse<T, 3557>(input);
      case 498: return static_radical_inverse<T, 3559>(input);
      case 499: return static_radical_inverse<T, 3571>(input);
      case 500: return static_radical_inverse<T, 3581>(input);
      case 501: return static_radical_inverse<T, 3583>(input);
      case 502: return static_radical_inverse<T, 3593>(input);
      case 503: return static_radical_inverse<T, 3607>(input);
      case 504: return static_radical_inverse<T, 3613>(input);
      case 505: return static_radical_inverse<T, 3617>(input);
      case 506: return static_radical_inverse<T, 3623>(input);
      case 507: return static_radical_inverse<T, 3631>(input);
      case 508: return static_radical_inverse<T, 3637>(input);
      case 509: return static_radical_inverse<T, 3643>(input);
      case 510: return static_radical_inverse<T, 3659>(input);
      case 511: return static_radical_inverse<T, 3671>(input);
      case 512: return static_radical_inverse<T, 3673>(input);
      case 513: return static_radical_inverse<T, 3677>(input);
      case 514: return static_radical_inverse<T, 3691>(input);
      case 515: return static_radical_inverse<T, 3697>(input);
      case 516: return static_radical_inverse<T, 3701>(input);
      case 517: return static_radical_inverse<T, 3709>(input);
      case 518: return static_radical_inverse<T, 3719>(input);
      case 519: return static_radical_inverse<T, 3727>(input);
      case 520: return static_radical_inverse<T, 3733>(input);
      case 521: return static_radical_inverse<T, 3739>(input);
      case 522: return static_radical_inverse<T, 3761>(input);
      case 523: return static_radical_inverse<T, 3767>(input);
      case 524: return static_radical_inverse<T, 3769>(input);
      case 525: return static_radical_inverse<T, 3779>(input);
      case 526: return static_radical_inverse<T, 3793>(input);
      case 527: return static_radical_inverse<T, 3797>(input);
      case 528: return static_radical_inverse<T, 3803>(input);
      case 529: return static_radical_inverse<T, 3821>(input);
      case 530: return static_radical_inverse<T, 3823>(input);
      case 531: return static_radical_inverse<T, 3833>(input);
      case 532: return static_radical_inverse<T, 3847>(input);
      case 533: return static_radical_inverse<T, 3851>(input);
      case 534: return static_radical_inverse<T, 3853>(input);
      case 535: return static_radical_inverse<T, 3863>(input);
      case 536: return static_radical_inverse<T, 3877>(input);
      case 537: return static_radical_inverse<T, 3881>(input);
      case 538: return static_radical_inverse<T, 3889>(input);
      case 539: return static_radical_inverse<T, 3907>(input);
      case 540: return static_radical_inverse<T, 3911>(input);
      case 541: return static_radical_inverse<T, 3917>(input);
      case 542: return static_radical_inverse<T, 3919>(input);
      case 543: return static_radical_inverse<T, 3923>(input);
      case 544: return static_radical_inverse<T, 3929>(input);
      case 545: return static_radical_inverse<T, 3931>(input);
      case 546: return static_radical_inverse<T, 3943>(input);
      case 547: return static_radical_inverse<T, 3947>(input);
      case 548: return static_radical_inverse<T, 3967>(input);
      case 549: return static_radical_inverse<T, 3989>(input);
      case 550: return static_radical_inverse<T, 4001>(input);
      case 551: return static_radical_inverse<T, 4003>(input);
      case 552: return static_radical_inverse<T, 4007>(input);
      case 553: return static_radical_inverse<T, 4013>(input);
      case 554: return static_radical_inverse<T, 4019>(input);
      case 555: return static_radical_inverse<T, 4021>(input);
      case 556: return static_radical_inverse<T, 4027>(input);
      case 557: return static_radical_inverse<T, 4049>(input);
      case 558: return static_radical_inverse<T, 4051>(input);
      case 559: return static_radical_inverse<T, 4057>(input);
      case 560: return static_radical_inverse<T, 4073>(input);
      case 561: return static_radical_inverse<T, 4079>(input);
      case 562: return static_radical_inverse<T, 4091>(input);
      case 563: return static_radical_inverse<T, 4093>(input);
      case 564: return static_radical_inverse<T, 4099>(input);
      case 565: return static_radical_inverse<T, 4111>(input);
      case 566: return static_radical_inverse<T, 4127>(input);
      case 567: return static_radical_inverse<T, 4129>(input);
      case 568: return static_radical_inverse<T, 4133>(input);
      case 569: return static_radical_inverse<T, 4139>(input);
      case 570: return static_radical_inverse<T, 4153>(input);
      case 571: return static_radical_inverse<T, 4157>(input);
      case 572: return static_radical_inverse<T, 4159>(input);
      case 573: return static_radical_inverse<T, 4177>(input);
      case 574: return static_radical_inverse<T, 4201>(input);
      case 575: return static_radical_inverse<T, 4211>(input);
      case 576: return static_radical_inverse<T, 4217>(input);
      case 577: return static_radical_inverse<T, 4219>(input);
      case 578: return static_radical_inverse<T, 4229>(input);
      case 579: return static_radical_inverse<T, 4231>(input);
      case 580: return static_radical_inverse<T, 4241>(input);
      case 581: return static_radical_inverse<T, 4243>(input);
      case 582: return static_radical_inverse<T, 4253>(input);
      case 583: return static_radical_inverse<T, 4259>(input);
      case 584: return static_radical_inverse<T, 4261>(input);
      case 585: return static_radical_inverse<T, 4271>(input);
      case 586: return static_radical_inverse<T, 4273>(input);
      case 587: return static_radical_inverse<T, 4283>(input);
      case 588: return static_radical_inverse<T, 4289>(input);
      case 589: return static_radical_inverse<T, 4297>(input);
      case 590: return static_radical_inverse<T, 4327>(input);
      case 591: return static_radical_inverse<T, 4337>(input);
      case 592: return static_radical_inverse<T, 4339>(input);
      case 593: return static_radical_inverse<T, 4349>(input);
      case 594: return static_radical_inverse<T, 4357>(input);
      case 595: return static_radical_inverse<T, 4363>(input);
      case 596: return static_radical_inverse<T, 4373>(input);
      case 597: return static_radical_inverse<T, 4391>(input);
      case 598: return static_radical_inverse<T, 4397>(input);
      case 599: return static_radical_inverse<T, 4409>(input);
      case 600: return static_radical_inverse<T, 4421>(input);
      case 601: return static_radical_inverse<T, 4423>(input);
      case 602: return static_radical_inverse<T, 4441>(input);
      case 603: return static_radical_inverse<T, 4447>(input);
      case 604: return static_radical_inverse<T, 4451>(input);
      case 605: return static_radical_inverse<T, 4457>(input);
      case 606: return static_radical_inverse<T, 4463>(input);
      case 607: return static_radical_inverse<T, 4481>(input);
      case 608: return static_radical_inverse<T, 4483>(input);
      case 609: return static_radical_inverse<T, 4493>(input);
      case 610: return static_radical_inverse<T, 4507>(input);
      case 611: return static_radical_inverse<T, 4513>(input);
      case 612: return static_radical_inverse<T, 4517>(input);
      case 613: return static_radical_inverse<T, 4519>(input);
      case 614: return static_radical_inverse<T, 4523>(input);
      case 615: return static_radical_inverse<T, 4547>(input);
      case 616: return static_radical_inverse<T, 4549>(input);
      case 617: return static_radical_inverse<T, 4561>(input);
      case 618: return static_radical_inverse<T, 4567>(input);
      case 619: return static_radical_inverse<T, 4583>(input);
      case 620: return static_radical_inverse<T, 4591>(input);
      case 621: return static_radical_inverse<T, 4597>(input);
      case 622: return static_radical_inverse<T, 4603>(input);
      case 623: return static_radical_inverse<T, 4621>(input);
      case 624: return static_radical_inverse<T, 4637>(input);
      case 625: return static_radical_inverse<T, 4639>(input);
      case 626: return static_radical_inverse<T, 4643>(input);
      case 627: return static_radical_inverse<T, 4649>(input);
      case 628: return static_radical_inverse<T, 4651>(input);
      case 629: return static_radical_inverse<T, 4657>(input);
      case 630: return static_radical_inverse<T, 4663>(input);
      case 631: return static_radical_inverse<T, 4673>(input);
      case 632: return static_radical_inverse<T, 4679>(input);
      case 633: return static_radical_inverse<T, 4691>(input);
      case 634: return static_radical_inverse<T, 4703>(input);
      case 635: return static_radical_inverse<T, 4721>(input);
      case 636: return static_radical_inverse<T, 4723>(input);
      case 637: return static_radical_inverse<T, 4729>(input);
      case 638: return static_radical_inverse<T, 4733>(input);
      case 639: return static_radical_inverse<T, 4751>(input);
      case 640: return static_radical_inverse<T, 4759>(input);
      case 641: return static_radical_inverse<T, 4783>(input);
      case 642: return static_radical_inverse<T, 4787>(input);
      case 643: return static_radical_inverse<T, 4789>(input);
      case 644: return static_radical_inverse<T, 4793>(input);
      case 645: return static_radical_inverse<T, 4799>(input);
      case 646: return static_radical_inverse<T, 4801>(input);
      case 647: return static_radical_inverse<T, 4813>(input);
      case 648: return static_radical_inverse<T, 4817>(input);
      case 649: return static_radical_inverse<T, 4831>(input);
      case 650: return static_radical_inverse<T, 4861>(input);
      case 651: return static_radical_inverse<T, 4871>(input);
      case 652: return static_radical_inverse<T, 4877>(input);
      case 653: return static_radical_inverse<T, 4889>(input);
      case 654: return static_radical_inverse<T, 4903>(input);
      case 655: return static_radical_inverse<T, 4909>(input);
      case 656: return static_radical_inverse<T, 4919>(input);
      case 657: return static_radical_inverse<T, 4931>(input);
      case 658: return static_radical_inverse<T, 4933>(input);
      case 659: return static_radical_inverse<T, 4937>(input);
      case 660: return static_radical_inverse<T, 4943>(input);
      case 661: return static_radical_inverse<T, 4951>(input);
      case 662: return static_radical_inverse<T, 4957>(input);
      case 663: return static_radical_inverse<T, 4967>(input);
      case 664: return static_radical_inverse<T, 4969>(input);
      case 665: return static_radical_inverse<T, 4973>(input);
      case 666: return static_radical_inverse<T, 4987>(input);
      case 667: return static_radical_inverse<T, 4993>(input);
      case 668: return static_radical_inverse<T, 4999>(input);
      case 669: return static_radical_inverse<T, 5003>(input);
      case 670: return static_radical_inverse<T, 5009>(input);
      case 671: return static_radical_inverse<T, 5011>(input);
      case 672: return static_radical_inverse<T, 5021>(input);
      case 673: return static_radical_inverse<T, 5023>(input);
      case 674: return static_radical_inverse<T, 5039>(input);
      case 675: return static_radical_inverse<T, 5051>(input);
      case 676: return static_radical_inverse<T, 5059>(input);
      case 677: return static_radical_inverse<T, 5077>(input);
      case 678: return static_radical_inverse<T, 5081>(input);
      case 679: return static_radical_inverse<T, 5087>(input);
      case 680: return static_radical_inverse<T, 5099>(input);
      case 681: return static_radical_inverse<T, 5101>(input);
      case 682: return static_radical_inverse<T, 5107>(input);
      case 683: return static_radical_inverse<T, 5113>(input);
      case 684: return static_radical_inverse<T, 5119>(input);
      case 685: return static_radical_inverse<T, 5147>(input);
      case 686: return static_radical_inverse<T, 5153>(input);
      case 687: return static_radical_inverse<T, 5167>(input);
      case 688: return static_radical_inverse<T, 5171>(input);
      case 689: return static_radical_inverse<T, 5179>(input);
      case 690: return static_radical_inverse<T, 5189>(input);
      case 691: return static_radical_inverse<T, 5197>(input);
      case 692: return static_radical_inverse<T, 5209>(input);
      case 693: return static_radical_inverse<T, 5227>(input);
      case 694: return static_radical_inverse<T, 5231>(input);
      case 695: return static_radical_inverse<T, 5233>(input);
      case 696: return static_radical_inverse<T, 5237>(input);
      case 697: return static_radical_inverse<T, 5261>(input);
      case 698: return static_radical_inverse<T, 5273>(input);
      case 699: return static_radical_inverse<T, 5279>(input);
      case 700: return static_radical_inverse<T, 5281>(input);
      case 701: return static_radical_inverse<T, 5297>(input);
      case 702: return static_radical_inverse<T, 5303>(input);
      case 703: return static_radical_inverse<T, 5309>(input);
      case 704: return static_radical_inverse<T, 5323>(input);
      case 705: return static_radical_inverse<T, 5333>(input);
      case 706: return static_radical_inverse<T, 5347>(input);
      case 707: return static_radical_inverse<T, 5351>(input);
      case 708: return static_radical_inverse<T, 5381>(input);
      case 709: return static_radical_inverse<T, 5387>(input);
      case 710: return static_radical_inverse<T, 5393>(input);
      case 711: return static_radical_inverse<T, 5399>(input);
      case 712: return static_radical_inverse<T, 5407>(input);
      case 713: return static_radical_inverse<T, 5413>(input);
      case 714: return static_radical_inverse<T, 5417>(input);
      case 715: return static_radical_inverse<T, 5419>(input);
      case 716: return static_radical_inverse<T, 5431>(input);
      case 717: return static_radical_inverse<T, 5437>(input);
      case 718: return static_radical_inverse<T, 5441>(input);
      case 719: return static_radical_inverse<T, 5443>(input);
      case 720: return static_radical_inverse<T, 5449>(input);
      case 721: return static_radical_inverse<T, 5471>(input);
      case 722: return static_radical_inverse<T, 5477>(input);
      case 723: return static_radical_inverse<T, 5479>(input);
      case 724: return static_radical_inverse<T, 5483>(input);
      case 725: return static_radical_inverse<T, 5501>(input);
      case 726: return static_radical_inverse<T, 5503>(input);
      case 727: return static_radical_inverse<T, 5507>(input);
      case 728: return static_radical_inverse<T, 5519>(input);
      case 729: return static_radical_inverse<T, 5521>(input);
      case 730: return static_radical_inverse<T, 5527>(input);
      case 731: return static_radical_inverse<T, 5531>(input);
      case 732: return static_radical_inverse<T, 5557>(input);
      case 733: return static_radical_inverse<T, 5563>(input);
      case 734: return static_radical_inverse<T, 5569>(input);
      case 735: return static_radical_inverse<T, 5573>(input);
      case 736: return static_radical_inverse<T, 5581>(input);
      case 737: return static_radical_inverse<T, 5591>(input);
      case 738: return static_radical_inverse<T, 5623>(input);
      case 739: return static_radical_inverse<T, 5639>(input);
      case 740: return static_radical_inverse<T, 5641>(input);
      case 741: return static_radical_inverse<T, 5647>(input);
      case 742: return static_radical_inverse<T, 5651>(input);
      case 743: return static_radical_inverse<T, 5653>(input);
      case 744: return static_radical_inverse<T, 5657>(input);
      case 745: return static_radical_inverse<T, 5659>(input);
      case 746: return static_radical_inverse<T, 5669>(input);
      case 747: return static_radical_inverse<T, 5683>(input);
      case 748: return static_radical_inverse<T, 5689>(input);
      case 749: return static_radical_inverse<T, 5693>(input);
      case 750: return static_radical_inverse<T, 5701>(input);
      case 751: return static_radical_inverse<T, 5711>(input);
      case 752: return static_radical_inverse<T, 5717>(input);
      case 753: return static_radical_inverse<T, 5737>(input);
      case 754: return static_radical_inverse<T, 5741>(input);
      case 755: return static_radical_inverse<T, 5743>(input);
      case 756: return static_radical_inverse<T, 5749>(input);
      case 757: return static_radical_inverse<T, 5779>(input);
      case 758: return static_radical_inverse<T, 5783>(input);
      case 759: return static_radical_inverse<T, 5791>(input);
      case 760: return static_radical_inverse<T, 5801>(input);
      case 761: return static_radical_inverse<T, 5807>(input);
      case 762: return static_radical_inverse<T, 5813>(input);
      case 763: return static_radical_inverse<T, 5821>(input);
      case 764: return static_radical_inverse<T, 5827>(input);
      case 765: return static_radical_inverse<T, 5839>(input);
      case 766: return static_radical_inverse<T, 5843>(input);
      case 767: return static_radical_inverse<T, 5849>(input);
      case 768: return static_radical_inverse<T, 5851>(input);
      case 769: return static_radical_inverse<T, 5857>(input);
      case 770: return static_radical_inverse<T, 5861>(input);
      case 771: return static_radical_inverse<T, 5867>(input);
      case 772: return static_radical_inverse<T, 5869>(input);
      case 773: return static_radical_inverse<T, 5879>(input);
      case 774: return static_radical_inverse<T, 5881>(input);
      case 775: return static_radical_inverse<T, 5897>(input);
      case 776: return static_radical_inverse<T, 5903>(input);
      case 777: return static_radical_inverse<T, 5923>(input);
      case 778: return static_radical_inverse<T, 5927>(input);
      case 779: return static_radical_inverse<T, 5939>(input);
      case 780: return static_radical_inverse<T, 5953>(input);
      case 781: return static_radical_inverse<T, 5981>(input);
      case 782: return static_radical_inverse<T, 5987>(input);
      case 783: return static_radical_inverse<T, 6007>(input);
      case 784: return static_radical_inverse<T, 6011>(input);
      case 785: return static_radical_inverse<T, 6029>(input);
      case 786: return static_radical_inverse<T, 6037>(input);
      case 787: return static_radical_inverse<T, 6043>(input);
      case 788: return static_radical_inverse<T, 6047>(input);
      case 789: return static_radical_inverse<T, 6053>(input);
      case 790: return static_radical_inverse<T, 6067>(input);
      case 791: return static_radical_inverse<T, 6073>(input);
      case 792: return static_radical_inverse<T, 6079>(input);
      case 793: return static_radical_inverse<T, 6089>(input);
      case 794: return static_radical_inverse<T, 6091>(input);
      case 795: return static_radical_inverse<T, 6101>(input);
      case 796: return static_radical_inverse<T, 6113>(input);
      case 797: return static_radical_inverse<T, 6121>(input);
      case 798: return static_radical_inverse<T, 6131>(input);
      case 799: return static_radical_inverse<T, 6133>(input);
      case 800: return static_radical_inverse<T, 6143>(input);
      case 801: return static_radical_inverse<T, 6151>(input);
      case 802: return static_radical_inverse<T, 6163>(input);
      case 803: return static_radical_inverse<T, 6173>(input);
      case 804: return static_radical_inverse<T, 6197>(input);
      case 805: return static_radical_inverse<T, 6199>(input);
      case 806: return static_radical_inverse<T, 6203>(input);
      case 807: return static_radical_inverse<T, 6211>(input);
      case 808: return static_radical_inverse<T, 6217>(input);
      case 809: return static_radical_inverse<T, 6221>(input);
      case 810: return static_radical_inverse<T, 6229>(input);
      case 811: return static_radical_inverse<T, 6247>(input);
      case 812: return static_radical_inverse<T, 6257>(input);
      case 813: return static_radical_inverse<T, 6263>(input);
      case 814: return static_radical_inverse<T, 6269>(input);
      case 815: return static_radical_inverse<T, 6271>(input);
      case 816: return static_radical_inverse<T, 6277>(input);
      case 817: return static_radical_inverse<T, 6287>(input);
      case 818: return static_radical_inverse<T, 6299>(input);
      case 819: return static_radical_inverse<T, 6301>(input);
      case 820: return static_radical_inverse<T, 6311>(input);
      case 821: return static_radical_inverse<T, 6317>(input);
      case 822: return static_radical_inverse<T, 6323>(input);
      case 823: return static_radical_inverse<T, 6329>(input);
      case 824: return static_radical_inverse<T, 6337>(input);
      case 825: return static_radical_inverse<T, 6343>(input);
      case 826: return static_radical_inverse<T, 6353>(input);
      case 827: return static_radical_inverse<T, 6359>(input);
      case 828: return static_radical_inverse<T, 6361>(input);
      case 829: return static_radical_inverse<T, 6367>(input);
      case 830: return static_radical_inverse<T, 6373>(input);
      case 831: return static_radical_inverse<T, 6379>(input);
      case 832: return static_radical_inverse<T, 6389>(input);
      case 833: return static_radical_inverse<T, 6397>(input);
      case 834: return static_radical_inverse<T, 6421>(input);
      case 835: return static_radical_inverse<T, 6427>(input);
      case 836: return static_radical_inverse<T, 6449>(input);
      case 837: return static_radical_inverse<T, 6451>(input);
      case 838: return static_radical_inverse<T, 6469>(input);
      case 839: return static_radical_inverse<T, 6473>(input);
      case 840: return static_radical_inverse<T, 6481>(input);
      case 841: return static_radical_inverse<T, 6491>(input);
      case 842: return static_radical_inverse<T, 6521>(input);
      case 843: return static_radical_inverse<T, 6529>(input);
      case 844: return static_radical_inverse<T, 6547>(input);
      case 845: return static_radical_inverse<T, 6551>(input);
      case 846: return static_radical_inverse<T, 6553>(input);
      case 847: return static_radical_inverse<T, 6563>(input);
      case 848: return static_radical_inverse<T, 6569>(input);
      case 849: return static_radical_inverse<T, 6571>(input);
      case 850: return static_radical_inverse<T, 6577>(input);
      case 851: return static_radical_inverse<T, 6581>(input);
      case 852: return static_radical_inverse<T, 6599>(input);
      case 853: return static_radical_inverse<T, 6607>(input);
      case 854: return static_radical_inverse<T, 6619>(input);
      case 855: return static_radical_inverse<T, 6637>(input);
      case 856: return static_radical_inverse<T, 6653>(input);
      case 857: return static_radical_inverse<T, 6659>(input);
      case 858: return static_radical_inverse<T, 6661>(input);
      case 859: return static_radical_inverse<T, 6673>(input);
      case 860: return static_radical_inverse<T, 6679>(input);
      case 861: return static_radical_inverse<T, 6689>(input);
      case 862: return static_radical_inverse<T, 6691>(input);
      case 863: return static_radical_inverse<T, 6701>(input);
      case 864: return static_radical_inverse<T, 6703>(input);
      case 865: return static_radical_inverse<T, 6709>(input);
      case 866: return static_radical_inverse<T, 6719>(input);
      case 867: return static_radical_inverse<T, 6733>(input);
      case 868: return static_radical_inverse<T, 6737>(input);
      case 869: return static_radical_inverse<T, 6761>(input);
      case 870: return static_radical_inverse<T, 6763>(input);
      case 871: return static_radical_inverse<T, 6779>(input);
      case 872: return static_radical_inverse<T, 6781>(input);
      case 873: return static_radical_inverse<T, 6791>(input);
      case 874: return static_radical_inverse<T, 6793>(input);
      case 875: return static_radical_inverse<T, 6803>(input);
      case 876: return static_radical_inverse<T, 6823>(input);
      case 877: return static_radical_inverse<T, 6827>(input);
      case 878: return static_radical_inverse<T, 6829>(input);
      case 879: return static_radical_inverse<T, 6833>(input);
      case 880: return static_radical_inverse<T, 6841>(input);
      case 881: return static_radical_inverse<T, 6857>(input);
      case 882: return static_radical_inverse<T, 6863>(input);
      case 883: return static_radical_inverse<T, 6869>(input);
      case 884: return static_radical_inverse<T, 6871>(input);
      case 885: return static_radical_inverse<T, 6883>(input);
      case 886: return static_radical_inverse<T, 6899>(input);
      case 887: return static_radical_inverse<T, 6907>(input);
      case 888: return static_radical_inverse<T, 6911>(input);
      case 889: return static_radical_inverse<T, 6917>(input);
      case 890: return static_radical_inverse<T, 6947>(input);
      case 891: return static_radical_inverse<T, 6949>(input);
      case 892: return static_radical_inverse<T, 6959>(input);
      case 893: return static_radical_inverse<T, 6961>(input);
      case 894: return static_radical_inverse<T, 6967>(input);
      case 895: return static_radical_inverse<T, 6971>(input);
      case 896: return static_radical_inverse<T, 6977>(input);
      case 897: return static_radical_inverse<T, 6983>(input);
      case 898: return static_radical_inverse<T, 6991>(input);
      case 899: return static_radical_inverse<T, 6997>(input);
      case 900: return static_radical_inverse<T, 7001>(input);
      case 901: return static_radical_inverse<T, 7013>(input);
      case 902: return static_radical_inverse<T, 7019>(input);
      case 903: return static_radical_inverse<T, 7027>(input);
      case 904: return static_radical_inverse<T, 7039>(input);
      case 905: return static_radical_inverse<T, 7043>(input);
      case 906: return static_radical_inverse<T, 7057>(input);
      case 907: return static_radical_inverse<T, 7069>(input);
      case 908: return static_radical_inverse<T, 7079>(input);
      case 909: return static_radical_inverse<T, 7103>(input);
      case 910: return static_radical_inverse<T, 7109>(input);
      case 911: return static_radical_inverse<T, 7121>(input);
      case 912: return static_radical_inverse<T, 7127>(input);
      case 913: return static_radical_inverse<T, 7129>(input);
      case 914: return static_radical_inverse<T, 7151>(input);
      case 915: return static_radical_inverse<T, 7159>(input);
      case 916: return static_radical_inverse<T, 7177>(input);
      case 917: return static_radical_inverse<T, 7187>(input);
      case 918: return static_radical_inverse<T, 7193>(input);
      case 919: return static_radical_inverse<T, 7207>(input);
      case 920: return static_radical_inverse<T, 7211>(input);
      case 921: return static_radical_inverse<T, 7213>(input);
      case 922: return static_radical_inverse<T, 7219>(input);
      case 923: return static_radical_inverse<T, 7229>(input);
      case 924: return static_radical_inverse<T, 7237>(input);
      case 925: return static_radical_inverse<T, 7243>(input);
      case 926: return static_radical_inverse<T, 7247>(input);
      case 927: return static_radical_inverse<T, 7253>(input);
      case 928: return static_radical_inverse<T, 7283>(input);
      case 929: return static_radical_inverse<T, 7297>(input);
      case 930: return static_radical_inverse<T, 7307>(input);
      case 931: return static_radical_inverse<T, 7309>(input);
      case 932: return static_radical_inverse<T, 7321>(input);
      case 933: return static_radical_inverse<T, 7331>(input);
      case 934: return static_radical_inverse<T, 7333>(input);
      case 935: return static_radical_inverse<T, 7349>(input);
      case 936: return static_radical_inverse<T, 7351>(input);
      case 937: return static_radical_inverse<T, 7369>(input);
      case 938: return static_radical_inverse<T, 7393>(input);
      case 939: return static_radical_inverse<T, 7411>(input);
      case 940: return static_radical_inverse<T, 7417>(input);
      case 941: return static_radical_inverse<T, 7433>(input);
      case 942: return static_radical_inverse<T, 7451>(input);
      case 943: return static_radical_inverse<T, 7457>(input);
      case 944: return static_radical_inverse<T, 7459>(input);
      case 945: return static_radical_inverse<T, 7477>(input);
      case 946: return static_radical_inverse<T, 7481>(input);
      case 947: return static_radical_inverse<T, 7487>(input);
      case 948: return static_radical_inverse<T, 7489>(input);
      case 949: return static_radical_inverse<T, 7499>(input);
      case 950: return static_radical_inverse<T, 7507>(input);
      case 951: return static_radical_inverse<T, 7517>(input);
      case 952: return static_radical_inverse<T, 7523>(input);
      case 953: return static_radical_inverse<T, 7529>(input);
      case 954: return static_radical_inverse<T, 7537>(input);
      case 955: return static_radical_inverse<T, 7541>(input);
      case 956: return static_radical_inverse<T, 7547>(input);
      case 957: return static_radical_inverse<T, 7549>(input);
      case 958: return static_radical_inverse<T, 7559>(input);
      case 959: return static_radical_inverse<T, 7561>(input);
      case 960: return static_radical_inverse<T, 7573>(input);
      case 961: return static_radical_inverse<T, 7577>(input);
      case 962: return static_radical_inverse<T, 7583>(input);
      case 963: return static_radical_inverse<T, 7589>(input);
      case 964: return static_radical_inverse<T, 7591>(input);
      case 965: return static_radical_inverse<T, 7603>(input);
      case 966: return static_radical_inverse<T, 7607>(input);
      case 967: return static_radical_inverse<T, 7621>(input);
      case 968: return static_radical_inverse<T, 7639>(input);
      case 969: return static_radical_inverse<T, 7643>(input);
      case 970: return static_radical_inverse<T, 7649>(input);
      case 971: return static_radical_inverse<T, 7669>(input);
      case 972: return static_radical_inverse<T, 7673>(input);
      case 973: return static_radical_inverse<T, 7681>(input);
      case 974: return static_radical_inverse<T, 7687>(input);
      case 975: return static_radical_inverse<T, 7691>(input);
      case 976: return static_radical_inverse<T, 7699>(input);
      case 977: return static_radical_inverse<T, 7703>(input);
      case 978: return static_radical_inverse<T, 7717>(input);
      case 979: return static_radical_inverse<T, 7723>(input);
      case 980: return static_radical_inverse<T, 7727>(input);
      case 981: return static_radical_inverse<T, 7741>(input);
      case 982: return static_radical_inverse<T, 7753>(input);
      case 983: return static_radical_inverse<T, 7757>(input);
      case 984: return static_radical_inverse<T, 7759>(input);
      case 985: return static_radical_inverse<T, 7789>(input);
      case 986: return static_radical_inverse<T, 7793>(input);
      case 987: return static_radical_inverse<T, 7817>(input);
      case 988: return static_radical_inverse<T, 7823>(input);
      case 989: return static_radical_inverse<T, 7829>(input);
      case 990: return static_radical_inverse<T, 7841>(input);
      case 991: return static_radical_inverse<T, 7853>(input);
      case 992: return static_radical_inverse<T, 7867>(input);
      case 993: return static_radical_inverse<T, 7873>(input);
      case 994: return static_radical_inverse<T, 7877>(input);
      case 995: return static_radical_inverse<T, 7879>(input);
      case 996: return static_radical_inverse<T, 7883>(input);
      case 997: return static_radical_inverse<T, 7901>(input);
      case 998: return static_radical_inverse<T, 7907>(input);
      case 999: return static_radical_inverse<T, 7919>(input);
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
    const size_t        input)
{
    assert(base >= 2);

    const isize_t signed_base = static_cast<isize_t>(base);
    const T rcp_base = T(1.0) / signed_base;

    isize_t i = static_cast<isize_t>(input);
    isize_t offset = 0;
    T x = T(0.0);
    T b = rcp_base;

    while (x + (signed_base - 1) * b > x)
    {
        x += ((i + offset) % signed_base) * b;
        b *= rcp_base;
        i /= signed_base;
        ++offset;
    }

    return x;
}

template <typename T>
inline T permuted_radical_inverse(
    const size_t        base,
    const size_t        perm[],
    const size_t        input)
{
    assert(base >= 2);

    const isize_t signed_base = static_cast<isize_t>(base);
    const T rcp_base = T(1.0) / signed_base;

    isize_t i = static_cast<isize_t>(input);
    isize_t x = 0;
    T b = T(1.0);

    while (i)
    {
        x *= signed_base;
        x += static_cast<isize_t>(perm[i % signed_base]);
        i /= signed_base;
        b *= rcp_base;
    }

    return static_cast<T>(x) * b;
}

template <typename T, isize_t Base>
inline T static_permuted_radical_inverse(
    const size_t        perm[],
    const size_t        input)
{
    BOOST_STATIC_ASSERT(Base >= 2);

    const T rcp_base = T(1.0) / Base;

    isize_t i = static_cast<isize_t>(input);
    isize_t x = 0;
    T b = T(1.0);

    while (i)
    {
        x *= Base;
        x += static_cast<isize_t>(perm[i % Base]);
        i /= Base;
        b *= rcp_base;
    }

    return static_cast<T>(x) * b;
}

template <typename T>
T fast_permuted_radical_inverse(
    const size_t        base_index,
    const size_t        perm[],
    const size_t        input)
{
    switch (base_index)
    {
      case   0: return radical_inverse_base2<T>(input);
      case   1: return static_permuted_radical_inverse<T, 3>(perm, input);
      case   2: return static_permuted_radical_inverse<T, 5>(perm, input);
      case   3: return static_permuted_radical_inverse<T, 7>(perm, input);
      case   4: return static_permuted_radical_inverse<T, 11>(perm, input);
      case   5: return static_permuted_radical_inverse<T, 13>(perm, input);
      case   6: return static_permuted_radical_inverse<T, 17>(perm, input);
      case   7: return static_permuted_radical_inverse<T, 19>(perm, input);
      case   8: return static_permuted_radical_inverse<T, 23>(perm, input);
      case   9: return static_permuted_radical_inverse<T, 29>(perm, input);
      case  10: return static_permuted_radical_inverse<T, 31>(perm, input);
      case  11: return static_permuted_radical_inverse<T, 37>(perm, input);
      case  12: return static_permuted_radical_inverse<T, 41>(perm, input);
      case  13: return static_permuted_radical_inverse<T, 43>(perm, input);
      case  14: return static_permuted_radical_inverse<T, 47>(perm, input);
      case  15: return static_permuted_radical_inverse<T, 53>(perm, input);
      case  16: return static_permuted_radical_inverse<T, 59>(perm, input);
      case  17: return static_permuted_radical_inverse<T, 61>(perm, input);
      case  18: return static_permuted_radical_inverse<T, 67>(perm, input);
      case  19: return static_permuted_radical_inverse<T, 71>(perm, input);
      case  20: return static_permuted_radical_inverse<T, 73>(perm, input);
      case  21: return static_permuted_radical_inverse<T, 79>(perm, input);
      case  22: return static_permuted_radical_inverse<T, 83>(perm, input);
      case  23: return static_permuted_radical_inverse<T, 89>(perm, input);
      case  24: return static_permuted_radical_inverse<T, 97>(perm, input);
      case  25: return static_permuted_radical_inverse<T, 101>(perm, input);
      case  26: return static_permuted_radical_inverse<T, 103>(perm, input);
      case  27: return static_permuted_radical_inverse<T, 107>(perm, input);
      case  28: return static_permuted_radical_inverse<T, 109>(perm, input);
      case  29: return static_permuted_radical_inverse<T, 113>(perm, input);
      case  30: return static_permuted_radical_inverse<T, 127>(perm, input);
      case  31: return static_permuted_radical_inverse<T, 131>(perm, input);
      case  32: return static_permuted_radical_inverse<T, 137>(perm, input);
      case  33: return static_permuted_radical_inverse<T, 139>(perm, input);
      case  34: return static_permuted_radical_inverse<T, 149>(perm, input);
      case  35: return static_permuted_radical_inverse<T, 151>(perm, input);
      case  36: return static_permuted_radical_inverse<T, 157>(perm, input);
      case  37: return static_permuted_radical_inverse<T, 163>(perm, input);
      case  38: return static_permuted_radical_inverse<T, 167>(perm, input);
      case  39: return static_permuted_radical_inverse<T, 173>(perm, input);
      case  40: return static_permuted_radical_inverse<T, 179>(perm, input);
      case  41: return static_permuted_radical_inverse<T, 181>(perm, input);
      case  42: return static_permuted_radical_inverse<T, 191>(perm, input);
      case  43: return static_permuted_radical_inverse<T, 193>(perm, input);
      case  44: return static_permuted_radical_inverse<T, 197>(perm, input);
      case  45: return static_permuted_radical_inverse<T, 199>(perm, input);
      case  46: return static_permuted_radical_inverse<T, 211>(perm, input);
      case  47: return static_permuted_radical_inverse<T, 223>(perm, input);
      case  48: return static_permuted_radical_inverse<T, 227>(perm, input);
      case  49: return static_permuted_radical_inverse<T, 229>(perm, input);
      case  50: return static_permuted_radical_inverse<T, 233>(perm, input);
      case  51: return static_permuted_radical_inverse<T, 239>(perm, input);
      case  52: return static_permuted_radical_inverse<T, 241>(perm, input);
      case  53: return static_permuted_radical_inverse<T, 251>(perm, input);
      case  54: return static_permuted_radical_inverse<T, 257>(perm, input);
      case  55: return static_permuted_radical_inverse<T, 263>(perm, input);
      case  56: return static_permuted_radical_inverse<T, 269>(perm, input);
      case  57: return static_permuted_radical_inverse<T, 271>(perm, input);
      case  58: return static_permuted_radical_inverse<T, 277>(perm, input);
      case  59: return static_permuted_radical_inverse<T, 281>(perm, input);
      case  60: return static_permuted_radical_inverse<T, 283>(perm, input);
      case  61: return static_permuted_radical_inverse<T, 293>(perm, input);
      case  62: return static_permuted_radical_inverse<T, 307>(perm, input);
      case  63: return static_permuted_radical_inverse<T, 311>(perm, input);
      case  64: return static_permuted_radical_inverse<T, 313>(perm, input);
      case  65: return static_permuted_radical_inverse<T, 317>(perm, input);
      case  66: return static_permuted_radical_inverse<T, 331>(perm, input);
      case  67: return static_permuted_radical_inverse<T, 337>(perm, input);
      case  68: return static_permuted_radical_inverse<T, 347>(perm, input);
      case  69: return static_permuted_radical_inverse<T, 349>(perm, input);
      case  70: return static_permuted_radical_inverse<T, 353>(perm, input);
      case  71: return static_permuted_radical_inverse<T, 359>(perm, input);
      case  72: return static_permuted_radical_inverse<T, 367>(perm, input);
      case  73: return static_permuted_radical_inverse<T, 373>(perm, input);
      case  74: return static_permuted_radical_inverse<T, 379>(perm, input);
      case  75: return static_permuted_radical_inverse<T, 383>(perm, input);
      case  76: return static_permuted_radical_inverse<T, 389>(perm, input);
      case  77: return static_permuted_radical_inverse<T, 397>(perm, input);
      case  78: return static_permuted_radical_inverse<T, 401>(perm, input);
      case  79: return static_permuted_radical_inverse<T, 409>(perm, input);
      case  80: return static_permuted_radical_inverse<T, 419>(perm, input);
      case  81: return static_permuted_radical_inverse<T, 421>(perm, input);
      case  82: return static_permuted_radical_inverse<T, 431>(perm, input);
      case  83: return static_permuted_radical_inverse<T, 433>(perm, input);
      case  84: return static_permuted_radical_inverse<T, 439>(perm, input);
      case  85: return static_permuted_radical_inverse<T, 443>(perm, input);
      case  86: return static_permuted_radical_inverse<T, 449>(perm, input);
      case  87: return static_permuted_radical_inverse<T, 457>(perm, input);
      case  88: return static_permuted_radical_inverse<T, 461>(perm, input);
      case  89: return static_permuted_radical_inverse<T, 463>(perm, input);
      case  90: return static_permuted_radical_inverse<T, 467>(perm, input);
      case  91: return static_permuted_radical_inverse<T, 479>(perm, input);
      case  92: return static_permuted_radical_inverse<T, 487>(perm, input);
      case  93: return static_permuted_radical_inverse<T, 491>(perm, input);
      case  94: return static_permuted_radical_inverse<T, 499>(perm, input);
      case  95: return static_permuted_radical_inverse<T, 503>(perm, input);
      case  96: return static_permuted_radical_inverse<T, 509>(perm, input);
      case  97: return static_permuted_radical_inverse<T, 521>(perm, input);
      case  98: return static_permuted_radical_inverse<T, 523>(perm, input);
      case  99: return static_permuted_radical_inverse<T, 541>(perm, input);
      case 100: return static_permuted_radical_inverse<T, 547>(perm, input);
      case 101: return static_permuted_radical_inverse<T, 557>(perm, input);
      case 102: return static_permuted_radical_inverse<T, 563>(perm, input);
      case 103: return static_permuted_radical_inverse<T, 569>(perm, input);
      case 104: return static_permuted_radical_inverse<T, 571>(perm, input);
      case 105: return static_permuted_radical_inverse<T, 577>(perm, input);
      case 106: return static_permuted_radical_inverse<T, 587>(perm, input);
      case 107: return static_permuted_radical_inverse<T, 593>(perm, input);
      case 108: return static_permuted_radical_inverse<T, 599>(perm, input);
      case 109: return static_permuted_radical_inverse<T, 601>(perm, input);
      case 110: return static_permuted_radical_inverse<T, 607>(perm, input);
      case 111: return static_permuted_radical_inverse<T, 613>(perm, input);
      case 112: return static_permuted_radical_inverse<T, 617>(perm, input);
      case 113: return static_permuted_radical_inverse<T, 619>(perm, input);
      case 114: return static_permuted_radical_inverse<T, 631>(perm, input);
      case 115: return static_permuted_radical_inverse<T, 641>(perm, input);
      case 116: return static_permuted_radical_inverse<T, 643>(perm, input);
      case 117: return static_permuted_radical_inverse<T, 647>(perm, input);
      case 118: return static_permuted_radical_inverse<T, 653>(perm, input);
      case 119: return static_permuted_radical_inverse<T, 659>(perm, input);
      case 120: return static_permuted_radical_inverse<T, 661>(perm, input);
      case 121: return static_permuted_radical_inverse<T, 673>(perm, input);
      case 122: return static_permuted_radical_inverse<T, 677>(perm, input);
      case 123: return static_permuted_radical_inverse<T, 683>(perm, input);
      case 124: return static_permuted_radical_inverse<T, 691>(perm, input);
      case 125: return static_permuted_radical_inverse<T, 701>(perm, input);
      case 126: return static_permuted_radical_inverse<T, 709>(perm, input);
      case 127: return static_permuted_radical_inverse<T, 719>(perm, input);
      case 128: return static_permuted_radical_inverse<T, 727>(perm, input);
      case 129: return static_permuted_radical_inverse<T, 733>(perm, input);
      case 130: return static_permuted_radical_inverse<T, 739>(perm, input);
      case 131: return static_permuted_radical_inverse<T, 743>(perm, input);
      case 132: return static_permuted_radical_inverse<T, 751>(perm, input);
      case 133: return static_permuted_radical_inverse<T, 757>(perm, input);
      case 134: return static_permuted_radical_inverse<T, 761>(perm, input);
      case 135: return static_permuted_radical_inverse<T, 769>(perm, input);
      case 136: return static_permuted_radical_inverse<T, 773>(perm, input);
      case 137: return static_permuted_radical_inverse<T, 787>(perm, input);
      case 138: return static_permuted_radical_inverse<T, 797>(perm, input);
      case 139: return static_permuted_radical_inverse<T, 809>(perm, input);
      case 140: return static_permuted_radical_inverse<T, 811>(perm, input);
      case 141: return static_permuted_radical_inverse<T, 821>(perm, input);
      case 142: return static_permuted_radical_inverse<T, 823>(perm, input);
      case 143: return static_permuted_radical_inverse<T, 827>(perm, input);
      case 144: return static_permuted_radical_inverse<T, 829>(perm, input);
      case 145: return static_permuted_radical_inverse<T, 839>(perm, input);
      case 146: return static_permuted_radical_inverse<T, 853>(perm, input);
      case 147: return static_permuted_radical_inverse<T, 857>(perm, input);
      case 148: return static_permuted_radical_inverse<T, 859>(perm, input);
      case 149: return static_permuted_radical_inverse<T, 863>(perm, input);
      case 150: return static_permuted_radical_inverse<T, 877>(perm, input);
      case 151: return static_permuted_radical_inverse<T, 881>(perm, input);
      case 152: return static_permuted_radical_inverse<T, 883>(perm, input);
      case 153: return static_permuted_radical_inverse<T, 887>(perm, input);
      case 154: return static_permuted_radical_inverse<T, 907>(perm, input);
      case 155: return static_permuted_radical_inverse<T, 911>(perm, input);
      case 156: return static_permuted_radical_inverse<T, 919>(perm, input);
      case 157: return static_permuted_radical_inverse<T, 929>(perm, input);
      case 158: return static_permuted_radical_inverse<T, 937>(perm, input);
      case 159: return static_permuted_radical_inverse<T, 941>(perm, input);
      case 160: return static_permuted_radical_inverse<T, 947>(perm, input);
      case 161: return static_permuted_radical_inverse<T, 953>(perm, input);
      case 162: return static_permuted_radical_inverse<T, 967>(perm, input);
      case 163: return static_permuted_radical_inverse<T, 971>(perm, input);
      case 164: return static_permuted_radical_inverse<T, 977>(perm, input);
      case 165: return static_permuted_radical_inverse<T, 983>(perm, input);
      case 166: return static_permuted_radical_inverse<T, 991>(perm, input);
      case 167: return static_permuted_radical_inverse<T, 997>(perm, input);
      case 168: return static_permuted_radical_inverse<T, 1009>(perm, input);
      case 169: return static_permuted_radical_inverse<T, 1013>(perm, input);
      case 170: return static_permuted_radical_inverse<T, 1019>(perm, input);
      case 171: return static_permuted_radical_inverse<T, 1021>(perm, input);
      case 172: return static_permuted_radical_inverse<T, 1031>(perm, input);
      case 173: return static_permuted_radical_inverse<T, 1033>(perm, input);
      case 174: return static_permuted_radical_inverse<T, 1039>(perm, input);
      case 175: return static_permuted_radical_inverse<T, 1049>(perm, input);
      case 176: return static_permuted_radical_inverse<T, 1051>(perm, input);
      case 177: return static_permuted_radical_inverse<T, 1061>(perm, input);
      case 178: return static_permuted_radical_inverse<T, 1063>(perm, input);
      case 179: return static_permuted_radical_inverse<T, 1069>(perm, input);
      case 180: return static_permuted_radical_inverse<T, 1087>(perm, input);
      case 181: return static_permuted_radical_inverse<T, 1091>(perm, input);
      case 182: return static_permuted_radical_inverse<T, 1093>(perm, input);
      case 183: return static_permuted_radical_inverse<T, 1097>(perm, input);
      case 184: return static_permuted_radical_inverse<T, 1103>(perm, input);
      case 185: return static_permuted_radical_inverse<T, 1109>(perm, input);
      case 186: return static_permuted_radical_inverse<T, 1117>(perm, input);
      case 187: return static_permuted_radical_inverse<T, 1123>(perm, input);
      case 188: return static_permuted_radical_inverse<T, 1129>(perm, input);
      case 189: return static_permuted_radical_inverse<T, 1151>(perm, input);
      case 190: return static_permuted_radical_inverse<T, 1153>(perm, input);
      case 191: return static_permuted_radical_inverse<T, 1163>(perm, input);
      case 192: return static_permuted_radical_inverse<T, 1171>(perm, input);
      case 193: return static_permuted_radical_inverse<T, 1181>(perm, input);
      case 194: return static_permuted_radical_inverse<T, 1187>(perm, input);
      case 195: return static_permuted_radical_inverse<T, 1193>(perm, input);
      case 196: return static_permuted_radical_inverse<T, 1201>(perm, input);
      case 197: return static_permuted_radical_inverse<T, 1213>(perm, input);
      case 198: return static_permuted_radical_inverse<T, 1217>(perm, input);
      case 199: return static_permuted_radical_inverse<T, 1223>(perm, input);
      case 200: return static_permuted_radical_inverse<T, 1229>(perm, input);
      case 201: return static_permuted_radical_inverse<T, 1231>(perm, input);
      case 202: return static_permuted_radical_inverse<T, 1237>(perm, input);
      case 203: return static_permuted_radical_inverse<T, 1249>(perm, input);
      case 204: return static_permuted_radical_inverse<T, 1259>(perm, input);
      case 205: return static_permuted_radical_inverse<T, 1277>(perm, input);
      case 206: return static_permuted_radical_inverse<T, 1279>(perm, input);
      case 207: return static_permuted_radical_inverse<T, 1283>(perm, input);
      case 208: return static_permuted_radical_inverse<T, 1289>(perm, input);
      case 209: return static_permuted_radical_inverse<T, 1291>(perm, input);
      case 210: return static_permuted_radical_inverse<T, 1297>(perm, input);
      case 211: return static_permuted_radical_inverse<T, 1301>(perm, input);
      case 212: return static_permuted_radical_inverse<T, 1303>(perm, input);
      case 213: return static_permuted_radical_inverse<T, 1307>(perm, input);
      case 214: return static_permuted_radical_inverse<T, 1319>(perm, input);
      case 215: return static_permuted_radical_inverse<T, 1321>(perm, input);
      case 216: return static_permuted_radical_inverse<T, 1327>(perm, input);
      case 217: return static_permuted_radical_inverse<T, 1361>(perm, input);
      case 218: return static_permuted_radical_inverse<T, 1367>(perm, input);
      case 219: return static_permuted_radical_inverse<T, 1373>(perm, input);
      case 220: return static_permuted_radical_inverse<T, 1381>(perm, input);
      case 221: return static_permuted_radical_inverse<T, 1399>(perm, input);
      case 222: return static_permuted_radical_inverse<T, 1409>(perm, input);
      case 223: return static_permuted_radical_inverse<T, 1423>(perm, input);
      case 224: return static_permuted_radical_inverse<T, 1427>(perm, input);
      case 225: return static_permuted_radical_inverse<T, 1429>(perm, input);
      case 226: return static_permuted_radical_inverse<T, 1433>(perm, input);
      case 227: return static_permuted_radical_inverse<T, 1439>(perm, input);
      case 228: return static_permuted_radical_inverse<T, 1447>(perm, input);
      case 229: return static_permuted_radical_inverse<T, 1451>(perm, input);
      case 230: return static_permuted_radical_inverse<T, 1453>(perm, input);
      case 231: return static_permuted_radical_inverse<T, 1459>(perm, input);
      case 232: return static_permuted_radical_inverse<T, 1471>(perm, input);
      case 233: return static_permuted_radical_inverse<T, 1481>(perm, input);
      case 234: return static_permuted_radical_inverse<T, 1483>(perm, input);
      case 235: return static_permuted_radical_inverse<T, 1487>(perm, input);
      case 236: return static_permuted_radical_inverse<T, 1489>(perm, input);
      case 237: return static_permuted_radical_inverse<T, 1493>(perm, input);
      case 238: return static_permuted_radical_inverse<T, 1499>(perm, input);
      case 239: return static_permuted_radical_inverse<T, 1511>(perm, input);
      case 240: return static_permuted_radical_inverse<T, 1523>(perm, input);
      case 241: return static_permuted_radical_inverse<T, 1531>(perm, input);
      case 242: return static_permuted_radical_inverse<T, 1543>(perm, input);
      case 243: return static_permuted_radical_inverse<T, 1549>(perm, input);
      case 244: return static_permuted_radical_inverse<T, 1553>(perm, input);
      case 245: return static_permuted_radical_inverse<T, 1559>(perm, input);
      case 246: return static_permuted_radical_inverse<T, 1567>(perm, input);
      case 247: return static_permuted_radical_inverse<T, 1571>(perm, input);
      case 248: return static_permuted_radical_inverse<T, 1579>(perm, input);
      case 249: return static_permuted_radical_inverse<T, 1583>(perm, input);
      case 250: return static_permuted_radical_inverse<T, 1597>(perm, input);
      case 251: return static_permuted_radical_inverse<T, 1601>(perm, input);
      case 252: return static_permuted_radical_inverse<T, 1607>(perm, input);
      case 253: return static_permuted_radical_inverse<T, 1609>(perm, input);
      case 254: return static_permuted_radical_inverse<T, 1613>(perm, input);
      case 255: return static_permuted_radical_inverse<T, 1619>(perm, input);
      case 256: return static_permuted_radical_inverse<T, 1621>(perm, input);
      case 257: return static_permuted_radical_inverse<T, 1627>(perm, input);
      case 258: return static_permuted_radical_inverse<T, 1637>(perm, input);
      case 259: return static_permuted_radical_inverse<T, 1657>(perm, input);
      case 260: return static_permuted_radical_inverse<T, 1663>(perm, input);
      case 261: return static_permuted_radical_inverse<T, 1667>(perm, input);
      case 262: return static_permuted_radical_inverse<T, 1669>(perm, input);
      case 263: return static_permuted_radical_inverse<T, 1693>(perm, input);
      case 264: return static_permuted_radical_inverse<T, 1697>(perm, input);
      case 265: return static_permuted_radical_inverse<T, 1699>(perm, input);
      case 266: return static_permuted_radical_inverse<T, 1709>(perm, input);
      case 267: return static_permuted_radical_inverse<T, 1721>(perm, input);
      case 268: return static_permuted_radical_inverse<T, 1723>(perm, input);
      case 269: return static_permuted_radical_inverse<T, 1733>(perm, input);
      case 270: return static_permuted_radical_inverse<T, 1741>(perm, input);
      case 271: return static_permuted_radical_inverse<T, 1747>(perm, input);
      case 272: return static_permuted_radical_inverse<T, 1753>(perm, input);
      case 273: return static_permuted_radical_inverse<T, 1759>(perm, input);
      case 274: return static_permuted_radical_inverse<T, 1777>(perm, input);
      case 275: return static_permuted_radical_inverse<T, 1783>(perm, input);
      case 276: return static_permuted_radical_inverse<T, 1787>(perm, input);
      case 277: return static_permuted_radical_inverse<T, 1789>(perm, input);
      case 278: return static_permuted_radical_inverse<T, 1801>(perm, input);
      case 279: return static_permuted_radical_inverse<T, 1811>(perm, input);
      case 280: return static_permuted_radical_inverse<T, 1823>(perm, input);
      case 281: return static_permuted_radical_inverse<T, 1831>(perm, input);
      case 282: return static_permuted_radical_inverse<T, 1847>(perm, input);
      case 283: return static_permuted_radical_inverse<T, 1861>(perm, input);
      case 284: return static_permuted_radical_inverse<T, 1867>(perm, input);
      case 285: return static_permuted_radical_inverse<T, 1871>(perm, input);
      case 286: return static_permuted_radical_inverse<T, 1873>(perm, input);
      case 287: return static_permuted_radical_inverse<T, 1877>(perm, input);
      case 288: return static_permuted_radical_inverse<T, 1879>(perm, input);
      case 289: return static_permuted_radical_inverse<T, 1889>(perm, input);
      case 290: return static_permuted_radical_inverse<T, 1901>(perm, input);
      case 291: return static_permuted_radical_inverse<T, 1907>(perm, input);
      case 292: return static_permuted_radical_inverse<T, 1913>(perm, input);
      case 293: return static_permuted_radical_inverse<T, 1931>(perm, input);
      case 294: return static_permuted_radical_inverse<T, 1933>(perm, input);
      case 295: return static_permuted_radical_inverse<T, 1949>(perm, input);
      case 296: return static_permuted_radical_inverse<T, 1951>(perm, input);
      case 297: return static_permuted_radical_inverse<T, 1973>(perm, input);
      case 298: return static_permuted_radical_inverse<T, 1979>(perm, input);
      case 299: return static_permuted_radical_inverse<T, 1987>(perm, input);
      case 300: return static_permuted_radical_inverse<T, 1993>(perm, input);
      case 301: return static_permuted_radical_inverse<T, 1997>(perm, input);
      case 302: return static_permuted_radical_inverse<T, 1999>(perm, input);
      case 303: return static_permuted_radical_inverse<T, 2003>(perm, input);
      case 304: return static_permuted_radical_inverse<T, 2011>(perm, input);
      case 305: return static_permuted_radical_inverse<T, 2017>(perm, input);
      case 306: return static_permuted_radical_inverse<T, 2027>(perm, input);
      case 307: return static_permuted_radical_inverse<T, 2029>(perm, input);
      case 308: return static_permuted_radical_inverse<T, 2039>(perm, input);
      case 309: return static_permuted_radical_inverse<T, 2053>(perm, input);
      case 310: return static_permuted_radical_inverse<T, 2063>(perm, input);
      case 311: return static_permuted_radical_inverse<T, 2069>(perm, input);
      case 312: return static_permuted_radical_inverse<T, 2081>(perm, input);
      case 313: return static_permuted_radical_inverse<T, 2083>(perm, input);
      case 314: return static_permuted_radical_inverse<T, 2087>(perm, input);
      case 315: return static_permuted_radical_inverse<T, 2089>(perm, input);
      case 316: return static_permuted_radical_inverse<T, 2099>(perm, input);
      case 317: return static_permuted_radical_inverse<T, 2111>(perm, input);
      case 318: return static_permuted_radical_inverse<T, 2113>(perm, input);
      case 319: return static_permuted_radical_inverse<T, 2129>(perm, input);
      case 320: return static_permuted_radical_inverse<T, 2131>(perm, input);
      case 321: return static_permuted_radical_inverse<T, 2137>(perm, input);
      case 322: return static_permuted_radical_inverse<T, 2141>(perm, input);
      case 323: return static_permuted_radical_inverse<T, 2143>(perm, input);
      case 324: return static_permuted_radical_inverse<T, 2153>(perm, input);
      case 325: return static_permuted_radical_inverse<T, 2161>(perm, input);
      case 326: return static_permuted_radical_inverse<T, 2179>(perm, input);
      case 327: return static_permuted_radical_inverse<T, 2203>(perm, input);
      case 328: return static_permuted_radical_inverse<T, 2207>(perm, input);
      case 329: return static_permuted_radical_inverse<T, 2213>(perm, input);
      case 330: return static_permuted_radical_inverse<T, 2221>(perm, input);
      case 331: return static_permuted_radical_inverse<T, 2237>(perm, input);
      case 332: return static_permuted_radical_inverse<T, 2239>(perm, input);
      case 333: return static_permuted_radical_inverse<T, 2243>(perm, input);
      case 334: return static_permuted_radical_inverse<T, 2251>(perm, input);
      case 335: return static_permuted_radical_inverse<T, 2267>(perm, input);
      case 336: return static_permuted_radical_inverse<T, 2269>(perm, input);
      case 337: return static_permuted_radical_inverse<T, 2273>(perm, input);
      case 338: return static_permuted_radical_inverse<T, 2281>(perm, input);
      case 339: return static_permuted_radical_inverse<T, 2287>(perm, input);
      case 340: return static_permuted_radical_inverse<T, 2293>(perm, input);
      case 341: return static_permuted_radical_inverse<T, 2297>(perm, input);
      case 342: return static_permuted_radical_inverse<T, 2309>(perm, input);
      case 343: return static_permuted_radical_inverse<T, 2311>(perm, input);
      case 344: return static_permuted_radical_inverse<T, 2333>(perm, input);
      case 345: return static_permuted_radical_inverse<T, 2339>(perm, input);
      case 346: return static_permuted_radical_inverse<T, 2341>(perm, input);
      case 347: return static_permuted_radical_inverse<T, 2347>(perm, input);
      case 348: return static_permuted_radical_inverse<T, 2351>(perm, input);
      case 349: return static_permuted_radical_inverse<T, 2357>(perm, input);
      case 350: return static_permuted_radical_inverse<T, 2371>(perm, input);
      case 351: return static_permuted_radical_inverse<T, 2377>(perm, input);
      case 352: return static_permuted_radical_inverse<T, 2381>(perm, input);
      case 353: return static_permuted_radical_inverse<T, 2383>(perm, input);
      case 354: return static_permuted_radical_inverse<T, 2389>(perm, input);
      case 355: return static_permuted_radical_inverse<T, 2393>(perm, input);
      case 356: return static_permuted_radical_inverse<T, 2399>(perm, input);
      case 357: return static_permuted_radical_inverse<T, 2411>(perm, input);
      case 358: return static_permuted_radical_inverse<T, 2417>(perm, input);
      case 359: return static_permuted_radical_inverse<T, 2423>(perm, input);
      case 360: return static_permuted_radical_inverse<T, 2437>(perm, input);
      case 361: return static_permuted_radical_inverse<T, 2441>(perm, input);
      case 362: return static_permuted_radical_inverse<T, 2447>(perm, input);
      case 363: return static_permuted_radical_inverse<T, 2459>(perm, input);
      case 364: return static_permuted_radical_inverse<T, 2467>(perm, input);
      case 365: return static_permuted_radical_inverse<T, 2473>(perm, input);
      case 366: return static_permuted_radical_inverse<T, 2477>(perm, input);
      case 367: return static_permuted_radical_inverse<T, 2503>(perm, input);
      case 368: return static_permuted_radical_inverse<T, 2521>(perm, input);
      case 369: return static_permuted_radical_inverse<T, 2531>(perm, input);
      case 370: return static_permuted_radical_inverse<T, 2539>(perm, input);
      case 371: return static_permuted_radical_inverse<T, 2543>(perm, input);
      case 372: return static_permuted_radical_inverse<T, 2549>(perm, input);
      case 373: return static_permuted_radical_inverse<T, 2551>(perm, input);
      case 374: return static_permuted_radical_inverse<T, 2557>(perm, input);
      case 375: return static_permuted_radical_inverse<T, 2579>(perm, input);
      case 376: return static_permuted_radical_inverse<T, 2591>(perm, input);
      case 377: return static_permuted_radical_inverse<T, 2593>(perm, input);
      case 378: return static_permuted_radical_inverse<T, 2609>(perm, input);
      case 379: return static_permuted_radical_inverse<T, 2617>(perm, input);
      case 380: return static_permuted_radical_inverse<T, 2621>(perm, input);
      case 381: return static_permuted_radical_inverse<T, 2633>(perm, input);
      case 382: return static_permuted_radical_inverse<T, 2647>(perm, input);
      case 383: return static_permuted_radical_inverse<T, 2657>(perm, input);
      case 384: return static_permuted_radical_inverse<T, 2659>(perm, input);
      case 385: return static_permuted_radical_inverse<T, 2663>(perm, input);
      case 386: return static_permuted_radical_inverse<T, 2671>(perm, input);
      case 387: return static_permuted_radical_inverse<T, 2677>(perm, input);
      case 388: return static_permuted_radical_inverse<T, 2683>(perm, input);
      case 389: return static_permuted_radical_inverse<T, 2687>(perm, input);
      case 390: return static_permuted_radical_inverse<T, 2689>(perm, input);
      case 391: return static_permuted_radical_inverse<T, 2693>(perm, input);
      case 392: return static_permuted_radical_inverse<T, 2699>(perm, input);
      case 393: return static_permuted_radical_inverse<T, 2707>(perm, input);
      case 394: return static_permuted_radical_inverse<T, 2711>(perm, input);
      case 395: return static_permuted_radical_inverse<T, 2713>(perm, input);
      case 396: return static_permuted_radical_inverse<T, 2719>(perm, input);
      case 397: return static_permuted_radical_inverse<T, 2729>(perm, input);
      case 398: return static_permuted_radical_inverse<T, 2731>(perm, input);
      case 399: return static_permuted_radical_inverse<T, 2741>(perm, input);
      case 400: return static_permuted_radical_inverse<T, 2749>(perm, input);
      case 401: return static_permuted_radical_inverse<T, 2753>(perm, input);
      case 402: return static_permuted_radical_inverse<T, 2767>(perm, input);
      case 403: return static_permuted_radical_inverse<T, 2777>(perm, input);
      case 404: return static_permuted_radical_inverse<T, 2789>(perm, input);
      case 405: return static_permuted_radical_inverse<T, 2791>(perm, input);
      case 406: return static_permuted_radical_inverse<T, 2797>(perm, input);
      case 407: return static_permuted_radical_inverse<T, 2801>(perm, input);
      case 408: return static_permuted_radical_inverse<T, 2803>(perm, input);
      case 409: return static_permuted_radical_inverse<T, 2819>(perm, input);
      case 410: return static_permuted_radical_inverse<T, 2833>(perm, input);
      case 411: return static_permuted_radical_inverse<T, 2837>(perm, input);
      case 412: return static_permuted_radical_inverse<T, 2843>(perm, input);
      case 413: return static_permuted_radical_inverse<T, 2851>(perm, input);
      case 414: return static_permuted_radical_inverse<T, 2857>(perm, input);
      case 415: return static_permuted_radical_inverse<T, 2861>(perm, input);
      case 416: return static_permuted_radical_inverse<T, 2879>(perm, input);
      case 417: return static_permuted_radical_inverse<T, 2887>(perm, input);
      case 418: return static_permuted_radical_inverse<T, 2897>(perm, input);
      case 419: return static_permuted_radical_inverse<T, 2903>(perm, input);
      case 420: return static_permuted_radical_inverse<T, 2909>(perm, input);
      case 421: return static_permuted_radical_inverse<T, 2917>(perm, input);
      case 422: return static_permuted_radical_inverse<T, 2927>(perm, input);
      case 423: return static_permuted_radical_inverse<T, 2939>(perm, input);
      case 424: return static_permuted_radical_inverse<T, 2953>(perm, input);
      case 425: return static_permuted_radical_inverse<T, 2957>(perm, input);
      case 426: return static_permuted_radical_inverse<T, 2963>(perm, input);
      case 427: return static_permuted_radical_inverse<T, 2969>(perm, input);
      case 428: return static_permuted_radical_inverse<T, 2971>(perm, input);
      case 429: return static_permuted_radical_inverse<T, 2999>(perm, input);
      case 430: return static_permuted_radical_inverse<T, 3001>(perm, input);
      case 431: return static_permuted_radical_inverse<T, 3011>(perm, input);
      case 432: return static_permuted_radical_inverse<T, 3019>(perm, input);
      case 433: return static_permuted_radical_inverse<T, 3023>(perm, input);
      case 434: return static_permuted_radical_inverse<T, 3037>(perm, input);
      case 435: return static_permuted_radical_inverse<T, 3041>(perm, input);
      case 436: return static_permuted_radical_inverse<T, 3049>(perm, input);
      case 437: return static_permuted_radical_inverse<T, 3061>(perm, input);
      case 438: return static_permuted_radical_inverse<T, 3067>(perm, input);
      case 439: return static_permuted_radical_inverse<T, 3079>(perm, input);
      case 440: return static_permuted_radical_inverse<T, 3083>(perm, input);
      case 441: return static_permuted_radical_inverse<T, 3089>(perm, input);
      case 442: return static_permuted_radical_inverse<T, 3109>(perm, input);
      case 443: return static_permuted_radical_inverse<T, 3119>(perm, input);
      case 444: return static_permuted_radical_inverse<T, 3121>(perm, input);
      case 445: return static_permuted_radical_inverse<T, 3137>(perm, input);
      case 446: return static_permuted_radical_inverse<T, 3163>(perm, input);
      case 447: return static_permuted_radical_inverse<T, 3167>(perm, input);
      case 448: return static_permuted_radical_inverse<T, 3169>(perm, input);
      case 449: return static_permuted_radical_inverse<T, 3181>(perm, input);
      case 450: return static_permuted_radical_inverse<T, 3187>(perm, input);
      case 451: return static_permuted_radical_inverse<T, 3191>(perm, input);
      case 452: return static_permuted_radical_inverse<T, 3203>(perm, input);
      case 453: return static_permuted_radical_inverse<T, 3209>(perm, input);
      case 454: return static_permuted_radical_inverse<T, 3217>(perm, input);
      case 455: return static_permuted_radical_inverse<T, 3221>(perm, input);
      case 456: return static_permuted_radical_inverse<T, 3229>(perm, input);
      case 457: return static_permuted_radical_inverse<T, 3251>(perm, input);
      case 458: return static_permuted_radical_inverse<T, 3253>(perm, input);
      case 459: return static_permuted_radical_inverse<T, 3257>(perm, input);
      case 460: return static_permuted_radical_inverse<T, 3259>(perm, input);
      case 461: return static_permuted_radical_inverse<T, 3271>(perm, input);
      case 462: return static_permuted_radical_inverse<T, 3299>(perm, input);
      case 463: return static_permuted_radical_inverse<T, 3301>(perm, input);
      case 464: return static_permuted_radical_inverse<T, 3307>(perm, input);
      case 465: return static_permuted_radical_inverse<T, 3313>(perm, input);
      case 466: return static_permuted_radical_inverse<T, 3319>(perm, input);
      case 467: return static_permuted_radical_inverse<T, 3323>(perm, input);
      case 468: return static_permuted_radical_inverse<T, 3329>(perm, input);
      case 469: return static_permuted_radical_inverse<T, 3331>(perm, input);
      case 470: return static_permuted_radical_inverse<T, 3343>(perm, input);
      case 471: return static_permuted_radical_inverse<T, 3347>(perm, input);
      case 472: return static_permuted_radical_inverse<T, 3359>(perm, input);
      case 473: return static_permuted_radical_inverse<T, 3361>(perm, input);
      case 474: return static_permuted_radical_inverse<T, 3371>(perm, input);
      case 475: return static_permuted_radical_inverse<T, 3373>(perm, input);
      case 476: return static_permuted_radical_inverse<T, 3389>(perm, input);
      case 477: return static_permuted_radical_inverse<T, 3391>(perm, input);
      case 478: return static_permuted_radical_inverse<T, 3407>(perm, input);
      case 479: return static_permuted_radical_inverse<T, 3413>(perm, input);
      case 480: return static_permuted_radical_inverse<T, 3433>(perm, input);
      case 481: return static_permuted_radical_inverse<T, 3449>(perm, input);
      case 482: return static_permuted_radical_inverse<T, 3457>(perm, input);
      case 483: return static_permuted_radical_inverse<T, 3461>(perm, input);
      case 484: return static_permuted_radical_inverse<T, 3463>(perm, input);
      case 485: return static_permuted_radical_inverse<T, 3467>(perm, input);
      case 486: return static_permuted_radical_inverse<T, 3469>(perm, input);
      case 487: return static_permuted_radical_inverse<T, 3491>(perm, input);
      case 488: return static_permuted_radical_inverse<T, 3499>(perm, input);
      case 489: return static_permuted_radical_inverse<T, 3511>(perm, input);
      case 490: return static_permuted_radical_inverse<T, 3517>(perm, input);
      case 491: return static_permuted_radical_inverse<T, 3527>(perm, input);
      case 492: return static_permuted_radical_inverse<T, 3529>(perm, input);
      case 493: return static_permuted_radical_inverse<T, 3533>(perm, input);
      case 494: return static_permuted_radical_inverse<T, 3539>(perm, input);
      case 495: return static_permuted_radical_inverse<T, 3541>(perm, input);
      case 496: return static_permuted_radical_inverse<T, 3547>(perm, input);
      case 497: return static_permuted_radical_inverse<T, 3557>(perm, input);
      case 498: return static_permuted_radical_inverse<T, 3559>(perm, input);
      case 499: return static_permuted_radical_inverse<T, 3571>(perm, input);
      case 500: return static_permuted_radical_inverse<T, 3581>(perm, input);
      case 501: return static_permuted_radical_inverse<T, 3583>(perm, input);
      case 502: return static_permuted_radical_inverse<T, 3593>(perm, input);
      case 503: return static_permuted_radical_inverse<T, 3607>(perm, input);
      case 504: return static_permuted_radical_inverse<T, 3613>(perm, input);
      case 505: return static_permuted_radical_inverse<T, 3617>(perm, input);
      case 506: return static_permuted_radical_inverse<T, 3623>(perm, input);
      case 507: return static_permuted_radical_inverse<T, 3631>(perm, input);
      case 508: return static_permuted_radical_inverse<T, 3637>(perm, input);
      case 509: return static_permuted_radical_inverse<T, 3643>(perm, input);
      case 510: return static_permuted_radical_inverse<T, 3659>(perm, input);
      case 511: return static_permuted_radical_inverse<T, 3671>(perm, input);
      case 512: return static_permuted_radical_inverse<T, 3673>(perm, input);
      case 513: return static_permuted_radical_inverse<T, 3677>(perm, input);
      case 514: return static_permuted_radical_inverse<T, 3691>(perm, input);
      case 515: return static_permuted_radical_inverse<T, 3697>(perm, input);
      case 516: return static_permuted_radical_inverse<T, 3701>(perm, input);
      case 517: return static_permuted_radical_inverse<T, 3709>(perm, input);
      case 518: return static_permuted_radical_inverse<T, 3719>(perm, input);
      case 519: return static_permuted_radical_inverse<T, 3727>(perm, input);
      case 520: return static_permuted_radical_inverse<T, 3733>(perm, input);
      case 521: return static_permuted_radical_inverse<T, 3739>(perm, input);
      case 522: return static_permuted_radical_inverse<T, 3761>(perm, input);
      case 523: return static_permuted_radical_inverse<T, 3767>(perm, input);
      case 524: return static_permuted_radical_inverse<T, 3769>(perm, input);
      case 525: return static_permuted_radical_inverse<T, 3779>(perm, input);
      case 526: return static_permuted_radical_inverse<T, 3793>(perm, input);
      case 527: return static_permuted_radical_inverse<T, 3797>(perm, input);
      case 528: return static_permuted_radical_inverse<T, 3803>(perm, input);
      case 529: return static_permuted_radical_inverse<T, 3821>(perm, input);
      case 530: return static_permuted_radical_inverse<T, 3823>(perm, input);
      case 531: return static_permuted_radical_inverse<T, 3833>(perm, input);
      case 532: return static_permuted_radical_inverse<T, 3847>(perm, input);
      case 533: return static_permuted_radical_inverse<T, 3851>(perm, input);
      case 534: return static_permuted_radical_inverse<T, 3853>(perm, input);
      case 535: return static_permuted_radical_inverse<T, 3863>(perm, input);
      case 536: return static_permuted_radical_inverse<T, 3877>(perm, input);
      case 537: return static_permuted_radical_inverse<T, 3881>(perm, input);
      case 538: return static_permuted_radical_inverse<T, 3889>(perm, input);
      case 539: return static_permuted_radical_inverse<T, 3907>(perm, input);
      case 540: return static_permuted_radical_inverse<T, 3911>(perm, input);
      case 541: return static_permuted_radical_inverse<T, 3917>(perm, input);
      case 542: return static_permuted_radical_inverse<T, 3919>(perm, input);
      case 543: return static_permuted_radical_inverse<T, 3923>(perm, input);
      case 544: return static_permuted_radical_inverse<T, 3929>(perm, input);
      case 545: return static_permuted_radical_inverse<T, 3931>(perm, input);
      case 546: return static_permuted_radical_inverse<T, 3943>(perm, input);
      case 547: return static_permuted_radical_inverse<T, 3947>(perm, input);
      case 548: return static_permuted_radical_inverse<T, 3967>(perm, input);
      case 549: return static_permuted_radical_inverse<T, 3989>(perm, input);
      case 550: return static_permuted_radical_inverse<T, 4001>(perm, input);
      case 551: return static_permuted_radical_inverse<T, 4003>(perm, input);
      case 552: return static_permuted_radical_inverse<T, 4007>(perm, input);
      case 553: return static_permuted_radical_inverse<T, 4013>(perm, input);
      case 554: return static_permuted_radical_inverse<T, 4019>(perm, input);
      case 555: return static_permuted_radical_inverse<T, 4021>(perm, input);
      case 556: return static_permuted_radical_inverse<T, 4027>(perm, input);
      case 557: return static_permuted_radical_inverse<T, 4049>(perm, input);
      case 558: return static_permuted_radical_inverse<T, 4051>(perm, input);
      case 559: return static_permuted_radical_inverse<T, 4057>(perm, input);
      case 560: return static_permuted_radical_inverse<T, 4073>(perm, input);
      case 561: return static_permuted_radical_inverse<T, 4079>(perm, input);
      case 562: return static_permuted_radical_inverse<T, 4091>(perm, input);
      case 563: return static_permuted_radical_inverse<T, 4093>(perm, input);
      case 564: return static_permuted_radical_inverse<T, 4099>(perm, input);
      case 565: return static_permuted_radical_inverse<T, 4111>(perm, input);
      case 566: return static_permuted_radical_inverse<T, 4127>(perm, input);
      case 567: return static_permuted_radical_inverse<T, 4129>(perm, input);
      case 568: return static_permuted_radical_inverse<T, 4133>(perm, input);
      case 569: return static_permuted_radical_inverse<T, 4139>(perm, input);
      case 570: return static_permuted_radical_inverse<T, 4153>(perm, input);
      case 571: return static_permuted_radical_inverse<T, 4157>(perm, input);
      case 572: return static_permuted_radical_inverse<T, 4159>(perm, input);
      case 573: return static_permuted_radical_inverse<T, 4177>(perm, input);
      case 574: return static_permuted_radical_inverse<T, 4201>(perm, input);
      case 575: return static_permuted_radical_inverse<T, 4211>(perm, input);
      case 576: return static_permuted_radical_inverse<T, 4217>(perm, input);
      case 577: return static_permuted_radical_inverse<T, 4219>(perm, input);
      case 578: return static_permuted_radical_inverse<T, 4229>(perm, input);
      case 579: return static_permuted_radical_inverse<T, 4231>(perm, input);
      case 580: return static_permuted_radical_inverse<T, 4241>(perm, input);
      case 581: return static_permuted_radical_inverse<T, 4243>(perm, input);
      case 582: return static_permuted_radical_inverse<T, 4253>(perm, input);
      case 583: return static_permuted_radical_inverse<T, 4259>(perm, input);
      case 584: return static_permuted_radical_inverse<T, 4261>(perm, input);
      case 585: return static_permuted_radical_inverse<T, 4271>(perm, input);
      case 586: return static_permuted_radical_inverse<T, 4273>(perm, input);
      case 587: return static_permuted_radical_inverse<T, 4283>(perm, input);
      case 588: return static_permuted_radical_inverse<T, 4289>(perm, input);
      case 589: return static_permuted_radical_inverse<T, 4297>(perm, input);
      case 590: return static_permuted_radical_inverse<T, 4327>(perm, input);
      case 591: return static_permuted_radical_inverse<T, 4337>(perm, input);
      case 592: return static_permuted_radical_inverse<T, 4339>(perm, input);
      case 593: return static_permuted_radical_inverse<T, 4349>(perm, input);
      case 594: return static_permuted_radical_inverse<T, 4357>(perm, input);
      case 595: return static_permuted_radical_inverse<T, 4363>(perm, input);
      case 596: return static_permuted_radical_inverse<T, 4373>(perm, input);
      case 597: return static_permuted_radical_inverse<T, 4391>(perm, input);
      case 598: return static_permuted_radical_inverse<T, 4397>(perm, input);
      case 599: return static_permuted_radical_inverse<T, 4409>(perm, input);
      case 600: return static_permuted_radical_inverse<T, 4421>(perm, input);
      case 601: return static_permuted_radical_inverse<T, 4423>(perm, input);
      case 602: return static_permuted_radical_inverse<T, 4441>(perm, input);
      case 603: return static_permuted_radical_inverse<T, 4447>(perm, input);
      case 604: return static_permuted_radical_inverse<T, 4451>(perm, input);
      case 605: return static_permuted_radical_inverse<T, 4457>(perm, input);
      case 606: return static_permuted_radical_inverse<T, 4463>(perm, input);
      case 607: return static_permuted_radical_inverse<T, 4481>(perm, input);
      case 608: return static_permuted_radical_inverse<T, 4483>(perm, input);
      case 609: return static_permuted_radical_inverse<T, 4493>(perm, input);
      case 610: return static_permuted_radical_inverse<T, 4507>(perm, input);
      case 611: return static_permuted_radical_inverse<T, 4513>(perm, input);
      case 612: return static_permuted_radical_inverse<T, 4517>(perm, input);
      case 613: return static_permuted_radical_inverse<T, 4519>(perm, input);
      case 614: return static_permuted_radical_inverse<T, 4523>(perm, input);
      case 615: return static_permuted_radical_inverse<T, 4547>(perm, input);
      case 616: return static_permuted_radical_inverse<T, 4549>(perm, input);
      case 617: return static_permuted_radical_inverse<T, 4561>(perm, input);
      case 618: return static_permuted_radical_inverse<T, 4567>(perm, input);
      case 619: return static_permuted_radical_inverse<T, 4583>(perm, input);
      case 620: return static_permuted_radical_inverse<T, 4591>(perm, input);
      case 621: return static_permuted_radical_inverse<T, 4597>(perm, input);
      case 622: return static_permuted_radical_inverse<T, 4603>(perm, input);
      case 623: return static_permuted_radical_inverse<T, 4621>(perm, input);
      case 624: return static_permuted_radical_inverse<T, 4637>(perm, input);
      case 625: return static_permuted_radical_inverse<T, 4639>(perm, input);
      case 626: return static_permuted_radical_inverse<T, 4643>(perm, input);
      case 627: return static_permuted_radical_inverse<T, 4649>(perm, input);
      case 628: return static_permuted_radical_inverse<T, 4651>(perm, input);
      case 629: return static_permuted_radical_inverse<T, 4657>(perm, input);
      case 630: return static_permuted_radical_inverse<T, 4663>(perm, input);
      case 631: return static_permuted_radical_inverse<T, 4673>(perm, input);
      case 632: return static_permuted_radical_inverse<T, 4679>(perm, input);
      case 633: return static_permuted_radical_inverse<T, 4691>(perm, input);
      case 634: return static_permuted_radical_inverse<T, 4703>(perm, input);
      case 635: return static_permuted_radical_inverse<T, 4721>(perm, input);
      case 636: return static_permuted_radical_inverse<T, 4723>(perm, input);
      case 637: return static_permuted_radical_inverse<T, 4729>(perm, input);
      case 638: return static_permuted_radical_inverse<T, 4733>(perm, input);
      case 639: return static_permuted_radical_inverse<T, 4751>(perm, input);
      case 640: return static_permuted_radical_inverse<T, 4759>(perm, input);
      case 641: return static_permuted_radical_inverse<T, 4783>(perm, input);
      case 642: return static_permuted_radical_inverse<T, 4787>(perm, input);
      case 643: return static_permuted_radical_inverse<T, 4789>(perm, input);
      case 644: return static_permuted_radical_inverse<T, 4793>(perm, input);
      case 645: return static_permuted_radical_inverse<T, 4799>(perm, input);
      case 646: return static_permuted_radical_inverse<T, 4801>(perm, input);
      case 647: return static_permuted_radical_inverse<T, 4813>(perm, input);
      case 648: return static_permuted_radical_inverse<T, 4817>(perm, input);
      case 649: return static_permuted_radical_inverse<T, 4831>(perm, input);
      case 650: return static_permuted_radical_inverse<T, 4861>(perm, input);
      case 651: return static_permuted_radical_inverse<T, 4871>(perm, input);
      case 652: return static_permuted_radical_inverse<T, 4877>(perm, input);
      case 653: return static_permuted_radical_inverse<T, 4889>(perm, input);
      case 654: return static_permuted_radical_inverse<T, 4903>(perm, input);
      case 655: return static_permuted_radical_inverse<T, 4909>(perm, input);
      case 656: return static_permuted_radical_inverse<T, 4919>(perm, input);
      case 657: return static_permuted_radical_inverse<T, 4931>(perm, input);
      case 658: return static_permuted_radical_inverse<T, 4933>(perm, input);
      case 659: return static_permuted_radical_inverse<T, 4937>(perm, input);
      case 660: return static_permuted_radical_inverse<T, 4943>(perm, input);
      case 661: return static_permuted_radical_inverse<T, 4951>(perm, input);
      case 662: return static_permuted_radical_inverse<T, 4957>(perm, input);
      case 663: return static_permuted_radical_inverse<T, 4967>(perm, input);
      case 664: return static_permuted_radical_inverse<T, 4969>(perm, input);
      case 665: return static_permuted_radical_inverse<T, 4973>(perm, input);
      case 666: return static_permuted_radical_inverse<T, 4987>(perm, input);
      case 667: return static_permuted_radical_inverse<T, 4993>(perm, input);
      case 668: return static_permuted_radical_inverse<T, 4999>(perm, input);
      case 669: return static_permuted_radical_inverse<T, 5003>(perm, input);
      case 670: return static_permuted_radical_inverse<T, 5009>(perm, input);
      case 671: return static_permuted_radical_inverse<T, 5011>(perm, input);
      case 672: return static_permuted_radical_inverse<T, 5021>(perm, input);
      case 673: return static_permuted_radical_inverse<T, 5023>(perm, input);
      case 674: return static_permuted_radical_inverse<T, 5039>(perm, input);
      case 675: return static_permuted_radical_inverse<T, 5051>(perm, input);
      case 676: return static_permuted_radical_inverse<T, 5059>(perm, input);
      case 677: return static_permuted_radical_inverse<T, 5077>(perm, input);
      case 678: return static_permuted_radical_inverse<T, 5081>(perm, input);
      case 679: return static_permuted_radical_inverse<T, 5087>(perm, input);
      case 680: return static_permuted_radical_inverse<T, 5099>(perm, input);
      case 681: return static_permuted_radical_inverse<T, 5101>(perm, input);
      case 682: return static_permuted_radical_inverse<T, 5107>(perm, input);
      case 683: return static_permuted_radical_inverse<T, 5113>(perm, input);
      case 684: return static_permuted_radical_inverse<T, 5119>(perm, input);
      case 685: return static_permuted_radical_inverse<T, 5147>(perm, input);
      case 686: return static_permuted_radical_inverse<T, 5153>(perm, input);
      case 687: return static_permuted_radical_inverse<T, 5167>(perm, input);
      case 688: return static_permuted_radical_inverse<T, 5171>(perm, input);
      case 689: return static_permuted_radical_inverse<T, 5179>(perm, input);
      case 690: return static_permuted_radical_inverse<T, 5189>(perm, input);
      case 691: return static_permuted_radical_inverse<T, 5197>(perm, input);
      case 692: return static_permuted_radical_inverse<T, 5209>(perm, input);
      case 693: return static_permuted_radical_inverse<T, 5227>(perm, input);
      case 694: return static_permuted_radical_inverse<T, 5231>(perm, input);
      case 695: return static_permuted_radical_inverse<T, 5233>(perm, input);
      case 696: return static_permuted_radical_inverse<T, 5237>(perm, input);
      case 697: return static_permuted_radical_inverse<T, 5261>(perm, input);
      case 698: return static_permuted_radical_inverse<T, 5273>(perm, input);
      case 699: return static_permuted_radical_inverse<T, 5279>(perm, input);
      case 700: return static_permuted_radical_inverse<T, 5281>(perm, input);
      case 701: return static_permuted_radical_inverse<T, 5297>(perm, input);
      case 702: return static_permuted_radical_inverse<T, 5303>(perm, input);
      case 703: return static_permuted_radical_inverse<T, 5309>(perm, input);
      case 704: return static_permuted_radical_inverse<T, 5323>(perm, input);
      case 705: return static_permuted_radical_inverse<T, 5333>(perm, input);
      case 706: return static_permuted_radical_inverse<T, 5347>(perm, input);
      case 707: return static_permuted_radical_inverse<T, 5351>(perm, input);
      case 708: return static_permuted_radical_inverse<T, 5381>(perm, input);
      case 709: return static_permuted_radical_inverse<T, 5387>(perm, input);
      case 710: return static_permuted_radical_inverse<T, 5393>(perm, input);
      case 711: return static_permuted_radical_inverse<T, 5399>(perm, input);
      case 712: return static_permuted_radical_inverse<T, 5407>(perm, input);
      case 713: return static_permuted_radical_inverse<T, 5413>(perm, input);
      case 714: return static_permuted_radical_inverse<T, 5417>(perm, input);
      case 715: return static_permuted_radical_inverse<T, 5419>(perm, input);
      case 716: return static_permuted_radical_inverse<T, 5431>(perm, input);
      case 717: return static_permuted_radical_inverse<T, 5437>(perm, input);
      case 718: return static_permuted_radical_inverse<T, 5441>(perm, input);
      case 719: return static_permuted_radical_inverse<T, 5443>(perm, input);
      case 720: return static_permuted_radical_inverse<T, 5449>(perm, input);
      case 721: return static_permuted_radical_inverse<T, 5471>(perm, input);
      case 722: return static_permuted_radical_inverse<T, 5477>(perm, input);
      case 723: return static_permuted_radical_inverse<T, 5479>(perm, input);
      case 724: return static_permuted_radical_inverse<T, 5483>(perm, input);
      case 725: return static_permuted_radical_inverse<T, 5501>(perm, input);
      case 726: return static_permuted_radical_inverse<T, 5503>(perm, input);
      case 727: return static_permuted_radical_inverse<T, 5507>(perm, input);
      case 728: return static_permuted_radical_inverse<T, 5519>(perm, input);
      case 729: return static_permuted_radical_inverse<T, 5521>(perm, input);
      case 730: return static_permuted_radical_inverse<T, 5527>(perm, input);
      case 731: return static_permuted_radical_inverse<T, 5531>(perm, input);
      case 732: return static_permuted_radical_inverse<T, 5557>(perm, input);
      case 733: return static_permuted_radical_inverse<T, 5563>(perm, input);
      case 734: return static_permuted_radical_inverse<T, 5569>(perm, input);
      case 735: return static_permuted_radical_inverse<T, 5573>(perm, input);
      case 736: return static_permuted_radical_inverse<T, 5581>(perm, input);
      case 737: return static_permuted_radical_inverse<T, 5591>(perm, input);
      case 738: return static_permuted_radical_inverse<T, 5623>(perm, input);
      case 739: return static_permuted_radical_inverse<T, 5639>(perm, input);
      case 740: return static_permuted_radical_inverse<T, 5641>(perm, input);
      case 741: return static_permuted_radical_inverse<T, 5647>(perm, input);
      case 742: return static_permuted_radical_inverse<T, 5651>(perm, input);
      case 743: return static_permuted_radical_inverse<T, 5653>(perm, input);
      case 744: return static_permuted_radical_inverse<T, 5657>(perm, input);
      case 745: return static_permuted_radical_inverse<T, 5659>(perm, input);
      case 746: return static_permuted_radical_inverse<T, 5669>(perm, input);
      case 747: return static_permuted_radical_inverse<T, 5683>(perm, input);
      case 748: return static_permuted_radical_inverse<T, 5689>(perm, input);
      case 749: return static_permuted_radical_inverse<T, 5693>(perm, input);
      case 750: return static_permuted_radical_inverse<T, 5701>(perm, input);
      case 751: return static_permuted_radical_inverse<T, 5711>(perm, input);
      case 752: return static_permuted_radical_inverse<T, 5717>(perm, input);
      case 753: return static_permuted_radical_inverse<T, 5737>(perm, input);
      case 754: return static_permuted_radical_inverse<T, 5741>(perm, input);
      case 755: return static_permuted_radical_inverse<T, 5743>(perm, input);
      case 756: return static_permuted_radical_inverse<T, 5749>(perm, input);
      case 757: return static_permuted_radical_inverse<T, 5779>(perm, input);
      case 758: return static_permuted_radical_inverse<T, 5783>(perm, input);
      case 759: return static_permuted_radical_inverse<T, 5791>(perm, input);
      case 760: return static_permuted_radical_inverse<T, 5801>(perm, input);
      case 761: return static_permuted_radical_inverse<T, 5807>(perm, input);
      case 762: return static_permuted_radical_inverse<T, 5813>(perm, input);
      case 763: return static_permuted_radical_inverse<T, 5821>(perm, input);
      case 764: return static_permuted_radical_inverse<T, 5827>(perm, input);
      case 765: return static_permuted_radical_inverse<T, 5839>(perm, input);
      case 766: return static_permuted_radical_inverse<T, 5843>(perm, input);
      case 767: return static_permuted_radical_inverse<T, 5849>(perm, input);
      case 768: return static_permuted_radical_inverse<T, 5851>(perm, input);
      case 769: return static_permuted_radical_inverse<T, 5857>(perm, input);
      case 770: return static_permuted_radical_inverse<T, 5861>(perm, input);
      case 771: return static_permuted_radical_inverse<T, 5867>(perm, input);
      case 772: return static_permuted_radical_inverse<T, 5869>(perm, input);
      case 773: return static_permuted_radical_inverse<T, 5879>(perm, input);
      case 774: return static_permuted_radical_inverse<T, 5881>(perm, input);
      case 775: return static_permuted_radical_inverse<T, 5897>(perm, input);
      case 776: return static_permuted_radical_inverse<T, 5903>(perm, input);
      case 777: return static_permuted_radical_inverse<T, 5923>(perm, input);
      case 778: return static_permuted_radical_inverse<T, 5927>(perm, input);
      case 779: return static_permuted_radical_inverse<T, 5939>(perm, input);
      case 780: return static_permuted_radical_inverse<T, 5953>(perm, input);
      case 781: return static_permuted_radical_inverse<T, 5981>(perm, input);
      case 782: return static_permuted_radical_inverse<T, 5987>(perm, input);
      case 783: return static_permuted_radical_inverse<T, 6007>(perm, input);
      case 784: return static_permuted_radical_inverse<T, 6011>(perm, input);
      case 785: return static_permuted_radical_inverse<T, 6029>(perm, input);
      case 786: return static_permuted_radical_inverse<T, 6037>(perm, input);
      case 787: return static_permuted_radical_inverse<T, 6043>(perm, input);
      case 788: return static_permuted_radical_inverse<T, 6047>(perm, input);
      case 789: return static_permuted_radical_inverse<T, 6053>(perm, input);
      case 790: return static_permuted_radical_inverse<T, 6067>(perm, input);
      case 791: return static_permuted_radical_inverse<T, 6073>(perm, input);
      case 792: return static_permuted_radical_inverse<T, 6079>(perm, input);
      case 793: return static_permuted_radical_inverse<T, 6089>(perm, input);
      case 794: return static_permuted_radical_inverse<T, 6091>(perm, input);
      case 795: return static_permuted_radical_inverse<T, 6101>(perm, input);
      case 796: return static_permuted_radical_inverse<T, 6113>(perm, input);
      case 797: return static_permuted_radical_inverse<T, 6121>(perm, input);
      case 798: return static_permuted_radical_inverse<T, 6131>(perm, input);
      case 799: return static_permuted_radical_inverse<T, 6133>(perm, input);
      case 800: return static_permuted_radical_inverse<T, 6143>(perm, input);
      case 801: return static_permuted_radical_inverse<T, 6151>(perm, input);
      case 802: return static_permuted_radical_inverse<T, 6163>(perm, input);
      case 803: return static_permuted_radical_inverse<T, 6173>(perm, input);
      case 804: return static_permuted_radical_inverse<T, 6197>(perm, input);
      case 805: return static_permuted_radical_inverse<T, 6199>(perm, input);
      case 806: return static_permuted_radical_inverse<T, 6203>(perm, input);
      case 807: return static_permuted_radical_inverse<T, 6211>(perm, input);
      case 808: return static_permuted_radical_inverse<T, 6217>(perm, input);
      case 809: return static_permuted_radical_inverse<T, 6221>(perm, input);
      case 810: return static_permuted_radical_inverse<T, 6229>(perm, input);
      case 811: return static_permuted_radical_inverse<T, 6247>(perm, input);
      case 812: return static_permuted_radical_inverse<T, 6257>(perm, input);
      case 813: return static_permuted_radical_inverse<T, 6263>(perm, input);
      case 814: return static_permuted_radical_inverse<T, 6269>(perm, input);
      case 815: return static_permuted_radical_inverse<T, 6271>(perm, input);
      case 816: return static_permuted_radical_inverse<T, 6277>(perm, input);
      case 817: return static_permuted_radical_inverse<T, 6287>(perm, input);
      case 818: return static_permuted_radical_inverse<T, 6299>(perm, input);
      case 819: return static_permuted_radical_inverse<T, 6301>(perm, input);
      case 820: return static_permuted_radical_inverse<T, 6311>(perm, input);
      case 821: return static_permuted_radical_inverse<T, 6317>(perm, input);
      case 822: return static_permuted_radical_inverse<T, 6323>(perm, input);
      case 823: return static_permuted_radical_inverse<T, 6329>(perm, input);
      case 824: return static_permuted_radical_inverse<T, 6337>(perm, input);
      case 825: return static_permuted_radical_inverse<T, 6343>(perm, input);
      case 826: return static_permuted_radical_inverse<T, 6353>(perm, input);
      case 827: return static_permuted_radical_inverse<T, 6359>(perm, input);
      case 828: return static_permuted_radical_inverse<T, 6361>(perm, input);
      case 829: return static_permuted_radical_inverse<T, 6367>(perm, input);
      case 830: return static_permuted_radical_inverse<T, 6373>(perm, input);
      case 831: return static_permuted_radical_inverse<T, 6379>(perm, input);
      case 832: return static_permuted_radical_inverse<T, 6389>(perm, input);
      case 833: return static_permuted_radical_inverse<T, 6397>(perm, input);
      case 834: return static_permuted_radical_inverse<T, 6421>(perm, input);
      case 835: return static_permuted_radical_inverse<T, 6427>(perm, input);
      case 836: return static_permuted_radical_inverse<T, 6449>(perm, input);
      case 837: return static_permuted_radical_inverse<T, 6451>(perm, input);
      case 838: return static_permuted_radical_inverse<T, 6469>(perm, input);
      case 839: return static_permuted_radical_inverse<T, 6473>(perm, input);
      case 840: return static_permuted_radical_inverse<T, 6481>(perm, input);
      case 841: return static_permuted_radical_inverse<T, 6491>(perm, input);
      case 842: return static_permuted_radical_inverse<T, 6521>(perm, input);
      case 843: return static_permuted_radical_inverse<T, 6529>(perm, input);
      case 844: return static_permuted_radical_inverse<T, 6547>(perm, input);
      case 845: return static_permuted_radical_inverse<T, 6551>(perm, input);
      case 846: return static_permuted_radical_inverse<T, 6553>(perm, input);
      case 847: return static_permuted_radical_inverse<T, 6563>(perm, input);
      case 848: return static_permuted_radical_inverse<T, 6569>(perm, input);
      case 849: return static_permuted_radical_inverse<T, 6571>(perm, input);
      case 850: return static_permuted_radical_inverse<T, 6577>(perm, input);
      case 851: return static_permuted_radical_inverse<T, 6581>(perm, input);
      case 852: return static_permuted_radical_inverse<T, 6599>(perm, input);
      case 853: return static_permuted_radical_inverse<T, 6607>(perm, input);
      case 854: return static_permuted_radical_inverse<T, 6619>(perm, input);
      case 855: return static_permuted_radical_inverse<T, 6637>(perm, input);
      case 856: return static_permuted_radical_inverse<T, 6653>(perm, input);
      case 857: return static_permuted_radical_inverse<T, 6659>(perm, input);
      case 858: return static_permuted_radical_inverse<T, 6661>(perm, input);
      case 859: return static_permuted_radical_inverse<T, 6673>(perm, input);
      case 860: return static_permuted_radical_inverse<T, 6679>(perm, input);
      case 861: return static_permuted_radical_inverse<T, 6689>(perm, input);
      case 862: return static_permuted_radical_inverse<T, 6691>(perm, input);
      case 863: return static_permuted_radical_inverse<T, 6701>(perm, input);
      case 864: return static_permuted_radical_inverse<T, 6703>(perm, input);
      case 865: return static_permuted_radical_inverse<T, 6709>(perm, input);
      case 866: return static_permuted_radical_inverse<T, 6719>(perm, input);
      case 867: return static_permuted_radical_inverse<T, 6733>(perm, input);
      case 868: return static_permuted_radical_inverse<T, 6737>(perm, input);
      case 869: return static_permuted_radical_inverse<T, 6761>(perm, input);
      case 870: return static_permuted_radical_inverse<T, 6763>(perm, input);
      case 871: return static_permuted_radical_inverse<T, 6779>(perm, input);
      case 872: return static_permuted_radical_inverse<T, 6781>(perm, input);
      case 873: return static_permuted_radical_inverse<T, 6791>(perm, input);
      case 874: return static_permuted_radical_inverse<T, 6793>(perm, input);
      case 875: return static_permuted_radical_inverse<T, 6803>(perm, input);
      case 876: return static_permuted_radical_inverse<T, 6823>(perm, input);
      case 877: return static_permuted_radical_inverse<T, 6827>(perm, input);
      case 878: return static_permuted_radical_inverse<T, 6829>(perm, input);
      case 879: return static_permuted_radical_inverse<T, 6833>(perm, input);
      case 880: return static_permuted_radical_inverse<T, 6841>(perm, input);
      case 881: return static_permuted_radical_inverse<T, 6857>(perm, input);
      case 882: return static_permuted_radical_inverse<T, 6863>(perm, input);
      case 883: return static_permuted_radical_inverse<T, 6869>(perm, input);
      case 884: return static_permuted_radical_inverse<T, 6871>(perm, input);
      case 885: return static_permuted_radical_inverse<T, 6883>(perm, input);
      case 886: return static_permuted_radical_inverse<T, 6899>(perm, input);
      case 887: return static_permuted_radical_inverse<T, 6907>(perm, input);
      case 888: return static_permuted_radical_inverse<T, 6911>(perm, input);
      case 889: return static_permuted_radical_inverse<T, 6917>(perm, input);
      case 890: return static_permuted_radical_inverse<T, 6947>(perm, input);
      case 891: return static_permuted_radical_inverse<T, 6949>(perm, input);
      case 892: return static_permuted_radical_inverse<T, 6959>(perm, input);
      case 893: return static_permuted_radical_inverse<T, 6961>(perm, input);
      case 894: return static_permuted_radical_inverse<T, 6967>(perm, input);
      case 895: return static_permuted_radical_inverse<T, 6971>(perm, input);
      case 896: return static_permuted_radical_inverse<T, 6977>(perm, input);
      case 897: return static_permuted_radical_inverse<T, 6983>(perm, input);
      case 898: return static_permuted_radical_inverse<T, 6991>(perm, input);
      case 899: return static_permuted_radical_inverse<T, 6997>(perm, input);
      case 900: return static_permuted_radical_inverse<T, 7001>(perm, input);
      case 901: return static_permuted_radical_inverse<T, 7013>(perm, input);
      case 902: return static_permuted_radical_inverse<T, 7019>(perm, input);
      case 903: return static_permuted_radical_inverse<T, 7027>(perm, input);
      case 904: return static_permuted_radical_inverse<T, 7039>(perm, input);
      case 905: return static_permuted_radical_inverse<T, 7043>(perm, input);
      case 906: return static_permuted_radical_inverse<T, 7057>(perm, input);
      case 907: return static_permuted_radical_inverse<T, 7069>(perm, input);
      case 908: return static_permuted_radical_inverse<T, 7079>(perm, input);
      case 909: return static_permuted_radical_inverse<T, 7103>(perm, input);
      case 910: return static_permuted_radical_inverse<T, 7109>(perm, input);
      case 911: return static_permuted_radical_inverse<T, 7121>(perm, input);
      case 912: return static_permuted_radical_inverse<T, 7127>(perm, input);
      case 913: return static_permuted_radical_inverse<T, 7129>(perm, input);
      case 914: return static_permuted_radical_inverse<T, 7151>(perm, input);
      case 915: return static_permuted_radical_inverse<T, 7159>(perm, input);
      case 916: return static_permuted_radical_inverse<T, 7177>(perm, input);
      case 917: return static_permuted_radical_inverse<T, 7187>(perm, input);
      case 918: return static_permuted_radical_inverse<T, 7193>(perm, input);
      case 919: return static_permuted_radical_inverse<T, 7207>(perm, input);
      case 920: return static_permuted_radical_inverse<T, 7211>(perm, input);
      case 921: return static_permuted_radical_inverse<T, 7213>(perm, input);
      case 922: return static_permuted_radical_inverse<T, 7219>(perm, input);
      case 923: return static_permuted_radical_inverse<T, 7229>(perm, input);
      case 924: return static_permuted_radical_inverse<T, 7237>(perm, input);
      case 925: return static_permuted_radical_inverse<T, 7243>(perm, input);
      case 926: return static_permuted_radical_inverse<T, 7247>(perm, input);
      case 927: return static_permuted_radical_inverse<T, 7253>(perm, input);
      case 928: return static_permuted_radical_inverse<T, 7283>(perm, input);
      case 929: return static_permuted_radical_inverse<T, 7297>(perm, input);
      case 930: return static_permuted_radical_inverse<T, 7307>(perm, input);
      case 931: return static_permuted_radical_inverse<T, 7309>(perm, input);
      case 932: return static_permuted_radical_inverse<T, 7321>(perm, input);
      case 933: return static_permuted_radical_inverse<T, 7331>(perm, input);
      case 934: return static_permuted_radical_inverse<T, 7333>(perm, input);
      case 935: return static_permuted_radical_inverse<T, 7349>(perm, input);
      case 936: return static_permuted_radical_inverse<T, 7351>(perm, input);
      case 937: return static_permuted_radical_inverse<T, 7369>(perm, input);
      case 938: return static_permuted_radical_inverse<T, 7393>(perm, input);
      case 939: return static_permuted_radical_inverse<T, 7411>(perm, input);
      case 940: return static_permuted_radical_inverse<T, 7417>(perm, input);
      case 941: return static_permuted_radical_inverse<T, 7433>(perm, input);
      case 942: return static_permuted_radical_inverse<T, 7451>(perm, input);
      case 943: return static_permuted_radical_inverse<T, 7457>(perm, input);
      case 944: return static_permuted_radical_inverse<T, 7459>(perm, input);
      case 945: return static_permuted_radical_inverse<T, 7477>(perm, input);
      case 946: return static_permuted_radical_inverse<T, 7481>(perm, input);
      case 947: return static_permuted_radical_inverse<T, 7487>(perm, input);
      case 948: return static_permuted_radical_inverse<T, 7489>(perm, input);
      case 949: return static_permuted_radical_inverse<T, 7499>(perm, input);
      case 950: return static_permuted_radical_inverse<T, 7507>(perm, input);
      case 951: return static_permuted_radical_inverse<T, 7517>(perm, input);
      case 952: return static_permuted_radical_inverse<T, 7523>(perm, input);
      case 953: return static_permuted_radical_inverse<T, 7529>(perm, input);
      case 954: return static_permuted_radical_inverse<T, 7537>(perm, input);
      case 955: return static_permuted_radical_inverse<T, 7541>(perm, input);
      case 956: return static_permuted_radical_inverse<T, 7547>(perm, input);
      case 957: return static_permuted_radical_inverse<T, 7549>(perm, input);
      case 958: return static_permuted_radical_inverse<T, 7559>(perm, input);
      case 959: return static_permuted_radical_inverse<T, 7561>(perm, input);
      case 960: return static_permuted_radical_inverse<T, 7573>(perm, input);
      case 961: return static_permuted_radical_inverse<T, 7577>(perm, input);
      case 962: return static_permuted_radical_inverse<T, 7583>(perm, input);
      case 963: return static_permuted_radical_inverse<T, 7589>(perm, input);
      case 964: return static_permuted_radical_inverse<T, 7591>(perm, input);
      case 965: return static_permuted_radical_inverse<T, 7603>(perm, input);
      case 966: return static_permuted_radical_inverse<T, 7607>(perm, input);
      case 967: return static_permuted_radical_inverse<T, 7621>(perm, input);
      case 968: return static_permuted_radical_inverse<T, 7639>(perm, input);
      case 969: return static_permuted_radical_inverse<T, 7643>(perm, input);
      case 970: return static_permuted_radical_inverse<T, 7649>(perm, input);
      case 971: return static_permuted_radical_inverse<T, 7669>(perm, input);
      case 972: return static_permuted_radical_inverse<T, 7673>(perm, input);
      case 973: return static_permuted_radical_inverse<T, 7681>(perm, input);
      case 974: return static_permuted_radical_inverse<T, 7687>(perm, input);
      case 975: return static_permuted_radical_inverse<T, 7691>(perm, input);
      case 976: return static_permuted_radical_inverse<T, 7699>(perm, input);
      case 977: return static_permuted_radical_inverse<T, 7703>(perm, input);
      case 978: return static_permuted_radical_inverse<T, 7717>(perm, input);
      case 979: return static_permuted_radical_inverse<T, 7723>(perm, input);
      case 980: return static_permuted_radical_inverse<T, 7727>(perm, input);
      case 981: return static_permuted_radical_inverse<T, 7741>(perm, input);
      case 982: return static_permuted_radical_inverse<T, 7753>(perm, input);
      case 983: return static_permuted_radical_inverse<T, 7757>(perm, input);
      case 984: return static_permuted_radical_inverse<T, 7759>(perm, input);
      case 985: return static_permuted_radical_inverse<T, 7789>(perm, input);
      case 986: return static_permuted_radical_inverse<T, 7793>(perm, input);
      case 987: return static_permuted_radical_inverse<T, 7817>(perm, input);
      case 988: return static_permuted_radical_inverse<T, 7823>(perm, input);
      case 989: return static_permuted_radical_inverse<T, 7829>(perm, input);
      case 990: return static_permuted_radical_inverse<T, 7841>(perm, input);
      case 991: return static_permuted_radical_inverse<T, 7853>(perm, input);
      case 992: return static_permuted_radical_inverse<T, 7867>(perm, input);
      case 993: return static_permuted_radical_inverse<T, 7873>(perm, input);
      case 994: return static_permuted_radical_inverse<T, 7877>(perm, input);
      case 995: return static_permuted_radical_inverse<T, 7879>(perm, input);
      case 996: return static_permuted_radical_inverse<T, 7883>(perm, input);
      case 997: return static_permuted_radical_inverse<T, 7901>(perm, input);
      case 998: return static_permuted_radical_inverse<T, 7907>(perm, input);
      case 999: return static_permuted_radical_inverse<T, 7919>(perm, input);
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
    const size_t        input)
{
    Vector<T, Dim> p;

    for (size_t i = 0; i < Dim; ++i)
    {
        const size_t base = bases[i];

        p[i] =
            base == 2
                ? radical_inverse_base2<T>(input)
                : radical_inverse<T>(base, input);
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_sequence(
    const size_t        bases[],
    const size_t        perms[],
    const size_t        input)
{
    Vector<T, Dim> p;

    for (size_t i = 0; i < Dim; ++i)
    {
        const size_t base = bases[i];

        p[i] =
            base == 2
                ? radical_inverse_base2<T>(input)
                : permuted_radical_inverse<T>(base, perms, input);

        perms += base;
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> halton_zaremba_sequence(
    const size_t        bases[],
    const size_t        input)
{
    Vector<T, Dim> p;

    for (size_t i = 0; i < Dim; ++i)
    {
        const size_t base = bases[i];

        p[i] =
            base == 2
                ? folded_radical_inverse_base2<T>(input)
                : folded_radical_inverse<T>(base, input);
    }

    return p;
}


//
// Hammersley sequences implementation.
//

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],
    const size_t        input,
    const size_t        count)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(input) / count;

    for (size_t i = 1; i < Dim; ++i)
    {
        const size_t base = bases[i - 1];

        p[i] =
            base == 2
                ? radical_inverse_base2<T>(input)
                : radical_inverse<T>(base, input);
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_sequence(
    const size_t        bases[],
    const size_t        perms[],
    const size_t        input,
    const size_t        count)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(input) / count;

    for (size_t i = 1; i < Dim; ++i)
    {
        const size_t base = bases[i - 1];

        p[i] =
            base == 2
                ? radical_inverse_base2<T>(input)
                : permuted_radical_inverse<T>(base, perms, input);

        perms += base;
    }

    return p;
}

template <typename T, size_t Dim>
inline Vector<T, Dim> hammersley_zaremba_sequence(
    const size_t        bases[],
    const size_t        input,
    const size_t        count)
{
    Vector<T, Dim> p;

    p[0] = static_cast<T>(input) / count;

    for (size_t i = 1; i < Dim; ++i)
    {
        const size_t base = bases[i - 1];

        p[i] =
            base == 2
                ? folded_radical_inverse_base2<T>(input)
                : folded_radical_inverse<T>(bases[i - 1], input);
    }

    return p;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_QMC_H
