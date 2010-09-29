
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_SSE_H
#define APPLESEED_FOUNDATION_PLATFORM_SSE_H

#ifndef APPLESEED_FOUNDATION_USE_SSE
#error SSE support not enabled.
#endif

// Platform headers.
#include <xmmintrin.h>      // SSE1 intrinsics
#include <emmintrin.h>      // SSE2 intrinsics

namespace foundation
{

//
// SSE vector types.
//

// Four single-precision values.
typedef __m128  sse4f;

// Two double-precision values.
typedef __m128d sse2d;


//
// Shortcuts for SSE intrinsics.
//

// Single-precision packet instructions.
#define addps           _mm_add_ps
#define andps           _mm_and_ps
#define andnotps        _mm_andnot_ps
#define cmpgeps         _mm_cmpge_ps
#define cmpgtps         _mm_cmpgt_ps
#define cmpleps         _mm_cmple_ps
#define cmpltps         _mm_cmplt_ps
#define divps           _mm_div_ps
#define loadps          _mm_load_ps
#define loadups         _mm_loadu_ps
#define load1ps         _mm_load1_ps
#define maxss           _mm_max_ss
#define maxps           _mm_max_ps
#define minss           _mm_min_ss
#define minps           _mm_min_ps
#define movemaskps      _mm_movemask_ps
#define mulps           _mm_mul_ps
#define orps            _mm_or_ps
#define setss           _mm_set_ss
#define setps           _mm_set_ps
#define set1ps          _mm_set1_ps
#define shuffleps       _mm_shuffle_ps
#define storess         _mm_store_ss
#define storeps         _mm_store_ps
#define storeups        _mm_storeu_ps
#define subps           _mm_sub_ps

// Double-precision packet instructions.
#define addpd           _mm_add_pd
#define andpd           _mm_and_pd
#define andnotpd        _mm_andnot_pd
#define cmpgepd         _mm_cmpge_pd
#define cmpgtpd         _mm_cmpgt_pd
#define cmplepd         _mm_cmple_pd
#define cmpltpd         _mm_cmplt_pd
#define divpd           _mm_div_pd
#define loadpd          _mm_load_pd
#define loadupd         _mm_loadu_pd
#define load1pd         _mm_load1_pd
#define maxsd           _mm_max_sd
#define maxpd           _mm_max_pd
#define minsd           _mm_min_sd
#define minpd           _mm_min_pd
#define movemaskpd      _mm_movemask_pd
#define mulpd           _mm_mul_pd
#define orpd            _mm_or_pd
#define setsd           _mm_set_sd
#define setpd           _mm_set_pd
#define set1pd          _mm_set1_pd
#define shufflepd       _mm_shuffle_pd
#define storesd         _mm_store_sd
#define storepd         _mm_store_pd
#define storeupd        _mm_storeu_pd
#define subpd           _mm_sub_pd


//
// 4-way SSE implementation of std::floor().
//
// Reference:
//
//   http://www.masm32.com/board/index.php?topic=9515.msg78719#msg78719
//

#define FLOOR_SSE(x)                                    \
    _mm_cvtepi32_ps(                                    \
        _mm_sub_epi32(                                  \
            _mm_cvttps_epi32(x),                        \
            _mm_srli_epi32(_mm_castps_si128(x), 31)))

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_SSE_H
