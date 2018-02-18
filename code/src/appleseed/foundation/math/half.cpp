
// This code was taken from Ptex with minimal changes.
// Original license follows:

/*
PTEX SOFTWARE
Copyright 2014 Disney Enterprises, Inc.  All rights reserved

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

  * The names "Disney", "Walt Disney Pictures", "Walt Disney Animation
    Studios" or the names of its contributors may NOT be used to
    endorse or promote products derived from this software without
    specific prior written permission from Walt Disney Pictures.

Disclaimer: THIS SOFTWARE IS PROVIDED BY WALT DISNEY PICTURES AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE, NONINFRINGEMENT AND TITLE ARE DISCLAIMED.
IN NO EVENT SHALL WALT DISNEY PICTURES, THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND BASED ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/

// Interface header.
#include "half.h"
#include "halftables.h"

// Standard headers.
#include <cmath>

using namespace std;

namespace foundation
{

//
// Half class implementation.
//

uint16_t Half::from_float_except(const uint32_t i)
{
    uint32_t s = ((i>>16) & 0x8000);
    int32_t e = ((i>>13) & 0x3fc00) - 0x1c000;

    if (e <= 0)
    {
        // Denormalized.
        union { uint32_t i; float f; } u;
        u.i = i;

        return static_cast<uint16_t>(
            s | static_cast<int>(fabs(u.f) * 1.6777216e7 + 0.5));
    }

    if (e == 0x23c00)
        // inf/nan, preserve msb bits of m for nan code
        return static_cast<uint16_t>(s | 0x7c00 | ((i & 0x7fffff) >> 13));
    else
        // overflow - convert to inf
        return static_cast<uint16_t>(s | 0x7c00);
}

}   // namespace foundation
