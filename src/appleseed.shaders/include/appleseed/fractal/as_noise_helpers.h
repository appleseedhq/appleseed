
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Luis Barrancos, The appleseedhq Organization
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

// Taken from noises.h, slightly tweaked for OSL

/************************************************************************
 * noises.h - various noise-based patterns
 *
 * Author: Larry Gritz (gritzl@acm.org)
 *
 * Reference:
 *   _Advanced RenderMan: Creating CGI for Motion Picture_, 
 *   by Anthony A. Apodaca and Larry Gritz, Morgan Kaufmann, 1999.
 *
 * $Revision: 1.1 $    $Date: 2004/03/02 04:50:34 $
 *
 ************************************************************************/

#ifndef AS_NOISE_HELPERS_H
#define AS_NOISE_HELPERS_H

#define filtered_noise(p, filter_width)                                 \
    (noise(p) * (1 - smoothstep(0.2, 0.75, filter_width)))

#define filtered_snoise(p, filter_width)                                \
    (snoise(p) * (1 - smoothstep(0.2, 0.75, filter_width)))

// Variable lacunarity noise.
#define vlnoise(p, scale)                                               \
    (snoise((vector) snoise(p) * scale + p))

#define filtered_vlnoise(p, scale)                                      \
    (filtered_snoise((vector) noise(p) *                                \
    (1 - smoothstep(0.2, 0.75, filter_width)) * scale + p,              \
    filter_width)

#endif // AS_NOISE_HELPERS_H
