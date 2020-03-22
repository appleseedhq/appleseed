//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Gray Olson, The appleseedhq Organization
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

#version 420

uniform sampler2D u_accum_tex;
uniform sampler2D u_revealage_tex;

out vec4 Target0;

float max4(vec4 v)
{
    return max(max(max(v.x, v.y), v.z), v.w);
}

void main()
{
    const ivec2 coord = ivec2(gl_FragCoord.xy);

    const float revealage = texelFetch(u_revealage_tex, coord, 0).r;

    vec4 accum = texelFetch(u_accum_tex, coord, 0);

    // Suppress overflow.
    if (isinf(max4(abs(accum))))
    {
        accum.rgb = vec3(accum.a);
    }

    const vec3 averageColor = accum.rgb / max(accum.a, 0.00001);

    Target0 = vec4(averageColor, 1.0 - revealage);
}
