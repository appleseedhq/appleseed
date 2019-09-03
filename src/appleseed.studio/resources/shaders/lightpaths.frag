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

#version 410
#extension GL_ARB_separate_shader_objects : enable

flat in vec3 f_color;
in float f_aa_norm;
flat in float f_thickness;
flat in float f_total_thickness;
flat in float f_aspect_expansion_len;

uniform vec2 u_res;

layout(location = 0) out vec4 accum_target;
layout(location = 1) out float revealage_target;

void write_pixel(vec4 premultiplied_col, vec3 transmit) { 
    // premultiplied_col.a *= 1.0 - clamp((transmit.r + transmit.g + transmit.b) * (1.0 / 3.0), 0, 1);
 
    float a = min(1.0, premultiplied_col.a) * 8.0 + 0.01;
    float b = -gl_FragCoord.z * 0.95 + 1.0;
 
    float w = clamp(a * a * a * 1e8 * b * b * b, 1e-2, 3e2);

    accum_target = premultiplied_col * w;
    revealage_target = premultiplied_col.a;
}
void main()
{
    float dist = abs(f_aa_norm) * f_total_thickness - 0.5 / f_aspect_expansion_len;
    float eps = fwidth(dist);
    float a = 1.0 - smoothstep(f_thickness - eps, f_thickness + eps, dist);

    vec4 premult = vec4(f_color * a, a);
    write_pixel(premult, vec3(1.0));
}
