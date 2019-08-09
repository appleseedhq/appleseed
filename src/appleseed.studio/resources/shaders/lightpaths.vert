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

const float AA_BUFFER_SIZE = 1.0;

layout(location = 0) in vec3 v_previous;
layout(location = 1) in vec3 v_position;
layout(location = 2) in vec3 v_next;
layout(location = 3) in float v_luminance;
layout(location = 4) in int v_direction_start_end;
layout(location = 5) in vec3 v_color;
layout(location = 6) in vec3 v_surface_normal;

uniform mat4 u_proj;
uniform mat4 u_view;
uniform vec2 u_res;
uniform float u_max_luminance;
uniform float u_max_thickness;
uniform float u_min_thickness;

uniform int u_first_selected;
uniform int u_last_selected;

flat out vec4 f_color;
out float f_aa_norm;
flat out float f_thickness;
flat out float f_total_thickness;
flat out float f_aspect_expansion_len;

void main() {
    float aspect = u_res.x / u_res.y;
    vec2 aspect_vec = vec2(aspect, 1.0);
    mat4 vp = u_proj * u_view;
    vec4 prev_proj = vp * vec4(v_previous, 1.0);
    vec4 curr_proj = vp * vec4(v_position, 1.0);
    vec4 next_proj = vp * vec4(v_next, 1.0);

    //get 2D screen space with W divide and aspect correction
    vec2 curr_screen = curr_proj.xy / curr_proj.w * aspect_vec;
    vec2 prev_screen = prev_proj.xy / prev_proj.w * aspect_vec;
    vec2 next_screen = next_proj.xy / next_proj.w * aspect_vec;

    float orientation = 1.0;
    if ((v_direction_start_end & 1) == 1)
    {
        orientation = -1.0;
    }

    vec2 dir = vec2(0.0);
    if ((v_direction_start_end & 2) == 2)
    {
        //starting point uses (next - current)
        dir = normalize(next_screen - curr_screen);
    } 
    else if ((v_direction_start_end & 4) == 4)
    {
        //ending point uses (current - v_previous)
        dir = normalize(curr_screen - prev_screen);
    }
    vec2 perp_dir = vec2(-dir.y, dir.x);

    vec4 normal_clip = vp * vec4(v_surface_normal, 0.0);
    normal_clip.xy *= aspect_vec;
    normal_clip = normalize(normal_clip);
    vec2 tang_clip = vec2(-normal_clip.y, normal_clip.x);

    float tdp = dot(tang_clip, perp_dir);
    vec2 expansion = perp_dir;
    if (tdp > 0.05)
        expansion = tang_clip / tdp;
    
    vec2 norm_exp = normalize(expansion);
    vec2 res_exp_dir = vec2(norm_exp.x * u_res.x, norm_exp.y * u_res.y);
    f_aspect_expansion_len = length(res_exp_dir);

    f_thickness = (max(max(min(v_luminance / u_max_luminance, 1.0), 0.0) * u_max_thickness, u_min_thickness) / 2.0) / f_aspect_expansion_len;

    f_total_thickness = f_thickness + AA_BUFFER_SIZE / f_aspect_expansion_len;

    expansion *= f_total_thickness;
    expansion *= orientation;

    gl_Position = vec4((curr_screen + expansion) / aspect_vec, curr_proj.z / curr_proj.w, 1.0);
    f_aa_norm = orientation;

    bool is_selected = gl_VertexID >= u_first_selected && gl_VertexID < u_last_selected;
    float a = is_selected ? 1.0 : 0.2;
    f_color = vec4(v_color, a);
}
