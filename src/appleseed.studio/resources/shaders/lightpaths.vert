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

const float AA_BUFFER_SIZE = 1.0;

layout(location = 0) in vec3 v_previous;
layout(location = 1) in vec3 v_position;
layout(location = 2) in vec3 v_next;
layout(location = 3) in int v_bitmask; // flag defining which part of a light path is currently drawn.
layout(location = 4) in vec3 v_color;
layout(location = 5) in vec3 v_surface_normal;

uniform mat4 u_proj;
uniform mat4 u_view;
uniform vec2 u_res;  // resolution of the frame.

uniform int u_first_selected;
uniform int u_last_selected;

flat out vec4 f_color;
out float f_aa_norm;
flat out float f_thickness;
flat out float f_total_thickness;
flat out float f_aspect_expansion_len;

// Light path vertex world space position is
// moved away from the surface to avoid clipping.
const float CLIPPING_PREVENTION_FACTOR = 0.0001;

// Light path thickness in screen space.
const float LINE_THICKNESS = 1.0;

const float SELECTED_PATH_TRANSPARENCY = 1.0;
const float NON_SELECTED_PATH_TRANSPARENCY = 0.05;

//
// Reference:
// - https://mattdesl.svbtle.com/drawing-lines-is-hard#screenspace-projected-lines_2
//

// Compute current line direction
// depending on which part of the path we
// are drawing (start, middle or end).
vec2 get_line_direction(vec2 curr_screen, vec2 prev_screen, vec2 next_screen)
{
    return
        (v_bitmask & 2) == 2
            ? normalize(curr_screen - prev_screen)
            : normalize(next_screen - curr_screen);
}

// Each point on the light path is duplicated to render a real line.
// The duplicated vertex of each light path point is flagged.
bool is_second_point()
{
    return ((v_bitmask & 1) == 1);
}

void main()
{
    // Aspect ratio correction is applied on
    // screen points (only on the X axis).
    const vec2 aspect_correction = vec2(u_res.x / u_res.y, 1.0);

    // Project points.
    // The currently drawn point is offset
    // from the surface to ensure path
    // doesn't go through it.
    const mat4 vp = u_proj * u_view;
    const vec4 curr_proj = vp * vec4(v_position + v_surface_normal * CLIPPING_PREVENTION_FACTOR, 1.0);
    const vec4 prev_proj = vp * vec4(v_previous, 1.0);
    const vec4 next_proj = vp * vec4(v_next, 1.0);

    // Project points in screenspace and apply aspect ratio correction.
    const vec2 curr_screen = (curr_proj.xy / curr_proj.w) * aspect_correction;
    const vec2 prev_screen = (prev_proj.xy / prev_proj.w) * aspect_correction;
    const vec2 next_screen = (next_proj.xy / next_proj.w) * aspect_correction;

    const bool is_second_point = is_second_point();

    const vec2 line_direction = get_line_direction(curr_screen, prev_screen, next_screen);

    // Compute the normal of the line.
    const vec2 line_normal = vec2(-line_direction.y, line_direction.x);

    vec4 normal_clip = vp * vec4(v_surface_normal, 0.0);
    normal_clip.x *= aspect_correction.x;
    normal_clip = normalize(normal_clip);

    const vec2 tang_clip = vec2(-normal_clip.y, normal_clip.x);

    const float tdp = dot(tang_clip, line_normal);
    // it uses tangent to surface unless the tangent to surface is too much aligned with the line direction,
    // in which case it uses the line normal as fallback. it does that to reduce the amount of 'pokethrough' of the lines through the geometry.
    // if you always expand along line normal then depending on the angle of the line and camera the edges of the rectangle will poke through i the backside of the surface
    vec2 expansion =
        tdp > 0.05
            ? tang_clip / tdp
            : line_normal;

    const vec2 norm_exp = normalize(expansion);
    const vec2 res_exp_line_direction = vec2(norm_exp.x * u_res.x, norm_exp.y * u_res.y);
    f_aspect_expansion_len = length(res_exp_line_direction);

    const float half_thickness = LINE_THICKNESS * 0.5;
    f_thickness = half_thickness / f_aspect_expansion_len;

    f_total_thickness = f_thickness + AA_BUFFER_SIZE / f_aspect_expansion_len;

    expansion *= f_total_thickness;

    // Reverse expansion line_directionection for the duplicated point.
    if (is_second_point) expansion *= -1.0;

    vec2 screen_pos = curr_screen + expansion;
    screen_pos /= aspect_correction;
    gl_Position = vec4(screen_pos * curr_proj.w, curr_proj.z /* curr_proj.w*/, curr_proj.w);
    f_aa_norm = is_second_point ? 1.0 : -1.0;

    const bool is_selected = gl_VertexID >= u_first_selected && gl_VertexID < u_last_selected;

    const float alpha =
        is_selected
            ? SELECTED_PATH_TRANSPARENCY
            : NON_SELECTED_PATH_TRANSPARENCY;

    f_color = vec4(v_color, alpha);
}
