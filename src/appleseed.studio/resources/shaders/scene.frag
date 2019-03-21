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

#version 330
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0) in vec3 v_world_pos;
layout(location = 1) in vec3 v_norm;

uniform vec3 u_camera_pos;

const vec3 LIGHT_POS = vec3(0.0, 0.0, 0.0);
const vec3 LIGHT_COLOR = vec3(1.0);
const vec3 AMBIENT_COLOR = vec3(0.05);

const vec3 MATERIAL_BASE_COLOR = vec3(0.8, 0.8, 0.8);
const float MATERIAL_DIFFUSE_FACTOR = 0.4;
const float MATERIAL_SPECULAR_FACTOR = 0.15;
const float MATERIAL_METALNESS_FACTOR = 0.0;
const float MATERIAL_SHININESS = 80.0;

out vec4 Target0;

void main()
{
    vec3 acc = vec3(0.0);

    vec3 N = normalize(v_norm);
    vec3 L = normalize(LIGHT_POS - v_world_pos);
    vec3 V = normalize(u_camera_pos - v_world_pos);
    vec3 H = normalize(L + V);

    vec3 specular_color = mix(vec3(1.0), MATERIAL_BASE_COLOR, MATERIAL_METALNESS_FACTOR);

    acc += AMBIENT_COLOR * MATERIAL_BASE_COLOR;
    acc += LIGHT_COLOR * MATERIAL_DIFFUSE_FACTOR * MATERIAL_BASE_COLOR * clamp(dot(N, L), 0.0, 1.0);
    acc += LIGHT_COLOR * specular_color * pow(clamp(dot(N, H), 0.0, 1.0), MATERIAL_SHININESS);

    Target0 = vec4(acc, 1.0);
}
