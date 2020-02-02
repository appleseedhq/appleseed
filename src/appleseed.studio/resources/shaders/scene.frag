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

layout(location = 0) in vec3 frag_pos;
layout(location = 1) in vec3 frag_norm;

const vec3 MATERIAL_BASE_COLOR = vec3(0.8, 0.8, 0.8);
const float MATERIAL_DIFFUSE_FACTOR = 0.4;
const float MATERIAL_SPECULAR_FACTOR = 0.15;
const float MATERIAL_SHININESS = 32.0;

out vec4 Target0;

void main()
{
    vec3 norm = normalize(frag_norm);

    vec3 view_dir = normalize(-frag_pos);
    vec3 reflect_dir = reflect(-view_dir, norm);

    float diffuse_ratio = max(0.0, dot(view_dir, norm));
    float specular_ratio = pow(max(0.0, dot(view_dir, reflect_dir)), MATERIAL_SHININESS);

    vec3 diffuse = MATERIAL_DIFFUSE_FACTOR * MATERIAL_BASE_COLOR * diffuse_ratio;
    vec3 specular = MATERIAL_SPECULAR_FACTOR * MATERIAL_BASE_COLOR * specular_ratio;

    Target0 = vec4(diffuse + specular, 1.0);
}
