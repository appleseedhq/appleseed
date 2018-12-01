//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Luis Barrancos, The appleseedhq Organization
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

#pragma once

void compute_id_manifold(
    int manifold_type,
    int domain,
    int seed,
    string expression,
    output int hash_id,
    output color color_id,
    output float greyscale_id)
{
    string manifold_str = "";

    hash_id = 0;
    color_id = color(0);
    greyscale_id = 0.0;

    if (manifold_type == 0)
    {
        getattribute("object:object_name", manifold_str);
        hash_id = hash(manifold_str);
    }
    else if (manifold_type == 1)
    {
        getattribute("object:object_instance_name", manifold_str);
        hash_id= hash(manifold_str);
    }
    else if (manifold_type == 2)
    {
        getattribute("object:assembly_name", manifold_str);
        hash_id = hash(manifold_str);
    }
    else if (manifold_type == 3)
    {
        getattribute("object:assembly_instance_name", manifold_str);
        hash_id = hash(manifold_str);
    }
    else if (manifold_type == 4)
    {
        getattribute("object:face_id", hash_id);
    }
    else if (expression != "")
    {
        if (domain == 0)
        {
            getattribute("object:object_name", manifold_str);
        }
        else if (domain == 1)
        {
            getattribute("object:object_instance_name", manifold_str);
        }
        else if (domain == 2)
        {
            getattribute("object:assembly_name", manifold_str);
        }
        else
        {
            getattribute("object:assembly_instance_name", manifold_str);
        }

        if (manifold_type == 5)
        {
            if (startswith(manifold_str, expression))
            {
                hash_id = hash(seed);
            }
        }
        else if (manifold_type == 6)
        {
            if (endswith(manifold_str, expression))
            {
                hash_id = hash(seed);
            }
        }
        else if (regex_search(manifold_str, expression))
        {
            hash_id = hash(seed);
        }
    }

    greyscale_id = (float) cellnoise(hash_id);
    color_id = (color) cellnoise(hash_id);
}

void rotate2d(
    float x,
    float y,
    float angle_in_degrees,
    output float rx,
    output float ry)
{
    float angle_rad = radians(angle_in_degrees);
    float c, s;        
    sincos(angle_rad, s, c);
    rx = x * c - s * y;
    ry = x * s + c * y;
}
