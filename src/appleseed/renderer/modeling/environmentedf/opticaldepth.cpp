
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Joel Barmettler, The appleseedhq Organization
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

#include "opticaldepth.h"

sky::opticaldepth::opticaldepth() {
    sky::opticaldepth(0.0f, 0.0f, 0.0f);
}

sky::opticaldepth::opticaldepth(float r, float m, float o) {
    sky::opticaldepth(r, m, o, 1.0f, 1.0f, 1.0f);
}

sky::opticaldepth::opticaldepth(float r, float m, float o, float ad, float dd, float od) :
    rayleigh(r),
    mie(m),
    ozone(o),
    air_particle_density(ad),
    dust_particle_density(dd),
    ozone_particle_density(od) { }

void sky::opticaldepth::increase(float segment_length, float rayleigh_density, float mie_density, float ozone_density) {
    rayleigh += segment_length * rayleigh_density * air_particle_density;
    mie += segment_length * mie_density * dust_particle_density;
    ozone += segment_length * ozone_density * ozone_particle_density;
}

