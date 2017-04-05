
//
// This software is released under the MIT licence
//
// Copyright (c) 2013 Anders Langlands
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
// IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//

// This code comes from alshaders OSL branch, with minimal changes.
// https://bitbucket.org/anderslanglands/alshaders/branch/osl

#ifndef ALSHADERS_OSL_REMAP_H
#define ALSHADERS_OSL_REMAP_H

// Params.
#define REMAP_FLOAT_DECLARE_PARAMS(Page)  \
float RMPinputMin = 0                     \
[[                                        \
    string label = "Input min",           \
    string page = Page                    \
]],                                       \
float RMPinputMax = 1                     \
[[                                        \
    string label = "Input max",           \
    string page = Page                    \
]],                                       \
float RMPcontrast = 1                     \
[[                                        \
    string label = "Contrast",            \
    string page = Page                    \
]],                                       \
float RMPcontrastPivot = 0.5              \
[[                                        \
    string label = "Pivot",               \
    string page = Page                    \
]],                                       \
float RMPbias = 0.5                       \
[[                                        \
    string label = "Bias",                \
    string page = Page                    \
]],                                       \
float RMPgain = 0.5                       \
[[                                        \
    string label = "Gain",                \
    string page = Page                    \
]],                                       \
float RMPoutputMin = 0                    \
[[                                        \
    string label = "Output min",          \
    string page = Page                    \
]],                                       \
float RMPoutputMax = 1                    \
[[                                        \
    string label = "Output max",          \
    string page = Page                    \
]],                                       \
int RMPclampEnable = 1                    \
[[                                        \
    string label = "Clamp",               \
    string widget = "checkBox",           \
    string page = Page                    \
]],                                       \
int RMPthreshold = 0                      \
[[                                        \
    string label = "Expand",              \
    string widget = "checkBox",           \
    string page = Page                    \
]],                                       \
float RMPclampMin = 0                     \
[[                                        \
    string label = "Min",                 \
    string page = Page                    \
]],                                       \
float RMPclampMax = 1                     \
[[                                        \
    string label = "Max",                 \
    string page = Page                    \
]]

struct RemapFloatParams
{
   float inputMin;
   float inputMax;
   float contrastVal;
   float contrastPivot;
   float bias;
   float gain;
   float outputMin;
   float outputMax;
   int clampEnable;
   int expand;
   float clampMin;
   float clampMax;
};

#define REMAP_FLOAT_CREATE  \
{                           \
    RMPinputMin,            \
    RMPinputMax,            \
    RMPcontrast,            \
    RMPcontrastPivot,       \
    RMPbias,                \
    RMPgain,                \
    RMPoutputMin,           \
    RMPoutputMax,           \
    RMPclampEnable,         \
    RMPthreshold,           \
    RMPclampMin,            \
    RMPclampMax             \
}

float contrast(float input, float c, float pivot)
{
    if (c == 1.0)
        return input;

    return (input - pivot) * c + pivot;
}

float bias(float f, float b)
{
    if (b > 0.0)
        return pow(f, log(b) / log(0.5));

    return 0.0;
}

float biasandgain(float f, float b, float g)
{
    if (f < 0)
        return f;

    if (b != 0.5)
        f = bias(f, b);
    if (g != 0.5)
    {
        if (f < 0.5)
            f = 0.5 * bias(2.0 * f, 1.0 - g);
        else
            f = 1.0 - bias(2.0 - 2.0 * f, 1.0 - g) * 0.5;
    }

    return f;
}

float remap(float input, RemapFloatParams p)
{
    float f = (input-p.inputMin) / (p.inputMax-p.inputMin);
    f = contrast(f, p.contrastVal, p.contrastPivot);
    f = biasandgain(f, p.bias, p.gain);
    f = mix(p.outputMin, p.outputMax, f);

    if (p.clampEnable)
    {
        f = min(p.clampMax, f);
        f = max(p.clampMin, f);

        if (p.expand)
            f = (f - p.clampMin) / (p.clampMax - p.clampMin);
    }

    return f;
}

#endif
