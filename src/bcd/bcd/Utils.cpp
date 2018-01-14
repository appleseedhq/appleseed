// This file comes from the original BCD implementation,
// with minor changes to remove dependencies, unused code
// and re-formatting. Original license follows:

// This file is part of the reference implementation for the paper
//   Bayesian Collaborative Denoising for Monte-Carlo Rendering
//   Malik Boughida and Tamy Boubekeur.
//   Computer Graphics Forum (Proc. EGSR 2017), vol. 36, no. 4, p. 137-153, 2017
//
// All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.txt file.

// BCD headers.
#include "DeepImage.h"
#include "Utils.h"

// Standard headers.
#include <cassert>
#include <cstring>

using namespace std;

namespace bcd
{

bool Utils::separateNbOfSamplesFromHistogram(
    Deepimf&        o_rHistoImage,
    Deepimf&        o_rNbOfSamplesImage,
    const Deepimf&  i_rHistoAndNbOfSamplesImage)
{
    int w = i_rHistoAndNbOfSamplesImage.getWidth();
    int h = i_rHistoAndNbOfSamplesImage.getHeight();
    int d = i_rHistoAndNbOfSamplesImage.getDepth() - 1;

    o_rHistoImage.resize(w, h, d);
    o_rNbOfSamplesImage.resize(w, h, 1);

    size_t histoDataSize = d * sizeof(float);
    ImfIt histoIt = o_rHistoImage.begin();
    ImfIt nbOfSamplesIt = o_rNbOfSamplesImage.begin();
    ImfConstIt histoAndNbOfSamplesIt = i_rHistoAndNbOfSamplesImage.begin();
    ImfConstIt histoAndNbOfSamplesItEnd = i_rHistoAndNbOfSamplesImage.end();

    for (; histoAndNbOfSamplesIt != histoAndNbOfSamplesItEnd; ++histoIt, ++nbOfSamplesIt, ++histoAndNbOfSamplesIt)
    {
        memcpy(*histoIt, *histoAndNbOfSamplesIt, histoDataSize);
        nbOfSamplesIt[0] = histoAndNbOfSamplesIt[d];
    }

    return true;
}

Deepimf Utils::mergeHistogramAndNbOfSamples(
    const Deepimf& i_rHistoImage,
    const Deepimf& i_rNbOfSamplesImage)
{
    Deepimf histoAndNbOfSamplesImage;

    int w = i_rHistoImage.getWidth();
    int h = i_rHistoImage.getHeight();
    int d = i_rHistoImage.getDepth();

    assert(i_rNbOfSamplesImage.getWidth() == w);
    assert(i_rNbOfSamplesImage.getHeight() == h);
    assert(i_rNbOfSamplesImage.getDepth() == 1);

    histoAndNbOfSamplesImage.resize(w, h, d + 1);

    size_t histoDataSize = d * sizeof(float);
    ImfConstIt histoIt = i_rHistoImage.begin();
    ImfConstIt nbOfSamplesIt = i_rNbOfSamplesImage.begin();
    ImfIt histoAndNbOfSamplesIt = histoAndNbOfSamplesImage.begin();
    ImfIt histoAndNbOfSamplesItEnd = histoAndNbOfSamplesImage.end();

    for (; histoAndNbOfSamplesIt != histoAndNbOfSamplesItEnd; ++histoIt, ++nbOfSamplesIt, ++histoAndNbOfSamplesIt)
    {
        memcpy(*histoAndNbOfSamplesIt, *histoIt, histoDataSize);
        histoAndNbOfSamplesIt[d] = nbOfSamplesIt[0];
    }

    return histoAndNbOfSamplesImage;
}

}
