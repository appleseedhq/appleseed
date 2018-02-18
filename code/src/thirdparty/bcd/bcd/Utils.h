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

#ifndef UTILS_H
#define UTILS_H

// Standard headers.
#include <cfloat>
#include <cmath>

//  A macro to mark a variable as unused.

#if defined(__GNUC__) && ((__GNUC__ > 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 6)))
    #define BCD_UNUSED   __attribute__((unused))
#elif defined(__clang__)
    #define BCD_UNUSED   __attribute__((unused))
#else
    #define BCD_UNUSED
#endif

namespace bcd
{

inline bool isFinite(const float x)
{
#ifdef _WIN32
    return _finite(x) != 0;
#else
    return !(std::isnan(x) || std::isinf(x));
#endif
}

template<typename T>
class DeepImage;

//  Class for various useful functions
class Utils
{
  private:
    Utils() {}

  public:
    static bool separateNbOfSamplesFromHistogram(
      DeepImage<float>&       o_rHistoImage,
      DeepImage<float>&       o_rNbOfSamplesImage,
      const DeepImage<float>& i_rHistoAndNbOfSamplesImage);

    static DeepImage<float> mergeHistogramAndNbOfSamples(
      const DeepImage<float>& i_rHistoImage,
      const DeepImage<float>& i_rNbOfSamplesImage);
};

}

#endif // UTILS_H
