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

#ifndef IMAGE_IO_H
#define IMAGE_IO_H

// Standard headers.
#include <vector>

namespace bcd
{

template<typename T>
class DeepImage;

//  Class used as an interface for loading .exr files as DeepImage
class ImageIO
{
  private:
    ImageIO() {}

  public:
    static bool loadEXR(DeepImage<float>& o_rImage, const char* i_pFilePath);

    static bool writeEXR(
        const DeepImage<float>& i_rImage,
        const char*             i_pFilePath);

    static bool loadMultiChannelsEXR(
        DeepImage<float>&       o_rImage,
        const char*             i_pFilePath);

    static bool writeMultiChannelsEXR(
        const DeepImage<float>& i_rImage,
        const char*             i_pFilePath);
};

}

#endif // IMAGE_IO_H
