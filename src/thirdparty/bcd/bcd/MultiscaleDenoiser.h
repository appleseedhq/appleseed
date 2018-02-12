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

#ifndef MULTISCALE_DENOISER_H
#define MULTISCALE_DENOISER_H

// BCD headers.
#include "IDenoiser.h"

// Standard headers.
#include <memory>
#include <vector>

namespace bcd
{

template<class T>
class DeepImage;

//  Class to implement the multiscale Bayesian Collaborative Filtering for
//  Monte-Carlo Rendering
class MultiscaleDenoiser : public IDenoiser
{
  public:
    explicit MultiscaleDenoiser(int i_nbOfScales);

    virtual ~MultiscaleDenoiser() {}

  public:
    virtual bool denoise() override;

  private:

    void setInputs(const DenoiserInputs& i_rInputs) override;

    typedef std::vector<std::unique_ptr<DeepImage<float>>> DeepImageVec;

    static DeepImageVec generateDownscaledEmptyImages(
        const DeepImage<float>& i_rScale0Image,
        int                     i_nbOfScalesToGenerate);

    static DeepImageVec generateDownscaledSumImages(
        const DeepImage<float>& i_rScale0Image,
        int                     i_nbOfScalesToGenerate);

    static DeepImageVec generateDownscaledAverageImages(
        const DeepImage<float>& i_rScale0Image,
        int                     i_nbOfScalesToGenerate);

    static DeepImageVec generateDownscaledSampleCovarianceSumImages(
        const DeepImage<float>& i_rScale0SampleCovarianceImage,
        const DeepImage<float>& i_rScale0NbOfSamplesImage,
        const DeepImageVec&     i_rDownscaledNbOfSamplesImages,
        int                     i_nbOfScalesToGenerate);

    static std::unique_ptr<DeepImage<float>> downscaleSum(
        const DeepImage<float>& i_rImage);

    static std::unique_ptr<DeepImage<float>> downscaleAverage(
        const DeepImage<float>& i_rImage);

    static std::unique_ptr<DeepImage<float>> downscaleSampleCovarianceSum(
        const DeepImage<float>& i_rSampleCovarianceImage,
        const DeepImage<float>& i_rNbOfSamplesImage);

    // Merges two buffers of two successive scales
    // o_rMergedImage and i_rHighFrequencyImage can point to the same buffer
    static void mergeOutputs(
        DeepImage<float>&       o_rMergedHighResImage,
        DeepImage<float>&       o_rTmpHighResImage,
        DeepImage<float>&       o_rTmpLowResImage,
        const DeepImage<float>& i_rLowResImage,
        const DeepImage<float>& i_rHighResImage);

    static void interpolate(
        DeepImage<float>&       o_rInterpolatedImage,
        const DeepImage<float>& i_rImage);

    static void downscale(
        DeepImage<float>&       o_rDownscaledImage,
        const DeepImage<float>& i_rImage);

    //  equivalent to downscale followed by interpolate
    static void lowPass(
        DeepImage<float>&       o_rFilteredImage,
        DeepImage<float>&       o_rTmpLowResImage,
        const DeepImage<float>& i_rImage);

  private:
    int m_nbOfScales;
};

} // namespace bcd

#endif // MULTISCALE_DENOISER_H
