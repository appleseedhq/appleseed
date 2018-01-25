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

#ifndef DENOISER_H
#define DENOISER_H

// BCD headers.
#include "DeepImage.h"
#include "IDenoiser.h"

// Boost headers.
#include "boost/atomic/atomic.hpp"

// Standard headers.
#include <memory>
#include <vector>

namespace bcd
{

class PixelPosition;

//  Class to implement the monoscale Bayesian Collaborative Filtering for Monte-Carlo Rendering
class Denoiser
  : public IDenoiser
{
  public:
    virtual ~Denoiser() {}

    virtual bool denoise();

    const Deepimf& getNbOfSamplesSqrtImage() const
    {
        return m_nbOfSamplesSqrtImage;
    }

    const Deepimf& getPixelCovarianceImage() const
    {
        return m_pixelCovarianceImage;
    }

    Deepimf& getOutputSummedColorImage(int i_index)
    {
        return m_outputSummedColorImages[i_index];
    }

    DeepImage<int>& getEstimatesCountImage(int i_index)
    {
        return m_estimatesCountImages[i_index];
    }

    DeepImage<bool>& getIsCenterOfAlreadyDenoisedPatchImage()
    {
        return m_isCenterOfAlreadyDenoisedPatchImage;
    }

    int getImagesWidth() const
    {
        return m_width;
    }

    int getImagesHeight() const
    {
        return m_height;
    }

  private:
    int                         m_width;
    int                         m_height;
    int                         m_nbOfPixels;
    Deepimf                     m_nbOfSamplesSqrtImage;
    Deepimf                     m_pixelCovarianceImage;
    std::vector<Deepimf>        m_outputSummedColorImages;
    std::vector<DeepImage<int>> m_estimatesCountImages;
    DeepImage<bool>             m_isCenterOfAlreadyDenoisedPatchImage; // For the "marking strategy"

    void doDenoise(
        std::vector<PixelPosition>::const_iterator  i_pixelBegin,
        std::vector<PixelPosition>::const_iterator  i_pixelEnd,
        const std::size_t                           i_totalNbOfPixels,
        boost::atomic<int>&                         i_nbOfPixelsComputed,
        const std::size_t                           i_threadIndex,
        bool&                                       i_abortRequested);

    void computeNbOfSamplesSqrt();
    void computePixelCovFromSampleCov();

    void reorderPixelSet(
        std::vector<PixelPosition>& io_rPixelSet) const; // Reorders the pixel set

    // Reorders the pixel by cutting the image in horizontal strips,
    // and scanning the strips in the order 1, 3, 5, 7, ... 2, 4, 6, 8, ...
    void reorderPixelSetJumpNextStrip(
        std::vector<PixelPosition>& io_rPixelSet) const;

    static void reorderPixelSetJumpNextChunk(
        std::vector<PixelPosition>& io_rPixelSet,
        int                         i_chunkSize);

    static void reorderPixelSetShuffle(
        std::vector<PixelPosition>& io_rPixelSet); // Totally random reordering

    void finalAggregation();

    void fixNegativeInfNaNValues();
    void markNegativeInfNaNValues();
};

} // namespace bcd

#endif // DENOISER_H
