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

#include "MultiscaleDenoiser.h"

#include "Denoiser.h"

#include "DeepImage.h"

#include "ImageIO.h" // temporary
#include <sstream>

#include <iostream>

#include <cassert>

#include "foundation/platform/compiler.h"

using namespace std;

namespace bcd
{
namespace
{

class MultiScaleProgressReporter
  : public IProgressReporter
{
  public:
    MultiScaleProgressReporter(
        int                 i_scale,
        int                 i_nbOfScales,
        IProgressReporter*  i_progress_reporter)
      : m_scale(i_scale)
      , m_nbOfScales(i_nbOfScales)
      , m_progress_reporter(i_progress_reporter)
    {
    }

    void progress(const float i_progress) const override
    {
        if (m_progress_reporter)
        {
            if (m_scale == m_nbOfScales - 1)
            {
                m_progress_reporter->progress(
                    i_progress / float(((1 << (2 * m_nbOfScales)) - 1) / 3));
            }
            else
            {
                int s = m_nbOfScales - 1 - m_scale;
                // next higher definition scale is 4 times slower
                // 1 + 4 + 4^2 + ... 4^s = (4^(s+1) - 1) / (4 - 1) = (2^(2*(s+1)) - 1) / 3
                // = ((1 << (2*(s+1))) - 1) / 3
                float factor = 1.f / float(((1 << (2 * m_nbOfScales)) - 1) / 3);
                float minValue = factor * float(((1 << (2 * s)) - 1) / 3);
                float maxValue = factor * float(((1 << (2 * (s + 1))) - 1) / 3);
                m_progress_reporter->progress(minValue + i_progress * (maxValue - minValue));
            }
        }
    }

    bool isAborted() const override
    {
        if (m_progress_reporter)
            return m_progress_reporter->isAborted();

        return false;
    }

  private:
    int                 m_scale;
    int                 m_nbOfScales;
    IProgressReporter*  m_progress_reporter;
};

}


bool MultiscaleDenoiser::denoise()
{
    if (!inputsOutputsAreOk())
        return false;

    DeepImageVec additionalColorImages =
        generateDownscaledAverageImages(*m_inputs.m_pColors, m_nbOfScales - 1);

    DeepImageVec additionalNbOfSamplesImages =
        generateDownscaledSumImages(*m_inputs.m_pNbOfSamples, m_nbOfScales - 1);

    DeepImageVec additionalHistogramImages =
        generateDownscaledSumImages(*m_inputs.m_pHistograms, m_nbOfScales - 1);

    DeepImageVec additionalSampleCovarianceImages =
        generateDownscaledSampleCovarianceSumImages(
            *m_inputs.m_pSampleCovariances,
            *m_inputs.m_pNbOfSamples,
            additionalNbOfSamplesImages,
            m_nbOfScales - 1);

    DeepImageVec additionalOutputImages =
        generateDownscaledEmptyImages(
            *m_outputs.m_pDenoisedColors,
            m_nbOfScales - 1);

    unique_ptr<Deepimf> uScale0TmpImage =
        unique_ptr<Deepimf>(new Deepimf(*m_outputs.m_pDenoisedColors));

    DeepImageVec additionalTmpImages =
        generateDownscaledEmptyImages(*m_outputs.m_pDenoisedColors, m_nbOfScales - 1);

    vector<DenoiserInputs> inputsArray(m_nbOfScales);
    vector<DenoiserOutputs> outputsArray(m_nbOfScales);
    vector<Deepimf*> tmpImagesArray(m_nbOfScales);

    inputsArray[0] = m_inputs;
    outputsArray[0] = m_outputs;
    tmpImagesArray[0] = uScale0TmpImage.get();

    for (int scale = 1; scale < m_nbOfScales; ++scale)
    {
        inputsArray[scale].m_pColors = additionalColorImages[scale - 1].get();

        inputsArray[scale].m_pNbOfSamples =
                additionalNbOfSamplesImages[scale - 1].get();

        inputsArray[scale].m_pHistograms =
                additionalHistogramImages[scale - 1].get();

        inputsArray[scale].m_pSampleCovariances =
                additionalSampleCovarianceImages[scale - 1].get();

        outputsArray[scale].m_pDenoisedColors =
                additionalOutputImages[scale - 1].get();

        tmpImagesArray[scale] = additionalTmpImages[scale - 1].get();
    }

    {
        Denoiser denoiser;
        denoiser.setInputs(inputsArray[m_nbOfScales - 1]);
        denoiser.setOutputs(outputsArray[m_nbOfScales - 1]);
        denoiser.setParameters(m_parameters);

        MultiScaleProgressReporter progressReporter(
            m_nbOfScales - 1,
            m_nbOfScales,
            m_progressReporter);
        denoiser.setProgressReporter(&progressReporter);

        const bool success = denoiser.denoise();

        if (success == false)
            return false;
    }

    for (int scale = m_nbOfScales - 2; scale >= 0; --scale)
    {
        Denoiser denoiser;
        denoiser.setInputs(inputsArray[scale]);
        denoiser.setOutputs(outputsArray[scale]);
        denoiser.setParameters(m_parameters);

        MultiScaleProgressReporter progressReporter(
            scale,
            m_nbOfScales,
            m_progressReporter);
        denoiser.setProgressReporter(&progressReporter);

        const bool success = denoiser.denoise();

        if (success == false)
            return false;

        if (m_progressReporter && m_progressReporter->isAborted())
            return false;

        mergeOutputs(
            *outputsArray[scale].m_pDenoisedColors,
            *tmpImagesArray[scale],
            *tmpImagesArray[scale + 1],
            *outputsArray[scale + 1].m_pDenoisedColors,
            *outputsArray[scale].m_pDenoisedColors);
    }

    return true;
}

MultiscaleDenoiser::DeepImageVec MultiscaleDenoiser::generateDownscaledEmptyImages(
    const Deepimf&      i_rScale0Image,
    int                 i_nbOfScalesToGenerate)
{
    DeepImageVec emptyImages(i_nbOfScalesToGenerate);

    int width = i_rScale0Image.getWidth();
    int height = i_rScale0Image.getHeight();
    const int depth = i_rScale0Image.getDepth();

    for (int scale = 0; scale < i_nbOfScalesToGenerate; ++scale)
    {
        width /= 2;
        height /= 2;
        emptyImages[scale] = unique_ptr<Deepimf>(new Deepimf(width, height, depth));
    }

    return emptyImages;
}

MultiscaleDenoiser::DeepImageVec MultiscaleDenoiser::generateDownscaledSumImages(
    const Deepimf&      i_rScale0Image,
    int                 i_nbOfScalesToGenerate)
{
    DeepImageVec downscaledImages(i_nbOfScalesToGenerate);
    const Deepimf* pPreviousImage = &i_rScale0Image;

    for (int scale = 0; scale < i_nbOfScalesToGenerate; ++scale)
    {
        downscaledImages[scale] = downscaleSum(*pPreviousImage);
        pPreviousImage = downscaledImages[scale].get();
    }

    return downscaledImages;
}

MultiscaleDenoiser::DeepImageVec MultiscaleDenoiser::generateDownscaledAverageImages(
    const Deepimf&      i_rScale0Image,
    int                 i_nbOfScalesToGenerate)
{
    DeepImageVec downscaledImages(i_nbOfScalesToGenerate);
    const Deepimf* pPreviousImage = &i_rScale0Image;

    for (int scale = 0; scale < i_nbOfScalesToGenerate; ++scale)
    {
        downscaledImages[scale] = downscaleAverage(*pPreviousImage);
        pPreviousImage = downscaledImages[scale].get();
    }

    return downscaledImages;
}

MultiscaleDenoiser::DeepImageVec MultiscaleDenoiser::generateDownscaledSampleCovarianceSumImages(
    const Deepimf&      i_rScale0SampleCovarianceImage,
    const Deepimf&      i_rScale0NbOfSamplesImage,
    const DeepImageVec& i_rDownscaledNbOfSamplesImages,
    int                 i_nbOfScalesToGenerate)
{
    DeepImageVec downscaledImages(i_nbOfScalesToGenerate);
    const Deepimf* pPreviousImage = &i_rScale0SampleCovarianceImage;
    const Deepimf* pPreviousNbOfSamplesImage = &i_rScale0NbOfSamplesImage;

    for (int scale = 0; scale < i_nbOfScalesToGenerate; ++scale)
    {
        downscaledImages[scale] =
                downscaleSampleCovarianceSum(*pPreviousImage, *pPreviousNbOfSamplesImage);
        pPreviousImage = downscaledImages[scale].get();
        pPreviousNbOfSamplesImage = i_rDownscaledNbOfSamplesImages[scale].get();
    }

    return downscaledImages;
}

unique_ptr<Deepimf> MultiscaleDenoiser::downscaleSum(const Deepimf& i_rImage)
{
    const int width = i_rImage.getWidth();
    const int height = i_rImage.getHeight();
    const int depth = i_rImage.getDepth();
    const int downscaledWidth = width / 2;
    const int downscaledHeight = height / 2;

    int line, col, z;
    PixelPosition p1, p2, p3, p4;

    unique_ptr<Deepimf> uImage(
                new Deepimf(downscaledWidth, downscaledHeight, depth));

    for (line = 0; line < downscaledHeight; ++line)
    {
        for (col = 0; col < downscaledWidth; ++col)
        {
            p1 = PixelPosition(2 * line, 2 * col);
            p2 = p1 + PixelVector(1, 0);
            p3 = p1 + PixelVector(0, 1);
            p4 = p1 + PixelVector(1, 1);

            for (z = 0; z < depth; ++z)
            {
                uImage->set(line,
                            col,
                            z,
                            i_rImage.get(p1, z) + i_rImage.get(p2, z) +
                            i_rImage.get(p3, z) + i_rImage.get(p4, z));
            }
        }
    }

    return move(uImage);
}

unique_ptr<Deepimf> MultiscaleDenoiser::downscaleAverage(const Deepimf& i_rImage)
{
    const int width = i_rImage.getWidth();
    const int height = i_rImage.getHeight();
    const int depth = i_rImage.getDepth();
    const int downscaledWidth = width / 2;
    const int downscaledHeight = height / 2;

    int line, col, z;
    PixelPosition p1, p2, p3, p4;

    unique_ptr<Deepimf> uImage(new Deepimf(downscaledWidth, downscaledHeight, depth));

    for (line = 0; line < downscaledHeight; ++line)
    {
        for (col = 0; col < downscaledWidth; ++col)
        {
            p1 = PixelPosition(2 * line, 2 * col);
            p2 = p1 + PixelVector(1, 0);
            p3 = p1 + PixelVector(0, 1);
            p4 = p1 + PixelVector(1, 1);
            for (z = 0; z < depth; ++z)
            {
                uImage->set(line,
                            col,
                            z,
                            0.25f * (i_rImage.get(p1, z) + i_rImage.get(p2, z) +
                                     i_rImage.get(p3, z) + i_rImage.get(p4, z)));
            }
        }
    }

    return uImage;
}

unique_ptr<Deepimf> MultiscaleDenoiser::downscaleSampleCovarianceSum(
  const Deepimf&        i_rSampleCovarianceImage,
  const Deepimf&        i_rNbOfSamplesImage)
{
    const int width = i_rSampleCovarianceImage.getWidth();
    const int height = i_rSampleCovarianceImage.getHeight();
    const int depth = i_rSampleCovarianceImage.getDepth();
    const int downscaledWidth = width / 2;
    const int downscaledHeight = height / 2;

    int line, col, z;
    PixelPosition p1, p2, p3, p4;
    float n1, n2, n3, n4, nSum, w1, w2, w3, w4;

    unique_ptr<Deepimf> uImage(
                new Deepimf(downscaledWidth, downscaledHeight, depth));

    const float squaredWeight = (1.f / 4.f) * (1.f / 4.f);

    for (line = 0; line < downscaledHeight; ++line)
    {
        for (col = 0; col < downscaledWidth; ++col)
        {
            p1 = PixelPosition(2 * line, 2 * col);
            p2 = p1 + PixelVector(1, 0);
            p3 = p1 + PixelVector(0, 1);
            p4 = p1 + PixelVector(1, 1);
            n1 = i_rNbOfSamplesImage.get(p1, 0);
            n2 = i_rNbOfSamplesImage.get(p2, 0);
            n3 = i_rNbOfSamplesImage.get(p3, 0);
            n4 = i_rNbOfSamplesImage.get(p4, 0);
            nSum = n1 + n2 + n3 + n4;
            w1 = squaredWeight * nSum / n1;
            w2 = squaredWeight * nSum / n2;
            w3 = squaredWeight * nSum / n3;
            w4 = squaredWeight * nSum / n4;
            for (z = 0; z < depth; ++z)
                uImage->set(line,
                            col,
                            z,
                            w1 * i_rSampleCovarianceImage.get(p1, z) +
                            w2 * i_rSampleCovarianceImage.get(p2, z) +
                            w3 * i_rSampleCovarianceImage.get(p3, z) +
                            w4 * i_rSampleCovarianceImage.get(p4, z));
        }
    }

    return uImage;
}

void MultiscaleDenoiser::mergeOutputsNoInterpolation(
    Deepimf&            o_rMergedImage,
    const Deepimf&      i_rLowFrequencyImage,
    const Deepimf&      i_rHighFrequencyImage)
{
    const int width = o_rMergedImage.getWidth();
    const int height = o_rMergedImage.getHeight();
    const int depth = o_rMergedImage.getDepth();
    const int downscaledWidth = width / 2;
    const int downscaledHeight = height / 2;

    assert(width == i_rHighFrequencyImage.getWidth());
    assert(height == i_rHighFrequencyImage.getHeight());
    assert(downscaledWidth == i_rLowFrequencyImage.getWidth());
    assert(downscaledHeight == i_rLowFrequencyImage.getHeight());

    int line, col, z;
    PixelPosition p1, p2, p3, p4;
    float v, v1, v2, v3, v4, dv;

    for (line = 0; line < downscaledHeight; ++line)
    {
        for (col = 0; col < downscaledWidth; ++col)
        {
            p1 = PixelPosition(2 * line, 2 * col);
            p2 = p1 + PixelVector(1, 0);
            p3 = p1 + PixelVector(0, 1);
            p4 = p1 + PixelVector(1, 1);

            for (z = 0; z < depth; ++z)
            {
                v = i_rLowFrequencyImage.get(line, col, z);
                v1 = i_rHighFrequencyImage.get(p1, z);
                v2 = i_rHighFrequencyImage.get(p2, z);
                v3 = i_rHighFrequencyImage.get(p3, z);
                v4 = i_rHighFrequencyImage.get(p4, z);
                dv = v - 0.25f * (v1 + v2 + v3 + v4);
                o_rMergedImage.set(p1, z, v1 + dv);
                o_rMergedImage.set(p2, z, v2 + dv);
                o_rMergedImage.set(p3, z, v3 + dv);
                o_rMergedImage.set(p4, z, v4 + dv);
            }
        }
    }
}

void MultiscaleDenoiser::mergeOutputs(
    Deepimf&            o_rMergedHighResImage,
    Deepimf&            o_rTmpHighResImage,
    Deepimf&            o_rTmpLowResImage,
    const Deepimf&      i_rLowResImage,
    const Deepimf&      i_rHighResImage)
{
  if (&o_rMergedHighResImage != &i_rHighResImage)
    o_rMergedHighResImage = i_rHighResImage;

  lowPass(o_rTmpHighResImage, o_rTmpLowResImage, i_rHighResImage);
  o_rMergedHighResImage -= o_rTmpHighResImage;

  interpolate(o_rTmpHighResImage, i_rLowResImage);
  o_rMergedHighResImage += o_rTmpHighResImage;
}

inline int clampPositiveInteger(int i_value, int i_maxValuePlusOne)
{
    return (i_value <= 0
            ? 0
            : (i_value >= i_maxValuePlusOne ? i_maxValuePlusOne - 1 : i_value));
}

void MultiscaleDenoiser::interpolate(
    Deepimf&            o_rInterpolatedImage,
    const Deepimf&      i_rImage)
{
    const int width = i_rImage.getWidth();
    const int height = i_rImage.getHeight();
    const int depth = i_rImage.getDepth();
    const int upscaledWidth = o_rInterpolatedImage.getWidth();
    const int upscaledHeight = o_rInterpolatedImage.getHeight();

    assert(width == upscaledWidth / 2);
    assert(height == upscaledHeight / 2);
    assert(depth == o_rInterpolatedImage.getDepth());

    const float mainPixelWeight = 9.f / 16.f;
    const float adjacentPixelWeight = 3.f / 16.f;
    const float diagonalPixelWeight = 1.f / 16.f;

    int upscaledLine, upscaledCol, z, line, col, adjacentLine, adjacentCol;

    for (upscaledLine = 0; upscaledLine < upscaledHeight; ++upscaledLine)
    {
        for (upscaledCol = 0; upscaledCol < upscaledWidth; ++upscaledCol)
        {
            line = upscaledLine / 2;
            col = upscaledCol / 2;
            adjacentLine = clampPositiveInteger(line + ((upscaledLine % 2) * 2 - 1), height);
            adjacentCol = clampPositiveInteger(col + ((upscaledCol % 2) * 2 - 1), width);

            for (z = 0; z < depth; ++z)
            {
                o_rInterpolatedImage.set(
                            upscaledLine,
                            upscaledCol,
                            z,
                            mainPixelWeight * i_rImage.get(line, col, z) +
                            adjacentPixelWeight * (i_rImage.get(line, adjacentCol, z) +
                                                   i_rImage.get(adjacentLine, col, z)) +
                            diagonalPixelWeight * i_rImage.get(adjacentLine, adjacentCol, z));
            }
        }
    }
}

void MultiscaleDenoiser::downscale(
    Deepimf&            o_rDownscaledImage,
    const Deepimf&      i_rImage)
{
    const int depth = i_rImage.getDepth();
    const int downscaledWidth = o_rDownscaledImage.getWidth();
    const int downscaledHeight = o_rDownscaledImage.getHeight();

    APPLESEED_UNUSED const int width = i_rImage.getWidth();
    APPLESEED_UNUSED const int height = i_rImage.getHeight();
    assert(downscaledWidth == width / 2);
    assert(downscaledHeight == height / 2);

    int line, col, z;
    PixelPosition p1, p2, p3, p4;

    for (line = 0; line < downscaledHeight; ++line)
    {
        for (col = 0; col < downscaledWidth; ++col)
        {
            p1 = PixelPosition(2 * line, 2 * col);
            p2 = p1 + PixelVector(1, 0);
            p3 = p1 + PixelVector(0, 1);
            p4 = p1 + PixelVector(1, 1);
            for (z = 0; z < depth; ++z)
            {
                o_rDownscaledImage.set(line,
                                       col,
                                       z,
                                       0.25f *
                                       (i_rImage.get(p1, z) + i_rImage.get(p2, z) +
                                        i_rImage.get(p3, z) + i_rImage.get(p4, z)));
            }
        }
    }
}

void MultiscaleDenoiser::lowPass(
    Deepimf&            o_rFilteredImage,
    Deepimf&            o_rTmpLowResImage,
    const Deepimf&      i_rImage)
{
    downscale(o_rTmpLowResImage, i_rImage);
    interpolate(o_rFilteredImage, o_rTmpLowResImage);
}

void MultiscaleDenoiser::interpolateThenDownscale(
    Deepimf&            o_rFilteredImage,
    const Deepimf&      i_rImage)
{
    const int width = i_rImage.getWidth();
    const int height = i_rImage.getHeight();
    const int depth = i_rImage.getDepth();

    assert(width == o_rFilteredImage.getWidth());
    assert(height == o_rFilteredImage.getHeight());
    assert(depth == o_rFilteredImage.getDepth());

    const float mainPixelWeight = 36.f / 64.f;
    const float adjacentPixelWeight = 6.f / 64.f;
    const float diagonalPixelWeight = 1.f / 64.f;

    int line, col, z, previousLine, previousCol, nextLine, nextCol;

    for (line = 0; line < height; ++line)
    {
        for (col = 0; col < width; ++col)
        {
            previousLine = clampPositiveInteger(line - 1, height);
            nextLine = clampPositiveInteger(line + 1, height);
            previousCol = clampPositiveInteger(col - 1, width);
            nextCol = clampPositiveInteger(col + 1, width);

            for (z = 0; z < depth; ++z)
            {
                o_rFilteredImage.set(
                            line,
                            col,
                            z,
                            mainPixelWeight * i_rImage.get(line, col, z) +
                            adjacentPixelWeight * (i_rImage.get(line, previousCol, z) +
                                                   i_rImage.get(line, nextCol, z) +
                                                   i_rImage.get(previousLine, col, z) +
                                                   i_rImage.get(nextLine, col, z)) +
                            diagonalPixelWeight * (i_rImage.get(previousLine, previousCol, z) +
                                                   i_rImage.get(previousLine, nextCol, z) +
                                                   i_rImage.get(nextLine, previousCol, z) +
                                                   i_rImage.get(nextLine, nextCol, z)));
            }
        }
    }
}

} // namespace bcd
