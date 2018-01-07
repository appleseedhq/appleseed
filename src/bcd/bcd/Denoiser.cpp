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

#include "Denoiser.h"

#include "DenoisingUnit.h"

#include <algorithm>
#include <iostream>
#include <random>

#include <chrono>

#include <cassert>

#include "foundation/platform/compiler.h"

#define USE_ATOMIC
#ifndef USE_ATOMIC
#define USE_CRITICAL
#endif

using namespace std;

namespace bcd
{

bool Denoiser::denoise()
{
    if (!inputsOutputsAreOk())
        return false;

    m_width = m_inputs.m_pColors->getWidth();
    m_height = m_inputs.m_pColors->getHeight();
    m_nbOfPixels = m_width * m_height;
    int widthWithoutBorder = m_width - 2 * m_parameters.m_patchRadius;
    int heightWithoutBorder = m_height - 2 * m_parameters.m_patchRadius;
    int nbOfPixelsWithoutBorder = widthWithoutBorder * heightWithoutBorder;

    computeNbOfSamplesSqrt();
    computePixelCovFromSampleCov();

    if (m_parameters.m_nbOfCores > 0)
        omp_set_num_threads(m_parameters.m_nbOfCores);

#pragma omp parallel
#pragma omp master
    {
        m_parameters.m_nbOfCores = omp_get_num_threads();
        // now m_parameters.m_nbOfCores is set to the actual number of threads
        // even if it was set to the default value 0
    }

    vector<PixelPosition> pixelSet(nbOfPixelsWithoutBorder);
    {
        int k = 0;
        int lMin = m_parameters.m_patchRadius;
        int lMax = m_height - m_parameters.m_patchRadius - 1;
        int cMin = m_parameters.m_patchRadius;
        int cMax = m_width - m_parameters.m_patchRadius - 1;
        for (int l = lMin; l <= lMax; l++)
            for (int c = cMin; c <= cMax; c++)
                pixelSet[k++] = PixelPosition(l, c);
    }
    reorderPixelSet(pixelSet);

    m_outputSummedColorImages.resize(m_parameters.m_nbOfCores);
    m_outputSummedColorImages[0].resize(m_width, m_height, m_inputs.m_pColors->getDepth());
    m_outputSummedColorImages[0].fill(0.f);

    for (int i = 1; i < m_parameters.m_nbOfCores; ++i)
        m_outputSummedColorImages[i] = m_outputSummedColorImages[0];

    m_estimatesCountImages.resize(m_parameters.m_nbOfCores);
    m_estimatesCountImages[0].resize(m_width, m_height, 1);
    m_estimatesCountImages[0].fill(0);

    for (int i = 1; i < m_parameters.m_nbOfCores; ++i)
        m_estimatesCountImages[i] = m_estimatesCountImages[0];

    m_isCenterOfAlreadyDenoisedPatchImage.resize(m_width, m_height, 1);
    m_isCenterOfAlreadyDenoisedPatchImage.fill(false);

    int chunkSize; // nb of pixels a thread has to treat before dynamically asking for more work
    chunkSize = widthWithoutBorder * (2 * m_parameters.m_searchWindowRadius);

    int nbOfPixelsComputed = 0;
    int currentPercentage = 0, newPercentage = 0;
#pragma omp parallel
    {
        DenoisingUnit denoisingUnit(*this);
#pragma omp for ordered schedule(dynamic, chunkSize)
        for (int pixelIndex = 0; pixelIndex < nbOfPixelsWithoutBorder; pixelIndex++)
        {
            PixelPosition mainPatchCenter = pixelSet[pixelIndex];
            denoisingUnit.denoisePatchAndSimilarPatches(mainPatchCenter);
#pragma omp atomic
            ++nbOfPixelsComputed;
            if (omp_get_thread_num() == 0)
            {
                newPercentage = (nbOfPixelsComputed * 100) / nbOfPixelsWithoutBorder;
                if (newPercentage != currentPercentage)
                {
                    currentPercentage = newPercentage;
                    m_progressCallback(float(currentPercentage) * 0.01f);
                }
            }
        }
    }

    m_outputs.m_pDenoisedColors->resize(m_width, m_height, 3);
    m_outputs.m_pDenoisedColors->fill(0.f);
    finalAggregation();

    return true;
}

void Denoiser::computeNbOfSamplesSqrt()
{
    m_nbOfSamplesSqrtImage = *(m_inputs.m_pNbOfSamples);
    for (float* pPixelValues : m_nbOfSamplesSqrtImage)
        pPixelValues[0] = sqrt(pPixelValues[0]);
}

void Denoiser::computePixelCovFromSampleCov()
{
    m_pixelCovarianceImage = *(m_inputs.m_pSampleCovariances);
    ImfIt covIt = m_pixelCovarianceImage.begin();
    float nbOfSamplesInv;
    for (const float* pPixelNbOfSamples : *(m_inputs.m_pNbOfSamples))
    {
        nbOfSamplesInv = 1.f / *pPixelNbOfSamples;
        covIt[0] *= nbOfSamplesInv;
        covIt[1] *= nbOfSamplesInv;
        covIt[2] *= nbOfSamplesInv;
        covIt[3] *= nbOfSamplesInv;
        covIt[4] *= nbOfSamplesInv;
        covIt[5] *= nbOfSamplesInv;
        ++covIt;
    }
}

void Denoiser::reorderPixelSet(vector<PixelPosition>& io_rPixelSet) const
{
    if (m_parameters.m_useRandomPixelOrder)
        reorderPixelSetShuffle(io_rPixelSet);
    else if (m_parameters.m_nbOfCores > 1)
        reorderPixelSetJumpNextStrip(io_rPixelSet);
}

void Denoiser::reorderPixelSetJumpNextStrip(
    vector<PixelPosition>&  io_rPixelSet) const
{
    int widthWithoutBorder = m_width - 2 * m_parameters.m_patchRadius;

    APPLESEED_UNUSED int heightWithoutBorder = m_height - 2 * m_parameters.m_patchRadius;
    APPLESEED_UNUSED int nbOfPixelsWithoutBorder = widthWithoutBorder * heightWithoutBorder;

    assert(nbOfPixelsWithoutBorder == io_rPixelSet.size());

    int chunkSize = widthWithoutBorder * (2 * m_parameters.m_searchWindowRadius);
    // chunkSize is the number of pixels of a strip of 2*searchWindowRadius lines
    reorderPixelSetJumpNextChunk(io_rPixelSet, chunkSize);
}

void Denoiser::reorderPixelSetJumpNextChunk(
    vector<PixelPosition>&  io_rPixelSet,
    int                     i_chunkSize)
{
    int doubleChunkSize = 2 * i_chunkSize;
    int nbOfFullChunks = static_cast<int>(io_rPixelSet.size()) / i_chunkSize;

    vector<PixelPosition> pixelSetCopy(io_rPixelSet);
    vector<PixelPosition>::iterator inputIt = pixelSetCopy.begin();
    vector<PixelPosition>::iterator outputIt = io_rPixelSet.begin();

    for (int chunkIndexStart = 0; chunkIndexStart < 2; ++chunkIndexStart)
    {
        inputIt = pixelSetCopy.begin() + chunkIndexStart * i_chunkSize;
        for (int chunkIndex = chunkIndexStart; chunkIndex < nbOfFullChunks;)
        {
            copy(inputIt, inputIt + i_chunkSize, outputIt);
            outputIt += i_chunkSize;
            chunkIndex += 2;
            if (chunkIndex < nbOfFullChunks)
                inputIt += doubleChunkSize;
        }
    }
}

void reorderPixelSetShuffleCPP11(vector<PixelPosition>& io_rPixelSet)
{
    unsigned seed = static_cast<unsigned>(std::chrono::system_clock::now().time_since_epoch().count());
    shuffle(io_rPixelSet.begin(), io_rPixelSet.end(), std::default_random_engine(seed));
}

void Denoiser::reorderPixelSetShuffle(vector<PixelPosition>& io_rPixelSet)
{
    reorderPixelSetShuffleCPP11(io_rPixelSet);
}

void Denoiser::finalAggregation()
{
    int nbOfImages = static_cast<int>(m_outputSummedColorImages.size());

    for (int bufferIndex = 1; bufferIndex < nbOfImages; ++bufferIndex)
    {
        ImfIt it = m_outputSummedColorImages[bufferIndex].begin();
        for (float* pSum : m_outputSummedColorImages[0])
        {
            pSum[0] += it[0];
            pSum[1] += it[1];
            pSum[2] += it[2];
            ++it;
        }
    }

    for (int bufferIndex = 1; bufferIndex < nbOfImages; ++bufferIndex)
    {
        DeepImage<int>::iterator it = m_estimatesCountImages[bufferIndex].begin();
        for (int* pSum : m_estimatesCountImages[0])
        {
            pSum[0] += it[0];
            ++it;
        }
    }

    ImfIt sumIt = m_outputSummedColorImages[0].begin();
    DeepImage<int>::iterator countIt = m_estimatesCountImages[0].begin();
    float countInv;

    for (float* pFinalColor : *(m_outputs.m_pDenoisedColors))
    {
        countInv = 1.f / countIt[0];
        pFinalColor[0] = countInv * sumIt[0];
        pFinalColor[1] = countInv * sumIt[1];
        pFinalColor[2] = countInv * sumIt[2];
        ++sumIt;
        ++countIt;
    }
}

void Denoiser::fixNegativeInfNaNValues()
{
    const Deepimf& src = *m_inputs.m_pColors;
    Deepimf& dst = *m_outputs.m_pDenoisedColors;

    using std::isnan;
    using std::isinf;
    int width = dst.getWidth();
    int height = dst.getHeight();
    int depth = dst.getDepth();
    vector<float> values(depth);
    float val = 0.f;

    for (int line = 0; line < height; ++line)
    {
        for (int col = 0; col < width; ++col)
        {
            for (int z = 0; z < depth; ++z)
            {
                val = values[z] = dst.get(line, col, z);

                if(val < 0 || isnan(val) || isinf(val))
                {
                    // Recover the original, not denoised value.
                    dst.set(line, col, z, src.get(line, col, z));
                }
            }
        }
    }
}

} // namespace bcd
