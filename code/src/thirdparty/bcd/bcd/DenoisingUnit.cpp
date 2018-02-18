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
#include "Denoiser.h"
#include "DenoisingUnit.h"

// Standard headers.
#include <cassert>

using namespace std;
using namespace Eigen;

namespace bcd
{

// To shorten notations.
const size_t g_xx = static_cast<size_t>(ESymmetricMatrix3x3Data::e_xx);
const size_t g_yy = static_cast<size_t>(ESymmetricMatrix3x3Data::e_yy);
const size_t g_zz = static_cast<size_t>(ESymmetricMatrix3x3Data::e_zz);
const size_t g_yz = static_cast<size_t>(ESymmetricMatrix3x3Data::e_yz);
const size_t g_xz = static_cast<size_t>(ESymmetricMatrix3x3Data::e_xz);
const size_t g_xy = static_cast<size_t>(ESymmetricMatrix3x3Data::e_xy);

DenoisingUnit::DenoisingUnit(Denoiser& i_rDenoiser, const int i_threadIndex)
  : m_rDenoiser(i_rDenoiser)
  , m_width(i_rDenoiser.getImagesWidth())
  , m_height(i_rDenoiser.getImagesHeight())
  , m_histogramDistanceThreshold(i_rDenoiser.getParameters().m_histogramDistanceThreshold)
  , m_patchRadius(i_rDenoiser.getParameters().m_patchRadius)
  , m_searchWindowRadius(i_rDenoiser.getParameters().m_searchWindowRadius)
  , m_nbOfPixelsInPatch(
        (2 * i_rDenoiser.getParameters().m_patchRadius + 1) *
        (2 * i_rDenoiser.getParameters().m_patchRadius + 1))
  , m_maxNbOfSimilarPatches(
      (2 * i_rDenoiser.getParameters().m_searchWindowRadius + 1) *
      (2 * i_rDenoiser.getParameters().m_searchWindowRadius + 1))
  , m_colorPatchDimension(3 * m_nbOfPixelsInPatch)
  , m_pColorImage(i_rDenoiser.getInputs().m_pColors)
  , m_pNbOfSamplesImage(i_rDenoiser.getInputs().m_pNbOfSamples)
  , m_pHistogramImage(i_rDenoiser.getInputs().m_pHistograms)
  , m_pCovarianceImage(&(i_rDenoiser.getPixelCovarianceImage()))
  , m_pNbOfSamplesSqrtImage(&(i_rDenoiser.getNbOfSamplesSqrtImage()))
  , m_pOutputSummedColorImage(
        &(i_rDenoiser.getOutputSummedColorImage(i_threadIndex)))
  , m_pEstimatesCountImage(
        &(i_rDenoiser.getEstimatesCountImage(i_threadIndex)))
  , m_pIsCenterOfAlreadyDenoisedPatchImage(
        &(i_rDenoiser.getIsCenterOfAlreadyDenoisedPatchImage()))
  , m_nbOfBins(i_rDenoiser.getInputs().m_pHistograms->getDepth())
  , m_mainPatchCenter()
  , m_similarPatchesCenters(m_maxNbOfSimilarPatches)
  , m_nbOfSimilarPatches(0)
  , m_nbOfSimilarPatchesInv(0.0f)
  , m_noiseCovPatchesMean(static_cast<size_t>(m_nbOfPixelsInPatch))
  , m_colorPatches(m_maxNbOfSimilarPatches, VectorXf(m_colorPatchDimension))
  , m_colorPatchesMean(m_colorPatchDimension)
  , m_centeredColorPatches(
        m_maxNbOfSimilarPatches,
        VectorXf(m_colorPatchDimension))
  , m_colorPatchesCovMat(m_colorPatchDimension, m_colorPatchDimension)
  , m_clampedCovMat(m_colorPatchDimension, m_colorPatchDimension)
  , m_inversedCovMat(m_colorPatchDimension, m_colorPatchDimension)
  , m_denoisedColorPatches(
        m_maxNbOfSimilarPatches,
        VectorXf(m_colorPatchDimension))
  , m_eigenSolver(m_colorPatchDimension)
  , m_tmpNoiseCovPatch(static_cast<size_t>(m_nbOfPixelsInPatch))
  , m_tmpVec(m_colorPatchDimension)
  , m_tmpMatrix(m_colorPatchDimension, m_colorPatchDimension)
{
    // Check if we passed the sample count as a channel of the
    // histograms image. If so, do not count it.
    if (m_nbOfBins % 3 != 0)
    {
        --m_nbOfBins;
        assert(m_nbOfBins % 3 == 0);
    }
}

void DenoisingUnit::denoisePatchAndSimilarPatches(
    const PixelPosition&                i_rMainPatchCenter)
{
    m_mainPatchCenter = i_rMainPatchCenter;
    {
        float skippingProbability = m_rDenoiser.getParameters().m_markedPixelsSkippingProbability;
        if (skippingProbability != 0.0f)
        {
            if (m_pIsCenterOfAlreadyDenoisedPatchImage->getValue(m_mainPatchCenter, 0))
            {
                if (skippingProbability == 1.0f || rand() < static_cast<int>(skippingProbability * RAND_MAX))
                    return;
            }
        }
    }

    selectSimilarPatches();

    if (m_nbOfSimilarPatches < m_colorPatchDimension + 1)
    {
        // Cannot inverse covariance matrix: fallback
        // to simple average ; + 1 for safety

        denoiseOnlyMainPatch();
    }
    else denoiseSelectedPatches();
}

void DenoisingUnit::selectSimilarPatches()
{
    m_nbOfSimilarPatches = 0;

    PixelWindow searchWindow(
        m_width,
        m_height,
        m_mainPatchCenter,
        m_searchWindowRadius,
        m_patchRadius);

    m_nbOfSimilarPatches = 0;

    m_similarPatchesCenters.resize(m_maxNbOfSimilarPatches);

    for (PixelPosition neighborPixel : searchWindow)
    {
        const float patchDist = histogramPatchDistance(m_mainPatchCenter, neighborPixel);

        if (patchDist <= m_histogramDistanceThreshold)
            m_similarPatchesCenters[m_nbOfSimilarPatches++] = neighborPixel;
    }

    assert(m_nbOfSimilarPatches > 0);

    m_similarPatchesCenters.resize(m_nbOfSimilarPatches);
    m_nbOfSimilarPatchesInv = 1.0f / m_nbOfSimilarPatches;
}

float DenoisingUnit::histogramPatchDistance(
    const PixelPosition&                i_rPatchCenter1,
    const PixelPosition&                i_rPatchCenter2)
{
    float summedDistance = 0;
    PixelPatch pixPatch1(m_width, m_height, i_rPatchCenter1, m_patchRadius);
    PixelPatch pixPatch2(m_width, m_height, i_rPatchCenter2, m_patchRadius);

    assert(pixPatch1.getSize() == pixPatch2.getSize());

    PixPatchIt pixPatch1It = pixPatch1.begin();
    PixPatchIt pixPatch2It = pixPatch2.begin();
    PixPatchIt pixPatch1ItEnd = pixPatch1.end();
    int totalNbOfNonBoth0Bins = 0;
    int nbOfNonBoth0Bins = 0;

    for (; pixPatch1It != pixPatch1ItEnd; ++pixPatch1It, ++pixPatch2It)
    {
        summedDistance += pixelSummedHistogramDistance(nbOfNonBoth0Bins, *pixPatch1It, *pixPatch2It);
        totalNbOfNonBoth0Bins += nbOfNonBoth0Bins;
    }

    // If the histograms have no bins in common, don't consider them close.
    if (totalNbOfNonBoth0Bins == 0)
        return numeric_limits<float>::max();

    return summedDistance / totalNbOfNonBoth0Bins;
}

float DenoisingUnit::pixelSummedHistogramDistance(
    int&                                i_rNbOfNonBoth0Bins,
    const PixelPosition&                i_rPixel1,
    const PixelPosition&                i_rPixel2)
{
    i_rNbOfNonBoth0Bins = 0;
    const float* pHistogram1Val = &(m_pHistogramImage->get(i_rPixel1, 0));
    const float* pHistogram2Val = &(m_pHistogramImage->get(i_rPixel2, 0));

    float nbOfSamples1 = m_pNbOfSamplesImage->get(i_rPixel1, 0);
    float nbOfSamples2 = m_pNbOfSamplesImage->get(i_rPixel2, 0);

    float sum = 0.0f;

    for (int binIndex = 0; binIndex < m_nbOfBins; ++binIndex)
    {
        const float binValue1 = *pHistogram1Val++;
        const float binValue2 = *pHistogram2Val++;

        if (binValue1 + binValue2 <= 1.0f) // To avoid problems due to small values.
            continue;

        ++i_rNbOfNonBoth0Bins;
        const float diff = nbOfSamples2 * binValue1 - nbOfSamples1 * binValue2;
        sum += diff * diff / (nbOfSamples1 * nbOfSamples2 * (binValue1 + binValue2));
    }

    return sum;
}

void DenoisingUnit::denoiseSelectedPatches()
{
    computeNoiseCovPatchesMean();
    denoiseSelectedPatchesStep1();
    denoiseSelectedPatchesStep2();
    aggregateOutputPatches();
}

void DenoisingUnit::computeNoiseCovPatchesMean()
{
    CovMat3x3 zero3x3;
    zero3x3.m_data.fill(0.0f);
    fill(
        m_noiseCovPatchesMean.m_blocks.begin(),
        m_noiseCovPatchesMean.m_blocks.end(),
        zero3x3);

    for (PixelPosition similarPatchCenter : m_similarPatchesCenters)
    {
        size_t patchPixelIndex = 0;
        ConstPatch patch(*m_pCovarianceImage, similarPatchCenter, m_patchRadius);

        for (const float* pPixelCovData : patch)
            m_tmpNoiseCovPatch.m_blocks[patchPixelIndex++].copyFrom(pPixelCovData);

        m_noiseCovPatchesMean += m_tmpNoiseCovPatch;
    }

    m_noiseCovPatchesMean *= m_nbOfSimilarPatchesInv;
}

void DenoisingUnit::denoiseSelectedPatchesStep1()
{
    pickColorPatchesFromColorImage(m_colorPatches);
    empiricalMean(m_colorPatchesMean, m_colorPatches, m_nbOfSimilarPatches);

    centerPointCloud(
        m_centeredColorPatches,
        m_colorPatchesMean,
        m_colorPatches,
        m_nbOfSimilarPatches);

    empiricalCovarianceMatrix(
        m_colorPatchesCovMat, m_centeredColorPatches, m_nbOfSimilarPatches);

    substractCovMatPatchFromMatrix(m_colorPatchesCovMat, m_noiseCovPatchesMean);
    clampNegativeEigenValues(m_clampedCovMat, m_colorPatchesCovMat);
    addCovMatPatchToMatrix(m_clampedCovMat, m_noiseCovPatchesMean);
    inverseSymmetricMatrix(m_inversedCovMat, m_clampedCovMat);

    finalDenoisingMatrixMultiplication(
        m_denoisedColorPatches,
        m_colorPatches,
        m_noiseCovPatchesMean,
        m_inversedCovMat,
        m_centeredColorPatches);
}

void DenoisingUnit::denoiseSelectedPatchesStep2()
{
    empiricalMean(
        m_colorPatchesMean, m_denoisedColorPatches, m_nbOfSimilarPatches);

    centerPointCloud(
        m_centeredColorPatches,
        m_colorPatchesMean,
        m_denoisedColorPatches,
        m_nbOfSimilarPatches);

    empiricalCovarianceMatrix(
        m_colorPatchesCovMat, m_centeredColorPatches, m_nbOfSimilarPatches);

    m_clampedCovMat = m_colorPatchesCovMat;

    addCovMatPatchToMatrix(m_clampedCovMat, m_noiseCovPatchesMean);
    inverseSymmetricMatrix(m_inversedCovMat, m_clampedCovMat);

    centerPointCloud(
        m_centeredColorPatches,
        m_colorPatchesMean,
        m_colorPatches,
        m_nbOfSimilarPatches);

    finalDenoisingMatrixMultiplication(
        m_denoisedColorPatches,
        m_colorPatches,
        m_noiseCovPatchesMean,
        m_inversedCovMat,
        m_centeredColorPatches);
}

void DenoisingUnit::denoiseOnlyMainPatch()
{
    m_colorPatchesMean.fill(0.0f);
    int patchDataIndex = 0;

    for (const PixelPosition& rSimilarPatchCenter : m_similarPatchesCenters)
    {
        patchDataIndex = 0;
        ConstPatch patch(*m_pColorImage, rSimilarPatchCenter, m_patchRadius);

        for (const float* pPixelColorData : patch)
        {
            m_colorPatchesMean(patchDataIndex++) += pPixelColorData[0];
            m_colorPatchesMean(patchDataIndex++) += pPixelColorData[1];
            m_colorPatchesMean(patchDataIndex++) += pPixelColorData[2];
        }
    }

    Patch outputMainPatch(*m_pOutputSummedColorImage, m_mainPatchCenter, m_patchRadius);
    patchDataIndex = 0;

    for (float* pPixelOutputColorData : outputMainPatch)
    {
        pPixelOutputColorData[0] += m_nbOfSimilarPatchesInv * m_colorPatchesMean(patchDataIndex++);
        pPixelOutputColorData[1] += m_nbOfSimilarPatchesInv * m_colorPatchesMean(patchDataIndex++);
        pPixelOutputColorData[2] += m_nbOfSimilarPatchesInv * m_colorPatchesMean(patchDataIndex++);
    }

    ImageWindow<int> estimatesCountPatch(
        *m_pEstimatesCountImage, m_mainPatchCenter, m_patchRadius);

    for (int* pPixelEstimateCount : estimatesCountPatch)
        ++(pPixelEstimateCount[0]);
}

void DenoisingUnit::pickColorPatchesFromColorImage(
        vector<VectorXf>&               o_rColorPatches) const
{
    int patchIndex = 0;

    for (const PixelPosition& rSimilarPatchCenter : m_similarPatchesCenters)
    {
        VectorXf& rColorPatch = o_rColorPatches[patchIndex++];
        int patchDataIndex = 0;
        ConstPatch patch(*m_pColorImage, rSimilarPatchCenter, m_patchRadius);

        for (const float* pPixelColorData : patch)
        {
            rColorPatch(patchDataIndex++) = pPixelColorData[0];
            rColorPatch(patchDataIndex++) = pPixelColorData[1];
            rColorPatch(patchDataIndex++) = pPixelColorData[2];
        }
    }
}

void DenoisingUnit::empiricalMean(
    VectorXf&                           o_rMean,
    const vector<VectorXf>&             i_rPointCloud,
    size_t                              i_nbOfPoints) const
{
    o_rMean.fill(0.0f);

    for (size_t i = 0; i < i_nbOfPoints; ++i)
        o_rMean += i_rPointCloud[i];

    o_rMean *= 1.0f / i_nbOfPoints;
}

void DenoisingUnit::centerPointCloud(
    vector<VectorXf>&                   o_rCenteredPointCloud,
    VectorXf&                           o_rMean,
    const vector<VectorXf>&             i_rPointCloud,
    size_t                              i_nbOfPoints) const
{
    vector<VectorXf>::iterator it = o_rCenteredPointCloud.begin();

    for (size_t i = 0; i < i_nbOfPoints; ++i)
        *it++ = i_rPointCloud[i] - o_rMean;
}

void DenoisingUnit::empiricalCovarianceMatrix(
    MatrixXf&                           o_rCovMat,
    const vector<VectorXf>&             i_rCenteredPointCloud,
    size_t                              i_nbOfPoints) const
{
    int d = static_cast<int>(o_rCovMat.rows());
    assert(d == o_rCovMat.cols());
    assert(d == i_rCenteredPointCloud[0].rows());
    o_rCovMat.fill(0.0f);

    for (size_t i = 0; i < i_nbOfPoints; ++i)
    {
        for (int c = 0; c < d; ++c)
        {
            for (int r = 0; r < d; ++r)
                o_rCovMat(r, c) += i_rCenteredPointCloud[i](r) * i_rCenteredPointCloud[i](c);
        }
    }

    o_rCovMat *= 1.0f / (i_nbOfPoints - 1);
}

void DenoisingUnit::addCovMatPatchToMatrix(
    MatrixXf&                           io_rMatrix,
    const CovMatPatch&                  i_rCovMatPatch) const
{
    int blockXIndex = 0, blockYIndex = 1, blockZIndex = 2;
    for (const CovMat3x3& rCovMat3x3 : i_rCovMatPatch.m_blocks)
    {
        io_rMatrix(blockXIndex, blockXIndex) += rCovMat3x3.m_data[g_xx];
        io_rMatrix(blockYIndex, blockYIndex) += rCovMat3x3.m_data[g_yy];
        io_rMatrix(blockZIndex, blockZIndex) += rCovMat3x3.m_data[g_zz];
        io_rMatrix(blockYIndex, blockZIndex) += rCovMat3x3.m_data[g_yz];
        io_rMatrix(blockZIndex, blockYIndex) += rCovMat3x3.m_data[g_yz];
        io_rMatrix(blockXIndex, blockZIndex) += rCovMat3x3.m_data[g_xz];
        io_rMatrix(blockZIndex, blockXIndex) += rCovMat3x3.m_data[g_xz];
        io_rMatrix(blockXIndex, blockYIndex) += rCovMat3x3.m_data[g_xy];
        io_rMatrix(blockYIndex, blockXIndex) += rCovMat3x3.m_data[g_xy];
        blockXIndex += 3;
        blockYIndex += 3;
        blockZIndex += 3;
    }
}

void DenoisingUnit::substractCovMatPatchFromMatrix(
    MatrixXf&                           io_rMatrix,
    const CovMatPatch&                  i_rCovMatPatch) const
{
    int blockXIndex = 0, blockYIndex = 1, blockZIndex = 2;
    for (const CovMat3x3& rCovMat3x3 : i_rCovMatPatch.m_blocks)
    {
        io_rMatrix(blockXIndex, blockXIndex) -= rCovMat3x3.m_data[g_xx];
        io_rMatrix(blockYIndex, blockYIndex) -= rCovMat3x3.m_data[g_yy];
        io_rMatrix(blockZIndex, blockZIndex) -= rCovMat3x3.m_data[g_zz];
        io_rMatrix(blockYIndex, blockZIndex) -= rCovMat3x3.m_data[g_yz];
        io_rMatrix(blockZIndex, blockYIndex) -= rCovMat3x3.m_data[g_yz];
        io_rMatrix(blockXIndex, blockZIndex) -= rCovMat3x3.m_data[g_xz];
        io_rMatrix(blockZIndex, blockXIndex) -= rCovMat3x3.m_data[g_xz];
        io_rMatrix(blockXIndex, blockYIndex) -= rCovMat3x3.m_data[g_xy];
        io_rMatrix(blockYIndex, blockXIndex) -= rCovMat3x3.m_data[g_xy];
        blockXIndex += 3;
        blockYIndex += 3;
        blockZIndex += 3;
    }
}

void DenoisingUnit::inverseSymmetricMatrix(
    MatrixXf&                           o_rInversedMatrix,
    const MatrixXf&                     i_rSymmetricMatrix)
{
    float minEigenVal = m_rDenoiser.getParameters().m_minEigenValue;

    int d = static_cast<int>(i_rSymmetricMatrix.rows());
    assert(d == i_rSymmetricMatrix.cols());
    assert(d == o_rInversedMatrix.rows());
    assert(d == o_rInversedMatrix.cols());
    assert(d == m_tmpMatrix.rows());
    assert(d == m_tmpMatrix.cols());

    m_eigenSolver.compute(i_rSymmetricMatrix);                      // Decomposes i_rSymmetricMatrix into V D V^T
    const MatrixXf& rEigenVectors = m_eigenSolver.eigenvectors();   // Matrix V
    const VectorXf& rEigenValues = m_eigenSolver.eigenvalues();     // Matrix D is rEigenValues.asDiagonal()

    float diagValue;
    for (int r = 0; r < d; ++r)
    {
        diagValue = 1.0f / max(minEigenVal, rEigenValues(r));

        for (int c = 0; c < d; ++c)
            m_tmpMatrix(r, c) = diagValue * rEigenVectors(c, r);
    }

    // now m_tmpMatrix equals (D^-1) V^T
    o_rInversedMatrix = rEigenVectors * m_tmpMatrix;
}

void DenoisingUnit::clampNegativeEigenValues(
    MatrixXf&                           o_rClampedMatrix,
    const MatrixXf&                     i_rSymmetricMatrix)
{
    float minEigenVal = 0;

    int d = static_cast<int>(i_rSymmetricMatrix.rows());
    assert(d == i_rSymmetricMatrix.cols());
    assert(d == o_rClampedMatrix.rows());
    assert(d == o_rClampedMatrix.cols());
    assert(d == m_tmpMatrix.rows());
    assert(d == m_tmpMatrix.cols());

    m_eigenSolver.compute(i_rSymmetricMatrix);                      // Decomposes i_rSymmetricMatrix into V D V^T
    const MatrixXf& rEigenVectors = m_eigenSolver.eigenvectors();   // Matrix V
    const VectorXf& rEigenValues = m_eigenSolver.eigenvalues();     // Matrix D is rEigenValues.asDiagonal()

    float diagValue;
    for (int r = 0; r < d; ++r)
    {
        diagValue = max(minEigenVal, rEigenValues(r));
        for (int c = 0; c < d; ++c)
            m_tmpMatrix(r, c) = diagValue * rEigenVectors(c, r);
    }
    // now m_tmpMatrix equals (D^-1) V^T
    o_rClampedMatrix = rEigenVectors * m_tmpMatrix;
}

//  o_rVector and i_rVector might be the same
void DenoisingUnit::multiplyCovMatPatchByVector(
    VectorXf&                           o_rVector,
    const CovMatPatch&                  i_rCovMatPatch,
    const VectorXf&                     i_rVector) const
{
    int blockXIndex = 0, blockYIndex = 1, blockZIndex = 2;
    for (const CovMat3x3& rCovMat3x3 : i_rCovMatPatch.m_blocks)
    {
        o_rVector(blockXIndex) =
            rCovMat3x3.m_data[g_xx] * i_rVector(blockXIndex) +
            rCovMat3x3.m_data[g_xy] * i_rVector(blockYIndex) +
            rCovMat3x3.m_data[g_xz] * i_rVector(blockZIndex);

        o_rVector(blockYIndex) =
            rCovMat3x3.m_data[g_xy] * i_rVector(blockXIndex) +
            rCovMat3x3.m_data[g_yy] * i_rVector(blockYIndex) +
            rCovMat3x3.m_data[g_yz] * i_rVector(blockZIndex);

        o_rVector(blockZIndex) =
            rCovMat3x3.m_data[g_xz] * i_rVector(blockXIndex) +
            rCovMat3x3.m_data[g_yz] * i_rVector(blockYIndex) +
            rCovMat3x3.m_data[g_zz] * i_rVector(blockZIndex);

        blockXIndex += 3;
        blockYIndex += 3;
        blockZIndex += 3;
    }
}

void DenoisingUnit::finalDenoisingMatrixMultiplication(
    std::vector<Eigen::VectorXf>&       o_rDenoisedColorPatches,
    const std::vector<Eigen::VectorXf>& i_rNoisyColorPatches,
    const CovMatPatch&                  i_rNoiseCovMatPatch,
    const Eigen::MatrixXf&              i_rInversedCovMat,
    const std::vector<Eigen::VectorXf>& i_rCenteredNoisyColorPatches)
{
    for (size_t i = 0; i < m_nbOfSimilarPatches; ++i)
    {
        m_tmpVec = i_rInversedCovMat * i_rCenteredNoisyColorPatches[i];
        m_tmpVec *= -1.0f;

        multiplyCovMatPatchByVector(o_rDenoisedColorPatches[i], i_rNoiseCovMatPatch, m_tmpVec);
        o_rDenoisedColorPatches[i] += i_rNoisyColorPatches[i];
    }
}

void DenoisingUnit::aggregateOutputPatches()
{
    size_t patchIndex = 0;

    for (const PixelPosition& rSimilarPatchCenter : m_similarPatchesCenters)
    {
        VectorXf& rColorPatch = m_denoisedColorPatches[patchIndex++];
        int patchDataIndex = 0;
        Patch outputPatch(*m_pOutputSummedColorImage, rSimilarPatchCenter, m_patchRadius);

        for (float* pPixelColorData : outputPatch)
        {
            pPixelColorData[0] += rColorPatch(patchDataIndex++);
            pPixelColorData[1] += rColorPatch(patchDataIndex++);
            pPixelColorData[2] += rColorPatch(patchDataIndex++);
        }

        ImageWindow<int> estimatesCountPatch(
            *m_pEstimatesCountImage, rSimilarPatchCenter, m_patchRadius);

        for (int* pPixelEstimateCount : estimatesCountPatch)
            ++(pPixelEstimateCount[0]);

        m_pIsCenterOfAlreadyDenoisedPatchImage->set(rSimilarPatchCenter, 0, true);
    }
}

} // namespace bcd
