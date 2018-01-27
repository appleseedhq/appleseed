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

#ifndef DENOISING_UNIT_H
#define DENOISING_UNIT_H

#include "CovarianceMatrix.h"
#include "DeepImage.h"

#include "Eigen/Dense"
#include "Eigen/Eigenvalues"

#include <array>
#include <memory>
#include <vector>

namespace bcd
{

class PixelPosition;
class Denoiser;

class DenoisingUnit
{
  public:
    DenoisingUnit(Denoiser& i_rDenoiser, const int i_threadIndex);

    void denoisePatchAndSimilarPatches(
        const PixelPosition&                i_rMainPatchCenter);

  private:
    void selectSimilarPatches();

    float histogramPatchDistance(
        const PixelPosition&                i_rPatchCenter1,
        const PixelPosition&                i_rPatchCenter2);

    float pixelSummedHistogramDistance(
        int&                                i_rNbOfNonBoth0Bins,
        const PixelPosition&                i_rPixel1,
        const PixelPosition&                i_rPixel2);

    void denoiseSelectedPatches();
    void computeNoiseCovPatchesMean();
    void denoiseSelectedPatchesStep1();
    void denoiseSelectedPatchesStep2();

    void denoiseOnlyMainPatch();

    void pickColorPatchesFromColorImage(
        std::vector<Eigen::VectorXf>&       o_rColorPatches) const;

    void empiricalMean(
        Eigen::VectorXf&                    o_rMean,
        const std::vector<Eigen::VectorXf>& i_rPointCloud,
        size_t                              i_nbOfPoints) const;

    void centerPointCloud(
        std::vector<Eigen::VectorXf>&       o_rCenteredPointCloud,
        Eigen::VectorXf&                    o_rMean,
        const std::vector<Eigen::VectorXf>& i_rPointCloud,
        size_t                              i_nbOfPoints) const;

    void empiricalCovarianceMatrix(
        Eigen::MatrixXf&                    o_rCovMat,
        const std::vector<Eigen::VectorXf>& i_rCenteredPointCloud,
        size_t                              i_nbOfPoints) const;

    void addCovMatPatchToMatrix(
        Eigen::MatrixXf&                    io_rMatrix,
        const CovMatPatch&                  i_rCovMatPatch) const;

    void substractCovMatPatchFromMatrix(
        Eigen::MatrixXf&                    io_rMatrix,
        const CovMatPatch&                  i_rCovMatPatch) const;

    void inverseSymmetricMatrix(
        Eigen::MatrixXf&                    o_rInversedMatrix,
        const Eigen::MatrixXf&              i_rSymmetricMatrix);

    void clampNegativeEigenValues(
        Eigen::MatrixXf&                    o_rClampedMatrix,
        const Eigen::MatrixXf&              i_rSymmetricMatrix);

    //  o_rVector and i_rVector might be the same
    void multiplyCovMatPatchByVector(
        Eigen::VectorXf&                    o_rVector,
        const CovMatPatch&                  i_rCovMatPatch,
        const Eigen::VectorXf&              i_rVector) const;

    void finalDenoisingMatrixMultiplication(
        std::vector<Eigen::VectorXf>&       o_rDenoisedColorPatches,
        const std::vector<Eigen::VectorXf>& i_rNoisyColorPatches,
        const CovMatPatch&                  i_rNoiseCovMatPatch,
        const Eigen::MatrixXf&              i_rInversedCovMat,
        const std::vector<Eigen::VectorXf>& i_rCenteredNoisyColorPatches);

    void aggregateOutputPatches();

  private:
    typedef Eigen::SelfAdjointEigenSolver<Eigen::MatrixXf> EigenSolver;

    Denoiser&                       m_rDenoiser;
    int                             m_width;
    int                             m_height;

    float                           m_histogramDistanceThreshold;   // Threshold to determine neighbor
                                                                    // patches of similar natures
    int                             m_patchRadius;                  // Patch has (1 + 2 x m_patchRadius)^2 pixels
    int                             m_searchWindowRadius;           // Search windows (for neighbors) spreads across
                                                                    // (1 + 2 x m_patchRadius)^2 pixels

    size_t                          m_nbOfPixelsInPatch;
    size_t                          m_maxNbOfSimilarPatches;
    size_t                          m_colorPatchDimension;

    const Deepimf*                  m_pColorImage;
    const Deepimf*                  m_pNbOfSamplesImage;
    const Deepimf*                  m_pHistogramImage;
    const Deepimf*                  m_pCovarianceImage;

    const Deepimf*                  m_pNbOfSamplesSqrtImage;

    Deepimf*                        m_pOutputSummedColorImage;
    DeepImage<int>*                 m_pEstimatesCountImage;
    DeepImage<bool>*                m_pIsCenterOfAlreadyDenoisedPatchImage; // For the "marking strategy"

    size_t                          m_nbOfBins;

    PixelPosition                   m_mainPatchCenter;
    std::vector<PixelPosition>      m_similarPatchesCenters;

    size_t                          m_nbOfSimilarPatches;
    float                           m_nbOfSimilarPatchesInv;

    CovMatPatch                     m_noiseCovPatchesMean;

    std::vector<Eigen::VectorXf>    m_colorPatches;
    Eigen::VectorXf                 m_colorPatchesMean;
    std::vector<Eigen::VectorXf>    m_centeredColorPatches;
    Eigen::MatrixXf                 m_colorPatchesCovMat;
    Eigen::MatrixXf                 m_clampedCovMat;
    Eigen::MatrixXf                 m_inversedCovMat;
    std::vector<Eigen::VectorXf>    m_denoisedColorPatches;

    EigenSolver                     m_eigenSolver;

    // Temporary auxiliary data
    CovMatPatch                     m_tmpNoiseCovPatch; // Used during computeNoiseCovPatchesMean
    Eigen::VectorXf                 m_tmpVec;           // Used during finalDenoisingMatrixMultiplication
    Eigen::MatrixXf                 m_tmpMatrix;        // Used for inverse and eigen values clamping
};

} // namespace bcd

#endif // DENOISING_UNIT_H
