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
#include "SpikeRemovalFilter.h"

// Standard headers.
#include <cmath>
#include <vector>

using namespace std;

namespace bcd
{

void SpikeRemovalFilter::filter(
    DeepImage<float>&       io_rInputColorImage,
    DeepImage<float>&       io_rInputNbOfSamplesImage,
    DeepImage<float>&       io_rInputHistogramImage,
    DeepImage<float>&       io_rInputCovImage,
    float                   i_thresholdStDevFactor)
{
    const int patchRadius = 1;
    const int patchWidth = 2 * patchRadius + 1;
    const int nbOfNeighbors =  patchWidth * patchWidth;

    DeepImage<float> inputColorImage(io_rInputColorImage);
    DeepImage<float> inputNbOfSamplesImage(io_rInputNbOfSamplesImage);
    DeepImage<float> inputHistogramImage(io_rInputHistogramImage);
    DeepImage<float> inputCovImage(io_rInputCovImage);

    int width = io_rInputColorImage.getWidth();
    int height = io_rInputColorImage.getHeight();

    vector<float> neighborValuesR(nbOfNeighbors);
    vector<float> neighborValuesG(nbOfNeighbors);
    vector<float> neighborValuesB(nbOfNeighbors);
    float average[3];
    float standardDeviation[3];

    for(int line = 0; line < height; ++line)
    {
        for(int column = 0; column < width; ++column)
        {
            int patchCenterLine = line < patchRadius
                ? patchRadius
                : (line > height - 1 - patchRadius ? height - 1 - patchRadius : line);

            int patchCenterColumn = column < patchRadius
                ? patchRadius
                : (column > width - 1 - patchRadius ? width - 1 - patchRadius : column);

            int neighborIndex = 0;

            for(int neighborLine = patchCenterLine - patchRadius; neighborLine <= patchCenterLine + patchRadius; ++neighborLine)
            {
                for(int neighborColumn = patchCenterColumn - patchRadius; neighborColumn <= patchCenterColumn + patchRadius; ++neighborColumn)
                {
                    neighborValuesR[neighborIndex] = inputColorImage.get(neighborLine, neighborColumn, 0);
                    neighborValuesG[neighborIndex] = inputColorImage.get(neighborLine, neighborColumn, 1);
                    neighborValuesB[neighborIndex] = inputColorImage.get(neighborLine, neighborColumn, 2);
                    ++neighborIndex;
                }
            }

            computeAverageAndStandardDeviation(average[0], standardDeviation[0], neighborValuesR);
            computeAverageAndStandardDeviation(average[1], standardDeviation[1], neighborValuesG);
            computeAverageAndStandardDeviation(average[2], standardDeviation[2], neighborValuesB);

            if(
               abs(inputColorImage.get(line, column, 0) - average[0]) > i_thresholdStDevFactor * standardDeviation[0] ||
               abs(inputColorImage.get(line, column, 1) - average[1]) > i_thresholdStDevFactor * standardDeviation[1] ||
               abs(inputColorImage.get(line, column, 2) - average[2]) > i_thresholdStDevFactor * standardDeviation[2])
            { // then it is an outlier (spike) so we copy the neighbor pixel with median color value
                int medianIndex = compute3DMedianIndex(neighborValuesR, neighborValuesG, neighborValuesB);
                int medianLine = patchCenterLine - patchRadius + (medianIndex / patchWidth);
                int medianColumn = patchCenterColumn - patchRadius + (medianIndex % patchWidth);

                io_rInputColorImage.set(line, column, &(inputColorImage.get(medianLine, medianColumn, 0)));
                io_rInputNbOfSamplesImage.set(line, column, &(inputNbOfSamplesImage.get(medianLine, medianColumn, 0)));
                io_rInputHistogramImage.set(line, column, &(inputHistogramImage.get(medianLine, medianColumn, 0)));
                io_rInputCovImage.set(line, column, &(inputCovImage.get(medianLine, medianColumn, 0)));
            }
        }
    }
}

void SpikeRemovalFilter::filter(
    DeepImage<float>&       io_rInputColorImage,
    float                   i_thresholdStDevFactor)
{
    const int patchRadius = 1;
    const int patchWidth = 2 * patchRadius + 1;
    const int nbOfNeighbors =  patchWidth * patchWidth;

    DeepImage<float> inputColorImage(io_rInputColorImage);

    int width = io_rInputColorImage.getWidth();
    int height = io_rInputColorImage.getHeight();

    vector<float> neighborValuesR(nbOfNeighbors);
    vector<float> neighborValuesG(nbOfNeighbors);
    vector<float> neighborValuesB(nbOfNeighbors);
    float average[3];
    float standardDeviation[3];

    for(int line = 0; line < height; ++line)
    {
        for(int column = 0; column < width; ++column)
        {
            int patchCenterLine = line < patchRadius
                ? patchRadius
                : (line > height - 1 - patchRadius ? height - 1 - patchRadius : line);

            int patchCenterColumn = column < patchRadius
                ? patchRadius
                : (column > width - 1 - patchRadius ? width - 1 - patchRadius : column);

            int neighborIndex = 0;

            for(int neighborLine = patchCenterLine - patchRadius; neighborLine <= patchCenterLine + patchRadius; ++neighborLine)
            {
                for(int neighborColumn = patchCenterColumn - patchRadius; neighborColumn <= patchCenterColumn + patchRadius; ++neighborColumn)
                {
                    neighborValuesR[neighborIndex] = inputColorImage.get(neighborLine, neighborColumn, 0);
                    neighborValuesG[neighborIndex] = inputColorImage.get(neighborLine, neighborColumn, 1);
                    neighborValuesB[neighborIndex] = inputColorImage.get(neighborLine, neighborColumn, 2);
                    ++neighborIndex;
                }
            }

            computeAverageAndStandardDeviation(average[0], standardDeviation[0], neighborValuesR);
            computeAverageAndStandardDeviation(average[1], standardDeviation[1], neighborValuesG);
            computeAverageAndStandardDeviation(average[2], standardDeviation[2], neighborValuesB);

            if(
               abs(inputColorImage.get(line, column, 0) - average[0]) > i_thresholdStDevFactor * standardDeviation[0] ||
               abs(inputColorImage.get(line, column, 1) - average[1]) > i_thresholdStDevFactor * standardDeviation[1] ||
               abs(inputColorImage.get(line, column, 2) - average[2]) > i_thresholdStDevFactor * standardDeviation[2])
            { // then it is an outlier (spike) so we copy the neighbor pixel with median color value
                int medianIndex = compute3DMedianIndex(neighborValuesR, neighborValuesG, neighborValuesB);
                int medianLine = patchCenterLine - patchRadius + (medianIndex / patchWidth);
                int medianColumn = patchCenterColumn - patchRadius + (medianIndex % patchWidth);

                io_rInputColorImage.set(line, column, &(inputColorImage.get(medianLine, medianColumn, 0)));
            }
        }
    }
}

void SpikeRemovalFilter::computeAverageAndStandardDeviation(
    float&                  o_rAverage,
    float&                  o_rStandardDeviation,
    const vector<float>&    i_rData)
{
    float total = 0.0f;
    int nbOfElements = static_cast<int>(i_rData.size());

    for (float value : i_rData)
        total += value;

    o_rAverage = total / nbOfElements;

    total = 0.0f;
    for (float value : i_rData)
        total += (value - o_rAverage) * (value - o_rAverage);

    o_rStandardDeviation = sqrt(total / (nbOfElements - 1));
}

int SpikeRemovalFilter::compute3DMedianIndex(
    const vector<float>&    i_rDataR,
    const vector<float>&    i_rDataG,
    const vector<float>&    i_rDataB)
{
    int nbOfElements = static_cast<int>(i_rDataR.size());
    int medianIndex = 0;
    float bestTotalL1Distance = -1.0f;

    for(int medianCandidateIndex = 0; medianCandidateIndex < nbOfElements; ++medianCandidateIndex)
    {
        float totalL1distance = 0.0f;

        for(int currentIndex = 0; currentIndex < nbOfElements; ++currentIndex)
        {
            totalL1distance +=
                abs(i_rDataR[currentIndex] - i_rDataR[medianCandidateIndex]) +
                abs(i_rDataG[currentIndex] - i_rDataG[medianCandidateIndex]) +
                abs(i_rDataB[currentIndex] - i_rDataB[medianCandidateIndex]);
        }

        if(bestTotalL1Distance < 0 || totalL1distance < bestTotalL1Distance)
        {
            bestTotalL1Distance = totalL1distance;
            medianIndex = medianCandidateIndex;
        }
    }

    return medianIndex;
}

} // namespace bcd
