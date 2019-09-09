// This file is part of the reference implementation for the paper
//   Bayesian Collaborative Denoising for Monte-Carlo Rendering
//   Malik Boughida and Tamy Boubekeur.
//   Computer Graphics Forum (Proc. EGSR 2017), vol. 36, no. 4, p. 137-153, 2017
//
// All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.txt file.

// BCD headers.
#include "bcd/DeepImage.h"
#include "bcd/Denoiser.h"
#include "bcd/IDenoiser.h"
#include "bcd/ImageIO.h"
#include "bcd/MultiscaleDenoiser.h"
#include "bcd/SpikeRemovalFilter.h"
#include "bcd/Utils.h"

// Eigen headers.
#include <Eigen/Dense>

// Standard headers.
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

using namespace bcd;

static const char* g_pProgramPath;

class ProgramArguments
{
  public:
    ProgramArguments() :
        m_denoisedOutputFilePath(""),
        m_colorImage(), m_nbOfSamplesImage(), m_histogramImage(), m_covarianceImage(),
        m_histogramPatchDistanceThreshold(1.f),
        m_patchRadius(1), m_searchWindowRadius(6),
        m_minEigenValue(1.e-8f),
        m_useRandomPixelOrder(false),
        m_prefilterSpikes(false),
        m_prefilterThresholdStDevFactor(2.f),
        m_markedPixelsSkippingProbability(1.f),
        m_nbOfScales(3),
        m_nbOfCores(0)
    {
    }

    std::string m_denoisedOutputFilePath; // File path to the denoised image output
    Deepimf m_colorImage; // Pixel color values
    Deepimf m_nbOfSamplesImage; // Pixel number of samples
    Deepimf m_histogramImage; // Pixel histograms
    Deepimf m_covarianceImage; // Pixel covariances
    float m_histogramPatchDistanceThreshold; // Histogram patch distance threshold
    int m_patchRadius; // Patch has (1 + 2 x m_patchRadius)^2 pixels
    int m_searchWindowRadius; // Search windows (for neighbors) spreads across (1 + 2 x m_patchRadius)^2 pixels
    float m_minEigenValue; // Minimum eigen value for matrix inversion
    bool m_useRandomPixelOrder; // True means the pixel will be processed in a random order ; could be useful to remove some "grid" artifacts
    bool m_prefilterSpikes; // True means a spike removal prefiltering will be applied
    float m_prefilterThresholdStDevFactor; // See SpikeRemovalFilter::filter argument
    float m_markedPixelsSkippingProbability; // 1 means the marked centers of the denoised patches will be skipped to accelerate a lot the computations
    int m_nbOfScales;
    int m_nbOfCores; // Number of cores used by OpenMP. O means using the value defined in environment variable OMP_NUM_THREADS
};

class Callbacks
  : public ICallbacks
{
  public:
    Callbacks()
    {
    }

    void progress(const float i_progress) const override
    {
    }

    bool isAborted() const override
    {
        return false;
    }

  private:
    void logInfo(const char* msg) const override
    {
        std::cout << "Info: " << msg << std::endl;
    }

    void logWarning(const char* msg) const override
    {
        std::cout << "Warning: " << msg << std::endl;
    }

    void logError(const char* msg) const override
    {
        std::cout << "Error: " << msg << std::endl;
    }

    void logDebug(const char* msg) const override
    {
        std::cout << "Debug: " << msg << std::endl;
    }
};

Callbacks g_callbacks;

void initializeRandomSeed()
{
    srand(static_cast<unsigned int>(time(0)));
}

static void printUsage()
{
    ProgramArguments defaultProgramArgs;
    std::cout << "Bayesian Collaborative Denoising"<< std::endl << std::endl;
    std::cout << "Usage: " << g_pProgramPath << " <arguments list>" << std::endl;
    std::cout << "Only EXR images are supported." << std::endl << std::endl;
    std::cout << "Required arguments list:" << std::endl;
    std::cout << "    -o <output>          The file path to the output image" << std::endl;
    std::cout << "    -i <input>           The file path to the input image" << std::endl;
    std::cout << "    -h <hist>            The file path to the input histograms buffer" << std::endl;
    std::cout << "    -c <cov>             The file path to the input covariance matrices buffer" << std::endl;
    std::cout << "Optional arguments list:" << std::endl;
    std::cout << "    -d <float>           Histogram patch distance threshold (default: " << defaultProgramArgs.m_histogramPatchDistanceThreshold << ")" << std::endl;
    std::cout << "    -b <int>             Radius of search windows (default: " << defaultProgramArgs.m_searchWindowRadius << ")" << std::endl;
    std::cout << "    -w <int>             Radius of patches (default: " << defaultProgramArgs.m_patchRadius << ")" << std::endl;
    std::cout << "    -r <0/1>             1 for random pixel order (in case of grid artifacts) (default: " << (defaultProgramArgs.m_useRandomPixelOrder ? 1 : 0) << ")" << std::endl;
    std::cout << "    -p <0/1>             1 for a spike removal prefiltering (default: " << (defaultProgramArgs.m_prefilterSpikes ? 1 : 0) << ")" << std::endl;
    std::cout << "    --p-factor <float>   Factor that is multiplied by standard deviation to get the threshold for classifying spikes during prefiltering. Put lower value to remove more spikes (default: " << defaultProgramArgs.m_prefilterThresholdStDevFactor << ")" << std::endl;
    std::cout << "    -m <float in [0,1]>  Probability of skipping marked centers of denoised patches. 1 accelerates a lot the computations. 0 helps removing potential grid artifacts (default: " << defaultProgramArgs.m_markedPixelsSkippingProbability << ")" << std::endl;
    std::cout << "    -s <int>             Number of Scales for Multi-Scaling (default: " << defaultProgramArgs.m_nbOfScales << ")" << std::endl;
    std::cout << "    --ncores <nbOfCores> Number of cores used by OpenMP (default: environment variable OMP_NUM_THREADS)" << std::endl;
    std::cout << "    -e <float>           Minimum eigen value for matrix inversion (default: " << defaultProgramArgs.m_minEigenValue << ")" << std::endl;
}

bool parseProgramArguments(int argc, const char** argv, ProgramArguments& o_rProgramArguments)
{
    int argIndex = 0;
    bool missingColor = true, missingHist = true, missingCov = true, missingOutput = true;
    std::string inputColorFilePath;
    while (++argIndex < argc)
    {
        if (strcmp(argv[argIndex], "-o") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting file path to the output image after '-o'" << std::endl;
                return false;
            }
            o_rProgramArguments.m_denoisedOutputFilePath = std::string(argv[argIndex]);
            std::ofstream outputFile(o_rProgramArguments.m_denoisedOutputFilePath, std::ofstream::out | std::ofstream::app);
            if(!outputFile)
            {
                std::cout << "Error in program arguments: cannot write output file '" << o_rProgramArguments.m_denoisedOutputFilePath << "'" << std::endl;
                return false;
            }
            missingOutput = false;
        }
        else if (strcmp(argv[argIndex], "-i") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting file path to the input color image after '-i'" << std::endl;
                return false;
            }
            inputColorFilePath = argv[argIndex];
            if (!ImageIO::loadEXR(o_rProgramArguments.m_colorImage, argv[argIndex]))
            {
                std::cout << "Error in program arguments: couldn't load input color image file '" << argv[argIndex] << "'" << std::endl;
                return false;
            }
            missingColor = false;
        }
        else if (strcmp(argv[argIndex], "-h") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting file path to the input histogram image after '-h'" << std::endl;
                return false;
            }
            Deepimf histAndNbOfSamplesImage;
            if (!ImageIO::loadMultiChannelsEXR(histAndNbOfSamplesImage, argv[argIndex]))
            {
                std::cout << "Error in program arguments: couldn't load input histogram image file '" << argv[argIndex] << "'" << std::endl;
                return false;
            }
            Utils::separateNbOfSamplesFromHistogram(o_rProgramArguments.m_histogramImage, o_rProgramArguments.m_nbOfSamplesImage, histAndNbOfSamplesImage);
            missingHist = false;
        }
        else if (strcmp(argv[argIndex], "-c") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting file path to the input covariance matrix image after '-c'" << std::endl;
                return false;
            }
            if (!ImageIO::loadMultiChannelsEXR(o_rProgramArguments.m_covarianceImage, argv[argIndex]))
            {
                std::cout << "Error in program arguments: couldn't load input covariance matrix image file '" << argv[argIndex] << "'" << std::endl;
                return false;
            }
            missingCov = false;
        }
        else if (strcmp(argv[argIndex], "-d") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting histogram patch distance threshold after '-d'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_histogramPatchDistanceThreshold;
        }
        else if (strcmp(argv[argIndex], "-b") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting radius of search window after '-b'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_searchWindowRadius;
        }
        else if (strcmp(argv[argIndex], "-w") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting radius of patch after '-w'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_patchRadius;
        }
        else if (strcmp(argv[argIndex], "-e") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting minimum eigen value after '-e'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_minEigenValue;
        }
        else if (strcmp(argv[argIndex], "-r") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting 0 or 1 after '-r'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            int useRandomPixelOrder;
            iss >> useRandomPixelOrder;
            if(useRandomPixelOrder != 0 && useRandomPixelOrder != 1)
            {
                std::cout << "Error in program arguments: expecting 0 or 1 after '-r'" << std::endl;
                return false;
            }
            o_rProgramArguments.m_useRandomPixelOrder = (useRandomPixelOrder==1);
        }
        else if (strcmp(argv[argIndex], "-p") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting 0 or 1 after '-p'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            int prefilterSpikes;
            iss >> prefilterSpikes;
            if(prefilterSpikes != 0 && prefilterSpikes != 1)
            {
                std::cout << "Error in program arguments: expecting 0 or 1 after '-p'" << std::endl;
                return false;
            }
            o_rProgramArguments.m_prefilterSpikes = (prefilterSpikes==1);
        }
        else if (strcmp(argv[argIndex], "--p-factor") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting standard deviation factor for spike prefiltering threshold after '--p-factor'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_prefilterThresholdStDevFactor;
        }
        else if(strcmp(argv[argIndex], "-m") == 0)
        {
            argIndex++;
            if(argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting float in [0,1] after '-m'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            float markedPixelsSkippingProbability;
            iss >> markedPixelsSkippingProbability;
            if(markedPixelsSkippingProbability < 0 || markedPixelsSkippingProbability > 1)
            {
                std::cout << "Error in program arguments: expecting float in [0,1] after '-m'" << std::endl;
                return false;
            }
            o_rProgramArguments.m_markedPixelsSkippingProbability = markedPixelsSkippingProbability;
        }
        else if(strcmp(argv[argIndex], "-s") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting number of scales after '-s'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_nbOfScales;
        }
        else if (strcmp(argv[argIndex], "--ncores") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                std::cout << "Error in program arguments: expecting number of cores for OpenMP after '--ncores'" << std::endl;
                return false;
            }
            std::istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_nbOfCores;
        }
    }
    if(!missingColor)
    {
        if(missingHist)
        {
            std::string inputHistFilePath = inputColorFilePath.substr(0, inputColorFilePath.length() - 4) + "_hist.exr"; // "-4" for removing extension .exr
            std::cout << "Warning: input histogram file not provided by -h argument: assuming '" + inputHistFilePath + "'" << std::endl;
            Deepimf histAndNbOfSamplesImage;
            if (!ImageIO::loadMultiChannelsEXR(histAndNbOfSamplesImage, inputHistFilePath.c_str()))
            {
                std::cout << "Error in program arguments: couldn't load input histogram image file '" << inputHistFilePath << "'" << std::endl;
                return false;
            }
            Utils::separateNbOfSamplesFromHistogram(o_rProgramArguments.m_histogramImage, o_rProgramArguments.m_nbOfSamplesImage, histAndNbOfSamplesImage);
            missingHist = false;
        }
        if(missingCov)
        {
            std::string inputCovFilePath = inputColorFilePath.substr(0, inputColorFilePath.length() - 4) + "_cov.exr"; // "-4" for removing extension .exr
            std::cout << "Warning: input covariance file not provided by -c argument: assuming '" + inputCovFilePath + "'" << std::endl;
            if (!ImageIO::loadMultiChannelsEXR(o_rProgramArguments.m_covarianceImage, inputCovFilePath.c_str()))
            {
                std::cout << "Error in program arguments: couldn't load input covariance matrix image file '" << inputCovFilePath << "'" << std::endl;
                return false;
            }
            missingCov = false;
        }
    }
    if (missingColor || missingHist || missingCov || missingOutput)
    {
        std::cout << "Error: Missing required program argument(s):";
        if (missingColor)
            std::cout << " -i";
        if (missingHist)
            std::cout << " -h";
        if (missingCov)
            std::cout << " -c";
        if (missingOutput)
            std::cout << " -o";
        std::cout << std::endl << std::endl;
        printUsage();
        return false;
    }
    return true;
}

int launchBayesianCollaborativeDenoising(int argc, const char** argv)
{
    ProgramArguments programArgs;
    if(!parseProgramArguments(argc, argv, programArgs))
        return 1;

    if(programArgs.m_prefilterSpikes)
        SpikeRemovalFilter::filter(
                programArgs.m_colorImage,
                programArgs.m_nbOfSamplesImage,
                programArgs.m_histogramImage,
                programArgs.m_covarianceImage,
                programArgs.m_prefilterThresholdStDevFactor);

    DenoiserInputs inputs;
    DenoiserOutputs outputs;
    DenoiserParameters parameters;

    inputs.m_pColors = &(programArgs.m_colorImage);
    inputs.m_pNbOfSamples = &(programArgs.m_nbOfSamplesImage);
    inputs.m_pHistograms = &(programArgs.m_histogramImage);
    inputs.m_pSampleCovariances = &(programArgs.m_covarianceImage);

    Deepimf outputDenoisedColorImage(programArgs.m_colorImage);
    outputs.m_pDenoisedColors = &outputDenoisedColorImage;

    parameters.m_histogramDistanceThreshold = programArgs.m_histogramPatchDistanceThreshold;
    parameters.m_patchRadius = programArgs.m_patchRadius;
    parameters.m_searchWindowRadius = programArgs.m_searchWindowRadius;
    parameters.m_minEigenValue = programArgs.m_minEigenValue;
    parameters.m_useRandomPixelOrder = programArgs.m_useRandomPixelOrder;
    parameters.m_markedPixelsSkippingProbability = programArgs.m_markedPixelsSkippingProbability;
    parameters.m_nbOfCores = programArgs.m_nbOfCores;

    std::unique_ptr<IDenoiser> uDenoiser = nullptr;

    if(programArgs.m_nbOfScales > 1)
        uDenoiser.reset(new MultiscaleDenoiser(programArgs.m_nbOfScales));
    else
        uDenoiser.reset(new Denoiser());

    uDenoiser->setCallbacks(&g_callbacks);

    uDenoiser->setInputs(inputs);
    uDenoiser->setOutputs(outputs);
    uDenoiser->setParameters(parameters);

    if (!uDenoiser->inputsOutputsAreOk())
        return 1;

    uDenoiser->denoise();

    ImageIO::writeEXR(outputDenoisedColorImage, programArgs.m_denoisedOutputFilePath.c_str());

    std::cout << "Written denoised output in file " << programArgs.m_denoisedOutputFilePath.c_str() << std::endl;

    return 0;
}

int main(int argc, const char** argv)
{
    g_pProgramPath = argv[0];
    return launchBayesianCollaborativeDenoising(argc, argv);
}
