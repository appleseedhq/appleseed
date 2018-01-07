// This file is part of the reference implementation for the paper
//   Bayesian Collaborative Denoising for Monte-Carlo Rendering
//   Malik Boughida and Tamy Boubekeur.
//   Computer Graphics Forum (Proc. EGSR 2017), vol. 36, no. 4, p. 137-153, 2017
//
// All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.txt file.

#include "bcd/Denoiser.h"
#include "bcd/MultiscaleDenoiser.h"
#include "bcd/IDenoiser.h"

#include "bcd/SpikeRemovalFilter.h"

#include "bcd/ImageIO.h"
#include "bcd/DeepImage.h"

#include "bcd/Utils.h"

#include <Eigen/Dense>

#include <iostream>
#include <ctime>
#include <fstream>
#include <sstream>
#include <string>
#include <cstdlib>
#include <memory>

using namespace std;
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
        m_nbOfCores(0),
        m_useCuda(true)
    {
    }

    string m_denoisedOutputFilePath; // File path to the denoised image output
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
    bool m_useCuda; // True means that the program will use Cuda (if available) to parallelize computations
};

void initializeRandomSeed()
{
    srand(static_cast<unsigned int>(time(0)));
}

static void printUsage()
{
    ProgramArguments defaultProgramArgs;
    cout << "Bayesian Collaborative Denoising"<< endl << endl;
    cout << "Usage: " << g_pProgramPath << " <arguments list>" << endl;
    cout << "Only EXR images are supported." << endl << endl;
    cout << "Required arguments list:" << endl;
    cout << "    -o <output>          The file path to the output image" << endl;
    cout << "    -i <input>           The file path to the input image" << endl;
    cout << "    -h <hist>            The file path to the input histograms buffer" << endl;
    cout << "    -c <cov>             The file path to the input covariance matrices buffer" << endl;
    cout << "Optional arguments list:" << endl;
    cout << "    -d <float>           Histogram patch distance threshold (default: " << defaultProgramArgs.m_histogramPatchDistanceThreshold << ")" << endl;
    cout << "    -b <int>             Radius of search windows (default: " << defaultProgramArgs.m_searchWindowRadius << ")" << endl;
    cout << "    -w <int>             Radius of patches (default: " << defaultProgramArgs.m_patchRadius << ")" << endl;
    cout << "    -r <0/1>             1 for random pixel order (in case of grid artifacts) (default: " << (defaultProgramArgs.m_useRandomPixelOrder ? 1 : 0) << ")" << endl;
    cout << "    -p <0/1>             1 for a spike removal prefiltering (default: " << (defaultProgramArgs.m_prefilterSpikes ? 1 : 0) << ")" << endl;
    cout << "    --p-factor <float>   Factor that is multiplied by standard deviation to get the threshold for classifying spikes during prefiltering. Put lower value to remove more spikes (default: " << defaultProgramArgs.m_prefilterThresholdStDevFactor << ")" << endl;
    cout << "    -m <float in [0,1]>  Probability of skipping marked centers of denoised patches. 1 accelerates a lot the computations. 0 helps removing potential grid artifacts (default: " << defaultProgramArgs.m_markedPixelsSkippingProbability << ")" << endl;
    cout << "    -s <int>             Number of Scales for Multi-Scaling (default: " << defaultProgramArgs.m_nbOfScales << ")" << endl;
    cout << "    --ncores <nbOfCores> Number of cores used by OpenMP (default: environment variable OMP_NUM_THREADS)" << endl;
    cout << "    --use-cuda <0/1>     1 to use cuda, 0 not to use it (default: " << (defaultProgramArgs.m_useCuda ? 1 : 0) << ")" << endl;
    cout << "    -e <float>           Minimum eigen value for matrix inversion (default: " << defaultProgramArgs.m_minEigenValue << ")" << endl;
}

bool parseProgramArguments(int argc, const char** argv, ProgramArguments& o_rProgramArguments)
{
    int argIndex = 0;
    bool missingColor = true, missingHist = true, missingCov = true, missingOutput = true;
    string inputColorFilePath;
    while (++argIndex < argc)
    {
        if (strcmp(argv[argIndex], "-o") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting file path to the output image after '-o'" << endl;
                return false;
            }
            o_rProgramArguments.m_denoisedOutputFilePath = string(argv[argIndex]);
            ofstream outputFile(o_rProgramArguments.m_denoisedOutputFilePath, ofstream::out | ofstream::app);
            if(!outputFile)
            {
                cout << "ERROR in program arguments: cannot write output file '" << o_rProgramArguments.m_denoisedOutputFilePath << "'" << endl;
                return false;
            }
            missingOutput = false;
        }
        else if (strcmp(argv[argIndex], "-i") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting file path to the input color image after '-i'" << endl;
                return false;
            }
            inputColorFilePath = argv[argIndex];
            if (!ImageIO::loadEXR(o_rProgramArguments.m_colorImage, argv[argIndex]))
            {
                cout << "ERROR in program arguments: couldn't load input color image file '" << argv[argIndex] << "'" << endl;
                return false;
            }
            missingColor = false;
        }
        else if (strcmp(argv[argIndex], "-h") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting file path to the input histogram image after '-h'" << endl;
                return false;
            }
            Deepimf histAndNbOfSamplesImage;
            if (!ImageIO::loadMultiChannelsEXR(histAndNbOfSamplesImage, argv[argIndex]))
            {
                cout << "ERROR in program arguments: couldn't load input histogram image file '" << argv[argIndex] << "'" << endl;
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
                cout << "ERROR in program arguments: expecting file path to the input covariance matrix image after '-c'" << endl;
                return false;
            }
            if (!ImageIO::loadMultiChannelsEXR(o_rProgramArguments.m_covarianceImage, argv[argIndex]))
            {
                cout << "ERROR in program arguments: couldn't load input covariance matrix image file '" << argv[argIndex] << "'" << endl;
                return false;
            }
            missingCov = false;
        }
        else if (strcmp(argv[argIndex], "-d") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting histogram patch distance threshold after '-d'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_histogramPatchDistanceThreshold;
        }
        else if (strcmp(argv[argIndex], "-b") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting radius of search window after '-b'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_searchWindowRadius;
        }
        else if (strcmp(argv[argIndex], "-w") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting radius of patch after '-w'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_patchRadius;
        }
        else if (strcmp(argv[argIndex], "-e") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting minimum eigen value after '-e'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_minEigenValue;
        }
        else if (strcmp(argv[argIndex], "-r") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting 0 or 1 after '-r'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            int useRandomPixelOrder;
            iss >> useRandomPixelOrder;
            if(useRandomPixelOrder != 0 && useRandomPixelOrder != 1)
            {
                cout << "ERROR in program arguments: expecting 0 or 1 after '-r'" << endl;
                return false;
            }
            o_rProgramArguments.m_useRandomPixelOrder = (useRandomPixelOrder==1);
        }
        else if (strcmp(argv[argIndex], "-p") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting 0 or 1 after '-p'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            int prefilterSpikes;
            iss >> prefilterSpikes;
            if(prefilterSpikes != 0 && prefilterSpikes != 1)
            {
                cout << "ERROR in program arguments: expecting 0 or 1 after '-p'" << endl;
                return false;
            }
            o_rProgramArguments.m_prefilterSpikes = (prefilterSpikes==1);
        }
        else if (strcmp(argv[argIndex], "--p-factor") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting standard deviation factor for spike prefiltering threshold after '--p-factor'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_prefilterThresholdStDevFactor;
        }
        else if(strcmp(argv[argIndex], "-m") == 0)
        {
            argIndex++;
            if(argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting float in [0,1] after '-m'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            float markedPixelsSkippingProbability;
            iss >> markedPixelsSkippingProbability;
            if(markedPixelsSkippingProbability < 0 || markedPixelsSkippingProbability > 1)
            {
                cout << "ERROR in program arguments: expecting float in [0,1] after '-m'" << endl;
                return false;
            }
            o_rProgramArguments.m_markedPixelsSkippingProbability = markedPixelsSkippingProbability;
        }
        else if(strcmp(argv[argIndex], "-s") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting number of scales after '-s'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_nbOfScales;
        }
        else if (strcmp(argv[argIndex], "--ncores") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting number of cores for OpenMP after '--ncores'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            iss >> o_rProgramArguments.m_nbOfCores;
        }
        else if (strcmp(argv[argIndex], "--use-cuda") == 0)
        {
            argIndex++;
            if (argIndex == argc)
            {
                cout << "ERROR in program arguments: expecting 0 or 1 after '--use-cuda'" << endl;
                return false;
            }
            istringstream iss(argv[argIndex]);
            int useCudaCode;
            iss >> useCudaCode;
            if(useCudaCode != 0 && useCudaCode != 1)
            {
                cout << "ERROR in program arguments: expecting 0 or 1 after '--use-cuda'" << endl;
                return false;
            }
            o_rProgramArguments.m_useCuda = (useCudaCode == 1);
        }
    }
    if(!missingColor)
    {
        if(missingHist)
        {
            string inputHistFilePath = inputColorFilePath.substr(0, inputColorFilePath.length() - 4) + "_hist.exr"; // "-4" for removing extension .exr
            cout << "Warning: input histogram file not provided by -h argument: assuming '" + inputHistFilePath + "'" << endl;
            Deepimf histAndNbOfSamplesImage;
            if (!ImageIO::loadMultiChannelsEXR(histAndNbOfSamplesImage, inputHistFilePath.c_str()))
            {
                cout << "ERROR in program arguments: couldn't load input histogram image file '" << inputHistFilePath << "'" << endl;
                return false;
            }
            Utils::separateNbOfSamplesFromHistogram(o_rProgramArguments.m_histogramImage, o_rProgramArguments.m_nbOfSamplesImage, histAndNbOfSamplesImage);
            missingHist = false;
        }
        if(missingCov)
        {
            string inputCovFilePath = inputColorFilePath.substr(0, inputColorFilePath.length() - 4) + "_cov.exr"; // "-4" for removing extension .exr
            cout << "Warning: input covariance file not provided by -c argument: assuming '" + inputCovFilePath + "'" << endl;
            if (!ImageIO::loadMultiChannelsEXR(o_rProgramArguments.m_covarianceImage, inputCovFilePath.c_str()))
            {
                cout << "ERROR in program arguments: couldn't load input covariance matrix image file '" << inputCovFilePath << "'" << endl;
                return false;
            }
            missingCov = false;
        }
    }
    if (missingColor || missingHist || missingCov || missingOutput)
    {
        cout << "ERROR: Missing required program argument(s):";
        if (missingColor)
            cout << " -i";
        if (missingHist)
            cout << " -h";
        if (missingCov)
            cout << " -c";
        if (missingOutput)
            cout << " -o";
        cout << endl << endl;
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
    parameters.m_useCuda = programArgs.m_useCuda;

    unique_ptr<IDenoiser> uDenoiser = nullptr;

    if(programArgs.m_nbOfScales > 1)
        uDenoiser.reset(new MultiscaleDenoiser(programArgs.m_nbOfScales));
    else
        uDenoiser.reset(new Denoiser());

    uDenoiser->setInputs(inputs);
    uDenoiser->setOutputs(outputs);
    uDenoiser->setParameters(parameters);

    uDenoiser->denoise();

    ImageIO::writeEXR(outputDenoisedColorImage, programArgs.m_denoisedOutputFilePath.c_str());
    cout << "Written denoised output in file " << programArgs.m_denoisedOutputFilePath.c_str() << endl;

    return 0;
}

int main(int argc, const char** argv)
{
    g_pProgramPath = argv[0];
    return launchBayesianCollaborativeDenoising(argc, argv);
}
