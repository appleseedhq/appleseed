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

#ifndef I_DENOISER_H
#define I_DENOISER_H

#include <functional>
#include <memory>

namespace bcd
{

template<class T>
class DeepImage;

struct DenoiserParameters
{
    DenoiserParameters()
        : m_histogramDistanceThreshold(1.0f)
        , m_patchRadius(1)
        , m_searchWindowRadius(6)
        , m_minEigenValue(0.00000001f), // (0.0001f) TEMPORARILY CHANGED
          m_useRandomPixelOrder(false)
        , m_markedPixelsSkippingProbability(1.0f)
        , m_nbOfCores(0)
        , m_useCuda(true)
    {
    }

    float m_histogramDistanceThreshold;      // Threshold to determine neighbor patches of similar natures
    int   m_patchRadius;                     // Patch has (1 + 2 x m_patchRadius)^2 pixels
    int   m_searchWindowRadius;              // Search windows (for neighbors) spreads across (1 + 2 x m_patchRadius)^2 pixels
    float m_minEigenValue;                   // Small positive value which serves as a minimum for eigen value clamping and matrix inversing
    bool  m_useRandomPixelOrder;
    float m_markedPixelsSkippingProbability;
    int   m_nbOfCores;                       // Number of cores used by OpenMP; 0 means using the default value, defined in environment variable OMP_NUM_THREADS
    bool  m_useCuda;
};

struct DenoiserInputs
{
    DenoiserInputs()
      : m_pColors(nullptr)
      , m_pNbOfSamples(nullptr)
      , m_pHistograms(nullptr)
      , m_pSampleCovariances(nullptr)
    {
    }

    const DeepImage<float>* m_pColors;            // Pixel color values
    const DeepImage<float>* m_pNbOfSamples;       // Pixel number of samples
    const DeepImage<float>* m_pHistograms;        // Pixel histograms
    const DeepImage<float>* m_pSampleCovariances; // Pixel covariances
};

struct DenoiserOutputs
{
    DenoiserOutputs()
      : m_pDenoisedColors(nullptr)
    {
    }

    DeepImage<float>* m_pDenoisedColors; // Pixel denoised color values
};

//  Interface class for monoscale and multiscale Bayesian Collaborative
//  Filtering for Monte-Carlo Rendering
class IDenoiser
{
  public:
    IDenoiser();

    virtual ~IDenoiser();

    virtual bool inputsOutputsAreOk() const;

    virtual bool denoise() = 0;

    const DenoiserInputs& getInputs() const
    {
        return m_inputs;
    }

    void setInputs(const DenoiserInputs& i_rInputs)
    {
        m_inputs = i_rInputs;
    }

    const DenoiserOutputs& getOutputs() const
    {
        return m_outputs;
    }

    void setOutputs(const DenoiserOutputs& i_rOutputs)
    {
        m_outputs = i_rOutputs;
    }

    const DenoiserParameters& getParameters() const
    {
        return m_parameters;
    }

    void setParameters(const DenoiserParameters& i_rParameters)
    {
        m_parameters = i_rParameters;
    }

    void setProgressCallback(std::function<void(float)> i_progressCallback)
    {
        m_progressCallback = i_progressCallback;
    }

  protected:
    DenoiserParameters          m_parameters;
    DenoiserInputs              m_inputs;
    DenoiserOutputs             m_outputs;
    std::function<void(float)>  m_progressCallback;
};

} // namespace bcd

#endif // I_DENOISER_H
