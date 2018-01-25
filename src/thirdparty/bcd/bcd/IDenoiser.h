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

// Standard headers.
#include <cstddef>
#include <functional>
#include <sstream>

namespace bcd
{

template<class T>
class DeepImage;

class ICallbacks
{
  public:
    ICallbacks();

    virtual ~ICallbacks();

    virtual void progress(const float i_progress) const = 0;

    virtual bool isAborted() const = 0;

    // Logging.

    class LogHelper
    {
      public:
        LogHelper(
            std::stringstream&                      ss,
            const std::function<void(const char*)>& log_fn);

        ~LogHelper();

        template <typename T>
        LogHelper& operator<<(const T& x)
        {
            m_ss << x;
            return *this;
        }

      private:
        std::stringstream&               m_ss;
        std::function<void(const char*)> m_logFn;
    };

    LogHelper info();
    LogHelper warning();
    LogHelper error();
    LogHelper debug();

  private:
    // Non-copyable.
    ICallbacks(const ICallbacks&);
    ICallbacks& operator=(const ICallbacks&);

    virtual void logInfo(const char* i_msg) const = 0;
    virtual void logWarning(const char* i_msg) const = 0;
    virtual void logError(const char* i_msg) const = 0;
    virtual void logDebug(const char* i_msg) const = 0;

    std::stringstream m_ss;
};

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
        , m_markInvalidPixels(false)
    {
    }

    float m_histogramDistanceThreshold;      // Threshold to determine neighbor patches of similar natures
    int   m_patchRadius;                     // Patch has (1 + 2 x m_patchRadius)^2 pixels
    int   m_searchWindowRadius;              // Search windows (for neighbors) spreads across (1 + 2 x m_patchRadius)^2 pixels
    float m_minEigenValue;                   // Small positive value which serves as a minimum for eigen value clamping and matrix inversing
    bool  m_useRandomPixelOrder;
    float m_markedPixelsSkippingProbability;
    int   m_nbOfCores;                       // Number of cores to use; 0 means using all the cores
    bool  m_markInvalidPixels;
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

    void checkImages(
        size_t& o_nbInvalidColors,
        size_t& o_nbInvalidNbSamplesCout,
        size_t& o_nbInvalidHistograms,
        size_t& o_nbInvalidCovariances) const;

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

    bool inputsOutputsAreOk() const;

    virtual bool denoise() = 0;

    const DenoiserInputs& getInputs() const
    {
        return m_inputs;
    }

    virtual void setInputs(const DenoiserInputs& i_rInputs)
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

    ICallbacks* getCallbacks() const
    {
        return m_callbacks;
    }

    void setCallbacks(ICallbacks* i_callbacks)
    {
        m_callbacks = i_callbacks;
    }

  protected:
    DenoiserParameters   m_parameters;
    DenoiserInputs       m_inputs;
    DenoiserOutputs      m_outputs;
    ICallbacks*          m_callbacks;
};

} // namespace bcd

#endif // I_DENOISER_H
