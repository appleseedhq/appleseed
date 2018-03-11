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
#include "Utils.h"

using namespace std;
using namespace std::placeholders;

namespace bcd
{

ICallbacks::LogHelper::LogHelper(
    stringstream&                       ss,
    const function<void(const char*)>&  logFn)
  : m_ss(ss)
  , m_logFn(logFn)
{
    m_ss.str("");
    m_ss.clear();
}

ICallbacks::LogHelper::~LogHelper()
{
    string msg = m_ss.str();

    if (!msg.empty())
        m_logFn(msg.c_str()) ;
}

ICallbacks::ICallbacks()
{
}

ICallbacks::~ICallbacks()
{
}

ICallbacks::LogHelper ICallbacks::info()
{
    return LogHelper(m_ss, bind(&ICallbacks::logInfo, this, _1));
}

ICallbacks::LogHelper ICallbacks::warning()
{
    return LogHelper(m_ss, bind(&ICallbacks::logWarning, this, _1));
}

ICallbacks::LogHelper ICallbacks::error()
{
    return LogHelper(m_ss, bind(&ICallbacks::logError, this, _1));
}

ICallbacks::LogHelper ICallbacks::debug()
{
    return LogHelper(m_ss, bind(&ICallbacks::logDebug, this, _1));
}

namespace
{
    void checkValue(
        const float i_val,
        size_t&     o_invalidCounter)
    {
        if (!isFinite(i_val))
            ++o_invalidCounter;
        else if (i_val < 0.0f)
            ++o_invalidCounter;
    }
}

void DenoiserInputs::checkImages(
    size_t& o_nbInvalidColors,
    size_t& o_nbInvalidNbSamplesCout,
    size_t& o_nbInvalidHistograms,
    size_t& o_nbInvalidCovariances) const
{
    o_nbInvalidColors = 0;
    o_nbInvalidNbSamplesCout = 0;
    o_nbInvalidHistograms = 0;
    o_nbInvalidCovariances = 0;

    const int w = m_pNbOfSamples->getWidth();
    const int h = m_pNbOfSamples->getHeight();
    const int numBins = m_pNbOfSamples->getDepth() - 1;

    for (int j = 0; j < h; ++j)
    {
        for (int i = 0; i < w; ++i)
        {
            {
                const float val = m_pNbOfSamples->get(j, i, 0);

                if (!isFinite(val))
                    ++o_nbInvalidNbSamplesCout;

                if (val < 1.0f)
                    ++o_nbInvalidNbSamplesCout;
            }

            for (int k = 0; k < numBins; ++k)
            {
                const float val = m_pHistograms->get(j, i, k);
                checkValue(val, o_nbInvalidHistograms);
            }

            for (int k = 0; k < 3; ++k)
            {
                const float val = m_pColors->get(j, i, k);
                checkValue(val, o_nbInvalidColors);
            }

            // Covariance matrix diagonal.
            for (int k = 0; k < 3; ++k)
            {
                const float val = m_pSampleCovariances->get(j, i, k);
                checkValue(val, o_nbInvalidCovariances);
            }

            // Off diagonal covariance entries can be negative.
            for (int k = 3; k < 6; ++k)
            {
                const float val = m_pSampleCovariances->get(j, i, k);
                if (!isFinite(val))
                    ++o_nbInvalidCovariances;
            }
        }
    }
}

IDenoiser::IDenoiser()
  : m_callbacks(nullptr)
{
}

IDenoiser::~IDenoiser()
{
}

namespace
{
    void reportInvalidInput(
        ICallbacks*   i_callbacks,
        const char*   i_category,
        const size_t  i_nbInvalid)
    {
        if (i_nbInvalid != 0 && i_callbacks)
        {
            i_callbacks->warning()
                << i_nbInvalid << " invalid " << i_category << " found while checking inputs.\n";
        }
    }
}

bool IDenoiser::inputsOutputsAreOk() const
{
    {
        bool imageNullptr = false;
        if (!m_inputs.m_pColors)
        {
            imageNullptr = true;

            if (m_callbacks)
                m_callbacks->error() << "Aborting denoising: nullptr for input color image\n";
        }

        if (!m_inputs.m_pNbOfSamples)
        {
            imageNullptr = true;

            if (m_callbacks)
                m_callbacks->error() << "Aborting denoising: nullptr for input number of samples image\n";
        }

        if (!m_inputs.m_pHistograms)
        {
            imageNullptr = true;
            if (m_callbacks)
                m_callbacks->error() << "Aborting denoising: nullptr for input histogram image\n";
        }

        if (!m_inputs.m_pSampleCovariances)
        {
            imageNullptr = true;
            if (m_callbacks)
                m_callbacks->error() << "Aborting denoising: nullptr for input covariance image\n";
        }

        if (imageNullptr)
            return false;
    }

    {
        bool emptyInput = false;
        if (m_inputs.m_pColors->isEmpty())
        {
            emptyInput = true;
            if (m_callbacks)
                m_callbacks->error() << "Aborting denoising: input color image is empty\n";
        }

        if (m_inputs.m_pNbOfSamples->isEmpty())
        {
            emptyInput = true;
            if (m_callbacks)
                m_callbacks->error() << "Aborting denoising: input number of samples image is empty\n";
        }

        if (m_inputs.m_pHistograms->isEmpty())
        {
            emptyInput = true;
            if (m_callbacks)
                m_callbacks->error() << "Aborting denoising: input histogram image is empty\n";
        }

        if (m_inputs.m_pSampleCovariances->isEmpty())
        {
            emptyInput = true;
            if (m_callbacks)
                m_callbacks->error() << "Aborting denoising: input covariance image is empty\n";
        }

        if (emptyInput)
            return false;
    }

    {
        int w = m_inputs.m_pColors->getWidth();
        int h = m_inputs.m_pColors->getHeight();
        bool badImageSize = false;

        if (m_inputs.m_pNbOfSamples->getWidth() != w || m_inputs.m_pNbOfSamples->getHeight() != h)
        {
            badImageSize = true;
            if (m_callbacks)
            {
                m_callbacks->error()
                    << "Aborting denoising: input number of samples image is "
                    << m_inputs.m_pNbOfSamples->getWidth() << "x"
                    << m_inputs.m_pNbOfSamples->getHeight()
                    << " but input color image is " << w << "x" << h << "\n";
            }
        }

        if (m_inputs.m_pHistograms->getWidth() != w || m_inputs.m_pHistograms->getHeight() != h)
        {
            badImageSize = true;
            if (m_callbacks)
            {
                m_callbacks->error()
                    << "Aborting denoising: input histogram image is "
                    << m_inputs.m_pHistograms->getWidth() << "x"
                    << m_inputs.m_pHistograms->getHeight()
                    << " but input color image is " << w << "x" << h << "\n";
            }
        }

        const int histExtraChannels = m_inputs.m_pHistograms->getDepth() % 3;
        if (histExtraChannels > 1)
        {
            badImageSize = true;

            if (m_callbacks)
            {
                m_callbacks->error()
                    << "Aborting denoising: input histogram image is "
                    << m_inputs.m_pHistograms->getWidth() << "x"
                    << m_inputs.m_pHistograms->getHeight()
                    << " but input color image is " << w << "x" << h << "\n";
            }
        }

        if (m_inputs.m_pSampleCovariances->getWidth() != w || m_inputs.m_pSampleCovariances->getHeight() != h)
        {
            badImageSize = true;

            if (m_callbacks)
            {
                m_callbacks->error()
                    << "Aborting denoising: input covariance image is "
                    << m_inputs.m_pSampleCovariances->getWidth() << "x"
                    << m_inputs.m_pSampleCovariances->getHeight()
                    << " but input color image is " << w << "x" << h << "\n";
            }
        }

        if (badImageSize)
            return false;
    }

#ifndef NDEBUG
    size_t nbInvalidColors;
    size_t nbInvalidNbSamplesCout;
    size_t nbInvalidHistograms;
    size_t nbInvalidCovariances;

    m_inputs.checkImages(
        nbInvalidColors,
        nbInvalidNbSamplesCout,
        nbInvalidHistograms,
        nbInvalidCovariances);

    reportInvalidInput(m_callbacks, "colors", nbInvalidColors);
    reportInvalidInput(m_callbacks, "sample counts", nbInvalidNbSamplesCout);
    reportInvalidInput(m_callbacks, "histograms", nbInvalidHistograms);
    reportInvalidInput(m_callbacks, "covariances", nbInvalidCovariances);
#endif

    return true;
}

} // namespace bcd
