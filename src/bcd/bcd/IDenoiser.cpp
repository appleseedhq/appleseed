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

using namespace std;

namespace bcd
{

IProgressReporter::IProgressReporter()
{
}

IProgressReporter::~IProgressReporter()
{
}

IDenoiser::IDenoiser()
  : m_progressReporter(nullptr)
{
}

IDenoiser::~IDenoiser()
{
}

bool IDenoiser::inputsOutputsAreOk() const
{
    {
        bool imageNullptr = false;
        if (!m_inputs.m_pColors)
        {
            imageNullptr = true;
            //cerr << "Aborting denoising: nullptr for input color image" << endl;
        }
        if (!m_inputs.m_pNbOfSamples)
        {
            imageNullptr = true;
            //cerr << "Aborting denoising: nullptr for input number of samples image" << endl;
        }
        if (!m_inputs.m_pHistograms)
        {
            imageNullptr = true;
            //cerr << "Aborting denoising: nullptr for input histogram image" << endl;
        }
        if (!m_inputs.m_pSampleCovariances)
        {
            imageNullptr = true;
            //cerr << "Aborting denoising: nullptr for input covariance image" << endl;
        }
        if (imageNullptr)
            return false;
    }
    {
        bool emptyInput = false;
        if (m_inputs.m_pColors->isEmpty())
        {
            emptyInput = true;
            // cerr << "Aborting denoising: input color image is empty" << endl;
        }
        if (m_inputs.m_pNbOfSamples->isEmpty())
        {
            emptyInput = true;
            // cerr << "Aborting denoising: input number of samples image is empty" << endl;
        }
        if (m_inputs.m_pHistograms->isEmpty())
        {
            emptyInput = true;
            // cerr << "Aborting denoising: input histogram image is empty" << endl;
        }
        if (m_inputs.m_pSampleCovariances->isEmpty())
        {
            emptyInput = true;
            // cerr << "Aborting denoising: input covariance image is empty" << endl;
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
            /*
            cerr << "Aborting denoising: input number of samples image is "
                 << m_inputs.m_pNbOfSamples->getWidth() << "x"
                 << m_inputs.m_pNbOfSamples->getHeight()
                 << "but input color image is " << w << "x" << h << endl;
            */
        }

        if (m_inputs.m_pHistograms->getWidth() != w || m_inputs.m_pHistograms->getHeight() != h)
        {
            badImageSize = true;
            /*
            cerr << "Aborting denoising: input histogram image is "
                 << m_inputs.m_pHistograms->getWidth() << "x"
                 << m_inputs.m_pHistograms->getHeight() << "but input color image is "
                 << w << "x" << h << endl;
            */
        }

        if (m_inputs.m_pSampleCovariances->getWidth() != w || m_inputs.m_pSampleCovariances->getHeight() != h)
        {
            badImageSize = true;
            /*
            cerr << "Aborting denoising: input covariance image is "
                 << m_inputs.m_pSampleCovariances->getWidth() << "x"
                 << m_inputs.m_pSampleCovariances->getHeight()
                 << "but input color image is " << w << "x" << h << endl;
            */
        }

        if (badImageSize)
            return false;
    }

    return true;
}

} // namespace bcd
