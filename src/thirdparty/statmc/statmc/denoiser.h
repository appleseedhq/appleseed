
// This file comes from the original StatMC-opencv_contrib implementation,
// with minor changes to fit them into Appleseed.
// Original license: Apache-2.0 license

// Implementation of the CUDA denoiser for the SIGGRAPH Asia 2024 conference paper "A Statistical Approach to Monte Carlo Denoising" [Sakai et al. 2024] based on OpenCV.
// Visit https://github.com/cg-tuwien/StatMC-opencv_contrib and https://users.cg.tuwien.ac.at/~hiroyuki/StatMC/ for additional information and resources.


#pragma once

// BCD headers.
#include "bcd/DeepImage.h"
#include "bcd/IDenoiser.h"

namespace statmc
{

struct DenoiserInputs
{
  DenoiserInputs()
    : m_sd(10.f)
    , m_radius(20)
    , m_normalSD(0.1f)
    , m_albedoSD(0.02f)
    , m_pAlbedo(nullptr)
    , m_pNormal(nullptr)
    , m_pMeans(nullptr)
    , m_pVariances(nullptr)
    , m_pSkewdnesses(nullptr)
  {
  }

  // Spatial Filter standard Deviation.
  float m_sd;
  // Filter Pixel Radius (Neighborhood).
  int   m_radius;
  // Normal Range Standard Deviation.
  float m_normalSD;
  // Albedo Range Standard Deviation.
  float m_albedoSD;

  const bcd::Deepimf* m_pAlbedo;
  const bcd::Deepimf* m_pNormal;
  const bcd::Deepimf* m_pMeans;
  const bcd::Deepimf* m_pVariances;
  const bcd::Deepimf* m_pSkewdnesses;
};

class Denoiser
  : public bcd::IDenoiser
{
  public:
    virtual ~Denoiser() {}

    virtual bool denoise();

    // virtual bool denoise(DenoiserInputs stat_inputs);

    void setInputs(const bcd::DenoiserInputs& i_rInputs)
    {
        m_inputs = i_rInputs;
    }

    const DenoiserInputs& getStatInputs() const
    {
        return m_stat_inputs;
    }

    void setStatInputs(const DenoiserInputs& i_rInputs)
    {
        m_stat_inputs = i_rInputs;
    }

  private:
    DenoiserInputs m_stat_inputs;
};
    
} // namespace statmc
