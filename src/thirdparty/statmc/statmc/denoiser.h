
// TODO: Proper Head

#pragma once

// BCD headers.
#include "bcd/DeepImage.h"
#include "bcd/IDenoiser.h"

namespace statmc
{

struct DenoiserInputs
{
  DenoiserInputs()
    : m_pAlbedo(nullptr)
    , m_pNormal(nullptr)
    , m_pMeans(nullptr)
    , m_pVariances(nullptr)
    , m_pSkewdnesses(nullptr)
  {
  }

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
