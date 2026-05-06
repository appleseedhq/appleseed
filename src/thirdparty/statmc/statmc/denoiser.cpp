
// This file comes from the original StatMC-opencv_contrib implementation,
// with minor changes to fit them into Appleseed.
// Original license: Apache-2.0 license

// Implementation of the CUDA denoiser for the SIGGRAPH Asia 2024 conference paper "A Statistical Approach to Monte Carlo Denoising" [Sakai et al. 2024] based on OpenCV.
// Visit https://github.com/cg-tuwien/StatMC-opencv_contrib and https://users.cg.tuwien.ac.at/~hiroyuki/StatMC/ for additional information and resources.


// Interface header.
#include "denoiser.h"

// BCD headers.
#include "bcd/DeepImage.h"

// OpenCV headers.
#include <opencv2/cudaimgproc.hpp>
#include <opencv2/imgcodecs.hpp>


using std::string, std::vector;
using cv::Mat, cv::Mat_, cv::imread, cv::IMREAD_UNCHANGED;
using cv::cuda::GpuMat, cv::cuda::PtrStepSzb, cv::cuda::Stream;
using Vec3 = cv::Vec<float, 3>;

struct float3 {
    float x, y, z;
};

namespace
{

void inline alloc(Mat mat, vector<Mat> &mats, vector<GpuMat> &gpuMats)
{
    mats.emplace_back(mat);
    gpuMats.emplace_back(GpuMat(mat.rows, mat.cols, mat.type()));
}

void inline alloc(const bcd::Deepimf *deepimf, vector<Mat> &mats, vector<GpuMat> &gpuMats, int type = -1)
{

    const int height = deepimf->getHeight();
    const int width  = deepimf->getWidth();
    const int depth  = deepimf->getDepth();
    const int size   = deepimf->getSize();

    float* pDeepimfData;

    if ( type == -1)
    {
        // NOTE: Here we cast `const float*` (of `deepimf->getDataPtr()`) ot `float*`.
        //       This is normally ill-advised and can lead to undefined behavior.
        //       However, here we know that the contents of the memory will not be changed and not be accessed outside of this context here.
        //       So we can save a `memcpy` operation and just pass the pointer that points to the original (constant) memory.
        //
        //       For images that change type (i.e. `type != -1`), we still have to copy the contents.

        pDeepimfData = (float*) deepimf->getDataPtr();
    }
    else
    {
        const size_t memSize = size * sizeof(float);

        pDeepimfData = (float*) malloc( memSize );
        memcpy(pDeepimfData, deepimf->getDataPtr(), memSize);
    }

    Mat mat = Mat(height, width, CV_32FC(depth), pDeepimfData);

    // Convert matrix to different type (default: float (32F)).
    // Note: Like for OpenCV's `convertTo`, -1 means "keep the type" (which we assume to be 32 bit floats).
    if ( type != -1 )
        mat.convertTo(mat, type);

    alloc(mat, mats, gpuMats);
}

void inline uploadGPUPtrs(vector<GpuMat> &gpuMats, GpuMat &gpuPtrs, Stream stream)
{
    Mat gpuPtrsCPU = Mat(1, gpuMats.size(), CV_8UC(sizeof(PtrStepSzb)));
    PtrStepSzb *gpuPtrsCPUPtr = gpuPtrsCPU.ptr<PtrStepSzb>();
    for (auto &gpuMat : gpuMats)
        *gpuPtrsCPUPtr++ = gpuMat;
    gpuPtrs.upload(gpuPtrsCPU, stream);
}

void inline uploadGBufferChannelCounts(vector<GpuMat> &gpuMats, GpuMat &channelCounts, Stream stream)
{
    Mat channelCountsCPU = Mat(1, gpuMats.size(), CV_8UC1);
    unsigned char *channelCountsCPUPtr = channelCountsCPU.ptr<unsigned char>();
    for (auto &gpuMat : gpuMats)
        *channelCountsCPUPtr++ = gpuMat.channels();
    channelCounts.upload(channelCountsCPU, stream);
}
} // namespace (anonymous)

namespace statmc
{

bool Denoiser::denoise()
{
    // Set denoising parameters.
    const float sd       = m_stat_inputs.m_sd;
    const int   radius   = m_stat_inputs.m_radius;
    const float normalSD = m_stat_inputs.m_normalSD;
    const float albedoSD = m_stat_inputs.m_albedoSD;


    // CUDA stream.
    Stream stream;

    // Load images and allocate buffers.
    // Note: The StatMC denoiser can be given multiple renders to denoise, as such it expects vectors of images [matrices].
    //       Hence, here all images (for our one render) are placed into vectors of length 1.
    vector<Mat> films;
    vector<Mat> ns;
    vector<Mat> means;
    vector<Mat> m2s;
    vector<Mat> m3s;
    vector<Mat> gBuffers;
    vector<Mat> meanCorrs;
    vector<Mat> discriminators;
    vector<Mat> denoisedFilms;

    vector<GpuMat> gpuFilms;
    vector<GpuMat> gpuNs;
    vector<GpuMat> gpuMeans;
    vector<GpuMat> gpuM2s;
    vector<GpuMat> gpuM3s;
    vector<GpuMat> gpuGBuffers;
    vector<GpuMat> gpuMeanCorrs;
    vector<GpuMat> gpuDiscriminators;
    vector<GpuMat> gpuDenoisedFilms;

    alloc(m_inputs.m_pColors,           films, gpuFilms);
    alloc(m_inputs.m_pNbOfSamples,      ns,    gpuNs,   CV_32SC1);
    alloc(m_stat_inputs.m_pMeans,       means, gpuMeans);
    alloc(m_stat_inputs.m_pVariances,   m2s,   gpuM2s);
    alloc(m_stat_inputs.m_pSkewdnesses, m3s,   gpuM3s);

    alloc(m_stat_inputs.m_pNormal,    gBuffers, gpuGBuffers);
    alloc(m_stat_inputs.m_pAlbedo, gBuffers, gpuGBuffers);

    const int width  = m_inputs.m_pColors->getWidth();
    const int height = m_inputs.m_pColors->getHeight();

    alloc(Mat_<Vec3>(height, width), meanCorrs,      gpuMeanCorrs);
    alloc(Mat_<Vec3>(height, width), discriminators, gpuDiscriminators);
    alloc(Mat_<Vec3>(height, width), denoisedFilms,  gpuDenoisedFilms);


    // Upload vectors containing pointers to GPU buffers
    GpuMat filmGPUPtrs;
    GpuMat nGPUPtrs;
    GpuMat meanGPUPtrs;
    GpuMat m2GPUPtrs;
    GpuMat m3GPUPtrs;

    GpuMat gBufferGPUPtrs;
    GpuMat gBufferChannelCounts;
    GpuMat gBufferDRFactors;

    GpuMat meanCorrGPUPtrs;
    GpuMat discriminatorGPUPtrs;
    GpuMat denoisedFilmGPUPtrs;

    uploadGPUPtrs(gpuFilms, filmGPUPtrs, stream);
    uploadGPUPtrs(gpuNs,    nGPUPtrs,    stream);
    uploadGPUPtrs(gpuMeans, meanGPUPtrs, stream);
    uploadGPUPtrs(gpuM2s,   m2GPUPtrs,   stream);
    uploadGPUPtrs(gpuM3s,   m3GPUPtrs,   stream);

    uploadGPUPtrs(gpuGBuffers, gBufferGPUPtrs, stream);
    // For the G-buffers, we additionally need to upload the channel counts for the individual buffers.
    uploadGBufferChannelCounts(gpuGBuffers, gBufferChannelCounts, stream);

    uploadGPUPtrs(gpuMeanCorrs,      meanCorrGPUPtrs,      stream);
    uploadGPUPtrs(gpuDiscriminators, discriminatorGPUPtrs, stream);
    uploadGPUPtrs(gpuDenoisedFilms,  denoisedFilmGPUPtrs,  stream);


    // Upload images.
    gpuFilms[0].upload(films[0], stream);
    gpuNs[0]   .upload(ns[0],    stream);
    gpuMeans[0].upload(means[0], stream);
    gpuM2s[0]  .upload(m2s[0],   stream);
    gpuM3s[0]  .upload(m3s[0],   stream);

    for (int i = 0; i < gBuffers.size(); i++)
        gpuGBuffers[i].upload(gBuffers[i], stream);


    // Prepare denoising parameters.
    float dsFactor = -.5f / (sd * sd);
    vector<float> drFactors{
        -.5f / (normalSD * normalSD),
        -.5f / (albedoSD * albedoSD)
    };
    gBufferDRFactors.upload(Mat(drFactors), stream);


    // Denoise.
    cv::cuda::stat_denoiser::filter<float3>(
        1, // Number of renderings to denoise
        width, // Width of the renderings
        height, // Height of the renderings
        dsFactor, // Spatial distance factor calculated from spatial filter standard deviation
        radius, // Filter radius
        nGPUPtrs, // Pointers to GPU buffers for sample sizes (n)
        meanGPUPtrs, // Pointers to GPU buffers for means
        m2GPUPtrs, // Pointers to GPU buffers for sums of squared deviations (m2)
        m3GPUPtrs, // Pointers to GPU buffers for sums of cubed deviations (m3)
        filmGPUPtrs, // Pointers to GPU buffers for noisy input images
        gBufferGPUPtrs, // Pointers to GPU buffers for G-buffers
        gBufferChannelCounts, // G-buffer channel counts
        gBufferDRFactors, // Range distance factors calculated from range standard deviations for G-buffers
        gBuffers.size(), // Number of G-buffers
        meanCorrGPUPtrs, // Pointers to GPU buffers for Johnson-corrected means
        discriminatorGPUPtrs, // Pointers to GPU buffers for discriminators
        denoisedFilmGPUPtrs, // Pointers to GPU buffers for denoised output images
        stream // CUDA stream
    );

    // Download denoised image from the GPU.
    gpuDenoisedFilms[0].download(denoisedFilms[0], stream);

    Mat denoisedFilm = denoisedFilms[0];
    denoisedFilm.convertTo(denoisedFilm, CV_32FC(sizeof(PtrStepSzb)));

    // Pass denoised image to output.
    m_outputs.m_pDenoisedColors->copyDataFrom( denoisedFilm.ptr<float>(0) );

    return true;
}

} // namespace statmc
