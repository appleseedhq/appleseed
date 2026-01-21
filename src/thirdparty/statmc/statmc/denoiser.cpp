
// TODO: Proper Head
// TODO: Proper citation of https://github.com/cg-tuwien/StatMC-opencv_contrib. The code here is written after their example.


// Interface header.
#include "denoiser.h"

// BCD headers.
#include "bcd/DeepImage.h"

// OpenCV headers.
#include <opencv2/cudaimgproc.hpp>
#include <opencv2/imgcodecs.hpp>

#include <iostream>
#include <stdexcept>

using std::string, std::vector;
using cv::Mat, cv::Mat_, cv::imread, cv::IMREAD_UNCHANGED;
using cv::cuda::GpuMat, cv::cuda::PtrStepSzb, cv::cuda::Stream;
using Vec3 = cv::Vec<float, 3>;

struct float3 {
    float x, y, z;
};

namespace
{

void inline alloc(Mat mat, vector<Mat> &mats, vector<GpuMat> &gpuMats) {
    mats.emplace_back(mat);
    gpuMats.emplace_back(GpuMat(mat.rows, mat.cols, mat.type()));
}

void inline alloc(const bcd::Deepimf *deepimf, vector<Mat> &mats, vector<GpuMat> &gpuMats, int type = -1) {
    Mat mat;

    int idx;

    const int height = deepimf->getHeight();
    const int width  = deepimf->getWidth();
    const int depth  = deepimf->getDepth();
    const int size   = deepimf->getSize();

    const int area   = height * width; // area [pixels] of the image
    const int areax2 = 2 * area;

    std::cout << "height: " << height << " width: " << width << " (area: " << area << ") depth: " << depth << " size: " << size << std::endl;

    // create cv matrix
    float deepimfData[size];

    deepimf->copyDataTo(deepimfData);


    if (depth == 1)
    {
        mat = Mat(height, width, CV_32FC1, &deepimfData);
    }
    else if (depth == 3)
    {
        mat = Mat(height, width, CV_32FC3, &deepimfData);

        //float3 elem;
//
        //for (int i=0; i<height; i++)
        //{
        //    for (int j=0; j<width; j++)
        //    {
        //        idx = i * height + j; // array idx of pixel i,j (disregarding depth)
//
        //        // TODO: This is bad, because with 0,1,2 times area we jump areound greatly in memory.
        //        elem.x = deepimfData[         idx]; // 0 * area + idx
        //        elem.y = deepimfData[  area + idx]; // 1 * area + idx
        //        elem.z = deepimfData[areax2 + idx]; // 2 * area + idx
//
        //        mat.at<float3>(i,j) = elem;
        //    }
        //}
    }
    else
    {
        std::cerr << "Unsupported matrix depth: " << depth << std::endl;
        throw std::runtime_error("Unsupported matrix depth.");
    }

    
    if (size <= 128)
    {
        for (int i=0; i<height; i++)
        {
            for (int j=0; j<width; j++)
            {
                for (int k=0; k<depth; k++)
                {
                    idx = (i * width + j) * depth + k;

                    std::cout << deepimfData[idx] << " ";
                }
                std::cout << std::endl;
            }
            std::cout << std::endl;
        }

        std::cout << "M_opencv =" << std::endl << mat << std::endl << std::endl;
    }

    // TODO: OpenCV stores color as BGR (not RGB).
    // -> But does this even matter? After all, all RGB values are switched around anyway. The operations should be per-channel. So this should not affect (break) thinkgs, I think.
    alloc(mat, mats, gpuMats);
}

void inline uploadGPUPtrs(vector<GpuMat> &gpuMats, GpuMat &gpuPtrs, Stream stream) {
    Mat gpuPtrsCPU = Mat(1, gpuMats.size(), CV_8UC(sizeof(PtrStepSzb)));
    PtrStepSzb *gpuPtrsCPUPtr = gpuPtrsCPU.ptr<PtrStepSzb>();
    for (auto &gpuMat : gpuMats)
        *gpuPtrsCPUPtr++ = gpuMat;
    gpuPtrs.upload(gpuPtrsCPU, stream);
}

void inline uploadGBufferChannelCounts(vector<GpuMat> &gpuMats, GpuMat &channelCounts, Stream stream) {
    Mat channelCountsCPU = Mat(1, gpuMats.size(), CV_8UC1);
    unsigned char *channelCountsCPUPtr = channelCountsCPU.ptr<unsigned char>();
    for (auto &gpuMat : gpuMats)
        *channelCountsCPUPtr++ = gpuMat.channels();
    channelCounts.upload(channelCountsCPU, stream);
}
} // namespace (anonymous)

namespace statmc {

bool Denoiser::denoise()
{
    // string prefix = "staircase";
    // string suffix = "16";
    // vector<string> indices = {"0", "1"}; // We denoise two different renderings with these indices.
    // int nRenderings = indices.size();

    int nRenderings = 1; // TODO: Are more possible/sensical in Appleseed usage?

    // Set denoising parameters
    // TODO: Make arguments.
    float ciZValue = 1.95996f; // Note: Not Used
    float sd = 10.f;
    int radius = 20;
    float normalSD = 0.1f;
    float albedoSD = 0.02f;


    // CUDA stream
    Stream stream;

    // Load images and allocate buffers
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

    // for (auto &index : indices) {
    //     alloc(prefix + "-" + index + "-" + suffix + "-film.pfm",       films, gpuFilms);
    //     alloc(prefix + "-" + index + "-" + suffix + "-t0-b0-n.pfm",    ns,    gpuNs, CV_32SC1);
    //     alloc(prefix + "-" + index + "-" + suffix + "-t0-b0-mean.pfm", means, gpuMeans);
    //     alloc(prefix + "-" + index + "-" + suffix + "-t0-b0-m2.pfm",   m2s,   gpuM2s);
    //     alloc(prefix + "-" + index + "-" + suffix + "-t0-b0-m3.pfm",   m3s,   gpuM3s);
    // }

    std::cout << "films >>> ";
    alloc(m_inputs.m_pColors,           films, gpuFilms);
    std::cout << "ns >>> ";
    alloc(m_inputs.m_pNbOfSamples,      ns,    gpuNs,   CV_32SC1);
    // TODO: these are 3-dim images (r,g,b) - shoud they be???
    std::cout << "m1 >>> ";
    alloc(m_stat_inputs.m_pMeans,       means, gpuMeans);
    std::cout << "m2 >>> ";
    alloc(m_stat_inputs.m_pVariances,   m2s,   gpuM2s);
    std::cout << "m3 >>> ";
    alloc(m_stat_inputs.m_pSkewdnesses, m3s,   gpuM3s);


    // // We use the same set of G-buffers to denoise both renderings.
    // alloc(prefix + "-0-" + suffix + "-t1-b0-film-mean.pfm", gBuffers, gpuGBuffers); // normals
    // alloc(prefix + "-0-" + suffix + "-t2-b0-film-mean.pfm", gBuffers, gpuGBuffers); // diffuse colors

    std::cout << "norm >>> ";
    alloc(m_stat_inputs.m_pNorm,    gBuffers, gpuGBuffers);
    std::cout << "diffuse >>> ";
    alloc(m_stat_inputs.m_pDiffuse, gBuffers, gpuGBuffers);

    int width  = m_inputs.m_pColors->getWidth();
    int height = m_inputs.m_pColors->getHeight();

    // for (auto &index : indices) {
        alloc(Mat_<Vec3>(height, width), meanCorrs,      gpuMeanCorrs);
        alloc(Mat_<Vec3>(height, width), discriminators, gpuDiscriminators);
        alloc(Mat_<Vec3>(height, width), denoisedFilms,  gpuDenoisedFilms);
    // }


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


    // Upload images
    for (int i = 0; i < nRenderings; i++) {
        // std::cout << "M =" << std::endl << films[i] << std::endl << std::endl;
        gpuFilms[i].upload(films[i], stream);
        gpuNs[i]   .upload(ns[i],    stream);
        gpuMeans[i].upload(means[i], stream);
        gpuM2s[i]  .upload(m2s[i],   stream);
        gpuM3s[i]  .upload(m3s[i],   stream);
    }

    for (int i = 0; i < gBuffers.size(); i++)
        gpuGBuffers[i].upload(gBuffers[i], stream);


    // Prepare denoising parameters
    float dsFactor = -.5f / (sd * sd);
    vector<float> drFactors{
        -.5f / (normalSD * normalSD),
        -.5f / (albedoSD * albedoSD)
    };
    gBufferDRFactors.upload(Mat(drFactors), stream);

    /** In Variables
     * - film           -> filmGPUPtrs
     * - ns             -> nGPUPtrs
     * - mean           -> meanGPUPtrs
     * - m2             -> m2GPUPtrs
     * - m3             -> m3GPUPtrs
     * - gBuffers       -> gBufferGPUPtrs, gBufferChannelCounts
     * Denoising Parameters
     * - radius
     * - sd, normalSD, albedoSD -> dsFactor
     */

    /** Constants
     * nRenderings = 1 (?)
     */

    // Denoise
    cv::cuda::stat_denoiser::filter<float3>(
        nRenderings, // Number of renderings to denoise
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


    // Download denoised images
    for (int i = 0; i < nRenderings; i++) {
        gpuDenoisedFilms[i].download(denoisedFilms[i], stream);
    }

    Mat denoisedFilm = denoisedFilms[0];
    denoisedFilm.convertTo(denoisedFilm, CV_32FC(sizeof(PtrStepSzb)));

    m_outputs.m_pDenoisedColors->copyDataFrom( denoisedFilm.ptr<float>(0) );

    return true;
}

} // namespace statmc
