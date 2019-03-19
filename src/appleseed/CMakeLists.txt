
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
# Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

set (foundation_array_sources
    foundation/array/algorithm.cpp
    foundation/array/algorithm.h
    foundation/array/applyvisitor.h
    foundation/array/array.cpp
    foundation/array/array.h
    foundation/array/arrayref.h
    foundation/array/arraytraits.h
    foundation/array/arrayview.h
    foundation/array/exception.cpp
    foundation/array/exception.h
    foundation/array/meshalgorithm.cpp
    foundation/array/meshalgorithm.h
)
list (APPEND appleseed_sources
    ${foundation_array_sources}
)
source_group ("foundation\\array" FILES
    ${foundation_array_sources}
)

set (foundation_core_concepts_sources
    foundation/core/concepts/iunknown.h
    foundation/core/concepts/noncopyable.h
    foundation/core/concepts/singleton.h
)
list (APPEND appleseed_sources
    ${foundation_core_concepts_sources}
)
source_group ("foundation\\core\\concepts" FILES
    ${foundation_core_concepts_sources}
)

set (foundation_core_exceptions_sources
    foundation/core/exceptions/exception.h
    foundation/core/exceptions/exceptionioerror.h
    foundation/core/exceptions/exceptionnotimplemented.h
    foundation/core/exceptions/exceptionunsupportedfileformat.h
    foundation/core/exceptions/stringexception.h
)
list (APPEND appleseed_sources
    ${foundation_core_exceptions_sources}
)
source_group ("foundation\\core\\exceptions" FILES
    ${foundation_core_exceptions_sources}
)

set (foundation_core_sources
    foundation/core/appleseed.cpp
    foundation/core/appleseed.h
    foundation/core/thirdparties.cpp
    foundation/core/thirdparties.h
    foundation/core/version.h.in
)
list (APPEND appleseed_sources
    ${foundation_core_sources}
)
source_group ("foundation\\core" FILES
    ${foundation_core_sources}
)

set (foundation_curve_sources
    foundation/curve/binarycurvefilereader.cpp
    foundation/curve/binarycurvefilereader.h
    foundation/curve/binarycurvefilewriter.cpp
    foundation/curve/binarycurvefilewriter.h
    foundation/curve/curvebasis.cpp
    foundation/curve/curvebasis.h
    foundation/curve/genericcurvefilereader.cpp
    foundation/curve/genericcurvefilereader.h
    foundation/curve/genericcurvefilewriter.cpp
    foundation/curve/genericcurvefilewriter.h
    foundation/curve/icurvebuilder.h
    foundation/curve/icurvefilereader.h
    foundation/curve/icurvefilewriter.h
    foundation/curve/icurvewalker.h
    foundation/curve/mitshairfilereader.cpp
    foundation/curve/mitshairfilereader.h
)
list (APPEND appleseed_sources
    ${foundation_curve_sources}
)
source_group ("foundation\\curve" FILES
    ${foundation_curve_sources}
)

set (foundation_image_text_sources
    foundation/image/text/stb_truetype.cpp
    foundation/image/text/stb_truetype.h
    foundation/image/text/textrenderer.cpp
    foundation/image/text/textrenderer.h
)
list (APPEND appleseed_sources
    ${foundation_image_text_sources}
)
source_group ("foundation\\image\\text" FILES
    ${foundation_image_text_sources}
)

set (foundation_image_sources
    foundation/image/analysis.cpp
    foundation/image/analysis.h
    foundation/image/canvasproperties.h
    foundation/image/color.h
    foundation/image/colorspace.cpp
    foundation/image/colorspace.h
    foundation/image/drawing.cpp
    foundation/image/drawing.h
    foundation/image/exceptionunsupportedimageformat.h
    foundation/image/filteredtile.cpp
    foundation/image/filteredtile.h
    foundation/image/genericimagefilereader.cpp
    foundation/image/genericimagefilereader.h
    foundation/image/genericimagefilewriter.cpp
    foundation/image/genericimagefilewriter.h
    foundation/image/genericprogressiveimagefilereader.cpp
    foundation/image/genericprogressiveimagefilereader.h
    foundation/image/icanvas.h
    foundation/image/iimagefilereader.h
    foundation/image/iimagefilewriter.h
    foundation/image/image.cpp
    foundation/image/image.h
    foundation/image/imageattributes.cpp
    foundation/image/imageattributes.h
    foundation/image/iprogressiveimagefilereader.h
    foundation/image/nativedrawing.cpp
    foundation/image/nativedrawing.h
    foundation/image/pixel.cpp
    foundation/image/pixel.h
    foundation/image/regularspectrum.h
    foundation/image/tile.cpp
    foundation/image/tile.h
)
list (APPEND appleseed_sources
    ${foundation_image_sources}
)
source_group ("foundation\\image" FILES
    ${foundation_image_sources}
)

set (foundation_math_bsp_sources
    foundation/math/bsp/bsp_builder.h
    foundation/math/bsp/bsp_intersector.h
    foundation/math/bsp/bsp_node.h
    foundation/math/bsp/bsp_statistics.cpp
    foundation/math/bsp/bsp_statistics.h
    foundation/math/bsp/bsp_tree.h
)
list (APPEND appleseed_sources
    ${foundation_math_bsp_sources}
)
source_group ("foundation\\math\\bsp" FILES
    ${foundation_math_bsp_sources}
)

set (foundation_math_bvh_sources
    foundation/math/bvh/bvh_bboxsortpredicate.h
    foundation/math/bvh/bvh_builder.h
    foundation/math/bvh/bvh_intersector.h
    foundation/math/bvh/bvh_medianpartitioner.h
    foundation/math/bvh/bvh_middlepartitioner.h
    foundation/math/bvh/bvh_node.h
    foundation/math/bvh/bvh_partitionerbase.h
    foundation/math/bvh/bvh_sahpartitioner.h
    foundation/math/bvh/bvh_sbvhpartitioner.h
    foundation/math/bvh/bvh_spatialbuilder.h
    foundation/math/bvh/bvh_statistics.cpp
    foundation/math/bvh/bvh_statistics.h
    foundation/math/bvh/bvh_tree.h
)
list (APPEND appleseed_sources
    ${foundation_math_bvh_sources}
)
source_group ("foundation\\math\\bvh" FILES
    ${foundation_math_bvh_sources}
)

set (foundation_math_intersection_sources
    foundation/math/intersection/aabbtriangle.h
    foundation/math/intersection/frustumaabb.h
    foundation/math/intersection/frustumsegment.h
    foundation/math/intersection/planesegment.h
    foundation/math/intersection/rayaabb.h
    foundation/math/intersection/rayplane.h
    foundation/math/intersection/raysphere.h
    foundation/math/intersection/raytrianglehh.h
    foundation/math/intersection/raytrianglemt.h
    foundation/math/intersection/raytrianglessk.h
)
list (APPEND appleseed_sources
    ${foundation_math_intersection_sources}
)
source_group ("foundation\\math\\intersection" FILES
    ${foundation_math_intersection_sources}
)

set (foundation_math_knn_sources
    foundation/math/knn/knn_answer.h
    foundation/math/knn/knn_builder.h
    foundation/math/knn/knn_node.h
    foundation/math/knn/knn_query.h
    foundation/math/knn/knn_statistics.cpp
    foundation/math/knn/knn_statistics.h
    foundation/math/knn/knn_tree.h
)
list (APPEND appleseed_sources
    ${foundation_math_knn_sources}
)
source_group ("foundation\\math\\knn" FILES
    ${foundation_math_knn_sources}
)

set (foundation_math_rng_sources
    foundation/math/rng/distribution.h
    foundation/math/rng/lcg.h
    foundation/math/rng/mersennetwister.h
    foundation/math/rng/pcg.h
    foundation/math/rng/serialmersennetwister.cpp
    foundation/math/rng/serialmersennetwister.h
    foundation/math/rng/xoroshiro128plus.h
    foundation/math/rng/xorshift32.h
    foundation/math/rng/xorshift64.h
)
if (USE_SSE)
    list (APPEND foundation_math_rng_sources
        foundation/math/rng/simdmersennetwister.cpp
        foundation/math/rng/simdmersennetwister.h
    )
endif ()
list (APPEND appleseed_sources
    ${foundation_math_rng_sources}
)
source_group ("foundation\\math\\rng" FILES
    ${foundation_math_rng_sources}
)

set (foundation_math_sampling_sources
    foundation/math/sampling/imageimportancesampler.h
    foundation/math/sampling/mappings.h
    foundation/math/sampling/qmcsamplingcontext.h
    foundation/math/sampling/rngsamplingcontext.h
    foundation/math/sampling/sphericalimportancesampler.h
)
list (APPEND appleseed_sources
    ${foundation_math_sampling_sources}
)
source_group ("foundation\\math\\sampling" FILES
    ${foundation_math_sampling_sources}
)

set (foundation_math_voxel_sources
    foundation/math/voxel/voxel_builder.h
    foundation/math/voxel/voxel_intersector.h
    foundation/math/voxel/voxel_node.h
    foundation/math/voxel/voxel_statistics.cpp
    foundation/math/voxel/voxel_statistics.h
    foundation/math/voxel/voxel_tree.h
)
list (APPEND appleseed_sources
    ${foundation_math_voxel_sources}
)
source_group ("foundation\\math\\voxel" FILES
    ${foundation_math_voxel_sources}
)

set (foundation_math_sources
    foundation/math/aabb.h
    foundation/math/area.h
    foundation/math/basis.h
    foundation/math/bezier.h
    foundation/math/beziercurve.h
    foundation/math/bsp.h
    foundation/math/bvh.h
    foundation/math/cdf.h
    foundation/math/combination.h
    foundation/math/compressedunitvector.h
    foundation/math/distance.h
    foundation/math/dual.h
    foundation/math/fastmath.h
    foundation/math/filter.h
    foundation/math/fp.h
    foundation/math/fresnel.h
    foundation/math/frustum.h
    foundation/math/half.cpp
    foundation/math/half.h
    foundation/math/hash.h
    foundation/math/knn.h
    foundation/math/matrix.h
    foundation/math/microfacet.cpp
    foundation/math/microfacet.h
    foundation/math/minmax.h
    foundation/math/mis.h
    foundation/math/noise.cpp
    foundation/math/noise.h
    foundation/math/ordering.cpp
    foundation/math/ordering.h
    foundation/math/permutation.cpp
    foundation/math/permutation.h
    foundation/math/phasefunction.cpp
    foundation/math/phasefunction.h
    foundation/math/population.h
    foundation/math/primes.cpp
    foundation/math/primes.h
    foundation/math/qmc.cpp
    foundation/math/qmc.h
    foundation/math/quaternion.h
    foundation/math/ray.h
    foundation/math/root.h
    foundation/math/rr.h
    foundation/math/sah.h
    foundation/math/scalar.h
    foundation/math/specialfunctions.cpp
    foundation/math/specialfunctions.h
    foundation/math/sphericaltriangle.h
    foundation/math/spline.h
    foundation/math/split.h
    foundation/math/transform.h
    foundation/math/treeoptimizer.h
    foundation/math/triangulator.h
    foundation/math/vector.h
    foundation/math/voxel.h
    foundation/math/voxelgrid.h
)
list (APPEND appleseed_sources
    ${foundation_math_sources}
)
source_group ("foundation\\math" FILES
    ${foundation_math_sources}
)

set (foundation_mesh_sources
    foundation/mesh/binarymeshfilereader.cpp
    foundation/mesh/binarymeshfilereader.h
    foundation/mesh/binarymeshfilewriter.cpp
    foundation/mesh/binarymeshfilewriter.h
    foundation/mesh/genericmeshfilereader.cpp
    foundation/mesh/genericmeshfilereader.h
    foundation/mesh/genericmeshfilewriter.cpp
    foundation/mesh/genericmeshfilewriter.h
    foundation/mesh/imeshbuilder.h
    foundation/mesh/imeshfilereader.h
    foundation/mesh/imeshfilewriter.h
    foundation/mesh/imeshwalker.h
    foundation/mesh/meshbuilderbase.h
    foundation/mesh/objmeshfilelexer.h
    foundation/mesh/objmeshfilereader.cpp
    foundation/mesh/objmeshfilereader.h
    foundation/mesh/objmeshfilewriter.cpp
    foundation/mesh/objmeshfilewriter.h
)
list (APPEND appleseed_sources
    ${foundation_mesh_sources}
)
source_group ("foundation\\mesh" FILES
    ${foundation_mesh_sources}
)

set (foundation_meta_benchmarks_sources
    foundation/meta/benchmarks/benchmark_basis.cpp
    foundation/meta/benchmarks/benchmark_cache.cpp
    foundation/meta/benchmarks/benchmark_cdf.cpp
    foundation/meta/benchmarks/benchmark_colorspace.cpp
    foundation/meta/benchmarks/benchmark_distance.cpp
    foundation/meta/benchmarks/benchmark_fastmath.cpp
    foundation/meta/benchmarks/benchmark_filteredtile.cpp
    foundation/meta/benchmarks/benchmark_half.cpp
    foundation/meta/benchmarks/benchmark_hash.cpp
    foundation/meta/benchmarks/benchmark_imageimportancesampler.cpp
    foundation/meta/benchmarks/benchmark_integerdivision.cpp
    foundation/meta/benchmarks/benchmark_intersection.cpp
    foundation/meta/benchmarks/benchmark_job.cpp
    foundation/meta/benchmarks/benchmark_knn.cpp
    foundation/meta/benchmarks/benchmark_math_filter.cpp
    foundation/meta/benchmarks/benchmark_matrix.cpp
    foundation/meta/benchmarks/benchmark_microfacet.cpp
    foundation/meta/benchmarks/benchmark_permutation.cpp
    foundation/meta/benchmarks/benchmark_poolallocator.cpp
    foundation/meta/benchmarks/benchmark_qmc.cpp
    foundation/meta/benchmarks/benchmark_quaternion.cpp
    foundation/meta/benchmarks/benchmark_ray.cpp
    foundation/meta/benchmarks/benchmark_regularspectrum.cpp
    foundation/meta/benchmarks/benchmark_rng.cpp
    foundation/meta/benchmarks/benchmark_samesign.cpp
    foundation/meta/benchmarks/benchmark_sampling.cpp
    foundation/meta/benchmarks/benchmark_string.cpp
    foundation/meta/benchmarks/benchmark_tile.cpp
    foundation/meta/benchmarks/benchmark_transform.cpp
    foundation/meta/benchmarks/benchmark_vector.cpp
    foundation/meta/benchmarks/benchmark_voxelgrid.cpp
)
list (APPEND appleseed_sources
    ${foundation_meta_benchmarks_sources}
)
source_group ("foundation\\meta\\benchmarks" FILES
    ${foundation_meta_benchmarks_sources}
)

set (foundation_meta_tests_sources
    foundation/meta/tests/test_aabb.cpp
    foundation/meta/tests/test_analysis.cpp
    foundation/meta/tests/test_array.cpp
    foundation/meta/tests/test_arrayalgorithm.cpp
    foundation/meta/tests/test_arrayapplyvisitor.cpp
    foundation/meta/tests/test_attributeset.cpp
    foundation/meta/tests/test_autoreleaseptr.cpp
    foundation/meta/tests/test_benchmarkaggregator.cpp
    foundation/meta/tests/test_beziercurve.cpp
    foundation/meta/tests/test_bitmask.cpp
    foundation/meta/tests/test_boost_datetime.cpp
    foundation/meta/tests/test_boost_path.cpp
    foundation/meta/tests/test_boost_regex.cpp
    foundation/meta/tests/test_bsp.cpp
    foundation/meta/tests/test_bufferedfile.cpp
    foundation/meta/tests/test_bvh.cpp
    foundation/meta/tests/test_bvh_partitioner.cpp
    foundation/meta/tests/test_cache.cpp
    foundation/meta/tests/test_cameracontroller.cpp
    foundation/meta/tests/test_casts.cpp
    foundation/meta/tests/test_cdf.cpp
    foundation/meta/tests/test_color.cpp
    foundation/meta/tests/test_colorspace.cpp
    foundation/meta/tests/test_commandlineparser.cpp
    foundation/meta/tests/test_compressedunitvector.cpp
    foundation/meta/tests/test_concepts.cpp
    foundation/meta/tests/test_copyonwrite.cpp
    foundation/meta/tests/test_countof.cpp
    foundation/meta/tests/test_datetime.cpp
    foundation/meta/tests/test_dictionary.cpp
    foundation/meta/tests/test_distance.cpp
    foundation/meta/tests/test_fastmath.cpp
    foundation/meta/tests/test_filteredtile.cpp
    foundation/meta/tests/test_fp.cpp
    foundation/meta/tests/test_fresnel.cpp
    foundation/meta/tests/test_genericimagefilewriter.cpp
    foundation/meta/tests/test_genericprogressiveimagefilereader.cpp
    foundation/meta/tests/test_half.cpp
    foundation/meta/tests/test_hash.cpp
    foundation/meta/tests/test_hashtable.cpp
    foundation/meta/tests/test_iesparser.cpp
    foundation/meta/tests/test_image.cpp
    foundation/meta/tests/test_imageimportancesampler.cpp
    foundation/meta/tests/test_intersection_frustumaabb.cpp
    foundation/meta/tests/test_intersection_frustumsegment.cpp
    foundation/meta/tests/test_intersection_planesegment.cpp
    foundation/meta/tests/test_intersection_rayaabb.cpp
    foundation/meta/tests/test_intersection_raysphere.cpp
    foundation/meta/tests/test_intersection_raytriangle.cpp
    foundation/meta/tests/test_iostreamop.cpp
    foundation/meta/tests/test_job.cpp
    foundation/meta/tests/test_knn.cpp
    foundation/meta/tests/test_kvpair.cpp
    foundation/meta/tests/test_lazy.cpp
    foundation/meta/tests/test_makevector.cpp
    foundation/meta/tests/test_math_filter.cpp
    foundation/meta/tests/test_matrix.cpp
    foundation/meta/tests/test_memory.cpp
    foundation/meta/tests/test_microfacet.cpp
    foundation/meta/tests/test_minmax.cpp
    foundation/meta/tests/test_mis.cpp
    foundation/meta/tests/test_murmurhash.cpp
    foundation/meta/tests/test_noise.cpp
    foundation/meta/tests/test_objmeshfilereader.cpp
    foundation/meta/tests/test_objmeshfilewriter.cpp
    foundation/meta/tests/test_otherwise.cpp
    foundation/meta/tests/test_path.cpp
    foundation/meta/tests/test_permutation.cpp
    foundation/meta/tests/test_pixel.cpp
    foundation/meta/tests/test_poison.cpp
    foundation/meta/tests/test_poolallocator.cpp
    foundation/meta/tests/test_population.cpp
    foundation/meta/tests/test_preprocessor.cpp
    foundation/meta/tests/test_qmc.cpp
    foundation/meta/tests/test_quaternion.cpp
    foundation/meta/tests/test_ray.cpp
    foundation/meta/tests/test_registrar.cpp
    foundation/meta/tests/test_regularspectrum.cpp
    foundation/meta/tests/test_rng.cpp
    foundation/meta/tests/test_sampling.cpp
    foundation/meta/tests/test_scalar.cpp
    foundation/meta/tests/test_searchpaths.cpp
    foundation/meta/tests/test_settings.cpp
    foundation/meta/tests/test_sharedlibrary.cpp
    foundation/meta/tests/test_siphash.cpp
    foundation/meta/tests/test_snprintf.cpp
    foundation/meta/tests/test_sphericalimportancesampler.cpp
    foundation/meta/tests/test_spline.cpp
    foundation/meta/tests/test_statistics.cpp
    foundation/meta/tests/test_stlallocatortestbed.cpp
    foundation/meta/tests/test_stopwatch.cpp
    foundation/meta/tests/test_string.cpp
    foundation/meta/tests/test_test.cpp
    foundation/meta/tests/test_thread.cpp
    foundation/meta/tests/test_tile.cpp
    foundation/meta/tests/test_timers.cpp
    foundation/meta/tests/test_transform.cpp
    foundation/meta/tests/test_triangulator.cpp
    foundation/meta/tests/test_typetraits.cpp
    foundation/meta/tests/test_utility_filter.cpp
    foundation/meta/tests/test_vector.cpp
    foundation/meta/tests/test_voxelgrid.cpp
    foundation/meta/tests/test_windows.cpp
    foundation/meta/tests/test_zip.cpp
)
if (WITH_DISNEY_MATERIAL)
    list (APPEND foundation_meta_tests_sources
        foundation/meta/tests/test_seexpr.cpp
    )
endif ()
list (APPEND appleseed_sources
    ${foundation_meta_tests_sources}
)
source_group ("foundation\\meta\\tests" FILES
    ${foundation_meta_tests_sources}
)

set (foundation_platform_snprintf_sources
    foundation/platform/snprintf/snprintf.cpp
)
list (APPEND appleseed_sources
    ${foundation_platform_snprintf_sources}
)
source_group ("foundation\\platform\\snprintf" FILES
    ${foundation_platform_snprintf_sources}
)

set (foundation_platform_sources
    foundation/platform/_beginexrheaders.h
    foundation/platform/_beginoiioheaders.h
    foundation/platform/_beginoslheaders.h
    foundation/platform/_endexrheaders.h
    foundation/platform/_endoiioheaders.h
    foundation/platform/_endoslheaders.h
    foundation/platform/arch.h
    foundation/platform/atomic.h
    foundation/platform/compiler.cpp
    foundation/platform/compiler.h
    foundation/platform/console.cpp
    foundation/platform/console.h
    foundation/platform/datetime.h
    foundation/platform/debugger.cpp
    foundation/platform/debugger.h
    foundation/platform/defaulttimers.cpp
    foundation/platform/defaulttimers.h
    foundation/platform/opengl.h
    foundation/platform/path.cpp
    foundation/platform/path.h
    foundation/platform/python.h
    foundation/platform/sharedlibrary.cpp
    foundation/platform/sharedlibrary.h
    foundation/platform/snprintf.h
    foundation/platform/sse.h
    foundation/platform/system.cpp
    foundation/platform/system.h
    foundation/platform/thread.cpp
    foundation/platform/thread.h
    foundation/platform/timers.h
    foundation/platform/types.h
    foundation/platform/win32stackwalker.cpp
    foundation/platform/win32stackwalker.h
    foundation/platform/windows.cpp
    foundation/platform/windows.h
)
if (is_x86)
    list (APPEND foundation_platform_sources
        foundation/platform/x86timer.cpp
        foundation/platform/x86timer.h
    )
endif ()
list (APPEND appleseed_sources
    ${foundation_platform_sources}
)
source_group ("foundation\\platform" FILES
    ${foundation_platform_sources}
)

set (foundation_resources_fonts_sources
    foundation/resources/fonts/Ubuntu-L.ttf.cpp
    foundation/resources/fonts/Ubuntu-L.ttf.h
    foundation/resources/fonts/Ubuntu-M.ttf.cpp
    foundation/resources/fonts/Ubuntu-M.ttf.h
)
list (APPEND appleseed_sources
    ${foundation_resources_fonts_sources}
)
source_group ("foundation\\resources\\fonts" FILES
    ${foundation_resources_fonts_sources}
)

set (foundation_resources_logo_sources
    foundation/resources/logo/appleseed-seeds-16.cpp
    foundation/resources/logo/appleseed-seeds-16.h
    foundation/resources/logo/appleseed-seeds-32.cpp
    foundation/resources/logo/appleseed-seeds-32.h
    foundation/resources/logo/appleseed-seeds-64.cpp
    foundation/resources/logo/appleseed-seeds-64.h
    foundation/resources/logo/appleseed-seeds-256.cpp
    foundation/resources/logo/appleseed-seeds-256.h
    foundation/resources/logo/generatelogofiles.cpp
)
list (APPEND appleseed_sources
    ${foundation_resources_logo_sources}
)
source_group ("foundation\\resources\\logo" FILES
    ${foundation_resources_logo_sources}
)

set (foundation_utility_api_sources
    foundation/utility/api/apiarray.h
    foundation/utility/api/apistring.cpp
    foundation/utility/api/apistring.h
    foundation/utility/api/apistringpair.cpp
    foundation/utility/api/apistringpair.h
    foundation/utility/api/specializedapiarrays.cpp
    foundation/utility/api/specializedapiarrays.h
)
list (APPEND appleseed_sources
    ${foundation_utility_api_sources}
)
source_group ("foundation\\utility\\api" FILES
    ${foundation_utility_api_sources}
)

set (foundation_utility_benchmark_sources
    foundation/utility/benchmark/benchmarkaggregator.cpp
    foundation/utility/benchmark/benchmarkaggregator.h
    foundation/utility/benchmark/benchmarkdatapoint.h
    foundation/utility/benchmark/benchmarklistenerbase.h
    foundation/utility/benchmark/benchmarkresult.cpp
    foundation/utility/benchmark/benchmarkresult.h
    foundation/utility/benchmark/benchmarkseries.cpp
    foundation/utility/benchmark/benchmarkseries.h
    foundation/utility/benchmark/benchmarksuite.cpp
    foundation/utility/benchmark/benchmarksuite.h
    foundation/utility/benchmark/benchmarksuiterepository.cpp
    foundation/utility/benchmark/benchmarksuiterepository.h
    foundation/utility/benchmark/helpers.h
    foundation/utility/benchmark/ibenchmarkcase.h
    foundation/utility/benchmark/ibenchmarkcasefactory.h
    foundation/utility/benchmark/ibenchmarklistener.h
    foundation/utility/benchmark/loggerbenchmarklistener.cpp
    foundation/utility/benchmark/loggerbenchmarklistener.h
    foundation/utility/benchmark/timingresult.h
    foundation/utility/benchmark/xmlfilebenchmarklistener.cpp
    foundation/utility/benchmark/xmlfilebenchmarklistener.h
)
list (APPEND appleseed_sources
    ${foundation_utility_benchmark_sources}
)
source_group ("foundation\\utility\\benchmark" FILES
    ${foundation_utility_benchmark_sources}
)

set (foundation_utility_commandlineparser_sources
    foundation/utility/commandlineparser/commandlineparser.h
    foundation/utility/commandlineparser/flagoptionhandler.h
    foundation/utility/commandlineparser/messagelist.cpp
    foundation/utility/commandlineparser/messagelist.h
    foundation/utility/commandlineparser/optionhandler.h
    foundation/utility/commandlineparser/parseresults.h
    foundation/utility/commandlineparser/valueoptionhandler.h
)
list (APPEND appleseed_sources
    ${foundation_utility_commandlineparser_sources}
)
source_group ("foundation\\utility\\commandlineparser" FILES
    ${foundation_utility_commandlineparser_sources}
)

set (foundation_utility_containers_sources
    foundation/utility/containers/dictionary.cpp
    foundation/utility/containers/dictionary.h
    foundation/utility/containers/hashtable.h
)
list (APPEND appleseed_sources
    ${foundation_utility_containers_sources}
)
source_group ("foundation\\utility\\containers" FILES
    ${foundation_utility_containers_sources}
)

set (foundation_utility_filter_sources
    foundation/utility/filter/ifilter.h
    foundation/utility/filter/passthroughfilter.h
    foundation/utility/filter/regexfilter.cpp
    foundation/utility/filter/regexfilter.h
    foundation/utility/filter/rejectallfilter.h
)
list (APPEND appleseed_sources
    ${foundation_utility_filter_sources}
)
source_group ("foundation\\utility\\filter" FILES
    ${foundation_utility_filter_sources}
)

set (foundation_utility_job_sources
    foundation/utility/job/abortswitch.h
    foundation/utility/job/iabortswitch.h
    foundation/utility/job/ijob.h
    foundation/utility/job/jobmanager.cpp
    foundation/utility/job/jobmanager.h
    foundation/utility/job/jobqueue.cpp
    foundation/utility/job/jobqueue.h
    foundation/utility/job/workerthread.cpp
    foundation/utility/job/workerthread.h
)
list (APPEND appleseed_sources
    ${foundation_utility_job_sources}
)
source_group ("foundation\\utility\\job" FILES
    ${foundation_utility_job_sources}
)

set (foundation_utility_log_sources
    foundation/utility/log/consolelogtarget.cpp
    foundation/utility/log/consolelogtarget.h
    foundation/utility/log/filelogtarget.cpp
    foundation/utility/log/filelogtarget.h
    foundation/utility/log/filelogtargetbase.cpp
    foundation/utility/log/filelogtargetbase.h
    foundation/utility/log/helpers.h
    foundation/utility/log/ilogtarget.h
    foundation/utility/log/logformatter.cpp
    foundation/utility/log/logformatter.h
    foundation/utility/log/logger.cpp
    foundation/utility/log/logger.h
    foundation/utility/log/logmessage.cpp
    foundation/utility/log/logmessage.h
    foundation/utility/log/openfilelogtarget.cpp
    foundation/utility/log/openfilelogtarget.h
    foundation/utility/log/stringlogtarget.cpp
    foundation/utility/log/stringlogtarget.h
)
list (APPEND appleseed_sources
    ${foundation_utility_log_sources}
)
source_group ("foundation\\utility\\log" FILES
    ${foundation_utility_log_sources}
)

set (foundation_utility_minizip_sources
    foundation/utility/minizip/crypt.h
    foundation/utility/minizip/ioapi.c
    foundation/utility/minizip/ioapi.h
    foundation/utility/minizip/unzip.c
    foundation/utility/minizip/unzip.h
    foundation/utility/minizip/zip.c
    foundation/utility/minizip/zip.h
)
list (APPEND appleseed_sources
    ${foundation_utility_minizip_sources}
)
source_group ("foundation\\utility\\minizip" FILES
    ${foundation_utility_minizip_sources}
)

set (foundation_utility_settings_sources
    foundation/utility/settings/settings.xsd
    foundation/utility/settings/settingsfilereader.cpp
    foundation/utility/settings/settingsfilereader.h
    foundation/utility/settings/settingsfilewriter.cpp
    foundation/utility/settings/settingsfilewriter.h
)
list (APPEND appleseed_sources
    ${foundation_utility_settings_sources}
)
source_group ("foundation\\utility\\settings" FILES
    ${foundation_utility_settings_sources}
)

set (foundation_utility_test_sources
    foundation/utility/test/assertions.h
    foundation/utility/test/exceptionassertionfailure.h
    foundation/utility/test/globaltestsuite.h
    foundation/utility/test/helpers.h
    foundation/utility/test/itestcase.h
    foundation/utility/test/itestcasefactory.h
    foundation/utility/test/itestlistener.h
    foundation/utility/test/loggertestlistener.cpp
    foundation/utility/test/loggertestlistener.h
    foundation/utility/test/testlistenerbase.h
    foundation/utility/test/testlistenercollection.cpp
    foundation/utility/test/testlistenercollection.h
    foundation/utility/test/testlistenerhelper.cpp
    foundation/utility/test/testlistenerhelper.h
    foundation/utility/test/testmessage.h
    foundation/utility/test/testresult.cpp
    foundation/utility/test/testresult.h
    foundation/utility/test/testsuite.cpp
    foundation/utility/test/testsuite.h
    foundation/utility/test/testsuiterepository.cpp
    foundation/utility/test/testsuiterepository.h
)
list (APPEND appleseed_sources
    ${foundation_utility_test_sources}
)
source_group ("foundation\\utility\\test" FILES
    ${foundation_utility_test_sources}
)

set (foundation_utility_ui_sources
    foundation/utility/ui/cameracontroller.h
)
list (APPEND appleseed_sources
    ${foundation_utility_ui_sources}
)
source_group ("foundation\\utility\\ui" FILES
    ${foundation_utility_ui_sources}
)

set (foundation_utility_sources
    foundation/utility/alignedallocator.h
    foundation/utility/alignedvector.h
    foundation/utility/arena.h
    foundation/utility/attributeset.cpp
    foundation/utility/attributeset.h
    foundation/utility/autoreleaseptr.h
    foundation/utility/benchmark.h
    foundation/utility/bitmask.h
    foundation/utility/bufferedfile.cpp
    foundation/utility/bufferedfile.h
    foundation/utility/cache.cpp
    foundation/utility/cache.h
    foundation/utility/casts.h
    foundation/utility/cc.h
    foundation/utility/commandlineparser.h
    foundation/utility/copyonwrite.h
    foundation/utility/countof.h
    foundation/utility/filter.h
    foundation/utility/foreach.h
    foundation/utility/gnuplotfile.cpp
    foundation/utility/gnuplotfile.h
    foundation/utility/iesparser.cpp
    foundation/utility/iesparser.h
    foundation/utility/indenter.cpp
    foundation/utility/indenter.h
    foundation/utility/iostreamop.h
    foundation/utility/iterators.h
    foundation/utility/job.h
    foundation/utility/kvpair.h
    foundation/utility/lazy.h
    foundation/utility/log.h
    foundation/utility/makevector.h
    foundation/utility/memory.cpp
    foundation/utility/memory.h
    foundation/utility/murmurhash.cpp
    foundation/utility/murmurhash.h
    foundation/utility/numerictype.h
    foundation/utility/otherwise.h
    foundation/utility/poison.h
    foundation/utility/poolallocator.h
    foundation/utility/preprocessor.cpp
    foundation/utility/preprocessor.h
    foundation/utility/registrar.h
    foundation/utility/searchpaths.cpp
    foundation/utility/searchpaths.h
    foundation/utility/settings.h
    foundation/utility/siphash.cpp
    foundation/utility/siphash.h
    foundation/utility/statistics.cpp
    foundation/utility/statistics.h
    foundation/utility/stopwatch.h
    foundation/utility/string.cpp
    foundation/utility/string.h
    foundation/utility/test.h
    foundation/utility/testutils.cpp
    foundation/utility/testutils.h
    foundation/utility/tls.h
    foundation/utility/typetraits.h
    foundation/utility/uid.cpp
    foundation/utility/uid.h
    foundation/utility/version.h
    foundation/utility/vpythonfile.cpp
    foundation/utility/vpythonfile.h
    foundation/utility/xercesc.cpp
    foundation/utility/xercesc.h
    foundation/utility/xmlelement.h
    foundation/utility/zip.cpp
    foundation/utility/zip.h
)
if (WITH_DISNEY_MATERIAL)
    list (APPEND foundation_utility_sources
        foundation/utility/seexpr.cpp
        foundation/utility/seexpr.h
    )
endif ()
list (APPEND appleseed_sources
    ${foundation_utility_sources}
)
source_group ("foundation\\utility" FILES
    ${foundation_utility_sources}
)

set (main_sources
    main/allocator.cpp
    main/allocator.h
    main/dllmain.cpp
    main/dllsymbol.h
    main/dllvisibility.h
)
list (APPEND appleseed_sources
    ${main_sources}
)
source_group ("main" FILES
    ${main_sources}
)

set (renderer_api_sources
    renderer/api/aov.h
    renderer/api/bsdf.h
    renderer/api/bssrdf.h
    renderer/api/camera.h
    renderer/api/color.h
    renderer/api/display.h
    renderer/api/edf.h
    renderer/api/entity.h
    renderer/api/environment.h
    renderer/api/environmentedf.h
    renderer/api/environmentshader.h
    renderer/api/frame.h
    renderer/api/light.h
    renderer/api/lighting.h
    renderer/api/log.h
    renderer/api/material.h
    renderer/api/object.h
    renderer/api/postprocessing.h
    renderer/api/project.h
    renderer/api/rasterization.h
    renderer/api/rendering.h
    renderer/api/scene.h
    renderer/api/scenepicker.h
    renderer/api/shadergroup.h
    renderer/api/source.h
    renderer/api/surfaceshader.h
    renderer/api/texture.h
    renderer/api/trace.h
    renderer/api/types.h
    renderer/api/utility.h
    renderer/api/version.h
    renderer/api/volume.h
)
list (APPEND appleseed_sources
    ${renderer_api_sources}
)
source_group ("renderer\\api" FILES
    ${renderer_api_sources}
)

set (renderer_global_sources
    renderer/global/globallogger.cpp
    renderer/global/globallogger.h
    renderer/global/globaltypes.h
)
list (APPEND appleseed_sources
    ${renderer_global_sources}
)
source_group ("renderer\\global" FILES
    ${renderer_global_sources}
)

set (renderer_kernel_aov_sources
    renderer/kernel/aov/aovaccumulator.cpp
    renderer/kernel/aov/aovaccumulator.h
    renderer/kernel/aov/aovcomponents.cpp
    renderer/kernel/aov/aovcomponents.h
    renderer/kernel/aov/aovsettings.h
    renderer/kernel/aov/imagestack.cpp
    renderer/kernel/aov/imagestack.h
    renderer/kernel/aov/tilestack.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_aov_sources}
)
source_group ("renderer\\kernel\\aov" FILES
    ${renderer_kernel_aov_sources}
)

set (renderer_kernel_denoising_sources
    renderer/kernel/denoising/denoiser.cpp
    renderer/kernel/denoising/denoiser.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_denoising_sources}
)
source_group ("renderer\\kernel\\denoising" FILES
    ${renderer_kernel_denoising_sources}
)

set (renderer_kernel_intersection_sources
    renderer/kernel/intersection/assemblytree.cpp
    renderer/kernel/intersection/assemblytree.h
    renderer/kernel/intersection/curvekey.h
    renderer/kernel/intersection/curvetree.cpp
    renderer/kernel/intersection/curvetree.h
    renderer/kernel/intersection/intersectionfilter.cpp
    renderer/kernel/intersection/intersectionfilter.h
    renderer/kernel/intersection/intersectionsettings.h
    renderer/kernel/intersection/intersector.cpp
    renderer/kernel/intersection/intersector.h
    renderer/kernel/intersection/probevisitorbase.h
    renderer/kernel/intersection/tracecontext.cpp
    renderer/kernel/intersection/tracecontext.h
    renderer/kernel/intersection/treerepository.h
    renderer/kernel/intersection/triangleencoder.cpp
    renderer/kernel/intersection/triangleencoder.h
    renderer/kernel/intersection/triangleitemhandler.cpp
    renderer/kernel/intersection/triangleitemhandler.h
    renderer/kernel/intersection/trianglekey.h
    renderer/kernel/intersection/triangletree.cpp
    renderer/kernel/intersection/triangletree.h
    renderer/kernel/intersection/trianglevertexinfo.h
)
if (WITH_EMBREE)
    list (APPEND renderer_kernel_intersection_sources
        renderer/kernel/intersection/embreescene.cpp
        renderer/kernel/intersection/embreescene.h
    )
endif ()
list (APPEND appleseed_sources
    ${renderer_kernel_intersection_sources}
)
source_group ("renderer\\kernel\\intersection" FILES
    ${renderer_kernel_intersection_sources}
)

set (renderer_kernel_lighting_bdpt_sources
    renderer/kernel/lighting/bdpt/bdptlightingengine.cpp
    renderer/kernel/lighting/bdpt/bdptlightingengine.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_lighting_bdpt_sources}
)
source_group ("renderer\\kernel\\lighting\\bdpt" FILES
    ${renderer_kernel_lighting_bdpt_sources}
)

set (renderer_kernel_lighting_lighttracing_sources
    renderer/kernel/lighting/lighttracing/lighttracingsamplegenerator.cpp
    renderer/kernel/lighting/lighttracing/lighttracingsamplegenerator.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_lighting_lighttracing_sources}
)
source_group ("renderer\\kernel\\lighting\\lighttracing" FILES
    ${renderer_kernel_lighting_lighttracing_sources}
)

set (renderer_kernel_lighting_null_sources
    renderer/kernel/lighting/null/nulllightingengine.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_lighting_null_sources}
)
source_group ("renderer\\kernel\\lighting\\null" FILES
    ${renderer_kernel_lighting_null_sources}
)

set (renderer_kernel_lighting_pt_sources
    renderer/kernel/lighting/pt/ptlightingengine.cpp
    renderer/kernel/lighting/pt/ptlightingengine.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_lighting_pt_sources}
)
source_group ("renderer\\kernel\\lighting\\pt" FILES
    ${renderer_kernel_lighting_pt_sources}
)

set (renderer_kernel_lighting_sppm_sources
    renderer/kernel/lighting/sppm/sppmlightingengine.cpp
    renderer/kernel/lighting/sppm/sppmlightingengine.h
    renderer/kernel/lighting/sppm/sppmparameters.cpp
    renderer/kernel/lighting/sppm/sppmparameters.h
    renderer/kernel/lighting/sppm/sppmpasscallback.cpp
    renderer/kernel/lighting/sppm/sppmpasscallback.h
    renderer/kernel/lighting/sppm/sppmphoton.cpp
    renderer/kernel/lighting/sppm/sppmphoton.h
    renderer/kernel/lighting/sppm/sppmphotonmap.cpp
    renderer/kernel/lighting/sppm/sppmphotonmap.h
    renderer/kernel/lighting/sppm/sppmphotontracer.cpp
    renderer/kernel/lighting/sppm/sppmphotontracer.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_lighting_sppm_sources}
)
source_group ("renderer\\kernel\\lighting\\sppm" FILES
    ${renderer_kernel_lighting_sppm_sources}
)

set (renderer_kernel_lighting_sources
    renderer/kernel/lighting/backwardlightsampler.cpp
    renderer/kernel/lighting/backwardlightsampler.h
    renderer/kernel/lighting/directlightingintegrator.cpp
    renderer/kernel/lighting/directlightingintegrator.h
    renderer/kernel/lighting/forwardlightsampler.cpp
    renderer/kernel/lighting/forwardlightsampler.h
    renderer/kernel/lighting/ilightingengine.h
    renderer/kernel/lighting/imagebasedlighting.cpp
    renderer/kernel/lighting/imagebasedlighting.h
    renderer/kernel/lighting/lightpathrecorder.cpp
    renderer/kernel/lighting/lightpathrecorder.h
    renderer/kernel/lighting/lightpathstream.cpp
    renderer/kernel/lighting/lightpathstream.h
    renderer/kernel/lighting/lightsample.cpp
    renderer/kernel/lighting/lightsample.h
    renderer/kernel/lighting/lightsamplerbase.cpp
    renderer/kernel/lighting/lightsamplerbase.h
    renderer/kernel/lighting/lighttree.cpp
    renderer/kernel/lighting/lighttree.h
    renderer/kernel/lighting/lighttree_node.h
    renderer/kernel/lighting/lighttypes.h
    renderer/kernel/lighting/materialsamplers.cpp
    renderer/kernel/lighting/materialsamplers.h
    renderer/kernel/lighting/pathtracer.h
    renderer/kernel/lighting/pathvertex.cpp
    renderer/kernel/lighting/pathvertex.h
    renderer/kernel/lighting/scatteringmode.h
    renderer/kernel/lighting/tracer.cpp
    renderer/kernel/lighting/tracer.h
    renderer/kernel/lighting/volumelightingintegrator.cpp
    renderer/kernel/lighting/volumelightingintegrator.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_lighting_sources}
)
source_group ("renderer\\kernel\\lighting" FILES
    ${renderer_kernel_lighting_sources}
)

set (renderer_kernel_npr_sources
    renderer/kernel/npr/nprclosures.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_npr_sources}
)
source_group ("renderer\\kernel\\npr" FILES
    ${renderer_kernel_npr_sources}
)

set (renderer_kernel_rasterization_sources
    renderer/kernel/rasterization/objectrasterizer.h
    renderer/kernel/rasterization/rasterizationcamera.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_rasterization_sources}
)
source_group ("renderer\\kernel\\rasterization" FILES
    ${renderer_kernel_rasterization_sources}
)

set (renderer_kernel_rendering_debug_sources
    renderer/kernel/rendering/debug/blanksamplerenderer.cpp
    renderer/kernel/rendering/debug/blanksamplerenderer.h
    renderer/kernel/rendering/debug/blanktilerenderer.cpp
    renderer/kernel/rendering/debug/blanktilerenderer.h
    renderer/kernel/rendering/debug/debugsamplerenderer.cpp
    renderer/kernel/rendering/debug/debugsamplerenderer.h
    renderer/kernel/rendering/debug/debugtilerenderer.cpp
    renderer/kernel/rendering/debug/debugtilerenderer.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_rendering_debug_sources}
)
source_group ("renderer\\kernel\\rendering\\debug" FILES
    ${renderer_kernel_rendering_debug_sources}
)

set (renderer_kernel_rendering_final_sources
    renderer/kernel/rendering/final/adaptivetilerenderer.cpp
    renderer/kernel/rendering/final/adaptivetilerenderer.h
    renderer/kernel/rendering/final/pixelsampler.cpp
    renderer/kernel/rendering/final/pixelsampler.h
    renderer/kernel/rendering/final/uniformpixelrenderer.cpp
    renderer/kernel/rendering/final/uniformpixelrenderer.h
    renderer/kernel/rendering/final/variationtracker.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_rendering_final_sources}
)
source_group ("renderer\\kernel\\rendering\\final" FILES
    ${renderer_kernel_rendering_final_sources}
)

set (renderer_kernel_rendering_generic_sources
    renderer/kernel/rendering/generic/genericframerenderer.cpp
    renderer/kernel/rendering/generic/genericframerenderer.h
    renderer/kernel/rendering/generic/genericsamplegenerator.cpp
    renderer/kernel/rendering/generic/genericsamplegenerator.h
    renderer/kernel/rendering/generic/genericsamplerenderer.cpp
    renderer/kernel/rendering/generic/genericsamplerenderer.h
    renderer/kernel/rendering/generic/generictilerenderer.cpp
    renderer/kernel/rendering/generic/generictilerenderer.h
    renderer/kernel/rendering/generic/tilejob.cpp
    renderer/kernel/rendering/generic/tilejob.h
    renderer/kernel/rendering/generic/tilejobfactory.cpp
    renderer/kernel/rendering/generic/tilejobfactory.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_rendering_generic_sources}
)
source_group ("renderer\\kernel\\rendering\\generic" FILES
    ${renderer_kernel_rendering_generic_sources}
)

set (renderer_kernel_rendering_progressive_sources
    renderer/kernel/rendering/progressive/progressiveframerenderer.cpp
    renderer/kernel/rendering/progressive/progressiveframerenderer.h
    renderer/kernel/rendering/progressive/samplecounter.cpp
    renderer/kernel/rendering/progressive/samplecounter.h
    renderer/kernel/rendering/progressive/samplecounthistory.h
    renderer/kernel/rendering/progressive/samplegeneratorjob.cpp
    renderer/kernel/rendering/progressive/samplegeneratorjob.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_rendering_progressive_sources}
)
source_group ("renderer\\kernel\\rendering\\progressive" FILES
    ${renderer_kernel_rendering_progressive_sources}
)

set (renderer_kernel_rendering_sources
    renderer/kernel/rendering/defaultrenderercontroller.cpp
    renderer/kernel/rendering/defaultrenderercontroller.h
    renderer/kernel/rendering/ephemeralshadingresultframebufferfactory.cpp
    renderer/kernel/rendering/ephemeralshadingresultframebufferfactory.h
    renderer/kernel/rendering/globalsampleaccumulationbuffer.cpp
    renderer/kernel/rendering/globalsampleaccumulationbuffer.h
    renderer/kernel/rendering/iframerenderer.h
    renderer/kernel/rendering/ipasscallback.h
    renderer/kernel/rendering/ipixelrenderer.h
    renderer/kernel/rendering/irenderercontroller.h
    renderer/kernel/rendering/isamplegenerator.h
    renderer/kernel/rendering/isamplerenderer.h
    renderer/kernel/rendering/ishadingresultframebufferfactory.h
    renderer/kernel/rendering/itilecallback.h
    renderer/kernel/rendering/itilerenderer.h
    renderer/kernel/rendering/localsampleaccumulationbuffer.cpp
    renderer/kernel/rendering/localsampleaccumulationbuffer.h
    renderer/kernel/rendering/masterrenderer.cpp
    renderer/kernel/rendering/masterrenderer.h
    renderer/kernel/rendering/nulltilecallback.cpp
    renderer/kernel/rendering/nulltilecallback.h
    renderer/kernel/rendering/oiioerrorhandler.cpp
    renderer/kernel/rendering/oiioerrorhandler.h
    renderer/kernel/rendering/permanentshadingresultframebufferfactory.cpp
    renderer/kernel/rendering/permanentshadingresultframebufferfactory.h
    renderer/kernel/rendering/pixelcontext.h
    renderer/kernel/rendering/pixelrendererbase.cpp
    renderer/kernel/rendering/pixelrendererbase.h
    renderer/kernel/rendering/renderercomponents.cpp
    renderer/kernel/rendering/renderercomponents.h
    renderer/kernel/rendering/rendererservices.cpp
    renderer/kernel/rendering/rendererservices.h
    renderer/kernel/rendering/sample.h
    renderer/kernel/rendering/sampleaccumulationbuffer.h
    renderer/kernel/rendering/samplegeneratorbase.cpp
    renderer/kernel/rendering/samplegeneratorbase.h
    renderer/kernel/rendering/scenepicker.cpp
    renderer/kernel/rendering/scenepicker.h
    renderer/kernel/rendering/serialrenderercontroller.cpp
    renderer/kernel/rendering/serialrenderercontroller.h
    renderer/kernel/rendering/serialtilecallback.cpp
    renderer/kernel/rendering/serialtilecallback.h
    renderer/kernel/rendering/shadingresultframebuffer.cpp
    renderer/kernel/rendering/shadingresultframebuffer.h
    renderer/kernel/rendering/tilecallbackbase.h
    renderer/kernel/rendering/tilecallbackcollection.cpp
    renderer/kernel/rendering/tilecallbackcollection.h
    renderer/kernel/rendering/timedrenderercontroller.cpp
    renderer/kernel/rendering/timedrenderercontroller.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_rendering_sources}
)
source_group ("renderer\\kernel\\rendering" FILES
    ${renderer_kernel_rendering_sources}
)

set (renderer_kernel_shading_sources
    renderer/kernel/shading/ambientocclusion.h
    renderer/kernel/shading/closures.cpp
    renderer/kernel/shading/closures.h
    renderer/kernel/shading/directshadingcomponents.cpp
    renderer/kernel/shading/directshadingcomponents.h
    renderer/kernel/shading/fastambientocclusion.cpp
    renderer/kernel/shading/fastambientocclusion.h
    renderer/kernel/shading/oslshadergroupexec.cpp
    renderer/kernel/shading/oslshadergroupexec.h
    renderer/kernel/shading/oslshadingsystem.cpp
    renderer/kernel/shading/oslshadingsystem.h
    renderer/kernel/shading/shadingcomponents.cpp
    renderer/kernel/shading/shadingcomponents.h
    renderer/kernel/shading/shadingcontext.cpp
    renderer/kernel/shading/shadingcontext.h
    renderer/kernel/shading/shadingengine.cpp
    renderer/kernel/shading/shadingengine.h
    renderer/kernel/shading/shadingpoint.cpp
    renderer/kernel/shading/shadingpoint.h
    renderer/kernel/shading/shadingpointbuilder.cpp
    renderer/kernel/shading/shadingpointbuilder.h
    renderer/kernel/shading/shadingray.cpp
    renderer/kernel/shading/shadingray.h
    renderer/kernel/shading/shadingresult.cpp
    renderer/kernel/shading/shadingresult.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_shading_sources}
)
source_group ("renderer\\kernel\\shading" FILES
    ${renderer_kernel_shading_sources}
)

set (renderer_kernel_tessellation_sources
    renderer/kernel/tessellation/statictessellation.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_tessellation_sources}
)
source_group ("renderer\\kernel\\tessellation" FILES
    ${renderer_kernel_tessellation_sources}
)

set (renderer_kernel_texturing_sources
    renderer/kernel/texturing/oiiotexturesystem.cpp
    renderer/kernel/texturing/oiiotexturesystem.h
    renderer/kernel/texturing/texturecache.h
    renderer/kernel/texturing/texturestore.cpp
    renderer/kernel/texturing/texturestore.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_texturing_sources}
)
source_group ("renderer\\kernel\\texturing" FILES
    ${renderer_kernel_texturing_sources}
)

set (renderer_kernel_volume_sources
    renderer/kernel/volume/occupancygrid.cpp
    renderer/kernel/volume/occupancygrid.h
    renderer/kernel/volume/volume.cpp
    renderer/kernel/volume/volume.h
)
list (APPEND appleseed_sources
    ${renderer_kernel_volume_sources}
)
source_group ("renderer\\kernel\\volume" FILES
    ${renderer_kernel_volume_sources}
)

set (renderer_meta_benchmarks_sources
    renderer/meta/benchmarks/benchmark_dynamicspectrum.cpp
    renderer/meta/benchmarks/benchmark_frame.cpp
    renderer/meta/benchmarks/benchmark_localsampleaccumulationbuffer.cpp
    renderer/meta/benchmarks/benchmark_transformsequence.cpp
)
list (APPEND appleseed_sources
    ${renderer_meta_benchmarks_sources}
)
source_group ("renderer\\meta\\benchmarks" FILES
    ${renderer_meta_benchmarks_sources}
)

set (renderer_meta_tests_sources
    renderer/meta/tests/test_assembly.cpp
    renderer/meta/tests/test_backwardlightsampler.cpp
    renderer/meta/tests/test_containers.cpp
    renderer/meta/tests/test_dynamicspectrum.cpp
    renderer/meta/tests/test_energycompensation.cpp
    renderer/meta/tests/test_entitymap.cpp
    renderer/meta/tests/test_entityvector.cpp
    renderer/meta/tests/test_environmentedf.cpp
    renderer/meta/tests/test_forwardlightsampler.cpp
    renderer/meta/tests/test_frame.cpp
    renderer/meta/tests/test_imagetools.cpp
    renderer/meta/tests/test_inputarray.cpp
    renderer/meta/tests/test_intersector.cpp
    renderer/meta/tests/test_localsampleaccumulationbuffer.cpp
    renderer/meta/tests/test_paramarray.cpp
    renderer/meta/tests/test_pinholecamera.cpp
    renderer/meta/tests/test_pixelsampler.cpp
    renderer/meta/tests/test_projectfilereader.cpp
    renderer/meta/tests/test_projectfilewriter.cpp
    renderer/meta/tests/test_samplecounter.cpp
    renderer/meta/tests/test_samplecounthistory.cpp
    renderer/meta/tests/test_samplegeneratorjob.cpp
    renderer/meta/tests/test_scene.cpp
    renderer/meta/tests/test_shaderparamparser.cpp
    renderer/meta/tests/test_shadingresult.cpp
    renderer/meta/tests/test_sphericalcamera.cpp
    renderer/meta/tests/test_sss.cpp
    renderer/meta/tests/test_texturestore.cpp
    renderer/meta/tests/test_tracer.cpp
    renderer/meta/tests/test_transformsequence.cpp
    renderer/meta/tests/test_variationtracker.cpp
    renderer/meta/tests/test_volume.cpp
)
list (APPEND appleseed_sources
    ${renderer_meta_tests_sources}
)
source_group ("renderer\\meta\\tests" FILES
    ${renderer_meta_tests_sources}
)

set (renderer_modeling_aov_sources
    renderer/modeling/aov/albedoaov.cpp
    renderer/modeling/aov/albedoaov.h
    renderer/modeling/aov/aov.cpp
    renderer/modeling/aov/aov.h
    renderer/modeling/aov/aovcontainer.h
    renderer/modeling/aov/aovfactoryregistrar.cpp
    renderer/modeling/aov/aovfactoryregistrar.h
    renderer/modeling/aov/aovtraits.h
    renderer/modeling/aov/cryptomatteaov.cpp
    renderer/modeling/aov/cryptomatteaov.h
    renderer/modeling/aov/denoiseraov.cpp
    renderer/modeling/aov/denoiseraov.h
    renderer/modeling/aov/depthaov.cpp
    renderer/modeling/aov/depthaov.h
    renderer/modeling/aov/diffuseaov.cpp
    renderer/modeling/aov/diffuseaov.h
    renderer/modeling/aov/emissionaov.cpp
    renderer/modeling/aov/emissionaov.h
    renderer/modeling/aov/glossyaov.cpp
    renderer/modeling/aov/glossyaov.h
    renderer/modeling/aov/iaovfactory.h
    renderer/modeling/aov/invalidsamplesaov.cpp
    renderer/modeling/aov/invalidsamplesaov.h
    renderer/modeling/aov/normalaov.cpp
    renderer/modeling/aov/normalaov.h
    renderer/modeling/aov/npraovs.cpp
    renderer/modeling/aov/npraovs.h
    renderer/modeling/aov/pixelsamplecountaov.cpp
    renderer/modeling/aov/pixelsamplecountaov.h
    renderer/modeling/aov/pixeltimeaov.cpp
    renderer/modeling/aov/pixeltimeaov.h
    renderer/modeling/aov/pixelvariationaov.cpp
    renderer/modeling/aov/pixelvariationaov.h
    renderer/modeling/aov/positionaov.cpp
    renderer/modeling/aov/positionaov.h
    renderer/modeling/aov/screenspacevelocityaov.cpp
    renderer/modeling/aov/screenspacevelocityaov.h
    renderer/modeling/aov/uvaov.cpp
    renderer/modeling/aov/uvaov.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_aov_sources}
)
source_group ("renderer\\modeling\\aov" FILES
    ${renderer_modeling_aov_sources}
)

set (renderer_modeling_bsdf_sources
    renderer/modeling/bsdf/ashikhminbrdf.cpp
    renderer/modeling/bsdf/ashikhminbrdf.h
    renderer/modeling/bsdf/blinnbrdf.cpp
    renderer/modeling/bsdf/blinnbrdf.h
    renderer/modeling/bsdf/bsdf.cpp
    renderer/modeling/bsdf/bsdf.h
    renderer/modeling/bsdf/bsdfblend.cpp
    renderer/modeling/bsdf/bsdfblend.h
    renderer/modeling/bsdf/bsdffactoryregistrar.cpp
    renderer/modeling/bsdf/bsdffactoryregistrar.h
    renderer/modeling/bsdf/bsdfmix.cpp
    renderer/modeling/bsdf/bsdfmix.h
    renderer/modeling/bsdf/bsdfsample.cpp
    renderer/modeling/bsdf/bsdfsample.h
    renderer/modeling/bsdf/bsdftraits.h
    renderer/modeling/bsdf/bsdfwrapper.h
    renderer/modeling/bsdf/diffusebtdf.cpp
    renderer/modeling/bsdf/diffusebtdf.h
    renderer/modeling/bsdf/disneybrdf.cpp
    renderer/modeling/bsdf/disneybrdf.h
    renderer/modeling/bsdf/energycompensation.cpp
    renderer/modeling/bsdf/energycompensation.h
    renderer/modeling/bsdf/energycompensationtables.cpp
    renderer/modeling/bsdf/energycompensationtables.h
    renderer/modeling/bsdf/fresnel.h
    renderer/modeling/bsdf/glassbsdf.cpp
    renderer/modeling/bsdf/glassbsdf.h
    renderer/modeling/bsdf/glossybrdf.cpp
    renderer/modeling/bsdf/glossybrdf.h
    renderer/modeling/bsdf/ibsdffactory.h
    renderer/modeling/bsdf/kelemenbrdf.cpp
    renderer/modeling/bsdf/kelemenbrdf.h
    renderer/modeling/bsdf/lambertianbrdf.cpp
    renderer/modeling/bsdf/lambertianbrdf.h
    renderer/modeling/bsdf/metalbrdf.cpp
    renderer/modeling/bsdf/metalbrdf.h
    renderer/modeling/bsdf/microfacethelper.cpp
    renderer/modeling/bsdf/microfacethelper.h
    renderer/modeling/bsdf/nullbsdf.h
    renderer/modeling/bsdf/orennayarbrdf.cpp
    renderer/modeling/bsdf/orennayarbrdf.h
    renderer/modeling/bsdf/oslbsdf.cpp
    renderer/modeling/bsdf/oslbsdf.h
    renderer/modeling/bsdf/plasticbrdf.cpp
    renderer/modeling/bsdf/plasticbrdf.h
    renderer/modeling/bsdf/sheenbrdf.cpp
    renderer/modeling/bsdf/sheenbrdf.h
    renderer/modeling/bsdf/specularbrdf.cpp
    renderer/modeling/bsdf/specularbrdf.h
    renderer/modeling/bsdf/specularbtdf.cpp
    renderer/modeling/bsdf/specularbtdf.h
    renderer/modeling/bsdf/specularhelper.h
)
if (WITH_DISNEY_MATERIAL)
    list (APPEND renderer_modeling_bsdf_sources
        renderer/modeling/bsdf/disneylayeredbrdf.cpp
        renderer/modeling/bsdf/disneylayeredbrdf.h
    )
endif ()
list (APPEND appleseed_sources
    ${renderer_modeling_bsdf_sources}
)
source_group ("renderer\\modeling\\bsdf" FILES
    ${renderer_modeling_bsdf_sources}
)

set (renderer_modeling_bssrdf_sources
    renderer/modeling/bssrdf/betterdipolebssrdf.cpp
    renderer/modeling/bssrdf/betterdipolebssrdf.h
    renderer/modeling/bssrdf/bssrdf.cpp
    renderer/modeling/bssrdf/bssrdf.h
    renderer/modeling/bssrdf/bssrdffactoryregistrar.cpp
    renderer/modeling/bssrdf/bssrdffactoryregistrar.h
    renderer/modeling/bssrdf/bssrdfsample.h
    renderer/modeling/bssrdf/bssrdftraits.h
    renderer/modeling/bssrdf/dipolebssrdf.cpp
    renderer/modeling/bssrdf/dipolebssrdf.h
    renderer/modeling/bssrdf/dipolebssrdffactory.h
    renderer/modeling/bssrdf/directionaldipolebssrdf.cpp
    renderer/modeling/bssrdf/directionaldipolebssrdf.h
    renderer/modeling/bssrdf/gaussianbssrdf.cpp
    renderer/modeling/bssrdf/gaussianbssrdf.h
    renderer/modeling/bssrdf/ibssrdffactory.h
    renderer/modeling/bssrdf/normalizeddiffusionbssrdf.cpp
    renderer/modeling/bssrdf/normalizeddiffusionbssrdf.h
    renderer/modeling/bssrdf/oslbssrdf.cpp
    renderer/modeling/bssrdf/oslbssrdf.h
    renderer/modeling/bssrdf/randomwalkbssrdf.cpp
    renderer/modeling/bssrdf/randomwalkbssrdf.h
    renderer/modeling/bssrdf/separablebssrdf.cpp
    renderer/modeling/bssrdf/separablebssrdf.h
    renderer/modeling/bssrdf/sss.cpp
    renderer/modeling/bssrdf/sss.h
    renderer/modeling/bssrdf/standarddipolebssrdf.cpp
    renderer/modeling/bssrdf/standarddipolebssrdf.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_bssrdf_sources}
)
source_group ("renderer\\modeling\\bssrdf" FILES
    ${renderer_modeling_bssrdf_sources}
)

set (renderer_modeling_camera_sources
    renderer/modeling/camera/camera.cpp
    renderer/modeling/camera/camera.h
    renderer/modeling/camera/camerafactoryregistrar.cpp
    renderer/modeling/camera/camerafactoryregistrar.h
    renderer/modeling/camera/cameratraits.h
    renderer/modeling/camera/fisheyelenscamera.cpp
    renderer/modeling/camera/fisheyelenscamera.h
    renderer/modeling/camera/icamerafactory.h
    renderer/modeling/camera/orthographiccamera.cpp
    renderer/modeling/camera/orthographiccamera.h
    renderer/modeling/camera/perspectivecamera.cpp
    renderer/modeling/camera/perspectivecamera.h
    renderer/modeling/camera/pinholecamera.cpp
    renderer/modeling/camera/pinholecamera.h
    renderer/modeling/camera/sphericalcamera.cpp
    renderer/modeling/camera/sphericalcamera.h
    renderer/modeling/camera/thinlenscamera.cpp
    renderer/modeling/camera/thinlenscamera.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_camera_sources}
)
source_group ("renderer\\modeling\\camera" FILES
    ${renderer_modeling_camera_sources}
)

set (renderer_modeling_color_sources
    renderer/modeling/color/colorentity.cpp
    renderer/modeling/color/colorentity.h
    renderer/modeling/color/colorspace.cpp
    renderer/modeling/color/colorspace.h
    renderer/modeling/color/colortraits.h
    renderer/modeling/color/wavelengths.cpp
    renderer/modeling/color/wavelengths.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_color_sources}
)
source_group ("renderer\\modeling\\color" FILES
    ${renderer_modeling_color_sources}
)

set (renderer_modeling_display_sources
    renderer/modeling/display/display.cpp
    renderer/modeling/display/display.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_display_sources}
)
source_group ("renderer\\modeling\\display" FILES
    ${renderer_modeling_display_sources}
)

set (renderer_modeling_edf_sources
    renderer/modeling/edf/coneedf.cpp
    renderer/modeling/edf/coneedf.h
    renderer/modeling/edf/diffuseedf.cpp
    renderer/modeling/edf/diffuseedf.h
    renderer/modeling/edf/edf.cpp
    renderer/modeling/edf/edf.h
    renderer/modeling/edf/edffactoryregistrar.cpp
    renderer/modeling/edf/edffactoryregistrar.h
    renderer/modeling/edf/edftraits.h
    renderer/modeling/edf/iedffactory.cpp
    renderer/modeling/edf/iedffactory.h
    renderer/modeling/edf/osledf.cpp
    renderer/modeling/edf/osledf.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_edf_sources}
)
source_group ("renderer\\modeling\\edf" FILES
    ${renderer_modeling_edf_sources}
)

set (renderer_modeling_entity_sources
    renderer/modeling/entity/connectableentity.cpp
    renderer/modeling/entity/connectableentity.h
    renderer/modeling/entity/entity.cpp
    renderer/modeling/entity/entity.h
    renderer/modeling/entity/entityfactoryregistrar.h
    renderer/modeling/entity/entitymap.cpp
    renderer/modeling/entity/entitymap.h
    renderer/modeling/entity/entitytraits.h
    renderer/modeling/entity/entityvector.cpp
    renderer/modeling/entity/entityvector.h
    renderer/modeling/entity/onframebeginrecorder.cpp
    renderer/modeling/entity/onframebeginrecorder.h
    renderer/modeling/entity/onrenderbeginrecorder.cpp
    renderer/modeling/entity/onrenderbeginrecorder.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_entity_sources}
)
source_group ("renderer\\modeling\\entity" FILES
    ${renderer_modeling_entity_sources}
)

set (renderer_modeling_environment_sources
    renderer/modeling/environment/environment.cpp
    renderer/modeling/environment/environment.h
    renderer/modeling/environment/environmenttraits.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_environment_sources}
)
source_group ("renderer\\modeling\\environment" FILES
    ${renderer_modeling_environment_sources}
)

set (renderer_modeling_environmentedf_sources
    renderer/modeling/environmentedf/ArHosekSkyModelData_CIEXYZ.h
    renderer/modeling/environmentedf/constantenvironmentedf.cpp
    renderer/modeling/environmentedf/constantenvironmentedf.h
    renderer/modeling/environmentedf/constanthemisphereenvironmentedf.cpp
    renderer/modeling/environmentedf/constanthemisphereenvironmentedf.h
    renderer/modeling/environmentedf/environmentedf.cpp
    renderer/modeling/environmentedf/environmentedf.h
    renderer/modeling/environmentedf/environmentedffactoryregistrar.cpp
    renderer/modeling/environmentedf/environmentedffactoryregistrar.h
    renderer/modeling/environmentedf/environmentedftraits.h
    renderer/modeling/environmentedf/gradientenvironmentedf.cpp
    renderer/modeling/environmentedf/gradientenvironmentedf.h
    renderer/modeling/environmentedf/hosekenvironmentedf.cpp
    renderer/modeling/environmentedf/hosekenvironmentedf.h
    renderer/modeling/environmentedf/ienvironmentedffactory.cpp
    renderer/modeling/environmentedf/ienvironmentedffactory.h
    renderer/modeling/environmentedf/latlongmapenvironmentedf.cpp
    renderer/modeling/environmentedf/latlongmapenvironmentedf.h
    renderer/modeling/environmentedf/mirrorballmapenvironmentedf.cpp
    renderer/modeling/environmentedf/mirrorballmapenvironmentedf.h
    renderer/modeling/environmentedf/oslenvironmentedf.cpp
    renderer/modeling/environmentedf/oslenvironmentedf.h
    renderer/modeling/environmentedf/preethamenvironmentedf.cpp
    renderer/modeling/environmentedf/preethamenvironmentedf.h
    renderer/modeling/environmentedf/sphericalcoordinates.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_environmentedf_sources}
)
source_group ("renderer\\modeling\\environmentedf" FILES
    ${renderer_modeling_environmentedf_sources}
)

set (renderer_modeling_environmentshader_sources
    renderer/modeling/environmentshader/backgroundenvironmentshader.cpp
    renderer/modeling/environmentshader/backgroundenvironmentshader.h
    renderer/modeling/environmentshader/edfenvironmentshader.cpp
    renderer/modeling/environmentshader/edfenvironmentshader.h
    renderer/modeling/environmentshader/environmentshader.cpp
    renderer/modeling/environmentshader/environmentshader.h
    renderer/modeling/environmentshader/environmentshaderfactoryregistrar.cpp
    renderer/modeling/environmentshader/environmentshaderfactoryregistrar.h
    renderer/modeling/environmentshader/environmentshadertraits.h
    renderer/modeling/environmentshader/ienvironmentshaderfactory.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_environmentshader_sources}
)
source_group ("renderer\\modeling\\environmentshader" FILES
    ${renderer_modeling_environmentshader_sources}
)

set (renderer_modeling_frame_sources
    renderer/modeling/frame/frame.cpp
    renderer/modeling/frame/frame.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_frame_sources}
)
source_group ("renderer\\modeling\\frame" FILES
    ${renderer_modeling_frame_sources}
)

set (renderer_modeling_input_sources
    renderer/modeling/input/colorsource.cpp
    renderer/modeling/input/colorsource.h
    renderer/modeling/input/inputarray.cpp
    renderer/modeling/input/inputarray.h
    renderer/modeling/input/inputbinder.cpp
    renderer/modeling/input/inputbinder.h
    renderer/modeling/input/scalarsource.h
    renderer/modeling/input/sourceinputs.h
    renderer/modeling/input/sourceinputs.cpp
    renderer/modeling/input/source.h
    renderer/modeling/input/symbol.h
    renderer/modeling/input/texturesource.cpp
    renderer/modeling/input/texturesource.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_input_sources}
)
source_group ("renderer\\modeling\\input" FILES
    ${renderer_modeling_input_sources}
)

set (renderer_modeling_light_sources
    renderer/modeling/light/directionallight.cpp
    renderer/modeling/light/directionallight.h
    renderer/modeling/light/ilightfactory.cpp
    renderer/modeling/light/ilightfactory.h
    renderer/modeling/light/light.cpp
    renderer/modeling/light/light.h
    renderer/modeling/light/lightfactoryregistrar.cpp
    renderer/modeling/light/lightfactoryregistrar.h
    renderer/modeling/light/lighttarget.cpp
    renderer/modeling/light/lighttarget.h
    renderer/modeling/light/lighttraits.h
    renderer/modeling/light/maxomnilight.cpp
    renderer/modeling/light/maxomnilight.h
    renderer/modeling/light/maxspotlight.cpp
    renderer/modeling/light/maxspotlight.h
    renderer/modeling/light/pointlight.cpp
    renderer/modeling/light/pointlight.h
    renderer/modeling/light/spotlight.cpp
    renderer/modeling/light/spotlight.h
    renderer/modeling/light/sunlight.cpp
    renderer/modeling/light/sunlight.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_light_sources}
)
source_group ("renderer\\modeling\\light" FILES
    ${renderer_modeling_light_sources}
)

set (renderer_modeling_material_sources
    renderer/modeling/material/bumpmappingmodifier.cpp
    renderer/modeling/material/bumpmappingmodifier.h
    renderer/modeling/material/genericmaterial.cpp
    renderer/modeling/material/genericmaterial.h
    renderer/modeling/material/ibasismodifier.h
    renderer/modeling/material/imaterialfactory.cpp
    renderer/modeling/material/imaterialfactory.h
    renderer/modeling/material/material.cpp
    renderer/modeling/material/material.h
    renderer/modeling/material/materialfactoryregistrar.cpp
    renderer/modeling/material/materialfactoryregistrar.h
    renderer/modeling/material/materialtraits.h
    renderer/modeling/material/normalmappingmodifier.cpp
    renderer/modeling/material/normalmappingmodifier.h
    renderer/modeling/material/oslmaterial.cpp
    renderer/modeling/material/oslmaterial.h
)
if (WITH_DISNEY_MATERIAL)
    list (APPEND renderer_modeling_material_sources
        renderer/modeling/material/disneymaterial.cpp
        renderer/modeling/material/disneymaterial.h
    )
endif ()
list (APPEND appleseed_sources
    ${renderer_modeling_material_sources}
)
source_group ("renderer\\modeling\\material" FILES
    ${renderer_modeling_material_sources}
)

set (renderer_modeling_object_sources
    renderer/modeling/object/curveobject.cpp
    renderer/modeling/object/curveobject.h
    renderer/modeling/object/curveobjectreader.cpp
    renderer/modeling/object/curveobjectreader.h
    renderer/modeling/object/curveobjectwriter.cpp
    renderer/modeling/object/curveobjectwriter.h
    renderer/modeling/object/iobjectfactory.cpp
    renderer/modeling/object/iobjectfactory.h
    renderer/modeling/object/meshobject.cpp
    renderer/modeling/object/meshobject.h
    renderer/modeling/object/meshobjectoperations.cpp
    renderer/modeling/object/meshobjectoperations.h
    renderer/modeling/object/meshobjectprimitives.cpp
    renderer/modeling/object/meshobjectprimitives.h
    renderer/modeling/object/meshobjectreader.cpp
    renderer/modeling/object/meshobjectreader.h
    renderer/modeling/object/meshobjectwriter.cpp
    renderer/modeling/object/meshobjectwriter.h
    renderer/modeling/object/object.cpp
    renderer/modeling/object/object.h
    renderer/modeling/object/objectfactoryregistrar.cpp
    renderer/modeling/object/objectfactoryregistrar.h
    renderer/modeling/object/objecttraits.h
    renderer/modeling/object/proceduralobject.cpp
    renderer/modeling/object/proceduralobject.h
    renderer/modeling/object/triangle.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_object_sources}
)
source_group ("renderer\\modeling\\object" FILES
    ${renderer_modeling_object_sources}
)

set (renderer_modeling_postprocessingstage_sources
    renderer/modeling/postprocessingstage/colormapdata.cpp
    renderer/modeling/postprocessingstage/colormapdata.h
    renderer/modeling/postprocessingstage/colormappostprocessingstage.cpp
    renderer/modeling/postprocessingstage/colormappostprocessingstage.h
    renderer/modeling/postprocessingstage/ipostprocessingstagefactory.cpp
    renderer/modeling/postprocessingstage/ipostprocessingstagefactory.h
    renderer/modeling/postprocessingstage/postprocessingstage.cpp
    renderer/modeling/postprocessingstage/postprocessingstage.h
    renderer/modeling/postprocessingstage/postprocessingstagecontainer.h
    renderer/modeling/postprocessingstage/postprocessingstagefactoryregistrar.cpp
    renderer/modeling/postprocessingstage/postprocessingstagefactoryregistrar.h
    renderer/modeling/postprocessingstage/postprocessingstagetraits.h
    renderer/modeling/postprocessingstage/renderstamppostprocessingstage.cpp
    renderer/modeling/postprocessingstage/renderstamppostprocessingstage.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_postprocessingstage_sources}
)
source_group ("renderer\\modeling\\postprocessingstage" FILES
    ${renderer_modeling_postprocessingstage_sources}
)

set (renderer_modeling_project_sources
    renderer/modeling/project/assethandler.cpp
    renderer/modeling/project/assethandler.h
    renderer/modeling/project/configuration.cpp
    renderer/modeling/project/configuration.h
    renderer/modeling/project/configurationcontainer.h
    renderer/modeling/project/eventcounters.h
    renderer/modeling/project/project.cpp
    renderer/modeling/project/project.h
    renderer/modeling/project/project.xsd
    renderer/modeling/project/projectfilereader.cpp
    renderer/modeling/project/projectfilereader.h
    renderer/modeling/project/projectfileupdater.cpp
    renderer/modeling/project/projectfileupdater.h
    renderer/modeling/project/projectfilewriter.cpp
    renderer/modeling/project/projectfilewriter.h
    renderer/modeling/project/projectformatrevision.h
    renderer/modeling/project/projecttracker.cpp
    renderer/modeling/project/projecttracker.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_project_sources}
)
source_group ("renderer\\modeling\\project" FILES
    ${renderer_modeling_project_sources}
)

set (renderer_modeling_project-builtin_sources
    renderer/modeling/project-builtin/cornellboxproject.cpp
    renderer/modeling/project-builtin/cornellboxproject.h
    renderer/modeling/project-builtin/defaultproject.cpp
    renderer/modeling/project-builtin/defaultproject.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_project-builtin_sources}
)
source_group ("renderer\\modeling\\project-builtin" FILES
    ${renderer_modeling_project-builtin_sources}
)

set (renderer_modeling_scene_sources
    renderer/modeling/scene/archiveassembly.cpp
    renderer/modeling/scene/archiveassembly.h
    renderer/modeling/scene/assembly.cpp
    renderer/modeling/scene/assembly.h
    renderer/modeling/scene/assemblyfactoryregistrar.cpp
    renderer/modeling/scene/assemblyfactoryregistrar.h
    renderer/modeling/scene/assemblyinstance.cpp
    renderer/modeling/scene/assemblyinstance.h
    renderer/modeling/scene/assemblyinstancetraits.h
    renderer/modeling/scene/assemblytraits.h
    renderer/modeling/scene/basegroup.cpp
    renderer/modeling/scene/basegroup.h
    renderer/modeling/scene/containers.cpp
    renderer/modeling/scene/containers.h
    renderer/modeling/scene/iassemblyfactory.h
    renderer/modeling/scene/objectinstance.cpp
    renderer/modeling/scene/objectinstance.h
    renderer/modeling/scene/objectinstancetraits.h
    renderer/modeling/scene/proceduralassembly.cpp
    renderer/modeling/scene/proceduralassembly.h
    renderer/modeling/scene/scene.cpp
    renderer/modeling/scene/scene.h
    renderer/modeling/scene/textureinstance.cpp
    renderer/modeling/scene/textureinstance.h
    renderer/modeling/scene/textureinstancetraits.h
    renderer/modeling/scene/visibilityflags.cpp
    renderer/modeling/scene/visibilityflags.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_scene_sources}
)
source_group ("renderer\\modeling\\scene" FILES
    ${renderer_modeling_scene_sources}
)

set (renderer_modeling_shadergroup
    renderer/modeling/shadergroup/shader.cpp
    renderer/modeling/shadergroup/shader.h
    renderer/modeling/shadergroup/shadercompiler.cpp
    renderer/modeling/shadergroup/shadercompiler.h
    renderer/modeling/shadergroup/shaderconnection.cpp
    renderer/modeling/shadergroup/shaderconnection.h
    renderer/modeling/shadergroup/shadergroup.cpp
    renderer/modeling/shadergroup/shadergroup.h
    renderer/modeling/shadergroup/shadergrouptraits.h
    renderer/modeling/shadergroup/shaderparam.cpp
    renderer/modeling/shadergroup/shaderparam.h
    renderer/modeling/shadergroup/shaderquery.cpp
    renderer/modeling/shadergroup/shaderquery.h
    renderer/modeling/shadergroup/shaderparamparser.cpp
    renderer/modeling/shadergroup/shaderparamparser.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_shadergroup}
)
source_group ("renderer\\modeling\\shadergroup" FILES
    ${renderer_modeling_shadergroup}
)

set (renderer_modeling_surfaceshader_sources
    renderer/modeling/surfaceshader/aosurfaceshader.cpp
    renderer/modeling/surfaceshader/aosurfaceshader.h
    renderer/modeling/surfaceshader/constantsurfaceshader.cpp
    renderer/modeling/surfaceshader/constantsurfaceshader.h
    renderer/modeling/surfaceshader/diagnosticsurfaceshader.cpp
    renderer/modeling/surfaceshader/diagnosticsurfaceshader.h
    renderer/modeling/surfaceshader/isurfaceshaderfactory.h
    renderer/modeling/surfaceshader/nprsurfaceshaderhelper.cpp
    renderer/modeling/surfaceshader/nprsurfaceshaderhelper.h
    renderer/modeling/surfaceshader/physicalsurfaceshader.cpp
    renderer/modeling/surfaceshader/physicalsurfaceshader.h
    renderer/modeling/surfaceshader/surfaceshader.cpp
    renderer/modeling/surfaceshader/surfaceshader.h
    renderer/modeling/surfaceshader/surfaceshaderfactoryregistrar.cpp
    renderer/modeling/surfaceshader/surfaceshaderfactoryregistrar.h
    renderer/modeling/surfaceshader/surfaceshadertraits.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_surfaceshader_sources}
)
source_group ("renderer\\modeling\\surfaceshader" FILES
    ${renderer_modeling_surfaceshader_sources}
)

set (renderer_modeling_texture_sources
    renderer/modeling/texture/disktexture2d.cpp
    renderer/modeling/texture/disktexture2d.h
    renderer/modeling/texture/itexturefactory.h
    renderer/modeling/texture/memorytexture2d.cpp
    renderer/modeling/texture/memorytexture2d.h
    renderer/modeling/texture/texture.cpp
    renderer/modeling/texture/texture.h
    renderer/modeling/texture/texturefactoryregistrar.cpp
    renderer/modeling/texture/texturefactoryregistrar.h
    renderer/modeling/texture/texturetraits.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_texture_sources}
)
source_group ("renderer\\modeling\\texture" FILES
    ${renderer_modeling_texture_sources}
)

set (renderer_utility_sources
    renderer/utility/autodeskmax.h
    renderer/utility/bbox.h
    renderer/utility/dynamicspectrum.h
    renderer/utility/filesystem.cpp
    renderer/utility/filesystem.h
    renderer/utility/iostreamop.h
    renderer/utility/messagecontext.cpp
    renderer/utility/messagecontext.h
    renderer/utility/paramarray.cpp
    renderer/utility/paramarray.h
    renderer/utility/plugin.cpp
    renderer/utility/plugin.h
    renderer/utility/pluginstore.cpp
    renderer/utility/pluginstore.h
    renderer/utility/projectpoints.cpp
    renderer/utility/projectpoints.h
    renderer/utility/seexpr.h
    renderer/utility/settingsparsing.cpp
    renderer/utility/settingsparsing.h
    renderer/utility/spectrumclamp.h
    renderer/utility/stochasticcast.h
    renderer/utility/testutils.cpp
    renderer/utility/testutils.h
    renderer/utility/transformsequence.cpp
    renderer/utility/transformsequence.h
    renderer/utility/triangle.h
)
list (APPEND appleseed_sources
    ${renderer_utility_sources}
)
source_group ("renderer\\utility" FILES
    ${renderer_utility_sources}
)

set (renderer_modeling_volume_sources
    renderer/modeling/volume/genericvolume.cpp
    renderer/modeling/volume/genericvolume.h
    renderer/modeling/volume/ivolumefactory.h
    renderer/modeling/volume/volume.cpp
    renderer/modeling/volume/volume.h
    renderer/modeling/volume/volumefactoryregistrar.cpp
    renderer/modeling/volume/volumefactoryregistrar.h
    renderer/modeling/volume/volumetraits.h
)
list (APPEND appleseed_sources
    ${renderer_modeling_volume_sources}
)
source_group ("renderer\\modeling\\volume" FILES
    ${renderer_modeling_volume_sources}
)

# Collect all XML Schema files amongst the source files.
filter_list (
    appleseed_schema_files
    "${appleseed_sources}"
    ".*\\\\.xsd"
)

# Exclude all XML Schema files from the build.
set_source_files_properties (${appleseed_schema_files} PROPERTIES
    HEADER_FILE_ONLY TRUE
)


#--------------------------------------------------------------------------------------------------
# Target.
#--------------------------------------------------------------------------------------------------

add_library (appleseed SHARED
    ${appleseed_sources}
)

set_target_properties (appleseed PROPERTIES FOLDER "Core")

if (USE_VISIBILITY_MAP)
    set_target_properties (appleseed PROPERTIES
        LINK_FLAGS "-Wl,--version-script=${PROJECT_SOURCE_DIR}/src/appleseed/libappleseed.map"
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Preprocessor definitions.
#--------------------------------------------------------------------------------------------------

apply_preprocessor_definitions (appleseed)

append_custom_preprocessor_definitions (appleseed
    APPLESEED_ENABLE_IMATH_INTEROP
)


#--------------------------------------------------------------------------------------------------
# Static libraries.
# Static libraries must be specified in order of reverse-dependency.
#--------------------------------------------------------------------------------------------------

link_against_platform (appleseed)
link_against_libpng (appleseed)
link_against_xercesc (appleseed)

link_against_openexr (appleseed)
link_against_zlib (appleseed)
link_against_lz4 (appleseed)

link_against_oiio (appleseed)
link_against_osl (appleseed)

if (WITH_EMBREE)
    link_against_embree (appleseed)
endif ()

if (WITH_DISNEY_MATERIAL)
    link_against_seexpr (appleseed)
endif ()

target_link_libraries (appleseed
    bcd
    ${Boost_LIBRARIES}
    murmurhash3
    ${CMAKE_DL_LIBS}
    "${APPLESEED_LINK_EXTRA_LIBRARIES}"
)


#--------------------------------------------------------------------------------------------------
# Pre-build commands.
#--------------------------------------------------------------------------------------------------

# Copy OSL and OIIO tools to the sandbox.
if (USE_FIND_PACKAGE_FOR_OSL)
    add_custom_command (TARGET appleseed
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${OSL_QUERY_INFO} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
    add_custom_command (TARGET appleseed
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${OSL_MAKETX} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
    add_custom_command (TARGET appleseed
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${OPENIMAGEIO_OIIOTOOL} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
    add_custom_command (TARGET appleseed
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${OPENIMAGEIO_IDIFF} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
else ()
    add_custom_command (TARGET appleseed
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${APPLESEED_DEPS_STAGE_DIR}/osl-release/bin/oslinfo${exe_ext} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
    add_custom_command (TARGET appleseed
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${APPLESEED_DEPS_STAGE_DIR}/oiio-release/bin/maketx${exe_ext} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
    add_custom_command (TARGET appleseed
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${APPLESEED_DEPS_STAGE_DIR}/oiio-release/bin/oiiotool${exe_ext} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
    add_custom_command (TARGET appleseed
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${APPLESEED_DEPS_STAGE_DIR}/oiio-release/bin/idiff${exe_ext} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Post-build commands.
#--------------------------------------------------------------------------------------------------

add_copy_target_lib_to_sandbox_command (appleseed)

# Copy XML schema files to the sandbox.
foreach (schema_file ${appleseed_schema_files})
    add_custom_command (TARGET appleseed POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${schema_file} ${PROJECT_SOURCE_DIR}/sandbox/schemas/
    )
endforeach ()


#--------------------------------------------------------------------------------------------------
# Installation.
#--------------------------------------------------------------------------------------------------

if (CMAKE_SYSTEM_NAME STREQUAL "Windows")
    install (TARGETS appleseed
        RUNTIME
        DESTINATION bin
    )

    install (TARGETS appleseed
        ARCHIVE
        DESTINATION lib
    )
else ()
    install (TARGETS appleseed
        DESTINATION lib
    )
endif ()
