
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2010 Francois Beaune
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


#
# Source Files.
#

set(foundation_core_concepts_sources
    foundation/core/concepts/icloneable.h
    foundation/core/concepts/iunknown.h
    foundation/core/concepts/noncopyable.h
    foundation/core/concepts/singleton.h
)
list(APPEND appleseed_sources
    ${foundation_core_concepts_sources}
)
source_group("foundation\\core\\concepts" FILES
    ${foundation_core_concepts_sources}
)

set(foundation_core_sources
    foundation/core/appleseed.cpp
    foundation/core/appleseed.h
    foundation/core/concepts.h
    foundation/core/exception.h
    foundation/core/exceptionnotimplemented.h
    foundation/core/stringexception.h
)
list(APPEND appleseed_sources
    ${foundation_core_sources}
)
source_group("foundation\\core" FILES
    ${foundation_core_sources}
)

set(foundation_image_sources
    foundation/image/canvasproperties.h
    foundation/image/color.h
    foundation/image/colorspace.cpp
    foundation/image/colorspace.h
    foundation/image/drawing.h
    foundation/image/exrimagefilereader.cpp
    foundation/image/exrimagefilereader.h
    foundation/image/exrimagefilewriter.cpp
    foundation/image/exrimagefilewriter.h
    foundation/image/exrutils.cpp
    foundation/image/exrutils.h
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
    foundation/image/iprogressiveimagefilewriter.h
    foundation/image/nativedrawing.cpp
    foundation/image/nativedrawing.h
    foundation/image/pixel.cpp
    foundation/image/pixel.h
    foundation/image/pngimagefilereader.cpp
    foundation/image/pngimagefilereader.h
    foundation/image/pngimagefilewriter.cpp
    foundation/image/pngimagefilewriter.h
    foundation/image/progressiveexrimagefilereader.cpp
    foundation/image/progressiveexrimagefilereader.h
    foundation/image/progressiveexrimagefilewriter.cpp
    foundation/image/progressiveexrimagefilewriter.h
    foundation/image/spectrum.h
    foundation/image/tile.cpp
    foundation/image/tile.h
)
list(APPEND appleseed_sources
    ${foundation_image_sources}
)
source_group("foundation\\image" FILES
    ${foundation_image_sources}
)

set(foundation_math_bsp_sources
    foundation/math/bsp/bsp_builder.h
    foundation/math/bsp/bsp_intersector.h
    foundation/math/bsp/bsp_node.h
    foundation/math/bsp/bsp_statistics.cpp
    foundation/math/bsp/bsp_statistics.h
    foundation/math/bsp/bsp_tree.h
)
list(APPEND appleseed_sources
    ${foundation_math_bsp_sources}
)
source_group("foundation\\math\\bsp" FILES
    ${foundation_math_bsp_sources}
)

set(foundation_math_bvh_sources
    foundation/math/bvh/bvh_builder.h
    foundation/math/bvh/bvh_intersector.h
    foundation/math/bvh/bvh_node.h
    foundation/math/bvh/bvh_statistics.cpp
    foundation/math/bvh/bvh_statistics.h
    foundation/math/bvh/bvh_tree.h
)
list(APPEND appleseed_sources
    ${foundation_math_bvh_sources}
)
source_group("foundation\\math\\bvh" FILES
    ${foundation_math_bvh_sources}
)

set(foundation_math_intersection_sources
    foundation/math/intersection/aabbtriangle.h
    foundation/math/intersection/rayaabb.h
    foundation/math/intersection/rayplane.h
    foundation/math/intersection/raysphere.h
    foundation/math/intersection/raytrianglemt.h
    foundation/math/intersection/raytrianglessk.h
)
list(APPEND appleseed_sources
    ${foundation_math_intersection_sources}
)
source_group("foundation\\math\\intersection" FILES
    ${foundation_math_intersection_sources}
)

set(foundation_math_rng_sources
    foundation/math/rng/distribution.h
    foundation/math/rng/lcg.h
    foundation/math/rng/mersennetwister.cpp
    foundation/math/rng/mersennetwister.h
    foundation/math/rng/xorshift.h
)
list(APPEND appleseed_sources
    ${foundation_math_rng_sources}
)
source_group("foundation\\math\\rng" FILES
    ${foundation_math_rng_sources}
)

set(foundation_math_sampling_sources
    foundation/math/sampling/distribution.h
    foundation/math/sampling/qmcsamplingcontext.h
    foundation/math/sampling/rngsamplingcontext.h
    foundation/math/sampling/rqmcsamplingcontext.h
)
list(APPEND appleseed_sources
    ${foundation_math_sampling_sources}
)
source_group("foundation\\math\\sampling" FILES
    ${foundation_math_sampling_sources}
)

set(foundation_math_voxel_sources
    foundation/math/voxel/voxel_builder.h
    foundation/math/voxel/voxel_intersector.h
    foundation/math/voxel/voxel_node.h
    foundation/math/voxel/voxel_statistics.cpp
    foundation/math/voxel/voxel_statistics.h
    foundation/math/voxel/voxel_tree.h
)
list(APPEND appleseed_sources
    ${foundation_math_voxel_sources}
)
source_group("foundation\\math\\voxel" FILES
    ${foundation_math_voxel_sources}
)

set(foundation_math_sources
    foundation/math/aabb.h
    foundation/math/area.h
    foundation/math/basis.h
    foundation/math/bih.h
    foundation/math/bsp.h
    foundation/math/bvh.h
    foundation/math/cdf.h
    foundation/math/combination.h
    foundation/math/distance.h
    foundation/math/fastmath.h
    foundation/math/fp.h
    foundation/math/fresnel.h
    foundation/math/hash.h
    foundation/math/intersection.h
    foundation/math/matrix.h
    foundation/math/minmax.h
    foundation/math/mis.h
    foundation/math/noise.cpp
    foundation/math/noise.h
    foundation/math/ordering.cpp
    foundation/math/ordering.h
    foundation/math/particlemap.h
    foundation/math/permutation.cpp
    foundation/math/permutation.h
    foundation/math/population.h
    foundation/math/primes.cpp
    foundation/math/primes.h
    foundation/math/qmc.h
    foundation/math/quaternion.h
    foundation/math/ray.h
    foundation/math/rng.h
    foundation/math/rr.h
    foundation/math/sah.h
    foundation/math/sampling.h
    foundation/math/scalar.h
    foundation/math/spline.h
    foundation/math/split.h
    foundation/math/transform.h
    foundation/math/triangulator.h
    foundation/math/vector.h
    foundation/math/voxel.h
    foundation/math/voxelgrid.h
)
list(APPEND appleseed_sources
    ${foundation_math_sources}
)
source_group("foundation\\math" FILES
    ${foundation_math_sources}
)

set(foundation_mesh_sources
    foundation/mesh/imeshbuilder.h
    foundation/mesh/imeshfilereader.h
    foundation/mesh/imeshfilewriter.h
    foundation/mesh/imeshwalker.h
    foundation/mesh/iobjmeshbuilder.h
    foundation/mesh/meshbuilderbase.h
    foundation/mesh/objmeshfilelexer.h
    foundation/mesh/objmeshfilereader.cpp
    foundation/mesh/objmeshfilereader.h
    foundation/mesh/objmeshfilewriter.cpp
    foundation/mesh/objmeshfilewriter.h
)
list(APPEND appleseed_sources
    ${foundation_mesh_sources}
)
source_group("foundation\\mesh" FILES
    ${foundation_mesh_sources}
)

set(foundation_meta_benchmarks_sources
    foundation/meta/benchmarks/benchmark_cdf.cpp
    foundation/meta/benchmarks/benchmark_intersection.cpp
    foundation/meta/benchmarks/benchmark_job.cpp
    foundation/meta/benchmarks/benchmark_matrix.cpp
    foundation/meta/benchmarks/benchmark_permutation.cpp
    foundation/meta/benchmarks/benchmark_poolallocator.cpp
    foundation/meta/benchmarks/benchmark_qmc.cpp
    foundation/meta/benchmarks/benchmark_ray.cpp
    foundation/meta/benchmarks/benchmark_samesign.cpp
    foundation/meta/benchmarks/benchmark_sampling.cpp
    foundation/meta/benchmarks/benchmark_spectrum.cpp
)
list(APPEND appleseed_sources
    ${foundation_meta_benchmarks_sources}
)
source_group("foundation\\meta\\benchmarks" FILES
    ${foundation_meta_benchmarks_sources}
)

set(foundation_meta_templates_sources
    foundation/meta/templates/foundation_benchmark_template.cpp
    foundation/meta/templates/foundation_component_template.cpp
    foundation/meta/templates/foundation_component_template.h
    foundation/meta/templates/foundation_test_template.cpp
)
list(APPEND appleseed_sources
    ${foundation_meta_templates_sources}
)
source_group("foundation\\meta\\templates" FILES
    ${foundation_meta_templates_sources}
)
set_source_files_properties(
    ${foundation_meta_templates_sources}
    PROPERTIES HEADER_FILE_ONLY TRUE
)

set(foundation_meta_tests_sources
    foundation/meta/tests/test_aabb.cpp
    foundation/meta/tests/test_anyarray.cpp
    foundation/meta/tests/test_attributeset.cpp
    foundation/meta/tests/test_boost_regex.cpp
    foundation/meta/tests/test_bsp.cpp
    foundation/meta/tests/test_bufferedfile.cpp
    foundation/meta/tests/test_cache.cpp
    foundation/meta/tests/test_casts.cpp
    foundation/meta/tests/test_cdf.cpp
    foundation/meta/tests/test_color.cpp
    foundation/meta/tests/test_colorspace.cpp
    foundation/meta/tests/test_concepts.cpp
    foundation/meta/tests/test_countof.cpp
    foundation/meta/tests/test_dictionary.cpp
    foundation/meta/tests/test_dispatcher.cpp
    foundation/meta/tests/test_distance.cpp
    foundation/meta/tests/test_exrimagefilewriter.cpp
    foundation/meta/tests/test_filter.cpp
    foundation/meta/tests/test_fp.cpp
    foundation/meta/tests/test_fresnel.cpp
    foundation/meta/tests/test_image.cpp
    foundation/meta/tests/test_implptr.cpp
    foundation/meta/tests/test_intersection.cpp
    foundation/meta/tests/test_job.cpp
    foundation/meta/tests/test_kvpair.cpp
    foundation/meta/tests/test_lazy.cpp
    foundation/meta/tests/test_log.cpp
    foundation/meta/tests/test_makevector.cpp
    foundation/meta/tests/test_matrix.cpp
    foundation/meta/tests/test_memory.cpp
    foundation/meta/tests/test_minmax.cpp
    foundation/meta/tests/test_noise.cpp
    foundation/meta/tests/test_objmeshfilereader.cpp
    foundation/meta/tests/test_otherwise.cpp
    foundation/meta/tests/test_pagedarray.cpp
    foundation/meta/tests/test_path.cpp
    foundation/meta/tests/test_permutation.cpp
    foundation/meta/tests/test_poolallocator.cpp
    foundation/meta/tests/test_population.cpp
    foundation/meta/tests/test_qmc.cpp
    foundation/meta/tests/test_ray.cpp
    foundation/meta/tests/test_sampling.cpp
    foundation/meta/tests/test_scalar.cpp
    foundation/meta/tests/test_settings.cpp
    foundation/meta/tests/test_snprintf.cpp
    foundation/meta/tests/test_spectrum.cpp
    foundation/meta/tests/test_spline.cpp
    foundation/meta/tests/test_string.cpp
    foundation/meta/tests/test_test.cpp
    foundation/meta/tests/test_tile.cpp
    foundation/meta/tests/test_timer.cpp
    foundation/meta/tests/test_transform.cpp
    foundation/meta/tests/test_typetraits.cpp
    foundation/meta/tests/test_uid.cpp
    foundation/meta/tests/test_vector.cpp
)
list(APPEND appleseed_sources
    ${foundation_meta_tests_sources}
)
source_group("foundation\\meta\\tests" FILES
    ${foundation_meta_tests_sources}
)

set(foundation_platform_snprintf_sources
    foundation/platform/snprintf/snprintf.cpp
)
list(APPEND appleseed_sources
    ${foundation_platform_snprintf_sources}
)
source_group("foundation\\platform\\snprintf" FILES
    ${foundation_platform_snprintf_sources}
)

set(foundation_platform_sources
    foundation/platform/breakpoint.h
    foundation/platform/compiler.cpp
    foundation/platform/compiler.h
    foundation/platform/console.cpp
    foundation/platform/console.h
    foundation/platform/defaulttimers.cpp
    foundation/platform/defaulttimers.h
    foundation/platform/opengl.h
    foundation/platform/path.cpp
    foundation/platform/path.h
    foundation/platform/snprintf.h
    foundation/platform/sse.h
    foundation/platform/system.cpp
    foundation/platform/system.h
    foundation/platform/thread.cpp
    foundation/platform/thread.h
    foundation/platform/timer.h
    foundation/platform/types.h
    foundation/platform/windows.h
    foundation/platform/x86timer.cpp
    foundation/platform/x86timer.h
)
list(APPEND appleseed_sources
    ${foundation_platform_sources}
)
source_group("foundation\\platform" FILES
    ${foundation_platform_sources}
)

set(foundation_utility_benchmark_sources
    foundation/utility/benchmark/benchmarklistenerbase.h
    foundation/utility/benchmark/benchmarkresult.cpp
    foundation/utility/benchmark/benchmarkresult.h
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
list(APPEND appleseed_sources
    ${foundation_utility_benchmark_sources}
)
source_group("foundation\\utility\\benchmark" FILES
    ${foundation_utility_benchmark_sources}
)

set(foundation_utility_commandlineparser_sources
    foundation/utility/commandlineparser/flagoptionhandler.h
    foundation/utility/commandlineparser/messagelist.cpp
    foundation/utility/commandlineparser/messagelist.h
    foundation/utility/commandlineparser/optionhandler.h
    foundation/utility/commandlineparser/parser.h
    foundation/utility/commandlineparser/valueoptionhandler.h
)
list(APPEND appleseed_sources
    ${foundation_utility_commandlineparser_sources}
)
source_group("foundation\\utility\\commandlineparser" FILES
    ${foundation_utility_commandlineparser_sources}
)

set(foundation_utility_containers_sources
    foundation/utility/containers/anyarray.h
    foundation/utility/containers/array.h
    foundation/utility/containers/dictionary.cpp
    foundation/utility/containers/dictionary.h
    foundation/utility/containers/pagedarray.h
)
list(APPEND appleseed_sources
    ${foundation_utility_containers_sources}
)
source_group("foundation\\utility\\containers" FILES
    ${foundation_utility_containers_sources}
)

set(foundation_utility_filter_sources
    foundation/utility/filter/ifilter.h
    foundation/utility/filter/letthroughfilter.h
    foundation/utility/filter/regexfilter.cpp
    foundation/utility/filter/regexfilter.h
)
list(APPEND appleseed_sources
    ${foundation_utility_filter_sources}
)
source_group("foundation\\utility\\filter" FILES
    ${foundation_utility_filter_sources}
)

set(foundation_utility_job_sources
    foundation/utility/job/ijob.h
    foundation/utility/job/jobmanager.cpp
    foundation/utility/job/jobmanager.h
    foundation/utility/job/jobqueue.cpp
    foundation/utility/job/jobqueue.h
    foundation/utility/job/workerthread.cpp
    foundation/utility/job/workerthread.h
)
list(APPEND appleseed_sources
    ${foundation_utility_job_sources}
)
source_group("foundation\\utility\\job" FILES
    ${foundation_utility_job_sources}
)

set(foundation_utility_log_sources
    foundation/utility/log/consolelogtarget.cpp
    foundation/utility/log/consolelogtarget.h
    foundation/utility/log/filelogtarget.cpp
    foundation/utility/log/filelogtarget.h
    foundation/utility/log/filelogtargetbase.cpp
    foundation/utility/log/filelogtargetbase.h
    foundation/utility/log/helpers.h
    foundation/utility/log/logger.cpp
    foundation/utility/log/logger.h
    foundation/utility/log/logmessage.h
    foundation/utility/log/logtargetbase.h
    foundation/utility/log/openfilelogtarget.cpp
    foundation/utility/log/openfilelogtarget.h
)
list(APPEND appleseed_sources
    ${foundation_utility_log_sources}
)
source_group("foundation\\utility\\log" FILES
    ${foundation_utility_log_sources}
)

set(foundation_utility_settings_sources
    foundation/utility/settings/settingsfilereader.cpp
    foundation/utility/settings/settingsfilereader.h
    foundation/utility/settings/settingsfilewriter.cpp
    foundation/utility/settings/settingsfilewriter.h
)
list(APPEND appleseed_sources
    ${foundation_utility_settings_sources}
)
source_group("foundation\\utility\\settings" FILES
    ${foundation_utility_settings_sources}
)

set(foundation_utility_test_sources
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
list(APPEND appleseed_sources
    ${foundation_utility_test_sources}
)
source_group("foundation\\utility\\test" FILES
    ${foundation_utility_test_sources}
)

set(foundation_utility_sources
    foundation/utility/attributeset.cpp
    foundation/utility/attributeset.h
    foundation/utility/autoreleaseptr.h
    foundation/utility/benchmark.h
    foundation/utility/bufferedfile.cpp
    foundation/utility/bufferedfile.h
    foundation/utility/cache.h
    foundation/utility/cachemanager.h
    foundation/utility/cameracontroller.h
    foundation/utility/casts.h
    foundation/utility/commandlineparser.h
    foundation/utility/countof.h
    foundation/utility/dispatcher.h
    foundation/utility/filter.h
    foundation/utility/foreach.h
    foundation/utility/implptr.h
    foundation/utility/indenter.h
    foundation/utility/iostreamop.h
    foundation/utility/iterators.h
    foundation/utility/job.h
    foundation/utility/kvpair.h
    foundation/utility/lazy.h
    foundation/utility/log.h
    foundation/utility/makevector.h
    foundation/utility/maplefile.cpp
    foundation/utility/maplefile.h
    foundation/utility/memory.cpp
    foundation/utility/memory.h
    foundation/utility/numerictype.h
    foundation/utility/otherwise.h
    foundation/utility/poolallocator.h
    foundation/utility/searchpaths.cpp
    foundation/utility/searchpaths.h
    foundation/utility/serialization.h
    foundation/utility/settings.h
    foundation/utility/stopwatch.h
    foundation/utility/string.cpp
    foundation/utility/string.h
    foundation/utility/test.h
    foundation/utility/tls.h
    foundation/utility/typetraits.h
    foundation/utility/uid.cpp
    foundation/utility/uid.h
    foundation/utility/version.h
    foundation/utility/xercesc.cpp
    foundation/utility/xercesc.h
)
list(APPEND appleseed_sources
    ${foundation_utility_sources}
)
source_group("foundation\\utility" FILES
    ${foundation_utility_sources}
)

set(main_sources
    main/allocator.cpp
    main/dllmain.cpp
)
list(APPEND appleseed_sources
    ${main_sources}
)
source_group("main" FILES
    ${main_sources}
)

set(renderer_api_sources
    renderer/api/bsdf.h
    renderer/api/camera.h
    renderer/api/color.h
    renderer/api/edf.h
    renderer/api/environmentedf.h
    renderer/api/environmentshader.h
    renderer/api/frame.h
    renderer/api/geometry.h
    renderer/api/light.h
    renderer/api/lighting.h
    renderer/api/log.h
    renderer/api/material.h
    renderer/api/paramarray.h
    renderer/api/project.h
    renderer/api/rendering.h
    renderer/api/scene.h
    renderer/api/surfaceshader.h
    renderer/api/texture.h
    renderer/api/tracecontext.h
    renderer/api/utility.h
)
list(APPEND appleseed_sources
    ${renderer_api_sources}
)
source_group("renderer\\api" FILES
    ${renderer_api_sources}
)

set(renderer_global_sources
    renderer/global/global.h
    renderer/global/globallogger.cpp
    renderer/global/globallogger.h
    renderer/global/globaltypes.h
    renderer/global/globaluidsource.cpp
    renderer/global/globaluidsource.h
    renderer/global/paramarray.cpp
    renderer/global/paramarray.h
    renderer/global/utility.h
)
list(APPEND appleseed_sources
    ${renderer_global_sources}
)
source_group("renderer\\global" FILES
    ${renderer_global_sources}
)

set(renderer_kernel_intersection_sources
    renderer/kernel/intersection/assemblytree.cpp
    renderer/kernel/intersection/assemblytree.h
    renderer/kernel/intersection/intersectionsettings.h
    renderer/kernel/intersection/intersector.cpp
    renderer/kernel/intersection/intersector.h
    renderer/kernel/intersection/probevisitorbase.h
    renderer/kernel/intersection/regioninfo.h
    renderer/kernel/intersection/regiontree.cpp
    renderer/kernel/intersection/regiontree.h
    renderer/kernel/intersection/tracecontext.cpp
    renderer/kernel/intersection/tracecontext.h
    renderer/kernel/intersection/triangleinfo.h
    renderer/kernel/intersection/triangletree.cpp
    renderer/kernel/intersection/triangletree.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_intersection_sources}
)
source_group("renderer\\kernel\\intersection" FILES
    ${renderer_kernel_intersection_sources}
)

set(renderer_kernel_lighting_drt_sources
    renderer/kernel/lighting/drt/drt.cpp
    renderer/kernel/lighting/drt/drt.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_lighting_drt_sources}
)
source_group("renderer\\kernel\\lighting\\drt" FILES
    ${renderer_kernel_lighting_drt_sources}
)

set(renderer_kernel_lighting_null_sources
    renderer/kernel/lighting/null/null.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_lighting_null_sources}
)
source_group("renderer\\kernel\\lighting\\null" FILES
    ${renderer_kernel_lighting_null_sources}
)

set(renderer_kernel_lighting_pathtracing_sources
    renderer/kernel/lighting/pathtracing/pathtracing.cpp
    renderer/kernel/lighting/pathtracing/pathtracing.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_lighting_pathtracing_sources}
)
source_group("renderer\\kernel\\lighting\\pathtracing" FILES
    ${renderer_kernel_lighting_pathtracing_sources}
)

set(renderer_kernel_lighting_sources
    renderer/kernel/lighting/directlighting.cpp
    renderer/kernel/lighting/directlighting.h
    renderer/kernel/lighting/ilightingengine.h
    renderer/kernel/lighting/imagebasedlighting.cpp
    renderer/kernel/lighting/imagebasedlighting.h
    renderer/kernel/lighting/imageimportancesampler.h
    renderer/kernel/lighting/lightsampler.cpp
    renderer/kernel/lighting/lightsampler.h
    renderer/kernel/lighting/transmission.cpp
    renderer/kernel/lighting/transmission.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_lighting_sources}
)
source_group("renderer\\kernel\\lighting" FILES
    ${renderer_kernel_lighting_sources}
)

set(renderer_kernel_rendering_debug_sources
    renderer/kernel/rendering/debug/blanktilerenderer.cpp
    renderer/kernel/rendering/debug/blanktilerenderer.h
    renderer/kernel/rendering/debug/debugtilerenderer.cpp
    renderer/kernel/rendering/debug/debugtilerenderer.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_rendering_debug_sources}
)
source_group("renderer\\kernel\\rendering\\debug" FILES
    ${renderer_kernel_rendering_debug_sources}
)

set(renderer_kernel_rendering_generic_sources
    renderer/kernel/rendering/generic/genericframerenderer.cpp
    renderer/kernel/rendering/generic/genericframerenderer.h
    renderer/kernel/rendering/generic/genericsamplerenderer.cpp
    renderer/kernel/rendering/generic/genericsamplerenderer.h
    renderer/kernel/rendering/generic/generictilerenderer.cpp
    renderer/kernel/rendering/generic/generictilerenderer.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_rendering_generic_sources}
)
source_group("renderer\\kernel\\rendering\\generic" FILES
    ${renderer_kernel_rendering_generic_sources}
)

set(renderer_kernel_rendering_progressive_sources
    renderer/kernel/rendering/progressive/accumulationframebuffer.cpp
    renderer/kernel/rendering/progressive/accumulationframebuffer.h
    renderer/kernel/rendering/progressive/progressiveframebuffer.cpp
    renderer/kernel/rendering/progressive/progressiveframebuffer.h
    renderer/kernel/rendering/progressive/progressiveframerenderer.cpp
    renderer/kernel/rendering/progressive/progressiveframerenderer.h
    renderer/kernel/rendering/progressive/sample.h
    renderer/kernel/rendering/progressive/samplegenerator.cpp
    renderer/kernel/rendering/progressive/samplegenerator.h
    renderer/kernel/rendering/progressive/samplegeneratorjob.cpp
    renderer/kernel/rendering/progressive/samplegeneratorjob.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_rendering_progressive_sources}
)
source_group("renderer\\kernel\\rendering\\progressive" FILES
    ${renderer_kernel_rendering_progressive_sources}
)

set(renderer_kernel_rendering_sources
    renderer/kernel/rendering/defaultrenderercontroller.cpp
    renderer/kernel/rendering/defaultrenderercontroller.h
    renderer/kernel/rendering/iframerenderer.h
    renderer/kernel/rendering/irenderercontroller.h
    renderer/kernel/rendering/isamplerenderer.h
    renderer/kernel/rendering/itilecallback.h
    renderer/kernel/rendering/itilerenderer.h
    renderer/kernel/rendering/masterrenderer.cpp
    renderer/kernel/rendering/masterrenderer.h
    renderer/kernel/rendering/tilecallbackbase.cpp
    renderer/kernel/rendering/tilecallbackbase.h
    renderer/kernel/rendering/tilejob.cpp
    renderer/kernel/rendering/tilejob.h
    renderer/kernel/rendering/tilejobfactory.cpp
    renderer/kernel/rendering/tilejobfactory.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_rendering_sources}
)
source_group("renderer\\kernel\\rendering" FILES
    ${renderer_kernel_rendering_sources}
)

set(renderer_kernel_shading_sources
    renderer/kernel/shading/ambientocclusion.cpp
    renderer/kernel/shading/ambientocclusion.h
    renderer/kernel/shading/fastambientocclusion.cpp
    renderer/kernel/shading/fastambientocclusion.h
    renderer/kernel/shading/shadingcontext.h
    renderer/kernel/shading/shadingengine.cpp
    renderer/kernel/shading/shadingengine.h
    renderer/kernel/shading/shadingpoint.cpp
    renderer/kernel/shading/shadingpoint.h
    renderer/kernel/shading/shadingpointbuilder.cpp
    renderer/kernel/shading/shadingpointbuilder.h
    renderer/kernel/shading/shadingray.h
    renderer/kernel/shading/shadingresult.cpp
    renderer/kernel/shading/shadingresult.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_shading_sources}
)
source_group("renderer\\kernel\\shading" FILES
    ${renderer_kernel_shading_sources}
)

set(renderer_kernel_tessellation_sources
    renderer/kernel/tessellation/statictessellation.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_tessellation_sources}
)
source_group("renderer\\kernel\\tessellation" FILES
    ${renderer_kernel_tessellation_sources}
)

set(renderer_kernel_texturing_sources
    renderer/kernel/texturing/texturecache.cpp
    renderer/kernel/texturing/texturecache.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_texturing_sources}
)
source_group("renderer\\kernel\\texturing" FILES
    ${renderer_kernel_texturing_sources}
)

set(renderer_kernel_volume_sources
    renderer/kernel/volume/volume.cpp
    renderer/kernel/volume/volume.h
)
list(APPEND appleseed_sources
    ${renderer_kernel_volume_sources}
)
source_group("renderer\\kernel\\volume" FILES
    ${renderer_kernel_volume_sources}
)

set(renderer_meta_templates_sources
    renderer/meta/templates/renderer_component_template.cpp
    renderer/meta/templates/renderer_component_template.h
    renderer/meta/templates/renderer_test_template.cpp
)
list(APPEND appleseed_sources
    ${renderer_meta_templates_sources}
)
source_group("renderer\\meta\\templates" FILES
    ${renderer_meta_templates_sources}
)
set_source_files_properties(
    ${renderer_meta_templates_sources}
    PROPERTIES HEADER_FILE_ONLY TRUE
)

set(renderer_meta_tests_sources
    renderer/meta/tests/test_entitymap.cpp
    renderer/meta/tests/test_entityvector.cpp
    renderer/meta/tests/test_environmentedf.cpp
    renderer/meta/tests/test_imagebasedlighting.cpp
    renderer/meta/tests/test_imageimportancesampler.cpp
    renderer/meta/tests/test_inputarray.cpp
    renderer/meta/tests/test_intersector.cpp
    renderer/meta/tests/test_lightsampler.cpp
    renderer/meta/tests/test_paramarray.cpp
)
list(APPEND appleseed_sources
    ${renderer_meta_tests_sources}
)
source_group("renderer\\meta\\tests" FILES
    ${renderer_meta_tests_sources}
)

set(renderer_modeling_bsdf_sources
    renderer/modeling/bsdf/ashikhminbrdf.cpp
    renderer/modeling/bsdf/ashikhminbrdf.h
    renderer/modeling/bsdf/brdfwrapper.h
    renderer/modeling/bsdf/bsdf.cpp
    renderer/modeling/bsdf/bsdf.h
    renderer/modeling/bsdf/bsdffactorydispatcher.cpp
    renderer/modeling/bsdf/bsdffactorydispatcher.h
    renderer/modeling/bsdf/btdfwrapper.h
    renderer/modeling/bsdf/lambertianbrdf.cpp
    renderer/modeling/bsdf/lambertianbrdf.h
    renderer/modeling/bsdf/lambertianbtdf.h
    renderer/modeling/bsdf/nullbsdf.h
    renderer/modeling/bsdf/phongbrdf.cpp
    renderer/modeling/bsdf/phongbrdf.h
    renderer/modeling/bsdf/phongbtdf.h
    renderer/modeling/bsdf/specularbrdf.cpp
    renderer/modeling/bsdf/specularbrdf.h
    renderer/modeling/bsdf/specularbtdf.cpp
    renderer/modeling/bsdf/specularbtdf.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_bsdf_sources}
)
source_group("renderer\\modeling\\bsdf" FILES
    ${renderer_modeling_bsdf_sources}
)

set(renderer_modeling_camera_sources
    renderer/modeling/camera/camera.cpp
    renderer/modeling/camera/camera.h
    renderer/modeling/camera/camerafactorydispatcher.cpp
    renderer/modeling/camera/camerafactorydispatcher.h
    renderer/modeling/camera/pinholecamera.cpp
    renderer/modeling/camera/pinholecamera.h
    renderer/modeling/camera/thinlenscamera.cpp
    renderer/modeling/camera/thinlenscamera.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_camera_sources}
)
source_group("renderer\\modeling\\camera" FILES
    ${renderer_modeling_camera_sources}
)

set(renderer_modeling_color_sources
    renderer/modeling/color/colorentity.cpp
    renderer/modeling/color/colorentity.h
    renderer/modeling/color/wavelengths.cpp
    renderer/modeling/color/wavelengths.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_color_sources}
)
source_group("renderer\\modeling\\color" FILES
    ${renderer_modeling_color_sources}
)

set(renderer_modeling_edf_sources
    renderer/modeling/edf/diffuseedf.cpp
    renderer/modeling/edf/diffuseedf.h
    renderer/modeling/edf/edf.h
    renderer/modeling/edf/edffactorydispatcher.cpp
    renderer/modeling/edf/edffactorydispatcher.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_edf_sources}
)
source_group("renderer\\modeling\\edf" FILES
    ${renderer_modeling_edf_sources}
)

set(renderer_modeling_entity_sources
    renderer/modeling/entity/connectableentity.h
    renderer/modeling/entity/entity.h
    renderer/modeling/entity/entitymap.cpp
    renderer/modeling/entity/entitymap.h
    renderer/modeling/entity/entityvector.cpp
    renderer/modeling/entity/entityvector.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_entity_sources}
)
source_group("renderer\\modeling\\entity" FILES
    ${renderer_modeling_entity_sources}
)

set(renderer_modeling_environment_sources
    renderer/modeling/environment/environment.cpp
    renderer/modeling/environment/environment.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_environment_sources}
)
source_group("renderer\\modeling\\environment" FILES
    ${renderer_modeling_environment_sources}
)

set(renderer_modeling_environmentedf_sources
    renderer/modeling/environmentedf/constantenvironmentedf.cpp
    renderer/modeling/environmentedf/constantenvironmentedf.h
    renderer/modeling/environmentedf/environmentedf.h
    renderer/modeling/environmentedf/environmentedffactorydispatcher.cpp
    renderer/modeling/environmentedf/environmentedffactorydispatcher.h
    renderer/modeling/environmentedf/gradientenvironmentedf.cpp
    renderer/modeling/environmentedf/gradientenvironmentedf.h
    renderer/modeling/environmentedf/latlongmapenvironmentedf.cpp
    renderer/modeling/environmentedf/latlongmapenvironmentedf.h
    renderer/modeling/environmentedf/mirrorballmapenvironmentedf.cpp
    renderer/modeling/environmentedf/mirrorballmapenvironmentedf.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_environmentedf_sources}
)
source_group("renderer\\modeling\\environmentedf" FILES
    ${renderer_modeling_environmentedf_sources}
)

set(renderer_modeling_environmentshader_sources
    renderer/modeling/environmentshader/edfenvironmentshader.cpp
    renderer/modeling/environmentshader/edfenvironmentshader.h
    renderer/modeling/environmentshader/environmentshader.h
    renderer/modeling/environmentshader/environmentshaderfactorydispatcher.cpp
    renderer/modeling/environmentshader/environmentshaderfactorydispatcher.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_environmentshader_sources}
)
source_group("renderer\\modeling\\environmentshader" FILES
    ${renderer_modeling_environmentshader_sources}
)

set(renderer_modeling_frame_sources
    renderer/modeling/frame/frame.cpp
    renderer/modeling/frame/frame.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_frame_sources}
)
source_group("renderer\\modeling\\frame" FILES
    ${renderer_modeling_frame_sources}
)

set(renderer_modeling_geometry_sources
    renderer/modeling/geometry/iregion.h
    renderer/modeling/geometry/meshobject.cpp
    renderer/modeling/geometry/meshobject.h
    renderer/modeling/geometry/meshobjectreader.cpp
    renderer/modeling/geometry/meshobjectreader.h
    renderer/modeling/geometry/meshobjectwriter.cpp
    renderer/modeling/geometry/meshobjectwriter.h
    renderer/modeling/geometry/object.h
    renderer/modeling/geometry/regionkit.h
    renderer/modeling/geometry/triangle.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_geometry_sources}
)
source_group("renderer\\modeling\\geometry" FILES
    ${renderer_modeling_geometry_sources}
)

set(renderer_modeling_input_sources
    renderer/modeling/input/colorsource.cpp
    renderer/modeling/input/colorsource.h
    renderer/modeling/input/inputarray.cpp
    renderer/modeling/input/inputarray.h
    renderer/modeling/input/inputevaluator.h
    renderer/modeling/input/inputparams.h
    renderer/modeling/input/scalarsource.h
    renderer/modeling/input/source.h
    renderer/modeling/input/texturesource.cpp
    renderer/modeling/input/texturesource.h
    renderer/modeling/input/uniforminputevaluator.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_input_sources}
)
source_group("renderer\\modeling\\input" FILES
    ${renderer_modeling_input_sources}
)

set(renderer_modeling_light_sources
    renderer/modeling/light/light.cpp
    renderer/modeling/light/light.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_light_sources}
)
source_group("renderer\\modeling\\light" FILES
    ${renderer_modeling_light_sources}
)

set(renderer_modeling_material_sources
    renderer/modeling/material/material.cpp
    renderer/modeling/material/material.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_material_sources}
)
source_group("renderer\\modeling\\material" FILES
    ${renderer_modeling_material_sources}
)

set(renderer_modeling_project_sources
    renderer/modeling/project/configuration.cpp
    renderer/modeling/project/configuration.h
    renderer/modeling/project/configurationcontainer.h
    renderer/modeling/project/eventcounters.h
    renderer/modeling/project/inputbinder.cpp
    renderer/modeling/project/inputbinder.h
    renderer/modeling/project/project.cpp
    renderer/modeling/project/project.h
    renderer/modeling/project/projectfilereader.cpp
    renderer/modeling/project/projectfilereader.h
    renderer/modeling/project/projectfilewriter.cpp
    renderer/modeling/project/projectfilewriter.h
    renderer/modeling/project/symbol.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_project_sources}
)
source_group("renderer\\modeling\\project" FILES
    ${renderer_modeling_project_sources}
)

set(renderer_modeling_project-builtin_sources
    renderer/modeling/project-builtin/cornellboxproject.cpp
    renderer/modeling/project-builtin/cornellboxproject.h
    renderer/modeling/project-builtin/defaultproject.cpp
    renderer/modeling/project-builtin/defaultproject.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_project-builtin_sources}
)
source_group("renderer\\modeling\\project-builtin" FILES
    ${renderer_modeling_project-builtin_sources}
)

set(renderer_modeling_scene_sources
    renderer/modeling/scene/assembly.cpp
    renderer/modeling/scene/assembly.h
    renderer/modeling/scene/assemblyinstance.cpp
    renderer/modeling/scene/assemblyinstance.h
    renderer/modeling/scene/containers.h
    renderer/modeling/scene/objectinstance.cpp
    renderer/modeling/scene/objectinstance.h
    renderer/modeling/scene/scene.cpp
    renderer/modeling/scene/scene.h
    renderer/modeling/scene/textureinstance.cpp
    renderer/modeling/scene/textureinstance.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_scene_sources}
)
source_group("renderer\\modeling\\scene" FILES
    ${renderer_modeling_scene_sources}
)

set(renderer_modeling_surfaceshader_sources
    renderer/modeling/surfaceshader/aosurfaceshader.cpp
    renderer/modeling/surfaceshader/aosurfaceshader.h
    renderer/modeling/surfaceshader/constantsurfaceshader.cpp
    renderer/modeling/surfaceshader/constantsurfaceshader.h
    renderer/modeling/surfaceshader/diagnosticsurfaceshader.cpp
    renderer/modeling/surfaceshader/diagnosticsurfaceshader.h
    renderer/modeling/surfaceshader/physicalsurfaceshader.cpp
    renderer/modeling/surfaceshader/physicalsurfaceshader.h
    renderer/modeling/surfaceshader/smokesurfaceshader.cpp
    renderer/modeling/surfaceshader/smokesurfaceshader.h
    renderer/modeling/surfaceshader/surfaceshader.h
    renderer/modeling/surfaceshader/surfaceshaderfactorydispatcher.cpp
    renderer/modeling/surfaceshader/surfaceshaderfactorydispatcher.h
    renderer/modeling/surfaceshader/voxelaosurfaceshader.cpp
    renderer/modeling/surfaceshader/voxelaosurfaceshader.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_surfaceshader_sources}
)
source_group("renderer\\modeling\\surfaceshader" FILES
    ${renderer_modeling_surfaceshader_sources}
)

set(renderer_modeling_texture_sources
    renderer/modeling/texture/disktexture.cpp
    renderer/modeling/texture/disktexture.h
    renderer/modeling/texture/texture.h
    renderer/modeling/texture/texturefactorydispatcher.cpp
    renderer/modeling/texture/texturefactorydispatcher.h
    renderer/modeling/texture/writabletexture.cpp
    renderer/modeling/texture/writabletexture.h
)
list(APPEND appleseed_sources
    ${renderer_modeling_texture_sources}
)
source_group("renderer\\modeling\\texture" FILES
    ${renderer_modeling_texture_sources}
)


#
# Target.
#

add_library(appleseed SHARED
    ${appleseed_sources}
)


#
# Preprocessor Definitions.
#

apply_preprocessor_definitions(appleseed)

append_custom_preprocessor_definitions(appleseed
    APPLESEED_FOUNDATION_EXPORTS
    APPLESEED_RENDERER_EXPORTS
    APPLESEED_FOUNDATION_USE_SSE
)


#
# Static Libraries.
#

link_against_common_static_libraries(appleseed)


#
# Post-Build Command.
#

add_copy_target_to_sandbox_command(appleseed)
