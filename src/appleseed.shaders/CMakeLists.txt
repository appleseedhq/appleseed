
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2016-2017 Esteban Tovagliari, The appleseedhq Organization
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
# Auto-generate OSL extensions header file.
#--------------------------------------------------------------------------------------------------

configure_file (
    ${PROJECT_SOURCE_DIR}/src/appleseed.shaders/as_osl_extensions.h.in
    ${PROJECT_SOURCE_DIR}/src/appleseed.shaders/as_osl_extensions.h
)


#--------------------------------------------------------------------------------------------------
# Header files.
#--------------------------------------------------------------------------------------------------

set (standard_osl_headers
    as_osl_extensions.h
    oslutil.h
    stdosl.h
)
list (APPEND osl_headers
    ${standard_osl_headers}
)
source_group ("" FILES
    ${standard_osl_headers}
)

set (include_appleseed_gaffer_sources
    include/appleseed/gaffer/color.h
    include/appleseed/gaffer/transform.h
    include/appleseed/gaffer/udim.h
)
list (APPEND osl_headers
    ${include_appleseed_gaffer_sources}
)
source_group ("include\\appleseed\\gaffer" FILES
    ${include_appleseed_gaffer_sources}
)

set (include_appleseed_color_sources
    include/appleseed/color/as_chromatic_adaptation.h
    include/appleseed/color/as_color_helpers.h
    include/appleseed/color/as_color_transforms.h
    include/appleseed/color/as_colorimetry.h
    include/appleseed/color/as_transfer_functions.h
)
list (APPEND osl_headers
    ${include_appleseed_color_sources}
)
source_group ("include\\appleseed\\color" FILES
    ${include_appleseed_color_sources}
)

set (include_appleseed_fractal_sources
    include/appleseed/fractal/as_fractal_helpers.h
    include/appleseed/fractal/as_noise_helpers.h
    include/appleseed/fractal/as_noise_tables.h
)
list (APPEND osl_headers
    ${include_appleseed_fractal_sources}
)
source_group ("include\\appleseed\\fractal" FILES
    ${include_appleseed_fractal_sources}
)

set (include_appleseed_material_sources
    include/appleseed/material/as_material_helpers.h
)
list (APPEND osl_headers
    ${include_appleseed_material_sources}
)
source_group ("include\\appleseed\\material" FILES
    ${include_appleseed_material_sources}
)

set (include_appleseed_math_sources
    include/appleseed/math/as_math_complex.h
    include/appleseed/math/as_math_helpers.h
)
list (APPEND osl_headers
    ${include_appleseed_math_sources}
)
source_group ("include\\appleseed\\math" FILES
    ${include_appleseed_math_sources}
)

set (include_appleseed_maya_sources
    include/appleseed/maya/as_maya_cms_syncolor_idt.h
    include/appleseed/maya/as_maya_fractal_helpers.h
    include/appleseed/maya/as_maya_helpers.h
    include/appleseed/maya/as_maya_layer_helpers.h
    include/appleseed/maya/as_maya_pattern_helpers.h
    include/appleseed/maya/as_maya_ramp_helpers.h
    include/appleseed/maya/as_maya_transform_helpers.h
)
list (APPEND osl_headers
    ${include_appleseed_maya_sources}
)
source_group ("include\\appleseed\\maya" FILES
    ${include_appleseed_maya_sources}
)

set (include_appleseed_pattern_sources
    include/appleseed/pattern/as_pattern_helpers.h
)
list (APPEND osl_headers
    ${include_appleseed_pattern_sources}
)
source_group ("include\\appleseed\\pattern" FILES
    ${include_appleseed_pattern_sources}
)

set (include_alshaders_sources
    include/alshaders/remap.h
    include/alshaders/space.h
)
list (APPEND osl_headers
    ${include_alshaders_sources}
)
source_group ("include\\alshaders" FILES
    ${include_alshaders_sources}
)


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

set (src_gaffer_color_sources
     src/gaffer/color/as_blackbody.osl
     src/gaffer/color/as_color_add.osl
     src/gaffer/color/as_color_build.osl
     src/gaffer/color/as_color_mix.osl
     src/gaffer/color/as_color_multiply.osl
     src/gaffer/color/as_color_saturation.osl
     src/gaffer/color/as_color_split.osl
     src/gaffer/color/as_luminance.osl
     src/gaffer/color/as_wavelength.osl
)
list (APPEND osl_sources
    ${src_gaffer_color_sources}
)
source_group ("src\\gaffer\\color" FILES
    ${src_gaffer_color_sources}
)

set (src_gaffer_float_sources
     src/gaffer/float/as_float_add.osl
     src/gaffer/float/as_float_cast.osl
     src/gaffer/float/as_float_mix.osl
     src/gaffer/float/as_float_multiply.osl
     src/gaffer/float/as_float_pow.osl
     src/gaffer/float/as_float_remap.osl
)
list (APPEND osl_sources
    ${src_gaffer_float_sources}
)
source_group ("src\\gaffer\\float" FILES
    ${src_gaffer_float_sources}
)

set (src_gaffer_input_sources
     src/gaffer/input/as_globals.osl
     src/gaffer/input/as_input.osl
     src/gaffer/input/as_object_input.osl
     src/gaffer/input/as_ray_info.osl
)
list (APPEND osl_sources
    ${src_gaffer_input_sources}
)
source_group ("src\\gaffer\\input" FILES
    ${src_gaffer_input_sources}
)

set (src_gaffer_material_sources
     src/gaffer/material/as_disney_material.osl
     src/gaffer/material/as_material_builder.osl
)
list (APPEND osl_sources
    ${src_gaffer_material_sources}
)
source_group ("src\\gaffer\\material" FILES
    ${src_gaffer_material_sources}
)

set (src_gaffer_normal_sources
     src/gaffer/normal/as_bump_map.osl
     src/gaffer/normal/as_normal_map.osl
)
list (APPEND osl_sources
    ${src_gaffer_normal_sources}
)
source_group ("src\\gaffer\\normal" FILES
    ${src_gaffer_normal_sources}
)

set (src_gaffer_surface_sources
     src/gaffer/surface/as_diffuse_surface.osl
     src/gaffer/surface/as_emission_surface.osl
     src/gaffer/surface/as_glass_surface.osl
     src/gaffer/surface/as_glossy_surface.osl
     src/gaffer/surface/as_metal_surface.osl
     src/gaffer/surface/as_sheen_surface.osl
     src/gaffer/surface/as_subsurface_surface.osl
     src/gaffer/surface/as_surface_add.osl
     src/gaffer/surface/as_surface_mix.osl
     src/gaffer/surface/as_surface_multiply.osl
     src/gaffer/surface/as_surface_switch.osl
     src/gaffer/surface/as_translucent_surface.osl
     src/gaffer/surface/as_transparency_surface.osl
)
list (APPEND osl_sources
    ${src_gaffer_surface_sources}
)
source_group ("src\\gaffer\\surface" FILES
    ${src_gaffer_surface_sources}
)

set (src_gaffer_texture2d_sources
     src/gaffer/texture2d/as_color_texture.osl
     src/gaffer/texture2d/as_noise2d.osl
     src/gaffer/texture2d/as_scalar_texture.osl
)
list (APPEND osl_sources
    ${src_gaffer_texture2d_sources}
)
source_group ("src\\gaffer\\texture2d" FILES
    ${src_gaffer_texture2d_sources}
)

set (src_gaffer_texture3d_sources
     src/gaffer/texture3d/as_noise3d.osl
     src/gaffer/texture3d/as_noise4d.osl
)
list (APPEND osl_sources
    ${src_gaffer_texture3d_sources}
)
source_group ("src\\gaffer\\texture3d" FILES
    ${src_gaffer_texture3d_sources}
)

set (src_gaffer_transform_sources
     src/gaffer/transform/as_map2d.osl
     src/gaffer/transform/as_map3d.osl
     src/gaffer/transform/as_transform.osl
)
list (APPEND osl_sources
    ${src_gaffer_transform_sources}
)
source_group ("src\\gaffer\\transform" FILES
    ${src_gaffer_transform_sources}
)

set (src_gaffer_utility_sources
     src/gaffer/utility/as_condition.osl
     src/gaffer/utility/as_float2_build.osl
     src/gaffer/utility/as_float2_split.osl
     src/gaffer/utility/as_switch.osl
)
list (APPEND osl_sources
    ${src_gaffer_utility_sources}
)
source_group ("src\\gaffer\\utility" FILES
    ${src_gaffer_utility_sources}
)

set (src_gaffer_vector_sources
     src/gaffer/vector/as_anisotropy_dir.osl
     src/gaffer/vector/as_vector_add.osl
     src/gaffer/vector/as_vector_build.osl
     src/gaffer/vector/as_vector_cross.osl
     src/gaffer/vector/as_vector_dot.osl
     src/gaffer/vector/as_vector_mix.osl
     src/gaffer/vector/as_vector_multiply.osl
     src/gaffer/vector/as_vector_normalize.osl
     src/gaffer/vector/as_vector_split.osl
)
list (APPEND osl_sources
    ${src_gaffer_vector_sources}
)
source_group ("src\\gaffer\\vector" FILES
    ${src_gaffer_vector_sources}
)

set (src_maya_sources
     src/maya/as_maya_addDoubleLinear.osl
     src/maya/as_maya_addMatrix.osl
     src/maya/as_maya_anisotropic.osl
     src/maya/as_maya_areaLight.osl
     src/maya/as_maya_blendColors.osl
     src/maya/as_maya_blinn.osl
     src/maya/as_maya_bulge.osl
     src/maya/as_maya_bump2d.osl
     src/maya/as_maya_bump3d.osl
     src/maya/as_maya_checker.osl
     src/maya/as_maya_clamp.osl
     src/maya/as_maya_clearCoat.osl
     src/maya/as_maya_closure2Surface.osl
     src/maya/as_maya_cloth.osl
     src/maya/as_maya_cloud.osl
     src/maya/as_maya_color2Components.osl
     src/maya/as_maya_components2Color.osl
     src/maya/as_maya_components2UV.osl
     src/maya/as_maya_components2Vector.osl
     src/maya/as_maya_condition.osl
     src/maya/as_maya_contrast.osl
     src/maya/as_maya_distanceBetween.osl
     src/maya/as_maya_doubleShadingSwitch.osl
     src/maya/as_maya_envChrome.osl
     src/maya/as_maya_file.osl
     src/maya/as_maya_fourByFourMatrix.osl
     src/maya/as_maya_fractal.osl
     src/maya/as_maya_gammaCorrect.osl
     src/maya/as_maya_grid.osl
     src/maya/as_maya_hsvToRgb.osl
     src/maya/as_maya_lambert.osl
     src/maya/as_maya_layeredTexture.osl
     src/maya/as_maya_luminance.osl
     src/maya/as_maya_mandelbrot.osl
     src/maya/as_maya_matrixInverse.osl
     src/maya/as_maya_mountain.osl
     src/maya/as_maya_movie.osl
     src/maya/as_maya_multDoubleLinear.osl
     src/maya/as_maya_multiplyDivide.osl
     src/maya/as_maya_multMatrix.osl
     src/maya/as_maya_noise.osl
     src/maya/as_maya_phong.osl
     src/maya/as_maya_phongE.osl
     src/maya/as_maya_place2dTexture.osl
     src/maya/as_maya_place3dTexture.osl
     src/maya/as_maya_plusMinusAverage.osl
     src/maya/as_maya_psdFileTex.osl
     src/maya/as_maya_quadShadingSwitch.osl
     src/maya/as_maya_ramp.osl
     src/maya/as_maya_remapColor.osl
     src/maya/as_maya_remapHsv.osl
     src/maya/as_maya_remapValue.osl
     src/maya/as_maya_reverse.osl
     src/maya/as_maya_rgbToHsv.osl
     src/maya/as_maya_samplerInfo.osl
     src/maya/as_maya_setRange.osl
     src/maya/as_maya_singleShadingSwitch.osl
     src/maya/as_maya_stencil.osl
     src/maya/as_maya_transposeMatrix.osl
     src/maya/as_maya_tripleShadingSwitch.osl
     src/maya/as_maya_uv2Components.osl
     src/maya/as_maya_vector2Components.osl
     src/maya/as_maya_vectorProduct.osl
     src/maya/as_maya_water.osl
     src/maya/as_maya_wtAddMatrix.osl
)
list (APPEND osl_sources
    ${src_maya_sources}
)
source_group ("src\\maya" FILES
    ${src_maya_sources}
)

set (src_alshaders_sources
    #src/alshaders/alBlackbody.osl
    #src/alshaders/alCellNoise.osl
    src/alshaders/alColorSpace.osl
    src/alshaders/alCombineColor.osl
    src/alshaders/alCombineFloat.osl
    #src/alshaders/alCurvature.osl
    #src/alshaders/alFlake.osl
    src/alshaders/alFlowNoise.osl
    src/alshaders/alFractal.osl
    src/alshaders/alJitterColor.osl
    src/alshaders/alLayerColor.osl
    src/alshaders/alLayerFloat.osl
    src/alshaders/alLayer.osl
    src/alshaders/alPattern.osl
    src/alshaders/alRemapColor.osl
    src/alshaders/alRemapFloat.osl
    src/alshaders/alSurface.osl
    src/alshaders/alSwitchColor.osl
    src/alshaders/alSwitchFloat.osl
    #src/alshaders/alTriplanar.osl
)
list (APPEND osl_sources
    ${src_alshaders_sources}
)
source_group ("src\\alshaders" FILES
    ${src_alshaders_sources}
)

set (src_appleseed_sources
     src/appleseed/asDisneyMaterial.osl
     src/appleseed/asGlass.osl
     src/appleseed/asVoronoi2D.osl
     src/appleseed/asVoronoi3D.osl
)
list (APPEND osl_sources
    ${src_appleseed_sources}
)
source_group ("src\\appleseed" FILES
    ${src_appleseed_sources}
)


#--------------------------------------------------------------------------------------------------
# Prepare the sandbox.
#--------------------------------------------------------------------------------------------------

add_custom_target (SANDBOX)

# Copy the OSL compiler to the sandbox.
if (USE_EXTERNAL_OSL)
    add_custom_command (
        TARGET SANDBOX
        COMMAND ${CMAKE_COMMAND} -E copy ${OSL_COMPILER} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
else ()
    add_custom_command (
        TARGET SANDBOX
        COMMAND ${CMAKE_COMMAND} -E copy ${APPLESEED_DEPS_STAGE_DIR}/osl-release/bin/oslc${exe_ext} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Compile shaders and deploy header files to sandbox.
#--------------------------------------------------------------------------------------------------

# Build lists of absolute paths to OSL header files, in the sources and in the sandbox.
foreach (osl_header ${osl_headers})
    list (APPEND abs_osl_headers_sources ${PROJECT_SOURCE_DIR}/src/appleseed.shaders/${osl_header})
    list (APPEND abs_osl_headers_sandbox ${PROJECT_SOURCE_DIR}/sandbox/shaders/${osl_header})
endforeach ()

# Compile OSL shaders into the sandbox.
foreach (osl_source ${osl_sources})
    # Build the .oso destination file name.
    get_filename_component (osl_source_we ${osl_source} NAME_WE)
    set (oso_filename ${osl_source_we}.oso)

    # Build the sandbox destination directory.
    get_filename_component (osl_dir ${osl_source} PATH)
    string (
        REGEX REPLACE
        "src/" "${PROJECT_SOURCE_DIR}/sandbox/shaders/"
        oso_dir ${osl_dir}
    )

    if (CMAKE_BUILD_TYPE STREQUAL "Debug")
        set (OSL_OPTIONS -O0 -DDEBUG)
    else ()
        set (OSL_OPTIONS -O2 -DNDEBUG)
    endif ()

    add_custom_command (
        OUTPUT ${oso_dir}/${oso_filename}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${oso_dir}
        COMMAND ${PROJECT_SOURCE_DIR}/sandbox/bin/oslc ${OSL_OPTIONS}
            -I"${PROJECT_SOURCE_DIR}/src/appleseed.shaders/include"
            -o "${oso_dir}/${oso_filename}" "${PROJECT_SOURCE_DIR}/src/appleseed.shaders/${osl_source}"
        MAIN_DEPENDENCY ${PROJECT_SOURCE_DIR}/src/appleseed.shaders/${osl_source}
        DEPENDS ${abs_osl_headers_sources} ${abs_osl_headers_sandbox}
    )
endforeach ()

# Copy OSL header files to the sandbox.
foreach (osl_header ${osl_headers})
    get_filename_component (path ${osl_header} PATH)
    add_custom_command (
        OUTPUT ${PROJECT_SOURCE_DIR}/sandbox/shaders/${osl_header}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_SOURCE_DIR}/sandbox/shaders/${path}
        COMMAND ${CMAKE_COMMAND} -E copy
            ${PROJECT_SOURCE_DIR}/src/appleseed.shaders/${osl_header}
            ${PROJECT_SOURCE_DIR}/sandbox/shaders/${osl_header}
        MAIN_DEPENDENCY ${PROJECT_SOURCE_DIR}/src/appleseed.shaders/${osl_header}
    )
endforeach ()

add_custom_target (
    appleseed.shaders ALL
    DEPENDS ${abs_osl_headers_sandbox}
    SOURCES ${abs_osl_headers_sources} ${osl_sources}
)

add_dependencies (appleseed.shaders SANDBOX)
