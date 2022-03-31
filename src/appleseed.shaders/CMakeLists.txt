
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2016-2018 Esteban Tovagliari, The appleseedhq Organization
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

set (include_appleseed_color_sources
    include/appleseed/color/as_chromatic_adaptation.h
    include/appleseed/color/as_color_blend_modes.h
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
)
list (APPEND osl_headers
    ${include_appleseed_fractal_sources}
)
source_group ("include\\appleseed\\fractal" FILES
    ${include_appleseed_fractal_sources}
)

set (include_appleseed_material_sources
    include/appleseed/material/as_material_helpers.h
    include/appleseed/material/as_hazy_gloss.h
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

set (include_appleseed_max_sources
)
list (APPEND osl_headers
    ${include_appleseed_max_sources}
)
source_group ("include\\appleseed\\max" FILES
    ${include_appleseed_max_sources}
)

set (include_appleseed_maya_sources
    include/appleseed/maya/as_maya_cms_syncolor_idt.h
    include/appleseed/maya/as_maya_fractal_helpers.h
    include/appleseed/maya/as_maya_helpers.h
    include/appleseed/maya/as_maya_layer_helpers.h
    include/appleseed/maya/as_maya_pattern_helpers.h
    include/appleseed/maya/as_maya_ramp_helpers.h
    include/appleseed/maya/as_maya_recurrence_helpers.h
    include/appleseed/maya/as_maya_recurrence_tables.h
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

set (include_appleseed_texture_sources
    include/appleseed/texture/as_texture_helpers.h
)
list (APPEND osl_headers
    ${include_appleseed_texture_sources}
)
source_group ("include\\appleseed\\texture" FILES
    ${include_appleseed_texture_sources}
)

set (include_appleseed_transform_sources
    include/appleseed/transform/as_transform_helpers.h
)
list (APPEND osl_headers
    ${include_appleseed_transform_sources}
)
source_group ("include\\appleseed\\transform" FILES
    ${include_appleseed_transform_sources}
)


#--------------------------------------------------------------------------------------------------
# Source files.
#--------------------------------------------------------------------------------------------------

set (src_blenderseed_sources
     src/blenderseed/as_blender_areaLight.osl
)
list (APPEND osl_sources
    ${src_blenderseed_sources}
)
source_group ("src\\blenderseed" FILES
    ${src_blenderseed_sources}
)

set (src_max_sources
     src/max/as_max_blend_material.osl
     src/max/as_max_bump_map.osl
     src/max/as_max_closure2surface.osl
     src/max/as_max_color_balance.osl
     src/max/as_max_color_texture.osl
     src/max/as_max_disney_material.osl
     src/max/as_max_float_texture.osl
     src/max/as_max_glass_material.osl
     src/max/as_max_light_material.osl
     src/max/as_max_metal_material.osl
     src/max/as_max_normal_map.osl
     src/max/as_max_plastic_material.osl
     src/max/as_max_srgb_to_linear_rgb.osl
     src/max/as_max_sss_material.osl
     src/max/as_max_uv_transform.osl
)
list (APPEND osl_sources
    ${src_max_sources}
)
source_group ("src\\max" FILES
    ${src_max_sources}
)

set (src_maya_sources
     src/maya/as_maya_addDoubleLinear.osl
     src/maya/as_maya_addMatrix.osl
     src/maya/as_maya_anisotropic.osl
     src/maya/as_maya_areaLight.osl
     src/maya/as_maya_blendColors.osl
     src/maya/as_maya_blinn.osl
     src/maya/as_maya_brownian.osl
     src/maya/as_maya_bulge.osl
     src/maya/as_maya_bump2d.osl
     src/maya/as_maya_bump3d.osl
     src/maya/as_maya_checker.osl
     src/maya/as_maya_clamp.osl
     src/maya/as_maya_clearCoat.osl
     src/maya/as_maya_cloth.osl
     src/maya/as_maya_cloud.osl
     src/maya/as_maya_color2Components.osl
     src/maya/as_maya_components2Color.osl
     src/maya/as_maya_components2UV.osl
     src/maya/as_maya_components2Vector.osl
     src/maya/as_maya_condition.osl
     src/maya/as_maya_contrast.osl
     src/maya/as_maya_crater.osl
     src/maya/as_maya_distanceBetween.osl
     src/maya/as_maya_doubleShadingSwitch.osl
     src/maya/as_maya_envChrome.osl
     src/maya/as_maya_file.osl
     src/maya/as_maya_fourByFourMatrix.osl
     src/maya/as_maya_fractal.osl
     src/maya/as_maya_gammaCorrect.osl
     src/maya/as_maya_granite.osl
     src/maya/as_maya_grid.osl
     src/maya/as_maya_hsvToRgb.osl
     src/maya/as_maya_lambert.osl
     src/maya/as_maya_layeredTexture.osl
     src/maya/as_maya_luminance.osl
     src/maya/as_maya_mandelbrot.osl
     src/maya/as_maya_marble.osl
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
     src/maya/as_maya_rock.osl
     src/maya/as_maya_samplerInfo.osl
     src/maya/as_maya_setRange.osl
     src/maya/as_maya_simplexNoise.osl
     src/maya/as_maya_singleShadingSwitch.osl
     src/maya/as_maya_snow.osl
     src/maya/as_maya_solidFractal.osl
     src/maya/as_maya_stencil.osl
     src/maya/as_maya_stucco.osl
     src/maya/as_maya_transposeMatrix.osl
     src/maya/as_maya_tripleShadingSwitch.osl
     src/maya/as_maya_uv2Components.osl
     src/maya/as_maya_vector2Components.osl
     src/maya/as_maya_vectorProduct.osl
     src/maya/as_maya_volumeNoise.osl
     src/maya/as_maya_water.osl
     src/maya/as_maya_wood.osl
     src/maya/as_maya_wtAddMatrix.osl
)
list (APPEND osl_sources
    ${src_maya_sources}
)
source_group ("src\\maya" FILES
    ${src_maya_sources}
)

set (src_appleseed_sources
     src/appleseed/as_anisotropy_vector_field.osl
     src/appleseed/as_asc_cdl.osl
     src/appleseed/as_attributes.osl
     src/appleseed/as_blackbody.osl
     src/appleseed/as_blend_color.osl
     src/appleseed/as_blend_normal.osl
     src/appleseed/as_blend_shader.osl
     src/appleseed/as_bump.osl
     src/appleseed/as_closure2surface.osl
     src/appleseed/as_color_transform.osl
     src/appleseed/as_composite_color.osl
     src/appleseed/as_create_mask.osl
     src/appleseed/as_disney_material.osl
     src/appleseed/as_double_shade.osl
     src/appleseed/as_falloff_angle.osl
     src/appleseed/as_fresnel.osl
     src/appleseed/as_glass.osl
     src/appleseed/as_globals.osl
     src/appleseed/as_hair_material.osl
     src/appleseed/as_id_manifold.osl
     src/appleseed/as_invert_color.osl
     src/appleseed/as_luminance.osl
     src/appleseed/as_manifold2d.osl
     src/appleseed/as_matte.osl
     src/appleseed/as_metal.osl
     src/appleseed/as_noise2d.osl
     src/appleseed/as_noise3d.osl
     src/appleseed/as_plastic.osl
     src/appleseed/as_ray_switch.osl
     src/appleseed/as_ray_switch_surface.osl
     src/appleseed/as_sbs_pbrmaterial.osl
     src/appleseed/as_space_transform.osl
     src/appleseed/as_standard_surface.osl
     src/appleseed/as_standard_surface_next.osl
     src/appleseed/as_subsurface.osl
     src/appleseed/as_surface_luminance.osl
     src/appleseed/as_switch_surface.osl
     src/appleseed/as_switch_texture.osl
     src/appleseed/as_swizzle.osl
     src/appleseed/as_texture.osl
     src/appleseed/as_texture2surface.osl
     src/appleseed/as_texture3d.osl
     src/appleseed/as_texture_info.osl
     src/appleseed/as_toon.osl
     src/appleseed/as_triplanar.osl
     src/appleseed/as_vary_color.osl
     src/appleseed/as_voronoi2d.osl
     src/appleseed/as_voronoi3d.osl
)
list (APPEND osl_sources
    ${src_appleseed_sources}
)
source_group ("src\\appleseed" FILES
    ${src_appleseed_sources}
)


#--------------------------------------------------------------------------------------------------
# Target: Prepare the sandbox.
#--------------------------------------------------------------------------------------------------

add_custom_target (
    prepare_sandbox
    VERBATIM
)

set_target_properties (prepare_sandbox PROPERTIES FOLDER "Contents")
set_target_progress_message (prepare_sandbox "Preparing sandbox...")

# Delete the sandbox/shaders/ directory.
# add_custom_command (
#     TARGET prepare_sandbox
#     COMMAND ${CMAKE_COMMAND} -E remove_directory ${PROJECT_SOURCE_DIR}/sandbox/shaders/
# )

# Copy the OSL compiler to sandbox/bin/.
if (USE_FIND_PACKAGE_FOR_OSL)
    add_custom_command (
        TARGET prepare_sandbox
        COMMAND ${CMAKE_COMMAND} -E copy ${OSL_COMPILER} ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
else ()
    add_custom_command (
        TARGET prepare_sandbox
        COMMAND ${CMAKE_COMMAND} -E copy ${APPLESEED_DEPS_STAGE_DIR}/osl-release/bin/oslc.exe ${PROJECT_SOURCE_DIR}/sandbox/bin/
    )
endif ()


#--------------------------------------------------------------------------------------------------
# Target: Compile shaders and deploy header files to sandbox.
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
        set (OSLC_OPTIONS -O0 -DDEBUG)
    else ()
        set (OSLC_OPTIONS -O2 -DNDEBUG)
    endif ()

    add_custom_command (
        OUTPUT ${oso_dir}/${oso_filename}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${oso_dir}
        COMMAND ${PROJECT_SOURCE_DIR}/sandbox/bin/oslc ${OSLC_OPTIONS}
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
    VERBATIM
)

set_target_properties (appleseed.shaders PROPERTIES FOLDER "Contents")
set_target_progress_message (appleseed.shaders "Compiling OSL shaders...")

add_dependencies (appleseed.shaders prepare_sandbox)
