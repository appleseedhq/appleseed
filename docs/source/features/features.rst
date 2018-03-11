.. _label_features:

********
Features
********

Appleseed supports amongst other features

    * fully programmable shading via `Sony Pictures Imageworks <http://www.imageworks.com/>`_’ `Open Shading Language (OSL) <https://github.com/imageworks/OpenShadingLanguage>`_ 
    * RGB/spectral/mixed rendering
    * fast and robust transformation and deformation motion blur
    * state-of-the-art ray traced subsurface scattering
    * exhaustive Python and C++ APIs
    * and many other production-oriented features.

.. _label_features_ecosystem:

Ecosystem
=========

Along with the core renderer, the team is actively developing high quality integrations

    * appleseed for Autodesk® Maya® → `appleseed-maya <https://github.com/appleseedhq/appleseed-maya>`_
    * appleseed for Autodesk® 3ds Max® → `appleseed-max <https://github.com/appleseedhq/appleseed-max>`_
    * appleseed for Blender®  → `blenderseed <https://github.com/appleseedhq/blenderseed>`_ 

appleseed is also the default rendering engine of `Image Engine’s Gaffer <http://www.gafferhq.org/>`_.

.. _label_features_detailed_features_list:

Detailed Features List
======================

.. _label_features_light_transport:

Light Transport
---------------

 * Unidirectional Path Tracing [#]_
 * Stochastic Progressive Photon Mapping [#]_
 * Light Tracing

.. _label_features_reflection_models:

Reflection Models
-----------------

 * Lambertian BRDF (purely diffuse)
 * Specular BRDF (perfect mirror)
 * Specular BSDF
 * Diffuse BTDF
 * Oren-Nayar Microfacet BRDF
 * Ward Microfacet BRDF
 * Blinn Microfacet BRDF
 * Microfacet BRDF with GGX, GTR, Student's t-MDF
 * Disney's principled BRDF
 * Ashikhmin-Shirley BRDF
 * Kelemen BRDF
 * Glossy BRDF
 * Conductor Fresnel metal BRDF
 * Stacked plastic BRDF
 * Glass BSDF with absorption
 * Sheen BRDF
 * Arbitrary mixtures of BRDFs
 * Energy compensation in microfacet BxDFs.

.. _label_features_subsurface_scattering:

Subsurface Scattering
---------------------

appleseed provides state-of-the art raytraced, animation-friendly subsurface scattering, fully exposed to OSL.
There is no precomputation and it's available in interactive rendering. 
SSS sets are also supported.

.. _label_features_subsurface_scattering_profiles:

Subsurface Scattering Profiles
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following subsurface scattering profiles are available

 * Better Dipole
 * Directional Dipole
 * Gaussian
 * Normalized Diffusion
 * Standard Dipole
 * Random Walk

.. _label_features_volume_rendering:

Volume rendering
----------------

 * Single scattering
 * Multiple scattering
 * Homogeneous media
 * Isotropic phase function
 * Henyey-Greenstein phase function

.. _label_features_camera_models:

Camera Models
-------------

 * Pinhole camera
 * Spherical camera
 * Thin lens camera (depth of field)
 * Polygonal diaphragm shapes
 * Full control over the bokeh via image-based diaphragm shapes

.. _label_features_motion_blur:

Motion Blur
-----------

 * Camera motion blur
 * Transformation motion blur
 * Deformation motion blur
 * Arbitrary number of motion steps

.. _label_features_light_source_models:

Light Source Models
-------------------

 * Point light
 * Spot light
 * Gobos
 * Directional/parallel light
 * Mesh light
 * Purely diffuse emission profile
 * Cone-shaped emission profile
 * Image-based lighting
 * Latitude-longitude environment maps
 * Mirror-ball environment maps
 * Preetham physically-based day sky model
 * Hosek & Wilkie physically-based day sky model
 * Physically-based Sun
 * Environment transforms

.. _label_features_production_features:

Production Features
-------------------

Besides the extensive list of features mentioned, appleseed provides

 * Open Shading Language support (OSL)
 * A production oriented OSL shader library
 * OpenColorIO support
 * Disney SeExpr expressions
 * AOVs
 * Hierarchical instancing
 * Per-instance visibility flags
 * Alpha mapping
 * Automatic color space conversions
 * Ray differentials
 * Ray bias
 * Light Near Start
 * Low light threshold
 * Max ray intensity
 * Bounce limits per scatterint types
 * Nested dielectrics
 * Automatic tracking of indices of refraction
 * Packed, single-file scenes for easy transport
 * Search paths for external assets
 * Dozens of diagnostic modes

.. _label_features_interoperability:

Interoperability
----------------

 * Windows, Linux and macOS (64-bit)
 * OBJ geometry files
 * OpenEXR, TIFF, HDR, PNG, JPEG, PSD...
 * Full Open Shading Language support
 * Integration with 3rd party applications via :ref:`connection plugins <label_features_ecosystem>`

.. _label_features_performance

Performance
-----------

 * Multithreaded, scalable
 * SSE, SSE2, SSE4.2 vectorization
 * Memory-bounded texture cache
 * Multiple Importance Sampling
 * Efficient handling of alpha maps
 * Advanced many-light sampling

.. _label_features_tools:

Tools
-----

 * Full featured graphical tool for scene editing (appleseed.studio)
 * Command line renderer
 * Dropbox-based render farm tools
 * OSL compiler and tools
 * Texture processor (OIIO)

.. _label_features_hackability

Hackability
-----------

 * 100% open source, MIT license
 * Exceptionally clean, fully reviewed code
 * Full featured C++ API
 * Full extensibility via external C++ plugins
 * Full featured Python 2.x/3.x API
 * Embedded Python scripting
 * More than 1300 built-in unit tests
 * Hundreds of built-in performance tests
 * Rich, automatic functional test suite

-----

.. rubric:: Footnotes

.. [#] UDPT for short

.. [#] SPPM for short

-----

.. rubric:: References

.. bibliography:: /bibtex/references.bib
    :filter: docname in docnames

