
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "projectfilereader.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/aov/aovfactoryregistrar.h"
#include "renderer/modeling/aov/iaovfactory.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdffactoryregistrar.h"
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdffactoryregistrar.h"
#include "renderer/modeling/bssrdf/ibssrdffactory.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/camera/camerafactoryregistrar.h"
#include "renderer/modeling/camera/icamerafactory.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/display/display.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/edf/edffactoryregistrar.h"
#include "renderer/modeling/edf/iedffactory.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/environmentedffactoryregistrar.h"
#include "renderer/modeling/environmentedf/ienvironmentedffactory.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/environmentshader/environmentshaderfactoryregistrar.h"
#include "renderer/modeling/environmentshader/ienvironmentshaderfactory.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/light/ilightfactory.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/light/lightfactoryregistrar.h"
#include "renderer/modeling/material/imaterialfactory.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/material/materialfactoryregistrar.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/curveobjectreader.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/meshobjectreader.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/eventcounters.h"
#include "renderer/modeling/project/irenderlayerrulefactory.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/projectfileupdater.h"
#include "renderer/modeling/project/projectformatrevision.h"
#include "renderer/modeling/project/renderlayerrule.h"
#include "renderer/modeling/project/renderlayerrulefactoryregistrar.h"
#include "renderer/modeling/project-builtin/cornellboxproject.h"
#include "renderer/modeling/project-builtin/defaultproject.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyfactoryregistrar.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/iassemblyfactory.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/surfaceshader/isurfaceshaderfactory.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/surfaceshader/surfaceshaderfactoryregistrar.h"
#include "renderer/modeling/texture/itexturefactory.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/texture/texturefactoryregistrar.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionunsupportedfileformat.h"
#include "foundation/math/aabb.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iterators.h"
#include "foundation/utility/log.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"
#include "foundation/utility/unzipper.h"
#include "foundation/utility/xercesc.h"

// Xerces-C++ headers.
#include "xercesc/sax2/Attributes.hpp"
#include "xercesc/sax2/SAX2XMLReader.hpp"
#include "xercesc/sax2/XMLReaderFactory.hpp"
#include "xercesc/util/XMLException.hpp"
#include "xercesc/util/XMLUni.hpp"

// Boost headers.
#include "boost/filesystem.hpp"
#include "boost/filesystem/operations.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

using namespace foundation;
using namespace std;
using namespace xercesc;
namespace bf = boost::filesystem;

namespace renderer
{

//
// ProjectFileReader class implementation.
//

namespace
{
    //
    // Like foundation::ErrorHandler, but additionally keeps track
    // of the number of warnings and errors that were emitted.
    //

    class ErrorLoggerAndCounter
      : public ErrorLogger
    {
      public:
        ErrorLoggerAndCounter(
            const string&   input_filepath,
            EventCounters&  event_counters)
          : ErrorLogger(global_logger(), input_filepath)
          , m_event_counters(event_counters)
        {
        }

        virtual void resetErrors() APPLESEED_OVERRIDE
        {
            m_event_counters.clear();
        }

        virtual void warning(const SAXParseException& e) APPLESEED_OVERRIDE
        {
            ErrorLogger::warning(e);
            m_event_counters.signal_warning();
        }

        virtual void error(const SAXParseException& e) APPLESEED_OVERRIDE
        {
            ErrorLogger::error(e);
            m_event_counters.signal_error();
            throw e;    // terminate parsing
        }

        virtual void fatalError(const SAXParseException& e) APPLESEED_OVERRIDE
        {
            ErrorLogger::fatalError(e);
            m_event_counters.signal_error();
            throw e;    // terminate parsing
        }

      private:
        EventCounters&  m_event_counters;
    };


    //
    // A set of objects that is passed to all element handlers.
    //

    class ParseContext
    {
      public:
        ParseContext(
            Project&        project,
            const int       options,
            EventCounters&  event_counters)
          : m_project(project)
          , m_options(options)
          , m_event_counters(event_counters)
        {
        }

        Project& get_project()
        {
            return m_project;
        }

        int get_options() const
        {
            return m_options;
        }

        EventCounters& get_event_counters()
        {
            return m_event_counters;
        }

      private:
        Project&            m_project;
        const int           m_options;
        EventCounters&      m_event_counters;
    };


    //
    // Utility functions.
    //

    template <typename T>
    T get_scalar(
        const string&       text,
        ParseContext&       context)
    {
        try
        {
            return from_string<T>(text);
        }
        catch (const ExceptionStringConversionError&)
        {
            RENDERER_LOG_ERROR("expected scalar value, got \"%s\".", text.c_str());
            context.get_event_counters().signal_error();
            return T(0.0);
        }
    }

    Vector3d get_vector3(
        const string&       text,
        ParseContext&       context)
    {
        Vector3d vec;
        bool succeeded = false;

        try
        {
            const size_t count = tokenize(text, Blanks, &vec[0], 3);
            if (count == 1)
            {
                vec[1] = vec[0];
                vec[2] = vec[0];
                succeeded = true;
            }
            else if (count == 3)
            {
                succeeded = true;
            }
        }
        catch (const ExceptionStringConversionError&)
        {
        }

        if (!succeeded)
        {
            RENDERER_LOG_ERROR("invalid vector format.");
            context.get_event_counters().signal_error();
            vec = Vector3d(0.0);
        }

        return vec;
    }

    template <typename Vec>
    void get_vector(
        const string&       text,
        Vec&                values,
        ParseContext&       context)
    {
        try
        {
            tokenize(text, Blanks, values);
        }
        catch (const ExceptionStringConversionError&)
        {
            RENDERER_LOG_ERROR("invalid vector format.");
            context.get_event_counters().signal_error();
            values.clear();
        }
    }

    template <typename Entity, typename EntityFactoryRegistrar>
    auto_release_ptr<Entity> create_entity(
        const EntityFactoryRegistrar&   registrar,
        const string&                   type,
        const string&                   model,
        const string&                   name,
        const ParamArray&               params,
        ParseContext&                   context)
    {
        try
        {
            const typename EntityFactoryRegistrar::FactoryType* factory =
                registrar.lookup(model.c_str());

            if (factory)
            {
                return factory->create(name.c_str(), params);
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "while defining %s \"%s\": invalid model \"%s\".",
                    type.c_str(),
                    name.c_str(),
                    model.c_str());
                context.get_event_counters().signal_error();
            }
        }
        catch (const ExceptionDictionaryKeyNotFound& e)
        {
            RENDERER_LOG_ERROR(
                "while defining %s \"%s\": required parameter \"%s\" missing.",
                type.c_str(),
                name.c_str(),
                e.string());
            context.get_event_counters().signal_error();
        }
        catch (const ExceptionUnknownEntity& e)
        {
            RENDERER_LOG_ERROR(
                "while defining %s \"%s\": unknown entity \"%s\".",
                type.c_str(),
                name.c_str(),
                e.string());
            context.get_event_counters().signal_error();
        }

        return auto_release_ptr<Entity>(0);
    }


    //
    // Numeric representation of the XML elements.
    //

    enum ProjectElementID
    {
        ElementAlpha,
        ElementAOV,
        ElementAOVs,
        ElementAssembly,
        ElementAssemblyInstance,
        ElementAssignMaterial,
        ElementBSDF,
        ElementBSSRDF,
        ElementCamera,
        ElementColor,
        ElementConfiguration,
        ElementConfigurations,
        ElementDisplay,
        ElementEDF,
        ElementEnvironment,
        ElementEnvironmentEDF,
        ElementEnvironmentShader,
        ElementFrame,
        ElementLight,
        ElementLookAt,
        ElementMaterial,
        ElementMatrix,
        ElementObject,
        ElementObjectInstance,
        ElementOutput,
        ElementParameter,
        ElementParameters,
        ElementProject,
        ElementRenderLayerAssignment,
        ElementRotation,
        ElementRules,
        ElementScaling,
        ElementScene,
        ElementSearchPath,
        ElementSearchPaths,
        ElementShader,
        ElementShaderConnection,
        ElementShaderGroup,
        ElementSurfaceShader,
        ElementTexture,
        ElementTextureInstance,
        ElementTransform,
        ElementTranslation,
        ElementValues
    };

    typedef IElementHandler<ProjectElementID> ElementHandlerType;
    typedef ElementHandlerBase<ProjectElementID> ElementHandlerBase;


    //
    // <parameter> element handler.
    //

    class ParameterElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit ParameterElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            m_name = ElementHandlerBase::get_value(attrs, "name");
            m_value = ElementHandlerBase::get_value(attrs, "value");
        }

        virtual void characters(
            const XMLCh* const  chars,
            const XMLSize_t     length) APPLESEED_OVERRIDE
        {
            const string inner_value = transcode(chars);
            if (!m_value.empty() && !inner_value.empty())
            {
                RENDERER_LOG_ERROR(
                    "while defining <parameter> element: value specified multiple times.");
                m_context.get_event_counters().signal_error();
            }
            else
            {
                m_value = inner_value;
            }
        }

        const string& get_name() const
        {
            return m_name;
        }

        const string& get_value() const
        {
            return m_value;
        }

      private:
        ParseContext&   m_context;
        string          m_name;
        string          m_value;
    };


    //
    // Handle an element containing a (hierarchical) set of parameters.
    //

    class ParametrizedElementHandler
      : public ElementHandlerBase
    {
      public:
        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE;

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE;

      protected:
        ParamArray m_params;
    };


    //
    // <parameters> element handler.
    //

    class ParametersElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit ParametersElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);

            m_params.clear();

            m_name = get_value(attrs, "name");
        }

        const string& get_name() const
        {
            return m_name;
        }

        const ParamArray& get_parameters() const
        {
            return m_params;
        }

      private:
        ParseContext&   m_context;
        string          m_name;
    };


    //
    // ParametrizedElementHandler class implementation.
    //

    void ParametrizedElementHandler::start_element(const Attributes& attrs)
    {
        m_params.clear();
    }

    void ParametrizedElementHandler::end_child_element(
        const ProjectElementID      element,
        ElementHandlerType*         handler)
    {
        switch (element)
        {
          case ElementParameter:
            {
                ParameterElementHandler* param_handler =
                    static_cast<ParameterElementHandler*>(handler);
                m_params.insert_path(
                    param_handler->get_name(),
                    param_handler->get_value());
            }
            break;

          case ElementParameters:
            {
                ParametersElementHandler* params_handler =
                    static_cast<ParametersElementHandler*>(handler);
                m_params.dictionaries().insert(
                    params_handler->get_name(),
                    params_handler->get_parameters());
            }
            break;

          assert_otherwise;
        }
    }


    //
    // <look_at> element handler.
    //

    class LookAtElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit LookAtElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            m_matrix = Matrix4d::identity();

            const Vector3d origin = get_vector3(get_value(attrs, "origin"), m_context);
            const Vector3d target = get_vector3(get_value(attrs, "target"), m_context);
            const Vector3d up = get_vector3(get_value(attrs, "up"), m_context);

            if (norm(origin - target) > 0.0 &&
                norm(up) > 0.0 &&
                norm(cross(up, origin - target)) > 0.0)
            {
                m_matrix = Matrix4d::make_lookat(origin, target, normalize(up));
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "while defining <look_at> element: the vectors\n"
                    "  origin  (%f, %f, %f)\n"
                    "  target  (%f, %f, %f)\n"
                    "  up      (%f, %f, %f)\n"
                    "form a singular transformation matrix.",
                    origin[0], origin[1], origin[2],
                    target[0], target[1], target[2],
                    up[0], up[1], up[2]);
                m_context.get_event_counters().signal_error();
            }
        }

        const Matrix4d& get_matrix() const
        {
            return m_matrix;
        }

      private:
        ParseContext&   m_context;
        Matrix4d        m_matrix;
    };


    //
    // <matrix> element handler.
    //

    class MatrixElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit MatrixElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            m_matrix = Matrix4d::identity();
            clear_keep_memory(m_values);
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            if (m_values.size() == 16)
            {
                for (size_t i = 0; i < 16; ++i)
                    m_matrix[i] = m_values[i];
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "while defining <matrix> element: expected 16 scalar coefficients, got " FMT_SIZE_T ".",
                    m_values.size());
                m_context.get_event_counters().signal_error();
            }
        }

        virtual void characters(
            const XMLCh* const  chars,
            const XMLSize_t     length) APPLESEED_OVERRIDE
        {
            get_vector(transcode(chars), m_values, m_context);
        }

        const Matrix4d& get_matrix() const
        {
            return m_matrix;
        }

      private:
        ParseContext&   m_context;
        Matrix4d        m_matrix;
        vector<double>  m_values;
    };


    //
    // <rotation> element handler.
    //

    class RotationElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit RotationElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            m_matrix = Matrix4d::identity();

            const Vector3d axis = get_vector3(get_value(attrs, "axis"), m_context);
            const double angle = get_scalar<double>(get_value(attrs, "angle"), m_context);

            if (norm(axis) > 0.0)
            {
                m_matrix = Matrix4d::make_rotation(normalize(axis), deg_to_rad(angle));
            }
            else
            {
                RENDERER_LOG_ERROR("while defining <rotation> element: the rotation axis cannot be null.");
                m_context.get_event_counters().signal_error();
            }
        }

        const Matrix4d& get_matrix() const
        {
            return m_matrix;
        }

      private:
        ParseContext&   m_context;
        Matrix4d        m_matrix;
    };


    //
    // <scaling> element handler.
    //

    class ScalingElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit ScalingElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            const Vector3d value = get_vector3(get_value(attrs, "value"), m_context);
            m_matrix = Matrix4d::make_scaling(value);
        }

        const Matrix4d& get_matrix() const
        {
            return m_matrix;
        }

      private:
        ParseContext&   m_context;
        Matrix4d        m_matrix;
    };


    //
    // <translation> element handler.
    //

    class TranslationElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit TranslationElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            const Vector3d value = get_vector3(get_value(attrs, "value"), m_context);
            m_matrix = Matrix4d::make_translation(value);
        }

        const Matrix4d& get_matrix() const
        {
            return m_matrix;
        }

      private:
        ParseContext&   m_context;
        Matrix4d        m_matrix;
    };


    //
    // <transform> element handler.
    //

    class TransformElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit TransformElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            m_time = get_scalar<float>(get_value(attrs, "time", "0.0"), m_context);
            m_matrix = Matrix4d::identity();
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            try
            {
                m_transform = Transformd::from_local_to_parent(m_matrix);
            }
            catch (const ExceptionSingularMatrix&)
            {
                RENDERER_LOG_ERROR("while defining <transform> element: the transformation matrix is singular.");
                m_context.get_event_counters().signal_error();
                m_transform = Transformd::identity();
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            switch (element)
            {
              case ElementLookAt:
                {
                    LookAtElementHandler* lookat_handler =
                        static_cast<LookAtElementHandler*>(handler);
                    m_matrix = lookat_handler->get_matrix() * m_matrix;
                }
                break;

              case ElementMatrix:
                {
                    MatrixElementHandler* matrix_handler =
                        static_cast<MatrixElementHandler*>(handler);
                    m_matrix = matrix_handler->get_matrix() * m_matrix;
                }
                break;

              case ElementRotation:
                {
                    RotationElementHandler* rotation_handler =
                        static_cast<RotationElementHandler*>(handler);
                    m_matrix = rotation_handler->get_matrix() * m_matrix;
                }
                break;

              case ElementScaling:
                {
                    ScalingElementHandler* scaling_handler =
                        static_cast<ScalingElementHandler*>(handler);
                    m_matrix = scaling_handler->get_matrix() * m_matrix;
                }
                break;

              case ElementTranslation:
                {
                    TranslationElementHandler* translation_handler =
                        static_cast<TranslationElementHandler*>(handler);
                    m_matrix = translation_handler->get_matrix() * m_matrix;
                }
                break;

              assert_otherwise;
            }
        }

        float get_time() const
        {
            return m_time;
        }

        const Transformd& get_transform() const
        {
            return m_transform;
        }

      private:
        ParseContext&   m_context;
        float           m_time;
        Matrix4d        m_matrix;
        Transformd      m_transform;
    };


    //
    // Handle a transformation sequence.
    //

    template <typename Base>
    class TransformSequenceElementHandler
      : public Base
    {
      public:
        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            Base::start_element(attrs);
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            if (m_transforms.size() > 1)
                collapse_transforms();

            if (m_transforms.empty())
                m_transforms[0.0f] = Transformd::identity();
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            switch (element)
            {
              case ElementTransform:
                {
                    TransformElementHandler* transform_handler =
                        static_cast<TransformElementHandler*>(handler);
                    m_transforms[transform_handler->get_time()] = transform_handler->get_transform();
                }
                break;

              default:
                Base::end_child_element(element, handler);
                break;
            }
        }

        void copy_transform_sequence_to(TransformSequence& target) const
        {
            target.clear();

            for (const_each<TransformMap> i = m_transforms; i; ++i)
                target.set_transform(i->first, i->second);
        }

        Transformd get_earliest_transform() const
        {
            TransformSequence sequence;
            copy_transform_sequence_to(sequence);
            return sequence.get_earliest_transform();
        }

      private:
        typedef map<float, Transformd> TransformMap;

        TransformMap m_transforms;

        void collapse_transforms()
        {
            if (are_transforms_identical())
            {
                const Transformd transform = m_transforms.begin()->second;
                m_transforms.clear();
                m_transforms[0.0f] = transform;
            }
        }

        bool are_transforms_identical() const
        {
            for (TransformMap::const_iterator i = m_transforms.begin(), e = pred(m_transforms.end()); i != e; ++i)
            {
                if (i->second != succ(i)->second)
                    return false;
            }

            return true;
        }
    };


    //
    // <values> element handler.
    //

    class ValuesElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit ValuesElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            m_values.clear();
        }

        virtual void characters(
            const XMLCh* const  chars,
            const XMLSize_t     length) APPLESEED_OVERRIDE
        {
            get_vector(transcode(chars), m_values, m_context);
        }

        const ColorValueArray& get_values() const
        {
            return m_values;
        }

      private:
        ParseContext&   m_context;
        ColorValueArray m_values;
    };


    //
    // <color> and <alpha> elements handler.
    //

    class ColorElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit ColorElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);

            m_color_entity.reset();
            m_values.clear();
            m_alpha.clear();

            m_name = get_value(attrs, "name");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::end_element();

            try
            {
                m_color_entity =
                    m_alpha.empty()
                        ? ColorEntityFactory::create(
                            m_name.c_str(),
                            m_params,
                            m_values)
                        : ColorEntityFactory::create(
                            m_name.c_str(),
                            m_params,
                            m_values,
                            m_alpha);
            }
            catch (const ExceptionDictionaryKeyNotFound& e)
            {
                RENDERER_LOG_ERROR(
                    "while defining color \"%s\": required parameter \"%s\" missing.",
                    m_name.c_str(),
                    e.string());
                m_context.get_event_counters().signal_error();
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            switch (element)
            {
              case ElementValues:
                m_values = static_cast<ValuesElementHandler*>(handler)->get_values();
                break;

              case ElementAlpha:
                m_alpha = static_cast<ValuesElementHandler*>(handler)->get_values();
                break;

              default:
                ParametrizedElementHandler::end_child_element(element, handler);
                break;
            }
        }

        auto_release_ptr<ColorEntity> get_color_entity()
        {
            return m_color_entity;
        }

      private:
        ParseContext&                   m_context;
        auto_release_ptr<ColorEntity>   m_color_entity;
        string                          m_name;
        ColorValueArray                 m_values;
        ColorValueArray                 m_alpha;
    };


    //
    // Handle an element defining an entity.
    //

    template <typename Entity, typename EntityFactoryRegistrar, typename Base>
    class EntityElementHandler
      : public Base
    {
      public:
        EntityElementHandler(const string& entity_type, ParseContext& context)
          : m_context(context)
          , m_entity_type(entity_type)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            Base::start_element(attrs);

            m_entity.reset();

            m_name = Base::get_value(attrs, "name");
            m_model = Base::get_value(attrs, "model");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            Base::end_element();

            m_entity =
                create_entity<Entity>(
                    m_registrar,
                    m_entity_type,
                    m_model,
                    m_name,
                    Base::m_params,
                    m_context);
        }

        auto_release_ptr<Entity> get_entity()
        {
            return m_entity;
        }

      protected:
        ParseContext&                   m_context;
        const EntityFactoryRegistrar    m_registrar;
        const string                    m_entity_type;
        auto_release_ptr<Entity>        m_entity;
        string                          m_name;
        string                          m_model;
    };


    //
    // <texture> element handler.
    //

    class TextureElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit TextureElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);

            m_texture.reset();

            m_name = get_value(attrs, "name");
            m_model = get_value(attrs, "model");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::end_element();

            try
            {
                const TextureFactoryRegistrar::FactoryType* factory =
                    m_texture_factory_registrar.lookup(m_model.c_str());

                if (factory)
                {
                    m_texture =
                        factory->create(
                            m_name.c_str(),
                            m_params,
                            m_context.get_project().search_paths());
                }
                else
                {
                    RENDERER_LOG_ERROR(
                        "while defining texture \"%s\": invalid model \"%s\".",
                        m_name.c_str(),
                        m_model.c_str());
                    m_context.get_event_counters().signal_error();
                }
            }
            catch (const ExceptionDictionaryKeyNotFound& e)
            {
                RENDERER_LOG_ERROR(
                    "while defining texture \"%s\": required parameter \"%s\" missing.",
                    m_name.c_str(),
                    e.string());
                m_context.get_event_counters().signal_error();
            }
        }

        auto_release_ptr<Texture> get_texture()
        {
            return m_texture;
        }

      private:
        const TextureFactoryRegistrar   m_texture_factory_registrar;
        ParseContext&                   m_context;
        auto_release_ptr<Texture>       m_texture;
        string                          m_name;
        string                          m_model;
    };


    //
    // <texture_instance> element handler.
    //

    class TextureInstanceElementHandler
      : public TransformSequenceElementHandler<ParametrizedElementHandler>
    {
      public:
        explicit TextureInstanceElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            Base::start_element(attrs);

            m_texture_instance.reset();

            m_name = get_value(attrs, "name");
            m_texture = get_value(attrs, "texture");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            Base::end_element();

            try
            {
                m_texture_instance =
                    TextureInstanceFactory::create(
                        m_name.c_str(),
                        m_params,
                        m_texture.c_str(),
                        Transformf(
                            get_earliest_transform().get_local_to_parent(),
                            get_earliest_transform().get_parent_to_local()));
            }
            catch (const ExceptionDictionaryKeyNotFound& e)
            {
                RENDERER_LOG_ERROR(
                    "while defining texture instance \"%s\": required parameter \"%s\" missing.",
                    m_name.c_str(),
                    e.string());
                m_context.get_event_counters().signal_error();
            }
        }

        auto_release_ptr<TextureInstance> get_texture_instance()
        {
            return m_texture_instance;
        }

      private:
        typedef TransformSequenceElementHandler<ParametrizedElementHandler> Base;

        ParseContext&                       m_context;
        auto_release_ptr<TextureInstance>   m_texture_instance;
        string                              m_name;
        string                              m_texture;
    };


    //
    // <bsdf> element handler.
    //

    class BSDFElementHandler
      : public EntityElementHandler<
                   BSDF,
                   BSDFFactoryRegistrar,
                   ParametrizedElementHandler>
    {
      public:
        explicit BSDFElementHandler(ParseContext& context)
          : EntityElementHandler<
                BSDF,
                BSDFFactoryRegistrar,
                ParametrizedElementHandler>("bsdf", context)
        {
        }
    };


    //
    // <bssrdf> element handler.
    //

    class BSSRDFElementHandler
      : public EntityElementHandler<
                   BSSRDF,
                   BSSRDFFactoryRegistrar,
                   ParametrizedElementHandler>
    {
      public:
        explicit BSSRDFElementHandler(ParseContext& context)
          : EntityElementHandler<
                BSSRDF,
                BSSRDFFactoryRegistrar,
                ParametrizedElementHandler>("bssrdf", context)
        {
        }
    };


    //
    // <edf> element handler.
    //

    class EDFElementHandler
      : public EntityElementHandler<
                   EDF,
                   EDFFactoryRegistrar,
                   ParametrizedElementHandler>
    {
      public:
        explicit EDFElementHandler(ParseContext& context)
          : EntityElementHandler<
                EDF,
                EDFFactoryRegistrar,
                ParametrizedElementHandler>("edf", context)
        {
        }
    };


    //
    // <surface_shader> element handler.
    //

    class SurfaceShaderElementHandler
      : public EntityElementHandler<
                   SurfaceShader,
                   SurfaceShaderFactoryRegistrar,
                   ParametrizedElementHandler>
    {
      public:
        explicit SurfaceShaderElementHandler(ParseContext& context)
          : EntityElementHandler<
                SurfaceShader,
                SurfaceShaderFactoryRegistrar,
                ParametrizedElementHandler>("surface shader", context)
        {
        }
    };


    //
    // <environment> element handler.
    //

    class EnvironmentElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit EnvironmentElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);

            m_environment.reset();

            m_name = get_value(attrs, "name");
            m_model = get_value(attrs, "model");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::end_element();

            if (m_model == EnvironmentFactory::get_model())
                m_environment = EnvironmentFactory::create(m_name.c_str(), m_params);
            else
            {
                RENDERER_LOG_ERROR(
                    "while defining environment \"%s\": invalid model \"%s\".",
                    m_name.c_str(),
                    m_model.c_str());
                m_context.get_event_counters().signal_error();
            }
        }

        auto_release_ptr<Environment> get_environment()
        {
            return m_environment;
        }

      private:
        ParseContext&                       m_context;
        auto_release_ptr<Environment>       m_environment;
        string                              m_name;
        string                              m_model;
    };


    //
    // <environment_edf> element handler.
    //

    class EnvironmentEDFElementHandler
      : public EntityElementHandler<
                   EnvironmentEDF,
                   EnvironmentEDFFactoryRegistrar,
                   TransformSequenceElementHandler<ParametrizedElementHandler> >
    {
      public:
        explicit EnvironmentEDFElementHandler(ParseContext& context)
          : Base("environment edf", context)
        {
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            Base::end_element();

            if (m_entity.get())
                copy_transform_sequence_to(m_entity->transform_sequence());
        }

      private:
        typedef EntityElementHandler<
            EnvironmentEDF,
            EnvironmentEDFFactoryRegistrar,
            TransformSequenceElementHandler<ParametrizedElementHandler>
        > Base;
    };


    //
    // <environment_shader> element handler.
    //

    class EnvironmentShaderElementHandler
      : public EntityElementHandler<
                   EnvironmentShader,
                   EnvironmentShaderFactoryRegistrar,
                   ParametrizedElementHandler>
    {
      public:
        explicit EnvironmentShaderElementHandler(ParseContext& context)
          : EntityElementHandler<
                EnvironmentShader,
                EnvironmentShaderFactoryRegistrar,
                ParametrizedElementHandler>("environment shader", context)
        {
        }
    };


    //
    // <light> element handler.
    //

    class LightElementHandler
      : public EntityElementHandler<
                   Light,
                   LightFactoryRegistrar,
                   TransformSequenceElementHandler<ParametrizedElementHandler> >
    {
      public:
        explicit LightElementHandler(ParseContext& context)
          : Base("light", context)
        {
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            Base::end_element();

            if (m_entity.get())
                m_entity->set_transform(get_earliest_transform());
        }

      private:
        typedef EntityElementHandler<
            Light,
            LightFactoryRegistrar,
            TransformSequenceElementHandler<ParametrizedElementHandler>
        > Base;
    };


    //
    // <material> element handler.
    //

    class MaterialElementHandler
      : public EntityElementHandler<
                   Material,
                   MaterialFactoryRegistrar,
                   ParametrizedElementHandler>
    {
      public:
        explicit MaterialElementHandler(ParseContext& context)
          : EntityElementHandler<
                Material,
                MaterialFactoryRegistrar,
                ParametrizedElementHandler>("material", context)
        {
        }
    };


    //
    // <camera> element handler.
    //

    class CameraElementHandler
      : public EntityElementHandler<
                   Camera,
                   CameraFactoryRegistrar,
                   TransformSequenceElementHandler<ParametrizedElementHandler> >
    {
      public:
        explicit CameraElementHandler(ParseContext& context)
          : Base("camera", context)
        {
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            Base::end_element();

            if (m_entity.get())
                copy_transform_sequence_to(m_entity->transform_sequence());
        }

      private:
        typedef EntityElementHandler<
            Camera,
            CameraFactoryRegistrar,
            TransformSequenceElementHandler<ParametrizedElementHandler>
        > Base;
    };


    //
    // <object> element handler.
    //

    class ObjectElementHandler
      : public ParametrizedElementHandler
    {
      public:
        typedef vector<Object*> ObjectVector;

        explicit ObjectElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);

            clear_keep_memory(m_objects);

            m_name = get_value(attrs, "name");
            m_model = get_value(attrs, "model");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::end_element();

            try
            {
                if (m_model == MeshObjectFactory::get_model())
                {
                    if (m_context.get_options() & ProjectFileReader::OmitReadingMeshFiles)
                    {
                        m_objects.push_back(
                            MeshObjectFactory::create(
                                m_name.c_str(),
                                m_params).release());
                    }
                    else
                    {
                        MeshObjectArray object_array;
                        if (MeshObjectReader::read(
                                m_context.get_project().search_paths(),
                                m_name.c_str(),
                                m_params,
                                object_array))
                            m_objects = array_vector<ObjectVector>(object_array);
                        else m_context.get_event_counters().signal_error();
                    }
                }
                else if (m_model == CurveObjectFactory::get_model())
                {
                    m_objects.push_back(
                        CurveObjectReader::read(
                            m_context.get_project().search_paths(),
                            m_name.c_str(),
                            m_params).release());
                }
                else
                {
                    RENDERER_LOG_ERROR(
                        "while defining object \"%s\": invalid model \"%s\".",
                        m_name.c_str(),
                        m_model.c_str());
                    m_context.get_event_counters().signal_error();
                }
            }
            catch (const ExceptionDictionaryKeyNotFound& e)
            {
                RENDERER_LOG_ERROR(
                    "while defining object \"%s\": required parameter \"%s\" missing.",
                    m_name.c_str(),
                    e.string());
                m_context.get_event_counters().signal_error();
            }
        }

        const ObjectVector& get_objects() const
        {
            return m_objects;
        }

      private:
        ParseContext&   m_context;
        ObjectVector    m_objects;
        string          m_name;
        string          m_model;
    };


    //
    // <assign_material> element handler.
    //

    class AssignMaterialElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit AssignMaterialElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            m_slot = get_value(attrs, "slot");
            m_side = get_value(attrs, "side", "front");
            m_material = get_value(attrs, "material");

            if (m_side != "front" && m_side != "back" && m_side != "both")
            {
                RENDERER_LOG_ERROR(
                    "while assigning material: side must be \"front\", \"back\" or \"both\", got \"%s\".",
                    m_side.c_str());
                m_context.get_event_counters().signal_error();
                m_side = "front";
            }
        }

        const string& get_material_slot() const
        {
            return m_slot;
        }

        const string& get_material_side() const
        {
            return m_side;
        }

        const string& get_material_name() const
        {
            return m_material;
        }

      private:
        ParseContext&           m_context;
        string                  m_slot;
        string                  m_side;
        string                  m_material;
    };


    //
    // <object_instance> element handler.
    //

    class ObjectInstanceElementHandler
      : public TransformSequenceElementHandler<ParametrizedElementHandler>
    {
      public:
        explicit ObjectInstanceElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            Base::start_element(attrs);

            m_object_instance.reset();
            m_front_material_mappings.clear();
            m_back_material_mappings.clear();

            m_name = get_value(attrs, "name");
            m_object = get_value(attrs, "object");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            Base::end_element();

            m_object_instance =
                ObjectInstanceFactory::create(
                    m_name.c_str(),
                    m_params,
                    m_object.c_str(),
                    get_earliest_transform(),
                    m_front_material_mappings,
                    m_back_material_mappings);
        }

        virtual void end_child_element(
            const ProjectElementID          element,
            ElementHandlerType*             handler) APPLESEED_OVERRIDE
        {
            switch (element)
            {
              case ElementAssignMaterial:
                {
                    AssignMaterialElementHandler* assign_mat_handler =
                        static_cast<AssignMaterialElementHandler*>(handler);

                    const string& material_slot = assign_mat_handler->get_material_slot();
                    const string& material_side = assign_mat_handler->get_material_side();
                    const string& material_name = assign_mat_handler->get_material_name();

                    if (material_side == "front")
                        m_front_material_mappings.insert(material_slot, material_name);
                    else if (material_side == "back")
                        m_back_material_mappings.insert(material_slot, material_name);
                    else if (material_side == "both")
                    {
                        m_front_material_mappings.insert(material_slot, material_name);
                        m_back_material_mappings.insert(material_slot, material_name);
                    }
                }
                break;

              default:
                Base::end_child_element(element, handler);
                break;
            }
        }

        auto_release_ptr<ObjectInstance> get_object_instance()
        {
            return m_object_instance;
        }

      private:
        typedef TransformSequenceElementHandler<ParametrizedElementHandler> Base;

        ParseContext&                       m_context;
        auto_release_ptr<ObjectInstance>    m_object_instance;
        StringDictionary                    m_front_material_mappings;
        StringDictionary                    m_back_material_mappings;
        string                              m_name;
        string                              m_object;
    };


    //
    // <assembly_instance> element handler.
    //

    class AssemblyInstanceElementHandler
      : public TransformSequenceElementHandler<ParametrizedElementHandler>
    {
      public:
        explicit AssemblyInstanceElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            Base::start_element(attrs);

            m_assembly_instance.reset();

            m_name = get_value(attrs, "name");
            m_assembly = get_value(attrs, "assembly");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            Base::end_element();

            m_assembly_instance =
                AssemblyInstanceFactory::create(
                    m_name.c_str(),
                    m_params,
                    m_assembly.c_str());

            copy_transform_sequence_to(m_assembly_instance->transform_sequence());
        }

        auto_release_ptr<AssemblyInstance> get_assembly_instance()
        {
            return m_assembly_instance;
        }

      private:
        typedef TransformSequenceElementHandler<ParametrizedElementHandler> Base;

        ParseContext&                       m_context;
        auto_release_ptr<AssemblyInstance>  m_assembly_instance;
        string                              m_name;
        string                              m_assembly;
    };


    //
    // <shader> element handler.
    //

    class ShaderElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit ShaderElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);
            m_type = get_value(attrs, "type");
            m_name = get_value(attrs, "name");
            m_layer = get_value(attrs, "layer");
        }

        const string& type() const
        {
            return m_type;
        }

        const string& name() const
        {
            return m_name;
        }

        const string& layer() const
        {
            return m_layer;
        }

        const ParamArray& params() const
        {
            return m_params;
        }

      private:
        ParseContext&   m_context;
        string          m_type;
        string          m_name;
        string          m_layer;
    };


    //
    // <connect_shaders> element handler.
    //

    class ShaderConnectionElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit ShaderConnectionElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            m_src_layer = get_value(attrs, "src_layer");
            m_src_param = get_value(attrs, "src_param");
            m_dst_layer = get_value(attrs, "dst_layer");
            m_dst_param = get_value(attrs, "dst_param");
        }

        const string& src_layer()
        {
            return m_src_layer;
        }

        const string& src_param()
        {
            return m_src_param;
        }

        const string& dst_layer()
        {
            return m_dst_layer;
        }

        const string& dst_param()
        {
            return m_dst_param;
        }

      private:
        ParseContext&   m_context;
        string          m_src_layer;
        string          m_src_param;
        string          m_dst_layer;
        string          m_dst_param;
    };


    //
    // <shader_group> element handler.
    //

    class ShaderGroupElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit ShaderGroupElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            m_name = get_value(attrs, "name");
            m_shader_group = ShaderGroupFactory::create(m_name.c_str());
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            switch (element)
            {
              case ElementShader:
                {
                    ShaderElementHandler* shader_handler =
                        static_cast<ShaderElementHandler*>(handler);
                    m_shader_group->add_shader(
                        shader_handler->type().c_str(),
                        shader_handler->name().c_str(),
                        shader_handler->layer().c_str(),
                        shader_handler->params());
                }
                break;

              case ElementShaderConnection:
                {
                    ShaderConnectionElementHandler* shader_handler =
                        static_cast<ShaderConnectionElementHandler*>(handler);
                    m_shader_group->add_connection(
                        shader_handler->src_layer().c_str(),
                        shader_handler->src_param().c_str(),
                        shader_handler->dst_layer().c_str(),
                        shader_handler->dst_param().c_str());
                }
                break;
            }
        }

        auto_release_ptr<ShaderGroup> get_shader_group()
        {
            return m_shader_group;
        }

      private:
        ParseContext&                   m_context;
        string                          m_name;
        auto_release_ptr<ShaderGroup>   m_shader_group;
    };


    //
    // Base class for assembly and scene element handlers.
    //

    class BaseGroupElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit BaseGroupElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

      protected:
        ParseContext& m_context;

        template <typename Container, typename Entity>
        void insert(Container& container, auto_release_ptr<Entity> entity)
        {
            if (entity.get() == 0)
                return;

            if (container.get_by_name(entity->get_name()) != 0)
            {
                RENDERER_LOG_ERROR(
                    "an entity with the path \"%s\" already exists.",
                    entity->get_path().c_str());
                m_context.get_event_counters().signal_error();
                return;
            }

            container.insert(entity);
        }
    };


    //
    // <assembly> element handler.
    //

    class AssemblyElementHandler
      : public BaseGroupElementHandler
    {
      public:
        explicit AssemblyElementHandler(ParseContext& context)
          : BaseGroupElementHandler(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);

            m_assembly.reset();

            m_assemblies.clear();
            m_assembly_instances.clear();
            m_bsdfs.clear();
            m_bssrdfs.clear();
            m_colors.clear();
            m_edfs.clear();
            m_lights.clear();
            m_materials.clear();
            m_objects.clear();
            m_object_instances.clear();
            m_shader_groups.clear();
            m_surface_shaders.clear();
            m_textures.clear();
            m_texture_instances.clear();

            m_name = get_value(attrs, "name");
            m_model = get_value(attrs, "model", AssemblyFactory().get_model());
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::end_element();

            const AssemblyFactoryRegistrar factories;
            const IAssemblyFactory *factory = factories.lookup(m_model.c_str());

            if (factory)
            {
                m_assembly = factory->create(m_name.c_str(), m_params);

                m_assembly->assemblies().swap(m_assemblies);
                m_assembly->assembly_instances().swap(m_assembly_instances);
                m_assembly->bsdfs().swap(m_bsdfs);
                m_assembly->bssrdfs().swap(m_bssrdfs);
                m_assembly->colors().swap(m_colors);
                m_assembly->edfs().swap(m_edfs);
                m_assembly->lights().swap(m_lights);
                m_assembly->materials().swap(m_materials);
                m_assembly->objects().swap(m_objects);
                m_assembly->object_instances().swap(m_object_instances);
                m_assembly->shader_groups().swap(m_shader_groups);
                m_assembly->surface_shaders().swap(m_surface_shaders);
                m_assembly->textures().swap(m_textures);
                m_assembly->texture_instances().swap(m_texture_instances);
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "while defining assembly \"%s\": invalid model \"%s\".",
                    m_name.c_str(),
                    m_model.c_str());
                m_context.get_event_counters().signal_error();
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            switch (element)
            {
              case ElementAssembly:
                insert(
                    m_assemblies,
                    static_cast<AssemblyElementHandler*>(handler)->get_assembly());
                break;

              case ElementAssemblyInstance:
                insert(
                    m_assembly_instances,
                    static_cast<AssemblyInstanceElementHandler*>(handler)->get_assembly_instance());
                break;

              case ElementBSDF:
                insert(
                    m_bsdfs,
                    static_cast<BSDFElementHandler*>(handler)->get_entity());
                break;

              case ElementBSSRDF:
                insert(
                    m_bssrdfs,
                    static_cast<BSSRDFElementHandler*>(handler)->get_entity());
                break;

              case ElementColor:
                insert(
                    m_colors,
                    static_cast<ColorElementHandler*>(handler)->get_color_entity());
                break;

              case ElementEDF:
                insert(
                    m_edfs,
                    static_cast<EDFElementHandler*>(handler)->get_entity());
                break;

              case ElementLight:
                insert(
                    m_lights,
                    static_cast<LightElementHandler*>(handler)->get_entity());
                break;

              case ElementMaterial:
                insert(
                    m_materials,
                    static_cast<MaterialElementHandler*>(handler)->get_entity());
                break;

              case ElementObject:
                for (const_each<ObjectElementHandler::ObjectVector> i =
                        static_cast<ObjectElementHandler*>(handler)->get_objects(); i; ++i)
                    insert(m_objects, auto_release_ptr<Object>(*i));
                break;

              case ElementObjectInstance:
                insert(
                    m_object_instances,
                    static_cast<ObjectInstanceElementHandler*>(handler)->get_object_instance());
                break;

              case ElementShaderGroup:
                insert(
                    m_shader_groups,
                    static_cast<ShaderGroupElementHandler*>(handler)->get_shader_group());
                break;

              case ElementSurfaceShader:
                insert(
                    m_surface_shaders,
                    static_cast<SurfaceShaderElementHandler*>(handler)->get_entity());
                break;

              case ElementTexture:
                insert(
                    m_textures,
                    static_cast<TextureElementHandler*>(handler)->get_texture());
                break;

              case ElementTextureInstance:
                insert(
                    m_texture_instances,
                    static_cast<TextureInstanceElementHandler*>(handler)->get_texture_instance());
                break;

              default:
                ParametrizedElementHandler::end_child_element(element, handler);
                break;
            }
        }

        auto_release_ptr<Assembly> get_assembly()
        {
            return m_assembly;
        }

      private:
        auto_release_ptr<Assembly>  m_assembly;
        string                      m_name;
        string                      m_model;
        AssemblyContainer           m_assemblies;
        AssemblyInstanceContainer   m_assembly_instances;
        BSDFContainer               m_bsdfs;
        BSSRDFContainer             m_bssrdfs;
        ColorContainer              m_colors;
        EDFContainer                m_edfs;
        LightContainer              m_lights;
        MaterialContainer           m_materials;
        ObjectContainer             m_objects;
        ObjectInstanceContainer     m_object_instances;
        ShaderGroupContainer        m_shader_groups;
        SurfaceShaderContainer      m_surface_shaders;
        TextureContainer            m_textures;
        TextureInstanceContainer    m_texture_instances;
    };


    //
    // <scene> element handler.
    //

    class SceneElementHandler
      : public BaseGroupElementHandler
    {
      public:
        explicit SceneElementHandler(ParseContext& context)
          : BaseGroupElementHandler(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);

            m_scene = SceneFactory::create();
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::end_element();

            m_scene->get_parameters() = m_params;

            const GAABB3 scene_bbox = m_scene->compute_bbox();
            const Vector3d scene_center(scene_bbox.center());

            RENDERER_LOG_INFO(
                "scene bounding box: (%f, %f, %f)-(%f, %f, %f).\n"
                "scene bounding sphere: center (%f, %f, %f), diameter %f.",
                scene_bbox.min[0], scene_bbox.min[1], scene_bbox.min[2],
                scene_bbox.max[0], scene_bbox.max[1], scene_bbox.max[2],
                scene_center[0], scene_center[1], scene_center[2],
                scene_bbox.diameter());
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            assert(m_scene.get());

            switch (element)
            {
              case ElementAssembly:
                insert(
                    m_scene->assemblies(),
                    static_cast<AssemblyElementHandler*>(handler)->get_assembly());
                break;

              case ElementAssemblyInstance:
                insert(
                    m_scene->assembly_instances(),
                    static_cast<AssemblyInstanceElementHandler*>(handler)->get_assembly_instance());
                break;

              case ElementColor:
                insert(
                    m_scene->colors(),
                    static_cast<ColorElementHandler*>(handler)->get_color_entity());
                break;

              case ElementCamera:
                {
                    auto_release_ptr<Camera> camera =
                        static_cast<CameraElementHandler*>(handler)->get_entity();
                    if (camera.get())
                        m_scene->cameras().insert(camera);
                }
                break;

              case ElementEnvironment:
                {
                    auto_release_ptr<Environment> environment =
                        static_cast<EnvironmentElementHandler*>(handler)->get_environment();
                    if (environment.get())
                    {
                        if (m_scene->get_environment())
                        {
                            RENDERER_LOG_ERROR("cannot define multiple environments.");
                            m_context.get_event_counters().signal_error();
                        }
                        m_scene->set_environment(environment);
                    }
                }
                break;

              case ElementEnvironmentEDF:
                insert(
                    m_scene->environment_edfs(),
                    static_cast<EnvironmentEDFElementHandler*>(handler)->get_entity());
                break;

              case ElementEnvironmentShader:
                insert(
                    m_scene->environment_shaders(),
                    static_cast<EnvironmentShaderElementHandler*>(handler)->get_entity());
                break;

              case ElementTexture:
                insert(
                    m_scene->textures(),
                    static_cast<TextureElementHandler*>(handler)->get_texture());
                break;

              case ElementTextureInstance:
                insert(
                    m_scene->texture_instances(),
                    static_cast<TextureInstanceElementHandler*>(handler)->get_texture_instance());
                break;

              case ElementShaderGroup:
                insert(
                    m_scene->shader_groups(),
                    static_cast<ShaderGroupElementHandler*>(handler)->get_shader_group());
                break;

              default:
                ParametrizedElementHandler::end_child_element(element, handler);
                break;
            }
        }

        auto_release_ptr<Scene> get_scene()
        {
            return m_scene;
        }

      private:
        auto_release_ptr<Scene> m_scene;
    };


    //
    // <render_layer_assignment> element handler.
    //

    class RenderLayerAssignmentElementHandler
      : public EntityElementHandler<
                   RenderLayerRule,
                   RenderLayerRuleFactoryRegistrar,
                   ParametrizedElementHandler>
    {
      public:
        explicit RenderLayerAssignmentElementHandler(ParseContext& context)
          : EntityElementHandler<
                RenderLayerRule,
                RenderLayerRuleFactoryRegistrar,
                ParametrizedElementHandler>("render layer assignment rule", context)
        {
        }
    };


    //
    // <rules> element handler.
    //

    class RulesElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit RulesElementHandler(ParseContext& context)
          : m_context(context)
          , m_project(0)
        {
        }

        void set_project(Project* project)
        {
            m_project = project;
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            assert(m_project);

            switch (element)
            {
              case ElementRenderLayerAssignment:
                {
                    RenderLayerAssignmentElementHandler* assignment_handler =
                        static_cast<RenderLayerAssignmentElementHandler*>(handler);
                    m_project->add_render_layer_rule(assignment_handler->get_entity());
                }
                break;

              assert_otherwise;
            }
        }

      private:
        ParseContext&   m_context;
        Project*        m_project;
    };


    //
    // <aov> element handler.
    //

    class AOVElementHandler
      : public EntityElementHandler<
                   AOV,
                   AOVFactoryRegistrar,
                   ParametrizedElementHandler>
    {
      public:
        explicit AOVElementHandler(ParseContext& context)
          : EntityElementHandler<
                AOV,
                AOVFactoryRegistrar,
                ParametrizedElementHandler>("AOV", context)
        {
        }
    };


    //
    // <aovs> element handler.
    //

    class AOVsElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit AOVsElementHandler(ParseContext& context)
          : m_context(context)
          , m_aovs(0)
        {
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            assert(m_aovs);

            switch (element)
            {
              case ElementAOV:
                {
                    auto_release_ptr<AOV> aov(
                        static_cast<AOVElementHandler*>(handler)->get_entity());

                    if (aov.get() == 0)
                        return;

                    if (m_aovs->get_by_name(aov->get_name()) != 0)
                    {
                        RENDERER_LOG_ERROR(
                            "an aov with the path \"%s\" already exists.",
                            aov->get_path().c_str());
                        m_context.get_event_counters().signal_error();
                        return;
                    }

                    m_aovs->insert(aov);
                }
                break;

              assert_otherwise;
            }
        }

        void set_aov_container(AOVContainer* aovs)
        {
            m_aovs = aovs;
        }

      private:
        ParseContext&   m_context;
        AOVContainer*   m_aovs;
    };


    //
    // <frame> element handler.
    //

    class FrameElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit FrameElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);

            m_frame.reset();

            m_name = get_value(attrs, "name");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::end_element();

            m_frame = FrameFactory::create(m_name.c_str(), m_params);
            m_frame->transfer_aovs(m_aovs);
        }

        virtual void start_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            switch (element)
            {
              case ElementAOVs:
                {
                    AOVsElementHandler* aovs_handler =
                        static_cast<AOVsElementHandler*>(handler);
                    aovs_handler->set_aov_container(&m_aovs);
                }
                break;

              default:
                ParametrizedElementHandler::start_child_element(element, handler);
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            if (element == ElementAOVs)
                return;

            ParametrizedElementHandler::end_child_element(element, handler);
        }

        auto_release_ptr<Frame> get_frame()
        {
            return m_frame;
        }

      private:
        ParseContext&           m_context;
        auto_release_ptr<Frame> m_frame;
        AOVContainer            m_aovs;
        string                  m_name;
    };


    //
    // <output> element handler.
    //

    class OutputElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit OutputElementHandler(ParseContext& context)
          : m_context(context)
          , m_project(0)
        {
        }

        void set_project(Project* project)
        {
            m_project = project;
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            assert(m_project);

            switch (element)
            {
              case ElementFrame:
                {
                    FrameElementHandler* frame_handler =
                        static_cast<FrameElementHandler*>(handler);
                    m_project->set_frame(frame_handler->get_frame());
                }
                break;

              assert_otherwise;
            }
        }

      private:
        ParseContext&   m_context;
        Project*        m_project;
    };


    //
    // <configuration> element handler.
    //

    class ConfigurationElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit ConfigurationElementHandler(ParseContext& context)
          : m_context(context)
          , m_project(0)
        {
        }

        void set_project(Project* project)
        {
            m_project = project;
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);

            m_configuration.reset();

            m_name = get_value(attrs, "name");
            m_base_name = get_value(attrs, "base");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::end_element();

            m_configuration =
                ConfigurationFactory::create(
                    m_name.c_str(),
                    m_params);

            // Handle configuration inheritance.
            if (!m_base_name.empty())
            {
                assert(m_project);
                const Configuration* base =
                    m_project->configurations().get_by_name(m_base_name.c_str());

                if (base)
                {
                    m_configuration->set_base(base);
                }
                else
                {
                    RENDERER_LOG_ERROR(
                        "while defining configuration \"%s\": the configuration \"%s\" does not exist.",
                        m_configuration->get_path().c_str(),
                        m_base_name.c_str());
                    m_context.get_event_counters().signal_error();
                }
            }
        }

        auto_release_ptr<Configuration> get_configuration()
        {
            return m_configuration;
        }

      private:
        ParseContext&                   m_context;
        Project*                        m_project;
        auto_release_ptr<Configuration> m_configuration;
        string                          m_name;
        string                          m_base_name;
    };


    //
    // <configurations> element handler.
    //

    class ConfigurationsElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit ConfigurationsElementHandler(ParseContext& context)
          : m_context(context)
          , m_project(0)
        {
        }

        void set_project(Project* project)
        {
            m_project = project;
        }

        virtual void start_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            assert(m_project);

            switch (element)
            {
              case ElementConfiguration:
                {
                    ConfigurationElementHandler* config_handler =
                        static_cast<ConfigurationElementHandler*>(handler);
                    config_handler->set_project(m_project);
                }
                break;

              assert_otherwise;
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            assert(m_project);

            switch (element)
            {
              case ElementConfiguration:
                {
                    // Insert the configuration directly into the project.
                    ConfigurationElementHandler* config_handler =
                        static_cast<ConfigurationElementHandler*>(handler);
                    m_project->configurations().insert(
                        config_handler->get_configuration());
                }
                break;

              assert_otherwise;
            }
        }

      private:
        ParseContext&   m_context;
        Project*        m_project;
    };


    //
    // <search_path> element handler.
    //

    class SearchPathElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit SearchPathElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void characters(
            const XMLCh* const  chars,
            const XMLSize_t     length) APPLESEED_OVERRIDE
        {
            m_path = trim_both(transcode(chars));
        }

        const string& get_path() const
        {
            return m_path;
        }

      private:
        ParseContext&   m_context;
        string          m_path;
    };


    //
    // <search_paths> element handler.
    //

    class SearchPathsElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit SearchPathsElementHandler(ParseContext& context)
          : m_context(context)
          , m_project(0)
        {
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            assert(m_project);

            switch (element)
            {
              case ElementSearchPath:
                {
                    // Skip search paths if asked to do so.
                    if (m_context.get_options() & ProjectFileReader::OmitSearchPaths)
                        return;

                    SearchPathElementHandler* path_handler =
                        static_cast<SearchPathElementHandler*>(handler);
                    const string& path = path_handler->get_path();
                    if (!path.empty())
                        m_project->search_paths().push_back(path);
                }
                break;

              assert_otherwise;
            }
        }

        void set_project(Project* project)
        {
            m_project = project;
        }

      private:
        ParseContext&   m_context;
        Project*        m_project;
    };


    //
    // <display> element handler.
    //

    class DisplayElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit DisplayElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::start_element(attrs);
            m_name = get_value(attrs, "name");
        }

        virtual void end_element() APPLESEED_OVERRIDE
        {
            ParametrizedElementHandler::end_element();
            m_project->set_display(DisplayFactory::create(m_name.c_str(), m_params));
        }

        void set_project(Project* project)
        {
            m_project = project;
        }

      private:
        ParseContext&   m_context;
        string          m_name;
        Project*        m_project;
    };


    //
    // <project> element handler.
    //

    class ProjectElementHandler
      : public ElementHandlerBase
    {
      public:
        ProjectElementHandler(ParseContext& context, Project* project)
          : m_context(context)
          , m_project(project)
        {
        }

        virtual void start_element(const Attributes& attrs) APPLESEED_OVERRIDE
        {
            ElementHandlerBase::start_element(attrs);

            const size_t format_revision =
                from_string<size_t>(
                    ElementHandlerBase::get_value(attrs, "format_revision", "2"));

            if (format_revision > ProjectFormatRevision)
            {
                RENDERER_LOG_WARNING(
                    "this project was created with a newer version of appleseed; it may fail to load with this version.");
                m_context.get_event_counters().signal_warning();
            }

            m_project->set_format_revision(format_revision);
        }

        virtual void start_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            assert(m_project);

            switch (element)
            {
              case ElementConfigurations:
                {
                    ConfigurationsElementHandler* configs_handler =
                        static_cast<ConfigurationsElementHandler*>(handler);
                    configs_handler->set_project(m_project);
                }
                break;

              case ElementOutput:
                {
                    OutputElementHandler* output_handler =
                        static_cast<OutputElementHandler*>(handler);
                    output_handler->set_project(m_project);
                }
                break;

              case ElementRules:
                {
                    RulesElementHandler* rules_handler =
                        static_cast<RulesElementHandler*>(handler);
                    rules_handler->set_project(m_project);
                }
                break;

              case ElementScene:
                break;

              case ElementSearchPaths:
                {
                    SearchPathsElementHandler* paths_handler =
                        static_cast<SearchPathsElementHandler*>(handler);
                    paths_handler->set_project(m_project);
                }
                break;

              case ElementDisplay:
                {
                    DisplayElementHandler* display_handler =
                        static_cast<DisplayElementHandler*>(handler);
                    display_handler->set_project(m_project);
                }
                break;

              assert_otherwise;
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) APPLESEED_OVERRIDE
        {
            assert(m_project);

            switch (element)
            {
              case ElementConfigurations:
                // Nothing to do, configurations were directly inserted into the project.
                break;

              case ElementOutput:
                // Nothing to do, frames were directly inserted into the project.
                break;

              case ElementRules:
                // Nothing to do, rules were directly inserted into the project.
                break;

              case ElementScene:
                {
                    SceneElementHandler* scene_handler =
                        static_cast<SceneElementHandler*>(handler);
                    auto_release_ptr<Scene> scene = scene_handler->get_scene();
                    if (scene.get())
                        m_project->set_scene(scene);
                }
                break;

              case ElementSearchPaths:
                // Nothing to do, search paths were directly inserted into the project.
                break;

              case ElementDisplay:
                // Nothing to do, display was directly inserted into the project.
                break;

              assert_otherwise;
            }
        }

      private:
        ParseContext&   m_context;
        Project*        m_project;
    };


    //
    // Content handler.
    //

    class ContentHandler
      : public SAX2ContentHandler<ProjectElementID>
    {
      public:
        ContentHandler(Project* project, ParseContext& context)
          : m_context(context)
        {
            register_factory_helper<ValuesElementHandler>("alpha", ElementAlpha);
            register_factory_helper<AOVElementHandler>("aov", ElementAOV);
            register_factory_helper<AOVsElementHandler>("aovs", ElementAOVs);
            register_factory_helper<AssemblyElementHandler>("assembly", ElementAssembly);
            register_factory_helper<AssemblyInstanceElementHandler>("assembly_instance", ElementAssemblyInstance);
            register_factory_helper<AssignMaterialElementHandler>("assign_material", ElementAssignMaterial);
            register_factory_helper<BSDFElementHandler>("bsdf", ElementBSDF);
            register_factory_helper<BSSRDFElementHandler>("bssrdf", ElementBSSRDF);
            register_factory_helper<CameraElementHandler>("camera", ElementCamera);
            register_factory_helper<ColorElementHandler>("color", ElementColor);
            register_factory_helper<ConfigurationElementHandler>("configuration", ElementConfiguration);
            register_factory_helper<ConfigurationsElementHandler>("configurations", ElementConfigurations);
            register_factory_helper<ShaderConnectionElementHandler>("connect_shaders", ElementShaderConnection);
            register_factory_helper<DisplayElementHandler>("display", ElementDisplay);
            register_factory_helper<EDFElementHandler>("edf", ElementEDF);
            register_factory_helper<EnvironmentElementHandler>("environment", ElementEnvironment);
            register_factory_helper<EnvironmentEDFElementHandler>("environment_edf", ElementEnvironmentEDF);
            register_factory_helper<EnvironmentShaderElementHandler>("environment_shader", ElementEnvironmentShader);
            register_factory_helper<FrameElementHandler>("frame", ElementFrame);
            register_factory_helper<LightElementHandler>("light", ElementLight);
            register_factory_helper<LookAtElementHandler>("look_at", ElementLookAt);
            register_factory_helper<MaterialElementHandler>("material", ElementMaterial);
            register_factory_helper<MatrixElementHandler>("matrix", ElementMatrix);
            register_factory_helper<ObjectElementHandler>("object", ElementObject);
            register_factory_helper<ObjectInstanceElementHandler>("object_instance", ElementObjectInstance);
            register_factory_helper<OutputElementHandler>("output", ElementOutput);
            register_factory_helper<ParameterElementHandler>("parameter", ElementParameter);
            register_factory_helper<ParametersElementHandler>("parameters", ElementParameters);
            register_factory_helper<RenderLayerAssignmentElementHandler>("render_layer_assignment", ElementRenderLayerAssignment);
            register_factory_helper<RotationElementHandler>("rotation", ElementRotation);
            register_factory_helper<RulesElementHandler>("rules", ElementRules);
            register_factory_helper<ScalingElementHandler>("scaling", ElementScaling);
            register_factory_helper<SceneElementHandler>("scene", ElementScene);
            register_factory_helper<SearchPathElementHandler>("search_path", ElementSearchPath);
            register_factory_helper<SearchPathsElementHandler>("search_paths", ElementSearchPaths);
            register_factory_helper<ShaderElementHandler>("shader", ElementShader);
            register_factory_helper<ShaderGroupElementHandler>("shader_group", ElementShaderGroup);
            register_factory_helper<SurfaceShaderElementHandler>("surface_shader", ElementSurfaceShader);
            register_factory_helper<TextureElementHandler>("texture", ElementTexture);
            register_factory_helper<TextureInstanceElementHandler>("texture_instance", ElementTextureInstance);
            register_factory_helper<TransformElementHandler>("transform", ElementTransform);
            register_factory_helper<TranslationElementHandler>("translation", ElementTranslation);
            register_factory_helper<ValuesElementHandler>("values", ElementValues);

            auto_ptr<IElementHandlerFactory<ProjectElementID> > factory(
                new ProjectElementHandlerFactory(m_context, project));

            register_factory("project", ElementProject, factory);
        }

      private:
        ParseContext& m_context;

        struct ProjectElementHandlerFactory
          : public IElementHandlerFactory<ProjectElementID>
        {
            ParseContext&   m_context;
            Project*        m_project;

            ProjectElementHandlerFactory(ParseContext& context, Project* project)
              : m_context(context)
              , m_project(project)
            {
            }

            virtual auto_ptr<ElementHandlerType> create() APPLESEED_OVERRIDE
            {
                return auto_ptr<ElementHandlerType>(
                    new ProjectElementHandler(m_context, m_project));
            }
        };

        template <typename ElementHandler>
        struct GenericElementHandlerFactory
          : public IElementHandlerFactory<ProjectElementID>
        {
            ParseContext&   m_context;

            explicit GenericElementHandlerFactory(ParseContext& context)
              : m_context(context)
            {
            }

            virtual auto_ptr<ElementHandlerType> create() APPLESEED_OVERRIDE
            {
                return auto_ptr<ElementHandlerType>(new ElementHandler(m_context));
            }
        };

        template <typename ElementHandler>
        void register_factory_helper(const string& name, const ProjectElementID id)
        {
            auto_ptr<IElementHandlerFactory<ProjectElementID> > factory(
                new GenericElementHandlerFactory<ElementHandler>(m_context));

            register_factory(name, id, factory);
        }
    };
}

namespace
{
    bool is_builtin_project(const string& project_filepath, string& project_name)
    {
        const string BuiltInPrefix = "builtin:";

        if (starts_with(project_filepath, BuiltInPrefix))
        {
            project_name = project_filepath.substr(BuiltInPrefix.size());
            return true;
        }

        return false;
    }
}

auto_release_ptr<Project> ProjectFileReader::read(
    const char*             project_filepath,
    const char*             schema_filepath,
    const int               options)
{
    assert(project_filepath);

    // Handle built-in projects.
    string project_name;
    if (is_builtin_project(project_filepath, project_name))
        return load_builtin(project_name.c_str());

    // Handle packed projects
    string actual_project_filepath;
    if (is_zip_file(project_filepath))
    {
        const vector<string> appleseed_files = get_filenames_with_extension_from_zip(project_filepath, ".appleseed");

        if (appleseed_files.size() != 1)
        {
            RENDERER_LOG_ERROR(
                "%s looks like a packed project file, but it should contain a single *.appleseed file in order to be valid.",
                project_filepath);
            return auto_release_ptr<Project>(0);
        }

        const bf::path project_path(project_filepath);
        const string unpacked_project_directory =
            (project_path.parent_path() / project_path.stem()).string() + ".unpacked";

        RENDERER_LOG_INFO(
            "%s appears to be a packed project; unpacking to %s...,",
            project_filepath,
            unpacked_project_directory.c_str());

        if (bf::exists(bf::path(unpacked_project_directory)))
            bf::remove_all(bf::path(unpacked_project_directory));

        unzip(project_filepath, unpacked_project_directory);

        const string project_name = appleseed_files[0];
        actual_project_filepath = (bf::path(unpacked_project_directory) / bf::path(project_name)).string().c_str();
        project_filepath = actual_project_filepath.data();
    }

    XercesCContext xerces_context(global_logger());
    if (!xerces_context.is_initialized())
        return auto_release_ptr<Project>(0);

    if ((options & OmitProjectSchemaValidation) == false && schema_filepath == 0)
    {
        RENDERER_LOG_ERROR(
            "project schema validation enabled, but no schema filepath provided.");
        return auto_release_ptr<Project>(0);
    }

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    EventCounters event_counters;
    auto_release_ptr<Project> project(
        load_project_file(
            project_filepath,
            schema_filepath,
            options,
            event_counters));

    if (project.get())
        postprocess_project(project.ref(), event_counters, options);

    stopwatch.measure();

    print_loading_results(
        project_filepath,
        false,
        event_counters,
        stopwatch.get_seconds());

    return event_counters.has_errors() ? auto_release_ptr<Project>(0) : project;
}

auto_release_ptr<Assembly> ProjectFileReader::read_archive(
    const char*             archive_filepath,
    const char*             schema_filepath,
    const SearchPaths&      search_paths,
    const int               options)
{
    assert(archive_filepath);

    XercesCContext xerces_context(global_logger());
    if (!xerces_context.is_initialized())
        return auto_release_ptr<Assembly>(0);

    if ((options & OmitProjectSchemaValidation) == false && schema_filepath == 0)
    {
        RENDERER_LOG_ERROR(
            "archive schema validation enabled, but no schema filepath provided.");
        return auto_release_ptr<Assembly>(0);
    }

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    EventCounters event_counters;
    auto_release_ptr<Project> project(
        load_project_file(
            archive_filepath,
            schema_filepath,
            options | OmitSearchPaths,
            event_counters,
            &search_paths));

    if (project.get())
    {
        if (!event_counters.has_errors() &&
            !(options & OmitProjectFileUpdate) &&
            project->get_format_revision() < ProjectFormatRevision)
            upgrade_project(*project, event_counters);
    }

    stopwatch.measure();

    print_loading_results(
        archive_filepath,
        false,
        event_counters,
        stopwatch.get_seconds());

    if (event_counters.has_errors())
        return auto_release_ptr<Assembly>(0);

    if (project->get_scene())
    {
        if (Assembly* assembly = project->get_scene()->assemblies().get_by_name("assembly"))
        {
            return auto_release_ptr<Assembly>(
                project->get_scene()->assemblies().remove(assembly));
        }
    }

    return auto_release_ptr<Assembly>(0);
}

auto_release_ptr<Project> ProjectFileReader::load_builtin(
    const char*             project_name)
{
    assert(project_name);

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    EventCounters event_counters;
    auto_release_ptr<Project> project(
        construct_builtin_project(project_name, event_counters));

    if (project.get())
        postprocess_project(project.ref(), event_counters);

    stopwatch.measure();

    print_loading_results(
        project_name,
        true,
        event_counters,
        stopwatch.get_seconds());

    return event_counters.has_errors() ? auto_release_ptr<Project>(0) : project;
}

auto_release_ptr<Project> ProjectFileReader::load_project_file(
    const char*                     project_filepath,
    const char*                     schema_filepath,
    const int                       options,
    EventCounters&                  event_counters,
    const foundation::SearchPaths*  search_paths) const
{
    // Create an empty project.
    auto_release_ptr<Project> project(ProjectFactory::create(project_filepath));
    project->set_path(project_filepath);

    if ((options & OmitSearchPaths) == false)
    {
        project->search_paths().set_root_path(
            bf::absolute(project_filepath).parent_path().string());
    }
    else
    {
        assert(search_paths);
        project->search_paths() = *search_paths;
    }

    // Create the error handler.
    auto_ptr<ErrorLogger> error_handler(
        new ErrorLoggerAndCounter(
            project_filepath,
            event_counters));

    // Create the content handler.
    ParseContext context(project.ref(), options, event_counters);
    auto_ptr<ContentHandler> content_handler(
        new ContentHandler(
            project.get(),
            context));

    // Create the parser.
    auto_ptr<SAX2XMLReader> parser(XMLReaderFactory::createXMLReader());
    parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);         // perform namespace processing

    if ((options & OmitProjectSchemaValidation) == false)
    {
        assert(schema_filepath);
        parser->setFeature(XMLUni::fgSAX2CoreValidation, true);     // report all validation errors
        parser->setFeature(XMLUni::fgXercesSchema, true);           // enable the parser's schema support
        parser->setProperty(
            XMLUni::fgXercesSchemaExternalNoNameSpaceSchemaLocation,
            const_cast<void*>(
                static_cast<const void*>(
                    transcode(schema_filepath).c_str())));
    }
    else
    {
        parser->setFeature(XMLUni::fgSAX2CoreValidation, false); // ignore all validation errors
        parser->setFeature(XMLUni::fgXercesSchema, false);       // disable the parser's schema support
    }

    parser->setErrorHandler(error_handler.get());
    parser->setContentHandler(content_handler.get());

    // Load the project file.
    RENDERER_LOG_INFO("loading project file %s...", project_filepath);
    try
    {
        parser->parse(project_filepath);
    }
    catch (const XMLException&)
    {
        return auto_release_ptr<Project>(0);
    }
    catch (const SAXParseException&)
    {
        return auto_release_ptr<Project>(0);
    }

    // Report a failure in case of warnings or errors.
    if (error_handler->get_warning_count() > 0 ||
        error_handler->get_error_count() > 0 ||
        error_handler->get_fatal_error_count() > 0)
        return auto_release_ptr<Project>(0);

    return project;
}

auto_release_ptr<Project> ProjectFileReader::construct_builtin_project(
    const char*             project_name,
    EventCounters&          event_counters) const
{
    if (!strcmp(project_name, "cornell_box"))
    {
        return CornellBoxProjectFactory::create();
    }
    else if (!strcmp(project_name, "default"))
    {
        return DefaultProjectFactory::create();
    }
    else
    {
        RENDERER_LOG_ERROR("unknown built-in project %s.", project_name);
        event_counters.signal_error();
        return auto_release_ptr<Project>(0);
    }
}

void ProjectFileReader::postprocess_project(
    Project&                project,
    EventCounters&          event_counters,
    const int               options) const
{
    if (!event_counters.has_errors())
        validate_project(project, event_counters);

    if (!event_counters.has_errors())
        complete_project(project, event_counters);

    if (!event_counters.has_errors() &&
        !(options & OmitProjectFileUpdate) &&
        project.get_format_revision() < ProjectFormatRevision)
        upgrade_project(project, event_counters);
}

void ProjectFileReader::validate_project(
    const Project&          project,
    EventCounters&          event_counters) const
{
    // Make sure the project contains a scene.
    if (project.get_scene())
    {
        // Make sure the scene contains at least one camera.
        if (project.get_scene()->cameras().empty())
        {
            RENDERER_LOG_ERROR("the scene does not define any camera.");
            event_counters.signal_error();
        }
    }
    else
    {
        RENDERER_LOG_ERROR("the project does not define a scene.");
        event_counters.signal_error();
    }

    // Make sure the project contains at least one output frame.
    if (project.get_frame() == 0)
    {
        RENDERER_LOG_ERROR("the project does not define any frame.");
        event_counters.signal_error();
    }

    // Make sure the project contains the required configurations.
    if (project.configurations().get_by_name("final") == 0)
    {
        RENDERER_LOG_ERROR("the project must define a \"final\" configuration.");
        event_counters.signal_error();
    }
    if (project.configurations().get_by_name("interactive") == 0)
    {
        RENDERER_LOG_ERROR("the project must define an \"interactive\" configuration.");
        event_counters.signal_error();
    }
}

void ProjectFileReader::complete_project(
    Project&                project,
    EventCounters&          event_counters) const
{
    // Add a default environment if the project doesn't define any.
    if (project.get_scene()->get_environment() == 0)
    {
        auto_release_ptr<Environment> environment(
            EnvironmentFactory::create("environment", ParamArray()));
        project.get_scene()->set_environment(environment);
    }
}

void ProjectFileReader::upgrade_project(
    Project&                project,
    EventCounters&          event_counters) const
{
    ProjectFileUpdater updater;
    updater.update(project, event_counters);
}

void ProjectFileReader::print_loading_results(
    const char*             project_name,
    const bool              builtin_project,
    const EventCounters&    event_counters,
    const double            loading_time) const
{
    const size_t warning_count = event_counters.get_warning_count();
    const size_t error_count = event_counters.get_error_count();

    const LogMessage::Category log_category =
        error_count > 0 ? LogMessage::Error :
        warning_count > 0 ? LogMessage::Warning :
        LogMessage::Info;

    if (error_count > 0)
    {
        RENDERER_LOG(
            log_category,
            "failed to load %s %s (" FMT_SIZE_T " %s, " FMT_SIZE_T " %s).",
            builtin_project ? "built-in project" : "project file",
            project_name,
            error_count,
            plural(error_count, "error").c_str(),
            warning_count,
            plural(warning_count, "warning").c_str());
    }
    else
    {
        RENDERER_LOG(
            log_category,
            "successfully loaded %s %s in %s (" FMT_SIZE_T " %s, " FMT_SIZE_T " %s).",
            builtin_project ? "built-in project" : "project file",
            project_name,
            pretty_time(loading_time).c_str(),
            error_count,
            plural(error_count, "error").c_str(),
            warning_count,
            plural(warning_count, "warning").c_str());
    }
}

}   // namespace renderer
