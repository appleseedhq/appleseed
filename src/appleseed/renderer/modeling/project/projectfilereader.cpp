
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdffactoryregistrar.h"
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/camera/camerafactoryregistrar.h"
#include "renderer/modeling/camera/icamerafactory.h"
#include "renderer/modeling/color/colorentity.h"
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
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/meshobjectreader.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/configurationcontainer.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project-builtin/cornellboxproject.h"
#include "renderer/modeling/project-builtin/defaultproject.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/surfaceshader/isurfaceshaderfactory.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/surfaceshader/surfaceshaderfactoryregistrar.h"
#include "renderer/modeling/texture/itexturefactory.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/texture/texturefactoryregistrar.h"
#include "renderer/utility/bbox.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"
#include "foundation/utility/xercesc.h"

// Xerces-C++ headers.
#include "xercesc/sax2/Attributes.hpp"
#include "xercesc/sax2/SAX2XMLReader.hpp"
#include "xercesc/sax2/XMLReaderFactory.hpp"
#include "xercesc/util/PlatformUtils.hpp"
#include "xercesc/util/XMLException.hpp"

// boost headers.
#include "boost/filesystem/path.hpp"

// Standard headers.
#include <cstring>
#include <exception>
#include <map>
#include <sstream>
#include <utility>
#include <vector>

using namespace foundation;
using namespace std;
using namespace xercesc;

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
            const string&   input_filename,
            EventCounters&  event_counters)
          : ErrorLogger(global_logger(), input_filename)
          , m_event_counters(event_counters)
        {
        }

        virtual void resetErrors() override
        {
            m_event_counters.clear();
        }

        virtual void warning(const SAXParseException& e) override
        {
            ErrorLogger::warning(e);
            m_event_counters.signal_warning();
        }

        virtual void error(const SAXParseException& e) override
        {
            ErrorLogger::error(e);
            m_event_counters.signal_error();
            throw e;    // terminate parsing
        }

        virtual void fatalError(const SAXParseException& e) override
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
            EventCounters&  event_counters)
          : m_project(project)
          , m_event_counters(event_counters)
        {
            // Extract the root path of the project.
            const boost::filesystem::path project_root_path =
                boost::filesystem::path(project.get_path()).parent_path();

            // Add the root path of the project to the search path collection.
            m_project.get_search_paths().push_back(project_root_path.string());
        }

        Project& get_project()
        {
            return m_project;
        }

        EventCounters& get_event_counters()
        {
            return m_event_counters;
        }

      private:
        Project&            m_project;
        EventCounters&      m_event_counters;
    };


    //
    // Utility functions.
    //

    double get_scalar(
        const string&       text,
        ParseContext&       context)
    {
        try
        {
            return from_string<double>(text);
        }
        catch (const ExceptionStringConversionError&)
        {
            RENDERER_LOG_ERROR("expected scalar value, got \"%s\".", text.c_str());
            context.get_event_counters().signal_error();
            return 0.0;
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
        catch (const ExceptionDictionaryItemNotFound& e)
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
        ElementAssembly,
        ElementAssemblyInstance,
        ElementAssignMaterial,
        ElementBSDF,
        ElementCamera,
        ElementColor,
        ElementConfiguration,
        ElementConfigurations,
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
        ElementRotation,
        ElementScaling,
        ElementScene,
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

        virtual void start_element(const Attributes& attrs) override
        {
            // We need to fully qualify the call to get_value().
            m_name = ElementHandlerBase::get_value(attrs, "name");
            m_value = ElementHandlerBase::get_value(attrs, "value");
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
        virtual void start_element(const Attributes& attrs) override;

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) override;

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

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);
            m_name = get_value(attrs, "name");
            m_params.clear();
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

        virtual void start_element(const Attributes& attrs) override
        {
            m_matrix = Matrix4d::identity();

            const Vector3d origin = get_vector3(get_value(attrs, "origin"), m_context);
            const Vector3d target = get_vector3(get_value(attrs, "target"), m_context);
            const Vector3d up = get_vector3(get_value(attrs, "up"), m_context);

            if (norm(origin - target) > 0.0 &&
                norm(up) > 0.0 &&
                norm(cross(up, origin - target)) > 0.0)
            {
                m_matrix = Matrix4d::lookat(origin, target, normalize(up));
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

        virtual void start_element(const Attributes& attrs) override
        {
            m_matrix = Matrix4d::identity();
            clear_keep_memory(m_values);
        }

        virtual void end_element() override
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
            const XMLSize_t     length) override
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

        virtual void start_element(const Attributes& attrs) override
        {
            m_matrix = Matrix4d::identity();

            const Vector3d axis = get_vector3(get_value(attrs, "axis"), m_context);
            const double angle = get_scalar(get_value(attrs, "angle"), m_context);

            if (norm(axis) > 0.0)
            {
                m_matrix = Matrix4d::rotation(normalize(axis), deg_to_rad(angle));
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

        virtual void start_element(const Attributes& attrs) override
        {
            const Vector3d value = get_vector3(get_value(attrs, "value"), m_context);
            m_matrix = Matrix4d::scaling(value);
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

        virtual void start_element(const Attributes& attrs) override
        {
            const Vector3d value = get_vector3(get_value(attrs, "value"), m_context);
            m_matrix = Matrix4d::translation(value);
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

        virtual void start_element(const Attributes& attrs) override
        {
            m_time = get_scalar(get_value(attrs, "time", "0.0"), m_context);
            m_matrix = Matrix4d::identity();
        }

        virtual void end_element() override
        {
            try
            {
                m_transform = Transformd(m_matrix);
            }
            catch (const ExceptionSingularMatrix&)
            {
                RENDERER_LOG_ERROR("while defining <transform> element: the transformation matrix is singular.");
                m_context.get_event_counters().signal_error();
                m_transform = Transformd(Matrix4d::identity());
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) override
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

        double get_time() const
        {
            return m_time;
        }

        const Transformd& get_transform() const
        {
            return m_transform;
        }

      private:
        ParseContext&   m_context;
        double          m_time;
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
        virtual void start_element(const Attributes& attrs) override
        {
            Base::start_element(attrs);
        }

        virtual void end_element() override
        {
            if (m_transforms.empty())
                m_transforms[0.0] = Transformd(Matrix4d::identity());
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) override
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

        void copy_transform_sequence_to(TransformSequence& target)
        {
            target.clear();

            for (const_each<TransformMap> i = m_transforms; i; ++i)
                target.set_transform(i->first, i->second);
        }

      private:
        typedef map<double, Transformd> TransformMap;

        TransformMap m_transforms;
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

        virtual void start_element(const Attributes& attrs) override
        {
            m_values.clear();
        }

        virtual void characters(
            const XMLCh* const  chars,
            const XMLSize_t     length) override
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

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);

            m_color_entity.reset();
            m_name = get_value(attrs, "name");
            m_values.clear();
            m_alpha.clear();
        }

        virtual void end_element() override
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
            catch (const ExceptionDictionaryItemNotFound& e)
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
            ElementHandlerType*         handler) override
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

        virtual void start_element(const Attributes& attrs) override
        {
            Base::start_element(attrs);

            m_entity.reset();
            m_name = Base::get_value(attrs, "name");
            m_model = Base::get_value(attrs, "model");
        }

        virtual void end_element() override
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

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);

            m_texture.reset();
            m_name = get_value(attrs, "name");
            m_model = get_value(attrs, "model");
        }

        virtual void end_element() override
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
                            m_context.get_project().get_search_paths());
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
            catch (const ExceptionDictionaryItemNotFound& e)
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
      : public ParametrizedElementHandler
    {
      public:
        explicit TextureInstanceElementHandler(ParseContext& context)
          : m_context(context)
          , m_textures(0)
        {
        }

        void set_texture_container(const TextureContainer* textures)
        {
            m_textures = textures;
        }

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);

            m_texture_instance.reset();
            m_name = get_value(attrs, "name");
            m_texture = get_value(attrs, "texture");
        }

        virtual void end_element() override
        {
            ParametrizedElementHandler::end_element();

            assert(m_textures);
            const Texture* texture = m_textures->get_by_name(m_texture.c_str());

            if (texture)
            {
                try
                {
                    m_texture_instance =
                        TextureInstanceFactory::create(
                            m_name.c_str(),
                            m_params,
                            texture->get_name());
                }
                catch (const ExceptionDictionaryItemNotFound& e)
                {
                    RENDERER_LOG_ERROR(
                        "while defining texture instance \"%s\": required parameter \"%s\" missing.",
                        m_name.c_str(),
                        e.string());
                    m_context.get_event_counters().signal_error();
                }
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "while defining texture instance \"%s\": the texture \"%s\" does not exist.",
                    m_name.c_str(),
                    m_texture.c_str());
                m_context.get_event_counters().signal_error();
            }
        }

        auto_release_ptr<TextureInstance> get_texture_instance()
        {
            return m_texture_instance;
        }

      private:
        ParseContext&                       m_context;
        const TextureContainer*             m_textures;
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

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);

            m_environment.reset();
            m_name = get_value(attrs, "name");
            m_model = get_value(attrs, "model");
        }

        virtual void end_element() override
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
                   ParametrizedElementHandler>
    {
      public:
        explicit EnvironmentEDFElementHandler(ParseContext& context)
          : EntityElementHandler<
                EnvironmentEDF,
                EnvironmentEDFFactoryRegistrar,
                ParametrizedElementHandler>("environment edf", context)
        {
        }
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
          : EntityElementHandler<
                Light,
                LightFactoryRegistrar,
                TransformSequenceElementHandler<ParametrizedElementHandler> >("light", context)
        {
        }
    };


    //
    // <material> element handler.
    //

    class MaterialElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit MaterialElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);

            m_material.reset();
            m_name = get_value(attrs, "name");
            m_model = get_value(attrs, "model");
        }

        virtual void end_element() override
        {    
            ParametrizedElementHandler::end_element();

            if (m_model == MaterialFactory::get_model())
                m_material = MaterialFactory::create(m_name.c_str(), m_params);
            else
            {
                RENDERER_LOG_ERROR(
                    "while defining material \"%s\": invalid model \"%s\".",
                    m_name.c_str(),
                    m_model.c_str());
                m_context.get_event_counters().signal_error();
            }
        }

        auto_release_ptr<Material> get_material()
        {
            return m_material;
        }

      private:
        ParseContext&                       m_context;
        auto_release_ptr<Material>          m_material;
        string                              m_name;
        string                              m_model;
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

        virtual void end_element() override
        {
            Base::end_element();

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

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);

            clear_keep_memory(m_objects);
            m_name = get_value(attrs, "name");
            m_model = get_value(attrs, "model");
        }

        virtual void end_element() override
        {
            ParametrizedElementHandler::end_element();

            try
            {
                if (m_model == MeshObjectFactory::get_model())
                {
                    MeshObjectArray object_array;

                    if (MeshObjectReader::read(
                            m_context.get_project().get_search_paths(),
                            m_name.c_str(),
                            m_params,
                            object_array))
                        m_objects = array_vector<ObjectVector>(object_array);
                    else m_context.get_event_counters().signal_error();
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
            catch (const ExceptionDictionaryItemNotFound& e)
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

        virtual void start_element(const Attributes& attrs) override
        {
            const string slot_string = get_value(attrs, "slot");
            try
            {
                m_slot = from_string<size_t>(slot_string);
            }
            catch (const ExceptionStringConversionError&)
            {
                RENDERER_LOG_ERROR(
                    "while assigning material: slot must be an integer >= 0, got \"%s\".",
                    slot_string.c_str());
                m_context.get_event_counters().signal_error();
                m_slot = 0;
            }

            const string side_string = get_value(attrs, "side", "front");
            if (side_string == "front")
                m_side = ObjectInstance::FrontSide;
            else if (side_string == "back")
                m_side = ObjectInstance::BackSide;
            else
            {
                RENDERER_LOG_ERROR(
                    "while assigning material: side must be \"front\" or \"back\", got \"%s\".",
                    side_string.c_str());
                m_context.get_event_counters().signal_error();
                m_side = ObjectInstance::FrontSide;
            }

            m_material = get_value(attrs, "material");
        }

        size_t get_material_slot() const
        {
            return m_slot;
        }

        ObjectInstance::Side get_material_side() const
        {
            return m_side;
        }

        const string& get_material_name() const
        {
            return m_material;
        }

      private:
        ParseContext&           m_context;
        size_t                  m_slot;
        ObjectInstance::Side    m_side;
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
          , m_objects(0)
          , m_materials(0)
        {
        }

        void set_containers(
            const ObjectContainer*          objects,
            const MaterialContainer*        materials)
        {
            m_objects = objects;
            m_materials = materials;
        }

        virtual void start_element(const Attributes& attrs) override
        {
            Base::start_element(attrs);

            m_object_instance.reset();
            m_transform = Transformd(Matrix4d::identity());
            m_front_material_names.clear();
            m_back_material_names.clear();
            m_name = get_value(attrs, "name");
            m_object = get_value(attrs, "object");
        }

        virtual void end_element() override
        {
            Base::end_element();

            assert(m_objects);
            Object* object = m_objects->get_by_name(m_object.c_str());

            if (object)
            {
                m_object_instance =
                    ObjectInstanceFactory::create(
                        m_name.c_str(),
                        m_params,
                        *object,
                        m_transform,
                        m_front_material_names,
                        m_back_material_names);
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "while defining object instance \"%s\": the object \"%s\" does not exist.",
                    m_name.c_str(),
                    m_object.c_str());
                m_context.get_event_counters().signal_error();
            }
        }

        virtual void end_child_element(
            const ProjectElementID          element,
            ElementHandlerType*             handler) override
        {
            switch (element)
            {
              case ElementAssignMaterial:
                {
                    AssignMaterialElementHandler* assign_mat_handler =
                        static_cast<AssignMaterialElementHandler*>(handler);

                    const size_t material_slot = assign_mat_handler->get_material_slot();
                    const ObjectInstance::Side material_side = assign_mat_handler->get_material_side();
                    const string& material_name = assign_mat_handler->get_material_name();
                    StringArray& material_names =
                        material_side == ObjectInstance::FrontSide
                            ? m_front_material_names
                            : m_back_material_names;

                    const size_t MaxMaterialSlots = 256;

                    if (material_slot < MaxMaterialSlots)
                    {
                        ensure_minimum_size(material_names, material_slot + 1);
                        material_names.set(material_slot, material_name.c_str());
                    }
                    else
                    {
                        RENDERER_LOG_ERROR(
                            "while defining object instance \"%s\": "
                            "material slot should be in [0, " FMT_SIZE_T "], got " FMT_SIZE_T ".",
                            m_name.c_str(),
                            MaxMaterialSlots - 1,
                            material_slot);
                        m_context.get_event_counters().signal_error();
                    }
                }
                break;

              case ElementTransform:
                {
                    TransformElementHandler* transform_handler =
                        static_cast<TransformElementHandler*>(handler);
                    m_transform = transform_handler->get_transform();
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
        const ObjectContainer*              m_objects;
        const MaterialContainer*            m_materials;
        auto_release_ptr<ObjectInstance>    m_object_instance;
        string                              m_name;
        string                              m_object;
        Transformd                          m_transform;
        StringArray                         m_front_material_names;
        StringArray                         m_back_material_names;
    };


    //
    // <assembly> element handler.
    //

    class AssemblyElementHandler
      : public ParametrizedElementHandler
    {
      public:
        explicit AssemblyElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);

            m_assembly.reset();
            m_name = get_value(attrs, "name");
            m_bsdfs.clear();
            m_colors.clear();
            m_edfs.clear();
            m_lights.clear();
            m_materials.clear();
            m_objects.clear();
            m_object_instances.clear();
            m_surface_shaders.clear();
            m_textures.clear();
            m_texture_instances.clear();
        }

        virtual void end_element() override
        {
            ParametrizedElementHandler::end_element();

            m_assembly = AssemblyFactory::create(m_name.c_str(), m_params);
            m_assembly->bsdfs().swap(m_bsdfs);
            m_assembly->colors().swap(m_colors);
            m_assembly->edfs().swap(m_edfs);
            m_assembly->lights().swap(m_lights);
            m_assembly->materials().swap(m_materials);
            m_assembly->objects().swap(m_objects);
            m_assembly->object_instances().swap(m_object_instances);
            m_assembly->surface_shaders().swap(m_surface_shaders);
            m_assembly->textures().swap(m_textures);
            m_assembly->texture_instances().swap(m_texture_instances);
        }

        virtual void start_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) override
        {
            switch (element)
            {
              case ElementBSDF:
                break;

              case ElementColor:
                break;

              case ElementEDF:
                break;

              case ElementLight:
                break;

              case ElementMaterial:
                break;

              case ElementObject:
                break;

              case ElementObjectInstance:
                {
                    ObjectInstanceElementHandler* object_inst_handler =
                        static_cast<ObjectInstanceElementHandler*>(handler);
                    object_inst_handler->set_containers(&m_objects, &m_materials);
                }
                break;

              case ElementSurfaceShader:
                break;

              case ElementTexture:
                break;

              case ElementTextureInstance:
                {
                    TextureInstanceElementHandler* texture_inst_handler =
                        static_cast<TextureInstanceElementHandler*>(handler);
                    texture_inst_handler->set_texture_container(&m_textures);
                }
                break;

              default:
                ParametrizedElementHandler::start_child_element(element, handler);
                break;
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) override
        {
            switch (element)
            {
              case ElementBSDF:
                {
                    BSDFElementHandler* bsdf_handler =
                        static_cast<BSDFElementHandler*>(handler);
                    auto_release_ptr<BSDF> bsdf = bsdf_handler->get_entity();
                    if (bsdf.get())
                        m_bsdfs.insert(bsdf);
                }
                break;

              case ElementColor:
                {
                    ColorElementHandler* color_handler =
                        static_cast<ColorElementHandler*>(handler);
                    auto_release_ptr<ColorEntity> color_entity =
                        color_handler->get_color_entity();
                    if (color_entity.get())
                        m_colors.insert(color_entity);
                }
                break;

              case ElementEDF:
                {
                    EDFElementHandler* edf_handler =
                        static_cast<EDFElementHandler*>(handler);
                    auto_release_ptr<EDF> edf = edf_handler->get_entity();
                    if (edf.get())
                        m_edfs.insert(edf);
                }
                break;

              case ElementLight:
                {
                    LightElementHandler* light_handler =
                        static_cast<LightElementHandler*>(handler);
                    auto_release_ptr<Light> light = light_handler->get_entity();
                    if (light.get())
                        m_lights.insert(light);
                }
                break;

              case ElementMaterial:
                {
                    MaterialElementHandler* material_handler =
                        static_cast<MaterialElementHandler*>(handler);
                    auto_release_ptr<Material> material = material_handler->get_material();
                    if (material.get())
                        m_materials.insert(material);
                }
                break;

              case ElementObject:
                {
                    ObjectElementHandler* object_handler =
                        static_cast<ObjectElementHandler*>(handler);
                    for (const_each<ObjectElementHandler::ObjectVector> i =
                         object_handler->get_objects(); i; ++i)
                        m_objects.insert(auto_release_ptr<Object>(*i));
                }
                break;

              case ElementObjectInstance:
                {
                    ObjectInstanceElementHandler* object_inst_handler =
                        static_cast<ObjectInstanceElementHandler*>(handler);
                    auto_release_ptr<ObjectInstance> instance = object_inst_handler->get_object_instance();
                    if (instance.get())
                        m_object_instances.insert(instance);
                }
                break;

              case ElementSurfaceShader:
                {
                    SurfaceShaderElementHandler* surface_shader_handler =
                        static_cast<SurfaceShaderElementHandler*>(handler);
                    auto_release_ptr<SurfaceShader> surface_shader =
                        surface_shader_handler->get_entity();
                    if (surface_shader.get())
                        m_surface_shaders.insert(surface_shader);
                }
                break;

              case ElementTexture:
                {
                    TextureElementHandler* texture_handler =
                        static_cast<TextureElementHandler*>(handler);
                    auto_release_ptr<Texture> texture = texture_handler->get_texture();
                    if (texture.get())
                        m_textures.insert(texture);
                }
                break;

              case ElementTextureInstance:
                {
                    TextureInstanceElementHandler* texture_inst_handler =
                        static_cast<TextureInstanceElementHandler*>(handler);
                    auto_release_ptr<TextureInstance> instance = texture_inst_handler->get_texture_instance();
                    if (instance.get())
                        m_texture_instances.insert(instance);
                }
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
        ParseContext&               m_context;
        auto_release_ptr<Assembly>  m_assembly;
        string                      m_name;
        BSDFContainer               m_bsdfs;
        ColorContainer              m_colors;
        EDFContainer                m_edfs;
        LightContainer              m_lights;
        MaterialContainer           m_materials;
        ObjectContainer             m_objects;
        ObjectInstanceContainer     m_object_instances;
        SurfaceShaderContainer      m_surface_shaders;
        TextureContainer            m_textures;
        TextureInstanceContainer    m_texture_instances;
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
          , m_assemblies(0)
        {
        }

        void set_assembly_container(const AssemblyContainer* assemblies)
        {
            m_assemblies = assemblies;
        }

        virtual void start_element(const Attributes& attrs) override
        {
            Base::start_element(attrs);

            m_assembly_instance.reset();
            m_name = get_value(attrs, "name");
            m_assembly = get_value(attrs, "assembly");
        }

        virtual void end_element() override
        {
            Base::end_element();

            assert(m_assemblies);
            const Assembly* assembly = m_assemblies->get_by_name(m_assembly.c_str());

            if (assembly)
            {
                m_assembly_instance =
                    AssemblyInstanceFactory::create(m_name.c_str(), m_params, *assembly);
                copy_transform_sequence_to(m_assembly_instance->transform_sequence());
            }
            else
            {
                RENDERER_LOG_ERROR(
                    "while defining assembly instance \"%s\": the assembly \"%s\" does not exist",
                    m_name.c_str(),
                    m_assembly.c_str());
                m_context.get_event_counters().signal_error();
            }
        }

        auto_release_ptr<AssemblyInstance> get_assembly_instance()
        {
            return m_assembly_instance;
        }

      private:
        typedef TransformSequenceElementHandler<ParametrizedElementHandler> Base;

        ParseContext&                       m_context;
        const AssemblyContainer*            m_assemblies;
        auto_release_ptr<AssemblyInstance>  m_assembly_instance;
        string                              m_name;
        string                              m_assembly;
    };


    //
    // <scene> element handler.
    //

    class SceneElementHandler
      : public ElementHandlerBase
    {
      public:
        explicit SceneElementHandler(ParseContext& context)
          : m_context(context)
        {
        }

        virtual void start_element(const Attributes& attrs) override
        {
            m_scene = SceneFactory::create();
        }

        virtual void end_element() override
        {
            // Compute the bounding box of the scene.
            const GAABB3 bbox =
                compute_parent_bbox<GAABB3>(
                    m_scene->assembly_instances().begin(),
                    m_scene->assembly_instances().end());

            // Print the bounding box of the scene.
            if (bbox.is_valid())
            {
                RENDERER_LOG_INFO("scene bounding box: (%f, %f, %f)-(%f, %f, %f).",
                    bbox.min[0], bbox.min[1], bbox.min[2],
                    bbox.max[0], bbox.max[1], bbox.max[2]);
            }
            else
            {
                RENDERER_LOG_INFO("scene bounding box is empty.");
            }
        }

        virtual void start_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) override
        {
            assert(m_scene.get());

            switch (element)
            {
              case ElementAssembly:
                break;

              case ElementAssemblyInstance:
                {
                    AssemblyInstanceElementHandler* asm_inst_handler =
                        static_cast<AssemblyInstanceElementHandler*>(handler);
                    asm_inst_handler->set_assembly_container(&m_scene->assemblies());
                }
                break;

              case ElementCamera:
                break;

              case ElementColor:
                break;

              case ElementEnvironment:
                break;

              case ElementEnvironmentEDF:
                break;

              case ElementEnvironmentShader:
                break;

              case ElementTexture:
                break;

              case ElementTextureInstance:
                {
                    TextureInstanceElementHandler* texture_inst_handler =
                        static_cast<TextureInstanceElementHandler*>(handler);
                    texture_inst_handler->set_texture_container(&m_scene->textures());
                }
                break;

              assert_otherwise;
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) override
        {
            assert(m_scene.get());

            switch (element)
            {
              case ElementAssembly:
                {
                    AssemblyElementHandler* assembly_handler =
                        static_cast<AssemblyElementHandler*>(handler);
                    auto_release_ptr<Assembly> assembly =
                        assembly_handler->get_assembly();
                    if (assembly.get())
                        m_scene->assemblies().insert(assembly);
                }
                break;

              case ElementAssemblyInstance:
                {
                    AssemblyInstanceElementHandler* asm_inst_handler =
                        static_cast<AssemblyInstanceElementHandler*>(handler);
                    auto_release_ptr<AssemblyInstance> instance =
                        asm_inst_handler->get_assembly_instance();
                    if (instance.get())
                        m_scene->assembly_instances().insert(instance);
                }
                break;

              case ElementColor:
                {
                    ColorElementHandler* color_handler =
                        static_cast<ColorElementHandler*>(handler);
                    auto_release_ptr<ColorEntity> color_entity =
                        color_handler->get_color_entity();
                    if (color_entity.get())
                        m_scene->colors().insert(color_entity);
                }
                break;

              case ElementCamera:
                {
                    CameraElementHandler* camera_handler =
                        static_cast<CameraElementHandler*>(handler);
                    auto_release_ptr<Camera> camera = camera_handler->get_entity();
                    if (camera.get())
                    {
                        if (m_scene->get_camera())
                        {
                            RENDERER_LOG_WARNING("support for multiple cameras is not implemented yet.");
                            m_context.get_event_counters().signal_warning();
                        }
                        m_scene->set_camera(camera);
                    }
                }
                break;

              case ElementEnvironment:
                {
                    EnvironmentElementHandler* env_handler =
                        static_cast<EnvironmentElementHandler*>(handler);
                    auto_release_ptr<Environment> environment = env_handler->get_environment();
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
                {
                    EnvironmentEDFElementHandler* env_edf_handler =
                        static_cast<EnvironmentEDFElementHandler*>(handler);
                    auto_release_ptr<EnvironmentEDF> env_edf = env_edf_handler->get_entity();
                    if (env_edf.get())
                        m_scene->environment_edfs().insert(env_edf);
                }
                break;

              case ElementEnvironmentShader:
                {
                    EnvironmentShaderElementHandler* env_shader_handler =
                        static_cast<EnvironmentShaderElementHandler*>(handler);
                    auto_release_ptr<EnvironmentShader> env_shader =
                        env_shader_handler->get_entity();
                    if (env_shader.get())
                        m_scene->environment_shaders().insert(env_shader);
                }
                break;

              case ElementTexture:
                {
                    TextureElementHandler* texture_handler =
                        static_cast<TextureElementHandler*>(handler);
                    auto_release_ptr<Texture> texture = texture_handler->get_texture();
                    if (texture.get())
                        m_scene->textures().insert(texture);
                }
                break;

              case ElementTextureInstance:
                {
                    TextureInstanceElementHandler* texture_inst_handler =
                        static_cast<TextureInstanceElementHandler*>(handler);
                    auto_release_ptr<TextureInstance> instance = texture_inst_handler->get_texture_instance();
                    if (instance.get())
                        m_scene->texture_instances().insert(instance);
                }
                break;

              assert_otherwise;
            }
        }

        auto_release_ptr<Scene> get_scene()
        {
            return m_scene;
        }

      private:
        ParseContext&           m_context;
        auto_release_ptr<Scene> m_scene;
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

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);

            m_frame.reset();
            m_name = get_value(attrs, "name");
        }

        virtual void end_element() override
        {    
            ParametrizedElementHandler::end_element();

            m_frame = FrameFactory::create(m_name.c_str(), m_params);
        }

        auto_release_ptr<Frame> get_frame()
        {
            return m_frame;
        }

      private:
        ParseContext&           m_context;
        auto_release_ptr<Frame> m_frame;
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
            ElementHandlerType*         handler) override
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

        virtual void start_element(const Attributes& attrs) override
        {
            ParametrizedElementHandler::start_element(attrs);

            m_configuration.reset();
            m_name = get_value(attrs, "name");
            m_base_name = get_value(attrs, "base");
        }

        virtual void end_element() override
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
                        m_configuration->get_name(),
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
            ElementHandlerType*         handler) override
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
            ElementHandlerType*         handler) override
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

        virtual void start_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) override
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

              case ElementScene:
                break;

              assert_otherwise;
            }
        }

        virtual void end_child_element(
            const ProjectElementID      element,
            ElementHandlerType*         handler) override
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

              case ElementScene:
                {
                    SceneElementHandler* scene_handler =
                        static_cast<SceneElementHandler*>(handler);
                    auto_release_ptr<Scene> scene = scene_handler->get_scene();
                    if (scene.get())
                        m_project->set_scene(scene);
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
            register_factory_helper<AssemblyElementHandler>("assembly", ElementAssembly);
            register_factory_helper<AssemblyInstanceElementHandler>("assembly_instance", ElementAssemblyInstance);
            register_factory_helper<AssignMaterialElementHandler>("assign_material", ElementAssignMaterial);
            register_factory_helper<BSDFElementHandler>("bsdf", ElementBSDF);
            register_factory_helper<CameraElementHandler>("camera", ElementCamera);
            register_factory_helper<ColorElementHandler>("color", ElementColor);
            register_factory_helper<ConfigurationElementHandler>("configuration", ElementConfiguration);
            register_factory_helper<ConfigurationsElementHandler>("configurations", ElementConfigurations);
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
            register_factory_helper<RotationElementHandler>("rotation", ElementRotation);
            register_factory_helper<ScalingElementHandler>("scaling", ElementScaling);
            register_factory_helper<SceneElementHandler>("scene", ElementScene);
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

            virtual auto_ptr<ElementHandlerType> create() override
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

            virtual auto_ptr<ElementHandlerType> create() override
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

auto_release_ptr<Project> ProjectFileReader::read(
    const char*             project_filename,
    const char*             schema_filename)
{
    assert(project_filename);
    assert(schema_filename);

    XercesCContext xerces_context(global_logger());
    if (!xerces_context.is_initialized())
        return auto_release_ptr<Project>(0);

    EventCounters event_counters;
    auto_release_ptr<Project> project(
        load_project_file(
            project_filename,
            schema_filename,
            event_counters));

    if (project.get())
        postprocess_project(project.ref(), event_counters);

    print_loading_results(project_filename, false, event_counters);

    return event_counters.has_errors() ? auto_release_ptr<Project>(0) : project;
}

auto_release_ptr<Project> ProjectFileReader::load_builtin(
    const char*             project_name)
{
    assert(project_name);

    EventCounters event_counters;
    auto_release_ptr<Project> project(
        construct_builtin_project(project_name, event_counters));

    if (project.get())
        postprocess_project(project.ref(), event_counters);

    print_loading_results(project_name, true, event_counters);

    return event_counters.has_errors() ? auto_release_ptr<Project>(0) : project;
}

auto_release_ptr<Project> ProjectFileReader::load_project_file(
    const char*             project_filename,
    const char*             schema_filename,
    EventCounters&          event_counters) const
{
    // Create an empty project.
    auto_release_ptr<Project> project(ProjectFactory::create(project_filename));
    project->set_path(project_filename);

    // Create the error handler.
    auto_ptr<ErrorLogger> error_handler(
        new ErrorLoggerAndCounter(
            project_filename,
            event_counters));

    // Create the content handler.
    ParseContext context(project.ref(), event_counters);
    auto_ptr<ContentHandler> content_handler(
        new ContentHandler(
            project.get(),
            context));

    // Create the parser.
    auto_ptr<SAX2XMLReader> parser(XMLReaderFactory::createXMLReader());
    parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);         // perform namespace processing
    parser->setFeature(XMLUni::fgSAX2CoreValidation, true);         // report all validation errors
    parser->setFeature(XMLUni::fgXercesSchema, true);               // enable the parser's schema support
    parser->setProperty(
        XMLUni::fgXercesSchemaExternalNoNameSpaceSchemaLocation,
        const_cast<void*>(
            static_cast<const void*>(
                transcode(schema_filename).c_str())));
    parser->setErrorHandler(error_handler.get());
    parser->setContentHandler(content_handler.get());

    // Load the project file.
    RENDERER_LOG_INFO("loading project file %s...", project_filename);
    try
    {
        parser->parse(project_filename);
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
    const Project&          project,
    EventCounters&          event_counters) const
{
    if (!event_counters.has_errors())
        validate_project(project, event_counters);

    if (!event_counters.has_errors())
        complete_project(project, event_counters);
}

void ProjectFileReader::validate_project(
    const Project&          project,
    EventCounters&          event_counters) const
{
    // Make sure the project contains a scene.
    if (project.get_scene())
    {
        // Make sure the scene contains a camera.
        if (project.get_scene()->get_camera() == 0)
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
    const Project&          project,
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

void ProjectFileReader::print_loading_results(
    const char*             project_name,
    const bool              builtin_project,
    const EventCounters&    event_counters) const
{
    const size_t warning_count = event_counters.get_warning_count();
    const size_t error_count = event_counters.get_error_count();

    const LogMessage::Category log_category =
        error_count > 0 ? LogMessage::Error :
        warning_count > 0 ? LogMessage::Warning :
        LogMessage::Info;

    RENDERER_LOG(
        log_category,
        "%s %s %s (" FMT_SIZE_T " %s, " FMT_SIZE_T " %s).",
        error_count > 0 ? "failed to load" : "successfully loaded",
        builtin_project ? "built-in project" : "project file",
        project_name,
        error_count,
        plural(error_count, "error").c_str(),
        warning_count,
        plural(warning_count, "warning").c_str());
}

}   // namespace renderer
