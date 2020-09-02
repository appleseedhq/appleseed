
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Entity; }
namespace renderer  { class Source; }
namespace renderer  { class SourceInputs; }
namespace renderer  { class TextureCache; }

namespace renderer
{

//
// Input formats.
//

enum class InputFormat
{
    Float,
    SpectralReflectance,
    SpectralIlluminance,
    SpectralReflectanceWithAlpha,
    SpectralIlluminanceWithAlpha,
    Entity
};


//
// Input types.
//

enum InputType
{
    InputTypeRequired,
    InputTypeOptional
};


//
// A collection of named and typed inputs.
//

class APPLESEED_DLLSYMBOL InputArray
  : public foundation::NonCopyable
{
  public:
    class iterator;

    // Constant iterator.
    class APPLESEED_DLLSYMBOL const_iterator
    {
      public:
        // Value type.
        typedef const_iterator value_type;

        // Construction from a mutable iterator.
        const_iterator(const iterator& rhs);

        // Copy constructor.
        const_iterator(const const_iterator& rhs);

        // Assignment operator.
        const_iterator& operator=(const const_iterator& rhs);

        // Equality and inequality tests.
        bool operator==(const const_iterator& rhs) const;
        bool operator!=(const const_iterator& rhs) const;

        // Preincrement and predecrement operators.
        const_iterator& operator++();
        const_iterator& operator--();

        // Dereference operator.
        const const_iterator& operator*() const;

        // Get the name of this input.
        const char* name() const;

        // Get the format of this input.
        InputFormat format() const;

        // Get the type (required or optional) of this input.
        InputType type() const;

        // Get the default value of this input, or 0 if this input has no default value.
        const char* default_value() const;

        // Get the source bound to this input, or 0 if no source is bound to this input.
        Source* source() const;

        // Get the entity bound to this input, or 0 if no entity is bound to this input.
        Entity* get_entity() const;

      protected:
        friend class InputArray;

        // Constructor.
        const_iterator(const InputArray* array, const size_t index);

        const InputArray*   m_input_array;
        size_t              m_input_index;
    };

    // Mutable iterator.
    class APPLESEED_DLLSYMBOL iterator
      : public const_iterator
    {
      public:
        // Value type.
        typedef iterator value_type;

        // Copy constructor.
        iterator(const iterator& rhs);

        // Assignment operator.
        iterator& operator=(const iterator& rhs);

        // Preincrement and predecrement operators.
        iterator& operator++();
        iterator& operator--();

        // Dereference operator.
        iterator& operator*();

        // Bind a source to this input, unbinding any previously bound source.
        // The ownership of the source is passed to renderer::InputArray.
        // 'source' may be 0, in which case the source is unbound.
        void bind(Source* source);

        // Bind an entity to this input, unbinding any previously bound entity.
        // The ownership of the entity is NOT passed to renderer::InputArray.
        // 'entity' may be 0, in which case the entity is unbound.
        void bind(Entity* entity);

      private:
        friend class InputArray;

        // Constructor.
        iterator(const InputArray* array, const size_t index);
    };

    // Constructor.
    InputArray();

    // Destructor.
    ~InputArray();

    // Declare an input.
    void declare(
        const char*                 name,
        const InputFormat           format,
        const char*                 default_value = nullptr);

    // Return mutable begin and end input iterators.
    iterator begin();
    iterator end();

    // Return constant begin and end input iterators.
    const_iterator begin() const;
    const_iterator end() const;

    // Find a given input.
    // Return end() if the input could not be found.
    iterator find(const char* name);
    const_iterator find(const char* name) const;

    // Get the source bound to a given input.
    // Return 0 if the input could not be found, or no source is bound to it.
    Source* source(const char* name) const;

    // Get the entity bound to a given input.
    // Return 0 if the input could not be found, or no entity is bound to it.
    Entity* get_entity(const char* name) const;

    // Compute the cumulated size in bytes of the input values.
    size_t compute_data_size() const;

    // Evaluate all inputs into a preallocated block of memory.
    // 'values' must be 16-byte aligned.
    void evaluate(
        TextureCache&               texture_cache,
        const SourceInputs&         source_inputs,
        void*                       values) const;

    // Evaluate all uniform inputs into a preallocated block of memory.
    // 'values' must be 16-byte aligned.
    void evaluate_uniforms(
        void*                       values) const;

  private:
    struct Impl;
    Impl* impl;
};


//
// Declare a collection of input values.
//
// Usage:
//
//     APPLESEED_DECLARE_INPUT_VALUES(InputValues)
//     {
//         Spectrum m_color;
//         Alpha    m_alpha;
//         float    m_multiplier;
//     };
//

#define APPLESEED_DECLARE_INPUT_VALUES(name) struct APPLESEED_ALIGN(16) name

}   // namespace renderer
