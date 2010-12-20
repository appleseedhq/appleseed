
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_SCENE_TEXTUREINSTANCE_H
#define APPLESEED_RENDERER_MODELING_SCENE_TEXTUREINSTANCE_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"

namespace renderer
{

//
// Texture mapping modes.
//

// Texture addressing modes.
enum TextureAddressingMode
{
    TextureAddressingClamp = 0,
    TextureAddressingWrap
};

// Texture filtering modes.
enum TextureFilteringMode
{
    TextureFilteringNearest = 0,
    TextureFilteringBilinear,
    TextureFilteringBicubic,
    TextureFilteringFeline,             // Reference: http://www.hpl.hp.com/techreports/Compaq-DEC/WRL-99-1.pdf
    TextureFilteringEWA
};


//
// An instance of a texture.
//
// todo: allow to specify the lighting conditions of a texture.
//

class RENDERERDLL TextureInstance
  : public Entity
{
  public:
    // Delete this instance.
    virtual void release();

    // Return the index in the assembly of the instantiated texture.
    size_t get_texture_index() const;

    // Return the texture mapping modes.
    TextureAddressingMode get_addressing_mode() const;
    TextureFilteringMode get_filtering_mode() const;

    // Retrieve the multiplier value.
    float get_multiplier() const;

  private:
    friend class TextureInstanceFactory;

    // Private implementation.
    struct Impl;
    Impl* impl;

    // Constructor.
    TextureInstance(
        const char*         name,
        const ParamArray&   params,
        const size_t        texture_index);

    // Destructor.
    ~TextureInstance();
};


//
// Texture instance factory.
//

class RENDERERDLL TextureInstanceFactory
{
  public:
    // Create a new texture instance.
    static foundation::auto_release_ptr<TextureInstance> create(
        const char*         name,
        const ParamArray&   params,
        const size_t        texture_index);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SCENE_TEXTUREINSTANCE_H
