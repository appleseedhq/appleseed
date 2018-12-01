
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
#include "foundation/core/exceptions/stringexception.h"
#include "foundation/utility/kvpair.h"

// Standard headers.
#include <cassert>
#include <map>
#include <string>
#include <utility>

namespace renderer
{

//
// Symbol table used for input binding.
//

class SymbolTable
{
  public:
    enum SymbolID
    {
        SymbolNotFound,
        SymbolAssembly,
        SymbolAssemblyInstance,
        SymbolBSDF,
        SymbolBSSRDF,
        SymbolCamera,
        SymbolColor,
        SymbolEDF,
        SymbolEnvironment,
        SymbolEnvironmentEDF,
        SymbolEnvironmentShader,
        SymbolMaterial,
        SymbolLight,
        SymbolObject,
        SymbolObjectInstance,
        SymbolShaderGroup,
        SymbolSurfaceShader,
        SymbolTexture,
        SymbolTextureInstance,
        SymbolVolume
    };

    // Exception thrown when attempting to insert a symbol
    // with the same name as an existing symbol.
    struct ExceptionDuplicateSymbol
      : public foundation::StringException
    {
        explicit ExceptionDuplicateSymbol(const char* name)
          : foundation::StringException("duplicate symbol", name) {}
    };

    // Return a human-readable representation of a symbol identifier.
    static const char* symbol_name(const SymbolID symbol_id);

    // Insert a symbol.
    void insert(
        const std::string&  name,
        const SymbolID      symbol_id);

    // Lookup a symbol.
    SymbolID lookup(const std::string& name) const;

  private:
    typedef std::map<std::string, SymbolID> SymbolContainer;

    SymbolContainer m_symbols;
};


//
// SymbolTable class implementation.
//

inline const char* SymbolTable::symbol_name(const SymbolID symbol_id)
{
    typedef foundation::KeyValuePair<SymbolID, const char*> SymbolNameEntry;

    static const SymbolNameEntry SymbolNames[] =
    {
        { SymbolNotFound,           "not found" },
        { SymbolAssembly,           "assembly" },
        { SymbolAssemblyInstance,   "assembly instance" },
        { SymbolBSDF,               "bsdf" },
        { SymbolBSSRDF,             "bssrdf" },
        { SymbolCamera,             "camera" },
        { SymbolColor,              "color" },
        { SymbolEDF,                "edf" },
        { SymbolEnvironment,        "environment" },
        { SymbolEnvironmentEDF,     "environment edf" },
        { SymbolEnvironmentShader,  "environment shader" },
        { SymbolMaterial,           "material" },
        { SymbolLight,              "light" },
        { SymbolObject,             "object" },
        { SymbolObjectInstance,     "object instance" },
        { SymbolShaderGroup,        "shader group" },
        { SymbolSurfaceShader,      "surface shader" },
        { SymbolTexture,            "texture" },
        { SymbolTextureInstance,    "texture instance" },
        { SymbolVolume,             "volume" }
    };

    const SymbolNameEntry* symbol = LOOKUP_KVPAIR_ARRAY(SymbolNames, symbol_id);

    assert(symbol);

    return symbol->m_value;
}

inline void SymbolTable::insert(
    const std::string&  name,
    const SymbolID      symbol_id)
{
    if (!m_symbols.insert(std::make_pair(name, symbol_id)).second)
        throw ExceptionDuplicateSymbol(name.c_str());
}

inline SymbolTable::SymbolID SymbolTable::lookup(const std::string& name) const
{
    const SymbolContainer::const_iterator i = m_symbols.find(name);
    return i == m_symbols.end() ? SymbolNotFound : i->second;
}

}   // namespace renderer
