
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

// Standard headers.
#include <fstream>
#include <string>
#include <vector>

namespace foundation
{

class GnuplotFile
  : public NonCopyable
{
  public:
    GnuplotFile();

    GnuplotFile& set_title(const std::string& title);
    GnuplotFile& set_xlabel(const std::string& label);
    GnuplotFile& set_ylabel(const std::string& label);
    GnuplotFile& set_xrange(const double low, const double high);
    GnuplotFile& set_yrange(const double low, const double high);
    GnuplotFile& set_logscale_x();
    GnuplotFile& set_logscale_y();

    class Plot
    {
      public:
        Plot& set_points(const std::vector<Vector2d>& points);
        Plot& set_points(const std::vector<Vector2f>& points);
        Plot& set_title(const std::string& title);
        Plot& set_color(const std::string& color);
        Plot& set_style(const std::string& style);
        Plot& set_smoothing(const std::string& smoothing);

      private:
        friend class GnuplotFile;

        std::vector<Vector2d>   m_points;
        std::string             m_title;
        std::string             m_color;
        std::string             m_style;
        std::string             m_smoothing;

        void write_decl(std::ofstream& file) const;
        void write_points(std::ofstream& file) const;
    };

    Plot& new_plot();

    bool write(const std::string& filepath) const;

  private:
    std::string         m_title;
    std::string         m_xlabel;
    std::string         m_ylabel;
    bool                m_has_xrange;
    bool                m_has_yrange;
    Vector2d            m_xrange;
    Vector2d            m_yrange;
    bool                m_logscale_x;
    bool                m_logscale_y;
    std::vector<Plot>   m_plots;
};

}   // namespace foundation
