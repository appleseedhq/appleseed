
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

#ifndef APPLESEED_RENDERER_MODELING_PROJECT_EVENTCOUNTERS_H
#define APPLESEED_RENDERER_MODELING_PROJECT_EVENTCOUNTERS_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

namespace renderer
{

//
// Warning and error counters.
//

class EventCounters
  : public foundation::NonCopyable
{
  public:
    // Constructor, resets all counters.
    EventCounters();

    // Reset all counters.
    void clear();

    // Signal a warning.
    void signal_warning();

    // Signal an error.
    void signal_error();
    
    // Read the event counters.
    size_t get_warning_count() const;
    size_t get_error_count() const;
    bool has_errors() const;

  private:
    size_t  m_warning_count;
    size_t  m_error_count;
};


//
// EventCounters class implementation.
//

// Constructor.
inline EventCounters::EventCounters()
{
    clear();
}

// Reset all counters.
inline void EventCounters::clear()
{
    m_warning_count = 0;
    m_error_count = 0;
}

// Signal a warning.
inline void EventCounters::signal_warning()
{
    ++m_warning_count;
}

// Signal an error.
inline void EventCounters::signal_error()
{
    ++m_error_count;
}

// Read the event counters.
inline size_t EventCounters::get_warning_count() const
{
    return m_warning_count;
}
inline size_t EventCounters::get_error_count() const
{
    return m_error_count;
}
inline bool EventCounters::has_errors() const
{
    return m_error_count > 0;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_PROJECT_EVENTCOUNTERS_H
