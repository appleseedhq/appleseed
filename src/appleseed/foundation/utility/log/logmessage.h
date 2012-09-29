
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

#ifndef APPLESEED_FOUNDATION_UTILITY_LOG_LOGMESSAGE_H
#define APPLESEED_FOUNDATION_UTILITY_LOG_LOGMESSAGE_H

// appleseed.foundation headers.
#include "foundation/core/dllsymbol.h"

namespace foundation
{

//
// Log message.
//

class FOUNDATIONDLL LogMessage
{
  public:
    // Message categories.
    enum Category
    {
        Info = 0,                   // general information, statistics, progress messages, etc.
        Debug,                      // debug message
        Warning,                    // warning message; execution can continue
        Error,                      // severe error message; the program will attempt to continue
        Fatal,                      // fatal error message; the program will terminate afterward
        NumMessageCategories        // number of message categories (keep at the end)
    };

    // Message formatting flags.
    enum FormattingFlags
    {
        DisplayNothing  = 0,
        DisplayCategory = 1 << 0,
        DisplayFile     = 1 << 1,
        DisplayLine     = 1 << 2,
        DisplayDate     = 1 << 3,
        DisplayTime     = 1 << 4,
        DisplayMessage  = 1 << 5
    };

    // Default message formatting flags.
    static const int DefaultFormattingFlags;

    // Return a string identifying a given message category.
    static const char* get_category_name(const Category c);

    // Return a category name padded with spaces to the right.
    static const char* get_padded_category_name(const Category c);
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_LOG_LOGMESSAGE_H
