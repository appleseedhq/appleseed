
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Gleb Mishchenko, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_UNZIPPER_H
#define APPLESEED_FOUNDATION_UTILITY_UNZIPPER_H

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"

// Standard headers.
#include <string>
#include <vector>

namespace foundation
{

//
// Exception class used for all unzipper exceptions
//

class UnzipException
  : public foundation::Exception
{
  public:
    UnzipException(const char* what);
    UnzipException(const char* what, const int err);
};  


//
// This function unzips zip file zipFilename to unzipped_dir directory.
//
// Throws UnzipException in case of exception.
// If exception is thrown, unzipped folder is deleted.
//

void unzip(const std::string& zip_filename, const std::string& unzipped_dir);


//
// Checks if file is in zip format by trying to open it.
//

bool is_zip_file(const char* filename);

//
// This function returns all filenames from zip_filenames zip with given extension.
// 
// Throws UnzipException if zip_filename can't be opened or is not a zip file.
//

std::vector<std::string> get_filenames_with_extension_from_zip(
    const std::string& zip_filename, 
    const std::string& extension);

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_UNZIPPER_H
