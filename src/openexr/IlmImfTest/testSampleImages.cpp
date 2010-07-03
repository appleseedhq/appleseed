///////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2002, Industrial Light & Magic, a division of Lucas
// Digital Ltd. LLC
// 
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
// *       Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
// *       Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
// *       Neither the name of Industrial Light & Magic nor the names of
// its contributors may be used to endorse or promote products derived
// from this software without specific prior written permission. 
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
///////////////////////////////////////////////////////////////////////////

#include <ImfRgbaFile.h>
#include <ImfArray.h>
#include "IlmThread.h"
#include <stdio.h>
#include <assert.h>

#ifndef ILM_IMF_TEST_IMAGEDIR
    #define ILM_IMF_TEST_IMAGEDIR
#endif


using namespace Imf;
using namespace Imath;
using namespace std;

namespace {

void
readImage (const char fileName[], unsigned int correctChecksum)
{
    cout << "file " << fileName << " " << flush;

    Imf::RgbaInputFile in (fileName);

    cout << "version " << in.version() << " " << flush;

    const Box2i &dw = in.dataWindow();

    int w = dw.max.x - dw.min.x + 1;
    int h = dw.max.y - dw.min.y + 1;
    int dx = dw.min.x;
    int dy = dw.min.y;

    Array<Imf::Rgba> pixels (w * h);
    in.setFrameBuffer (pixels - dx - dy * w, 1, w);
    in.readPixels (in.dataWindow().min.y, in.dataWindow().max.y);

    unsigned int checksum = 0;

    for (int i = 0; i < w * h; ++i)
    {
	checksum ^= pixels[i].r.bits();
	checksum ^= pixels[i].g.bits();
	checksum ^= pixels[i].b.bits();
	checksum ^= pixels[i].a.bits();
    }

    cout << "checksum = " << checksum << endl;

    assert (checksum == correctChecksum);
}

} // namespace


void
testSampleImages ()
{
    try
    {
	cout << "Testing sample image files" << endl;

	readImage (ILM_IMF_TEST_IMAGEDIR "comp_none.exr", 24988);
	readImage (ILM_IMF_TEST_IMAGEDIR "comp_rle.exr",  24988);
	readImage (ILM_IMF_TEST_IMAGEDIR "comp_zips.exr", 24988);
	readImage (ILM_IMF_TEST_IMAGEDIR "comp_zip.exr",  24988);
	readImage (ILM_IMF_TEST_IMAGEDIR "comp_piz.exr",  24988);
        
        for (int i = 0; i < 5; i++)
        {
            if (IlmThread::supportsThreads ())
            {
                setGlobalThreadCount (i);
                readImage (ILM_IMF_TEST_IMAGEDIR "lineOrder_increasing.exr", 46515);
                readImage (ILM_IMF_TEST_IMAGEDIR "lineOrder_decreasing.exr", 46515);
            }
        }

	cout << "ok\n" << endl;
    }
    catch (const std::exception &e)
    {
	cerr << "ERROR -- caught exception: " << e.what() << endl;
	assert (false);
    }
}
