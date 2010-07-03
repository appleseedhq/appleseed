///////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2004, Industrial Light & Magic, a division of Lucas
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


#include <tmpDir.h>

#include <ImfTiledRgbaFile.h>
#include <ImfArray.h>
#include <ImfThreading.h>
#include "IlmThread.h"
#include "ImathRandom.h"
#include <string>
#include <stdio.h>
#include <assert.h>
#include <vector>
#include <math.h>

using namespace Imf;
using namespace Imath;
using namespace std;

namespace {

void
fillPixels (Array2D<Rgba> &pixels, int w, int h)
{
    for (int y = 0; y < h; ++y)
    {
	for (int x = 0; x < w; ++x)
	{
	    Rgba &p = pixels[y][x];

	    p.r = 0.5 + 0.5 * sin (0.1 * x + 0.1 * y);
	    p.g = 0.5 + 0.5 * sin (0.1 * x + 0.2 * y);
	    p.b = 0.5 + 0.5 * sin (0.1 * x + 0.3 * y);
	    p.a = (p.r + p.b + p.g) / 3.0;
	}
    }
}


void
writeReadRGBAONE (const char fileName[],
	       int width,
	       int height,
	       RgbaChannels channels,
	       Compression comp,
           int xSize, int ySize)
{
    cout << "levelMode 0" <<
            ", compression " << comp <<
            ", tileSize " << xSize << "x" << ySize << endl;

    Header header (width, height);
    header.lineOrder() = INCREASING_Y;
    header.compression() = comp;
    
    Array2D<Rgba> p1 (height, width);

    {
        cout << " writing" << flush;
 
        remove (fileName);
        TiledRgbaOutputFile out (fileName, header, channels,
                                 xSize, ySize, ONE_LEVEL);
        
        fillPixels (p1, width, height);
        out.setFrameBuffer (&p1[0][0], 1, width);
        out.writeTiles (0, out.numXTiles() - 1, 0, out.numYTiles() - 1);
    }

    {
        cout << " reading" << flush;

        TiledRgbaInputFile in (fileName);
        const Box2i &dw = in.dataWindow();

        int w = dw.max.x - dw.min.x + 1;
        int h = dw.max.y - dw.min.y + 1;
        int dwx = dw.min.x;
        int dwy = dw.min.y;

        Array2D<Rgba> p2 (h, w);
        in.setFrameBuffer (&p2[-dwy][-dwx], 1, w);
        in.readTiles (0, in.numXTiles() - 1, 0, in.numYTiles() - 1);

        cout << " comparing" << endl << flush;               

        assert (in.displayWindow() == header.displayWindow());
        assert (in.dataWindow() == header.dataWindow());
        assert (in.pixelAspectRatio() == header.pixelAspectRatio());
        assert (in.screenWindowCenter() == header.screenWindowCenter());
        assert (in.screenWindowWidth() == header.screenWindowWidth());
        assert (in.lineOrder() == header.lineOrder());
        assert (in.compression() == header.compression());
        assert (in.channels() == channels);

        for (int y = 0; y < h; ++y)
        {
            for (int x = 0; x < w; ++x)
            {
		if (channels & WRITE_R)
		    assert (p2[y][x].r == p1[y][x].r);
		else
		    assert (p2[y][x].r == 0);

		if (channels & WRITE_G)
		    assert (p2[y][x].g == p1[y][x].g);
		else
		    assert (p2[y][x].g == 0);

		if (channels & WRITE_B)
		    assert (p2[y][x].b == p1[y][x].b);
		else
		    assert (p2[y][x].b == 0);

		if (channels & WRITE_A)
		    assert (p2[y][x].a == p1[y][x].a);
		else
		    assert (p2[y][x].a == 1);
            }
        }
    }

    remove (fileName);
}


void
writeReadRGBAMIP (const char fileName[],
	       int width,
	       int height,
	       RgbaChannels channels,
	       Compression comp,
           int xSize, int ySize)
{
    cout << "levelMode 1" <<
            ", compression " << comp <<
            ", tileSize " << xSize << "x" << ySize << endl;

    Header header (width, height);
    header.lineOrder() = INCREASING_Y;
    header.compression() = comp;
    
    Array < Array2D<Rgba> > levels;

    {
        cout << " writing" << flush;

        remove (fileName);
        TiledRgbaOutputFile out (fileName, header, channels,
                                 xSize, ySize, MIPMAP_LEVELS, ROUND_DOWN);
        
        int numLevels = out.numLevels();
	levels.resizeErase (numLevels);

        for (int level = 0; level < out.numLevels(); ++level)
        {
            int levelWidth  = out.levelWidth(level);
            int levelHeight = out.levelHeight(level);
            levels[level].resizeErase(levelHeight, levelWidth);
            fillPixels (levels[level], levelWidth, levelHeight);
        
            out.setFrameBuffer (&(levels[level])[0][0], 1, levelWidth);
            out.writeTiles (0, out.numXTiles(level) - 1,
                            0, out.numYTiles(level) - 1, level);
        }
    }

    {
        cout << " reading" << flush;

        TiledRgbaInputFile in (fileName);
        const Box2i &dw = in.dataWindow();
        int dwx = dw.min.x;
        int dwy = dw.min.y;

        int numLevels = in.numLevels();
        Array < Array2D<Rgba> > levels2 (numLevels);
        
        for (int level = 0; level < numLevels; ++level)
        {
            int levelWidth = in.levelWidth(level);
            int levelHeight = in.levelHeight(level);
            levels2[level].resizeErase(levelHeight, levelWidth);
        
            in.setFrameBuffer (&(levels2[level])[-dwy][-dwx], 1, levelWidth);
            in.readTiles (0, in.numXTiles(level) - 1,
                          0, in.numYTiles(level) - 1, level);
        }
        
        cout << " comparing" << endl << flush;

        assert (in.displayWindow() == header.displayWindow());
        assert (in.dataWindow() == header.dataWindow());
        assert (in.pixelAspectRatio() == header.pixelAspectRatio());
        assert (in.screenWindowCenter() == header.screenWindowCenter());
        assert (in.screenWindowWidth() == header.screenWindowWidth());
        assert (in.lineOrder() == header.lineOrder());
        assert (in.compression() == header.compression());
        assert (in.channels() == channels);

        for (int l = 0; l < numLevels; ++l)
        {
            for (int y = 0; y < in.levelHeight(l); ++y)
            {
                for (int x = 0; x < in.levelWidth(l); ++x)
                {
                    if (channels & WRITE_R)
                        assert ((levels2[l])[y][x].r == (levels[l])[y][x].r);
                    else
                        assert ((levels2[l])[y][x].r == 0);

                    if (channels & WRITE_G)
                        assert ((levels2[l])[y][x].g == (levels[l])[y][x].g);
                    else
                        assert ((levels2[l])[y][x].g == 0);

                    if (channels & WRITE_B)
                        assert ((levels2[l])[y][x].b == (levels[l])[y][x].b);
                    else
                        assert ((levels2[l])[y][x].b == 0);

                    if (channels & WRITE_A)
                        assert ((levels2[l])[y][x].a == (levels[l])[y][x].a);
                    else
                        assert ((levels2[l])[y][x].a == 1);
                }
            }
        }
    }

    remove (fileName);
}


void
writeReadRGBARIP (const char fileName[],
	       int width,
	       int height,
	       RgbaChannels channels,
	       Compression comp,
           int xSize, int ySize)
{
    cout << "levelMode 2" <<
            ", compression " << comp <<
            ", tileSize " << xSize << "x" << ySize << endl;

    Header header (width, height);
    header.lineOrder() = INCREASING_Y;
    header.compression() = comp;
    
    Array2D < Array2D<Rgba> > levels;

    {
        cout << " writing" << flush;

        remove (fileName);
        TiledRgbaOutputFile out (fileName, header, channels,
                                 xSize, ySize, RIPMAP_LEVELS, ROUND_UP);

	levels.resizeErase (out.numYLevels(), out.numXLevels());

        for (int ylevel = 0; ylevel < out.numYLevels(); ++ylevel)
        {            
            for (int xlevel = 0; xlevel < out.numXLevels(); ++xlevel)
            {
                int levelWidth = out.levelWidth(xlevel);
                int levelHeight = out.levelHeight(ylevel);
                levels[ylevel][xlevel].resizeErase(levelHeight, levelWidth);
                fillPixels (levels[ylevel][xlevel], levelWidth, levelHeight);

                out.setFrameBuffer (&(levels[ylevel][xlevel])[0][0], 1,
                                    levelWidth); 
                out.writeTiles (0, out.numXTiles(xlevel) - 1,
                                0, out.numYTiles(ylevel) - 1, xlevel, ylevel);
            }
        }
    }

    {
        cout << " reading" << flush;

        TiledRgbaInputFile in (fileName);
        const Box2i &dw = in.dataWindow();
        int dwx = dw.min.x;
        int dwy = dw.min.y;        
        
        int numXLevels = in.numXLevels();
        int numYLevels = in.numYLevels();
	Array2D < Array2D<Rgba> > levels2 (numYLevels, numXLevels);
        
        for (int ylevel = 0; ylevel < numYLevels; ++ylevel)
        {
            for (int xlevel = 0; xlevel < numXLevels; ++xlevel)
            {
                int levelWidth  = in.levelWidth(xlevel);
                int levelHeight = in.levelHeight(ylevel);
                levels2[ylevel][xlevel].resizeErase(levelHeight, levelWidth);
                in.setFrameBuffer (&(levels2[ylevel][xlevel])[-dwy][-dwx], 1,
                                   levelWidth);
                                   
                in.readTiles (0, in.numXTiles(xlevel) - 1,
                              0, in.numYTiles(ylevel) - 1, xlevel, ylevel);
            }
        }

        cout << " comparing" << endl << flush;

        assert (in.displayWindow() == header.displayWindow());
        assert (in.dataWindow() == header.dataWindow());
        assert (in.pixelAspectRatio() == header.pixelAspectRatio());
        assert (in.screenWindowCenter() == header.screenWindowCenter());
        assert (in.screenWindowWidth() == header.screenWindowWidth());
        assert (in.lineOrder() == header.lineOrder());
        assert (in.compression() == header.compression());
        assert (in.channels() == channels);

        for (int ly = 0; ly < numYLevels; ++ly)
        {
            for (int lx = 0; lx < numXLevels; ++lx)
            {
                for (int y = 0; y < in.levelHeight(ly); ++y)
                {
                    for (int x = 0; x < in.levelWidth(lx); ++x)
                    {
                        if (channels & WRITE_R)
                            assert ((levels2[ly][lx])[y][x].r ==
                                    (levels[ly][lx])[y][x].r);
                        else
                            assert ((levels2[ly][lx])[y][x].r == 0);

                        if (channels & WRITE_G)
                            assert ((levels2[ly][lx])[y][x].g ==
                                    (levels[ly][lx])[y][x].g);
                        else
                            assert ((levels2[ly][lx])[y][x].g == 0);

                        if (channels & WRITE_B)
                            assert ((levels2[ly][lx])[y][x].b ==
                                    (levels[ly][lx])[y][x].b);
                        else
                            assert ((levels2[ly][lx])[y][x].b == 0);

                        if (channels & WRITE_A)
                            assert ((levels2[ly][lx])[y][x].a ==
                                    (levels[ly][lx])[y][x].a);
                        else
                            assert ((levels2[ly][lx])[y][x].a == 1);
                    }
                }
            }
        }
    }

    remove (fileName);
}


void
writeRead (int W, int H, Compression comp, int xSize, int ySize)
{
    const char *filename = IMF_TMP_DIR "imf_test_tiled_rgba.exr";

    writeReadRGBAONE (filename, W, H, WRITE_RGBA, comp, xSize, ySize);
    writeReadRGBAMIP (filename, W, H, WRITE_RGBA, comp, xSize, ySize);
    writeReadRGBARIP (filename, W, H, WRITE_RGBA, comp, xSize, ySize);
}


void
writeReadIncomplete ()
{
    cout << "\nfile with missing and broken tiles" << endl;

    const char *fileName = IMF_TMP_DIR "imf_test_tiled_incomplete.exr";

    //
    // Write a file where every other tile is missing or broken.
    // Then try read the file and verify that all existing good
    // tiles can actually be read.
    //

    const int width = 400;
    const int height = 300;
    const int tileXSize = 30;
    const int tileYSize = 40;

    Array2D<Rgba> p1 (height, width);

    for (int y = 0; y < height; ++y)
	for (int x = 0; x < width; ++x)
	    p1[y][x] = Rgba (x % 5, x % 17, y % 23, y % 29);

    {
        cout << "writing" << endl;
 
        remove (fileName);

	Header header (width, height);
	header.lineOrder() = RANDOM_Y;

        TiledRgbaOutputFile out (fileName, header, WRITE_RGBA,
                                 tileXSize, tileYSize, ONE_LEVEL);
        
        out.setFrameBuffer (&p1[0][0], 1, width);

	out.writeTile (0, 0);

	for (int tileY = 0; tileY < out.numYTiles(); ++tileY)
	    for (int tileX = 0; tileX < out.numXTiles(); ++tileX)
		if ((tileX + tileY) & 1)
		    out.writeTile (tileX, tileY);

	out.writeTile (2, 0);

	out.breakTile (0, 0, 0, 0, 25, 10, 0xff);	// destroy tiles
	out.breakTile (2, 0, 0, 0, 25, 10, 0xff);	// (0,0) and (2,0)
    }

    {
	Array2D<Rgba> p2 (height, width);

	for (int y = 0; y < height; ++y)
	    for (int x = 0; x < width; ++x)
		p2[y][x] = Rgba (-1, -1, -1, -1);

        cout << "reading one tile at a time," << flush;

        TiledRgbaInputFile in (fileName);
        const Box2i &dw = in.dataWindow();

        assert (dw.max.x - dw.min.x + 1 == width);
        assert (dw.max.y - dw.min.y + 1 == height);
	assert (dw.min.x == 0);
	assert (dw.min.y == 0);
	
        in.setFrameBuffer (&p2[0][0], 1, width);

	for (int tileY = 0; tileY < in.numYTiles(); ++tileY)
	{
	    for (int tileX = 0; tileX < in.numXTiles(); ++tileX)
	    {
		bool tilePresent = true;
		bool tileBroken = false;

		try
		{
		    in.readTile (tileX, tileY);
		}
		catch (const Iex::InputExc &)
		{
		    tilePresent = false;	// tile is missing
		}
		catch (const Iex::IoExc &)
		{
		    tileBroken = true;		// tile cannot be decoded
		}

		assert (tileBroken || (tilePresent == ((tileX + tileY) & 1)));
	    }
	}

	cout << " comparing" << endl << flush;               

	for (int y = 0; y < height; ++y)
	{
	    int tileY = y / tileYSize;

	    for (int x = 0; x < width; ++x)
	    {
		int tileX = x / tileXSize;

		const Rgba &s = p1[y][x];
		const Rgba &t = p2[y][x];

		if ((tileX + tileY) & 1)
		{
		    assert (t.r == s.r &&
		            t.g == s.g &&
			    t.b == s.b &&
			    t.a == s.a);
		}
		else
		{
		    assert (t.r == -1 &&
			    t.g == -1 &&
			    t.b == -1 &&
			    t.a == -1);
		}
	    }
	}
    }

    {
	Array2D<Rgba> p2 (height, width);

	for (int y = 0; y < height; ++y)
	    for (int x = 0; x < width; ++x)
		p2[y][x] = Rgba (-1, -1, -1, -1);

        cout << "reading multiple tiles at a time," << flush;

        TiledRgbaInputFile in (fileName);
        const Box2i &dw = in.dataWindow();

        assert (dw.max.x - dw.min.x + 1 == width);
        assert (dw.max.y - dw.min.y + 1 == height);
	assert (dw.min.x == 0);
	assert (dw.min.y == 0);
	
        in.setFrameBuffer (&p2[0][0], 1, width);

	for (int tileY = 0; tileY < in.numYTiles(); ++tileY)
	{
	    bool tilesMissing = false;
	    bool tilesBroken = false;

	    try
	    {
		in.readTiles (0, in.numXTiles() - 1, tileY, tileY);
	    }
	    catch (const Iex::InputExc &)
	    {
		tilesMissing = true;
	    }
	    catch (const Iex::IoExc &)
	    {
		tilesBroken = true;
	    }

	    assert (tilesMissing || tilesBroken);
	}

	cout << " comparing" << endl << flush;               

	for (int y = 0; y < height; ++y)
	{
	    for (int x = 0; x < width; ++x)
	    {
		const Rgba &s = p1[y][x];
		const Rgba &t = p2[y][x];

		assert ((t.r == -1  && t.g == -1  && t.b == -1  && t.a == -1) ||
			(t.r == s.r && t.g == s.g && t.b == s.b && t.a == s.a));
	    }
	}
    }

    remove (fileName);
}

} // namespace


void
testTiledRgba ()
{
    try
    {
        cout << "Testing the tiled RGBA image interface" << endl;

	int maxThreads = IlmThread::supportsThreads()? 3: 0;

	for (int n = 0; n <= maxThreads; ++n)
	{
	    if (IlmThread::supportsThreads())
	    {
		setGlobalThreadCount (n);
		cout << "\nnumber of threads: " << globalThreadCount() << endl;
	    }

	    const int W[] = { 9, 69, 75, 80 };
	    const int H[] = { 7, 50, 52, 55 };

	    for (int i = 0; i < 4; ++i)
	    {
		cout << "\nImage size = " << W[i] << " x " << H[i] << endl;

		for (int comp = 0; comp < NUM_COMPRESSION_METHODS; ++comp)
		{
		    //
		    // for tiled files, ZIPS and ZIP are the same thing
		    //

		    if (comp == ZIP_COMPRESSION)
			comp++;

		    if (i == 0)
		    {
			//
			// for single-pixel tiles, we don't gain anything
			// by testing multiple image sizes (and singe-pixel
			// tiles are rather slow anyway)
			//

			writeRead (W[i], H[i], Compression (comp), 1, 1);
		    }

		    writeRead (W[i], H[i], Compression (comp), 35, 26);
		    writeRead (W[i], H[i], Compression (comp), 75, 52);
		    writeRead (W[i], H[i], Compression (comp), 264, 129);
		}
	    }

	    writeReadIncomplete();
	}

        cout << "ok\n" << endl;
    }
    catch (const std::exception &e)
    {
        cerr << "ERROR -- caught exception: " << e.what() << endl;
        assert (false);
    }
}
