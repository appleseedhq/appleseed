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


//-----------------------------------------------------------------------------
//
//	exrenvmap -- makes OpenEXR environment maps
//
//-----------------------------------------------------------------------------

#include <makeCubeMap.h>
#include <makeLatLongMap.h>
#include <ImfEnvmap.h>
#include <iostream>
#include <exception>
#include <stdlib.h>

using namespace Imf;
using namespace std;


namespace {

void
usageMessage (const char argv0[], bool verbose = false)
{
    cerr << "usage: " << argv0 << " [options] infile outfile" << endl;

    if (verbose)
    {
	cerr << "\n"
		"Converts OpenEXR latitude-longitude environment maps\n"
		"into cube-face environment maps or vice versa.\n"
		"Reads an environment map image from infile, converts\n"
		"it, and stores the result in outfile.\n"
		"\n"
		"Options:\n"
		"\n"
		"-o         produces a ONE_LEVEL output file (default)\n"
		"\n"
		"-m         produces a MIPMAP_LEVELS output file\n"
		"\n"
		"-c         the output file will be a cube-face environment\n"
	        "           map (default)\n"
		"\n"
		"-l         the output file will be a latitude-longitude\n"
	        "           environment map\n"
		"\n"
		"-w x       sets the width of the output image to x pixels\n"
	        "           (default is 256).  The height of the output image\n"
	        "           will be x*6 pixels for a cube-face map, or x/2\n"
	        "           pixels for a latitude-longitude map.\n"
		"\n"
		"-f r n     sets the antialiasing filter radius to r\n"
	        "           (default is 1.0) and the sampling rate to\n"
	        "           n by n (default is 5 by 5).  Increasing r\n"
		"           makes the output image blurrier; decreasing r\n"
		"           makes the image sharper but may cause aliasing.\n"
		"           Increasing n improves antialiasing, but\n"
		"           generating the output image takes longer.\n"
		"\n"
		"-t x y     sets the output file's tile size to x by y pixels\n"
	        "           (default is 64 by 64)\n"
		"\n"
		"-p t b     if the input image is a latitude-longitude map,\n"
	        "           pad the image at the top and bottom with t*h\n"
	        "           and b*h extra scan lines, where h is the height\n"
	        "           of the input image.  This is useful for images\n"
	        "           from 360-degree panoramic scans that cover\n"
	        "           less than 180 degrees vertically.\n"
		"\n"
		"-d         sets level size rounding to ROUND_DOWN (default)\n"
		"\n"
		"-u         sets level size rounding to ROUND_UP\n"
		"\n"
		"-z x       sets the data compression method to x\n"
		"           (none/rle/zip/piz/pxr24, default is zip)\n"
		"\n"
		"-v         verbose mode\n"
		"\n"
		"-h         prints this message\n";

	 cerr << endl;
    }

    exit (1);
}


Compression
getCompression (const string &str)
{
    Compression c;

    if (str == "no" || str == "none" || str == "NO" || str == "NONE")
    {
	c = NO_COMPRESSION;
    }
    else if (str == "rle" || str == "RLE")
    {
	c = RLE_COMPRESSION;
    }
    else if (str == "zip" || str == "ZIP")
    {
	c = ZIP_COMPRESSION;
    }
    else if (str == "piz" || str == "PIZ")
    {
	c = PIZ_COMPRESSION;
    }
    else if (str == "pxr24" || str == "PXR24")
    {
	c = PXR24_COMPRESSION;
    }
    else
    {
	cerr << "Unknown compression method \"" << str << "\"." << endl;
	exit (1);
    }

    return c;
}

} // namespace


int
main(int argc, char **argv)
{
    const char *inFile = 0;
    const char *outFile = 0;
    Envmap type = ENVMAP_CUBE;
    LevelMode levelMode = ONE_LEVEL;
    LevelRoundingMode roundingMode = ROUND_DOWN;
    Compression compression = ZIP_COMPRESSION;
    int mapWidth = 256;
    int tileWidth = 64;
    int tileHeight = 64;
    float padTop = 0;
    float padBottom = 0;
    float filterRadius = 1;
    int numSamples = 5;
    bool verbose = false;

    //
    // Parse the command line.
    //

    if (argc < 2)
	usageMessage (argv[0], true);

    int i = 1;

    while (i < argc)
    {
	if (!strcmp (argv[i], "-o"))	
	{
	    //
	    // generate a ONE_LEVEL image
	    //

	    levelMode = ONE_LEVEL;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-m"))
	{
	    //
	    // Generate a MIPMAP_LEVELS image
	    //

	    levelMode = MIPMAP_LEVELS;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-c"))
	{
	    //
	    // Generate a cube-face map
	    //

	    type = ENVMAP_CUBE;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-l"))
	{
	    //
	    // Generate a latitude-longitude map
	    //

	    type = ENVMAP_LATLONG;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-w"))
	{
	    //
	    // Set output image width
	    //

	    if (i > argc - 2)
		usageMessage (argv[0]);

	    mapWidth = strtol (argv[i + 1], 0, 0);

	    if (mapWidth <= 0)
	    {
		cerr << "Output image width must be greater than zero." << endl;
		return 1;
	    }

	    i += 2;
	}
	else if (!strcmp (argv[i], "-f"))
	{
	    //
	    // Set filter radius and supersampling rate
	    //

	    if (i > argc - 3)
		usageMessage (argv[0]);

	    filterRadius = strtod (argv[i + 1], 0);
	    numSamples   = strtol (argv[i + 2], 0, 0);

	    if (filterRadius < 0)
	    {
		cerr << "Filter radius must not be less than zero." << endl;
		return 1;
	    }

	    if (numSamples <= 0)
	    {
		cerr << "Sampling rate must be greater than zero." << endl;
		return 1;
	    }

	    i += 3;
	}
	else if (!strcmp (argv[i], "-t"))
	{
	    //
	    // Set tile size
	    //

	    if (i > argc - 3)
		usageMessage (argv[0]);

	    tileWidth  = strtol (argv[i + 1], 0, 0);
	    tileHeight = strtol (argv[i + 2], 0, 0);

	    if (tileWidth <= 0 || tileHeight <= 0)
	    {
		cerr << "Tile size must be greater than zero." << endl;
		return 1;
	    }

	    i += 3;
	}
	else if (!strcmp (argv[i], "-p"))
	{
	    //
	    // Set top and bottom padding
	    //

	    if (i > argc - 3)
		usageMessage (argv[0]);

	    padTop    = strtod (argv[i + 1], 0);
	    padBottom = strtod (argv[i + 2], 0);

	    if (padTop < 0 || padBottom < 0)
	    {
		cerr << "Padding must not be less than zero." << endl;
		return 1;
	    }

	    i += 3;
	}
	else if (!strcmp (argv[i], "-d"))
	{
	    //
	    // Round down
	    //

	    roundingMode = ROUND_DOWN;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-u"))
	{
	    //
	    // Round down
	    //

	    roundingMode = ROUND_UP;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-z"))
	{
	    //
	    // Set compression method
	    //

	    if (i > argc - 2)
		usageMessage (argv[0]);

	    compression = getCompression (argv[i + 1]);
	    i += 2;
	}
	else if (!strcmp (argv[i], "-v"))
	{
	    //
	    // Verbose mode
	    //

	    verbose = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-h"))
	{
	    //
	    // Print help message
	    //

	    usageMessage (argv[0], true);
	}
	else
	{
	    //
	    // Image file name
	    //

	    if (inFile == 0)
		inFile = argv[i];
	    else
		outFile = argv[i];

	    i += 1;
	}
    }

    if (inFile == 0 || outFile == 0)
	usageMessage (argv[0]);

    //
    // Load inFile, convert it, and save the result in outFile.
    //

    int exitStatus = 0;

    try
    {
	if (type == ENVMAP_CUBE)
	{
	    makeCubeMap (inFile, outFile,
			 tileWidth, tileHeight,
			 levelMode, roundingMode,
			 compression, mapWidth,
			 padTop, padBottom,
			 filterRadius, numSamples,
			 verbose);
	}
	else
	{
	    makeLatLongMap (inFile, outFile,
			    tileWidth, tileHeight,
			    levelMode, roundingMode,
			    compression, mapWidth,
			    padTop, padBottom,
			    filterRadius, numSamples,
			    verbose);
	}
    }
    catch (const exception &e)
    {
	cerr << e.what() << endl;
	exitStatus = 1;
    }

    return exitStatus;
}
