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
//	exrdisplay -- a simple program to display Imf::Rgba images
//
//-----------------------------------------------------------------------------

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Value_Slider.H>
#include <FL/Fl_Check_Button.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Valuator.H>

#include "OpenEXRConfig.h"
#include <ImageView.h>
#ifdef HAVE_FRAGMENT_SHADERS
#include <ImageViewFragShader.h>
#endif
#include <ImfArray.h>
#include <loadImage.h>
#include <scaleImage.h>

#include <iostream>
#include <algorithm>
#include <string>
#include <exception>
#include <string.h>
#include <stdlib.h>

using namespace Imath;
using namespace Imf;
using namespace std;


struct MainWindow
{
    Fl_Window *			window;
    Fl_Box *			exposureLabel;
    Fl_Value_Slider *		exposureSlider;
    Fl_Box *			defogLabel;
    Fl_Value_Slider *		defogSlider;
    Fl_Box *			kneeLowLabel;
    Fl_Value_Slider *		kneeLowSlider;
    Fl_Box *			kneeHighLabel;
    Fl_Value_Slider *		kneeHighSlider;
    ImageView *			image;
    Imf::Array<Imf::Rgba>	pixels;

    static void		exposureSliderCallback (Fl_Widget *widget, void *data);
    static void		defogSliderCallback (Fl_Widget *widget, void *data);
    static void		kneeLowSliderCallback (Fl_Widget *widget, void *data);
    static void		kneeHighSliderCallback (Fl_Widget *widget, void *data);
};


void	
MainWindow::exposureSliderCallback (Fl_Widget *widget, void *data)
{
    MainWindow *mainWindow = (MainWindow *) data;
    mainWindow->image->setExposure (mainWindow->exposureSlider->value());
}


void	
MainWindow::defogSliderCallback (Fl_Widget *widget, void *data)
{
    MainWindow *mainWindow = (MainWindow *) data;
    mainWindow->image->setDefog (mainWindow->defogSlider->value());
}


void	
MainWindow::kneeLowSliderCallback (Fl_Widget *widget, void *data)
{
    MainWindow *mainWindow = (MainWindow *) data;
    mainWindow->image->setKneeLow (mainWindow->kneeLowSlider->value());
}


void	
MainWindow::kneeHighSliderCallback (Fl_Widget *widget, void *data)
{
    MainWindow *mainWindow = (MainWindow *) data;
    mainWindow->image->setKneeHigh (mainWindow->kneeHighSlider->value());
}


MainWindow *
makeMainWindow (const char imageFile[],
		const char channel[],
		bool preview,
		int lx,
		int ly,
		bool noDisplayWindow,
		bool noAspect,
		bool zeroOneExposure,
		bool normalize,
		bool swap,
		bool bloom,
		bool continuousUpdate,
		bool useFragmentShader,
		const char * fragmentShaderName)
{
    MainWindow *mainWindow = new MainWindow;

    //
    // Read the image file.
    //

    Box2i displayWindow;
    Box2i dataWindow;
    float pixelAspect;

    if (preview)
    {
	loadPreviewImage (imageFile,
			  displayWindow,
			  dataWindow,
			  pixelAspect,
			  mainWindow->pixels);
    }
    else if (lx >= 0 || ly >= 0)
    {
	if (channel)
	{
	    loadTiledImageChannel (imageFile,
				   channel,
				   lx, ly,
				   displayWindow,
				   dataWindow,
				   pixelAspect,
				   mainWindow->pixels);
	}
	else
	{
	    loadTiledImage (imageFile,
			    lx, ly,
			    displayWindow,
			    dataWindow,
			    pixelAspect,
			    mainWindow->pixels);
	}
    }
    else
    {
	if (channel)
	{
	    loadImageChannel (imageFile,
			      channel,
			      displayWindow,
			      dataWindow,
			      pixelAspect,
			      mainWindow->pixels);
	}
	else
	{
	    loadImage (imageFile,
		       displayWindow,
		       dataWindow,
		       pixelAspect,
		       mainWindow->pixels);
	}
    }
    
    int w  = displayWindow.max.x - displayWindow.min.x + 1;
    int h  = displayWindow.max.y - displayWindow.min.y + 1;
    int dw = dataWindow.max.x - dataWindow.min.x + 1;
    int dh = dataWindow.max.y - dataWindow.min.y + 1;
    int dx = dataWindow.min.x - displayWindow.min.x;
    int dy = dataWindow.min.y - displayWindow.min.y;

    if (noDisplayWindow)
    {
	w = dw;
	h = dh;
	dx = 0;
	dy = 0;
    }

    if (noAspect)
    {
	pixelAspect = 1;
    }

    //
    // Normalize the pixel data if necessary.
    //

    if (normalize)
	normalizePixels (dw, dh, mainWindow->pixels);

    //
    // If necessary, swap the top and bottom half and then the
    // left and right half of the image.
    // 

    if (swap)
	swapPixels (dw, dh, mainWindow->pixels);

    //
    // Stretch the image horizontally or vertically to make the
    // pixels square (assuming that we are going to display the
    // image on a screen with square pixels).
    //

    if (pixelAspect > 1)
	scaleX (pixelAspect, w, h, dw, dh, dx, dy, mainWindow->pixels);
    else
	scaleY (1 / pixelAspect, w, h, dw, dh, dx, dy, mainWindow->pixels);

    //
    // If necessary, simulate blooming.
    //

    if (bloom)
	addBlooming (dw, dh, mainWindow->pixels);

    //
    // Build main window
    //

    int mw = max (200, w);

    Fl::set_color (FL_GRAY, 150, 150, 150);

    mainWindow->window =
	new Fl_Window (mw + 10, h + 135, imageFile);

    //
    // Add exposure slider
    //

    mainWindow->exposureLabel =
	new Fl_Box (5, 5, 60, 20, "exposure");

    mainWindow->exposureSlider =
	new Fl_Value_Slider (70, 5, mw - 65, 20, "");

    enum Fl_When when = (useFragmentShader || continuousUpdate)?
			    FL_WHEN_CHANGED : FL_WHEN_RELEASE;

    mainWindow->exposureSlider->type (FL_HORIZONTAL);
    mainWindow->exposureSlider->range (-10.0, +10.0);
    mainWindow->exposureSlider->step (1, 8);
    mainWindow->exposureSlider->value (zeroOneExposure? 1.02607: 0.0);
    mainWindow->exposureSlider->when (when);

    mainWindow->exposureSlider->callback
	(MainWindow::exposureSliderCallback, mainWindow);

    //
    // Add defog slider
    //

    mainWindow->defogLabel =
	new Fl_Box (5, 30, 60, 20, "defog");

    mainWindow->defogSlider =
	new Fl_Value_Slider (70, 30, mw - 65, 20, "");

    mainWindow->defogSlider->type (FL_HORIZONTAL);
    mainWindow->defogSlider->range (0.0, 0.01);
    mainWindow->defogSlider->step (1, 10000);
    mainWindow->defogSlider->value (0.0);
    mainWindow->defogSlider->when (when);

    mainWindow->defogSlider->callback
	(MainWindow::defogSliderCallback, mainWindow);

    //
    // Add kneeLow slider
    //

    mainWindow->kneeLowLabel =
	new Fl_Box (5, 55, 60, 20, "knee low");

    mainWindow->kneeLowSlider =
	new Fl_Value_Slider (70, 55, mw - 65, 20, "");

    mainWindow->kneeLowSlider->type (FL_HORIZONTAL);
    mainWindow->kneeLowSlider->range (-3.0, 3.0);
    mainWindow->kneeLowSlider->step (1, 8);
    mainWindow->kneeLowSlider->value (0.0);
    mainWindow->kneeLowSlider->when (when);

    mainWindow->kneeLowSlider->callback
	(MainWindow::kneeLowSliderCallback, mainWindow);

    //
    // Add kneeHigh slider
    //

    mainWindow->kneeHighLabel =
	new Fl_Box (5, 80, 60, 20, "knee high");

    mainWindow->kneeHighSlider =
	new Fl_Value_Slider (70, 80, mw - 65, 20, "");

    mainWindow->kneeHighSlider->type (FL_HORIZONTAL);
    mainWindow->kneeHighSlider->range (3.5, 7.5);
    mainWindow->kneeHighSlider->step (1, 8);
    mainWindow->kneeHighSlider->value ((preview | zeroOneExposure)? 3.5: 5.0);
    mainWindow->kneeHighSlider->when (when);

    mainWindow->kneeHighSlider->callback
	(MainWindow::kneeHighSliderCallback, mainWindow);

    //
    // Add RGB value display
    //

    Fl_Box *rgbaBox = new Fl_Box (80, 105, mw - 65, 20, "");
    rgbaBox->align (FL_ALIGN_LEFT | FL_ALIGN_INSIDE);

    //
    // Add image view:
    //
    // w, h		width and height of the display window
    //
    // dw, dh		width and height of the data window
    //
    // dx, dy		offset of the data window's upper left
    // 			corner from the display window's upper
    // 			left corner
    //

#ifdef HAVE_FRAGMENT_SHADERS
    if (useFragmentShader)
    {
	mainWindow->image =
	    new ImageViewFragShader (5 + (mw - w) / 2, 130, 
				     w, h,
				     "",
				     mainWindow->pixels,
				     dw, dh,
				     dx, dy,
				     rgbaBox,
				     mainWindow->exposureSlider->value(),
				     mainWindow->defogSlider->value(),
				     mainWindow->kneeLowSlider->value(),
				     mainWindow->kneeHighSlider->value(),
				     fragmentShaderName);
    }
    else
    {
	mainWindow->image =
	    new ImageView (5 + (mw - w) / 2, 130, 
			   w, h,
			   "",
			   mainWindow->pixels,
			   dw, dh,
			   dx, dy,
			   rgbaBox,
			   mainWindow->exposureSlider->value(),
			   mainWindow->defogSlider->value(),
			   mainWindow->kneeLowSlider->value(),
			   mainWindow->kneeHighSlider->value());
    }
#else
    mainWindow->image =
	new ImageView (5 + (mw - w) / 2, 130,
		       w, h,
		       "",
		       mainWindow->pixels,
		       dw, dh,
		       dx, dy,
		       rgbaBox,
		       mainWindow->exposureSlider->value(),
		       mainWindow->defogSlider->value(),
		       mainWindow->kneeLowSlider->value(),
		       mainWindow->kneeHighSlider->value());
#endif

    mainWindow->image->box (FL_ENGRAVED_BOX);

    mainWindow->window->end();

    return mainWindow;
}


void
usageMessage (const char argv0[], bool verbose = false)
{
    cerr << "usage: " << argv0 << " [options] imagefile" << endl;

    if (verbose)
    {
	cerr << "\n"
	        "Displays an OpenEXR image on the screen.\n"
		"\n"
		"Options:\n"
		"\n"
		"-p        displays the preview (thumbnail)\n"
		"          image instead of the main image\n"
		"\n"
		"-l lx ly  displays level (lx,ly) of a tiled\n"
		"          multiresolution image\n"
		"\n"
		"-w        displays all pixels in the data window,\n"
		"          ignoring the display window\n"
		"\n"
		"-a        ignores the image's pixel aspect ratio,\n"
		"          and does not scale the image to make\n"
		"          the pixels square\n"
		"\n"
		"-c x      loads only image channel x\n"
		"\n"
		"-1        sets exposure and knee sliders so that pixel\n"
		"          value 0.0 becomes black, and 1.0 becomes white\n"
		"\n"
		"-n        normalizes the pixels so that the smallest\n"
		"          value becomes 0.0 and the largest value\n"
		"          becomes 1.0\n"
		"\n"
		"-A        same as -c A -1 (displays alpha)\n"
		"\n"
		"-Z        same as -c Z -n (displays depth)\n"
		"\n"
		"-s        swaps the image's top and bottom half, then\n"
		"          swaps the left and right half, so that the\n"
		"          four corners of the image end up in the center.\n"
		"          (Useful for checking the seams of wrap-around\n"
		"          texture map images.)\n"
		"\n"
		"-b        approximates the blooming effect that occurs\n"
		"          when high-contrast images are recorded on\n"
		"          photographic film.  Blooming reduces aliasing\n"
		"          in computer-generated images that contain very\n"
		"          bright areas adjacent to dark areas.\n"
		"\n"
		"-u        changing the exposure and knee sliders\n"
		"          continuously updates the on-screen image\n"
		"\n"
		"-h        prints this message\n";

	 cerr << endl;
    }

    exit (1);
}


int
main(int argc, char **argv)
{
    const char *imageFile = 0;
    const char *channel = 0;
    bool preview = false;
    bool noDisplayWindow = false;
    bool noAspect = false;
    bool zeroOneExposure = false;
    bool normalize = false;
    bool swap = false;
    bool bloom = false;
    bool continuousUpdate = false;
    bool useFragmentShader = false;
    const char * fragmentShaderName = 0;
    
    int lx = -1;
    int ly = -1;

    //
    // Parse the command line.
    //

    if (argc < 2)
	usageMessage (argv[0], true);

    int i = 1;

    while (i < argc)
    {
	if (!strcmp (argv[i], "-p"))	
	{
	    //
	    // Show the preview image
	    //

	    preview = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-l"))
	{
	    //
	    // Assume that the image file is tiled,
	    // and show level (lx,ly) of the tiled image
	    //

	    if (i > argc - 3)
		usageMessage (argv[0]);

	    lx = strtol (argv[i + 1], 0, 0);
	    ly = strtol (argv[i + 2], 0, 0);
	    i += 3;
	}
	else if (!strcmp (argv[i], "-w"))
	{
	    //
	    // Ignore the file's display window
	    //

	    noDisplayWindow = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-a"))
	{
	    //
	    // Ignore the file's pixel aspect ratio
	    //

	    noAspect = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-c"))
	{
	    //
	    // Load only one image channel.
	    //

	    if (i > argc - 2)
		usageMessage (argv[0]);

	    channel = argv[i + 1];
	    i += 2;
	}
	else if (!strcmp (argv[i], "-1"))
	{
	    //
	    // Display 0.0 to 1.0 range.
	    //

	    zeroOneExposure = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-n"))
	{
	    //
	    // Normalize pixels.
	    //

	    zeroOneExposure = true;
	    normalize = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-A"))
	{
	    //
	    // Display alpha
	    //

	    zeroOneExposure = true;
	    normalize = false;
	    channel = "A";
	    i += 1;
	}
	else if (!strcmp (argv[i], "-Z"))
	{
	    //
	    // Display depth
	    //

	    zeroOneExposure = true;
	    normalize = true;
	    channel = "Z";
	    i += 1;
	}
	else if (!strcmp (argv[i], "-s"))
	{
	    //
	    // Swap top and bottom half, then left and right half.
	    //

	    swap = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-b"))
	{
	    //
	    // Simulate blooming.
	    //

	    bloom = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-u"))
	{
	    //
	    // Continuous update.
	    //

	    continuousUpdate = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-f"))
	{
	    //
	    // Use the built-in fragment shader.
	    //

	    useFragmentShader = true;
	    i += 1;
	}
	else if (!strcmp (argv[i], "-fn"))
	{
	    //
	    // Use the specified fragment shader.
	    //

	    useFragmentShader = true;
	    fragmentShaderName = argv[i + 1];
	    i += 2;
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
	    // image file name
	    //

	    imageFile = argv[i];
	    i += 1;
	}
    }

    if (imageFile == 0)
	usageMessage (argv[0]);

#ifndef HAVE_FRAGMENT_SHADERS
    if (useFragmentShader)
    {
	cerr << argv[0] << " was not compiled with fragment shader "
	    "support," << endl;
	cerr << "falling back to software" << endl;
	useFragmentShader = false;
	fragmentShaderName = 0;
    }
#endif

    //
    // Load the specified image file,
    // open a window on the screen, and
    // display the image.
    //

    int exitStatus = 0;

    try
    {
	MainWindow *mainWindow = makeMainWindow (imageFile,
						 channel,
						 preview,
						 lx, ly,
						 noDisplayWindow,
						 noAspect,
						 zeroOneExposure,
						 normalize,
						 swap,
						 bloom,
						 continuousUpdate,
						 useFragmentShader,
						 fragmentShaderName ?
						 fragmentShaderName : "");

	mainWindow->window->show (1, argv);
	exitStatus = Fl::run();
    }
    catch (const exception &e)
    {
	cerr << e.what() << endl;
	exitStatus = 1;
    }

    return exitStatus;
}
