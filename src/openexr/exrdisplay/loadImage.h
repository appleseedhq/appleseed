#ifndef INCLUDED_LOAD_IMAGE_H
#define INCLUDED_LOAD_IMAGE_H

//////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2004, Industrial Light & Magic, a division of Lucasfilm
// Entertainment Company Ltd.  Portions contributed and copyright held by
// others as indicated.  All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above
//       copyright notice, this list of conditions and the following
//       disclaimer.
//
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided with
//       the distribution.
//
//     * Neither the name of Industrial Light & Magic nor the names of
//       any other contributors to this software may be used to endorse or
//       promote products derived from this software without specific prior
//       written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------
//
//	Functions to load OpenEXR images into a pixel array.
//
//----------------------------------------------------------------------------

#include <ImfRgba.h>
#include <ImfArray.h>
#include "ImathBox.h"


//
// Load "the image" from the specified image file.
// If the file is tiled, load the level (0,0) image.
//

void	loadImage (const char fileName[],
		   Imath::Box2i &displayWindow,
		   Imath::Box2i &dataWindow,
		   float &pixelAspectRatio,
		   Imf::Array<Imf::Rgba> &pixels);

//
// For a tiled image file, load the level (lx, ly) image.
//

void	loadTiledImage (const char fileName[],
			int lx,
			int ly,
			Imath::Box2i &displayWindow,
			Imath::Box2i &dataWindow,
		        float &pixelAspectRatio,
			Imf::Array<Imf::Rgba> &pixels);

//
// Load the preview image from the specified image file.
//

void	loadPreviewImage (const char fileName[],
			  Imath::Box2i &displayWindow,
			  Imath::Box2i &dataWindow,
			  float &pixelAspectRatio,
			  Imf::Array<Imf::Rgba> &pixels);

//
// Load only the specified channel.
//

void	loadImageChannel (const char fileName[],
			  const char channelName[],
			  Imath::Box2i &displayWindow,
			  Imath::Box2i &dataWindow,
			  float &pixelAspectRatio,
			  Imf::Array<Imf::Rgba> &pixels);

//
// Load only the specified channel of level (lx, ly).
//

void	loadTiledImageChannel (const char fileName[],
			       const char channelName[],
			       int lx,
			       int ly,
			       Imath::Box2i &displayWindow,
			       Imath::Box2i &dataWindow,
			       float &pixelAspectRatio,
			       Imf::Array<Imf::Rgba> &pixels);

#endif
