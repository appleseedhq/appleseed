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

//
// Authors:
//   Simon Green <SGreen@nvidia.com>
//   Drew Hess <dhess@ilm.com>
//

//----------------------------------------------------------------------------
//
//	class ImageViewFragShader
//
//----------------------------------------------------------------------------

#include "OpenEXRConfig.h"
#include <ImageViewFragShader.h>

#ifdef HAVE_DARWIN
#include <OpenGL/glu.h>
#else
#include <GL/glu.h>
#endif
#define GL_GLEXT_PROTOTYPES
#include <GL/glext.h>

#include <iostream>
#include <stdlib.h>
#include <string.h>
#include "ImathFun.h"
#include "ImathMath.h"
#include "Iex.h"

//
// XXX dhess - hack
//

extern "C" {
void glGenProgramsNV(GLsizei n, GLuint *ids);
void glBindProgramNV(GLenum target, GLuint id);
void glLoadProgramNV(GLenum target, GLuint id, GLsizei len, const GLubyte
 *program);
void APIENTRY glProgramNamedParameter4fNV(GLuint id, GLsizei len, const GLubyte *name, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
}

ImageViewFragShader::ImageViewFragShader (int x, int y,
					  int width, int height,
					  const char label[],
					  const Imf::Rgba pixels[],
					  int dw, int dh,
					  int dx, int dy,
					  Fl_Box *rgbaBox,
					  float exposure,
					  float defog,
					  float kneeLow,
					  float kneeHigh,
					  const std::string & filename)
:
    ImageView (x, y,
	       width, height,
	       label,
	       pixels,
	       dw, dh,
	       dx, dy,
	       rgbaBox,
	       exposure,
	       defog,
	       kneeLow, 
	       kneeHigh),
    _fsFilename (filename),
    _useSoftware (false)
{
}


bool
ImageViewFragShader::initGL ()
{
    bool status = true;

    try
    {
#if 0	
	if (!glh_init_extensions("GL_ARB_multitexture "
				 "GL_NV_vertex_program "
				 "GL_NV_fragment_program "))
	{
	    THROW (Iex::BaseExc, "GL extensions required for fragment " <<
		   "shader support are not available: " << std::endl <<
		   "    " << glh_get_unsupported_extensions ());
	}	    
#endif

	if (_fsFilename.empty ())
	    loadBuiltinFragShader ();
	else
	    loadFragShaderFromFile ();

	loadTexture ();
    }
    catch (const Iex::BaseExc & e)
    {
	std::cerr << e.what () << std::endl;
	std::cerr << "Falling back to software rendering." << std::endl;
	status = false;
    }

    return status;
}


void
ImageViewFragShader::loadTexture ()
{
    //
    // load OpenEXR image as OpenGL texture
    //

    GLenum target = GL_TEXTURE_RECTANGLE_NV;
    glGenTextures (1, &_texture);
    glBindTexture (target, _texture);
    glTexParameteri (target, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri (target, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri (target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri (target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    
    glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
    
    glTexImage2D (target, 0, GL_FLOAT_RGBA16_NV, _dw, _dh, 0, 
		  GL_RGBA, GL_HALF_FLOAT_NV, _rawPixels);
    
    //
    // Check for errors
    //
    
    GLenum error = glGetError ();
    if (error != GL_NO_ERROR)
    {
	do
	{
	    std::cerr << "Error loading texture: " <<
		(char *) gluErrorString (error) << std::endl;
	} while ((error = glGetError ()) != GL_NO_ERROR);
	
	THROW (Iex::BaseExc, "Unable to load OpenEXR image as a texture.");
    }
    
}


void
ImageViewFragShader::draw()
{
    if (!valid())
    {
	if (!initGL ())
	    _useSoftware = true;

	//
	// Set up camera.
	//

	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	glViewport (0, 0, w (), h ());
	glOrtho (0, w (), h (), 0, -1, 1);
	glMatrixMode (GL_MODELVIEW);
	glLoadIdentity ();
    }

    if (_useSoftware)
    {
	ImageView::updateScreenPixels ();
	ImageView::draw ();
	return;
    }

    glClearColor (0.3, 0.3, 0.3, 1.0);
    glClear (GL_COLOR_BUFFER_BIT);

    //
    // Set fragment program parameters.  See ImageView.cpp for details
    // on what the parameters mean.
    //

    setCgNamedParameter ("exposure",
			Imath::Math<float>::pow (2, _exposure + 2.47393));
    setCgNamedParameter ("defog", _defog * _fogR, _defog * _fogG, 
			 _defog * _fogB);
    setCgNamedParameter ("gamma", 0.4545f);
    setCgNamedParameter ("grayTarget", 84.66/255.0);

    float kl = Imath::Math<float>::pow (2, _kneeLow);

    setCgNamedParameter ("kneeLow", kl);
    setCgNamedParameter ("kneeF", findKnee (Imath::Math<float>::pow (2, _kneeHigh) - kl,
					    Imath::Math<float>::pow (2, 3.5) - kl));

    //
    // draw a textured quad
    //

    glEnable (GL_FRAGMENT_PROGRAM_NV);
    glEnable (GL_TEXTURE_RECTANGLE_NV);
    glBegin (GL_QUADS);
    glTexCoord2f (0.0, 0.0); glVertex2f(0.0, 0.0);
    glTexCoord2f (_dw, 0.0); glVertex2f(_dw, 0.0);
    glTexCoord2f (_dw, _dh); glVertex2f(_dw, _dh);
    glTexCoord2f (0.0, _dh); glVertex2f(0.0, _dh);
    glEnd ();
    glDisable (GL_TEXTURE_RECTANGLE_NV);
    glDisable (GL_FRAGMENT_PROGRAM_NV);
}


void
ImageViewFragShader::loadFragShaderFromFile ()
{
    FILE * f = 0;
    char * code = 0;

    try
    {
	f = fopen (_fsFilename.c_str (), "rb");
	if (!f)
	{
	    THROW_ERRNO ("Can't load fragment shader file " << _fsFilename <<
			 " (%T)");
	}

	fseek (f, 0, SEEK_END);
	int len = ftell (f);
	fseek (f, 0, SEEK_SET);
	
	code = new char[len + 1];
	
	int read = fread (code, 1, len, f);
	if (read != len)
	{
	    THROW (Iex::BaseExc, "Expected " << len << " bytes in fragment " <<
	       "shader file " << _fsFilename << ", only got " << read);
	}

	code[read] = '\0';    // null-terminate

	if (!loadFragShader (code))
	{
	    THROW (Iex::BaseExc, "Can't download fragment shader " << 
		   _fsFilename);
	}

	delete [] code;
	fclose (f);
    }
    catch (Iex::BaseExc & e)
    {
	delete [] code;
	if (f)
	    fclose (f);

	throw e;
    }
}


namespace
{

//
// This is the built-in fragment shader.  It was generated by
// running:
//
//   cgc -profile fp30 -o exrdisplay.fp30 exrdisplay.cg
//
// and then trimming the comments out and formatting it as a C string.
//

const char *
theFragShader =
    "!!FP1.0\n" \
    "DECLARE defog;\n" \
    "DECLARE exposure;\n" \
    "DECLARE gamma;\n" \
    "DECLARE zerovec;\n" \
    "DECLARE grayTarget;\n" \
    "DECLARE kneeLow;\n" \
    "DECLARE kneeF;\n" \
    "TEX  H0.xyz, f[TEX0].xyxx, TEX0, RECT;\n" \
    "ADDR R0.xyz, H0.xyzx, -defog.xyzx;\n" \
    "MAXH H0.xyz, zerovec.xyzx, R0.xyzx;\n" \
    "MULR R0.xyz, H0.xyzx, exposure.x;\n" \
    "ADDH H0.xyz, R0.xyzx, -kneeLow.x;\n" \
    "MOVH H0.w, {1, 1, 1}.x;\n" \
    "MADH H0.xyz, H0.xyzx, kneeF.x, H0.w;\n" \
    "LG2H H0.w, H0.x;\n" \
    "MOVH H1.x, H0.w;\n" \
    "LG2H H0.w, H0.y;\n" \
    "MOVH H1.y, H0.w;\n" \
    "LG2H H0.x, H0.z;\n" \
    "MOVH H1.z, H0.x;\n" \
    "MULH H1.xyz, H1.xyzx, {0.69335938, 0.69335938, 0.69335938}.x;\n" \
    "RCPH H0.x, kneeF.x;\n" \
    "MADH H0.xyz, H1.xyzx, H0.x, kneeLow.x;\n" \
    "MOVR H1.xyz, R0.xyzx;\n" \
    "SGTH H2.xyz, R0.xyzx, kneeLow.x;\n" \
    "MOVXC HC.xyz, H2.xyzx;\n" \
    "MOVH H1.xyz(GT.xyzx), H0.xyzx;\n" \
    "POWH H0.x, H1.x, gamma.x;\n" \
    "POWH H0.w, H1.y, gamma.x;\n" \
    "MOVH H0.y, H0.w;\n" \
    "POWH H0.w, H1.z, gamma.x;\n" \
    "MOVH H0.z, H0.w;\n" \
    "MULH H0.xyz, H0.xyzx, grayTarget.x;\n" \
    "MOVH o[COLH].xyz, H0.xyzx;\n" \
    "END\n" \
    "\0";

}


void
ImageViewFragShader::loadBuiltinFragShader ()
{
    //
    // Use the built-in fragment shader
    //

    if (!loadFragShader (theFragShader))
	THROW (Iex::BaseExc, "There was a roblem downloading built-in " <<
	       "fragment shader.");
}


bool
ImageViewFragShader::loadFragShader (const char * code)
{
    int len = strlen (code);
    
    //
    // load the fragment shader into the card
    //
    
    glGenProgramsNV (1, &_fprog);
    glBindProgramNV (GL_FRAGMENT_PROGRAM_NV, _fprog);
    glLoadProgramNV (GL_FRAGMENT_PROGRAM_NV, _fprog, len, 
		     (const GLubyte *) code);

    //
    // check for errors
    //
    
    GLint errpos;
    glGetIntegerv (GL_PROGRAM_ERROR_POSITION_NV, &errpos);
    if (errpos != -1)
    {
	std::cerr << "Fragment shader error:" << std::endl << std::endl;
	
	int begin = (errpos - 20) < 0 ? 0 : errpos - 20;
	char errtxt[81];
	
	strncpy (errtxt, code + begin, 80);
	errtxt[80] = '\0';
	
	std::cerr << errtxt << std::endl << std::endl;
    }

    return errpos == -1;
}


void
ImageViewFragShader::setCgNamedParameter (const char * name,
					  float x,
					  float y,
					  float z,
					  float w)
{
    glProgramNamedParameter4fNV (_fprog, strlen (name), (GLubyte *) name,
				 x, y, z, w);
}
