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



#include <testColor.h>
#include "ImathColor.h"
#include "ImathColorAlgo.h"
#include "ImathLimits.h"
#include "ImathMath.h"
#include <iostream>
#include <assert.h>


using namespace std;


void
testColor ()
{
    cout << "Testing functions in ImathColor.h & ImathColorAlgo.h" << endl;

    cout << "rgb2packed -> packed2rgb" << endl;

    const float         epsilon = Imath::limits< float >::epsilon();

    Imath::PackedColor	packed;
    Imath::C3c	    	in3( 52, 128, 254 );
    Imath::C3c	    	out3;
    
    packed = Imath::rgb2packed( in3 );
    Imath::packed2rgb( packed, out3 );

    assert ( in3 == out3 );

    Imath::C4c	    	testConstructor1;
    Imath::C4c	    	testConstructor2( testConstructor1 );

    testConstructor1 = testConstructor2; // use these so the compiler doesn't emit a warning

    Imath::C4c	    	testConstructor3( 52, 128, 254, 127 );
    Imath::C4c	    	A( testConstructor3 );
    Imath::C4c	    	B;
    Imath::C4f	    	X, Y, tmp;

    packed = Imath::rgb2packed( A );
    Imath::packed2rgb( packed, B );

    assert ( A == B );

    cout << "Imath::Color4 * f" << endl;

    assert ( ( Imath::C4f( 0.330f, 0.710f, 0.010f, 0.999f ) * 0.999f ) ==
    	     Imath::C4f( 0.330f * 0.999f,
	     	    	 0.710f * 0.999f,
			 0.010f * 0.999f,
			 0.999f * 0.999f ) );

    cout << "Imath::Color4 / f" << endl;

    assert ( ( Imath::C4f( 0.330f, 0.710f, 0.010f, 0.999f ) / 0.999f ) ==
    	     Imath::C4f( 0.330f / 0.999f,
	     	    	 0.710f / 0.999f,
			 0.010f / 0.999f,
			 0.999f / 0.999f ) );

    cout << "Assignment and comparison" << endl;

    B = A;
    assert( B == A );
    assert( !( B != A ) );

    X = Y = Imath::C4f( 0.123f, -0.420f, 0.501f, 0.998f );
    
    X *= 0.001f;
    
    assert( Imath::Math<float>::fabs( ( Y.r * 0.001f ) - X.r ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( Y.g * 0.001f ) - X.g ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.b * 0.001f ) - X.b ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.a * 0.001f ) - X.a ) <= epsilon );

    X = Y = Imath::C4f( 0.123f, -0.420f, 0.501f, 0.998f );
    
    X /= -1.001f;
    
    assert( Imath::Math<float>::fabs( ( Y.r / -1.001f ) - X.r ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( Y.g / -1.001f ) - X.g ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.b / -1.001f ) - X.b ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.a / -1.001f ) - X.a ) <= epsilon );

    Y = Imath::C4f( 0.998f, -0.001f,  0.501f, 1.001f );
    X = Imath::C4f( 0.011f, -0.420f, -0.501f, 0.998f );

    tmp = X + Y;
    
    assert( Imath::Math<float>::fabs( ( X.r + Y.r ) - tmp.r ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.g + Y.g ) - tmp.g ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.b + Y.b ) - tmp.b ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.a + Y.a ) - tmp.a ) <= epsilon );

    tmp = X - Y;
    
    assert( Imath::Math<float>::fabs( ( X.r - Y.r ) - tmp.r ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.g - Y.g ) - tmp.g ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.b - Y.b ) - tmp.b ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.a - Y.a ) - tmp.a ) <= epsilon );

    tmp = X * Y;
    
    assert( Imath::Math<float>::fabs( ( X.r * Y.r ) - tmp.r ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.g * Y.g ) - tmp.g ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.b * Y.b ) - tmp.b ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.a * Y.a ) - tmp.a ) <= epsilon );

    tmp = X / Y;
    
    //
    // epsilon doesn't work here.
    //
    assert( Imath::Math<float>::fabs( ( X.r / Y.r ) - tmp.r ) <= 1e-5f &&
    	    Imath::Math<float>::fabs( ( X.g / Y.g ) - tmp.g ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.b / Y.b ) - tmp.b ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.a / Y.a ) - tmp.a ) <= 1e-5f );

    tmp = X;
    tmp += Y;
    
    assert( Imath::Math<float>::fabs( ( X.r + Y.r ) - tmp.r ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.g + Y.g ) - tmp.g ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.b + Y.b ) - tmp.b ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.a + Y.a ) - tmp.a ) <= epsilon );

    tmp = X;
    tmp -= Y;
    
    assert( Imath::Math<float>::fabs( ( X.r - Y.r ) - tmp.r ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.g - Y.g ) - tmp.g ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.b - Y.b ) - tmp.b ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.a - Y.a ) - tmp.a ) <= epsilon );

    tmp = X;
    tmp *= Y;
    
    assert( Imath::Math<float>::fabs( ( X.r * Y.r ) - tmp.r ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.g * Y.g ) - tmp.g ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.b * Y.b ) - tmp.b ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.a * Y.a ) - tmp.a ) <= epsilon );

    tmp = X;
    tmp /= Y;
    
    //
    // epsilon doesn't work here.
    //
    assert( Imath::Math<float>::fabs( ( X.r / Y.r ) - tmp.r ) <= 1e-5f &&
    	    Imath::Math<float>::fabs( ( X.g / Y.g ) - tmp.g ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.b / Y.b ) - tmp.b ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.a / Y.a ) - tmp.a ) <= 1e-5f );

    cout << "ok\n" << endl;
}
