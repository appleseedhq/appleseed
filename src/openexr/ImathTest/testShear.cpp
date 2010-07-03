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



#include <testShear.h>
#include "ImathShear.h"
#include "ImathLimits.h"
#include "ImathMath.h"
#include <iostream>
#include <assert.h>


using namespace std;


void
testShear ()
{
    cout << "Testing functions in ImathShear.h" << endl;

    cout << "Imath::Shear6 constructors" << endl;

    const float         epsilon = Imath::limits< float >::epsilon();

    float    	        array[6] = { 1.0F, 2.0F, 3.0F, 4.0F, 5.0F, 6.0F };
    Imath::Shear6f    	testConstructor1;
    Imath::Shear6f    	testConstructor2( testConstructor1 );

    testConstructor1 = testConstructor2; 

    Imath::Shear6f    	testConstructor3( 52, 128, 254, 127, 12, -20 );
    Imath::Shear6f    	A( testConstructor3 );
    Imath::Shear6f    	B = A;
    Imath::Shear6f    	X, Y, tmp;

    assert ( A == B );

    cout << "Imath::Shear6 * f" << endl;

    assert ( ( Imath::Shear6f( 0.330f, 0.710f, 0.010f, 
			       0.999f, -0.531f, -0.012f ) * 0.999f ) ==
    	     Imath::Shear6f( 0.330f * 0.999f,
			     0.710f * 0.999f,
			     0.010f * 0.999f,
			     0.999f * 0.999f,
			    -0.531f * 0.999f,
			    -0.012f * 0.999f ) );

    cout << "Imath::Shear6 / f" << endl;

    assert ( ( Imath::Shear6f( 0.330f, 0.710f, 0.010f, 
			       0.999f, -0.531f, -0.012f ) / 0.999f ) ==
    	     Imath::Shear6f( 0.330f / 0.999f,
			     0.710f / 0.999f,
			     0.010f / 0.999f,
			     0.999f / 0.999f,
			    -0.531f / 0.999f,
			    -0.012f / 0.999f ) );

    cout << "Assignment and comparison" << endl;

    B = A;
    assert( B == A );
    assert( !( B != A ) );

    X = Y = Imath::Shear6f( 0.123f, -0.420f,  0.501f, 
			    0.998f, -0.231f, -0.034f );
    
    X *= 0.001f;
    
    assert( Imath::Math<float>::fabs( ( Y.xy * 0.001f ) - X.xy ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( Y.xz * 0.001f ) - X.xz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.yz * 0.001f ) - X.yz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.yx * 0.001f ) - X.yx ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( Y.zx * 0.001f ) - X.zx ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.zy * 0.001f ) - X.zy ) <= epsilon );

    X = Y = Imath::Shear6f( 0.123f, -0.420f, 0.501f, 
			    0.998f, -0.231f, -0.034f );
    
    X /= -1.001f;
    
    assert( Imath::Math<float>::fabs( ( Y.xy / -1.001f ) - X.xy ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( Y.xz / -1.001f ) - X.xz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.yz / -1.001f ) - X.yz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.yx / -1.001f ) - X.yx ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( Y.zx / -1.001f ) - X.zx ) <= epsilon &&
	    Imath::Math<float>::fabs( ( Y.zy / -1.001f ) - X.zy ) <= epsilon );

    Y = Imath::Shear6f( 0.998f, -0.001f,  0.501f, 1.001f, -0.231f, -0.034f );
    X = Imath::Shear6f( 0.011f, -0.420f, -0.501f, 0.998f, -0.231f, -0.034f );

    tmp = X + Y;
    
    assert( Imath::Math<float>::fabs( ( X.xy + Y.xy ) - tmp.xy ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.xz + Y.xz ) - tmp.xz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yz + Y.yz ) - tmp.yz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yx + Y.yx ) - tmp.yx ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.zx + Y.zx ) - tmp.zx ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.zy + Y.zy ) - tmp.zy ) <= epsilon );

    tmp = X - Y;
    
    assert( Imath::Math<float>::fabs( ( X.xy - Y.xy ) - tmp.xy ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.xz - Y.xz ) - tmp.xz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yz - Y.yz ) - tmp.yz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yx - Y.yx ) - tmp.yx ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.zx - Y.zx ) - tmp.zx ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.zy - Y.zy ) - tmp.zy ) <= epsilon );

    tmp = X * Y;
    
    assert( Imath::Math<float>::fabs( ( X.xy * Y.xy ) - tmp.xy ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.xz * Y.xz ) - tmp.xz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yz * Y.yz ) - tmp.yz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yx * Y.yx ) - tmp.yx ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.zx * Y.zx ) - tmp.zx ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.zy * Y.zy ) - tmp.zy ) <= epsilon );

    tmp = X / Y;
    
    //
    // epsilon doesn't work here.
    //
    assert( Imath::Math<float>::fabs( ( X.xy / Y.xy ) - tmp.xy ) <= 1e-5f &&
    	    Imath::Math<float>::fabs( ( X.xz / Y.xz ) - tmp.xz ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.yz / Y.yz ) - tmp.yz ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.yx / Y.yx ) - tmp.yx ) <= 1e-5f &&
    	    Imath::Math<float>::fabs( ( X.zx / Y.zx ) - tmp.zx ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.zy / Y.zy ) - tmp.zy ) <= 1e-5f );

    tmp = X;
    tmp += Y;
    
    assert( Imath::Math<float>::fabs( ( X.xy + Y.xy ) - tmp.xy ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.xz + Y.xz ) - tmp.xz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yz + Y.yz ) - tmp.yz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yx + Y.yx ) - tmp.yx ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.zx + Y.zx ) - tmp.zx ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.zy + Y.zy ) - tmp.zy ) <= epsilon );

    tmp = X;
    tmp -= Y;
    
    assert( Imath::Math<float>::fabs( ( X.xy - Y.xy ) - tmp.xy ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.xz - Y.xz ) - tmp.xz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yz - Y.yz ) - tmp.yz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yx - Y.yx ) - tmp.yx ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.xz - Y.xz ) - tmp.xz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yz - Y.yz ) - tmp.yz ) <= epsilon );

    tmp = X;
    tmp *= Y;
    
    assert( Imath::Math<float>::fabs( ( X.xy * Y.xy ) - tmp.xy ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.xz * Y.xz ) - tmp.xz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yz * Y.yz ) - tmp.yz ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.yx * Y.yx ) - tmp.yx ) <= epsilon &&
    	    Imath::Math<float>::fabs( ( X.zx * Y.zx ) - tmp.zx ) <= epsilon &&
	    Imath::Math<float>::fabs( ( X.zy * Y.zy ) - tmp.zy ) <= epsilon );

    tmp = X;
    tmp /= Y;
    
    //
    // epsilon doesn't work here.
    //
    assert( Imath::Math<float>::fabs( ( X.xy / Y.xy ) - tmp.xy ) <= 1e-5f &&
    	    Imath::Math<float>::fabs( ( X.xz / Y.xz ) - tmp.xz ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.yz / Y.yz ) - tmp.yz ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.yx / Y.yx ) - tmp.yx ) <= 1e-5f &&
    	    Imath::Math<float>::fabs( ( X.zx / Y.zx ) - tmp.zx ) <= 1e-5f &&
	    Imath::Math<float>::fabs( ( X.zy / Y.zy ) - tmp.zy ) <= 1e-5f );

    cout << "ok\n" << endl;
}
