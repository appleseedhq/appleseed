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



#include <testFrustum.h>
#include "ImathFrustum.h"
#include "ImathFun.h"
#include <iostream>
#include <assert.h>


using namespace std;


void
testFrustum ()
{
    cout << "Testing functions in ImathFrustum.h";

    cout << "\nperspective ";

    float n = 1.7;
    float f = 567.0;
    float l = -3.5;
    float r = 2.0;
    float b = -1.3;
    float t = 0.9;

    Imath::Frustum<float> frustum (n, f, l, r, t, b, false);

    assert (Imath::abs<float> (frustum.fovx() -
			       (atan2(r,n) - atan2(l,n))) < 1e-6);
    assert (Imath::abs<float> (frustum.fovy() -
			       (atan2(t,n) - atan2(b,n))) < 1e-6);
    cout << "1";
    assert (Imath::abs<float> (frustum.aspect() - ((r-l)/(t-b))) < 1e-6);
    cout << "2";

    Imath::M44f m = frustum.projectionMatrix();
    assert (Imath::abs<float> (m[0][0] - ((2*n)/(r-l)))	    < 1e-6 &&
	    Imath::abs<float> (m[0][1])			    < 1e-6 &&
	    Imath::abs<float> (m[0][2])			    < 1e-6 &&
	    Imath::abs<float> (m[0][3])			    < 1e-6 &&
	    Imath::abs<float> (m[1][0])			    < 1e-6 &&
	    Imath::abs<float> (m[1][1] - ((2*n)/(t-b)))	    < 1e-6 &&
	    Imath::abs<float> (m[1][2])			    < 1e-6 &&
	    Imath::abs<float> (m[1][3])			    < 1e-6 &&
	    Imath::abs<float> (m[2][0] - ((r+l)/(r-l)))	    < 1e-6 &&
	    Imath::abs<float> (m[2][1] - ((t+b)/(t-b)))	    < 1e-6 &&
	    Imath::abs<float> (m[2][2] - (-(f+n)/(f-n)))    < 1e-6 &&
	    Imath::abs<float> (m[2][3] - -1.0)		    < 1e-6 &&
	    Imath::abs<float> (m[3][0])			    < 1e-6 &&
	    Imath::abs<float> (m[3][1])			    < 1e-6 &&
	    Imath::abs<float> (m[3][2] - ((-2*f*n)/(f-n)))  < 1e-6 &&
	    Imath::abs<float> (m[3][3])			    < 1e-6);
    cout << "3";

    cout << "\nexceptions ";
    Imath::Frustum<float> badFrustum;

    badFrustum.set (n, n, l, r, t, b, false);
    try
    {
	(void)badFrustum.projectionMatrix();
	assert (!"near == far didn't throw an exception");
    }
    catch (Iex::DivzeroExc) {}
    cout << "1";

    badFrustum.set (n, f, l, l, t, b, false);
    try
    {
	(void)badFrustum.projectionMatrix();
	assert (!"left == right didn't throw an exception");
    }
    catch (Iex::DivzeroExc) {}
    cout << "2";

    badFrustum.set (n, f, l, r, t, t, false);
    try
    {
	(void)badFrustum.projectionMatrix();
	assert (!"top == bottom didn't throw an exception");
    }
    catch (Iex::DivzeroExc) {}
    cout << "3";

    cout << "\northographic ";

    frustum.setOrthographic (true);

    m = frustum.projectionMatrix();
    assert (Imath::abs<float> (m[0][0] - (2/(r-l)))	    < 1e-6 &&
	    Imath::abs<float> (m[0][1])			    < 1e-6 &&
	    Imath::abs<float> (m[0][2])			    < 1e-6 &&
	    Imath::abs<float> (m[0][3])			    < 1e-6 &&
	    Imath::abs<float> (m[1][0])			    < 1e-6 &&
	    Imath::abs<float> (m[1][1] - (2/(t-b)))	    < 1e-6 &&
	    Imath::abs<float> (m[1][2])			    < 1e-6 &&
	    Imath::abs<float> (m[1][3])			    < 1e-6 &&
	    Imath::abs<float> (m[2][0])			    < 1e-6 &&
	    Imath::abs<float> (m[2][1])			    < 1e-6 &&
	    Imath::abs<float> (m[2][2] - (-2/(f-n)))	    < 1e-6 &&
	    Imath::abs<float> (m[2][3])			    < 1e-6 &&
	    Imath::abs<float> (m[3][0] - (-(r+l)/(r-l)))    < 1e-6 &&
	    Imath::abs<float> (m[3][1] - (-(t+b)/(t-b)))    < 1e-6 &&
	    Imath::abs<float> (m[3][2] - (-(f+n)/(f-n)))    < 1e-6 &&
	    Imath::abs<float> (m[3][3] - 1.0)		    < 1e-6);
    cout << "1";

    

    // TODO - There are many little functions in Imath::Frustum which
    // aren't tested here.  Those test should be added.  But this is
    // a start.

    Imath::Frustum<float> f1 (n, f, l, r, t, b, false);
    Imath::Frustum<float> f2 (n, f, l, r, t, b, true);
    assert (f1 != f2);
    f2.set(n + 0.1, f, l, r, t, b, false);
    assert (f1 != f2);
    f2.set(n, f + 0.1, l, r, t, b, false);
    assert (f1 != f2);
    f2.set(n, f, l + 0.1, r, t, b, false);
    assert (f1 != f2);
    f2.set(n, f, l, r + 0.1, t, b, false);
    assert (f1 != f2);
    f2.set(n, f, l, r, t + 0.1, b, false);
    assert (f1 != f2);
    f2.set(n, f, l, r, t, b + 0.1, false);
    assert (f1 != f2);
    cout << "\npassed inequality test";

    f1 = f2;
    assert (f1 == f2);
    cout << "\npassed equality test";

    cout << "\nok\n\n";
}
