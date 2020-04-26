
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/memory/alignedallocator.h"
#include "foundation/memory/poolallocator.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <complex>
#include <cstdarg>
#include <cstddef>
#include <cstring>
#include <deque>
#include <list>
#include <map>
#include <queue>
#include <set>
#include <stack>
#include <string>
#include <utility>
#include <vector>

using namespace foundation;

TEST_SUITE(StlAllocatorTestbed)
{
    #pragma warning (push)
    #pragma warning (disable: 4127)     // ignore conditional expression constant
    #pragma warning (disable: 4702)     // ignore unreachable code
    #pragma warning (disable: 4800)     // allow forcing int to bool

#if defined __GNUC__ && __GNUC__ >= 8
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wclass-memaccess"
#endif

    ///////////////////////////////////////////////////////////////////////////////
    //
    // StlAllocatorTestOutput.h
    //
    // Copyright (c) 2003 Pete Isensee (PKIsensee@msn.com).
    // All rights reserved worldwide.
    //
    // Permission to copy, modify, reproduce or redistribute this source code is
    // granted provided the above copyright notice is retained in the resulting
    // source code.
    //
    // This software is provided "as is" and without any express or implied
    // warranties.
    //
    // Description:
    //
    //    Error and output functions

    //-----------------------------------------------------------------------------

    // Forces the compiler to believe that 'a' cannot be optimized away
    #define USED(a) Used( 0, &a )

    // Verifies the expression
    #define VERIFY(b) { if (!(b)) throw Exception("VERIFY(" #b ") failed"); }

    // Output error message; includes file and line number
    #define OUTERR(e) { throw Exception(e); }

    //-----------------------------------------------------------------------------

    // This function forces the compiler to believe that the optional parameters
    // are important and cannot be optimized away
    void Used( int i, ... )
    {
        va_list Marker;
        va_start( Marker, i );
        i = va_arg( Marker, int );
        va_end( Marker );
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // StlAllocatorTestPolicy.h
    //
    // Copyright (c) 2003 Pete Isensee (PKIsensee@msn.com).
    // All rights reserved worldwide.
    //
    // Permission to copy, modify, reproduce or redistribute this source code is
    // granted provided the above copyright notice is retained in the resulting
    // source code.
    //
    // This software is provided "as is" and without any express or implied
    // warranties.
    //
    // Description:
    //
    //    Where allocator testing policy is defined

    //-----------------------------------------------------------------------------

    // The following template policies allow certain portions of the testbed
    // to be enabled or disabled. For instance, most allocators can be default
    // constructed. However, some custom allocators cannot be default constructed.

    template< typename Allocator >
    struct Policy
    {
        // Construct an allocator using the default constructor
        // Parameter is not used, but required for specialization
        static Allocator Construct( const Allocator& )
        {
            return Allocator();
        };

        // Construct a container using the container default constructor
        // Parameter is not used, but required for specialization
        template< typename Container >
        static Container GetDefault( const Allocator& )
        {
            return Container();
        };

        // Construct a set using the default set constructor
        // Separate function since specializations may be required
        template< typename Set >
        static Set GetDefaultSet( const Allocator& )
        {
            return Set();
        };

        // Construct a map using the default map constructor
        // Separate function since specializations may be required
        template< typename Map >
        static Map GetDefaultMap( const Allocator& )
        {
            return Map();
        };

        // Construct an adapter (stack, queue) using the default adapter constructor
        // Separate function since specializations may be required
        template< typename Adapter, typename Container >
        static Adapter GetDefaultAdapter( const Allocator& )
        {
            return Adapter();
        }

        // Construct a priority queue using the default priority queue constructor
        // Separate function since specializations may be required
        template< typename PriorityQueue, typename Container >
        static PriorityQueue GetDefaultPriorityQueue( const Allocator& )
        {
            return PriorityQueue();
        }

        // Construct a container by specifying the allocator
        template< typename Container >
        static Container GetCopied( const Allocator& a )
        {
            return Container( a );
        }

        // Construct a set by specifying the allocator
        template< typename Set >
        static Set GetCopiedSet( const Allocator& a )
        {
            // Sets require a comparison functor prior to the allocator
            // object in the container ctor
            return Set( std::less< typename Allocator::value_type >(), a );
        }

        // Construct a map by specifying the allocator
        template< typename Map >
        static Map GetCopiedMap( const Allocator& aIn )
        {
            // map allocators are pair allocators
            typedef std::pair< const typename Allocator::value_type, int > Pair;
            typedef typename Allocator::template rebind< Pair >::other Alloc;
            Alloc a( aIn );

            // Maps require a comparison functor prior to the allocator
            // object in the map ctor.
            return Map( std::less< typename Allocator::value_type >(), a );
        }

        // Construct an adapter (stack, queue) by specifying the allocator
        template< typename Adapter, typename Container >
        static Adapter GetCopiedAdapter( const Allocator& a )
        {
            // Adapters take the container in the constructor
            return Adapter( Container( a ) );
        }

        // Construct a priority queue by specifying the allocator
        template< typename PriorityQueue, typename Container >
        static PriorityQueue GetCopiedPriorityQueue( const Allocator& a )
        {
            // Priority queues require a comparison functor prior to the
            // allocator object in the container ctor.
            return PriorityQueue( std::less< typename Allocator::value_type >(), Container( a ) );
        }

    };

    // Any allocator that requires constructor parameters can use this macro
    // immediately following the inclusion of this header to disable the
    // portion of the testbed that constructs allocators using the default
    // constructor. Example: SET_DEFAULT_CONSTRUCTABLE_OFF( MyAllocator )

    #define SET_DEFAULT_CONSTRUCTABLE_OFF( x )              \
    template< typename T >                                  \
    struct Policy< x<T> >                                   \
    {                                                       \
        static x<T> Construct( const x<T>& a )              \
        {                                                   \
            return x<T>(a);                                 \
        };                                                  \
                                                            \
        template< typename Container >                      \
        static Container GetDefault( const x<T>& a )        \
        {                                                   \
            return GetCopied< Container >( a );             \
        };                                                  \
                                                            \
        template< typename Set >                            \
        static Set GetDefaultSet( const x<T>& a )           \
        {                                                   \
            return GetCopiedSet< Set >( a );                \
        };                                                  \
                                                            \
        template< typename Map >                            \
        static Map GetDefaultMap( const x<T>& a )           \
        {                                                   \
            return GetCopiedMap< Map >( a );                \
        };                                                  \
                                                            \
        template< typename Adapter, typename Container >    \
        static Adapter GetDefaultAdapter( const x<T>& a )   \
        {                                                   \
            return GetCopiedAdapter< Adapter, Container >( a ); \
        }                                                   \
                                                            \
        template< typename PriorityQueue, typename Container > \
        static PriorityQueue GetDefaultPriorityQueue( const x<T>& a ) \
        {                                                   \
            return GetCopiedPriorityQueue< PriorityQueue, Container >( a ); \
        }                                                   \
                                                            \
        template< typename Container >                      \
        static Container GetCopied( const x<T>& a )         \
        {                                                   \
            return Container( a );                          \
        }                                                   \
                                                            \
        template< typename Set >                            \
        static Set GetCopiedSet( const x<T>& a )            \
        {                                                   \
            return Set( std::less< T >(), a );              \
        }                                                   \
                                                            \
        template< typename Map >                            \
        static Map GetCopiedMap( const x<T>& aIn )          \
        {                                                   \
            typedef std::pair< const T, int > Pair;         \
            typedef typename x<T>::template rebind< Pair >::other Alloc; \
            Alloc a( aIn );                                 \
            return Map( std::less< T >(), a );              \
        }                                                   \
                                                            \
        template< typename Adapter, typename Container >    \
        static Adapter GetCopiedAdapter( const x<T>& a )    \
        {                                                   \
            return Adapter( Container( a ) );               \
        }                                                   \
                                                            \
        template< typename PriorityQueue, typename Container > \
        static PriorityQueue GetCopiedPriorityQueue( const x<T>& a ) \
        {                                                   \
            return PriorityQueue( std::less< T >(), Container( a ) ); \
        }                                                   \
    };

    ///////////////////////////////////////////////////////////////////////////////
    //
    // StlAllocatorTestTypes.h
    //
    // Copyright (c) 2003 Pete Isensee (PKIsensee@msn.com).
    // All rights reserved worldwide.
    //
    // Permission to copy, modify, reproduce or redistribute this source code is
    // granted provided the above copyright notice is retained in the resulting
    // source code.
    //
    // This software is provided "as is" and without any express or implied
    // warranties.
    //
    // Description:
    //
    //    Types used for testing allocators. Includes some simple types as well as
    //    objects designed to put maximum stress on allocators. All types can be
    //    constructed from integers, which eases testing containers that only support
    //    unique elements, like set and map.

    //-----------------------------------------------------------------------------

    // This test class contains plain old data types (PODs) only

    class C
    {
        char   c;
        short  s;
        int    i;
        long   l;
        float  f;
        double d;

    public:

        C()
        :
            c( '0' ),
            s( 0 ),
            i( 0 ),
            l( 0L ),
            f( 0.0f ),
            d( 0.0 )
        {
            Verify( *this );
        }

        C( const C& x )
        :
            c( x.c ),
            s( x.s ),
            i( x.i ),
            l( x.l ),
            f( x.f ),
            d( x.d )
        {
            Verify( *this );
        }

        explicit C( int in )
        :
            c( '0' ),
            s( 0 ),
            i( in ),
            l( 0L ),
            f( 0.0f ),
            d( 0.0 )
        {
            Verify( *this );
        }

        C& operator=( const C& x )
        {
            Verify( x );
            Verify( *this );

            C temp( x ); // Exceptional C++ copy assignment swap trick; Item 13
            std::swap( c, temp.c );
            std::swap( s, temp.s );
            std::swap( i, temp.i );
            std::swap( l, temp.l );
            std::swap( f, temp.f );
            std::swap( d, temp.d );

            return *this;
        }

        ~C()
        {
            Verify( *this );
        }

        // Comparable so it can be used in sets and maps
        bool operator<( const C& x ) const
        {
            Verify( x );
            Verify( *this );
            return i < x.i;
        }

        // Hashable so it can be used in hash containers
        operator size_t() const
        {
            Verify( *this );
            return size_t( &c );
        }

        bool operator==( const C& x ) const
        {
            return c == x.c &&
                    s == x.s &&
                    i == x.i &&
                    l == x.l &&
                    f == x.f &&
                    d == x.d;
        }

        void Verify( const C& x ) const
        {
            VERIFY( x.c == '0'  );
            VERIFY( x.s == 0    );
            VERIFY( x.i >= 0    );  // assumes outside callers never exceed 100
            VERIFY( x.i <= 100  );
            VERIFY( x.l == 0L   );
            VERIFY( x.f == 0.0f );
            VERIFY( x.d == 0.0  );
        }

    };

    //-----------------------------------------------------------------------------

    // This test class has non-trivial constructors, assignment op and destructor

    class D
    {
        char* p;

    public:

        D()
        :
            p( new char( 'p' ) )
        {
            Verify( *this );
        }

        D( const D& d )
        :
            p( new char( *d.p ) )
        {
            Verify( d );
        }

        explicit D( int i )
        :
            p( new char( static_cast<char>(i) ) )
        {
            Verify( *this );
        }

        D& operator=( const D& d )
        {
            Verify( d );
            Verify( *this );
            D temp( d ); // Exceptional C++ copy assignment swap trick; Item 13
            std::swap( p, temp.p );
            return *this;
        }

        ~D()
        {
            Verify( *this );
            delete p;
        }

        // Comparable so it can be used in sets and maps
        bool operator<( const D& d ) const
        {
            Verify( d );
            Verify( *this );
            return *p < *d.p;
        }

        // Hashable so it can be used in hash containers
        operator size_t() const
        {
            Verify( *this );
            return size_t( p );
        }

        bool operator==( const D& d ) const
        {
            return *p == *d.p;
        }

        void Verify( const D& d ) const
        {
            VERIFY( *d.p == 'p' || // assumes outside callers never exceed 100
                    ( *d.p >= 0 && *d.p <= 100 ) );
        }

    };

    //-----------------------------------------------------------------------------

    // A collection of a variety of types

    class E
    {
        std::string         s;
        std::complex<float> c;
        int                 i;
        double              d;

    public:

        E()
        :
            s( "0123456789abcxyz" ), // enough to exceed the small string optimization
            c( 0.1f, 2.3f ),
            i( 0x12345678 ),
            d( 123.456789 )
        {
            Verify( *this );
        }

        E( const E& e )
        :
            s( e.s ), // enough to exceed the small string optimization
            c( e.c ),
            i( e.i ),
            d( e.d )
        {
            Verify( *this );
        }

        explicit E( int in )
        :
            s( "0123456789abcxyz" ), // enough to exceed the small string optimization
            c( 0.1f, 2.3f ),
            i( in ),
            d( 123.456789 )
        {
            Verify( *this );
        }

        E& operator=( const E& e )
        {
            Verify( e );
            Verify( *this );
            E temp( e ); // Exceptional C++ copy assignment swap trick; Item 13
            std::swap( s, temp.s );
            std::swap( c, temp.c );
            std::swap( i, temp.i );
            std::swap( d, temp.d );
            return *this;
        }

        ~E()
        {
            Verify( *this );
        }

        // Comparable so it can be used in sets and maps
        bool operator<( const E& e ) const
        {
            Verify( e );
            Verify( *this );
            return i < e.i;
        }

        // Hashable so it can be used in hash containers
        operator size_t() const
        {
            Verify( *this );

            // Use the string pointer, since it's guaranteed unique
            return size_t( s.c_str() );
        }

        bool operator==( const E& e ) const
        {
            return s == e.s &&
                    c == e.c &&
                    i == e.i &&
                    d == e.d;
        }

        void Verify( const E& e ) const
        {
            VERIFY( e.s == "0123456789abcxyz" );
            VERIFY( e.c == std::complex<float>( 0.1f, 2.3f ) );
            VERIFY( e.i == 0x12345678 || // assumes outside callers never exceed 100
                    ( e.i >= 0 && e.i <= 100 ) );
            VERIFY( e.d == 123.456789 );
        }

    };

    ///////////////////////////////////////////////////////////////////////////////
    //
    // StlAllocatorTestMembers.h
    //
    // Copyright (c) 2003 Pete Isensee (PKIsensee@msn.com).
    // All rights reserved worldwide.
    //
    // Permission to copy, modify, reproduce or redistribute this source code is
    // granted provided the above copyright notice is retained in the resulting
    // source code.
    //
    // This software is provided "as is" and without any express or implied
    // warranties.
    //
    // Description:
    //
    //    Tests all required allocator member functions.

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestMemberFunctionPresence( const Allocator& a )
    {
        // Typedefs
        typename Allocator::size_type       st;
        typename Allocator::difference_type dt;
        typename Allocator::pointer         p;
        typename Allocator::const_pointer   cp;
        typename Allocator::value_type      v;
        typename Allocator::reference       r = v;   // Note: must be initialized
        typename Allocator::const_reference cr = v;  // Note: must be initialized

        // allocator()
        Allocator b( Policy< Allocator >::Construct( a ) );
        USED( b );

        // allocator( const allocator& )
        Allocator c(a);

        // allocator( const allocator<U>& ) and rebind
        typename Allocator::template rebind< C >::other d( c );

        // Although operator=() may be implemented by an allocator, it is
        // not required by the C++ Standard, nor is it to be used by
        // container implementations.

        // address( reference )
        p = c.address( r );

        // address( const_reference )
        cp = c.address( cr );

        // difference_type
        dt = p - cp;

        // max_size()
        st = c.max_size();

        // allocate( size_type )
        p = c.allocate( 1 );

        // deallocate( pointer, size_type )
        c.deallocate( p, 1 );

        // allocate( size_type, allocator<void>::const_pointer )
        p = c.allocate( 1, nullptr );

        // construct( pointer, const T& )
        c.construct( p, v );

        // destroy( pointer )
        c.destroy( p );

        // clean up
        c.deallocate( p, 1 );

        // operator ==( const A&, const A& )
        bool f1 = ( a == c );

        // operator ==( const A&, const B& )
        bool f2 = ( a == d );

        // operator !=( const A&, const A& )
        bool f3 = ( a != c );

        // operator !=( const A&, const B& )
        bool f4 = ( a != d );

        // Force the compiler to believe that all variables are important
        // so it doesn't optimize away any code
        USED( a );
        USED( c );
        USED( d );

        USED( f1 );
        USED( f2 );
        USED( f3 );
        USED( f4 );

        USED( st );
        USED( dt );
        USED( p  );
        USED( cp );
        USED( v  );
        USED( r  );
        USED( cr );

        // Implicit ~allocator() happens here
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestMemberFunctions( const Allocator& a )
    {
        // Typedefs
        typename Allocator::pointer         p = nullptr;
        typename Allocator::value_type      v;
        typename Allocator::reference       r = v;   // Note: must be initialized
        typename Allocator::const_reference cr = v;  // Note: must be initialized

        // allocator()
        Allocator b( Policy< Allocator >::Construct( a ) );
        VERIFY( b == Policy< Allocator >::Construct( a ) );

        // allocator( const allocator& )
        // The C++ Standard requires that Alloc<T> a(b) must give the post-condition
        // that Alloc<U>(a) == b
        Allocator c(a);
        VERIFY( c == a );

        // allocator( const allocator<U>& ) and rebind
        typename Allocator::template rebind< C >::other d( c );
        typename Allocator::template rebind< typename Allocator::value_type >::other e( d );
        VERIFY( c == e );

        // address( reference )
        VERIFY( c.address( r ) == &v );

        // address( const_reference )
        VERIFY( c.address( cr ) == &v );

        // max_size()
        VERIFY( c.max_size() > 0 );

        // allocate( size_type )
        try
        {
            p = c.allocate( 1 );
        }
        catch(...)
        {
            OUTERR( "Allocation Failure" );
        }
        VERIFY( p != nullptr );

        // Write to allocated memory
        memset( p, 'x', sizeof( typename Allocator::value_type ) );

        // deallocate( pointer, size_type )
        c.deallocate( p, 1 );

        // allocate( size_type, allocator<void>::const_pointer )
        try
        {
            p = c.allocate( 1, nullptr );
        }
        catch(...)
        {
            OUTERR( "Allocation Failure" );
        }
        VERIFY( p != nullptr );

        // Write to allocated memory
        memset( p, 'x', sizeof( typename Allocator::value_type ) );

        // construct( pointer, const T& )
        c.construct( p, v );
        VERIFY( *p == v );

        // destroy( pointer )
        c.destroy( p );

        // clean up
        c.deallocate( p, 1 );

        // allocate multiple items
        try
        {
            p = c.allocate( 2, nullptr );
        }
        catch(...)
        {
            OUTERR( "Allocation Failure" );
        }
        VERIFY( p != nullptr );

        // Write to allocated memory
        memset( p, 'x', sizeof( typename Allocator::value_type ) * 2 );

        // construct( pointer, const T& )
        c.construct( p, v );
        VERIFY( *p == v );

        c.construct( p + 1, v );
        VERIFY( *(p + 1) == v );

        // destroy( pointer )
        c.destroy( p );
        c.destroy( p + 1 );

        // clean up
        c.deallocate( p, 2 );

        // operator ==( const A&, const A& )
        VERIFY( a == c );

        // operator ==( const A&, const B& )
        bool f1 = ( a == d );

        // operator !=( const A&, const A& )
        VERIFY( !( a != c ) );

        // operator !=( const A&, const B& )
        bool f2 = ( a != d );

        // Force the compiler to believe that all variables are important
        // so it doesn't optimize away any code
        USED( a );
        USED( c );
        USED( d );

        USED( f1 );
        USED( f2 );

        USED( p  );
        USED( v  );
        USED( r  );
        USED( cr );

        // Implicit ~allocator() happens here
    }

    ///////////////////////////////////////////////////////////////////////////////
    //
    // StlAllocatorTestContainer.h
    //
    // Copyright (c) 2003 Pete Isensee (PKIsensee@msn.com).
    // All rights reserved worldwide.
    //
    // Permission to copy, modify, reproduce or redistribute this source code is
    // granted provided the above copyright notice is retained in the resulting
    // source code.
    //
    // This software is provided "as is" and without any express or implied
    // warranties.
    //
    // Description:
    //
    // Container tests for allocators. Typical tests involve:
    //
    //      Adding an element to the container (invokes allocate and construct)
    //      Removing an element from the container (invokes destroy and deallocate)
    //      Reserving elements (invokes allocate)
    //      Swapping containers (invokes deallocate and destroy)
    //      Extracting allocator object (invokes container get_allocator)

    //-----------------------------------------------------------------------------

    template< typename Allocator, typename Container >
    void TestVector( const Allocator& a, Container& c )
    {
        c.push_back( typename Allocator::value_type() );
        c.clear();
        c.push_back( typename Allocator::value_type() );
        c.reserve( 100 );
        VERIFY( c[0] == typename Allocator::value_type() );
        c.clear();
        USED( a );
        USED( c );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator, typename Container >
    void TestDeque( const Allocator& a, Container& c )
    {
        c.push_back( typename Allocator::value_type() );
        c.clear();
        c.assign( 100, typename Allocator::value_type() );
        VERIFY( c.front() == typename Allocator::value_type() );
        c.clear();
        USED( a );
        USED( c );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator, typename Container >
    void TestList( const Allocator& a, Container& c )
    {
        c.push_back( typename Allocator::value_type() );
        c.clear();
        c.insert( c.begin(), 100, typename Allocator::value_type() );
        VERIFY( c.front() == typename Allocator::value_type() );
        c.clear();
        USED( a );
        USED( c );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator, typename Container >
    void TestSet( const Allocator& a, Container& c )
    {
        c.insert( typename Allocator::value_type() );
        c.clear();
        for( int i = 0; i < 100; ++i )
            c.insert( typename Allocator::value_type( static_cast<intptr_t>(i) ) );
        c.insert( typename Allocator::value_type( 0 ) );
        VERIFY( c.find( typename Allocator::value_type( 0 ) ) == c.begin() );
        c.clear();
        USED( a );
        USED( c );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator, typename Container >
    void TestMap( const Allocator& a, Container& c )
    {
        c.insert( std::make_pair( typename Allocator::value_type(), 1 ) );
        c.clear();
        for( int i = 0; i < 100; ++i )
            c.insert( std::make_pair( typename Allocator::value_type( static_cast<intptr_t>(i) ), i ) );
        c.insert( std::make_pair( typename Allocator::value_type( 0 ), 0 ) );
        VERIFY( c.find( typename Allocator::value_type( 0 ) ) == c.begin() );
        c.clear();
        USED( a );
        USED( c );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator, typename Container >
    void TestString( const Allocator& a, Container& c )
    {
        c.push_back( typename Allocator::value_type() );
        c.clear();
        c.assign( 100, typename Allocator::value_type( 1 ) );
        VERIFY( c.find( typename Allocator::value_type( 1 ) ) == 0 );
        c.clear();
        USED( a );
        USED( c );
    }

    //-----------------------------------------------------------------------------

    template< typename Value, typename Allocator, typename Container >
    void TestStack( const Value& v, const Allocator&, Container& c )
    {
        c.push( v );
        c.pop();
        for( int i = 0; i < 100; ++i )
            c.push( typename Allocator::value_type( static_cast<intptr_t>(i) ) );
    #if( !defined(_MSC_VER) || _MSC_VER < 1310 )
        VERIFY( c.top() == typename Allocator::value_type( 99 ) );
    #endif
        for( int i = 0; i < 100; ++i )
            c.pop();
        USED( c );
    }

    #if( !defined(_MSC_VER) || _MSC_VER >= 1310 )

    // Specialize for bool, since we can't convert from c.top() to bool with
    // vector<bool> (see More Exceptional C++ Item 6)
    template< typename Value, typename Allocator, typename Container >
    void TestStack( const bool& v, const Allocator&, Container& c )
    {
        c.push( v );
        c.pop();
        for( int i = 0; i < 100; ++i )
            c.push( v );
        for( int i = 0; i < 100; ++i )
            c.pop();
        USED( c );
    }

    #endif

    //-----------------------------------------------------------------------------

    template< typename Allocator, typename Container >
    void TestQueue( const Allocator&, Container& c )
    {
        c.push( typename Allocator::value_type() );
        c.pop();
        for( int i = 0; i < 100; ++i )
            c.push( typename Allocator::value_type( static_cast<intptr_t>(i) ) );
        VERIFY( c.front() == typename Allocator::value_type( 0 ) );
        for( int i = 0; i < 100; ++i )
            c.pop();
        USED( c );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator, typename Container >
    void TestPriorityQueue( const Allocator&, Container& c )
    {
        c.push( typename Allocator::value_type() );
        c.pop();
        for( int i = 0; i < 100; ++i )
            c.push( typename Allocator::value_type( static_cast<intptr_t>(i) ) );
        for( int i = 0; i < 100; ++i )
            c.pop();
        USED( c );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestVector( const Allocator& a )
    {
        typedef std::vector< typename Allocator::value_type, Allocator > Vector;

        Vector v1( Policy< Allocator >::template GetDefault< Vector >( a ) );
        Vector v2( Policy< Allocator >::template GetCopied< Vector >( a ) );

        TestVector( a, v1 );
        TestVector( a, v2 );

        v1.swap( v2 );

        TestVector( a, v1 );
        TestVector( a, v2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestDeque( const Allocator& a )
    {
        typedef std::deque< typename Allocator::value_type, Allocator > Deque;

        Deque d1( Policy< Allocator >::template GetDefault< Deque >( a ) );
        Deque d2( Policy< Allocator >::template GetCopied< Deque >( a ) );

        TestDeque( a, d1 );
        TestDeque( a, d2 );

        d1.swap( d2 );

        TestDeque( a, d1 );
        TestDeque( a, d2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestList( const Allocator& a )
    {
        typedef std::list< typename Allocator::value_type, Allocator > List;

        List l1( Policy< Allocator >::template GetDefault< List >( a ) );
        List l2( Policy< Allocator >::template GetCopied< List >( a ) );

        TestList( a, l1 );
        TestList( a, l2 );

        l1.swap( l2 );

        TestList( a, l1 );
        TestList( a, l2 );

        l1.splice( l1.end(), l2 );

        TestList( a, l1 );
        TestList( a, l2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestSet( const Allocator& a )
    {
        typedef std::set< typename Allocator::value_type,
                            std::less< typename Allocator::value_type >,
                            Allocator > Set;

        Set s1( Policy< Allocator >::template GetDefaultSet< Set >( a ) );
        Set s2( Policy< Allocator >::template GetCopiedSet< Set >( a ) );

        TestSet( a, s1 );
        TestSet( a, s2 );

        s1.swap( s2 );

        TestSet( a, s1 );
        TestSet( a, s2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestMultiset( const Allocator& a )
    {
        typedef std::multiset< typename Allocator::value_type,
                                std::less< typename Allocator::value_type >,
                                Allocator > Multiset;

        Multiset s1( Policy< Allocator >::template GetDefaultSet< Multiset >( a ) );
        Multiset s2( Policy< Allocator >::template GetCopiedSet< Multiset >( a ) );

        TestSet( a, s1 );
        TestSet( a, s2 );

        s1.swap( s2 );

        TestSet( a, s1 );
        TestSet( a, s2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestMap( const Allocator& a )
    {
        // map allocators are pair allocators
        typedef std::pair< const typename Allocator::value_type, int > Pair;
        typedef typename Allocator::template rebind< Pair >::other Alloc;

        typedef std::map< typename Allocator::value_type, int,
                            std::less< typename Allocator::value_type >,
                            Alloc > Map;

        Map m1( Policy< Allocator >::template GetDefaultMap< Map >( a ) );
        Map m2( Policy< Allocator >::template GetCopiedMap< Map >( a ) );

        TestMap( a, m1 );
        TestMap( a, m2 );

        m1.swap( m2 );

        TestMap( a, m1 );
        TestMap( a, m2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestMultimap( const Allocator& a )
    {
        // map allocators are pair allocators
        typedef std::pair< const typename Allocator::value_type, int > Pair;
        typedef typename Allocator::template rebind< Pair >::other Alloc;

        typedef std::multimap< typename Allocator::value_type, int,
                                std::less< typename Allocator::value_type >,
                                Alloc > Multimap;

        Multimap m1( Policy< Allocator >::template GetDefaultMap< Multimap >( a ) );
        Multimap m2( Policy< Allocator >::template GetCopiedMap< Multimap >( a ) );

        TestMap( a, m1 );
        TestMap( a, m2 );

        m1.swap( m2 );

        TestMap( a, m1 );
        TestMap( a, m2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestString( const Allocator& aIn )
    {
        // Strings are made of chars or wide chars
        typedef typename Allocator::template rebind< char >::other CharAlloc;
        typedef typename Allocator::template rebind< wchar_t >::other WCharAlloc;

        typedef std::basic_string< char,
                                    std::char_traits< char >,
                                    CharAlloc > String;
        typedef std::basic_string< wchar_t,
                                    std::char_traits< wchar_t >,
                                    WCharAlloc > WString;

        // string
        CharAlloc a( aIn );

        String s1( Policy< Allocator >::template GetDefault< String >( a ) );
        String s2( Policy< Allocator >::template GetCopied< String >( a ) );

        TestString( a, s1 );
        TestString( a, s2 );

        s1.swap( s2 );

        TestString( a, s1 );
        TestString( a, s2 );

        // wstring
        WCharAlloc wa( aIn );
        WString w1( Policy< Allocator >::template GetDefault< WString >( wa ) );
        WString w2( Policy< Allocator >::template GetCopied< WString >( wa ) );

        TestString( wa, w1 );
        TestString( wa, w2 );

        w1.swap( w2 );

        TestString( wa, w1 );
        TestString( wa, w2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestStack( const Allocator& a )
    {
        // A stack can be made out of deque or vector
        typedef std::deque< typename Allocator::value_type, Allocator > Deque;
        typedef std::vector< typename Allocator::value_type, Allocator > Vector;

        typedef std::stack< typename Allocator::value_type, Deque > DStack;
        typedef std::stack< typename Allocator::value_type, Vector > VStack;

        DStack d1( Policy< Allocator >::template GetDefaultAdapter< DStack, Deque >( a ) );
        DStack d2( Policy< Allocator >::template GetCopiedAdapter< DStack, Deque >( a ) );
        VStack v1( Policy< Allocator >::template GetDefaultAdapter< VStack, Vector >( a ) );
        VStack v2( Policy< Allocator >::template GetCopiedAdapter< VStack, Vector >( a ) );

        typename Allocator::value_type v;

        // Test deque stacks
        TestStack< typename Allocator::value_type >( v, a, d1 );
        TestStack< typename Allocator::value_type >( v, a, d2 );

        // Test vector stacks
        TestStack< typename Allocator::value_type >( v, a, v1 );
        TestStack< typename Allocator::value_type >( v, a, v2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestQueue( const Allocator& a )
    {
        // A queue can be made out of deque or list
        typedef std::deque< typename Allocator::value_type, Allocator > Deque;
        typedef std::list< typename Allocator::value_type, Allocator > List;

        typedef std::queue< typename Allocator::value_type, Deque > DQueue;
        typedef std::queue< typename Allocator::value_type, List > LQueue;

        DQueue d1( Policy< Allocator >::template GetDefaultAdapter< DQueue, Deque >( a ) );
        DQueue d2( Policy< Allocator >::template GetCopiedAdapter< DQueue, Deque >( a ) );
        LQueue l1( Policy< Allocator >::template GetDefaultAdapter< LQueue, List >( a ) );
        LQueue l2( Policy< Allocator >::template GetCopiedAdapter< LQueue, List >( a ) );

        // Test deque queues
        TestQueue( a, d1 );
        TestQueue( a, d2 );

        // Test list queues
        TestQueue( a, l1 );
        TestQueue( a, l2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestPriorityQueue( const Allocator& a )
    {
        // A priority queue can be made out of vector or deque
        typedef std::vector< typename Allocator::value_type, Allocator > Vector;
        typedef std::deque< typename Allocator::value_type, Allocator > Deque;

        typedef std::priority_queue< typename Allocator::value_type, Vector > VQueue;
        typedef std::priority_queue< typename Allocator::value_type, Deque > DQueue;

        VQueue v1( Policy< Allocator >::template GetDefaultPriorityQueue< VQueue, Vector >( a ) );
        VQueue v2( Policy< Allocator >::template GetCopiedPriorityQueue< VQueue, Vector >( a ) );
        DQueue d1( Policy< Allocator >::template GetDefaultPriorityQueue< DQueue, Deque >( a ) );
        DQueue d2( Policy< Allocator >::template GetCopiedPriorityQueue< DQueue, Deque >( a ) );

        // Test vector queues
        TestPriorityQueue( a, v1 );
        TestPriorityQueue( a, v2 );

        // Test deque queues
        TestPriorityQueue( a, d1 );
        TestPriorityQueue( a, d2 );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestWithContainers( const Allocator& a )
    {
        TestVector( a );
        TestDeque( a );
        TestList( a );
        TestSet( a );
        TestMultiset( a );
        TestMap( a );
        TestMultimap( a );
        TestString( a );
        TestStack( a );
        TestQueue( a );
        TestPriorityQueue( a );
    }

    //-----------------------------------------------------------------------------

    template< typename Allocator >
    void TestAlloc( const Allocator& a )
    {
        // Verify functionality with a variety of types
        typename Allocator::template rebind< bool >::other   b( a ); // tests vector<bool> specialization
        typename Allocator::template rebind< C >::other      c( a ); // POD types
        typename Allocator::template rebind< D >::other      d( a ); // allocated internal type
        typename Allocator::template rebind< E >::other      e( a ); // complicated type
        typename Allocator::template rebind< int >::other    i( a ); // simple test
        typename Allocator::template rebind< void* >::other  v( a ); // void pointers
        typename Allocator::template rebind< char* >::other  p( a ); // regular pointers
//      typename Allocator::template rebind< void >::other   x( a ); // must provide this specialization

        //
        // About the commented line above:
        //
        // Really? The following code snippet doesn't compile on Comeau C++ 4.3.10.1:
        //
        //   #include <memory>
        //
        //   void foo()
        //   {
        //       std::allocator<int> a;
        //       std::allocator<int>::rebind<void>::other b(a);
        //   }
        //
        // It gives:
        //
        //   Error: no suitable user-defined conversion from
        //     "std::allocator<int>" to "std::allocator<void>" exists
        //

        TestMemberFunctionPresence( a ); // in StlAllocatorTestMembers.h

        TestMemberFunctions( a ); // in StlAllocatorTestMembers.h
        TestMemberFunctions( b );
        TestMemberFunctions( c );
        TestMemberFunctions( d );
        TestMemberFunctions( e );
        TestMemberFunctions( i );
        TestMemberFunctions( v );
        TestMemberFunctions( p );

        TestWithContainers( a ); // in StlAllocatorTestContainer.h
        TestWithContainers( b );
        TestWithContainers( c );
        TestWithContainers( d );
        TestWithContainers( e );
        TestWithContainers( i );
        TestWithContainers( v );
        TestWithContainers( p );
    }

    #undef OUTERR
    #undef VERIFY
    #undef USED

#if defined(__GNUC__) && (__GNUC__ * 100 + __GNUC_MINOR__ >= 800)
    #pragma GCC diagnostic pop
#endif

    #pragma warning (pop)

    TEST_CASE(PoolAllocator)
    {
        PoolAllocator<int, 2> allocator;
        TestAlloc(allocator);
    }

    SET_DEFAULT_CONSTRUCTABLE_OFF(AlignedAllocator);

    TEST_CASE(AlignedAllocator)
    {
        AlignedAllocator<int> allocator(32);
        TestAlloc(allocator);
    }
}
