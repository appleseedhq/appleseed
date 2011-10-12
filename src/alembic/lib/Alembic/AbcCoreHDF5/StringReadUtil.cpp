//-*****************************************************************************
//
// Copyright (c) 2009-2011,
//  Sony Pictures Imageworks, Inc. and
//  Industrial Light & Magic, a division of Lucasfilm Entertainment Company Ltd.
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
// *       Neither the name of Sony Pictures Imageworks, nor
// Industrial Light & Magic nor the names of their contributors may be used
// to endorse or promote products derived from this software without specific
// prior written permission.
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
//-*****************************************************************************

#include <Alembic/AbcCoreHDF5/StringReadUtil.h>
#include <Alembic/AbcCoreHDF5/ReadUtil.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
BOOST_STATIC_ASSERT( sizeof( char ) == sizeof( int8_t ) );
BOOST_STATIC_ASSERT( ( sizeof( wchar_t ) == sizeof( int8_t ) ) ||
                     ( sizeof( wchar_t ) == sizeof( int16_t ) ) ||
                     ( sizeof( wchar_t ) == sizeof( int32_t ) ) );

//-*****************************************************************************
template <class CharT>
static inline hid_t GetNativeDtype();

template <>
inline hid_t GetNativeDtype<char>() { return H5T_NATIVE_CHAR; }

template <>
inline hid_t GetNativeDtype<wchar_t>()
{
    // return H5T_NATIVE_INT32;
    if ( sizeof( wchar_t ) == 1 ) { return H5T_NATIVE_CHAR; }
    else if ( sizeof( wchar_t ) == 2 ) { return H5T_NATIVE_INT16; }
    else
    {
        assert( sizeof( wchar_t ) == 4 );
        return H5T_NATIVE_INT32;
    }
}

//-*****************************************************************************
// We always use little-endian types in the file itself.
// We also always use 32-bits for the wchars, even if on Windows wchar is
// only 16-bits.
template <class CharT>
static inline hid_t GetFileDtype();

template <>
inline hid_t GetFileDtype<char>() { return H5T_STD_I8LE; }

template <>
inline hid_t GetFileDtype<wchar_t>() { return H5T_STD_I32LE; }

//-*****************************************************************************
template <class StringT, class CharT>
void
ReadStringT( hid_t iParent,
             const std::string &iAttrName,
             StringT &oString )
{
    ABCA_ASSERT( iParent >= 0, "Invalid parent in ReadStringT" );

    // Open the attribute.
    hid_t attrId = H5Aopen( iParent, iAttrName.c_str(), H5P_DEFAULT );
    ABCA_ASSERT( attrId >= 0,
                 "Couldn't open attribute named: " << iAttrName );
    AttrCloser attrCloser( attrId );

    // Checking code.
    {
        hid_t attrFtype = H5Aget_type( attrId );
        DtypeCloser dtypeCloser( attrFtype );

        hid_t nativeDtype = GetNativeDtype<CharT>();
        ABCA_ASSERT( H5Tget_class( attrFtype ) ==
                     H5Tget_class( nativeDtype ) &&

                     H5Tget_sign( attrFtype ) ==
                     H5Tget_sign( nativeDtype ),

                     "Invalid datatype for stringT" );
    }

    hid_t attrSpace = H5Aget_space( attrId );
    ABCA_ASSERT( attrSpace >= 0,
                 "Couldn't get dataspace for attribute: " << iAttrName );
    DspaceCloser dspaceCloser( attrSpace );

    hssize_t numPoints = H5Sget_simple_extent_npoints( attrSpace );
    ABCA_ASSERT( numPoints > 0,
                 "Degenerate string dimensions in ReadStringT" );

    // Create temporary char storage buffer.
    std::vector<CharT> charStorage( ( size_t )( 1 + numPoints ),
                                    ( CharT )0 );

    // Read into it.
    herr_t status = H5Aread( attrId, GetNativeDtype<CharT>(),
                             ( void * )&charStorage.front() );
    ABCA_ASSERT( status >= 0, "Couldn't read from attribute: " << iAttrName );

    // Return it.
    oString = ( const CharT * )&charStorage.front();
}

//-*****************************************************************************
template <>
void
ReadStringT<std::string,char>( hid_t iParent,
                               const std::string &iAttrName,
                               std::string &oString )
{
    ABCA_ASSERT( iParent >= 0, "Invalid parent in ReadStringT" );

    // Open the attribute.
    hid_t attrId = H5Aopen( iParent, iAttrName.c_str(), H5P_DEFAULT );
    ABCA_ASSERT( attrId >= 0,
                 "Couldn't open attribute named: " << iAttrName );
    AttrCloser attrCloser( attrId );

    // Checking code.
    hid_t attrFtype = H5Aget_type( attrId );
    DtypeCloser dtypeCloser( attrFtype );

    size_t numChars = H5Tget_size( attrFtype );

    // Read and check space
    {
        hid_t attrSpace = H5Aget_space( attrId );
        ABCA_ASSERT( attrSpace >= 0,
                     "Couldn't get dataspace for attribute: " << iAttrName );
        DspaceCloser dspaceCloser( attrSpace );
        
        H5S_class_t attrSpaceClass = H5Sget_simple_extent_type( attrSpace );
        ABCA_ASSERT( attrSpaceClass == H5S_SCALAR,
                     "Tried to read non-scalar attribute: " << iAttrName
                     << " as scalar" );
    }

    // Create temporary char storage buffer.
    std::vector<char> charStorage( ( size_t )( 1 + numChars ),
                                   ( char )0 );

    // Read into it.
    herr_t status = H5Aread( attrId, attrFtype,
                             ( void * )&charStorage.front() );
    ABCA_ASSERT( status >= 0, "Couldn't read from attribute: " << iAttrName );

    // Return it.
    oString = ( const char * )&charStorage.front();
}

//-*****************************************************************************
void ReadString( hid_t iParent,
                 const std::string &iAttrName,
                 std::string &oString )
{
    ReadStringT<std::string,char>( iParent, iAttrName, oString );
}

//-*****************************************************************************
void ReadWstring( hid_t iParent,
                  const std::string &iAttrName,
                  std::wstring &oString )
{
    ReadStringT<std::wstring,wchar_t>( iParent, iAttrName, oString );
}

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// This function extracts multiple strings from a single linear character
// array based on the use of the '0' character to separate them.
template <class StringT, class CharT>
static void ExtractStrings( StringT *oStrings,
                            const CharT *iChars,
                            size_t iNumChars,
                            size_t iNumStringsExpected )
{
    // To read any string,
    // just imagine how we'd do it?
    // Start with two pointers, one for beginning and one for end.
    // move the end one forward until a zero is encountered.
    // then move it one more forward, and it is an end.
    // if end-beginning == 1, it is an empty string, which CAN
    // happen. Put that string into the current string, and move
    // to the next string.

    size_t nextStringBegin = 0;
    size_t nextStringEnd = 0;
    size_t strIdx;
    for ( strIdx = 0;
          strIdx < iNumStringsExpected && nextStringBegin < iNumChars;
          ++strIdx )
    {
        // Move the end caliper to the terminating zero, which may be
        // the beginning character in the case of an empty string.
        while ( iChars[nextStringEnd] != ( CharT )0 &&
                nextStringEnd < iNumChars )
        {
            ++nextStringEnd;
        }

        // Make sure we didn't have a premature EOS.
        if ( iChars[nextStringEnd] != ( CharT )0 )
        {
            assert( nextStringEnd == iNumChars );
            ABCA_THROW( "Corrupt compacted string array, premature end" );
        }

        // Set the string to either the empty string
        // or the appropriate 0-terminated char string.
        StringT &thisString = oStrings[strIdx];
        if ( nextStringEnd - nextStringBegin < 1 )
        {
            // Assuming this makes an empty string.
            thisString = StringT();
            assert( thisString.length() == 0 );
        }
        else
        {
            const CharT *thisStrC = iChars + nextStringBegin;
            thisString = thisStrC;
        }

        // Move the front and end caliper past the terminal zero
        // to the beginning of the next string.
        nextStringBegin = nextStringEnd + 1;
        nextStringEnd = nextStringBegin;
    }

    // Okay, make sure we read the appropriate number of strings.
    ABCA_ASSERT( strIdx == iNumStringsExpected,
                 "Corrupt compacted string array, premature end, "
                 << "too few strings. Expected: " << iNumStringsExpected
                 << ", but got: " << strIdx );

    // All done.
}

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// Read a single string attribute, but extract multiple strings from it
// by exploiting zero-termination of strings.
template <class StringT, class CharT>
void
ReadStringsT( hid_t iParent,
              const std::string &iAttrName,
              size_t iNumStrings,
              StringT *oStrings )
{
    ABCA_ASSERT( iParent >= 0, "Invalid parent in ReadStringsT" );

    // Open the attribute.
    hid_t attrId = H5Aopen( iParent, iAttrName.c_str(), H5P_DEFAULT );
    ABCA_ASSERT( attrId >= 0,
                 "Couldn't open attribute named: " << iAttrName );
    AttrCloser attrCloser( attrId );

    // Checking code.
    {
        hid_t attrFtype = H5Aget_type( attrId );
        DtypeCloser dtypeCloser( attrFtype );

        hid_t nativeDtype = GetNativeDtype<CharT>();
        ABCA_ASSERT( H5Tget_class( attrFtype ) ==
                     H5Tget_class( nativeDtype ) &&

                     H5Tget_sign( attrFtype ) ==
                     H5Tget_sign( nativeDtype ),

                     "Invalid datatype for stringT" );
    }

    hid_t attrSpace = H5Aget_space( attrId );
    ABCA_ASSERT( attrSpace >= 0,
                 "Couldn't get dataspace for attribute: " << iAttrName );
    DspaceCloser dspaceCloser( attrSpace );

    hssize_t numPoints = H5Sget_simple_extent_npoints( attrSpace );
    ABCA_ASSERT( numPoints > 0,
                 "Degenerate string dimensions in ReadStringsT" );

    // Create temporary char storage buffer.
    std::vector<CharT> charStorage( ( size_t )( 1 + numPoints ),
                                    ( CharT )0 );

    // Read into it.
    herr_t status = H5Aread( attrId, GetNativeDtype<CharT>(),
                             ( void * )&charStorage.front() );
    ABCA_ASSERT( status >= 0, "Couldn't read from attribute: " << iAttrName );

    // Extract 'em.
    ExtractStrings( oStrings, ( const CharT * )&charStorage.front(),
                    1 + numPoints, iNumStrings );
}

//-*****************************************************************************
void ReadStrings( hid_t iParent,
                  const std::string &iAttrName,
                  size_t iNumStrings,
                  std::string *oStrings )
{
    ReadStringsT<std::string, char>
        ( iParent, iAttrName, iNumStrings, oStrings );
}

//-*****************************************************************************
void ReadWstrings( hid_t iParent,
                   const std::string &iAttrName,
                   size_t iNumStrings,
                   std::wstring *oStrings )
{
    ReadStringsT<std::wstring, wchar_t>
        ( iParent, iAttrName, iNumStrings, oStrings );
}

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
template <class StringT, class CharT>
static AbcA::ArraySamplePtr
ReadStringArrayT( AbcA::ReadArraySampleCachePtr iCache,
                  hid_t iParent,
                  const std::string &iName,
                  const AbcA::DataType &iDataType )
{
    assert( iDataType.getExtent() > 0 );

    // Open the data set.
    hid_t dsetId = H5Dopen( iParent, iName.c_str(), H5P_DEFAULT );
    ABCA_ASSERT( dsetId >= 0, "Cannot open dataset: " << iName );
    DsetCloser dsetCloser( dsetId );

    // Read the data space.
    hid_t dspaceId = H5Dget_space( dsetId );
    ABCA_ASSERT( dspaceId >= 0, "Could not get dataspace for dataSet: "
                 << iName );
    DspaceCloser dspaceCloser( dspaceId );

    // Read the digest, if there's a cache.
    AbcA::ArraySample::Key key;
    bool foundDigest = false;

    // If we found a digest and there's a cache, see
    // if we're in there, and return it if so.
    if ( iCache )
    {
        key.origPOD = iDataType.getPod();
        key.readPOD = key.origPOD;

        hid_t dsetFtype = H5Dget_type( dsetId );
        DtypeCloser dtypeCloser( dsetFtype );

        // string arrays get packed together
        key.numBytes = H5Sget_simple_extent_npoints( dspaceId ) *
            H5Tget_size( dsetFtype );

        foundDigest = ReadKey( dsetId, "key", key );
        AbcA::ReadArraySampleID found = iCache->find( key );
        if ( found )
        {
            AbcA::ArraySamplePtr ret = found.getSample();
            assert( ret );
            if ( ret->getDataType() != iDataType )
            {
                ABCA_THROW( "ERROR: Read data type for dset: " << iName
                            << ": " << ret->getDataType()
                            << " does not match expected data type: "
                            << iDataType );
            }

            // Got it!
            return ret;
        }
    }

    // Okay, we haven't found it in a cache.

    // Read the data type.
    // Checking code.
    {
        hid_t dsetFtype = H5Dget_type( dsetId );
        DtypeCloser dtypeCloser( dsetFtype );

        hid_t nativeDtype = GetNativeDtype<CharT>();
        ABCA_ASSERT( H5Tget_class( dsetFtype ) ==
                     H5Tget_class( nativeDtype ) &&

                     H5Tget_sign( dsetFtype ) ==
                     H5Tget_sign( nativeDtype )

                     // CJH They can now be different
                     // sizes, because wchar_t is sometimes 16-bit,
                     // but we always store 32 bit.
                     // && H5Tget_size( dsetFtype ) ==
                     //H5Tget_size( nativeDtype ),

                     , "Invalid datatype for stringT" );
    }

    // String array datatypes require a "dimensions" to be stored
    // externally, since the strings themselves are stored in a compacted
    // array of rank 1.
    Dimensions realDims;
    std::string dimName = iName + ".dims";
    ReadDimensions( iParent, dimName, realDims );
    ABCA_ASSERT( realDims.rank() > 0,
                 "Degenerate rank in Dataset read" );

    AbcA::ArraySamplePtr ret;

    H5S_class_t dspaceClass = H5Sget_simple_extent_type( dspaceId );

    if ( dspaceClass == H5S_SIMPLE )
    {
        ABCA_ASSERT( realDims.numPoints() > 0,
                     "Degenerate dims in Dataset read" );
        size_t totalNumStrings = realDims.numPoints() * iDataType.getExtent();

        // Get the dimensions
        Dimensions dims;
        int rank = H5Sget_simple_extent_ndims( dspaceId );
        ABCA_ASSERT( rank == ( int ) realDims.rank(),
                     "H5Sget_simple_extent_ndims() failed." );

        HDimensions hdims;
        hdims.setRank( rank );
        rank = H5Sget_simple_extent_dims( dspaceId, hdims.rootPtr(), NULL );
        ABCA_ASSERT( rank == ( int ) hdims.rank(),
                     "H5Sget_simple_extent_dims() "
                     "found inconsistent ranks."
                     << std::endl
                     << "Expecting rank: " << hdims.rank()
                     << " instead was: " << rank );
        
        dims = hdims;
        ABCA_ASSERT( dims.numPoints() > 0,
                     "Degenerate dims in Dataset read" );
        

        // Create temporary char storage buffer.
        size_t totalNumChars = dims.numPoints() + 1;
        std::vector<CharT> charStorage( totalNumChars, ( CharT )0 );
        
        // Read into it.
        herr_t status = H5Dread( dsetId, GetNativeDtype<CharT>(),
                                 H5S_ALL, H5S_ALL, H5P_DEFAULT,
                                 ( void * )&charStorage.front() );
        ABCA_ASSERT( status >= 0,
                     "Could not read string array from data set. Weird." );

        // Make an appropriately dimensionalized (and manageable)
        // array of strings using the ArraySamples.
        ret = AbcA::AllocateArraySample( iDataType,
                                         realDims );
        StringT *strings = reinterpret_cast<StringT*>(
            const_cast<void*>( ret->getData() ) );
        assert( strings != NULL );

        // This part is hard. We have to go through the one dimensional
        // array extracting each string.
        ExtractStrings<StringT,CharT>( strings,
                                       ( const CharT * )&charStorage.front(),
                                       totalNumChars,
                                       totalNumStrings );
    }
    else if ( dspaceClass == H5S_NULL )
    {
        // Num points should be zero here.
        ABCA_ASSERT( realDims.numPoints() == 0,
                     "Expecting zero points in dimensions" );

        ret = AbcA::AllocateArraySample( iDataType, realDims );
    }
    else
    {
        ABCA_THROW( "Unexpected scalar dataspace encountered." );
    }

    // Store if there is a cache.
    if ( foundDigest && iCache )
    {
        AbcA::ReadArraySampleID stored = iCache->store( key, ret );
        if ( stored )
        {
            return stored.getSample();
        }
    }

    // Otherwise, just leave! ArraySamplePtr returned by AllocateArraySample
    // already has fancy-dan deleter built in.
    // I REALLY LOVE SMART PTRS.
    return ret;
}

//-*****************************************************************************
AbcA::ArraySamplePtr
ReadStringArray( AbcA::ReadArraySampleCachePtr iCache,
                 hid_t iParent,
                 const std::string &iName,
                 const AbcA::DataType &iDataType )
{
    return ReadStringArrayT<std::string, char>
        ( iCache, iParent, iName, iDataType );
}

//-*****************************************************************************
AbcA::ArraySamplePtr
ReadWstringArray( AbcA::ReadArraySampleCachePtr iCache,
                  hid_t iParent,
                  const std::string &iName,
                  const AbcA::DataType &iDataType )
{
    return ReadStringArrayT<std::wstring, wchar_t>
        ( iCache, iParent, iName, iDataType );
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
