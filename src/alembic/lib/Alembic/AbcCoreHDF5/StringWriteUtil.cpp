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

#include <Alembic/AbcCoreHDF5/StringWriteUtil.h>
#include <Alembic/AbcCoreHDF5/WriteUtil.h>
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
WriteStringT( hid_t iParent,
              const std::string &iAttrName,
              const StringT &iString )
{
    // Verify that no NULL characters have been hidden in the string.
    static const CharT NullChar = ( CharT )0;
    ABCA_ASSERT( iString.find( NullChar ) == StringT::npos,
                 "Illegal NULL character found in string in WriteStringT" );

    // Create the dataspace.
    size_t len = iString.length();
    Dimensions dims( len + 1 );
    HDimensions hdims( dims );
    size_t npoints = hdims.numPoints();
    ABCA_ASSERT( npoints > 0,
                 "Cannot create degenerate dataspace" );

    hid_t dspaceId = H5Screate_simple( hdims.rank(), hdims.rootPtr(), NULL );
    DspaceCloser dspaceCloser( dspaceId );

    // Get the data.
    static CharT emptyString = NullChar;
    const CharT *data;
    if ( len == 0 )
    {
        data = &emptyString;
    }
    else
    {
        data = iString.c_str();
    }

    // Write into it.
    WriteDataToAttr( iParent, dspaceId, iAttrName,
                     GetFileDtype<CharT>(), GetNativeDtype<CharT>(),
                     ( const void * )data );
}

//-*****************************************************************************
template <>
void
WriteStringT<std::string,char>( hid_t iParent,
                                const std::string &iAttrName,
                                const std::string &iString )
{
    // Verify that no NULL characters have been hidden in the string.
    static const char NullChar = ( char )0;
    ABCA_ASSERT( iString.find( NullChar ) == std::string::npos,
                 "Illegal NULL character found in string in WriteStringT" );
    
    size_t len = iString.length();
    if ( len < 1 ) { len = 1; }

    hid_t dtypeId = H5Tcopy( H5T_C_S1 );
    DtypeCloser dtypeCloser( dtypeId );
    H5Tset_size( dtypeId, len );

    hid_t dspaceId = H5Screate( H5S_SCALAR );
    DspaceCloser dspaceCloser( dspaceId );

    WriteDataToAttr( iParent, dspaceId, iAttrName,
                     dtypeId, dtypeId, ( const void * )iString.c_str() );
}

//-*****************************************************************************
void WriteString( hid_t iParent,
                  const std::string &iAttrName,
                  const std::string &iString )
{
    WriteStringT<std::string,char>( iParent, iAttrName, iString );
}

//-*****************************************************************************
void WriteWstring( hid_t iParent,
                   const std::string &iAttrName,
                   const std::wstring &iString )
{
    WriteStringT<std::wstring,wchar_t>( iParent, iAttrName, iString );
}

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// This function compacts multiple strings into a single linear character
// array based on the use of the '0' character to separate them.
template <class StringT, class CharT>
static void CompactStrings( const StringT *iStrings,
                            size_t iNumStrings,
                            std::vector<CharT> &oCompacted )
{
    // If strings are degenerate, don't bother with this.
    if ( iNumStrings < 1 )
    {
        assert( iStrings == NULL );
        oCompacted.resize( 0 );
        return;
    }

    // We first loop over the strings and figure out how many
    // total characters we'd need.
    static const CharT NullChar = ( CharT )0;
    size_t totalNumChars = 0;
    for ( size_t iStr = 0; iStr < iNumStrings; ++iStr )
    {
        ABCA_ASSERT( iStrings[iStr].find( NullChar ) == StringT::npos,
                     "Illegal NULL character found in string: "
                     << iStr << " of the string array." );
        
        totalNumChars += iStrings[iStr].length() + 1;
    }
    assert( totalNumChars >= iNumStrings );

    // Don't have to worry about setting array to zero, the
    // routine below will do write every location.
    oCompacted.resize( totalNumChars );

    // Loop over the thing, writing the strings.
    CharT *intoBegin = &oCompacted.front();
    for ( size_t iStr = 0; iStr < iNumStrings; ++iStr )
    {
        const StringT &str = iStrings[iStr];
        size_t strLen = str.length();
        if ( strLen > 0 )
        {
            const CharT *chars = str.c_str();
            std::copy( chars, chars + strLen, intoBegin );

            // Increment the next string beginning forward
            // to go past the characters already written.
            // If strLen < 1, this is not necessary.
            intoBegin += strLen;
        }

        // Write the zero at the end of the string.
        // This works even if the string being written is empty.
        (*intoBegin) = NullChar;

        // Increment past this.
        ++intoBegin;
    }
    assert( ( intoBegin - totalNumChars ) ==
            ( CharT * )&oCompacted.front() );

    // That's it!
}


//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// Write a single string attribute, but compact multiple strings into it
// by exploiting zero-termination of strings.
template <class StringT, class CharT>
void
WriteStringsT( hid_t iParent,
               const std::string &iAttrName,
               size_t iNumStrings,
               const StringT *iStrings )
{
    // Validate
    ABCA_ASSERT( iNumStrings > 0, "Degenerate num strings in WriteStringsT" );
    ABCA_ASSERT( iStrings, "Degenerate strings buffer in WriteStringsT" );

    // Compact the strings.
    std::vector<CharT> charBuffer;
    CompactStrings( iStrings, iNumStrings, charBuffer );

    // Create the dataspace.
    size_t len = charBuffer.size();
    assert( len >= iNumStrings );
    Dimensions dims( len );
    HDimensions hdims( dims );
    hid_t dspaceId = H5Screate_simple( hdims.rank(), hdims.rootPtr(), NULL );
    DspaceCloser dspaceCloser( dspaceId );

    ABCA_ASSERT( dspaceId >= 0,
                 "WriteStringsT() Failed in dataspace constructor" );

    // Create the attribute.
    WriteDataToAttr( iParent, dspaceId, iAttrName,
                     GetFileDtype<CharT>(), GetNativeDtype<CharT>(),
                     ( const void * )&charBuffer.front() );

    // That's it!
}

//-*****************************************************************************
void WriteStrings( hid_t iParent,
                   const std::string &iAttrName,
                   size_t iNumStrings,
                   const std::string *iStrings )
{
    WriteStringsT<std::string, char>
        ( iParent, iAttrName, iNumStrings, iStrings );
}

//-*****************************************************************************
void WriteWstrings( hid_t iParent,
                    const std::string &iAttrName,
                    size_t iNumStrings,
                    const std::wstring *iStrings )
{
    WriteStringsT<std::wstring, wchar_t>
        ( iParent, iAttrName, iNumStrings, iStrings );
}

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
template <class StringT, class CharT>
WrittenArraySampleIDPtr
WriteStringArrayT( WrittenArraySampleMap &iMap,
                   hid_t iGroup,
                   const std::string &iName,
                   const AbcA::ArraySample &iSamp,
                   const AbcA::ArraySample::Key &iKey,
                   int iCompressionLevel )
{
    // because strings are packed together, always write out the dimensions
    Dimensions dims = iSamp.getDimensions();
    ABCA_ASSERT( dims.rank() > 0,
        "String type can not have a rank-0 array sample" );
    std::string dimsName = iName + ".dims";
    WriteDimensions( iGroup, dimsName, dims );

    // See whether or not we've already stored this.
    WrittenArraySampleIDPtr writeID = iMap.find( iKey );
    if ( writeID )
    {
        CopyWrittenArray( iGroup, iName, writeID );
        return writeID;
    }

    // Okay, need to actually store it.
    // It will be a dataset with an internal attribute for storing
    // the hash id.

    bool hasData = dims.numPoints() > 0;
    hid_t dspaceId = -1;
    Dimensions wdims; // Used to store the "fake" dimensions.
    std::vector<CharT> charBuffer;

    // Get the dimensions, validate sample size.
    if ( hasData )
    {
        size_t extent = iSamp.getDataType().getExtent();
        size_t numStrings = dims.numPoints() * extent;
        ABCA_ASSERT( dims.rank() > 0 && numStrings > 0,
                     "Degenerate array sample in WriteStringArrayT" );

        // Get the data out of the array sample.
        const StringT *strings =
            reinterpret_cast<const StringT *>( iSamp.getData() );
        ABCA_ASSERT( strings,
                     "Degenerate strings in WriteStringArrayT" );

        // Compact the strings in the string array.
        CompactStrings( strings, numStrings, charBuffer );

        // Create the dataspace.
        size_t len = charBuffer.size();
        assert( len >= numStrings );
        wdims = Dimensions( len );
        HDimensions hdims( wdims );

        dspaceId = H5Screate_simple( hdims.rank(),
                                     hdims.rootPtr(), NULL );
    }
    else
    {
        dspaceId = H5Screate( H5S_NULL );
    }

    ABCA_ASSERT( dspaceId >= 0,
                 "WriteStringsT() Failed in dataspace constructor" );
    DspaceCloser dspaceCloser( dspaceId );

    hid_t dsetId = -1;
    if ( iCompressionLevel >= 0 && hasData )
    {
        // Make a compression plist
        hid_t zipPlist = DsetGzipCreatePlist( wdims,
            iCompressionLevel > 9 ? 9 : iCompressionLevel );
        PlistCloser plistCloser( zipPlist );

        //std::cout << "Creating compressed data set named: "
        //          << iName << " in group named: " << iGroup.name()
        //          << std::endl;

        // Make the dataset.
        dsetId = H5Dcreate2( iGroup, iName.c_str(), GetFileDtype<CharT>(),
                             dspaceId, H5P_DEFAULT, zipPlist, H5P_DEFAULT );
    }
    else
    {
        //std::cout << "Creating uncompressed data set named: "
        //          << iName << " in group named: " << iGroup.name()
        //          << std::endl;
        dsetId = H5Dcreate2( iGroup, iName.c_str(), GetFileDtype<CharT>(),
                             dspaceId, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
    }
    DsetCloser dsetCloser(dsetId);

    ABCA_ASSERT( dsetId >= 0,
                 "WriteArray() Failed in dataset constructor" );

    // Write the data.
    if ( hasData )
    {
        H5Dwrite( dsetId, GetNativeDtype<CharT>(), H5S_ALL,
                  H5S_ALL, H5P_DEFAULT, &charBuffer.front() );
    }

    // Write the key
    WriteKey( dsetId, "key", iKey );

    writeID.reset( new WrittenArraySampleID( iKey, dsetId ) );

    iMap.store( writeID );

    // Return the reference.
    return writeID;
}


//-*****************************************************************************
WrittenArraySampleIDPtr
WriteStringArray( WrittenArraySampleMap &iMap,
                  hid_t iGroup,
                  const std::string &iName,
                  const AbcA::ArraySample &iSamp,
                  const AbcA::ArraySample::Key &iKey,
                  int iCompressionLevel )
{
    return WriteStringArrayT<std::string,char>( iMap,
                                                iGroup,
                                                iName,
                                                iSamp,
                                                iKey,
                                                iCompressionLevel );
}

//-*****************************************************************************
WrittenArraySampleIDPtr
WriteWstringArray( WrittenArraySampleMap &iMap,
                   hid_t iGroup,
                   const std::string &iName,
                   const AbcA::ArraySample &iSamp,
                   const AbcA::ArraySample::Key &iKey,
                   int iCompressionLevel )
{
    return WriteStringArrayT<std::wstring,wchar_t>( iMap,
                                                    iGroup,
                                                    iName,
                                                    iSamp,
                                                    iKey,
                                                    iCompressionLevel );
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
