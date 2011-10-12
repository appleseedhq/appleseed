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

#include <Alembic/AbcCoreHDF5/ReadUtil.h>
#include <Alembic/AbcCoreHDF5/DataTypeRegistry.h>
#include <Alembic/AbcCoreHDF5/ArImpl.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************
// NON-PUBLICLY VISIBLE HELPERS
//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
void
ReadScalar( hid_t iParent,
            const std::string &iAttrName,
            hid_t iFileType,
            hid_t iNativeType,
            void *oData )
{
    ABCA_ASSERT( iParent >= 0, "Invalid parent" );

    hid_t attrId = H5Aopen( iParent, iAttrName.c_str(), H5P_DEFAULT );
    ABCA_ASSERT( attrId >= 0,
                 "Couldn't open attribute named: " << iAttrName );
    AttrCloser attrCloser( attrId );

    // This is all just checking code
    {
        hid_t attrFtype = H5Aget_type( attrId );
        ABCA_ASSERT( attrFtype >= 0,
                     "Couldn't get file datatype for attribute: "
                     << iAttrName );
        DtypeCloser dtypeCloser( attrFtype );

        ABCA_ASSERT( EquivalentDatatypes( attrFtype, iFileType ),
                     "File DataType clash for scalar attribute: "
                     << iAttrName );

        hid_t attrSpace = H5Aget_space( attrId );
        ABCA_ASSERT( attrSpace >= 0,
                     "Couldn't get dataspace for attribute: " << iAttrName );
        DspaceCloser dspaceCloser( attrSpace );

        H5S_class_t attrSpaceClass = H5Sget_simple_extent_type( attrSpace );
        ABCA_ASSERT( attrSpaceClass == H5S_SCALAR,
                     "Tried to read non-scalar attribute: " << iAttrName
                     << " as scalar" );
    }

    herr_t status = H5Aread( attrId, iNativeType, oData );
    ABCA_ASSERT( status >= 0, "Couldn't read from attribute: " << iAttrName );
}

//-*****************************************************************************
void
ReadSmallArray( hid_t iParent,
                const std::string &iAttrName,
                hid_t iFileType,
                hid_t iNativeType,
                size_t iMaxElems,
                size_t &oNumElems,
                void *oData )
{
    ABCA_ASSERT( iParent >= 0, "Invalid parent" );

    hid_t attrId = H5Aopen( iParent, iAttrName.c_str(), H5P_DEFAULT );
    ABCA_ASSERT( attrId >= 0,
                 "Couldn't open attribute named: " << iAttrName );
    AttrCloser attrCloser( attrId );

    // This is all just checking code
    {
        hid_t attrFtype = H5Aget_type( attrId );
        ABCA_ASSERT( attrFtype >= 0,
                     "Couldn't get file datatype for attribute: "
                     << iAttrName );
        DtypeCloser dtypeCloser( attrFtype );

        ABCA_ASSERT( EquivalentDatatypes( attrFtype, iFileType ),
                     "File DataType clash for scalar attribute: "
                     << iAttrName );

        hid_t attrSpace = H5Aget_space( attrId );
        ABCA_ASSERT( attrSpace >= 0,
                     "Couldn't get dataspace for attribute: " << iAttrName );
        DspaceCloser dspaceCloser( attrSpace );

        H5S_class_t attrSpaceClass = H5Sget_simple_extent_type( attrSpace );
        ABCA_ASSERT( attrSpaceClass == H5S_SIMPLE,
                     "Tried to read non-simple attribute: " << iAttrName
                     << " as scalar" );

        hssize_t numPoints = H5Sget_simple_extent_npoints( attrSpace );
        ABCA_ASSERT( numPoints <= ( hssize_t )iMaxElems && numPoints > -1,
                     "Too many points in SmallArrayRead" );

        oNumElems = ( size_t )numPoints;
    }

    herr_t status = H5Aread( attrId, iNativeType, oData );
    ABCA_ASSERT( status >= 0, "Couldn't read from attribute: " << iAttrName );
}

//-*****************************************************************************
// Dimensions aren't a scalar, and thus must be read carefully.
void
ReadDimensions( hid_t iParent,
                const std::string &iAttrName,
                Dimensions &oDims )
{
    // Assume a maximum rank of 128. This is totally reasonable.
    static const size_t maxRank = 128;
    static uint32_t dimVals[128];

    size_t readRank;
    ReadSmallArray( iParent, iAttrName, H5T_STD_U32LE, H5T_NATIVE_UINT32,
                    maxRank, readRank, ( void * )dimVals );

    Dimensions retDims;
    retDims.setRank( readRank );
    for ( size_t r = 0; r < readRank; ++r )
    {
        retDims[r] = ( size_t )dimVals[r];
    }

    oDims = retDims;
}

//-*****************************************************************************
// Get the dimensions directly off of the dataspace on the dataset
// This isn't suitable for string and wstring
void
ReadDataSetDimensions( hid_t iParent,
                       const std::string &iName,
                       hsize_t iExtent,
                       Dimensions &oDims )
{
    // Open the data set.
    hid_t dsetId = H5Dopen( iParent, iName.c_str(), H5P_DEFAULT );
    ABCA_ASSERT( dsetId >= 0, "Cannot open dataset: " << iName );
    DsetCloser dsetCloser( dsetId );

    // Read the data space.
    hid_t dspaceId = H5Dget_space( dsetId );
    ABCA_ASSERT( dspaceId >= 0, "Could not get dataspace for dataSet: "
                 << iName );
    DspaceCloser dspaceCloser( dspaceId );

    H5S_class_t dspaceClass = H5Sget_simple_extent_type( dspaceId );
    if ( dspaceClass == H5S_SIMPLE )
    {
        // Get the dimensions
        int rank = H5Sget_simple_extent_ndims( dspaceId );
        ABCA_ASSERT( rank == 1, "H5Sget_simple_extent_ndims() must be 1." );

        hsize_t hdim = 0;
        rank = H5Sget_simple_extent_dims( dspaceId, &hdim, NULL );
        oDims.setRank(1);
        oDims[0] = hdim / iExtent;
    }
    else
    {
        oDims.setRank(1);
        oDims[0] = 0;
    }
}

//-*****************************************************************************
bool
ReadKey( hid_t iParent,
         const std::string &iAttrName,
         AbcA::ArraySample::Key &oKey )
{
    ABCA_ASSERT( iParent >= 0, "Invalid parent in ReadKey" );
    if ( H5Aexists( iParent, iAttrName.c_str() ) > 0 )
    {
        size_t numRead;
        ReadSmallArray( iParent, iAttrName,
                        H5T_STD_U8LE,
                        H5T_NATIVE_UINT8,
                        16,
                        numRead,
                        ( void * )&oKey.digest );
        ABCA_ASSERT( numRead == 16, "Didn't read enough key bits" );

        return true;
    }
    else
    {
        return false;
    }
}

//-*****************************************************************************
bool
ReadMetaData( hid_t iParent,
              const std::string &iMetaDataName,
              AbcA::MetaData &oMetaData )
{
    std::string str;
    ABCA_ASSERT( iParent >= 0, "Invalid parent in ReadMetaData" );
    if ( H5Aexists( iParent, iMetaDataName.c_str() ) > 0 )
    {
        ReadString( iParent, iMetaDataName, str );
        oMetaData.deserialize( str );
        return true;
    }
    else
    {
        oMetaData = AbcA::MetaData();
        return false;
    }
}

//-*****************************************************************************
static bool
ReadTimeSamplingType( hid_t iParent,
                      const std::string &iPropName,
                      AbcA::TimeSamplingType &oTimeSamplingType )
{
    const std::string nameSPC = iPropName + ".tspc";
    const std::string nameTPC = iPropName + ".ttpc";

    ABCA_ASSERT( iParent >= 0, "Invalid parent in ReadTimeSamplingType" );

    if ( H5Aexists( iParent, nameSPC.c_str() ) > 0 )
    {
        // Read the samples per cycle. If it is 1, we don't need to read
        // time per cycle because we can assume that it is 1.0.
        // If time per cycle is other than 1.0, but the num samples is 1,
        // Alembic writes out just the time per cycle, and not the num samples.
        uint32_t spc = 0;
        ReadScalar( iParent, nameSPC,
                    H5T_STD_U32LE,
                    H5T_NATIVE_UINT32,
                    ( void * )&spc );
        ABCA_ASSERT( spc > 0, "Invalid Samples Per Cycle: " << spc );

        if ( spc == AbcA::TimeSamplingType::AcyclicNumSamples() )
        {
            // Acyclic.
            oTimeSamplingType = AbcA::TimeSamplingType(
                AbcA::TimeSamplingType::kAcyclic );
            return true;
        }
        else if ( spc == 1 )
        {
            // Uniform with time per cycle == 1.0
            oTimeSamplingType = AbcA::TimeSamplingType( 1, 1.0 );
            return true;
        }
        ABCA_ASSERT( spc > 1, "Corrupt TimeSamplingType spc: " << spc );

        chrono_t tpc = 1.0;

        if (H5Aexists( iParent, nameTPC.c_str() ) > 0)
        {
            ReadScalar( iParent, nameTPC, H5T_IEEE_F64LE,
                        H5T_NATIVE_DOUBLE, ( void * )&tpc );
        }

        ABCA_ASSERT( tpc > 0.0 && tpc <
                     AbcA::TimeSamplingType::AcyclicTimePerCycle(),
                     "Invalid Time Per Cycle: " << tpc );

        // Cyclic with time per cycle
        oTimeSamplingType = AbcA::TimeSamplingType( spc, tpc );
        return true;
    }
    else if ( H5Aexists( iParent, nameTPC.c_str() ) > 0 )
    {
        // Uniform with time per cycle
        chrono_t tpc = 1.0;
        ReadScalar( iParent, nameTPC,
                    H5T_IEEE_F64LE,
                    H5T_NATIVE_DOUBLE,
                    ( void * )&tpc );

        // Uniform
        oTimeSamplingType = AbcA::TimeSamplingType( tpc );
        return true;
    }
    else
    {
        // Identity
        oTimeSamplingType = AbcA::TimeSamplingType();
        return false;
    }
}

//-*****************************************************************************
void
ReadPropertyHeader( hid_t iParent,
                    const std::string & iPropName,
                    AbcA::PropertyHeader & oHeader,
                    bool & oIsScalarLike,
                    uint32_t & oNumSamples,
                    uint32_t & oFirstChangedIndex,
                    uint32_t & oLastChangedIndex,
                    uint32_t & oTimeSamplingIndex )
{
    uint32_t info[5] = {0, 0, 0, 0, 0};

    static const uint32_t ptypeMask = ( uint32_t )BOOST_BINARY (
        0000 0000 0000 0000 0000 0000 0000 0011 );

    static const uint32_t podMask = ( uint32_t )BOOST_BINARY (
        0000 0000 0000 0000 0000 0000 0011 1100 );

    static const uint32_t hasTsidxMask = ( uint32_t )BOOST_BINARY (
        0000 0000 0000 0000 0000 0000 0100 0000 );

    static const uint32_t noRepeatsMask = ( uint32_t )BOOST_BINARY (
        0000 0000 0000 0000 0000 0000 1000 0000 );

    static const uint32_t extentMask = ( uint32_t )BOOST_BINARY(
        0000 0000 0000 0000 1111 1111 0000 0000 );

    size_t numFields = 0;
    size_t fieldsUsed = 1;

    ReadSmallArray(iParent, iPropName + ".info", H5T_STD_U32LE,
                   H5T_NATIVE_UINT32, 5, numFields, (void *) info );

    AbcA::MetaData metaData;
    ReadMetaData( iParent, iPropName + ".meta", metaData );

    if ( numFields == 1 && info[0] == 0 )
    {
        oHeader = AbcA::PropertyHeader( iPropName, metaData );
    }
    else
    {
        // low two bits are the property type
        char ipt = info[0] & ptypeMask;

        // first bit is either scalar, or scalar like
        oIsScalarLike = ipt & 1;

        // is scalar like is set for this array attribute
        if (ipt == 3)
        {
            oHeader.setPropertyType( AbcA::kArrayProperty );
        }
        else
        {
            oHeader.setPropertyType( ( AbcA::PropertyType )ipt );
        }

        // Read the pod type out of bits 2-5
        char podt = ( char )( ( info[0] & podMask ) >> 2 );
        if ( podt != ( char )kBooleanPOD &&

             podt != ( char )kUint8POD &&
             podt != ( char )kInt8POD &&

             podt != ( char )kUint16POD &&
             podt != ( char )kInt16POD &&

             podt != ( char )kUint32POD &&
             podt != ( char )kInt32POD &&

             podt != ( char )kUint64POD &&
             podt != ( char )kInt64POD &&

             podt != ( char )kFloat16POD &&
             podt != ( char )kFloat32POD &&
             podt != ( char )kFloat64POD &&

             podt != ( char )kStringPOD &&
             podt != ( char )kWstringPOD )
        {
            ABCA_THROW( "Read invalid POD type: " << ( int )podt );
        }

        // bit 6 is the hint about whether time sampling index was written
        // at the end
        bool hasTsidx = ( (info[0] & hasTsidxMask ) >> 6 ) == 1;
        oTimeSamplingIndex = 0;

        if ( hasTsidx && numFields > 1 )
        {
            oTimeSamplingIndex = info[numFields - 1];
            fieldsUsed ++;
        }

        // bit 7 is a hint about whether first and last changed index
        // are intrinsically 1, and numSamples - 1
        // (no repeated data from the start or the end)
        bool noRepeats = ( (info[0] & noRepeatsMask ) >> 7 ) == 1;

        // Time Sampling Index could be written, but the number of samples
        // may not be.
        if ( numFields > fieldsUsed )
        {
            oNumSamples = info[1];

            if ( numFields >= 4 )
            {
                oFirstChangedIndex = info[2];
                oLastChangedIndex = info[3];
            }
            else if ( noRepeats )
            {
                oFirstChangedIndex = 1;
                oLastChangedIndex = oNumSamples - 1;
            }
            else
            {
                oFirstChangedIndex = 0;
                oLastChangedIndex = 0;
            }
        }
        else
        {
            oNumSamples = 0;
            oFirstChangedIndex = 0;
            oLastChangedIndex = 0;

            // if smp0 exists then we have 1 sample
            std::string smpName = iPropName + ".smp0";
            if ( oHeader.getPropertyType() == AbcA::kArrayProperty &&
                 H5Lexists( iParent, smpName.c_str(), H5P_DEFAULT ) > 0)
            {
                oNumSamples = 1;
            }
            else if ( oHeader.getPropertyType() == AbcA::kScalarProperty &&
                      H5Aexists( iParent, smpName.c_str() ) > 0)
            {
                oNumSamples = 1;
            }
        }

        // Read the extent out of bits 8-15
        uint8_t extent = ( uint8_t )( ( info[0] & extentMask ) >> 8 );
        if ( extent == 0 )
        {
            ABCA_THROW( "Degenerate extent 0" );
        }

        // bits 16-31 are currently not being used

        // the time sampling will be set on oHeader by the calling function
        // since we don't have access to the archive here.
        oHeader.setName( iPropName );
        oHeader.setMetaData( metaData );
        oHeader.setDataType(
            AbcA::DataType( ( Util::PlainOldDataType ) podt, extent ) );
    }
}

//-*****************************************************************************
AbcA::ArraySamplePtr
ReadArray( AbcA::ReadArraySampleCachePtr iCache,
           hid_t iParent,
           const std::string &iName,
           const AbcA::DataType &iDataType,
           hid_t iFileType,
           hid_t iNativeType )
{
    // Dispatch string stuff.
    if ( iDataType.getPod() == kStringPOD )
    {
        return ReadStringArray( iCache, iParent, iName, iDataType );
    }
    else if ( iDataType.getPod() == kWstringPOD )
    {
        return ReadWstringArray( iCache, iParent, iName, iDataType );
    }
    assert( iDataType.getPod() != kStringPOD &&
            iDataType.getPod() != kWstringPOD );

    // Open the data set.
    hid_t dsetId = H5Dopen( iParent, iName.c_str(), H5P_DEFAULT );
    ABCA_ASSERT( dsetId >= 0, "Cannot open dataset: " << iName );
    DsetCloser dsetCloser( dsetId );

    // Read the data space.
    hid_t dspaceId = H5Dget_space( dsetId );
    ABCA_ASSERT( dspaceId >= 0, "Could not get dataspace for dataSet: "
                 << iName );
    DspaceCloser dspaceCloser( dspaceId );

    AbcA::ArraySample::Key key;
    bool foundDigest = false;

    // if we are caching, get the key and see if it is being used
    if ( iCache )
    {
        key.origPOD = iDataType.getPod();
        key.readPOD = key.origPOD;

        key.numBytes = iDataType.getNumBytes() *
            H5Sget_simple_extent_npoints( dspaceId );

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
    hid_t dtypeId = H5Dget_type( dsetId );
    ABCA_ASSERT( dtypeId >= 0, "Could not get datatype for dataSet: "
                 << iName );
    DtypeCloser dtypeCloser( dtypeId );

    ABCA_ASSERT( EquivalentDatatypes( iFileType, dtypeId ),
                 "File DataType clash for array dataset: "
                 << iName );

    AbcA::ArraySamplePtr ret;

    H5S_class_t dspaceClass = H5Sget_simple_extent_type( dspaceId );
    if ( dspaceClass == H5S_SIMPLE )
    {
        // Get the dimensions
        int rank = H5Sget_simple_extent_ndims( dspaceId );
        ABCA_ASSERT( rank == 1,
                     "H5Sget_simple_extent_ndims() must be 1." );

        hsize_t hdim = 0;

        rank = H5Sget_simple_extent_dims( dspaceId, &hdim, NULL );

        Dimensions dims;
        std::string dimName = iName + ".dims";
        if ( H5Aexists( iParent, dimName.c_str() ) )
        {
            ReadDimensions( iParent, dimName, dims );
        }
        else
        {
            dims.setRank(1);
            dims[0] = hdim / iDataType.getExtent();
        }

        ABCA_ASSERT( dims.numPoints() > 0,
                     "Degenerate dims in Dataset read" );

        // Create a buffer into which we shall read.
        ret = AbcA::AllocateArraySample( iDataType, dims );
        assert( ret->getData() );

        // And... read into it.
        herr_t status = H5Dread( dsetId, iNativeType,
                                 H5S_ALL, H5S_ALL, H5P_DEFAULT,
                                 const_cast<void*>( ret->getData() ) );

        ABCA_ASSERT( status >= 0, "H5Dread() failed." );
    }
    else if ( dspaceClass == H5S_NULL )
    {
        Dimensions dims;
        std::string dimName = iName + ".dims";
        if ( H5Aexists( iParent, dimName.c_str() ) )
        {
            ReadDimensions( iParent, dimName, dims );
            ABCA_ASSERT( dims.rank() > 0,
                         "Degenerate rank in Dataset read" );
            // Num points should be zero here.
            ABCA_ASSERT( dims.numPoints() == 0,
                         "Expecting zero points in dimensions" );
        }
        else
        {
            dims.setRank(1);
            dims[0] = 0;
        }

        ret = AbcA::AllocateArraySample( iDataType, dims );
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
void
ReadTimeSamples( hid_t iParent,
                 std::vector <  AbcA::TimeSamplingPtr > & oTimeSamples )
{
    oTimeSamples.clear();
    // add the intrinsic default sampling
    AbcA::TimeSamplingPtr ts( new AbcA::TimeSampling() );
    oTimeSamples.push_back( ts );

    uint32_t i = 1;
    AbcA::TimeSamplingType tst;
    std::string tstname = "1";

    // keep trying to read till we can't find anymore
    while ( ReadTimeSamplingType( iParent, tstname, tst ) )
    {
        // try to open the time samples attribute
        std::string timeName = tstname + ".time";
        hid_t aid = H5Aopen( iParent, timeName.c_str(), H5P_DEFAULT );
        ABCA_ASSERT( aid >= 0,
                     "Couldn't open time samples named: " << timeName );
        AttrCloser attrCloser( aid );

        // figure out how big it is
        hid_t sid = H5Aget_space( aid );
        ABCA_ASSERT( sid >= 0,
                     "Couldn't get dataspace for time samples: " << timeName );
        DspaceCloser dspaceCloser( sid );

        hssize_t numPoints = H5Sget_simple_extent_npoints( sid );
        ABCA_ASSERT( numPoints > 0, "No time samples data: " << timeName );
        std::vector < chrono_t > times(numPoints);

        // do the read
        herr_t status = H5Aread( aid, H5T_NATIVE_DOUBLE, &(times.front()) );
        ABCA_ASSERT( status >= 0, "Can't read time samples: " << timeName );

        // create the TimeSampling and add it to our vector
        ts.reset( new AbcA::TimeSampling(tst, times) );
        oTimeSamples.push_back( ts );

        // increment to try and read the next one
        i++;
        std::stringstream strm;
        strm << i;
        tstname = strm.str();
    }
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
