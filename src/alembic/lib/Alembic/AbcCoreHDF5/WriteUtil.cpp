//-*****************************************************************************
//
// Copyright (c) 2009-2011,
//  Sony Pictures Imageworks Inc. and
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
// Industrial Light & Magic, nor the names of their contributors may be used
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

#include <Alembic/AbcCoreHDF5/WriteUtil.h>
#include <Alembic/AbcCoreHDF5/DataTypeRegistry.h>
#include <Alembic/AbcCoreHDF5/StringWriteUtil.h>
#include <Alembic/AbcCoreHDF5/AwImpl.h>
#include <Alembic/AbcCoreHDF5/HDF5Util.h>

namespace Alembic {
namespace AbcCoreHDF5 {
namespace ALEMBIC_VERSION_NS {

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
WrittenArraySampleMap &
GetWrittenArraySampleMap( AbcA::ArchiveWriterPtr iVal )
{
    AwImpl *ptr = dynamic_cast<AwImpl*>( iVal.get() );
    ABCA_ASSERT( ptr, "NULL Impl Ptr" );
    return ptr->getWrittenArraySampleMap();
}

//-*****************************************************************************
void
WriteDataToAttr( hid_t iParent,
                 hid_t iDspace,
                 const std::string &iAttrName,
                 hid_t iFileType,
                 hid_t iNativeType,
                 const void *iData )
{
    hid_t attrId = H5Acreate2( iParent, iAttrName.c_str(),
                               iFileType, iDspace,
                               H5P_DEFAULT, H5P_DEFAULT );
    AttrCloser attrCloser( attrId );

    herr_t status = H5Awrite( attrId, iNativeType, iData );

    ABCA_ASSERT( status >= 0, "Couldn't write attribute: " << iAttrName );
}

//-*****************************************************************************
void
WriteScalar( hid_t iParent,
             const std::string &iAttrName,
             hid_t iFileType,
             hid_t iNativeType,
             const void *iData )
{
    hid_t dspaceId = H5Screate( H5S_SCALAR );
    DspaceCloser dspaceCloser( dspaceId );

    WriteDataToAttr( iParent, dspaceId, iAttrName, iFileType, iNativeType,
                     iData );
}

//-*****************************************************************************
void
WriteSmallArray( hid_t iParent,
                 const std::string &iAttrName,
                 hid_t iFileType,
                 hid_t iNativeType,
                 size_t iNumVals,
                 const void *iData )
{
    Dimensions dims( iNumVals );
    HDimensions hdims( dims );
    size_t npoints = hdims.numPoints();
    ABCA_ASSERT( npoints > 0,
                  "Cannot create degenerate dataspace" );

    hid_t dspaceId = H5Screate_simple( hdims.rank(), hdims.rootPtr(), NULL );
    DspaceCloser dspaceCloser( dspaceId );

    WriteDataToAttr( iParent, dspaceId, iAttrName, iFileType, iNativeType,
                     iData );
}

//-*****************************************************************************
void
WriteKey( hid_t iHashDset,
          const std::string &iAttrName,
          const AbcA::ArraySample::Key &iKey )
{
    // keys are 16 bytes.
    WriteSmallArray( iHashDset, iAttrName,
                     H5T_STD_U8LE,
                     H5T_NATIVE_UINT8,
                     16,
                     ( const void * )&iKey.digest );
}

//-*****************************************************************************
//-*****************************************************************************
//-*****************************************************************************

//-*****************************************************************************
// Dimensions aren't a scalar, and thus must be written carefully.
void
WriteDimensions( hid_t iParent,
                 const std::string &iAttrName,
                 const Dimensions &iDims )
{

    size_t rank = iDims.rank();

    // Create temporary storage to write
    std::vector<uint32_t> dimStorage( rank );

    // Copy into it.
    for ( size_t r = 0; r < rank; ++r )
    {
        dimStorage[r] = ( uint32_t )iDims[r];
    }

    WriteSmallArray( iParent, iAttrName, H5T_STD_U32LE,
                     H5T_NATIVE_UINT32,
                     rank,
                     ( const void * )&dimStorage.front() );
}

//-*****************************************************************************
void
WriteMetaData( hid_t iGroup,
               const std::string &iName,
               const AbcA::MetaData &iMetaData )
{
    //std::cout << "Being asked to write MetaData named: " << iName
    //          << std::endl;
    if ( iMetaData.size() > 0 )
    {
        std::string str = iMetaData.serialize();
        if ( str.length() > 0 && str != "" )
        {
            //std::cout << "About to write MetaData string: "
            //          << str << " to name: " << iName << std::endl;
            WriteString( iGroup, iName, str );
        }
    }
}

//-*****************************************************************************
static void
WriteTimeSamplingType( hid_t iGroup,
                       const std::string &iName,
                       const AbcA::TimeSamplingType &iTimeSamplingType )
{
    const std::string nameSPC = iName + ".tspc";
    const std::string nameTPC = iName + ".ttpc";

    const uint32_t spc = iTimeSamplingType.getNumSamplesPerCycle();
    const chrono_t tpc = iTimeSamplingType.getTimePerCycle();

    if ( iTimeSamplingType.isUniform() )
    {
        // With uniform, we JUST write the time per sample
        assert( spc == 1 );
        WriteScalar( iGroup, nameTPC,
                     H5T_IEEE_F64LE,
                     H5T_NATIVE_DOUBLE,
                     ( const void * )&tpc );
    }
    else if ( iTimeSamplingType.isCyclic() )
    {
        // Here we have to write SPC, and if TPC is 1.0 we don't
        // bother writing it.
        assert( spc > 1 );
        assert( tpc < AbcA::TimeSamplingType::AcyclicTimePerCycle() );
        WriteScalar( iGroup, nameSPC,
                     H5T_STD_U32LE,
                     H5T_NATIVE_UINT32,
                     ( const void * )&spc );
        if ( tpc != 1.0 )
        {
            WriteScalar( iGroup, nameTPC,
                         H5T_IEEE_F64LE,
                         H5T_NATIVE_DOUBLE,
                         ( const void * )&tpc );
        }
    }
    else
    {
        assert( iTimeSamplingType.isAcyclic() );
        assert( spc == AbcA::TimeSamplingType::AcyclicNumSamples() );
        WriteScalar( iGroup, nameSPC,
                     H5T_STD_U32LE,
                     H5T_NATIVE_UINT32,
                     ( const void * )&spc );
    }
}

//-*****************************************************************************
WrittenArraySampleIDPtr
WriteArray( WrittenArraySampleMap &iMap,
            hid_t iGroup,
            const std::string &iName,
            const AbcA::ArraySample &iSamp,
            const AbcA::ArraySample::Key &iKey,
            hid_t iFileType,
            hid_t iNativeType,
            int iCompressionLevel )
{

    // Dispatch to string writing utils.
    const AbcA::DataType &dataType = iSamp.getDataType();
    if ( dataType.getPod() == kStringPOD )
    {
        return WriteStringArray( iMap, iGroup, iName, iSamp, iKey,
                                 iCompressionLevel );
    }
    else if ( dataType.getPod() == kWstringPOD )
    {
        return WriteWstringArray( iMap, iGroup, iName, iSamp, iKey,
                                  iCompressionLevel );
    }

    // write the dimensions as necessary
    Dimensions dims = iSamp.getDimensions();
    size_t rank = dims.rank();

    ABCA_ASSERT( rank > 0, "Cannot have a rank-0 array sample" );

    // rank 1 is the most common case, and we can easily infer it's size
    // from the dataspace for non-strings, so don't bother writing it out
    if (rank > 1)
    {
        std::string dimsName = iName + ".dims";
        WriteDimensions( iGroup, dimsName, dims );
    }

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
    if ( hasData )
    {
        hsize_t hdim = dims.numPoints() * dataType.getExtent();
        dspaceId = H5Screate_simple( 1, &hdim, NULL );
    }
    else
    {
        dspaceId = H5Screate( H5S_NULL );
    }

    ABCA_ASSERT( dspaceId >= 0,
                 "WriteArray() Failed in dataspace construction" );
    DspaceCloser dspaceCloser( dspaceId );

    hid_t dsetId = -1;
    if ( iCompressionLevel >= 0 && hasData )
    {
        // Make a compression plist
        hid_t zipPlist = DsetGzipCreatePlist( dims,
            iCompressionLevel > 9 ? 9 : iCompressionLevel );
        PlistCloser plistCloser( zipPlist );

        // Make the dataset.
        dsetId = H5Dcreate2( iGroup, iName.c_str(), iFileType, dspaceId,
                             H5P_DEFAULT, zipPlist, H5P_DEFAULT );
    }
    else
    {
        dsetId = H5Dcreate2( iGroup, iName.c_str(),
                             iFileType, dspaceId,
                             H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
    }
    DsetCloser dsetCloser(dsetId);

    ABCA_ASSERT( dsetId >= 0,
                 "WriteArray() Failed in dataset constructor" );

    // Write the data.
    if ( hasData )
    {
        H5Dwrite( dsetId, iNativeType, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                  iSamp.getData() );
    }

    // Write the array sample key.
    WriteKey( dsetId, "key", iKey );

    writeID.reset( new WrittenArraySampleID( iKey, dsetId ) );
    iMap.store( writeID );

    // Return the reference.
    return writeID;
}

//-*****************************************************************************
void
CopyWrittenArray( hid_t iGroup,
                  const std::string &iName,
                  WrittenArraySampleIDPtr iRef )
{
    ABCA_ASSERT( ( bool )iRef,
                  "CopyWrittenArray() passed a bogus ref" );

    hid_t fid = H5Iget_file_id(iGroup);
    ABCA_ASSERT( fid >= 0,
                "CopyWrittenArray() Could not get file ID from iGroup" );

    hid_t did = H5Dopen( fid,
        iRef->getObjectLocation().c_str(), H5P_DEFAULT );
    DsetCloser dcloser(did);

    // We have a reference. Create a link to it.
    // We are manually getting the source dataset instead of using
    // fid and iName because of a bug in HDF5 1.8.5 and earlier.
    // Files written using that approach would sometimes be corrupted.
    herr_t status = H5Lcreate_hard( did,
                                    ".",
                                    iGroup,
                                    iName.c_str(),
                                    H5P_DEFAULT,
                                    H5P_DEFAULT );

    H5Fclose( fid );
    ABCA_ASSERT( status >= 0,
                 "H5Lcreate_hard failed!" << std::endl
                  << "Dset obj id: " << did << std::endl
                  << "Link loc id: " << iGroup << std::endl
                  << "Link name: " << iName );
}

//-*****************************************************************************
void WritePropertyInfo( hid_t iGroup,
                    const std::string &iName,
                    AbcA::PropertyType iPropertyType,
                    const AbcA::DataType &iDataType,
                    bool isScalarLike,
                    uint32_t iTimeSamplingIndex,
                    uint32_t iNumSamples,
                    uint32_t iFirstChangedIndex,
                    uint32_t iLastChangedIndex )
{

    uint32_t info[5] = {0, 0, 0, 0, 0};
    uint32_t numFields = 1;

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

    // for compounds we just write out 0
    if ( iPropertyType != AbcA::kCompoundProperty )
    {
        // Slam the property type in there.
        info[0] |= ptypeMask & ( uint32_t )iPropertyType;

        // arrays may be scalar like, scalars are already scalar like
        info[0] |= ( uint32_t ) isScalarLike;

        uint32_t pod = ( uint32_t )iDataType.getPod();
        info[0] |= podMask & ( pod << 2 );

        if (iTimeSamplingIndex != 0)
        {
            info[0] |= hasTsidxMask;
        }

        if (iFirstChangedIndex == 1 && iLastChangedIndex == iNumSamples - 1)
        {
            info[0] |= noRepeatsMask;
        }

        uint32_t extent = ( uint32_t )iDataType.getExtent();
        info[0] |= extentMask & ( extent << 8 );

        ABCA_ASSERT( iFirstChangedIndex <= iNumSamples &&
            iLastChangedIndex <= iNumSamples &&
            iFirstChangedIndex <= iLastChangedIndex,
            "Illegal Sampling!" << std::endl <<
            "Num Samples: " << iNumSamples << std::endl <<
            "First Changed Index: " << iFirstChangedIndex << std::endl <<
            "Last Changed Index: " << iLastChangedIndex << std::endl );

        // Write the num samples. Only bother writing if
        // the num samples is greater than 1.  Existence of name.smp0
        // is used by the reader to determine if 0 or 1 sample.
        if ( iNumSamples > 1 )
        {
            info[1] = iNumSamples;
            numFields ++;
            if ( iFirstChangedIndex > 1 || ( iLastChangedIndex != 0 &&
                iLastChangedIndex != iNumSamples - 1 ) )
            {
                info[2] = iFirstChangedIndex;
                info[3] = iLastChangedIndex;
                numFields += 2;
            }
        }

        // finally set time sampling index on the end if necessary
        if (iTimeSamplingIndex != 0)
        {
            info[numFields] = iTimeSamplingIndex;
            numFields ++;
        }

    }

    WriteSmallArray( iGroup, iName + ".info",
        H5T_STD_U32LE, H5T_NATIVE_UINT32, numFields,
        ( const void * ) info );
}

//-*****************************************************************************
void WriteTimeSampling( hid_t iGroup,
                    const std::string &iName,
                    const AbcA::TimeSampling &iTsmp )
{

    AbcA::TimeSamplingType tst = iTsmp.getTimeSamplingType();
    WriteTimeSamplingType( iGroup, iName, tst );

    //-*************************************************************************
    // WRITE TIMES.
    //-*************************************************************************
    std::string timeSampsName = iName + ".time";

    const std::vector < chrono_t > & samps = iTsmp.getStoredTimes();
    ABCA_ASSERT( samps.size() > 0, "No TimeSamples to write!");
    H5LTset_attribute_double ( iGroup, ".", timeSampsName.c_str(),
        &samps.front(), samps.size() );
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace AbcCoreHDF5
} // End namespace Alembic
