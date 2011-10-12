/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Copyright by The HDF Group.                                               *
* Copyright by the Board of Trustees of the University of Illinois.         *
* All rights reserved.                                                      *
*                                                                           *
* This file is part of HDF5.  The full HDF5 copyright notice, including     *
* terms governing use, modification, and redistribution, is contained in    *
* the files COPYING and Copyright.html.  COPYING can be found at the root   *
* of the source code distribution tree; Copyright.html can be found at the  *
* root level of an installed copy of the electronic HDF5 document set and   *
* is linked from the top-level documents page.  It can also be found at     *
* http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
* access to either file, you may request a copy from help@hdfgroup.org.     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


#ifndef _H5LTf90proto_H
#define _H5LTf90proto_H

#include "H5public.h"
#include "H5f90i.h"
#include <stdlib.h>
#include <string.h>


H5_FCDLL char*  HD5f2cstring (_fcd fdesc, int len);
H5_FCDLL void HD5packFstring (char *src, char *dest, size_t len);


/*
*  Functions from H5LTfc.c
*/
#   define nh5ltmake_dataset_c       H5_FC_FUNC_(h5ltmake_dataset_c, H5LTMAKE_DATASET_C)
#   define nh5ltmake_dataset_int1_c  H5_FC_FUNC_(h5ltmake_dataset_int1_c, H5LTMAKE_DATASET_INT1_C)
#   define nh5ltmake_dataset_int2_c  H5_FC_FUNC_(h5ltmake_dataset_int2_c, H5LTMAKE_DATASET_INT2_C)
#   define nh5ltmake_dataset_int3_c  H5_FC_FUNC_(h5ltmake_dataset_int3_c, H5LTMAKE_DATASET_INT3_C)
#   define nh5ltmake_dataset_int4_c  H5_FC_FUNC_(h5ltmake_dataset_int4_c, H5LTMAKE_DATASET_INT4_C)
#   define nh5ltmake_dataset_int5_c  H5_FC_FUNC_(h5ltmake_dataset_int5_c, H5LTMAKE_DATASET_INT5_C)
#   define nh5ltmake_dataset_int6_c  H5_FC_FUNC_(h5ltmake_dataset_int6_c, H5LTMAKE_DATASET_INT6_C)
#   define nh5ltmake_dataset_int7_c  H5_FC_FUNC_(h5ltmake_dataset_int7_c, H5LTMAKE_DATASET_INT7_C)
#   define nh5ltmake_dataset_fl1_c   H5_FC_FUNC_(h5ltmake_dataset_fl1_c, H5LTMAKE_DATASET_FL1_C)
#   define nh5ltmake_dataset_fl2_c   H5_FC_FUNC_(h5ltmake_dataset_fl2_c, H5LTMAKE_DATASET_FL2_C)
#   define nh5ltmake_dataset_fl3_c   H5_FC_FUNC_(h5ltmake_dataset_fl3_c, H5LTMAKE_DATASET_FL3_C)
#   define nh5ltmake_dataset_fl4_c   H5_FC_FUNC_(h5ltmake_dataset_fl4_c, H5LTMAKE_DATASET_FL4_C)
#   define nh5ltmake_dataset_fl5_c   H5_FC_FUNC_(h5ltmake_dataset_fl5_c, H5LTMAKE_DATASET_FL5_C)
#   define nh5ltmake_dataset_fl6_c   H5_FC_FUNC_(h5ltmake_dataset_fl6_c, H5LTMAKE_DATASET_FL6_C)
#   define nh5ltmake_dataset_fl7_c   H5_FC_FUNC_(h5ltmake_dataset_fl7_c, H5LTMAKE_DATASET_FL7_C)
#   define nh5ltmake_dataset_dl1_c   H5_FC_FUNC_(h5ltmake_dataset_dl1_c, H5LTMAKE_DATASET_DL1_C)
#   define nh5ltmake_dataset_dl2_c   H5_FC_FUNC_(h5ltmake_dataset_dl2_c, H5LTMAKE_DATASET_DL2_C)
#   define nh5ltmake_dataset_dl3_c   H5_FC_FUNC_(h5ltmake_dataset_dl3_c, H5LTMAKE_DATASET_DL3_C)
#   define nh5ltmake_dataset_dl4_c   H5_FC_FUNC_(h5ltmake_dataset_dl4_c, H5LTMAKE_DATASET_DL4_C)
#   define nh5ltmake_dataset_dl5_c   H5_FC_FUNC_(h5ltmake_dataset_dl5_c, H5LTMAKE_DATASET_DL5_C)
#   define nh5ltmake_dataset_dl6_c   H5_FC_FUNC_(h5ltmake_dataset_dl6_c, H5LTMAKE_DATASET_DL6_C)
#   define nh5ltmake_dataset_dl7_c   H5_FC_FUNC_(h5ltmake_dataset_dl7_c, H5LTMAKE_DATASET_DL7_C)
#   define nh5ltmake_dataset_nint1_c H5_FC_FUNC_(h5ltmake_dataset_nint1_c, H5LTMAKE_DATASET_NINT1_C)
#   define nh5ltmake_dataset_nint2_c H5_FC_FUNC_(h5ltmake_dataset_nint2_c, H5LTMAKE_DATASET_NINT2_C)
#   define nh5ltmake_dataset_nint3_c H5_FC_FUNC_(h5ltmake_dataset_nint3_c, H5LTMAKE_DATASET_NINT3_C)
#   define nh5ltmake_dataset_nint4_c H5_FC_FUNC_(h5ltmake_dataset_nint4_c, H5LTMAKE_DATASET_NINT4_C)
#   define nh5ltmake_dataset_nint5_c H5_FC_FUNC_(h5ltmake_dataset_nint5_c, H5LTMAKE_DATASET_NINT5_C)
#   define nh5ltmake_dataset_nint6_c H5_FC_FUNC_(h5ltmake_dataset_nint6_c, H5LTMAKE_DATASET_NINT6_C)
#   define nh5ltmake_dataset_nint7_c H5_FC_FUNC_(h5ltmake_dataset_nint7_c, H5LTMAKE_DATASET_NINT7_C)
#   define nh5ltmake_dataset_nfl1_c  H5_FC_FUNC_(h5ltmake_dataset_nfl1_c, H5LTMAKE_DATASET_NFL1_C)
#   define nh5ltmake_dataset_nfl2_c  H5_FC_FUNC_(h5ltmake_dataset_nfl2_c, H5LTMAKE_DATASET_NFL2_C)
#   define nh5ltmake_dataset_nfl3_c  H5_FC_FUNC_(h5ltmake_dataset_nfl3_c, H5LTMAKE_DATASET_NFL3_C)
#   define nh5ltmake_dataset_nfl4_c  H5_FC_FUNC_(h5ltmake_dataset_nfl4_c, H5LTMAKE_DATASET_NFL4_C)
#   define nh5ltmake_dataset_nfl5_c  H5_FC_FUNC_(h5ltmake_dataset_nfl5_c, H5LTMAKE_DATASET_NFL5_C)
#   define nh5ltmake_dataset_nfl6_c  H5_FC_FUNC_(h5ltmake_dataset_nfl6_c, H5LTMAKE_DATASET_NFL6_C)
#   define nh5ltmake_dataset_nfl7_c  H5_FC_FUNC_(h5ltmake_dataset_nfl7_c, H5LTMAKE_DATASET_NFL7_C)
#   define nh5ltmake_dataset_ndl1_c  H5_FC_FUNC_(h5ltmake_dataset_ndl1_c, H5LTMAKE_DATASET_NDL1_C)
#   define nh5ltmake_dataset_ndl2_c  H5_FC_FUNC_(h5ltmake_dataset_ndl2_c, H5LTMAKE_DATASET_NDL2_C)
#   define nh5ltmake_dataset_ndl3_c  H5_FC_FUNC_(h5ltmake_dataset_ndl3_c, H5LTMAKE_DATASET_NDL3_C)
#   define nh5ltmake_dataset_ndl4_c  H5_FC_FUNC_(h5ltmake_dataset_ndl4_c, H5LTMAKE_DATASET_NDL4_C)
#   define nh5ltmake_dataset_ndl5_c  H5_FC_FUNC_(h5ltmake_dataset_ndl5_c, H5LTMAKE_DATASET_NDL5_C)
#   define nh5ltmake_dataset_ndl6_c  H5_FC_FUNC_(h5ltmake_dataset_ndl6_c, H5LTMAKE_DATASET_NDL6_C)
#   define nh5ltmake_dataset_ndl7_c  H5_FC_FUNC_(h5ltmake_dataset_ndl7_c, H5LTMAKE_DATASET_NDL7_C)
#   define nh5ltread_dataset_c       H5_FC_FUNC_(h5ltread_dataset_c, H5LTREAD_DATASET_C)
#   define nh5ltread_dataset_int1_c         H5_FC_FUNC_(h5ltread_dataset_int1_c, H5LTREAD_DATASET_INT1_C)
#   define nh5ltread_dataset_int2_c         H5_FC_FUNC_(h5ltread_dataset_int2_c, H5LTREAD_DATASET_INT2_C)
#   define nh5ltread_dataset_int3_c         H5_FC_FUNC_(h5ltread_dataset_int3_c, H5LTREAD_DATASET_INT3_C)
#   define nh5ltread_dataset_int4_c         H5_FC_FUNC_(h5ltread_dataset_int4_c, H5LTREAD_DATASET_INT4_C)
#   define nh5ltread_dataset_int5_c         H5_FC_FUNC_(h5ltread_dataset_int5_c, H5LTREAD_DATASET_INT5_C)
#   define nh5ltread_dataset_int6_c         H5_FC_FUNC_(h5ltread_dataset_int6_c, H5LTREAD_DATASET_INT6_C)
#   define nh5ltread_dataset_int7_c         H5_FC_FUNC_(h5ltread_dataset_int7_c, H5LTREAD_DATASET_INT7_C)
#   define nh5ltread_dataset_fl1_c         H5_FC_FUNC_(h5ltread_dataset_fl1_c, H5LTREAD_DATASET_FL1_C)
#   define nh5ltread_dataset_fl2_c         H5_FC_FUNC_(h5ltread_dataset_fl2_c, H5LTREAD_DATASET_FL2_C)
#   define nh5ltread_dataset_fl3_c         H5_FC_FUNC_(h5ltread_dataset_fl3_c, H5LTREAD_DATASET_FL3_C)
#   define nh5ltread_dataset_fl4_c         H5_FC_FUNC_(h5ltread_dataset_fl4_c, H5LTREAD_DATASET_FL4_C)
#   define nh5ltread_dataset_fl5_c         H5_FC_FUNC_(h5ltread_dataset_fl5_c, H5LTREAD_DATASET_FL5_C)
#   define nh5ltread_dataset_fl6_c         H5_FC_FUNC_(h5ltread_dataset_fl6_c, H5LTREAD_DATASET_FL6_C)
#   define nh5ltread_dataset_fl7_c         H5_FC_FUNC_(h5ltread_dataset_fl7_c, H5LTREAD_DATASET_FL7_C)
#   define nh5ltread_dataset_dl1_c         H5_FC_FUNC_(h5ltread_dataset_dl1_c, H5LTREAD_DATASET_DL1_C)
#   define nh5ltread_dataset_dl2_c         H5_FC_FUNC_(h5ltread_dataset_dl2_c, H5LTREAD_DATASET_DL2_C)
#   define nh5ltread_dataset_dl3_c         H5_FC_FUNC_(h5ltread_dataset_dl3_c, H5LTREAD_DATASET_DL3_C)
#   define nh5ltread_dataset_dl4_c         H5_FC_FUNC_(h5ltread_dataset_dl4_c, H5LTREAD_DATASET_DL4_C)
#   define nh5ltread_dataset_dl5_c         H5_FC_FUNC_(h5ltread_dataset_dl5_c, H5LTREAD_DATASET_DL5_C)
#   define nh5ltread_dataset_dl6_c         H5_FC_FUNC_(h5ltread_dataset_dl6_c, H5LTREAD_DATASET_DL6_C)
#   define nh5ltread_dataset_dl7_c         H5_FC_FUNC_(h5ltread_dataset_dl7_c, H5LTREAD_DATASET_DL7_C)
#   define nh5ltread_dataset_nint1_c         H5_FC_FUNC_(h5ltread_dataset_nint1_c, H5LTREAD_DATASET_NINT1_C)
#   define nh5ltread_dataset_nint2_c         H5_FC_FUNC_(h5ltread_dataset_nint2_c, H5LTREAD_DATASET_NINT2_C)
#   define nh5ltread_dataset_nint3_c         H5_FC_FUNC_(h5ltread_dataset_nint3_c, H5LTREAD_DATASET_NINT3_C)
#   define nh5ltread_dataset_nint4_c         H5_FC_FUNC_(h5ltread_dataset_nint4_c, H5LTREAD_DATASET_NINT4_C)
#   define nh5ltread_dataset_nint5_c         H5_FC_FUNC_(h5ltread_dataset_nint5_c, H5LTREAD_DATASET_NINT5_C)
#   define nh5ltread_dataset_nint6_c         H5_FC_FUNC_(h5ltread_dataset_nint6_c, H5LTREAD_DATASET_NINT6_C)
#   define nh5ltread_dataset_nint7_c         H5_FC_FUNC_(h5ltread_dataset_nint7_c, H5LTREAD_DATASET_NINT7_C)
#   define nh5ltread_dataset_nfl1_c         H5_FC_FUNC_(h5ltread_dataset_nfl1_c, H5LTREAD_DATASET_NFL1_C)
#   define nh5ltread_dataset_nfl2_c         H5_FC_FUNC_(h5ltread_dataset_nfl2_c, H5LTREAD_DATASET_NFL2_C)
#   define nh5ltread_dataset_nfl3_c         H5_FC_FUNC_(h5ltread_dataset_nfl3_c, H5LTREAD_DATASET_NFL3_C)
#   define nh5ltread_dataset_nfl4_c         H5_FC_FUNC_(h5ltread_dataset_nfl4_c, H5LTREAD_DATASET_NFL4_C)
#   define nh5ltread_dataset_nfl5_c         H5_FC_FUNC_(h5ltread_dataset_nfl5_c, H5LTREAD_DATASET_NFL5_C)
#   define nh5ltread_dataset_nfl6_c         H5_FC_FUNC_(h5ltread_dataset_nfl6_c, H5LTREAD_DATASET_NFL6_C)
#   define nh5ltread_dataset_nfl7_c         H5_FC_FUNC_(h5ltread_dataset_nfl7_c, H5LTREAD_DATASET_NFL7_C)
#   define nh5ltread_dataset_ndl1_c         H5_FC_FUNC_(h5ltread_dataset_ndl1_c, H5LTREAD_DATASET_NDL1_C)
#   define nh5ltread_dataset_ndl2_c         H5_FC_FUNC_(h5ltread_dataset_ndl2_c, H5LTREAD_DATASET_NDL2_C)
#   define nh5ltread_dataset_ndl3_c         H5_FC_FUNC_(h5ltread_dataset_ndl3_c, H5LTREAD_DATASET_NDL3_C)
#   define nh5ltread_dataset_ndl4_c         H5_FC_FUNC_(h5ltread_dataset_ndl4_c, H5LTREAD_DATASET_NDL4_C)
#   define nh5ltread_dataset_ndl5_c         H5_FC_FUNC_(h5ltread_dataset_ndl5_c, H5LTREAD_DATASET_NDL5_C)
#   define nh5ltread_dataset_ndl6_c         H5_FC_FUNC_(h5ltread_dataset_ndl6_c, H5LTREAD_DATASET_NDL6_C)
#   define nh5ltread_dataset_ndl7_c         H5_FC_FUNC_(h5ltread_dataset_ndl7_c, H5LTREAD_DATASET_NDL7_C)
#   define nh5ltmake_dataset_string_c  H5_FC_FUNC_(h5ltmake_dataset_string_c, H5LTMAKE_DATASET_STRING_C)
#   define nh5ltread_dataset_string_c  H5_FC_FUNC_(h5ltread_dataset_string_c, H5LTREAD_DATASET_STRING_C)

#   define nh5ltset_attribute_int_c    H5_FC_FUNC_(h5ltset_attribute_int_c, H5LTSET_ATTRIBUTE_INT_C)
#   define nh5ltset_attribute_float_c  H5_FC_FUNC_(h5ltset_attribute_float_c, H5LTSET_ATTRIBUTE_FLOAT_C)
#   define nh5ltset_attribute_double_c H5_FC_FUNC_(h5ltset_attribute_double_c, H5LTSET_ATTRIBUTE_DOUBLE_C)
#   define nh5ltset_attribute_string_c H5_FC_FUNC_(h5ltset_attribute_string_c, H5LTSET_ATTRIBUTE_STRING_C)

#   define nh5ltget_attribute_int_c    H5_FC_FUNC_(h5ltget_attribute_int_c, H5LTGET_ATTRIBUTE_INT_C)
#   define nh5ltget_attribute_float_c  H5_FC_FUNC_(h5ltget_attribute_float_c, H5LTGET_ATTRIBUTE_FLOAT_C)
#   define nh5ltget_attribute_double_c H5_FC_FUNC_(h5ltget_attribute_double_c, H5LTGET_ATTRIBUTE_DOUBLE_C)
#   define nh5ltget_attribute_string_c H5_FC_FUNC_(h5ltget_attribute_string_c, H5LTGET_ATTRIBUTE_STRING_C)

#   define nh5ltget_dataset_ndims_c    H5_FC_FUNC_(h5ltget_dataset_ndims_c, H5LTGET_DATASET_NDIMS_C)
#   define nh5ltfind_dataset_c         H5_FC_FUNC_(h5ltfind_dataset_c, H5LTFIND_DATASET_C)
#   define nh5ltget_dataset_info_c     H5_FC_FUNC_(h5ltget_dataset_info_c, H5LTGET_DATASET_INFO_C)

#   define nh5ltget_attribute_ndims_c  H5_FC_FUNC_(h5ltget_attribute_ndims_c, H5LTGET_ATTRIBUTE_NDIMS_C)
#   define nh5ltget_attribute_info_c   H5_FC_FUNC_(h5ltget_attribute_info_c, H5LTGET_ATTRIBUTE_INFO_C)

/*-------------------------------------------------------------------------
* Image
*-------------------------------------------------------------------------
*/
#   define nh5immake_image_8bit_c      H5_FC_FUNC_(h5immake_image_8bit_c, H5IMMAKE_IMAGE_8BIT_C)
#   define nh5immake_image_24bit_c     H5_FC_FUNC_(h5immake_image_24bit_c, H5IMMAKE_IMAGE_24BIT_C)
#   define nh5imread_image_c           H5_FC_FUNC_(h5imread_image_c, H5IMREAD_IMAGE_C)
#   define nh5imget_image_info_c       H5_FC_FUNC_(h5imget_image_info_c, H5IMGET_IMAGE_INFO_C)
#   define nh5imis_image_c             H5_FC_FUNC_(h5imis_image_c, H5IMIS_IMAGE_C)
#   define nh5immake_palette_c         H5_FC_FUNC_(h5immake_palette_c, H5IMMAKE_PALETTE_C)
#   define nh5imlink_palette_c         H5_FC_FUNC_(h5imlink_palette_c, H5IMLINK_PALETTE_C)
#   define nh5imunlink_palette_c       H5_FC_FUNC_(h5imunlink_palette_c, H5IMUNLINK_PALETTE_C)
#   define nh5imget_npalettes_c        H5_FC_FUNC_(h5imget_npalettes_c, H5IMGET_NPALETTES_C)
#   define nh5imget_palette_info_c     H5_FC_FUNC_(h5imget_palette_info_c, H5IMGET_PALETTE_INFO_C)
#   define nh5imget_palette_c          H5_FC_FUNC_(h5imget_palette_c, H5IMGET_PALETTE_C)
#   define nh5imis_palette_c           H5_FC_FUNC_(h5imis_palette_c, H5IMIS_PALETTE_C)

/*-------------------------------------------------------------------------
* Table
*-------------------------------------------------------------------------
*/
#   define nh5tbmake_table_c           H5_FC_FUNC_(h5tbmake_table_c, H5TBMAKE_TABLE_C)
#   define nh5tbwrite_field_name_c     H5_FC_FUNC_(h5tbwrite_field_name_c, H5TBWRITE_FIELD_NAME_C)
#   define nh5tbwrite_field_name_int_c    H5_FC_FUNC_(h5tbwrite_field_name_int_c, H5TBWRITE_FIELD_NAME_INT_C)
#   define nh5tbwrite_field_name_fl_c     H5_FC_FUNC_(h5tbwrite_field_name_fl_c, H5TBWRITE_FIELD_NAME_FL_C)
#   define nh5tbwrite_field_name_dl_c     H5_FC_FUNC_(h5tbwrite_field_name_dl_c, H5TBWRITE_FIELD_NAME_DL_C)
#   define nh5tbwrite_field_name_st_c     H5_FC_FUNC_(h5tbwrite_field_name_st_c, H5TBWRITE_FIELD_NAME_ST_C)
#   define nh5tbread_field_name_c      H5_FC_FUNC_(h5tbread_field_name_c, H5TBREAD_FIELD_NAME_C)
#   define nh5tbread_field_name_int_c     H5_FC_FUNC_(h5tbread_field_name_int_c, H5TBREAD_FIELD_NAME_INT_C)
#   define nh5tbread_field_name_fl_c      H5_FC_FUNC_(h5tbread_field_name_fl_c, H5TBREAD_FIELD_NAME_FL_C)
#   define nh5tbread_field_name_dl_c      H5_FC_FUNC_(h5tbread_field_name_dl_c, H5TBREAD_FIELD_NAME_DL_C)
#   define nh5tbread_field_name_st_c      H5_FC_FUNC_(h5tbread_field_name_st_c, H5TBREAD_FIELD_NAME_ST_C)
#   define nh5tbwrite_field_index_c    H5_FC_FUNC_(h5tbwrite_field_index_c, H5TBWRITE_FIELD_INDEX_C)
#   define nh5tbwrite_field_index_int_c   H5_FC_FUNC_(h5tbwrite_field_index_int_c, H5TBWRITE_FIELD_INDEX_INT_C)
#   define nh5tbwrite_field_index_fl_c    H5_FC_FUNC_(h5tbwrite_field_index_fl_c, H5TBWRITE_FIELD_INDEX_FL_C)
#   define nh5tbwrite_field_index_dl_c    H5_FC_FUNC_(h5tbwrite_field_index_dl_c, H5TBWRITE_FIELD_INDEX_DL_C)
#   define nh5tbwrite_field_index_st_c    H5_FC_FUNC_(h5tbwrite_field_index_st_c, H5TBWRITE_FIELD_INDEX_ST_C)
#   define nh5tbread_field_index_c     H5_FC_FUNC_(h5tbread_field_index_c, H5TBREAD_FIELD_INDEX_C)
#   define nh5tbread_field_index_int_c    H5_FC_FUNC_(h5tbread_field_index_int_c, H5TBREAD_FIELD_INDEX_INT_C)
#   define nh5tbread_field_index_fl_c     H5_FC_FUNC_(h5tbread_field_index_fl_c, H5TBREAD_FIELD_INDEX_FL_C)
#   define nh5tbread_field_index_dl_c     H5_FC_FUNC_(h5tbread_field_index_dl_c, H5TBREAD_FIELD_INDEX_DL_C)
#   define nh5tbread_field_index_st_c     H5_FC_FUNC_(h5tbread_field_index_st_c, H5TBREAD_FIELD_INDEX_ST_C)
#   define nh5tbinsert_field_c         H5_FC_FUNC_(h5tbinsert_field_c, H5TBINSERT_FIELD_C)
#   define nh5tbinsert_field_int_c        H5_FC_FUNC_(h5tbinsert_field_int_c, H5TBINSERT_FIELD_INT_C)
#   define nh5tbinsert_field_fl_c         H5_FC_FUNC_(h5tbinsert_field_fl_c, H5TBINSERT_FIELD_FL_C)
#   define nh5tbinsert_field_dl_c         H5_FC_FUNC_(h5tbinsert_field_dl_c, H5TBINSERT_FIELD_DL_C)
#   define nh5tbinsert_field_st_c         H5_FC_FUNC_(h5tbinsert_field_st_c, H5TBINSERT_FIELD_ST_C)
#   define nh5tbdelete_field_c         H5_FC_FUNC_(h5tbdelete_field_c, H5TBDELETE_FIELD_C)
#   define nh5tbget_table_info_c       H5_FC_FUNC_(h5tbget_table_info_c, H5TBGET_TABLE_INFO_C)
#   define nh5tbget_field_info_c       H5_FC_FUNC_(h5tbget_field_info_c, H5TBGET_FIELD_INFO_C)

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_c (hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     int_f *rank,
                     hsize_t_f *dims,
                     hid_t_f *type_id,
                     void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_int1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_int2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_int3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_int4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_int5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_int6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_int7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);


HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_fl1_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_fl2_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_fl3_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_fl4_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_fl5_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_fl6_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_fl7_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);


HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_dl1_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_dl2_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_dl3_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_dl4_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_dl5_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_dl6_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_dl7_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank,
                         hsize_t_f *dims,
                         hid_t_f *type_id,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nint1_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nint2_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nint3_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nint4_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nint5_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nint6_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nint7_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *rank,
                           hsize_t_f *dims,
                           hid_t_f *type_id,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nfl1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nfl2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nfl3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nfl4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nfl5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nfl6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_nfl7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_ndl1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_ndl2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_ndl3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_ndl4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_ndl5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_ndl6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_ndl7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *rank,
                          hsize_t_f *dims,
                          hid_t_f *type_id,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_c (hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     hid_t_f *type_id,
                     void *buf,
                     hsize_t_f *dims);


HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_int1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_int2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_int3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_int4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_int5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_int6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_int7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_fl1_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_fl2_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_fl3_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_fl4_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_fl5_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_fl6_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);


HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_fl7_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_dl1_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_dl2_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_dl3_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_dl4_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_dl5_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_dl6_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_dl7_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         hid_t_f *type_id,
                         void *buf,
                         hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nint1_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nint2_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nint3_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nint4_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nint5_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nint6_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nint7_c (hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           hid_t_f *type_id,
                           void *buf,
                           hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nfl1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nfl2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nfl3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nfl4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nfl5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nfl6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_nfl7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_ndl1_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_ndl2_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_ndl3_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_ndl4_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_ndl5_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_ndl6_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_ndl7_c (hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          hid_t_f *type_id,
                          void *buf,
                          hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltset_attribute_int_c(hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd dsetname,
                         int_f *attrnamelen,
                         _fcd attrname,
                         size_t_f *size,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltset_attribute_float_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd dsetname,
                           int_f *attrnamelen,
                           _fcd attrname,
                           size_t_f *size,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltset_attribute_double_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd dsetname,
                            int_f *attrnamelen,
                            _fcd attrname,
                            size_t_f *size,
                            void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltset_attribute_string_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd dsetname,
                            int_f *attrnamelen,
                            _fcd attrname,
                            int_f *buflen,
                            void *buf);


HDF5_HL_F90CSTUBDLL
int_f
nh5ltget_attribute_int_c(hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd dsetname,
                         int_f *attrnamelen,
                         _fcd attrname,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltget_attribute_float_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd dsetname,
                           int_f *attrnamelen,
                           _fcd attrname,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltget_attribute_double_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd dsetname,
                            int_f *attrnamelen,
                            _fcd attrname,
                            void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltget_attribute_string_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd dsetname,
                            int_f *attrnamelen,
                            _fcd attrname,
                            void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltget_dataset_ndims_c(hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *rank);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltfind_dataset_c(hid_t_f *loc_id,
                    int_f *namelen,
                    _fcd name);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltget_dataset_info_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        hsize_t_f *dims,
                        int_f *type_class,
                        size_t_f *type_size);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltget_attribute_ndims_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd dsetname,
                           int_f *attrnamelen,
                           _fcd attrname,
                           int_f *rank);
HDF5_HL_F90CSTUBDLL
int_f
nh5ltget_attribute_info_c(hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *attrnamelen,
                          _fcd attrname,
                          hsize_t_f *dims,
                          int_f *type_class,
                          size_t_f *type_size);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltmake_dataset_string_c (hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *buflen,
                            char *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5ltread_dataset_string_c (hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            char *buf);

/*-------------------------------------------------------------------------
* Image
*-------------------------------------------------------------------------
*/

HDF5_HL_F90CSTUBDLL
int_f
nh5immake_image_8bit_c (hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        hsize_t_f *width,
                        hsize_t_f *height,
                        int_f *buf);
HDF5_HL_F90CSTUBDLL
int_f
nh5imread_image_c (hid_t_f *loc_id,
                   int_f *namelen,
                   _fcd name,
                   int_f *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5immake_image_24bit_c (hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *ilen,
                         _fcd il,
                         hsize_t_f *width,
                         hsize_t_f *height,
                         void *buf);
HDF5_HL_F90CSTUBDLL
int_f
nh5imget_image_info_c(hid_t_f *loc_id,
                      int_f *namelen,
                      _fcd name,
                      hsize_t_f *width,
                      hsize_t_f *height,
                      hsize_t_f *planes,
                      hsize_t_f *npals,
                      int_f *ilen,
                      _fcd interlace);


HDF5_HL_F90CSTUBDLL
int_f
nh5imis_image_c(hid_t_f *loc_id,
                int_f *namelen,
                _fcd name);


HDF5_HL_F90CSTUBDLL
int_f
nh5immake_palette_c (hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     hsize_t_f *dims,
                     void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5imlink_palette_c (hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     int_f *ilen,
                     _fcd pal_name);

HDF5_HL_F90CSTUBDLL
int_f
nh5imunlink_palette_c (hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       int_f *ilen,
                       _fcd pal_name);

HDF5_HL_F90CSTUBDLL
int_f
nh5imget_npalettes_c(hid_t_f *loc_id,
                     int_f *namelen,
                     _fcd name,
                     hsize_t_f *npals);


HDF5_HL_F90CSTUBDLL
int_f
nh5imget_palette_info_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *pal_number,
                        hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
nh5imget_palette_c(hid_t_f *loc_id,
                   int_f *namelen,
                   _fcd name,
                   int_f *pal_number,
                   void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5imis_palette_c(hid_t_f *loc_id,
                  int_f *namelen,
                  _fcd name);



/*-------------------------------------------------------------------------
* Table
*-------------------------------------------------------------------------
*/

HDF5_HL_F90CSTUBDLL
int_f
nh5tbmake_table_c(int_f *namelen1,
                  _fcd name1,
                  hid_t_f *loc_id,
                  int_f *namelen,
                  _fcd name,
                  hsize_t_f *nfields,
                  hsize_t_f *nrecords,
                  size_t_f *type_size,
                  size_t_f *field_offset,
                  hid_t_f *field_types,
                  hsize_t_f *chunk_size,
                  int_f *compress,
                  int_f *char_len_field_names, /* field_names lenghts */
      int_f *max_char_size_field_names, /* char len of fields */
                  _fcd buf);          /* field_names */

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_name_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *namelen1,
                        _fcd field_name,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_name_int_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *namelen1,
                            _fcd field_name,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_name_fl_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *namelen1,
                           _fcd field_name,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_name_dl_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *namelen1,
                           _fcd field_name,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_name_st_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *namelen1,
                           _fcd field_name,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_name_c(hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       int_f *namelen1,
                       _fcd field_name,
                       hsize_t_f *start,
                       hsize_t_f *nrecords,
                       size_t_f *type_size,
                       void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_name_int_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *namelen1,
                           _fcd field_name,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_name_fl_c(hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *namelen1,
                          _fcd field_name,
                          hsize_t_f *start,
                          hsize_t_f *nrecords,
                          size_t_f *type_size,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_name_dl_c(hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *namelen1,
                          _fcd field_name,
                          hsize_t_f *start,
                          hsize_t_f *nrecords,
                          size_t_f *type_size,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_name_st_c(hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *namelen1,
                          _fcd field_name,
                          hsize_t_f *start,
                          hsize_t_f *nrecords,
                          size_t_f *type_size,
                          void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_index_c(hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *field_index,
                         hsize_t_f *start,
                         hsize_t_f *nrecords,
                         size_t_f *type_size,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_index_int_c(hid_t_f *loc_id,
                             int_f *namelen,
                             _fcd name,
                             int_f *field_index,
                             hsize_t_f *start,
                             hsize_t_f *nrecords,
                             size_t_f *type_size,
                             void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_index_fl_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *field_index,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_index_dl_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *field_index,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbwrite_field_index_st_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *field_index,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_index_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *field_index,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_index_int_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *field_index,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_index_fl_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *field_index,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_index_dl_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *field_index,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbread_field_index_st_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *field_index,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbinsert_field_c(hid_t_f *loc_id,
                    int_f *namelen,
                    _fcd name,
                    int_f *namelen1,
                    _fcd field_name,
                    hid_t_f *field_type,
                    int_f *position,
                    void *buf);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbinsert_field_int_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *namelen1,
                        _fcd field_name,
                        hid_t_f *field_type,
                        int_f *position,
                        void *buf);
HDF5_HL_F90CSTUBDLL
int_f
nh5tbinsert_field_fl_c(hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       int_f *namelen1,
                       _fcd field_name,
                       hid_t_f *field_type,
                       int_f *position,
                       void *buf);
HDF5_HL_F90CSTUBDLL
int_f
nh5tbinsert_field_dl_c(hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       int_f *namelen1,
                       _fcd field_name,
                       hid_t_f *field_type,
                       int_f *position,
                       void *buf);
HDF5_HL_F90CSTUBDLL
int_f
nh5tbinsert_field_st_c(hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       int_f *namelen1,
                       _fcd field_name,
                       hid_t_f *field_type,
                       int_f *position,
                       void *buf);
HDF5_HL_F90CSTUBDLL
int_f
nh5tbdelete_field_c(hid_t_f *loc_id,
                    int_f *namelen,
                    _fcd name,
                    int_f *namelen1,
                    _fcd field_name);


HDF5_HL_F90CSTUBDLL
int_f
nh5tbget_table_info_c(hid_t_f *loc_id,
                      int_f *namelen,
                      _fcd name,
                      hsize_t_f *nfields,
                      hsize_t_f *nrecords);

HDF5_HL_F90CSTUBDLL
int_f
nh5tbget_field_info_c(hid_t_f *loc_id,
                      int_f *namelen,
                      _fcd name,
                      hsize_t_f *nfields,
                      size_t_f *field_sizes,
                      size_t_f *field_offsets,
                      size_t_f *type_size,
                      int_f *namelen2,
          int_f *lenmax,
                      _fcd field_names,
          int_f *maxlen_out);    


#endif /* _H5LTf90proto_H */
