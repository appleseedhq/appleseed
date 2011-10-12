/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

#include <math.h>

/* This files contains C stubs for H5D Fortran APIs */

#include "H5TBprivate.h"
#include "H5LTf90proto.h"
#include "H5Eprivate.h"

/*-------------------------------------------------------------------------
* Function: h5tbmake_table_c
*
* Purpose: Call H5TBmake_table
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 06, 2004
*
* Comments:
*
*-------------------------------------------------------------------------
*/
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
                  char *field_names)            /* field_names */
{
    char *c_name = NULL;
    char *c_name1 = NULL;
    hsize_t num_elem;
    hsize_t i;
    hsize_t c_nfields = (hsize_t)*nfields;
    size_t *c_field_offset = NULL;
    hid_t *c_field_types = NULL;
    char **c_field_names = NULL;
    char *tmp = NULL, *tmp_p;
    int_f ret_value = 0;

    num_elem = *nfields;

    /*
     * convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_name1 = (char *)HD5f2cstring(name1, (size_t)*namelen1)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_field_offset = (size_t *)HDmalloc(sizeof(size_t) * (size_t)c_nfields)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_field_types = (hid_t *)HDmalloc(sizeof(hid_t) * (size_t)c_nfields)))
        HGOTO_DONE(FAIL)

    for(i = 0; i < num_elem; i++) {
        c_field_offset[i] = field_offset[i];
        c_field_types[i] = field_types[i];
    } /* end for */

    /*
     * allocate array of character pointers
     */
    if(NULL == (c_field_names = (char **)HDcalloc((size_t)num_elem, sizeof(char *))))
        HGOTO_DONE(FAIL)

    /* copy data to long C string */
    if(NULL == (tmp = (char *)HD5f2cstring(field_names, (size_t)*(max_char_size_field_names)*(size_t)num_elem)))
        HGOTO_DONE(FAIL)
    /*
     * move data from temorary buffer
     */
    tmp_p = tmp;
    for(i = 0; i < num_elem; i++) {
        if(NULL == (c_field_names[i] = (char *)HDmalloc((size_t)char_len_field_names[i] + 1)))
            HGOTO_DONE(FAIL)
        HDmemcpy(c_field_names[i], tmp_p, (size_t)char_len_field_names[i]);
	c_field_names[i][char_len_field_names[i]] = '\0';

        tmp_p = tmp_p + *max_char_size_field_names;
    } /* end for */

    /*
     * call H5TBmake_table function.
     */
    if(H5TBmake_table(c_name1, (hid_t)*loc_id, c_name, c_nfields, (hsize_t)*nrecords,
            (size_t)*type_size, (const char **)c_field_names, c_field_offset, c_field_types,
            (hsize_t)*chunk_size, NULL, *compress, NULL) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);
    if(c_name1)
        HDfree(c_name1);
    if(c_field_names) {
        for(i = 0; i < num_elem; i++) {
            if(c_field_names[i])
                HDfree(c_field_names[i]);
        } /* end for */
	HDfree(c_field_names);
    } /* end if */
    if(tmp)
        HDfree(tmp);
    if(c_field_offset)
        HDfree(c_field_offset);
    if(c_field_types)
        HDfree(c_field_types);

    return ret_value;
} /* end nh5tbmake_table_c() */

/*-------------------------------------------------------------------------
* Function: h5tbwrite_field_name_c
*
* Purpose: Call H5TBwrite_fields_name
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 12, 2004
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5tbwrite_field_name_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *namelen1,
                        _fcd field_name,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf)
{
    char   *c_name = NULL;
    char   *c_name1 = NULL;
    size_t  c_type_size[1]  = {(size_t)*type_size};
    int_f   ret_value = 0;

    /*
     * convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_name1 = (char *)HD5f2cstring(field_name, (size_t)*namelen1)))
        HGOTO_DONE(FAIL)

    /*
     * call H5TBwrite_fields_name function.
     */
    if(H5TBwrite_fields_name((hid_t)*loc_id, c_name, c_name1, (hsize_t)*start,
            (hsize_t)*nrecords, c_type_size[0], 0, c_type_size, buf) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);
    if(c_name1)
        HDfree(c_name1);

    return ret_value;
}

int_f
nh5tbwrite_field_name_int_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *namelen1,
                            _fcd field_name,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf)
{
    return nh5tbwrite_field_name_c(loc_id, namelen, name, namelen1, field_name,
            start, nrecords, type_size, buf);
}

int_f
nh5tbwrite_field_name_fl_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *namelen1,
                           _fcd field_name,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf)
{
    return nh5tbwrite_field_name_c(loc_id, namelen, name, namelen1, field_name,
            start, nrecords, type_size, buf);
}

int_f
nh5tbwrite_field_name_dl_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *namelen1,
                           _fcd field_name,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf)
{
    return nh5tbwrite_field_name_c(loc_id, namelen, name, namelen1, field_name,
            start, nrecords, type_size, buf);
}

int_f
nh5tbwrite_field_name_st_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *namelen1,
                           _fcd field_name,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf)
{
    return nh5tbwrite_field_name_c(loc_id, namelen, name, namelen1, field_name,
            start, nrecords, type_size, buf);
}

/*-------------------------------------------------------------------------
* Function: h5tbread_field_name_c
*
* Purpose: Call H5TBread_fields_name
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 12, 2004
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5tbread_field_name_c(hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       int_f *namelen1,
                       _fcd field_name,
                       hsize_t_f *start,
                       hsize_t_f *nrecords,
                       size_t_f *type_size,
                       void *buf)
{
    char   *c_name = NULL;
    char   *c_name1 = NULL;
    size_t  c_type_size[1]  = {(size_t)*type_size};
    int_f   ret_value = 0;

    /*
     * convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_name1 = (char *)HD5f2cstring(field_name, (size_t)*namelen1)))
        HGOTO_DONE(FAIL)

    /*
     * call H5TBread_fields_name function.
     */
    if(H5TBread_fields_name((hid_t)*loc_id, c_name, c_name1, (hsize_t)*start,
            (hsize_t)*nrecords, c_type_size[0], 0, c_type_size, buf) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);
    if(c_name1)
        HDfree(c_name1);

    return ret_value;
}

int_f
nh5tbread_field_name_int_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *namelen1,
                           _fcd field_name,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf)
{
    return nh5tbread_field_name_c(loc_id, namelen, name, namelen1, field_name,
            start, nrecords, type_size, buf);
}

int_f
nh5tbread_field_name_fl_c(hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *namelen1,
                          _fcd field_name,
                          hsize_t_f *start,
                          hsize_t_f *nrecords,
                          size_t_f *type_size,
                          void *buf)
{
    return nh5tbread_field_name_c(loc_id, namelen, name, namelen1, field_name,
            start, nrecords, type_size, buf);
}

int_f
nh5tbread_field_name_dl_c(hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *namelen1,
                          _fcd field_name,
                          hsize_t_f *start,
                          hsize_t_f *nrecords,
                          size_t_f *type_size,
                          void *buf)
{
    return nh5tbread_field_name_c(loc_id, namelen, name, namelen1, field_name,
            start, nrecords, type_size, buf);
}

int_f
nh5tbread_field_name_st_c(hid_t_f *loc_id,
                          int_f *namelen,
                          _fcd name,
                          int_f *namelen1,
                          _fcd field_name,
                          hsize_t_f *start,
                          hsize_t_f *nrecords,
                          size_t_f *type_size,
                          void *buf)
{
    return nh5tbread_field_name_c(loc_id, namelen, name, namelen1, field_name,
            start, nrecords, type_size, buf);
}

/*-------------------------------------------------------------------------
* Function: h5tbwrite_field_index_c
*
* Purpose: Call H5TBwrite_fields_index
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 12, 2004
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5tbwrite_field_index_c(hid_t_f *loc_id,
                         int_f *namelen,
                         _fcd name,
                         int_f *field_index,
                         hsize_t_f *start,
                         hsize_t_f *nrecords,
                         size_t_f *type_size,
                         void *buf)
{
    char   *c_name =  NULL;
    size_t  c_type_size  = *type_size;
    int     c_field_index = *field_index - 1; /* C zero based index */
    int_f   ret_value = 0;

    /*
     * convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)


    /*
     * call H5TBwrite_fields_name function.
     */
    if(H5TBwrite_fields_index((hid_t)*loc_id, c_name, (hsize_t)1, &c_field_index,
            (hsize_t)*start, (hsize_t)*nrecords, c_type_size, 0, &c_type_size, buf) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

int_f
nh5tbwrite_field_index_int_c(hid_t_f *loc_id,
                             int_f *namelen,
                             _fcd name,
                             int_f *field_index,
                             hsize_t_f *start,
                             hsize_t_f *nrecords,
                             size_t_f *type_size,
                             void *buf)
{
    return nh5tbwrite_field_index_c(loc_id, namelen, name, field_index, start,
            nrecords, type_size, buf);
}

int_f
nh5tbwrite_field_index_fl_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *field_index,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf)
{
    return nh5tbwrite_field_index_c(loc_id, namelen, name, field_index, start,
            nrecords, type_size, buf);
}

int_f
nh5tbwrite_field_index_dl_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *field_index,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf)
{
    return nh5tbwrite_field_index_c(loc_id, namelen, name, field_index, start,
            nrecords, type_size, buf);
}

int_f
nh5tbwrite_field_index_st_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *field_index,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf)
{
    return nh5tbwrite_field_index_c(loc_id, namelen, name, field_index, start,
            nrecords, type_size, buf);
}

/*-------------------------------------------------------------------------
* Function: h5tbread_field_index_c
*
* Purpose: Call H5TBread_fields_index
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 12, 2004
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5tbread_field_index_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *field_index,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf)
{
    char   *c_name = NULL;
    size_t  c_type_size  = *type_size;
    int     c_field_index = *field_index - 1; /* C zero based index */
    int_f   ret_value = 0;

    /*
     * convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)

    /*
     * call H5TBread_fields_index function.
     */
    if(H5TBread_fields_index((hid_t)*loc_id, c_name,(hsize_t)1, &c_field_index,
            (hsize_t)*start, (hsize_t)*nrecords, c_type_size, 0, &c_type_size, buf) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

int_f
nh5tbread_field_index_int_c(hid_t_f *loc_id,
                            int_f *namelen,
                            _fcd name,
                            int_f *field_index,
                            hsize_t_f *start,
                            hsize_t_f *nrecords,
                            size_t_f *type_size,
                            void *buf)
{
    return nh5tbread_field_index_c(loc_id, namelen, name, field_index, start,
        nrecords, type_size, buf);
}

int_f
nh5tbread_field_index_fl_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *field_index,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf)
{
    return nh5tbread_field_index_c(loc_id, namelen, name, field_index, start,
            nrecords, type_size, buf);
}

int_f
nh5tbread_field_index_dl_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *field_index,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf)
{
    return nh5tbread_field_index_c(loc_id, namelen, name, field_index, start,
            nrecords, type_size, buf);
}

int_f
nh5tbread_field_index_st_c(hid_t_f *loc_id,
                           int_f *namelen,
                           _fcd name,
                           int_f *field_index,
                           hsize_t_f *start,
                           hsize_t_f *nrecords,
                           size_t_f *type_size,
                           void *buf)
{
    return nh5tbread_field_index_c(loc_id, namelen, name, field_index, start,
            nrecords, type_size, buf);
}

/*-------------------------------------------------------------------------
* Function: h5tbinsert_field_c
*
* Purpose: Call H5TBinsert_field
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 13, 2004
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5tbinsert_field_c(hid_t_f *loc_id,
                    int_f *namelen,
                    _fcd name,
                    int_f *namelen1,
                    _fcd field_name,
                    hid_t_f *field_type,
                    int_f *position,
                    void *buf)
{
    char   *c_name = NULL;
    char   *c_name1 = NULL;
    int_f   ret_value = 0;

    /*
     * convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_name1 = (char *)HD5f2cstring(field_name, (size_t)*namelen1)))
        HGOTO_DONE(FAIL)
    /*
     * call H5TBinsert_field function.
     */

    if(H5TBinsert_field((hid_t)*loc_id, c_name, c_name1, (hid_t)*field_type, 
            (hsize_t)*position, NULL, buf) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);
    if(c_name1)
        HDfree(c_name1);

    return ret_value;
}

int_f
nh5tbinsert_field_int_c(hid_t_f *loc_id,
                        int_f *namelen,
                        _fcd name,
                        int_f *namelen1,
                        _fcd field_name,
                        hid_t_f *field_type,
                        int_f *position,
                        void *buf)
{
    return nh5tbinsert_field_c(loc_id, namelen, name, namelen1, field_name,
            field_type, position, buf);
}

int_f
nh5tbinsert_field_fl_c(hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       int_f *namelen1,
                       _fcd field_name,
                       hid_t_f *field_type,
                       int_f *position,
                       void *buf)
{
    return nh5tbinsert_field_c(loc_id, namelen, name, namelen1, field_name,
            field_type, position, buf);
}

int_f
nh5tbinsert_field_dl_c(hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       int_f *namelen1,
                       _fcd field_name,
                       hid_t_f *field_type,
                       int_f *position,
                       void *buf)
{
    return nh5tbinsert_field_c(loc_id, namelen, name, namelen1, field_name,
            field_type, position, buf);
}

int_f
nh5tbinsert_field_st_c(hid_t_f *loc_id,
                       int_f *namelen,
                       _fcd name,
                       int_f *namelen1,
                       _fcd field_name,
                       hid_t_f *field_type,
                       int_f *position,
                       void *buf)
{
    return nh5tbinsert_field_c(loc_id, namelen, name, namelen1, field_name,
            field_type, position, buf);
}

/*-------------------------------------------------------------------------
* Function: h5tbdelete_field_c
*
* Purpose: Call H5TBdelete_field
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 13, 2004
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5tbdelete_field_c(hid_t_f *loc_id,
                    int_f *namelen,
                    _fcd name,
                    int_f *namelen1,
                    _fcd field_name)
{
    char   *c_name = NULL;
    char   *c_name1 = NULL;
    int_f   ret_value = 0;

    /*
     * convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_name1 = (char *)HD5f2cstring(field_name, (size_t)*namelen1)))
        HGOTO_DONE(FAIL)

    /*
     * call H5TBinsert_field function.
     */
    if(H5TBdelete_field((hid_t)*loc_id, c_name, c_name1) < 0)
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);
    if(c_name1)
        HDfree(c_name1);

    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: h5tbget_table_info_c
*
* Purpose: Call H5TBread_fields_index
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 12, 2004
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5tbget_table_info_c(hid_t_f *loc_id,
                      int_f *namelen,
                      _fcd name,
                      hsize_t_f *nfields,
                      hsize_t_f *nrecords)
{
    char   *c_name = NULL;
    hsize_t c_nfields;
    hsize_t c_nrecords;
    int_f   ret_value = 0;

    /*
     * convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)


    /*
     * call H5TBread_fields_index function.
     */

    if(H5TBget_table_info((hid_t)*loc_id, c_name, &c_nfields, &c_nrecords) < 0)
        HGOTO_DONE(FAIL)
    *nfields = (hsize_t_f) c_nfields;
    *nrecords = (hsize_t_f) c_nrecords;

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: h5tbget_field_info_c
*
* Purpose: Call H5TBget_field_info
*
* Return: Success: 0, Failure: -1
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: October 13, 2004
*
* Comments:
*
*-------------------------------------------------------------------------
*/
int_f
nh5tbget_field_info_c(hid_t_f *loc_id,
                      int_f *namelen,
                      _fcd name,
                      hsize_t_f *nfields,
                      size_t_f *field_sizes,
                      size_t_f *field_offsets,
                      size_t_f *type_size,
                      int_f *namelen2,       /* field_names lenghts */
		      int_f *lenmax,         /* character len max */
                      _fcd field_names,      /* field_names */
                      int_f *maxlen_out)

{
    char   *c_name = NULL;
    hsize_t num_elem;
    hsize_t c_nfields  = *nfields;
    size_t *c_field_sizes = NULL;
    size_t *c_field_offsets = NULL;
    size_t  c_type_size;
    char  **c_field_names = NULL;
    char   *tmp = NULL, *tmp_p;
    hsize_t i;
    int_f   ret_value = 0;
    size_t c_lenmax;
    size_t length = 0;

    c_lenmax = (size_t)*lenmax;

    num_elem = c_nfields;

    /*
     * convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_field_offsets =  (size_t *)HDmalloc(sizeof(size_t) * (size_t)c_nfields)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_field_sizes =  (size_t *)HDmalloc(sizeof(size_t) * (size_t)c_nfields)))
        HGOTO_DONE(FAIL)
    if(NULL == (c_field_names = (char **)HDcalloc((size_t)c_nfields, sizeof(char *))))
        HGOTO_DONE(FAIL)

    for(i = 0; i < c_nfields; i++)
        if(NULL == (c_field_names[i] = (char *)HDmalloc(sizeof(char) * HLTB_MAX_FIELD_LEN)))
            HGOTO_DONE(FAIL)

    /*
     * call H5TBget_field_info function.
     */
    if(H5TBget_field_info((hid_t)*loc_id, c_name, c_field_names, c_field_sizes,
            c_field_offsets, &c_type_size) < 0)
        HGOTO_DONE(FAIL)
   
    /* return values */

    /* names array */
    if(NULL == (tmp = (char *)HDmalloc((c_lenmax * (size_t)c_nfields) + 1)))
        HGOTO_DONE(FAIL)
    tmp_p = tmp;
    HDmemset(tmp, ' ', c_lenmax * (size_t)c_nfields);
    tmp[c_lenmax * c_nfields] = '\0';
    for(i = 0; i < c_nfields; i++) {
         size_t field_name_len = HDstrlen(c_field_names[i]);

         HDmemcpy(tmp_p, c_field_names[i], field_name_len);
	 namelen2[i] = (int_f)field_name_len;
	 length = MAX(length, strlen((c_field_names[i])));
	 tmp_p = tmp_p + c_lenmax;
    } /* end for */

    HD5packFstring(tmp, _fcdtocp(field_names), (size_t)( c_lenmax* c_nfields));

    *type_size = (size_t_f)c_type_size;
    for(i = 0; i < num_elem; i++) {
        field_sizes[i]   = (size_t_f)c_field_sizes[i];
        field_offsets[i] = (size_t_f)c_field_offsets[i];
    } /* end for */

    *maxlen_out = (int_f)length; 

done:
    if(c_name)
        HDfree(c_name);
    if(c_field_names) {
        for(i = 0; i < num_elem; i++)
            if(c_field_names[i])
                HDfree(c_field_names[i]);
        HDfree(c_field_names);
    } /* end if */
    if(tmp)
        HDfree(tmp);
    if(c_field_offsets)
        HDfree(c_field_offsets);
    if(c_field_sizes)
        HDfree(c_field_sizes);

    return ret_value;
}

