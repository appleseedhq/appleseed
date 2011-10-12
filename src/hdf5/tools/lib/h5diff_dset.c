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

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5diff.h"
#include "ph5diff.h"


/*-------------------------------------------------------------------------
* Function: diff_dataset
*
* Purpose: check for comparable datasets and read into a compatible
*  memory type
*
* Return: Number of differences found
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: May 9, 2003
*
*-------------------------------------------------------------------------
*/
hsize_t diff_dataset( hid_t file1_id,
                      hid_t file2_id,
                      const char *obj1_name,
                      const char *obj2_name,
                      diff_opt_t *options)
{
    hid_t   did1 = -1;
    hid_t   did2 = -1;
    hid_t   dcpl1 = -1;
    hid_t   dcpl2 = -1;
    hsize_t nfound = 0;

    /*-------------------------------------------------------------------------
    * open the handles
    *-------------------------------------------------------------------------
    */
    /* disable error reporting */
    H5E_BEGIN_TRY
    {
        /* Open the datasets */
        if((did1 = H5Dopen2(file1_id, obj1_name, H5P_DEFAULT)) < 0)
        {
            parallel_print("Cannot open dataset <%s>\n", obj1_name);
            goto error;
        }
        if((did2 = H5Dopen2(file2_id, obj2_name, H5P_DEFAULT)) < 0)
        {
            parallel_print("Cannot open dataset <%s>\n", obj2_name);
            goto error;
        }
        /* enable error reporting */
    } H5E_END_TRY;


    if((dcpl1 = H5Dget_create_plist(did1)) < 0)
        goto error;
    if((dcpl2 = H5Dget_create_plist(did2)) < 0)
    {
        goto error;
    }

    /*-------------------------------------------------------------------------
    * check if the dataset creation property list has filters that
    * are not registered in the current configuration
    * 1) the external filters GZIP and SZIP might not be available
    * 2) the internal filters might be turned off
    *-------------------------------------------------------------------------
    */
    if ((h5tools_canreadf((options->m_verbose?obj1_name:NULL),dcpl1)==1) &&
        (h5tools_canreadf((options->m_verbose?obj2_name:NULL),dcpl2)==1))
    {
        nfound=diff_datasetid(did1,
            did2,
            obj1_name,
            obj2_name,
            options);
    }
    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    /* disable error reporting */
    H5E_BEGIN_TRY {
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
        H5Dclose(did1);
        H5Dclose(did2);
        /* enable error reporting */
    } H5E_END_TRY;


    return nfound;

error:
    options->err_stat=1;
    /* disable error reporting */
    H5E_BEGIN_TRY {
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
        H5Dclose(did1);
        H5Dclose(did2);
        /* enable error reporting */
    } H5E_END_TRY;

    return nfound;
}

/*-------------------------------------------------------------------------
* Function: diff_datasetid
*
* Purpose: check for comparable datasets and read into a compatible
*  memory type
*
* Return: Number of differences found
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: May 9, 2003
*
* Modifications:
*
*
* October 2006:  Read by hyperslabs for big datasets.
*
*  A threshold of H5TOOLS_MALLOCSIZE (128 MB) is the limit upon which I/O hyperslab is done
*  i.e., if the memory needed to read a dataset is greater than this limit,
*  then hyperslab I/O is done instead of one operation I/O
*  For each dataset, the memory needed is calculated according to
*
*  memory needed = number of elements * size of each element
*
*  if the memory needed is lower than H5TOOLS_MALLOCSIZE, then the following operations
*  are done
*
*  H5Dread( input_dataset1 )
*  H5Dread( input_dataset2 )
*
*  with all elements in the datasets selected. If the memory needed is greater than
*  H5TOOLS_MALLOCSIZE, then the following operations are done instead:
*
*  a strip mine is defined for each dimension k (a strip mine is defined as a
*  hyperslab whose size is memory manageable) according to the formula
*
*  (1) strip_mine_size[k ] = MIN(dimension[k ], H5TOOLS_BUFSIZE / size of memory type)
*
*  where H5TOOLS_BUFSIZE is a constant currently defined as 1MB. This formula assures
*  that for small datasets (small relative to the H5TOOLS_BUFSIZE constant), the strip
*  mine size k is simply defined as its dimension k, but for larger datasets the
*  hyperslab size is still memory manageable.
*  a cycle is done until the number of elements in the dataset is reached. In each
*  iteration, two parameters are defined for the function H5Sselect_hyperslab,
*  the start and size of each hyperslab, according to
*
*  (2) hyperslab_size [k] = MIN(dimension[k] - hyperslab_offset[k], strip_mine_size [k])
*
*  where hyperslab_offset [k] is initially set to zero, and later incremented in
*  hyperslab_size[k] offsets. The reason for the operation
*
*  dimension[k] - hyperslab_offset[k]
*
*  in (2) is that, when using the strip mine size, it assures that the "remaining" part
*  of the dataset that does not fill an entire strip mine is processed.
*
*-------------------------------------------------------------------------
*/
hsize_t diff_datasetid( hid_t did1,
                        hid_t did2,
                        const char *obj1_name,
                        const char *obj2_name,
                        diff_opt_t *options)
{
    hid_t      sid1=-1;
    hid_t      sid2=-1;
    hid_t      f_tid1=-1;
    hid_t      f_tid2=-1;
    hid_t      m_tid1=-1;
    hid_t      m_tid2=-1;
    size_t     m_size1;
    size_t     m_size2;
    H5T_sign_t sign1;
    H5T_sign_t sign2;
    int        rank1;
    int        rank2;
    hsize_t    nelmts1;
    hsize_t    nelmts2;
    hsize_t    dims1[H5S_MAX_RANK];
    hsize_t    dims2[H5S_MAX_RANK];
    hsize_t    maxdim1[H5S_MAX_RANK];
    hsize_t    maxdim2[H5S_MAX_RANK];
    const char *name1=NULL;            /* relative names */
    const char *name2=NULL;
    hsize_t    storage_size1;
    hsize_t    storage_size2;
    hsize_t    nfound=0;               /* number of differences found */
    int        can_compare=1;          /* do diff or not */
    void       *buf1=NULL;
    void       *buf2=NULL;
    void       *sm_buf1=NULL;
    void       *sm_buf2=NULL;
    hid_t      sm_space;               /*stripmine data space */
    size_t     need;                   /* bytes needed for malloc */
    int        i;
    unsigned int  vl_data = 0;         /*contains VL datatypes */

    /* Get the dataspace handle */
    if ( (sid1 = H5Dget_space(did1)) < 0 )
        goto error;

    /* Get rank */
    if ( (rank1 = H5Sget_simple_extent_ndims(sid1)) < 0 )
        goto error;

    /* Get the dataspace handle */
    if ( (sid2 = H5Dget_space(did2)) < 0 )
        goto error;

    /* Get rank */
    if ( (rank2 = H5Sget_simple_extent_ndims(sid2)) < 0 )
        goto error;

    /* Get dimensions */
    if ( H5Sget_simple_extent_dims(sid1,dims1,maxdim1) < 0 )
        goto error;

    /* Get dimensions */
    if ( H5Sget_simple_extent_dims(sid2,dims2,maxdim2) < 0 )
    {
        goto error;
    }

    /*-------------------------------------------------------------------------
    * get the file data type
    *-------------------------------------------------------------------------
    */

    /* Get the data type */
    if ( (f_tid1 = H5Dget_type(did1)) < 0 )
        goto error;

    /* Get the data type */
    if ( (f_tid2 = H5Dget_type(did2)) < 0 )
    {
        goto error;
    }

    /*-------------------------------------------------------------------------
    * check for empty datasets
    *-------------------------------------------------------------------------
    */

    storage_size1=H5Dget_storage_size(did1);
    storage_size2=H5Dget_storage_size(did2);

    if (storage_size1==0 || storage_size2==0)
    {
        if ( (options->m_verbose||options->m_list_not_cmp) && obj1_name && obj2_name)
            parallel_print("Not comparable: <%s> or <%s> is an empty dataset\n", obj1_name, obj2_name);
        can_compare=0;
        options->not_cmp=1;
    }

    /*-------------------------------------------------------------------------
    * check for comparable TYPE and SPACE
    *-------------------------------------------------------------------------
    */

    if (diff_can_type(f_tid1,
        f_tid2,
        rank1,
        rank2,
        dims1,
        dims2,
        maxdim1,
        maxdim2,
        obj1_name,
        obj2_name,
        options,
        0)!=1)
    {
        can_compare=0;
    }

    /*-------------------------------------------------------------------------
    * memory type and sizes
    *-------------------------------------------------------------------------
    */
    if ((m_tid1=h5tools_get_native_type(f_tid1)) < 0)
        goto error;

    if ((m_tid2=h5tools_get_native_type(f_tid2)) < 0)
        goto error;

    m_size1 = H5Tget_size( m_tid1 );
    m_size2 = H5Tget_size( m_tid2 );

    /*-------------------------------------------------------------------------
    * check for different signed/unsigned types
    *-------------------------------------------------------------------------
    */

    sign1=H5Tget_sign(m_tid1);
    sign2=H5Tget_sign(m_tid2);
    if ( sign1 != sign2 )
    {
        if ((options->m_verbose||options->m_list_not_cmp) && obj1_name && obj2_name)
        {
            parallel_print("Not comparable: <%s> has sign %s ", obj1_name, get_sign(sign1));
            parallel_print("and <%s> has sign %s\n", obj2_name, get_sign(sign2));
        }

        can_compare=0;
        options->not_cmp=1;
    }
    
    /* Check if type is either VLEN-data or VLEN-string to reclaim any
     * VLEN memory buffer later */
    if( TRUE == h5tools_detect_vlen(m_tid1) )
        vl_data = TRUE;

    /*-------------------------------------------------------------------------
    * only attempt to compare if possible
    *-------------------------------------------------------------------------
    */
    if(can_compare) /* it is possible to compare */
    {

        /*-------------------------------------------------------------------------
        * get number of elements
        *-------------------------------------------------------------------------
        */
        nelmts1 = 1;
        for(i = 0; i < rank1; i++)
            nelmts1 *= dims1[i];

        nelmts2 = 1;
        for(i = 0; i < rank2; i++)
            nelmts2 *= dims2[i];

        HDassert(nelmts1 == nelmts2);

        /*-------------------------------------------------------------------------
        * "upgrade" the smaller memory size
        *-------------------------------------------------------------------------
        */

        if(m_size1 != m_size2) {
            if(m_size1 < m_size2) {
                H5Tclose(m_tid1);

                if((m_tid1 = h5tools_get_native_type(f_tid2)) < 0)
                    goto error;

                m_size1 = H5Tget_size(m_tid1);
            } /* end if */
            else {
                H5Tclose(m_tid2);

                if((m_tid2 = h5tools_get_native_type(f_tid1)) < 0)
                    goto error;

                m_size2 = H5Tget_size(m_tid2);
            } /* end else */
        } /* end if */
        HDassert(m_size1 == m_size2);

        /* print names */
        if(obj1_name)
            name1 = diff_basename(obj1_name);
        if(obj2_name)
            name2 = diff_basename(obj2_name);


        /*-------------------------------------------------------------------------
        * read/compare
        *-------------------------------------------------------------------------
        */

        need = (size_t)(nelmts1 * m_size1);  /* bytes needed */
        if(need < H5TOOLS_MALLOCSIZE) {
            buf1 = HDmalloc(need);
            buf2 = HDmalloc(need);
        } /* end if */

        if(buf1 != NULL && buf2 != NULL) {
            if(H5Dread(did1, m_tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1) < 0)
                goto error;
            if(H5Dread(did2, m_tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2) < 0)
                goto error;

            /* array diff */
            nfound = diff_array(buf1, buf2, nelmts1, (hsize_t)0, rank1, dims1,
                options, name1, name2, m_tid1, did1, did2);

            /* reclaim any VL memory, if necessary */
       	    if(vl_data) {
                H5Dvlen_reclaim(m_tid1, sid1, H5P_DEFAULT, buf1);
                H5Dvlen_reclaim(m_tid2, sid2, H5P_DEFAULT, buf2);
            } /* end if */
        } /* end if */
        else /* possibly not enough memory, read/compare by hyperslabs */
        {
            size_t        p_type_nbytes = m_size1; /*size of memory type */
            hsize_t       p_nelmts = nelmts1;      /*total selected elmts */
            hsize_t       elmtno;                  /*counter  */
            int           carry;                   /*counter carry value */

            /* stripmine info */
            hsize_t       sm_size[H5S_MAX_RANK];   /*stripmine size */
            hsize_t       sm_nbytes;               /*bytes per stripmine */
            hsize_t       sm_nelmts;               /*elements per stripmine*/

            /* hyperslab info */
            hsize_t       hs_offset[H5S_MAX_RANK]; /*starting offset */
            hsize_t       hs_size[H5S_MAX_RANK];   /*size this pass */
            hsize_t       hs_nelmts;               /*elements in request */
            hsize_t       zero[8];                 /*vector of zeros */

            /*
             * determine the strip mine size and allocate a buffer. The strip mine is
             * a hyperslab whose size is manageable.
             */
            sm_nbytes = p_type_nbytes;

            for(i = rank1; i > 0; --i) {
                hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;

                if(size == 0) /* datum size > H5TOOLS_BUFSIZE */
                    size = 1;
                sm_size[i - 1] = MIN(dims1[i - 1], size);
                sm_nbytes *= sm_size[i - 1];
                assert(sm_nbytes > 0);
            } /* end for */

	    /* malloc return code should be verified.
             * If fail, need to handle the error.
             * This else branch should be recoded as a separate function.
             * Note that there are many "goto error" within this branch
             * that fails to address freeing other objects created here.
	     * E.g., sm_space.
	     */
            sm_buf1 = malloc((size_t)sm_nbytes);
	    assert(sm_buf1);
            sm_buf2 = malloc((size_t)sm_nbytes);
	    assert(sm_buf2);

            sm_nelmts = sm_nbytes / p_type_nbytes;
            sm_space = H5Screate_simple(1, &sm_nelmts, NULL);

            /* the stripmine loop */
            memset(hs_offset, 0, sizeof hs_offset);
            memset(zero, 0, sizeof zero);

            for(elmtno = 0; elmtno < p_nelmts; elmtno += hs_nelmts) {
                /* calculate the hyperslab size */
                if(rank1 > 0) {
                    for(i = 0, hs_nelmts = 1; i < rank1; i++) {
                        hs_size[i] = MIN(dims1[i] - hs_offset[i], sm_size[i]);
                        hs_nelmts *= hs_size[i];
                    } /* end for */
                    if(H5Sselect_hyperslab(sid1, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0)
                        goto error;
                    if(H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0)
                        goto error;
                    if(H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL, &hs_nelmts, NULL) < 0)
                        goto error;
                } /* end if */
                else
                    hs_nelmts = 1;

                if(H5Dread(did1,m_tid1,sm_space,sid1,H5P_DEFAULT,sm_buf1) < 0)
                    goto error;
                if(H5Dread(did2,m_tid2,sm_space,sid2,H5P_DEFAULT,sm_buf2) < 0)
                    goto error;

                /* get array differences. in the case of hyperslab read, increment the number of differences
                found in each hyperslab and pass the position at the beggining for printing */
                nfound += diff_array(sm_buf1, sm_buf2, hs_nelmts, elmtno, rank1,
                    dims1, options, name1, name2, m_tid1, did1, did2);

                /* reclaim any VL memory, if necessary */
                if(vl_data) {
                    H5Dvlen_reclaim(m_tid1, sm_space, H5P_DEFAULT, sm_buf1);
                    H5Dvlen_reclaim(m_tid1, sm_space, H5P_DEFAULT, sm_buf2);
                } /* end if */

                /* calculate the next hyperslab offset */
                for(i = rank1, carry = 1; i > 0 && carry; --i) {
                    hs_offset[i - 1] += hs_size[i - 1];
                    if(hs_offset[i - 1] == dims1[i - 1])
                        hs_offset[i - 1] = 0;
                    else
                        carry = 0;
                } /* i */
            } /* elmtno */

            H5Sclose(sm_space);
        } /* hyperslab read */
    } /*can_compare*/

    /*-------------------------------------------------------------------------
     * compare attributes
     * the if condition refers to cases when the dataset is a referenced object
     *-------------------------------------------------------------------------
     */
    if(obj1_name)
        nfound += diff_attr(did1,did2,obj1_name,obj2_name,options);

    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */

    /* free */
    if(buf1 != NULL) {
        free(buf1);
        buf1 = NULL;
    } /* end if */
    if(buf2 != NULL) {
        free(buf2);
        buf2 = NULL;
    } /* end if */
    if(sm_buf1 != NULL) {
        free(sm_buf1);
        sm_buf1 = NULL;
    } /* end if */
    if(sm_buf2 != NULL) {
        free(sm_buf2);
        sm_buf2 = NULL;
    } /* end if */

    H5E_BEGIN_TRY {
        H5Sclose(sid1);
        H5Sclose(sid2);
        H5Tclose(f_tid1);
        H5Tclose(f_tid2);
        H5Tclose(m_tid1);
        H5Tclose(m_tid2);
    } H5E_END_TRY;

    return nfound;

error:
    options->err_stat=1;

    /* free */
    if (buf1!=NULL)
    {
        /* reclaim any VL memory, if necessary */
        if(vl_data) 
            H5Dvlen_reclaim(m_tid1, sid1, H5P_DEFAULT, buf1);
        free(buf1);
        buf1=NULL;
    }
    if (buf2!=NULL)
    {
        /* reclaim any VL memory, if necessary */
        if(vl_data) 
            H5Dvlen_reclaim(m_tid2, sid2, H5P_DEFAULT, buf2);
        free(buf2);
        buf2=NULL;
    }
    if (sm_buf1!=NULL)
    {
        /* reclaim any VL memory, if necessary */
        if(vl_data)
            H5Dvlen_reclaim(m_tid1, sm_space, H5P_DEFAULT, sm_buf1);
        free(sm_buf1);
        sm_buf1=NULL;
    }
    if (sm_buf2!=NULL)
    {
        /* reclaim any VL memory, if necessary */
        if(vl_data)
            H5Dvlen_reclaim(m_tid1, sm_space, H5P_DEFAULT, sm_buf2);
        free(sm_buf2);
        sm_buf2=NULL;
    }

    /* disable error reporting */
    H5E_BEGIN_TRY {
        H5Sclose(sid1);
        H5Sclose(sid2);
        H5Tclose(f_tid1);
        H5Tclose(f_tid2);
        H5Tclose(m_tid1);
        H5Tclose(m_tid2);
        /* enable error reporting */
    } H5E_END_TRY;

    return nfound;
}

/*-------------------------------------------------------------------------
* Function: diff_can_type
*
* Purpose: check for comparable TYPE and SPACE
*
* Return:
*  1, can compare
*  0, cannot compare
* -1, error
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: November 3, 2003
*
*-------------------------------------------------------------------------
*/

int diff_can_type( hid_t       f_tid1, /* file data type */
                   hid_t       f_tid2, /* file data type */
                   int         rank1,
                   int         rank2,
                   hsize_t     *dims1,
                   hsize_t     *dims2,
                   hsize_t     *maxdim1,
                   hsize_t     *maxdim2,
                   const char  *obj1_name,
                   const char  *obj2_name,
                   diff_opt_t  *options,
                   int         is_compound)
{


    H5T_class_t  tclass1;
    H5T_class_t  tclass2;
    int          maxdim_diff=0;          /* maximum dimensions are different */
    int          dim_diff=0;             /* current dimensions are different */
    int          i;
    int          can_compare = 1;        /* return value */

    /*-------------------------------------------------------------------------
    * check for the same class
    *-------------------------------------------------------------------------
    */

    if ((tclass1=H5Tget_class(f_tid1)) < 0)
        return -1;

    if ((tclass2=H5Tget_class(f_tid2)) < 0)
        return -1;

    if ( tclass1 != tclass2 )
    {

        if ( (options->m_verbose||options->m_list_not_cmp) && obj1_name && obj2_name)
        {

            if ( is_compound )
            {

                parallel_print("Not comparable: <%s> has a class %s and <%s> has a class %s\n",
                    obj1_name, get_class(tclass1),
                    obj2_name, get_class(tclass2) );

            }

            else

            {

                parallel_print("Not comparable: <%s> is of class %s and <%s> is of class %s\n",
                    obj1_name, get_class(tclass1),
                    obj2_name, get_class(tclass2) );

            }

        }


        can_compare = 0;
        options->not_cmp = 1;
        return can_compare;
    }

    /*-------------------------------------------------------------------------
    * check for non supported classes
    *-------------------------------------------------------------------------
    */

    assert(tclass1==tclass2);
    switch (tclass1)
    {
    case H5T_INTEGER:
    case H5T_FLOAT:
    case H5T_COMPOUND:
    case H5T_STRING:
    case H5T_ARRAY:
    case H5T_BITFIELD:
    case H5T_OPAQUE:
    case H5T_ENUM:
    case H5T_VLEN:
    case H5T_REFERENCE:

        break;

    default: /*H5T_TIME */


        if ( (options->m_verbose||options->m_list_not_cmp) && obj1_name && obj2_name)
        {
            parallel_print("Not comparable: <%s> and <%s> are of class %s\n",
                obj1_name,obj2_name,get_class(tclass2) );
        }
        can_compare = 0;
        options->not_cmp = 1;
        return can_compare;
    }

    /*-------------------------------------------------------------------------
    * check for equal file datatype; warning only
    *-------------------------------------------------------------------------
    */

    if ( (H5Tequal(f_tid1, f_tid2)==0) &&
        (options->m_verbose) && obj1_name && obj2_name)
    {

        H5T_class_t cl = H5Tget_class(f_tid1);


        parallel_print("Warning: different storage datatype\n");
        if ( cl == H5T_INTEGER || cl == H5T_FLOAT )
        {
            parallel_print("<%s> has file datatype ", obj1_name);
            print_type(f_tid1);
            parallel_print("\n");
            parallel_print("<%s> has file datatype ", obj2_name);
            print_type(f_tid2);
            parallel_print("\n");
        }



    }

    /*-------------------------------------------------------------------------
    * check for the same rank
    *-------------------------------------------------------------------------
    */


    if ( rank1 != rank2 )
    {

        if ( (options->m_verbose||options->m_list_not_cmp) && obj1_name && obj2_name)
        {
            parallel_print("Not comparable: <%s> has rank %d, dimensions ", obj1_name, rank1);
            print_dimensions(rank1,dims1);
            parallel_print(", max dimensions ");
            print_dimensions(rank1,maxdim1);
            parallel_print("\n" );
            parallel_print("and <%s> has rank %d, dimensions ", obj2_name, rank2);
            print_dimensions(rank2,dims2);
            parallel_print(", max dimensions ");
            print_dimensions(rank2,maxdim2);
            parallel_print("\n");
        }

        can_compare = 0;
        options->not_cmp = 1;
        return can_compare;
    }

    /*-------------------------------------------------------------------------
    * check for different dimensions
    *-------------------------------------------------------------------------
    */

    assert(rank1==rank2);
    for ( i=0; i<rank1; i++)
    {
        if (maxdim1 && maxdim2)
        {
            if ( maxdim1[i] != maxdim2[i] )
                maxdim_diff=1;
        }
        if ( dims1[i] != dims2[i] )
            dim_diff=1;
    }

    /*-------------------------------------------------------------------------
    * current dimensions
    *-------------------------------------------------------------------------
    */

    if (dim_diff==1)
    {
        if ( (options->m_verbose||options->m_list_not_cmp) && obj1_name && obj2_name)
        {
            parallel_print("Not comparable: <%s> has rank %d, dimensions ", obj1_name, rank1);
            print_dimensions(rank1,dims1);
            if (maxdim1 && maxdim2)
            {
                parallel_print(", max dimensions ");
                print_dimensions(rank1,maxdim1);
                parallel_print("\n" );
                parallel_print("and <%s> has rank %d, dimensions ", obj2_name, rank2);
                print_dimensions(rank2,dims2);
                parallel_print(", max dimensions ");
                print_dimensions(rank2,maxdim2);
                parallel_print("\n");
            }
        }


        can_compare = 0;
        options->not_cmp = 1;
        return can_compare;



    }

    /*-------------------------------------------------------------------------
    * maximum dimensions; just give a warning
    *-------------------------------------------------------------------------
    */
    if (maxdim1 && maxdim2 && maxdim_diff==1 && obj1_name )
    {
        if (options->m_verbose) {
            parallel_print( "Warning: different maximum dimensions\n");
            parallel_print("<%s> has max dimensions ", obj1_name);
            print_dimensions(rank1,maxdim1);
            parallel_print("\n");
            parallel_print("<%s> has max dimensions ", obj2_name);
            print_dimensions(rank2,maxdim2);
            parallel_print("\n");
        }
    }


    if ( tclass1 == H5T_COMPOUND )
    {

        int   nmembs1;
        int   nmembs2;
        int   j;
        hid_t memb_type1;
        hid_t memb_type2;

        nmembs1 = H5Tget_nmembers(f_tid1);
        nmembs2 = H5Tget_nmembers(f_tid2);

        if ( nmembs1 != nmembs2 )
        {

            if ( (options->m_verbose||options->m_list_not_cmp) && obj1_name && obj2_name)
            {
                parallel_print("Not comparable: <%s> has %d members ", obj1_name, nmembs1);
                parallel_print("<%s> has %d members ", obj2_name, nmembs2);
                parallel_print("\n");
            }

            can_compare = 0;
            options->not_cmp = 1;
            return can_compare;
        }

        for (j = 0; j < nmembs1; j++)
        {
            memb_type1 = H5Tget_member_type(f_tid1, (unsigned)j);
            memb_type2 = H5Tget_member_type(f_tid2, (unsigned)j);

            if (diff_can_type(memb_type1,
                memb_type2,
                rank1,
                rank2,
                dims1,
                dims2,
                maxdim1,
                maxdim2,
                obj1_name,
                obj2_name,
                options,
                1)!=1)
            {
                can_compare = 0;
                options->not_cmp = 1;
                H5Tclose(memb_type1);
                H5Tclose(memb_type2);
                return can_compare;
            }

            H5Tclose(memb_type1);
            H5Tclose(memb_type2);

        }





    }





    return can_compare;
}




/*-------------------------------------------------------------------------
* Function: print_sizes
*
* Purpose: Print datatype sizes
*
*-------------------------------------------------------------------------
*/
#if defined (H5DIFF_DEBUG)
void print_sizes( const char *obj1,
                  const char *obj2,
                  hid_t f_tid1,
                  hid_t f_tid2,
                  hid_t m_tid1,
                  hid_t m_tid2 )
{
    size_t  f_size1, f_size2;       /* size of type in file */
    size_t  m_size1, m_size2;       /* size of type in memory */

    f_size1 = H5Tget_size( f_tid1 );
    f_size2 = H5Tget_size( f_tid2 );
    m_size1 = H5Tget_size( m_tid1 );
    m_size2 = H5Tget_size( m_tid2 );

    parallel_print("\n");
    parallel_print("------------------\n");
    parallel_print("sizeof(char)   %u\n", sizeof(char) );
    parallel_print("sizeof(short)  %u\n", sizeof(short) );
    parallel_print("sizeof(int)    %u\n", sizeof(int) );
    parallel_print("sizeof(long)   %u\n", sizeof(long) );
    parallel_print("<%s> ------------------\n", obj1);
    parallel_print("type on file   ");
    print_type(f_tid1);
    parallel_print("\n");
    parallel_print("size on file   %u\n", f_size1 );

    parallel_print("type on memory ");
    print_type(m_tid1);
    parallel_print("\n");
    parallel_print("size on memory %u\n", m_size1 );

    parallel_print("<%s> ------------------\n", obj2);
    parallel_print("type on file   ");
    print_type(f_tid2);
    parallel_print("\n");
    parallel_print("size on file   %u\n", f_size2 );

    parallel_print("type on memory ");
    print_type(m_tid2);
    parallel_print("\n");
    parallel_print("size on memory %u\n", m_size2 );
    parallel_print("\n");
}
#endif /* H5DIFF_DEBUG */
