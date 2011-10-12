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

#define ATTR_NAME_MAX 255

typedef struct table_attr_t {
    char      *name;
    unsigned   exist[2];
} match_attr_t;

typedef struct table_attrs_t {
    size_t      size;
    size_t      nattrs;
    size_t      nattrs_only1;
    size_t      nattrs_only2;
    match_attr_t *attrs;
} table_attrs_t;


/*-------------------------------------------------------------------------
 * Function: table_attrs_init
 *
 * Purpose: Initialize the table
 *
 * Parameter:
 *  - tbl [OUT]
 *
 * Programmer: Jonathan Kim
 *
 * Date: March 15, 2011
 *------------------------------------------------------------------------*/
static void table_attrs_init(table_attrs_t **tbl)
{
    table_attrs_t* table_attrs = (table_attrs_t*) HDmalloc(sizeof(table_attrs_t));

    table_attrs->size = 0;
    table_attrs->nattrs = 0;
    table_attrs->nattrs_only1 = 0;
    table_attrs->nattrs_only2 = 0;
    table_attrs->attrs = NULL;

    *tbl = table_attrs;
}

/*-------------------------------------------------------------------------
 * Function: table_attrs_free
 *
 * Purpose: free given table
 *
 * Parameter:
 *  - table [IN]
 *
 * Programmer: Jonathan Kim
 *
 * Date: March 15, 2011
 *------------------------------------------------------------------------*/
static void table_attrs_free( table_attrs_t *table )
{
    unsigned int i;

    if (table)
    {
        if(table->attrs) 
        {
            for(i = 0; i < table->nattrs; i++) 
            {
                if(table->attrs[i].name)
                    HDfree(table->attrs[i].name );
            } /* end for */
            HDfree(table->attrs);
            table->attrs = NULL;
        } /* end if */
        HDfree(table);
        table = NULL;
    }
}

/*-------------------------------------------------------------------------
 * Function: table_attr_mark_exist
 *
 * Purpose: mark given attribute name to table as sign of exsit
 *
 * Parameter:
 *  - exist [IN]
 *  - name [IN]  : attribute name
 *  - table [OUT]
 *
 * Programmer: Jonathan Kim
 *
 * Date: March 15, 2011
 *------------------------------------------------------------------------*/
static void table_attr_mark_exist(unsigned *exist, char *name, table_attrs_t *table)
{
    unsigned int new;

    if(table->nattrs == table->size) {
        table->size = MAX(1, table->size * 2);
        table->attrs = (match_attr_t *)HDrealloc(table->attrs, table->size * sizeof(match_attr_t));
    } /* end if */

    new = table->nattrs++;
    table->attrs[new].exist[0] = exist[0];
    table->attrs[new].exist[1] = exist[1];
    table->attrs[new].name = (char *)HDstrdup(name);
}

/*-------------------------------------------------------------------------
 * Function: build_match_list_attrs
 *
 * Purpose: get list of matching attribute name from obj1 and obj2
 *
 * Note:
 *  Find common attribute; the algorithm for search is referred from 
 *  build_match_list() in h5diff.c .
 *
 * Parameter:
 *  table_out [OUT] : return the list
 *
 * Programmer: Jonathan Kim
 *
 * Date: March 15, 2011
 *------------------------------------------------------------------------*/
static herr_t build_match_list_attrs(hid_t loc1_id, hid_t loc2_id, table_attrs_t ** table_out,  diff_opt_t *options)
{
    H5O_info_t oinfo1, oinfo2;  /* Object info */
    hid_t      attr1_id=-1;     /* attr ID */
    hid_t      attr2_id=-1;     /* attr ID */
    size_t curr1 = 0;
    size_t curr2 = 0;
    unsigned infile[2];
    char  name1[ATTR_NAME_MAX];
    char  name2[ATTR_NAME_MAX];
    int cmp;
    unsigned i;
    table_attrs_t *table_lp = NULL;

    if(H5Oget_info(loc1_id, &oinfo1) < 0)
        goto error;
    if(H5Oget_info(loc2_id, &oinfo2) < 0)
        goto error;

    table_attrs_init( &table_lp );

    
   /*--------------------------------------------------
    * build the list
    */
    while(curr1 < oinfo1.num_attrs && curr2 < oinfo2.num_attrs)
    {
        /*------------------ 
         * open attribute1 */
        if((attr1_id = H5Aopen_by_idx(loc1_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        /* get name */
        if(H5Aget_name(attr1_id, ATTR_NAME_MAX, name1) < 0)
            goto error;

        /*------------------ 
         * open attribute2 */
        if((attr2_id = H5Aopen_by_idx(loc2_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr2, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        /* get name */
        if(H5Aget_name(attr2_id, ATTR_NAME_MAX, name2) < 0)
            goto error;

        /* criteria is string compare */
        cmp = HDstrcmp(name1, name2);

        if(cmp == 0) 
        {
            infile[0] = 1;
            infile[1] = 1;
            table_attr_mark_exist(infile, name1, table_lp);
            curr1++;
            curr2++;
        }
        else if(cmp < 0)
        {
            infile[0] = 1;
            infile[1] = 0;
            table_attr_mark_exist(infile, name1, table_lp);
            table_lp->nattrs_only1++;
            curr1++;
        }
        else
        {
            infile[0] = 0;
            infile[1] = 1;
            table_attr_mark_exist(infile, name2, table_lp);
            table_lp->nattrs_only2++;
            curr2++;
        }
    } /* end while */

    /* list1 did not end */
    infile[0] = 1;
    infile[1] = 0;
    while(curr1 < oinfo1.num_attrs)
    {
        /*------------------ 
         * open attribute1 */
        if((attr1_id = H5Aopen_by_idx(loc1_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        /* get name */
        if(H5Aget_name(attr1_id, ATTR_NAME_MAX, name1) < 0)
            goto error;

        table_attr_mark_exist(infile, name1, table_lp);
        table_lp->nattrs_only1++;
        curr1++;
    }

    /* list2 did not end */
    infile[0] = 0;
    infile[1] = 1;
    while(curr2 < oinfo2.num_attrs)
    {
        /*------------------ 
         * open attribute2 */
        if((attr2_id = H5Aopen_by_idx(loc2_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr2, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        /* get name */
        if(H5Aget_name(attr2_id, ATTR_NAME_MAX, name2) < 0)
            goto error;

        table_attr_mark_exist(infile, name2, table_lp);
        table_lp->nattrs_only2++;
        curr2++;
    }

    /*------------------------------------------------------
    * print the list
    */
    if(options->m_verbose_level == 2)
    {
        /* if '-v2' is detected */
        parallel_print("   obj1   obj2\n");
        parallel_print(" --------------------------------------\n");
        for(i = 0; i < (unsigned int) table_lp->nattrs; i++) 
        {
            char c1, c2;
            c1 = (table_lp->attrs[i].exist[0]) ? 'x' : ' ';
            c2 = (table_lp->attrs[i].exist[1]) ? 'x' : ' ';
            parallel_print("%5c %6c    %-15s\n", c1, c2, table_lp->attrs[i].name);
        } /* end for */
    }

    if(options->m_verbose_level >= 1)
    {
        parallel_print("Attributes status:  %d common, %d only in obj1, %d only in obj2\n", table_lp->nattrs - table_lp->nattrs_only1 - table_lp->nattrs_only2, table_lp->nattrs_only1, table_lp->nattrs_only2 );
    }

    *table_out = table_lp;

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function: diff_attr
 *
 * Purpose: compare attributes located in LOC1_ID and LOC2_ID, which are
 *  obtained either from
 * loc_id = H5Gopen2(fid, name, H5P_DEFAULT);
 * loc_id = H5Dopen2(fid, name);
 * loc_id = H5Topen2(fid, name, H5P_DEFAULT);
 *
 * Return: number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November, 03, 2003
 *
 * Modifications:
 *  March, 02, 2007: return the number of differences found
 *
 *-------------------------------------------------------------------------
 */

hsize_t diff_attr(hid_t loc1_id,
                  hid_t loc2_id,
                  const char *path1,
                  const char *path2,
                  diff_opt_t *options)
{
    hid_t      attr1_id=-1;     /* attr ID */
    hid_t      attr2_id=-1;     /* attr ID */
    hid_t      space1_id=-1;    /* space ID */
    hid_t      space2_id=-1;    /* space ID */
    hid_t      ftype1_id=-1;    /* file data type ID */
    hid_t      ftype2_id=-1;    /* file data type ID */
    hid_t      mtype1_id=-1;    /* memory data type ID */
    hid_t      mtype2_id=-1;    /* memory data type ID */
    size_t     msize1;          /* memory size of memory type */
    size_t     msize2;          /* memory size of memory type */
    void       *buf1=NULL;      /* data buffer */
    void       *buf2=NULL;      /* data buffer */
    hsize_t    nelmts1;         /* number of elements in dataset */
    int        rank1;           /* rank of dataset */
    int        rank2;           /* rank of dataset */
    hsize_t    dims1[H5S_MAX_RANK];/* dimensions of dataset */
    hsize_t    dims2[H5S_MAX_RANK];/* dimensions of dataset */
    char       *name1;
    char       *name2;
    char       np1[512];
    char       np2[512];
    unsigned   u;                  /* Local index variable */
    hsize_t    nfound = 0;
    hsize_t    nfound_total = 0;
    int       j;

    table_attrs_t * match_list_attrs = NULL;
    if( build_match_list_attrs(loc1_id, loc2_id, &match_list_attrs, options) < 0)
        goto error;

    for(u = 0; u < (unsigned)match_list_attrs->nattrs; u++)
    {
        if( (match_list_attrs->attrs[u].exist[0]) && (match_list_attrs->attrs[u].exist[1]) )
        {
        name1 = name2 = match_list_attrs->attrs[u].name;

       /*--------------
        * attribute 1 */
        if((attr1_id = H5Aopen(loc1_id, name1, H5P_DEFAULT)) < 0)
            goto error;

       /*--------------
        * attribute 2 */
        if((attr2_id = H5Aopen(loc2_id, name2, H5P_DEFAULT)) < 0)
            goto error;

        /* get the datatypes  */
        if((ftype1_id = H5Aget_type(attr1_id)) < 0)
            goto error;
        if((ftype2_id = H5Aget_type(attr2_id)) < 0)
            goto error;
        if((mtype1_id = h5tools_get_native_type(ftype1_id))<0)
            goto error;
        if((mtype2_id = h5tools_get_native_type(ftype2_id))<0)
            goto error;
        if((msize1 = H5Tget_size(mtype1_id))==0)
            goto error;
        if((msize2 = H5Tget_size(mtype2_id))==0)
            goto error;

        /* get the dataspace   */
        if((space1_id = H5Aget_space(attr1_id)) < 0)
            goto error;
        if((space2_id = H5Aget_space(attr2_id)) < 0)
            goto error;

        /* get dimensions  */
        if((rank1 = H5Sget_simple_extent_dims(space1_id, dims1, NULL)) < 0)
            goto error;
        if((rank2 = H5Sget_simple_extent_dims(space2_id, dims2, NULL)) < 0)
            goto error;


       /*----------------------------------------------------------------------
        * check for comparable TYPE and SPACE
        *----------------------------------------------------------------------
        */

        if(msize1 != msize2 ||
                diff_can_type(ftype1_id, ftype2_id, rank1, rank2, dims1,
                    dims2, NULL, NULL, name1, name2, options, 0) != 1)
        {
            if(H5Tclose(ftype1_id) < 0)
                goto error;
            if(H5Tclose(ftype2_id) < 0)
                goto error;
            if(H5Sclose(space1_id) < 0)
                goto error;
            if(H5Sclose(space2_id) < 0)
                goto error;
            if(H5Aclose(attr1_id) < 0)
                goto error;
            if(H5Aclose(attr2_id) < 0)
                goto error;
            if(H5Tclose(mtype1_id) < 0)
                goto error;
            if(H5Tclose(mtype2_id) < 0)
                goto error;

            continue;
        }


        /*---------------------------------------------------------------------
        * read
        *----------------------------------------------------------------------
        */
        nelmts1 = 1;
        for(j = 0; j < rank1; j++)
            nelmts1 *= dims1[j];

        buf1 = (void *)HDmalloc((unsigned)(nelmts1 * msize1));
        buf2 = (void *)HDmalloc((unsigned)(nelmts1 * msize2));
        if(buf1 == NULL || buf2 == NULL) {
            parallel_print( "cannot read into memory\n" );
            goto error;
        }
        if(H5Aread(attr1_id,mtype1_id,buf1) < 0)
            goto error;
        if(H5Aread(attr2_id,mtype2_id,buf2) < 0)
            goto error;

        /* format output string */
        sprintf(np1,"%s of <%s>",name1,path1);
        sprintf(np2,"%s of <%s>",name2,path2);

        /*---------------------------------------------------------------------
        * array compare
        *----------------------------------------------------------------------
        */

        /* always print name */
        /* verbose (-v) and report (-r) mode */
        if(options->m_verbose || options->m_report) {
            do_print_attrname("attribute", np1, np2);

            nfound = diff_array(buf1, buf2, nelmts1, (hsize_t)0, rank1, dims1,
                options, np1, np2, mtype1_id, attr1_id, attr2_id);
            print_found(nfound);
        }
        /* quiet mode (-q), just count differences */
        else if(options->m_quiet) {
            nfound = diff_array(buf1, buf2, nelmts1, (hsize_t)0, rank1, dims1,
                options, np1, np2, mtype1_id, attr1_id, attr2_id);
        }
        /* the rest (-c, none, ...) */
        else {
            nfound = diff_array(buf1, buf2, nelmts1, (hsize_t)0, rank1, dims1,
                options, np1, np2, mtype1_id, attr1_id, attr2_id);

                /* not comparable, no display the different number */
                if(!options->not_cmp && nfound) {
                    do_print_attrname("attribute", np1, np2);
                    print_found(nfound);
                } /* end if */
        } /* end else */


       /*----------------------------------------------------------------------
        * close
        *----------------------------------------------------------------------
        */

        /* Free buf1 and buf2, check both VLEN-data VLEN-string to reclaim any 
         * VLEN memory first */
        if(TRUE == h5tools_detect_vlen(mtype1_id))
            H5Dvlen_reclaim(mtype1_id, space1_id, H5P_DEFAULT, buf1);
        HDfree(buf1);

        buf1 = NULL;
        if(TRUE == h5tools_detect_vlen(mtype2_id))
            H5Dvlen_reclaim(mtype2_id, space2_id, H5P_DEFAULT, buf2);
        HDfree(buf2);
        buf2 = NULL;

        if(H5Tclose(ftype1_id) < 0)
            goto error;
        if(H5Tclose(ftype2_id) < 0)
            goto error;
        if(H5Sclose(space1_id) < 0)
            goto error;
        if(H5Sclose(space2_id) < 0)
            goto error;
        if(H5Aclose(attr1_id) < 0)
            goto error;
        if(H5Aclose(attr2_id) < 0)
            goto error;
        if(H5Tclose(mtype1_id) < 0)
            goto error;
        if(H5Tclose(mtype2_id) < 0)
            goto error;

        nfound_total += nfound;
        }
    } /* u */

    table_attrs_free(match_list_attrs);

    return nfound_total;

error:
    H5E_BEGIN_TRY {
        if(buf1) {
            if(TRUE == h5tools_detect_vlen(mtype1_id))
                H5Dvlen_reclaim(mtype1_id, space1_id, H5P_DEFAULT, buf1);
            HDfree(buf1);
        } /* end if */
        if(buf2) {
            if(TRUE == h5tools_detect_vlen(mtype2_id))
                H5Dvlen_reclaim(mtype2_id, space2_id, H5P_DEFAULT, buf2);
            HDfree(buf2);
        } /* end if */

        table_attrs_free(match_list_attrs);

        H5Tclose(ftype1_id);
        H5Tclose(ftype2_id);
        H5Tclose(mtype1_id);
        H5Tclose(mtype2_id);
        H5Sclose(space1_id);
        H5Sclose(space2_id);
        H5Aclose(attr1_id);
        H5Aclose(attr2_id);
    } H5E_END_TRY;

    options->err_stat = 1;
    return nfound_total;
}

