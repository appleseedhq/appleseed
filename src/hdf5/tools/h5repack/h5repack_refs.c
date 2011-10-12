
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

#include "h5repack.h"
#include "h5diff.h"
#include "h5tools.h"


/*-------------------------------------------------------------------------
 * local functions
 *-------------------------------------------------------------------------
 */

static const char* MapIdToName(hid_t refobj_id,trav_table_t *travt);
static int copy_refs_attr(hid_t loc_in, hid_t loc_out, pack_opt_t *options,
                          trav_table_t *travt, hid_t fidout);

/*-------------------------------------------------------------------------
 * Function: do_copy_refobjs
 *
 * Purpose: duplicate all referenced HDF5 objects in the file
 *  and create hard links
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December, 10, 2003
 *
 *-------------------------------------------------------------------------
 */

int do_copy_refobjs(hid_t fidin,
                    hid_t fidout,
                    trav_table_t *travt,
                    pack_opt_t *options) /* repack options */
{
    hid_t     grp_in = (-1);          /* read group ID */
    hid_t     grp_out = (-1);         /* write group ID */
    hid_t     dset_in = (-1);         /* read dataset ID */
    hid_t     dset_out = (-1);        /* write dataset ID */
    hid_t     type_in = (-1);         /* named type ID */
    hid_t     dcpl_id = (-1);         /* dataset creation property list ID */
    hid_t     space_id = (-1);        /* space ID */
    hid_t     ftype_id = (-1);        /* file data type ID */
    hid_t     mtype_id = (-1);        /* memory data type ID */
    size_t    msize;                  /* memory size of memory type */
    hsize_t   nelmts;                 /* number of elements in dataset */
    int       rank;                   /* rank of dataset */
    hsize_t   dims[H5S_MAX_RANK];     /* dimensions of dataset */
    unsigned int i, j;
    int       k;
    named_dt_t *named_dt_head=NULL;   /* Pointer to the stack of named datatypes
                                         copied */

    /*-------------------------------------------------------------------------
    * browse
    *-------------------------------------------------------------------------
    */
    for(i = 0; i < travt->nobjs; i++) {
        switch(travt->objs[i].type)
        {
            /*-------------------------------------------------------------------------
            * H5TRAV_TYPE_GROUP
            *-------------------------------------------------------------------------
            */
            case H5TRAV_TYPE_GROUP:
                /*-------------------------------------------------------------------------
                * copy referenced objects in attributes
                *-------------------------------------------------------------------------
                */
                if((grp_out = H5Gopen2(fidout, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    goto error;

                if((grp_in = H5Gopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    goto error;

                if(copy_refs_attr(grp_in, grp_out, options, travt, fidout) < 0)
                    goto error;

                if(H5Gclose(grp_out) < 0)
                    goto error;
                if(H5Gclose(grp_in) < 0)
                    goto error;

                /*-------------------------------------------------------------------------
                * check for hard links
                *-------------------------------------------------------------------------
                */
                if(travt->objs[i].nlinks)
                    for(j = 0; j < travt->objs[i].nlinks; j++)
                        H5Lcreate_hard(fidout, travt->objs[i].name, H5L_SAME_LOC, travt->objs[i].links[j].new_name, H5P_DEFAULT, H5P_DEFAULT);
                break;

            /*-------------------------------------------------------------------------
            * H5TRAV_TYPE_DATASET
            *-------------------------------------------------------------------------
            */
            case H5TRAV_TYPE_DATASET:
                if((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    goto error;
                if((space_id = H5Dget_space(dset_in)) < 0)
                    goto error;
                if((ftype_id = H5Dget_type(dset_in)) < 0)
                    goto error;
                if((dcpl_id = H5Dget_create_plist(dset_in)) < 0)
                    goto error;
                if((rank = H5Sget_simple_extent_ndims(space_id)) < 0)
                    goto error;
                if(H5Sget_simple_extent_dims(space_id, dims, NULL) < 0)
                    goto error;
                nelmts = 1;
                for(k = 0; k < rank; k++)
                    nelmts *= dims[k];

                if((mtype_id = h5tools_get_native_type(ftype_id)) < 0)
                    goto error;

                if((msize = H5Tget_size(mtype_id)) == 0)
                    goto error;

                /*-------------------------------------------------------------------------
                 * check if the dataset creation property list has filters that
                 * are not registered in the current configuration
                 * 1) the external filters GZIP and SZIP might not be available
                 * 2) the internal filters might be turned off
                 *-------------------------------------------------------------------------
                 */
                if(h5tools_canreadf(NULL, dcpl_id) == 1) {
                    /*-------------------------------------------------------------------------
                    * test for a valid output dataset
                    *-------------------------------------------------------------------------
                    */
                    dset_out = FAIL;

                    /*-------------------------------------------------------------------------
                    * object references are a special case
                    * we cannot just copy the buffers, but instead we recreate the reference
                    *-------------------------------------------------------------------------
                    */
                    if(H5Tequal(mtype_id, H5T_STD_REF_OBJ)) {
                        hid_t            refobj_id;
                        hobj_ref_t       *refbuf = NULL; /* buffer for object references */
                        hobj_ref_t       *buf = NULL;
                        const char*      refname;
                        unsigned         u;

                        /*-------------------------------------------------------------------------
                        * read to memory
                        *-------------------------------------------------------------------------
                        */
                        if(nelmts) {
                            buf = (hobj_ref_t *)HDmalloc((unsigned)(nelmts * msize));
                            if(buf==NULL) {
                                printf("cannot read into memory\n" );
                                goto error;
                            } /* end if */
                            if(H5Dread(dset_in, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                                goto error;

                            refbuf = (hobj_ref_t*) HDcalloc((unsigned)nelmts, msize);
                            if(refbuf == NULL){
                                printf("cannot allocate memory\n" );
                                goto error;
                            } /* end if */
                            for(u = 0; u < nelmts; u++) {
                                H5E_BEGIN_TRY {
                                    if((refobj_id = H5Rdereference(dset_in, H5R_OBJECT, &buf[u])) < 0)
                                        continue;
                                } H5E_END_TRY;

                                /* get the name. a valid name could only occur
                                 * in the second traversal of the file
                                 */
                                if((refname = MapIdToName(refobj_id, travt)) != NULL) {
                                    /* create the reference, -1 parameter for objects */
                                    if(H5Rcreate(&refbuf[u], fidout, refname, H5R_OBJECT, -1) < 0)
                                        goto error;
                                    if(options->verbose)
                                    {


                                        printf(FORMAT_OBJ,"dset",travt->objs[i].name );
                                        printf("object <%s> object reference created to <%s>\n",
                                            travt->objs[i].name,
                                            refname);
                                    }
                                } /*refname*/
                                H5Oclose(refobj_id);
                            } /* u */
                        } /*nelmts*/

                        /*-------------------------------------------------------------------------
                        * create/write dataset/close
                        *-------------------------------------------------------------------------
                        */
                        if((dset_out = H5Dcreate2(fidout, travt->objs[i].name, mtype_id, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
                            goto error;
                        if(nelmts)
                            if(H5Dwrite(dset_out, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, refbuf) < 0)
                                goto error;

                        if(buf)
                            HDfree(buf);
                        if(refbuf)
                            HDfree(refbuf);

                       /*------------------------------------------------------
                        * copy attrs
                        *----------------------------------------------------*/
                        if(copy_attr(dset_in, dset_out, &named_dt_head, travt, options) < 0)
                            goto error;
                    } /*H5T_STD_REF_OBJ*/

                    /*-------------------------------------------------------------------------
                    * dataset region references
                    *-------------------------------------------------------------------------
                    */
                    else if(H5Tequal(mtype_id, H5T_STD_REF_DSETREG)) 
                    {
                        hid_t            refobj_id;
                        hdset_reg_ref_t  *refbuf = NULL; /* input buffer for region references */
                        hdset_reg_ref_t  *buf = NULL;    /* output buffer */
                        const char*      refname;
                        unsigned         u;

                        /*-------------------------------------------------------------------------
                        * read input to memory
                        *-------------------------------------------------------------------------
                        */
                        if(nelmts) {
                            buf = (hdset_reg_ref_t *)HDmalloc((unsigned)(nelmts * msize));
                            if(buf == NULL) {
                                printf("cannot read into memory\n");
                                goto error;
                            } /* end if */
                            if(H5Dread(dset_in, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                                goto error;

                            /*-------------------------------------------------------------------------
                            * create output
                            *-------------------------------------------------------------------------
                            */
                            refbuf = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), (size_t)nelmts); /*init to zero */
                            if(refbuf == NULL) {
                                printf("cannot allocate memory\n");
                                goto error;
                            } /* end if */

                            for(u = 0; u < nelmts; u++) {
                                H5E_BEGIN_TRY {
                                    if((refobj_id = H5Rdereference(dset_in, H5R_DATASET_REGION, &buf[u])) < 0)
                                        continue;
                                } H5E_END_TRY;

                                /* get the name. a valid name could only occur
                                 * in the second traversal of the file
                                 */
                                if((refname = MapIdToName(refobj_id, travt)) != NULL) {
                                    hid_t region_id;    /* region id of the referenced dataset */

                                    if((region_id = H5Rget_region(dset_in, H5R_DATASET_REGION, &buf[u])) < 0)
                                        goto error;

                                    /* create the reference, we need the space_id */
                                    if(H5Rcreate(&refbuf[u], fidout, refname, H5R_DATASET_REGION, region_id) < 0)
                                        goto error;
                                    if(H5Sclose(region_id) < 0)
                                        goto error;
                                    if(options->verbose)
                                    {



                                        printf(FORMAT_OBJ,"dset",travt->objs[i].name );
                                        printf("object <%s> region reference created to <%s>\n",
                                            travt->objs[i].name,
                                            refname);
                                    }
                                } /*refname*/
                                H5Oclose(refobj_id);
                            } /* u */
                        } /*nelmts*/

                        /*-------------------------------------------------------------------------
                        * create/write dataset/close
                        *-------------------------------------------------------------------------
                        */
                        if((dset_out = H5Dcreate2(fidout, travt->objs[i].name, mtype_id, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
                            goto error;
                        if(nelmts)
                            if(H5Dwrite(dset_out, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, refbuf) < 0)
                                goto error;

                        if(buf)
                            HDfree(buf);
                        if(refbuf)
                            HDfree(refbuf);

                       /*-----------------------------------------------------
                        * copy attrs
                        *----------------------------------------------------*/
                        if(copy_attr(dset_in, dset_out, &named_dt_head, travt, options) < 0)
                            goto error;
                    } /* H5T_STD_REF_DSETREG */
                    /*-------------------------------------------------------------------------
                    * not references, open previously created object in 1st traversal
                    *-------------------------------------------------------------------------
                    */
                    else {
                        if((dset_out = H5Dopen2(fidout, travt->objs[i].name, H5P_DEFAULT)) < 0)
                            goto error;
                    } /* end else */

                    assert(dset_out != FAIL);

                    /*-------------------------------------------------------------------------
                    * copy referenced objects in attributes
                    *-------------------------------------------------------------------------
                    */
                    if(copy_refs_attr(dset_in, dset_out, options, travt, fidout) < 0)
                        goto error;

                    /*-------------------------------------------------------------------------
                    * check for hard links
                    *-------------------------------------------------------------------------
                    */
                    if(travt->objs[i].nlinks)
                        for(j = 0; j < travt->objs[i].nlinks; j++)
                            H5Lcreate_hard(fidout, travt->objs[i].name, H5L_SAME_LOC, travt->objs[i].links[j].new_name, H5P_DEFAULT, H5P_DEFAULT);

                    if(H5Dclose(dset_out) < 0)
                        goto error;
                } /*can_read*/

                /*-------------------------------------------------------------------------
                * close
                *-------------------------------------------------------------------------
                */
                if(H5Tclose(ftype_id) < 0)
                    goto error;
                if(H5Tclose(mtype_id) < 0)
                    goto error;
                if(H5Pclose(dcpl_id) < 0)
                    goto error;
                if(H5Sclose(space_id) < 0)
                    goto error;
                if(H5Dclose(dset_in) < 0)
                    goto error;
                break;

            /*-------------------------------------------------------------------------
            * H5TRAV_TYPE_NAMED_DATATYPE
            *-------------------------------------------------------------------------
            */
            case H5TRAV_TYPE_NAMED_DATATYPE:
                if((type_in = H5Topen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    goto error;
                if(H5Tclose(type_in) < 0)
                    goto error;
                break;

            /*-------------------------------------------------------------------------
            * H5TRAV_TYPE_LINK
            *-------------------------------------------------------------------------
            */
            case H5TRAV_TYPE_LINK:
                /*nothing to do */
                break;

            case H5TRAV_TYPE_UNKNOWN:
            case H5TRAV_TYPE_UDLINK:
                goto error;

            default:
                break;
        } /* end switch */
    } /* end for */

    /* Finalize (link) the stack of named datatypes (if any) 
     * This function is paired with copy_named_datatype() which is called
     * in copy_attr(), so need to free.
     */
    named_datatype_free(&named_dt_head, 0);

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(grp_in);
        H5Gclose(grp_out);
        H5Pclose(dcpl_id);
        H5Sclose(space_id);
        H5Dclose(dset_in);
        H5Dclose(dset_out);
        H5Tclose(ftype_id);
        H5Tclose(mtype_id);
        H5Tclose(type_in);
        named_datatype_free(&named_dt_head, 0);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function: copy_refs_attr
 *
 * Purpose: duplicate all referenced HDF5 located in attributes
 *  relative to LOC_IN, which is obtained either from
 * loc_id = H5Gopen2(fid, name, H5P_DEFAULT);
 * loc_id = H5Dopen2(fid, name, H5P_DEFAULT);
 * loc_id = H5Topen2(fid, name, H5P_DEFAULT);
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 28, 2003
 *
 *-------------------------------------------------------------------------
 */

static int copy_refs_attr(hid_t loc_in,
                          hid_t loc_out,
                          pack_opt_t *options,
                          trav_table_t *travt,
                          hid_t fidout         /* for saving references */
                          )
{
    hid_t      attr_id = -1;      /* attr ID */
    hid_t      attr_out = -1;     /* attr ID */
    hid_t      space_id = -1;     /* space ID */
    hid_t      ftype_id = -1;     /* file data type ID */
    hid_t      mtype_id = -1;     /* memory data type ID */
    size_t     msize;             /* memory size of type */
    hsize_t    nelmts;            /* number of elements in dataset */
    int        rank;              /* rank of dataset */
    hsize_t    dims[H5S_MAX_RANK];/* dimensions of dataset */
    char       name[255];
    H5O_info_t oinfo;           /* Object info */
    int        j;
    unsigned   u;

    if(H5Oget_info(loc_in, &oinfo) < 0)
        goto error;

    for(u = 0; u < (unsigned)oinfo.num_attrs; u++)
    {
        /*-------------------------------------------------------------------------
        * open
        *-------------------------------------------------------------------------
        */
        /* open attribute */
        if((attr_id = H5Aopen_by_idx(loc_in, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

        /* get name */
        if(H5Aget_name(attr_id, 255, name) < 0)
            goto error;

        /* get the file datatype  */
        if((ftype_id = H5Aget_type(attr_id)) < 0)
            goto error;

        /* get the dataspace handle  */
        if((space_id = H5Aget_space(attr_id)) < 0)
            goto error;

        /* get dimensions  */
        if((rank = H5Sget_simple_extent_dims(space_id, dims, NULL)) < 0)
            goto error;


        /*-------------------------------------------------------------------------
        * elements
        *-------------------------------------------------------------------------
        */
        nelmts = 1;
        for(j = 0; j < rank; j++)
            nelmts *= dims[j];

        if((mtype_id = h5tools_get_native_type(ftype_id)) < 0)
            goto error;

        if((msize = H5Tget_size(mtype_id)) == 0)
            goto error;


        /*-------------------------------------------------------------------------
        * object references are a special case
        * we cannot just copy the buffers, but instead we recreate the reference
        *-------------------------------------------------------------------------
        */
        if(H5Tequal(mtype_id, H5T_STD_REF_OBJ))
        {
            hid_t       refobj_id;
            hobj_ref_t  *refbuf = NULL;
            unsigned    k;
            const char* refname;
            hobj_ref_t  *buf = NULL;

            /*-------------------------------------------------------------------------
            * read input to memory
            *-------------------------------------------------------------------------
            */

            if (nelmts)
            {
                buf = (hobj_ref_t *)HDmalloc((unsigned)(nelmts * msize));
                if(buf == NULL)
                {
                    printf("cannot read into memory\n");
                    goto error;
                } /* end if */
                if(H5Aread(attr_id, mtype_id, buf) < 0)
                    goto error;

                refbuf = (hobj_ref_t *)HDcalloc((unsigned)nelmts, msize);
                if(refbuf == NULL)
                {
                    printf( "cannot allocate memory\n" );
                    goto error;
                } /* end if */

                for(k = 0; k < nelmts; k++)
                {
                    H5E_BEGIN_TRY
                    {
                        if((refobj_id = H5Rdereference(attr_id, H5R_OBJECT, &buf[k])) < 0)
                            goto error;
                    } H5E_END_TRY;

                    /* get the name. a valid name could only occur in the
                     * second traversal of the file
                     */
                    if((refname = MapIdToName(refobj_id, travt)) != NULL)
                    {
                        /* create the reference */
                        if(H5Rcreate(&refbuf[k], fidout, refname, H5R_OBJECT, -1) < 0)
                            goto error;
                        if(options->verbose)
                            printf("object <%s> reference created to <%s>\n", name, refname);
                    }
                    H5Oclose(refobj_id);
                } /* k */
            } /*nelmts*/

            /*-------------------------------------------------------------------------
            * copy
            *-------------------------------------------------------------------------
            */
            if((attr_out = H5Acreate2(loc_out, name, ftype_id, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                goto error;
            if(nelmts)
                if(H5Awrite(attr_out, mtype_id, refbuf) < 0)
                    goto error;

            if(H5Aclose(attr_out) < 0)
                goto error;

            if(refbuf)
                HDfree(refbuf);
            if(buf)
                HDfree(buf);
        }/*H5T_STD_REF_OBJ*/

        /*-------------------------------------------------------------------------
        * dataset region references
        *-------------------------------------------------------------------------
        */
        else if(H5Tequal(mtype_id, H5T_STD_REF_DSETREG))
        {
            hid_t            refobj_id;
            hdset_reg_ref_t  *refbuf = NULL; /* input buffer for region references */
            hdset_reg_ref_t  *buf = NULL;    /* output buffer */
            const char*      refname;
            unsigned         k;

            /*-------------------------------------------------------------------------
            * read input to memory
            *-------------------------------------------------------------------------
            */
            if(nelmts)
            {
                buf = (hdset_reg_ref_t *)HDmalloc((unsigned)(nelmts * msize));
                if(buf == NULL)
                {
                    printf( "cannot read into memory\n" );
                    goto error;
                } /* end if */
                if(H5Aread(attr_id, mtype_id, buf) < 0)
                    goto error;

                /*-------------------------------------------------------------------------
                * create output
                *-------------------------------------------------------------------------
                */
                refbuf = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), (size_t)nelmts); /*init to zero */
                if(refbuf == NULL)
                {
                    printf( "cannot allocate memory\n" );
                    goto error;
                } /* end if */

                for(k = 0; k < nelmts; k++)
                {
                    H5E_BEGIN_TRY
                    {
                        if((refobj_id = H5Rdereference(attr_id, H5R_DATASET_REGION, &buf[k])) < 0)
                            continue;
                    } H5E_END_TRY;

                    /* get the name. a valid name could only occur in the
                     * second traversal of the file
                     */
                    if((refname = MapIdToName(refobj_id, travt)) != NULL)
                    {
                        hid_t region_id;    /* region id of the referenced dataset */

                        if((region_id = H5Rget_region(attr_id, H5R_DATASET_REGION, &buf[k])) < 0)
                            goto error;

                        /* create the reference, we need the space_id */
                        if(H5Rcreate(&refbuf[k], fidout, refname, H5R_DATASET_REGION, region_id) < 0)
                            goto error;
                        if(H5Sclose(region_id) < 0)
                            goto error;
                        if(options->verbose)
                            printf("object <%s> region reference created to <%s>\n", name, refname);
                    } /* end if */
                    H5Oclose(refobj_id);
                } /* k */
            } /*nelmts */

            /*-------------------------------------------------------------------------
            * copy
            *-------------------------------------------------------------------------
            */
            if((attr_out = H5Acreate2(loc_out, name, ftype_id, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                goto error;
            if(nelmts)
            {
                if(H5Awrite(attr_out, mtype_id, refbuf) < 0)
                    goto error;
            }

            if(H5Aclose(attr_out) < 0)
                goto error;

            if(refbuf)
                HDfree(refbuf);
            if(buf)
                HDfree(buf);
        } /* H5T_STD_REF_DSETREG */

        /*-------------------------------------------------------------------------
        * close
        *-------------------------------------------------------------------------
        */
        if(H5Tclose(ftype_id) < 0)
            goto error;
        if(H5Tclose(mtype_id) < 0)
            goto error;
        if(H5Sclose(space_id) < 0)
            goto error;
        if(H5Aclose(attr_id) < 0)
            goto error;
    } /* u */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Tclose(ftype_id);
        H5Tclose(mtype_id);
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Aclose(attr_out);
    } H5E_END_TRY;

    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	MapIdToName
 *
 * Purpose:	map a ID from a reference to a dataset name
 *
 *-------------------------------------------------------------------------
 */
static const char*
MapIdToName(hid_t refobj_id, trav_table_t *travt)
{
    unsigned int u;
    const char* ret = NULL;

    /* linear search */
    for(u = 0; u < travt->nobjs; u++) {
        if(travt->objs[u].type == H5O_TYPE_DATASET || 
                travt->objs[u].type == H5O_TYPE_GROUP ||
                travt->objs[u].type == H5O_TYPE_NAMED_DATATYPE) {
            H5O_info_t   ref_oinfo;     /* Stat for the refobj id */

            /* obtain information to identify the referenced object uniquely */
            if(H5Oget_info(refobj_id, &ref_oinfo) < 0)
                goto out;

            if(ref_oinfo.addr == travt->objs[u].objno) {
                ret = travt->objs[u].name;
                goto out;
            } /* end if */
        }  /* end if */
    } /* u */

out:
    return ret;
}

