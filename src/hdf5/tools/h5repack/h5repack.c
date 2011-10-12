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

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "H5private.h"
#include "h5repack.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/*-------------------------------------------------------------------------
* File: h5repack.c
* Purpose: Public API functions
*-------------------------------------------------------------------------
*/

static int check_options(pack_opt_t *options);
static int check_objects(const char* fname, pack_opt_t *options);
static const char* get_sfilter (H5Z_filter_t filtn);
static int have_request(pack_opt_t *options);



/*-------------------------------------------------------------------------
* Function: h5repack
*
* Purpose: locate all high-level HDF5 objects in the file
*  and compress/chunk them using options
*
* Algorithm: 2 traversals are made to the file; the 1st builds a list of
*  the objects, the 2nd makes a copy of them, using the options;
*  the reason for the 1st traversal is to check for invalid
*  object name requests
*
* Return: 0, ok, -1, fail
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September, 22, 2003
*
*-------------------------------------------------------------------------
*/
int h5repack(const char* infile,
             const char* outfile,
             pack_opt_t *options)
{
    /* check input */
    if (check_options(options)<0)
        return -1;

    /* check for objects in input that are in the file */
    if (check_objects(infile,options) < 0)
        return -1;

    /* copy the objects  */
    if (copy_objects(infile,outfile,options) < 0)
        return -1;


    return 0;
}



/*-------------------------------------------------------------------------
* Function: h5repack_init
*
* Purpose: initialize options
*
* Return: 0, ok, -1, fail
*
*-------------------------------------------------------------------------
*/

int h5repack_init (pack_opt_t *options,
                   int verbose)
{
    int k, n;
    memset(options,0,sizeof(pack_opt_t));
    options->min_comp = 1024;
    options->verbose  = verbose;

    for ( n = 0; n < H5_REPACK_MAX_NFILTERS; n++)
    {
        options->filter_g[n].filtn  = -1;
        options->filter_g[n].cd_nelmts  = 0;
        for ( k = 0; k < CD_VALUES; k++)
            options->filter_g[n].cd_values[k] = 0;
    }

    return (options_table_init(&(options->op_tbl)));
}


/*-------------------------------------------------------------------------
* Function: h5repack_end
*
* Purpose: free options table
*
*-------------------------------------------------------------------------
*/

int h5repack_end  (pack_opt_t *options)
{
    return options_table_free(options->op_tbl);
}


/*-------------------------------------------------------------------------
* Function: h5repack_addfilter
*
* Purpose: add a compression -f option to table
*   Example: -f dset:GZIP=6
*
* Return: 0, ok, -1, fail
*
*-------------------------------------------------------------------------
*/
int h5repack_addfilter(const char* str,
                       pack_opt_t *options)
{
    obj_list_t      *obj_list=NULL; /* one object list for the -f and -l option entry */
    filter_info_t   filter;         /* filter info for the current -f option entry */
    int             n_objs;         /* number of objects in the current -f or -l option entry */
    int             is_glb;         /* is the filter global */



    /* parse the -f option */
    if(NULL == (obj_list = parse_filter(str, &n_objs, &filter, options, &is_glb)))
        return -1;

    /* if it applies to all objects */
    if(is_glb)
    {
        int n;

        n = options->n_filter_g++; /* increase # of global filters */

        if(options->n_filter_g > H5_REPACK_MAX_NFILTERS)
        {
            error_msg("maximum number of filters exceeded for <%s>\n", str);
            free(obj_list);
            return -1;
        }

        options->filter_g[n] = filter;
    }
    else
        options_add_filter(obj_list, n_objs, filter, options->op_tbl);

    free(obj_list);
    return 0;
}


/*-------------------------------------------------------------------------
* Function: h5repack_addlayout
*
* Purpose: add a layout option
*
* Return: 0, ok, -1, fail
*
*-------------------------------------------------------------------------
*/


int h5repack_addlayout(const char* str,
                       pack_opt_t *options)
{

    obj_list_t  *obj_list=NULL;     /*one object list for the -t and -c option entry */
    int         n_objs;             /*number of objects in the current -t or -c option entry */
    pack_info_t pack;               /*info about layout to extract from parse */
    int         j;

    init_packobject(&pack);

    if (options->all_layout==1){
        error_msg("invalid layout input: 'all' option \
                            is present with other objects <%s>\n",str);
        return -1;
    }

    /* parse the layout option */
    obj_list=parse_layout(str,&n_objs,&pack,options);
    if (obj_list==NULL)
        return -1;

    /* set global layout option */
    if (options->all_layout==1 )
    {
        options->layout_g=pack.layout;
        if (pack.layout==H5D_CHUNKED)
        {
            /* -2 means the NONE option, remove chunking
            and set the global layout to contiguous */
            if (pack.chunk.rank==-2)
            {
                options->layout_g = H5D_CONTIGUOUS;
            }
            /* otherwise set the global chunking type */
            else
            {
                options->chunk_g.rank=pack.chunk.rank;
                for (j = 0; j < pack.chunk.rank; j++)
                    options->chunk_g.chunk_lengths[j] = pack.chunk.chunk_lengths[j];
            }
        }
    }

    if (options->all_layout==0)
        options_add_layout(obj_list,
        n_objs,
        &pack,
        options->op_tbl);

    free(obj_list);
    return 0;
}

/* Note: The below copy_named_datatype(), named_datatype_free(), copy_attr() 
 * were located in h5repack_copy.c as static prior to bugfix1726. 
 * Made shared functions as copy_attr() was needed in h5repack_refs.c. 
 * However copy_attr() may be obsoleted when H5Acopy is available and put back
 * others to static in h5repack_copy.c.
 */
/*-------------------------------------------------------------------------
* Function: copy_named_datatype
*
* Purpose: Copies the specified datatype anonymously, and returns an open
*          id for that datatype in the output file.  The first time this
*          is called it scans every named datatype in travt into a
*          private stack, afterwards it simply scans that stack.  The id
*          returned must be closed after it is no longer needed.
*          named_datatype_free must be called before the program exits
*          to free the stack.
*
* Programmer: Neil Fortner
*
* Date: April 14, 2009
*
*-------------------------------------------------------------------------
*/
hid_t copy_named_datatype(hid_t type_in, hid_t fidout, named_dt_t **named_dt_head_p, trav_table_t *travt, pack_opt_t *options)
{
    named_dt_t  *dt = *named_dt_head_p; /* Stack pointer */
    named_dt_t  *dt_ret = NULL;     /* Datatype to return */
    H5O_info_t  oinfo;              /* Object info of input dtype */
    hid_t       ret_value = -1;     /* The identifier of the named dtype in the out file */

    if(H5Oget_info(type_in, &oinfo) < 0)
        goto error;

    if(*named_dt_head_p) 
    {
        /* Stack already exists, search for the datatype */
        while(dt && dt->addr_in != oinfo.addr)
            dt = dt->next;

        dt_ret = dt;
    } 
    else 
    {
        /* Create the stack */
        size_t  i;

        for(i=0; i<travt->nobjs; i++)
            if(travt->objs[i].type == H5TRAV_TYPE_NAMED_DATATYPE) 
            {
                /* Push onto the stack */
                if(NULL == (dt = (named_dt_t *) HDmalloc(sizeof(named_dt_t))))
                    goto error;
                dt->next = *named_dt_head_p;
                *named_dt_head_p = dt;

                /* Update the address and id */
                dt->addr_in = travt->objs[i].objno;
                dt->id_out = -1;

                /* Check if this type is the one requested */
                if(oinfo.addr == dt->addr_in) 
                {
                    HDassert(!dt_ret);
                    dt_ret = dt;
                } /* end if */
            } /* end if */
    } /* end else */

    /* Handle the case that the requested datatype was not found.  This is
     * possible if the datatype was committed anonymously in the input file. */
    if(!dt_ret) 
    {
        /* Push the new datatype onto the stack */
        if(NULL == (dt_ret = (named_dt_t *) HDmalloc(sizeof(named_dt_t))))
            goto error;
        dt_ret->next = *named_dt_head_p;
        *named_dt_head_p = dt_ret;

        /* Update the address and id */
        dt_ret->addr_in = oinfo.addr;
        dt_ret->id_out = -1;
    } /* end if */

    /* If the requested datatype does not yet exist in the output file, copy it
     * anonymously */
    if(dt_ret->id_out < 0) 
    {
        if (options->use_native==1)
            dt_ret->id_out = h5tools_get_native_type(type_in);
        else
            dt_ret->id_out = H5Tcopy(type_in);
        if(dt_ret->id_out < 0)
            goto error;
        if(H5Tcommit_anon(fidout, dt_ret->id_out, H5P_DEFAULT, H5P_DEFAULT) < 0)
            goto error;
    } /* end if */

    /* Set return value */
    ret_value = dt_ret->id_out;

    /* Increment the ref count on id_out, because the calling function will try
     * to close it */
    if(H5Iinc_ref(ret_value) < 0)
        goto error;

    return(ret_value);

error:
    return(-1);
} /* end copy_named_datatype */


/*-------------------------------------------------------------------------
* Function: named_datatype_free
*
* Purpose: Frees the stack of named datatypes.
*
* Programmer: Neil Fortner
*
* Date: April 14, 2009
*
*-------------------------------------------------------------------------
*/
int named_datatype_free(named_dt_t **named_dt_head_p, int ignore_err)
{
    named_dt_t *dt = *named_dt_head_p;

    while(dt) 
    {
        /* Pop the datatype off the stack and free it */
        if(H5Tclose(dt->id_out) < 0 && !ignore_err)
            goto error;
        dt = dt->next;
        HDfree(*named_dt_head_p);
        *named_dt_head_p = dt;
    } /* end while */

    return 0;

error:
    return -1;
} /* end named_datatype_free */

/*-------------------------------------------------------------------------
* Function: copy_attr
*
* Purpose: copy attributes located in LOC_IN, which is obtained either from
* loc_id = H5Gopen2( fid, name);
* loc_id = H5Dopen2( fid, name);
* loc_id = H5Topen2( fid, name);
*
* Return: 0, ok, -1 no
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: October, 28, 2003
*
*-------------------------------------------------------------------------
*/
int copy_attr(hid_t loc_in,
              hid_t loc_out,
              named_dt_t **named_dt_head_p,
              trav_table_t *travt,
              pack_opt_t *options)
{
    hid_t      attr_id=-1;        /* attr ID */
    hid_t      attr_out=-1;       /* attr ID */
    hid_t      space_id=-1;       /* space ID */
    hid_t      ftype_id=-1;       /* file type ID */
    hid_t      wtype_id=-1;       /* read/write type ID */
    size_t     msize;             /* size of type */
    void       *buf = NULL;       /* data buffer */
    hsize_t    nelmts;            /* number of elements in dataset */
    int        rank;              /* rank of dataset */
    htri_t     is_named;          /* Whether the datatype is named */
    hsize_t    dims[H5S_MAX_RANK];/* dimensions of dataset */
    char       name[255];
    H5O_info_t oinfo;             /* object info */
    int        j;
    unsigned   u;

    if(H5Oget_info(loc_in, &oinfo) < 0)
        goto error;

    /*-------------------------------------------------------------------------
    * copy all attributes
    *-------------------------------------------------------------------------
    */
    for(u = 0; u < (unsigned)oinfo.num_attrs; u++) {
        /* open attribute */
        if((attr_id = H5Aopen_by_idx(loc_in, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

        /* get name */
        if(H5Aget_name(attr_id, (size_t)255, name) < 0)
            goto error;

        /* get the file datatype  */
        if((ftype_id = H5Aget_type(attr_id)) < 0 )
            goto error;

        /* Check if the datatype is committed */
        if((is_named = H5Tcommitted(ftype_id)) < 0)
            goto error;
        if(is_named && travt) {
            hid_t fidout;

            /* Create out file id */
            if((fidout = H5Iget_file_id(loc_out)) < 0)
                goto error;

            /* Copy named dt */
            if((wtype_id = copy_named_datatype(ftype_id, fidout, named_dt_head_p,
                    travt, options)) < 0) 
            {
                H5Fclose(fidout);
                goto error;
            } /* end if */

            if(H5Fclose(fidout) < 0)
                goto error;
        } /* end if */
        else {
            if(options->use_native == 1)
                wtype_id = h5tools_get_native_type(ftype_id);
            else
                wtype_id = H5Tcopy(ftype_id);
        } /* end else */

        /* get the dataspace handle  */
        if((space_id = H5Aget_space(attr_id)) < 0)
            goto error;

        /* get dimensions  */
        if((rank = H5Sget_simple_extent_dims(space_id, dims, NULL)) < 0)
            goto error;

        nelmts = 1;
        for(j = 0; j < rank; j++)
            nelmts *= dims[j];

        if((msize = H5Tget_size(wtype_id)) == 0)
            goto error;

        /*-------------------------------------------------------------------------
        * object references are a special case
        * we cannot just copy the buffers, but instead we recreate the reference
        * this is done on a second sweep of the file that just copies
        * the referenced objects
        *-------------------------------------------------------------------------
        */

        if(H5T_REFERENCE == H5Tget_class(wtype_id)) {
            ;
        }
        else {
            /*-------------------------------------------------------------------------
            * read to memory
            *-------------------------------------------------------------------------
            */

            buf = (void *)HDmalloc((size_t)(nelmts * msize));
            if(buf == NULL) {
                error_msg("h5repack", "cannot read into memory\n" );
                goto error;
            } /* end if */
            if(H5Aread(attr_id, wtype_id, buf) < 0)
                goto error;

            /*-------------------------------------------------------------------------
            * copy
            *-------------------------------------------------------------------------
            */

            if((attr_out = H5Acreate2(loc_out, name, wtype_id, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                goto error;
            if(H5Awrite(attr_out, wtype_id, buf) < 0)
                goto error;

            /*close*/
            if(H5Aclose(attr_out) < 0)
                goto error;

            /* Check if we have VL data in the attribute's  datatype that must
             * be reclaimed */
            if(TRUE == H5Tdetect_class(wtype_id, H5T_VLEN))
                H5Dvlen_reclaim(wtype_id, space_id, H5P_DEFAULT, buf);
            HDfree(buf);
            buf = NULL;
        } /*H5T_REFERENCE*/


        if(options->verbose)
            printf(FORMAT_OBJ_ATTR, "attr", name);

        /*-------------------------------------------------------------------------
        * close
        *-------------------------------------------------------------------------
        */

        if(H5Tclose(ftype_id) < 0)
            goto error;
        if(H5Tclose(wtype_id) < 0)
            goto error;
        if(H5Sclose(space_id) < 0)
            goto error;
        if(H5Aclose(attr_id) < 0)
            goto error;
    } /* u */

    return 0;

error:
    H5E_BEGIN_TRY {
        if(buf) {
            /* Check if we have VL data in the attribute's  datatype that must
             * be reclaimed */
            if(TRUE == H5Tdetect_class(wtype_id, H5T_VLEN))
                H5Dvlen_reclaim(wtype_id, space_id, H5P_DEFAULT, buf);

            /* Free buf */
            free(buf);
        } /* end if */

        H5Tclose(ftype_id);
        H5Tclose(wtype_id);
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Aclose(attr_out);
    } H5E_END_TRY;

    return -1;
} /* end copy_attr() */

/*-------------------------------------------------------------------------
* Function: check_options
*
* Purpose: print options, checks for invalid options
*
* Return: void, return -1 on error
*
* Programmer: pvn@ncsa.uiuc.edu
*
* Date: September, 22, 2003
*
* Modification:
*   Peter Cao, July 9, 2007
*   Add "-L, --latest" and other options to pack a file with the latest file format
*
*-------------------------------------------------------------------------
*/
static int check_options(pack_opt_t *options)
{
    unsigned int   i;
    int            k, j, has_cp=0, has_ck=0;
    char           slayout[30];

    /*-------------------------------------------------------------------------
    * objects to layout
    *-------------------------------------------------------------------------
    */
    if (options->verbose && have_request(options) /* only print if requested */)
    {
        printf("Objects to modify layout are...\n");
        if (options->all_layout==1)
        {
            switch (options->layout_g)
            {
            case H5D_COMPACT:
                strcpy(slayout,"compact");
                break;
            case H5D_CONTIGUOUS:
                strcpy(slayout,"contiguous");
                break;
            case H5D_CHUNKED:
                strcpy(slayout,"chunked");
                break;
            case H5D_LAYOUT_ERROR:
            case H5D_NLAYOUTS:
                error_msg("invalid layout\n");
                return -1;
            default:
                strcpy(slayout,"invalid layout\n");
                return -1;
            }
            printf(" Apply %s layout to all\n", slayout);
            if (H5D_CHUNKED==options->layout_g)
            {
                printf("with dimension [");
                for ( j = 0; j < options->chunk_g.rank; j++)
                    printf("%d ",(int)options->chunk_g.chunk_lengths[j]);
                printf("]\n");
            }
        }
    }/* verbose */

    for ( i = 0; i < options->op_tbl->nelems; i++)
    {
        char* name=options->op_tbl->objs[i].path;

        if (options->op_tbl->objs[i].chunk.rank>0)
        {
            if (options->verbose){
                printf(" <%s> with chunk size ",name);
                for ( k = 0; k < options->op_tbl->objs[i].chunk.rank; k++)
                    printf("%d ",(int)options->op_tbl->objs[i].chunk.chunk_lengths[k]);
                printf("\n");
            }
            has_ck=1;
        }
        else if (options->op_tbl->objs[i].chunk.rank==-2)
        {
            if (options->verbose)
                printf(" <%s> %s\n",name,"NONE (contigous)");
            has_ck=1;
        }
    }

    if (options->all_layout==1 && has_ck)
    {
        error_msg("invalid chunking input: 'all' option\
                            is present with other objects\n");
        return -1;
    }

    /*-------------------------------------------------------------------------
    * objects to filter
    *-------------------------------------------------------------------------
    */

    if (options->verbose && have_request(options) /* only print if requested */)
    {
        printf("Objects to apply filter are...\n");
        if (options->all_filter==1)
        {

            for (k = 0; k < options->n_filter_g; k++ )
            {
                H5Z_filter_t filtn=options->filter_g[k].filtn;
                switch (filtn)
                {
                case H5Z_FILTER_NONE:
                    printf(" Uncompress all\n");
                    break;
                case H5Z_FILTER_SHUFFLE:
                case H5Z_FILTER_FLETCHER32:
                    printf(" All with %s\n",get_sfilter(filtn));
                    break;
                case H5Z_FILTER_SZIP:
                case H5Z_FILTER_DEFLATE:
                    printf(" All with %s, parameter %d\n",
                        get_sfilter(filtn),
                        options->filter_g[k].cd_values[0]);
                    break;
                default:
                    break;
                } /* k */
            };
        }
    } /* verbose */

    for ( i = 0; i < options->op_tbl->nelems; i++)
    {
        pack_info_t pack  = options->op_tbl->objs[i];
        char*       name  = pack.path;

        for ( j=0; j<pack.nfilters; j++)
        {
            if (options->verbose)
            {
                printf(" <%s> with %s filter\n",
                    name,
                    get_sfilter(pack.filter[j].filtn));
            }

            has_cp=1;

        } /* j */
    } /* i */

    if (options->all_filter==1 && has_cp)
    {
        error_msg("invalid compression input: 'all' option\
                            is present with other objects\n");
        return -1;
    }

    /*-------------------------------------------------------------------------
    * check options for the latest format
    *-------------------------------------------------------------------------
    */

    if (options->grp_compact < 0)
    {
        error_msg("invalid maximum number of links to store as header messages\n");
        return -1;
    }
    if (options->grp_indexed < 0)
    {
        error_msg("invalid minimum number of links to store in the indexed format\n");
        return -1;
    }
    if (options->grp_indexed > options->grp_compact)
    {
        error_msg("minimum indexed size is greater than the maximum compact size\n");
        return -1;
    }
    for (i=0; i<8; i++)
    {
        if (options->msg_size[i]<0)
        {
            error_msg("invalid shared message size\n");
            return -1;
        }
    }


    /*--------------------------------------------------------------------------------
    * verify new user userblock options; file name must be present
    *---------------------------------------------------------------------------------
    */
    if ( options->ublock_filename != NULL && options->ublock_size == 0 )
    {
        if ( options->verbose )
        {
            printf("Warning: user block size missing for file %s. Assigning a default size of 1024...\n",
                options->ublock_filename);
            options->ublock_size = 1024;
        }
    }

    if ( options->ublock_filename == NULL && options->ublock_size != 0 )
    {
        error_msg("file name missing for user block\n",
            options->ublock_filename);
        return -1;
    }


    /*--------------------------------------------------------------------------------
    * verify alignment options; threshold is zero default but alignment not
    *---------------------------------------------------------------------------------
    */

    if ( options->alignment == 0 && options->threshold != 0 )
    {
        error_msg("alignment for H5Pset_alignment missing\n");
        return -1;
    }

    return 0;
}


/*-------------------------------------------------------------------------
* Function: check_objects
*
* Purpose: locate all HDF5 objects in the file and compare with user
*  supplied list
*
* Return: 0, ok, -1 no
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: September, 23, 2003
*
*-------------------------------------------------------------------------
*/
static int check_objects(const char* fname,
                         pack_opt_t *options)
{
    hid_t         fid;
    unsigned int  i;
    trav_table_t  *travt = NULL;

    /* nothing to do */
    if(options->op_tbl->nelems == 0)
        return 0;

    /*-------------------------------------------------------------------------
    * open the file
    *-------------------------------------------------------------------------
    */
    if((fid = h5tools_fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL, 0)) < 0)
    {
        printf("<%s>: %s\n", fname, H5FOPENERROR );
        return -1;
    }

    /*-------------------------------------------------------------------------
    * get the list of objects in the file
    *-------------------------------------------------------------------------
    */

    /* init table */
    trav_table_init(&travt);

    /* get the list of objects in the file */
    if(h5trav_gettable(fid, travt) < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * compare with user supplied list
    *-------------------------------------------------------------------------
    */

    if(options->verbose)
        printf("Opening file <%s>. Searching for objects to modify...\n", fname);

    for(i = 0; i < options->op_tbl->nelems; i++)
    {
        char* name=options->op_tbl->objs[i].path;
        if(options->verbose)
            printf(" <%s>",name);

        /* the input object names are present in the file and are valid */
        if(h5trav_getindext(name, travt) < 0)
        {
            error_msg("%s Could not find <%s> in file <%s>. Exiting...\n",
                (options->verbose?"\n":""),name,fname);
            goto out;
        }
        if(options->verbose)
            printf("...Found\n");

        /* check for extra filter conditions */
        switch(options->op_tbl->objs[i].filter->filtn)
        {
            /* chunk size must be smaller than pixels per block */
        case H5Z_FILTER_SZIP:
            {
                int      j;
                hsize_t  csize = 1;
                unsigned ppb = options->op_tbl->objs[i].filter->cd_values[0];
                hsize_t  dims[H5S_MAX_RANK];
                int      rank;
                hid_t    did;
                hid_t    sid;

                if(options->op_tbl->objs[i].chunk.rank > 0) {
                    rank = options->op_tbl->objs[i].chunk.rank;
                    for(j = 0; j < rank; j++)
                        csize *= options->op_tbl->objs[i].chunk.chunk_lengths[j];
                }
                else {
                    if((did = H5Dopen2(fid, name, H5P_DEFAULT)) < 0)
                        goto out;
                    if((sid = H5Dget_space(did)) < 0)
                        goto out;
                    if((rank = H5Sget_simple_extent_ndims(sid)) < 0)
                        goto out;
                    HDmemset(dims, 0, sizeof dims);
                    if(H5Sget_simple_extent_dims(sid, dims, NULL) < 0)
                        goto out;
                    for(j = 0; j < rank; j++)
                        csize *= dims[j];
                    if(H5Sclose(sid) < 0)
                        goto out;
                    if(H5Dclose(did) < 0)
                        goto out;
                }

                if (csize < ppb ) {
                    printf(" <warning: SZIP settins, chunk size is smaller than pixels per block>\n");
                    goto out;
                }
            }
            break;
        default:
            break;
        }
    } /* i */

    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    H5Fclose(fid);
    trav_table_free(travt);
    return 0;

out:
    H5Fclose(fid);
    trav_table_free(travt);
    return -1;
}





/*-------------------------------------------------------------------------
* Function: have_request
*
* Purpose: check if a filter or layout was requested
*
* Return: 1 yes, 0 no
*
* Date: May, 24, 2007
*
*-------------------------------------------------------------------------
*/
static int have_request(pack_opt_t *options)
{

    if (options->all_filter || options->all_layout || options->op_tbl->nelems)
        return 1;

    return 0;

}


/*-------------------------------------------------------------------------
* Function: get_sfilter
*
* Purpose: return the filter as a string name
*
* Return: name of filter, exit on error
*
*-------------------------------------------------------------------------
*/

static const char* get_sfilter(H5Z_filter_t filtn)
{
    if (filtn==H5Z_FILTER_NONE)
        return "NONE";
    else if (filtn==H5Z_FILTER_DEFLATE)
        return "GZIP";
    else if (filtn==H5Z_FILTER_SZIP)
        return "SZIP";
    else if (filtn==H5Z_FILTER_SHUFFLE)
        return "SHUFFLE";
    else if (filtn==H5Z_FILTER_FLETCHER32)
        return "FLETCHER32";
    else if (filtn==H5Z_FILTER_NBIT)
        return "NBIT";
    else if (filtn==H5Z_FILTER_SCALEOFFSET)
        return "SOFF";
    else {
        error_msg("input error in filter type\n");
        exit(EXIT_FAILURE);
    }
}

