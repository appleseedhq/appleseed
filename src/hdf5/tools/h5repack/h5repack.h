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


#ifndef H5REPACK_H__
#define H5REPACK_H__

#include "H5private.h"
#include "hdf5.h"
#include "h5trav.h"

#define H5FOPENERROR "unable to open file"
#define PFORMAT  "%-7s %-7s %-7s\n"   /* chunk info, compression info, name*/
#define PFORMAT1 "%-7s %-7s %-7s"     /* chunk info, compression info, name*/
#define MAX_NC_NAME 256               /* max length of a name */
#define MAX_VAR_DIMS 32               /* max per variable dimensions */
#define FORMAT_OBJ      " %-27s %s\n"   /* obj type, name */
#define FORMAT_OBJ_ATTR "  %-27s %s\n"  /* obj type, name */

/*-------------------------------------------------------------------------
 * data structures for command line options
 *-------------------------------------------------------------------------
 */

/* a list of names */
typedef struct {
 char obj[MAX_NC_NAME];
} obj_list_t;

/*
 the type of filter and additional parameter
 type can be one of the filters
 H5Z_FILTER_NONE        0,  uncompress if compressed
 H5Z_FILTER_DEFLATE     1 , deflation like gzip
 H5Z_FILTER_SHUFFLE     2 , shuffle the data
 H5Z_FILTER_FLETCHER32  3 , letcher32 checksum of EDC
 H5Z_FILTER_SZIP        4 , szip compression
 H5Z_FILTER_NBIT        5 , nbit compression
 H5Z_FILTER_SCALEOFFSET 6 , scaleoffset compression
*/

#define CD_VALUES 20

typedef struct {
 H5Z_filter_t filtn;                           /* filter identification number */
 unsigned     cd_values[CD_VALUES];            /* filter client data values */
 size_t       cd_nelmts;                       /* filter client number of values */
} filter_info_t;

/* chunk lengths along each dimension and rank */
typedef struct {
 hsize_t chunk_lengths[MAX_VAR_DIMS];
 int     rank;
} chunk_info_t;

/* we currently define a maximum value for the filters array,
   that corresponds to the current library filters */
#define H5_REPACK_MAX_NFILTERS 6

/* information for one object, contains PATH, CHUNK info and FILTER info */
typedef struct {
 char          path[MAX_NC_NAME];               /* name of object */
 filter_info_t filter[H5_REPACK_MAX_NFILTERS];  /* filter array */
 int           nfilters;                        /* current number of filters */
 H5D_layout_t  layout;                          /* layout information */
 chunk_info_t  chunk;                           /* chunk information */
 hid_t         refobj_id;                       /* object ID, references */
} pack_info_t;

/* store a table of all objects */
typedef struct {
 unsigned int size;
 unsigned int nelems;
 pack_info_t  *objs;
} pack_opttbl_t;


/*-------------------------------------------------------------------------
 * command line options
 *-------------------------------------------------------------------------
 */

/* all the above, ready to go to the hrepack call */
typedef struct {
 pack_opttbl_t   *op_tbl;     /*table with all -c and -f options */
 int             all_layout;  /*apply the layout to all objects */
 int             all_filter;  /*apply the filter to all objects */
 filter_info_t   filter_g[H5_REPACK_MAX_NFILTERS];    /*global filter array for the ALL case */
 int             n_filter_g;  /*number of global filters */
 chunk_info_t    chunk_g;     /*global chunk INFO for the ALL case */
 H5D_layout_t    layout_g;    /*global layout information for the ALL case */
 int             verbose;     /*verbose mode */
 hsize_t         min_comp;    /*minimum size to compress, in bytes */
 int             use_native;  /*use a native type in write */
 int             latest;      /*pack file with the latest file format */
 int             grp_compact; /* Set the maximum number of links to store as header messages in the group */
 int             grp_indexed; /* Set the minimum number of links to store in the indexed format */
 int             msg_size[8]; /* Minimum size of shared messages: dataspace,
                                 datatype, fill value, filter pipleline, attribute */
 const char      *ublock_filename; /* user block file name */
 hsize_t         ublock_size;      /* user block size */
 hsize_t         threshold;        /* alignment threshold for H5Pset_alignment */
 hsize_t         alignment ;       /* alignment for H5Pset_alignment */
} pack_opt_t;


typedef struct named_dt_t {
    haddr_t             addr_in;    /* Address of the named dtype in the in file */
    hid_t               id_out;     /* Open identifier for the dtype in the out file */
    struct named_dt_t   *next;      /* Next dtype */
} named_dt_t;

/*-------------------------------------------------------------------------
 * public functions
 *-------------------------------------------------------------------------
 */

#ifdef __cplusplus
extern "C" {
#endif

int h5repack           (const char* infile, const char* outfile, pack_opt_t *options);
int h5repack_addfilter (const char* str, pack_opt_t *options);
int h5repack_addlayout (const char* str, pack_opt_t *options);
int h5repack_init      (pack_opt_t *options, int verbose);
int h5repack_end       (pack_opt_t *options);
int h5repack_verify    (const char *fname,pack_opt_t *options);
int h5repack_cmp_pl   (const char *fname1,
                        const char *fname2);

/* Note: The below copy_named_datatype(), named_datatype_free(), copy_attr() 
 * and struct named_dt_t were located in h5repack_copy.c as static prior to 
 * bugfix1726. 
 * Made shared functions as copy_attr() was needed in h5repack_refs.c. 
 * However copy_attr() may be obsoleted when H5Acopy is available and put back
 * others to static in h5repack_copy.c.
 */
hid_t copy_named_datatype(hid_t type_in, hid_t fidout, named_dt_t **named_dt_head_p, trav_table_t *travt, pack_opt_t *options);
int named_datatype_free(named_dt_t **named_dt_head_p, int ignore_err);
int copy_attr(hid_t loc_in, hid_t loc_out, named_dt_t **named_dt_head_p,
              trav_table_t *travt, pack_opt_t *options);

#ifdef __cplusplus
}
#endif



/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */


/*-------------------------------------------------------------------------
 * copy module
 *-------------------------------------------------------------------------
 */

int copy_objects   (const char* fnamein,
                    const char* fnameout,
                    pack_opt_t *options);

int do_copy_refobjs(hid_t fidin,
                    hid_t fidout,
                    trav_table_t *travt,
                    pack_opt_t *options);

/*-------------------------------------------------------------------------
 * filters and verify module
 *-------------------------------------------------------------------------
 */
void init_packobject(pack_info_t *obj);


/*-------------------------------------------------------------------------
 * filters and copy module
 *-------------------------------------------------------------------------
 */

int apply_filters(const char* name,    /* object name from traverse list */
                  int rank,            /* rank of dataset */
                  hsize_t *dims,       /* dimensions of dataset */
                  size_t msize,        /* size of type */
                  hid_t dcpl_id,       /* dataset creation property list */
                  pack_opt_t *options, /* repack options */
                  int *has_filter);    /* (OUT) object NAME has a filter */


/*-------------------------------------------------------------------------
 * options table
 *-------------------------------------------------------------------------
 */
int          options_table_init( pack_opttbl_t **tbl );
int          options_table_free( pack_opttbl_t *table );
int          options_add_layout( obj_list_t *obj_list,
                                 int n_objs,
                                 pack_info_t *pack,
                                 pack_opttbl_t *table );
int          options_add_filter ( obj_list_t *obj_list,
                                 int n_objs,
                                 filter_info_t filt,
                                 pack_opttbl_t *table );
pack_info_t* options_get_object( const char *path,
                                 pack_opttbl_t *table);

/*-------------------------------------------------------------------------
 * parse functions
 *-------------------------------------------------------------------------
 */

obj_list_t* parse_filter(const char *str,
                         int *n_objs,
                         filter_info_t *filt,
                         pack_opt_t *options,
                         int *is_glb);

obj_list_t* parse_layout(const char *str,
                         int *n_objs,
                         pack_info_t *pack,    /* info about object */
                         pack_opt_t *options);




#endif  /* H5REPACK_H__ */

