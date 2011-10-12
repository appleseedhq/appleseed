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

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5diff.h"
#include "ph5diff.h"

/*
 * Debug printf macros. The prefix allows output filtering by test scripts.
 */
#ifdef H5DIFF_DEBUG
#define h5diffdebug(x) fprintf(stderr, "h5diff debug: " x)
#define h5diffdebug2(x1, x2) fprintf(stderr, "h5diff debug: " x1, x2)
#define h5diffdebug3(x1, x2, x3) fprintf(stderr, "h5diff debug: " x1, x2, x3)
#define h5diffdebug4(x1, x2, x3, x4) fprintf(stderr, "h5diff debug: " x1, x2, x3, x4)
#define h5diffdebug5(x1, x2, x3, x4, x5) fprintf(stderr, "h5diff debug: " x1, x2, x3, x4, x5)
#else
#define h5diffdebug(x)
#define h5diffdebug2(x1, x2)
#define h5diffdebug3(x1, x2, x3)
#define h5diffdebug4(x1, x2, x3, x4)
#define h5diffdebug5(x1, x2, x3, x4, x5)
#endif


/*-------------------------------------------------------------------------
 * Function: print_objname
 *
 * Purpose: check if object name is to be printed, only when:
 *  1) verbose mode
 *  2) when diff was found (normal mode)
 *-------------------------------------------------------------------------
 */
int print_objname (diff_opt_t * options, hsize_t nfound)
{
    return ((options->m_verbose || nfound) && !options->m_quiet) ? 1 : 0;
}

/*-------------------------------------------------------------------------
 * Function: do_print_objname
 *
 * Purpose: print object name
 *
 *-------------------------------------------------------------------------
 */
void do_print_objname (const char *OBJ, const char *path1, const char *path2, diff_opt_t * opts)
{
    /* if verbose level is higher than 0, put space line before
     * displaying any object or symbolic links. This improves
     * readability of the output. 
     */
    if (opts->m_verbose_level >= 1)
        parallel_print("\n");
    parallel_print("%-7s: <%s> and <%s>\n", OBJ, path1, path2);
}

/*-------------------------------------------------------------------------
 * Function: do_print_attrname
 *
 * Purpose: print attribute name
 *
 *-------------------------------------------------------------------------
 */
void
do_print_attrname (const char *attr, const char *path1, const char *path2)
{
    parallel_print("%-7s: <%s> and <%s>\n", attr, path1, path2);
}

/*-------------------------------------------------------------------------
 * Function: print_warn
 *
 * Purpose: check print warning condition.
 * Return: 
 *    1 if verbose mode
 *    0 if not verbos mode
 * Programmer: Jonathan Kim
 * Date: Feb 4, 2010
 *-------------------------------------------------------------------------
 */
static int print_warn(diff_opt_t *options)
{
    return ((options->m_verbose))?1:0;
}


#ifdef H5_HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function: phdiff_dismiss_workers
 *
 * Purpose: tell all workers to end.
 *
 * Return: none
 *
 * Programmer: Albert Cheng
 *
 * Date: Feb 6, 2005
 *
 *-------------------------------------------------------------------------
 */
void phdiff_dismiss_workers(void)
{
    int i;
    for(i=1; i<g_nTasks; i++)
        MPI_Send(NULL, 0, MPI_BYTE, i, MPI_TAG_END, MPI_COMM_WORLD);
}


/*-------------------------------------------------------------------------
 * Function: print_manager_output
 *
 * Purpose: special function that prints any output accumulated by the
 *      manager task.
 *
 * Return: none
 *
 * Programmer: Leon Arber
 *
 * Date: Feb 7, 2005
 *
 *-------------------------------------------------------------------------
 */
void print_manager_output(void)
{
    /* If there was something we buffered, let's print it now */
    if( (outBuffOffset>0) && g_Parallel)
    {
        printf("%s", outBuff);

        if(overflow_file)
        {
            int     tmp;
            rewind(overflow_file);
            while((tmp = getc(overflow_file)) >= 0)
                putchar(tmp);
            fclose(overflow_file);
            overflow_file = NULL;
        }

        fflush(stdout);
        memset(outBuff, 0, OUTBUFF_SIZE);
        outBuffOffset = 0;
    }
    else if( (outBuffOffset>0) && !g_Parallel)
    {
        fprintf(stderr, "h5diff error: outBuffOffset>0, but we're not in parallel!\n");
    }
}

/*-------------------------------------------------------------------------
 * Function: print_incoming_data
 *
 * Purpose: special function that prints any output that has been sent to the manager
 *      and is currently sitting in the incoming message queue
 *
 * Return: none
 *
 * Programmer: Leon Arber
 *
 * Date: March 7, 2005
 *
 *-------------------------------------------------------------------------
 */

static void print_incoming_data(void)
{
    char data[PRINT_DATA_MAX_SIZE+1];
    int  incomingMessage;
    MPI_Status Status;

    do
    {
        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &incomingMessage, &Status);
        if(incomingMessage)
        {
            memset(data, 0, PRINT_DATA_MAX_SIZE+1);
            MPI_Recv(data, PRINT_DATA_MAX_SIZE, MPI_CHAR, Status.MPI_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &Status);

            printf("%s", data);
        }
    } while(incomingMessage);
}
#endif

/*-------------------------------------------------------------------------
 * Function: is_valid_options
 *
 * Purpose: check if options are valid
 *
 * Return: 
 *  1 : Valid
 *  0 : Not valid
 *
 * Programmer: Jonathan Kim
 *
 * Date: Feb 17, 2010
 *
 *------------------------------------------------------------------------*/
static int is_valid_options(diff_opt_t *options)
{
    int ret=1; /* init to valid */

    /*-----------------------------------------------
     * no -q(quiet) with -v (verbose) or -r (report) */
    if(options->m_quiet && (options->m_verbose || options->m_report))
    {
        parallel_print("Error: -q (quiet mode) cannot be added to verbose or report modes\n");
        options->err_stat=1;
        ret = 0;
        goto out;
    }

    /* -------------------------------------------------------
     * only allow --no-dangling-links along with --follow-symlinks */
    if(options->no_dangle_links && !options->follow_links)
    {
        parallel_print("Error: --no-dangling-links must be used along with --follow-symlinks option.\n");
        options->err_stat=1;
        ret = 0;
        goto out;
    }

out:

    return ret;
}

/*-------------------------------------------------------------------------
 * Function: is_exclude_path
 *
 * Purpose: check if 'paths' are part of exclude path list
 *
 * Return:  
 *   1 - excluded path
 *   0 - not excluded path
 * 
 * Programmer: Jonathan Kim
 * Date: Aug 23, 2010
 *------------------------------------------------------------------------*/
static int is_exclude_path (char * path, h5trav_type_t type, diff_opt_t *options)
{
    struct exclude_path_list * exclude_path_ptr;
    int ret_cmp;
    int ret = 0;
    int len_grp;

    /* check if exclude path option is given */
    if (!options->exclude_path)
        goto out;

    /* assign to local exclude list pointer */
    exclude_path_ptr = options->exclude;

    /* search objects in exclude list */
    while (NULL != exclude_path_ptr)
    {
        /* if given object is group, exclude its members as well */
        if (exclude_path_ptr->obj_type == H5TRAV_TYPE_GROUP)
        {
            ret_cmp = HDstrncmp(exclude_path_ptr->obj_path, path,
                                strlen(exclude_path_ptr->obj_path));
            if (ret_cmp == 0)
            {
                /* check if given path belong to an excluding group, if so 
                 * exclude it as well.
                 * This verifies if “/grp1/dset1” is only under “/grp1”, but
                 * not under “/grp1xxx/” group.  
                 */ 
                len_grp = HDstrlen(exclude_path_ptr->obj_path);
                if (path[len_grp] == '/')
                {
                    /* belong to excluded group! */
                    ret = 1;
                    break;  /* while */
                }
            }
        }
        /* exclude target is not group, just exclude the object */
        else  
        {
            ret_cmp = HDstrcmp(exclude_path_ptr->obj_path, path);
            if (ret_cmp == 0)
            {
                /* excluded non-group object */
                ret = 1;
                /* assign type as scan progress, which is sufficient to 
                 * determine type for excluding groups from the above if. */
                exclude_path_ptr->obj_type = type;
                break; /* while */
            }
        }
        exclude_path_ptr = exclude_path_ptr->next;
    }

out:
    return  ret;
}


/*-------------------------------------------------------------------------
 * Function: free_exclude_path_list
 *
 * Purpose: free exclud object list from diff options
 *
 * Programmer: Jonathan Kim
 * Date: Aug 23, 2010
 *------------------------------------------------------------------------*/
static void free_exclude_path_list(diff_opt_t *options)
{
    struct exclude_path_list * curr = options->exclude;
    struct exclude_path_list * next;

    while (NULL != curr)
    {
        next = curr->next;
        HDfree(curr);
        curr = next;
    }
}

/*-------------------------------------------------------------------------
 * Function: build_match_list
 *
 * Purpose: get list of matching path_name from info1 and info2
 *
 * Note:
 *  Find common objects; the algorithm used for this search is the
 *  cosequential match algorithm and is described in
 *  Folk, Michael; Zoellick, Bill. (1992). File Structures. Addison-Wesley.
 *  Moved out from diff_match() to make code more flexible.
 *
 * Parameter:
 *  table_out [OUT] : return the list
 *
 * Programmer: Jonathan Kim
 *
 * Date: Aug 18, 2010
 *------------------------------------------------------------------------*/
static void build_match_list (const char *objname1, trav_info_t *info1, const char *objname2, trav_info_t *info2, trav_table_t ** table_out, diff_opt_t *options)
{
    unsigned i;
    size_t curr1 = 0;
    size_t curr2 = 0;
    unsigned infile[2];
    char * path1_lp;
    char * path2_lp;
    h5trav_type_t type1_l;
    h5trav_type_t type2_l;
    int path1_offset = 0;
    int path2_offset = 0;
    int cmp;
    trav_table_t *table;

    /* init */
    trav_table_init( &table );

    /*
     * This is necessary for the case that given objects are group and
     * have different names (ex: obj1 is /grp1 and obj2 is /grp5).
     * All the objects belong to given groups are the cadidates.
     * So prepare to compare paths without the group names.
     */
    /* if obj1 is not root */
    if (HDstrcmp (objname1,"/") != 0)
        path1_offset = HDstrlen(objname1);
    /* if obj2 is not root */
    if (HDstrcmp (objname2,"/") != 0)
        path2_offset = HDstrlen(objname2);

    /*--------------------------------------------------
    * build the list
    */
    while(curr1 < info1->nused && curr2 < info2->nused)
    {
        
        path1_lp = (info1->paths[curr1].path) + path1_offset;
        path2_lp = (info2->paths[curr2].path) + path2_offset;
        type1_l = info1->paths[curr1].type;
        type2_l = info2->paths[curr2].type;
        
        /* criteria is string compare */
        cmp = HDstrcmp(path1_lp, path2_lp);

        if(cmp == 0) {
            if(!is_exclude_path(path1_lp, type1_l, options))
            {
                infile[0] = 1;
                infile[1] = 1;
                trav_table_addflags(infile, path1_lp, info1->paths[curr1].type, table);
            }
            curr1++;
            curr2++;
        } /* end if */
        else if(cmp < 0)
        {
            if(!is_exclude_path(path1_lp, type1_l, options))
            {
                infile[0] = 1;
                infile[1] = 0;
                trav_table_addflags(infile, path1_lp, info1->paths[curr1].type, table);
            }
            curr1++;
        } /* end else-if */
        else
        {
            if (!is_exclude_path(path2_lp, type2_l, options))
            {
                infile[0] = 0;
                infile[1] = 1;
                trav_table_addflags(infile, path2_lp, info2->paths[curr2].type, table);
            }
            curr2++;
        } /* end else */
    } /* end while */

    /* list1 did not end */
    infile[0] = 1;
    infile[1] = 0;
    while(curr1 < info1->nused)
    {
        if(!is_exclude_path(path1_lp, type1_l, options))
        {
            path1_lp = (info1->paths[curr1].path) + path1_offset;
            trav_table_addflags(infile, path1_lp, info1->paths[curr1].type, table);
        }
        curr1++;
    } /* end while */

    /* list2 did not end */
    infile[0] = 0;
    infile[1] = 1;
    while(curr2 < info2->nused)
    {
        if (!is_exclude_path(path2_lp, type2_l, options))
        {
            path2_lp = (info2->paths[curr2].path) + path2_offset;
            trav_table_addflags(infile, path2_lp, info2->paths[curr2].type, table);
        } 
        curr2++;
    } /* end while */

    free_exclude_path_list (options);
   /*------------------------------------------------------
    * print the list
    */
    if(options->m_verbose)
    {
        parallel_print("\n");
        /* if given objects is group under root */
        if (HDstrcmp (objname1,"/") || HDstrcmp (objname2,"/"))
            parallel_print("group1   group2\n");
        else
            parallel_print("file1     file2\n");
        parallel_print("---------------------------------------\n");
        for(i = 0; i < table->nobjs; i++) 
        {
            char c1, c2;
            c1 = (table->objs[i].flags[0]) ? 'x' : ' ';
            c2 = (table->objs[i].flags[1]) ? 'x' : ' ';
            parallel_print("%5c %6c    %-15s\n", c1, c2, table->objs[i].name);
        } /* end for */
        parallel_print ("\n");
    } /* end if */

    *table_out = table;
}


/*-------------------------------------------------------------------------
 * Function: trav_grp_objs
 *
 * Purpose: 
 *  Call back function from h5trav_visit(). 
 *
 * Programmer: Jonathan Kim
 *
 * Date: Aug 16, 2010
 *------------------------------------------------------------------------*/
static herr_t trav_grp_objs(const char *path, const H5O_info_t *oinfo,
    const char *already_visited, void *udata)
{
    trav_info_visit_obj(path, oinfo, already_visited, udata);

    return 0;
} 

/*-------------------------------------------------------------------------
 * Function: trav_grp_symlinks
 *
 * Purpose: 
 *  Call back function from h5trav_visit(). 
 *  Track and extra checkings while visiting all symbolic-links.
 *
 * Programmer: Jonathan Kim
 *
 * Date: Aug 16, 2010
 *------------------------------------------------------------------------*/
static herr_t trav_grp_symlinks(const char *path, const H5L_info_t *linfo, 
                               void *udata)
{                               
    trav_info_t *tinfo = (trav_info_t *)udata;
    diff_opt_t *opts = (diff_opt_t *)tinfo->opts;
    int ret;
    h5tool_link_info_t lnk_info;
    const char *ext_fname;
    const char *ext_path;

    /* init linkinfo struct */
    memset(&lnk_info, 0, sizeof(h5tool_link_info_t));

    if (!opts->follow_links)
    {
        trav_info_visit_lnk(path, linfo, tinfo);
        goto done;
    }

    switch(linfo->type)
    {
    case H5L_TYPE_SOFT:
        ret = H5tools_get_symlink_info(tinfo->fid, path, &lnk_info, opts->follow_links);
        /* error */
        if (ret < 0)
            goto done;
        /* no dangling link option given and detect dangling link */
        else if (ret == 0)
        {
            tinfo->symlink_visited.dangle_link = TRUE;
            trav_info_visit_lnk(path, linfo, tinfo);
            if (opts->no_dangle_links)
                opts->err_stat = 1; /* make dgangling link is error */
            goto done;
        }

        /* check if already visit the target object */        
        if(symlink_is_visited( &(tinfo->symlink_visited), linfo->type, NULL, lnk_info.trg_path)) 
            goto done;

        /* add this link as visited link */
        if(symlink_visit_add( &(tinfo->symlink_visited), linfo->type, NULL, lnk_info.trg_path) < 0) 
            goto done;
                
        if(h5trav_visit(tinfo->fid, path, TRUE, TRUE,
                     trav_grp_objs,trav_grp_symlinks, tinfo) < 0)
        {
            parallel_print("Error: Could not get file contents\n");
            opts->err_stat = 1;
            goto done;
        }
        break;
    
    case H5L_TYPE_EXTERNAL:    
        ret = H5tools_get_symlink_info(tinfo->fid, path, &lnk_info, opts->follow_links);
        /* error */
        if (ret < 0)
            goto done;
        /* no dangling link option given and detect dangling link */
        else if (ret == 0)
        {
            tinfo->symlink_visited.dangle_link = TRUE;
            trav_info_visit_lnk(path, linfo, tinfo);
            if (opts->no_dangle_links)
                opts->err_stat = 1; /* make dgangling link is error */
            goto done;
        }

        if(H5Lunpack_elink_val(lnk_info.trg_path, linfo->u.val_size, NULL, &ext_fname, &ext_path) < 0) 
            goto done;

        /* check if already visit the target object */        
        if(symlink_is_visited( &(tinfo->symlink_visited), linfo->type, ext_fname, ext_path)) 
            goto done;

        /* add this link as visited link */
        if(symlink_visit_add( &(tinfo->symlink_visited), linfo->type, ext_fname, ext_path) < 0) 
            goto done;
                
        if(h5trav_visit(tinfo->fid, path, TRUE, TRUE,
                        trav_grp_objs,trav_grp_symlinks, tinfo) < 0)
        {
            parallel_print("Error: Could not get file contents\n");
            opts->err_stat = 1;
            goto done;
        }
        break;
    default:
        ;
        break;
    } /* end of switch */

done:    
    if (lnk_info.trg_path)
        HDfree(lnk_info.trg_path);
    return 0;
}    


/*-------------------------------------------------------------------------
 * Function: h5diff
 *
 * Purpose: public function, can be called in an application program.
 *   return differences between 2 HDF5 files
 *
 * Return: Number of differences found.
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 22, 2003
 *
 *-------------------------------------------------------------------------
 */
hsize_t h5diff(const char *fname1,
               const char *fname2,
               const char *objname1,
               const char *objname2,
               diff_opt_t *options)
{
    hid_t        file1_id = (-1);
    hid_t        file2_id = (-1);
    char         filenames[2][MAX_FILENAME];
    hsize_t      nfound = 0;
    int i;
    //int i1, i2;
    int l_ret;
    const char * obj1fullname = NULL;
    const char * obj2fullname = NULL;
    /* init to group type */
    h5trav_type_t obj1type = H5TRAV_TYPE_GROUP;
    h5trav_type_t obj2type = H5TRAV_TYPE_GROUP;
    /* for single object */
    H5O_info_t oinfo1, oinfo2; /* object info */
    trav_info_t  *info1_obj = NULL;
    trav_info_t  *info2_obj = NULL;
    /* for group object */
    trav_info_t  *info1_grp = NULL;
    trav_info_t  *info2_grp = NULL;
    /* local pointer */
    trav_info_t  *info1_lp;
    trav_info_t  *info2_lp;
    /* link info from specified object */
    H5L_info_t src_linfo1;
    H5L_info_t src_linfo2;
    /* link info from member object */
    h5tool_link_info_t trg_linfo1;
    h5tool_link_info_t trg_linfo2;
    /* list for common objects */
    trav_table_t *match_list = NULL;

    /* init filenames */
    HDmemset(filenames, 0, MAX_FILENAME * 2);
    /* init link info struct */
    HDmemset(&trg_linfo1, 0, sizeof(h5tool_link_info_t));
    HDmemset(&trg_linfo2, 0, sizeof(h5tool_link_info_t));

   /*-------------------------------------------------------------------------
    * check invalid combination of options
    *-----------------------------------------------------------------------*/
    if(!is_valid_options(options))
        goto out;

    /*-------------------------------------------------------------------------
    * open the files first; if they are not valid, no point in continuing
    *-------------------------------------------------------------------------
    */

    /* disable error reporting */
    H5E_BEGIN_TRY
    {
        /* open file 1 */
        if((file1_id = h5tools_fopen(fname1, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL, (size_t)0)) < 0) 
        {
            parallel_print("h5diff: <%s>: unable to open file\n", fname1);
            options->err_stat = 1;
            goto out;
        } /* end if */


        /* open file 2 */
        if((file2_id = h5tools_fopen(fname2, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL, (size_t)0)) < 0) 
        {
            parallel_print("h5diff: <%s>: unable to open file\n", fname2);
            options->err_stat = 1;
            goto out;
        } /* end if */
    /* enable error reporting */
    } H5E_END_TRY;

    /*-------------------------------------------------------------------------
    * Initialize the info structs
    *-------------------------------------------------------------------------
    */
    trav_info_init(fname1, file1_id, &info1_obj);
    trav_info_init(fname2, file2_id, &info2_obj);

    /* if any object is specified */
    if (objname1)
    {
        /* malloc 2 more for "/" and end-of-line */
        obj1fullname = (char*)HDcalloc(HDstrlen(objname1) + 2, sizeof(char));
        obj2fullname = (char*)HDcalloc(HDstrlen(objname2) + 2, sizeof(char));

        /* make the given object1 fullpath, start with "/"  */
        if (HDstrncmp(objname1, "/", 1))
        {
            HDstrcpy(obj1fullname, "/");
            HDstrcat(obj1fullname, objname1);
        }
        else
            HDstrcpy(obj1fullname, objname1);

        /* make the given object2 fullpath, start with "/" */
        if (HDstrncmp(objname2, "/", 1))
        {
            HDstrcpy(obj2fullname, "/");
            HDstrcat(obj2fullname, objname2);
        }
        else
            HDstrcpy(obj2fullname, objname2);

        /*----------------------------------------------------------
         * check if obj1 is root, group, single object or symlink
         */
        if(!HDstrcmp(obj1fullname, "/"))
        {
            obj1type = H5TRAV_TYPE_GROUP;
        }
        else
        {
            /* check if link itself exist */
            if(H5Lexists(file1_id, obj1fullname, H5P_DEFAULT) <= 0) 
            {
                parallel_print ("Object <%s> could not be found in <%s>\n", obj1fullname, fname1);
                options->err_stat = 1;
                goto out;
            }
            /* get info from link */
            if(H5Lget_info(file1_id, obj1fullname, &src_linfo1, H5P_DEFAULT) < 0) 
            {
                parallel_print("Unable to get link info from <%s>\n", obj1fullname);
                goto out;
            }

            info1_lp = info1_obj;

            /* 
             * check the type of specified path for hard and symbolic links
             */
            if(src_linfo1.type == H5L_TYPE_HARD)
            {
                /* optional data pass */
                info1_obj->opts = (diff_opt_t*)options;

                if(H5Oget_info_by_name(file1_id, obj1fullname, &oinfo1, H5P_DEFAULT) < 0)
                {
                    parallel_print("Error: Could not get file contents\n");
                    options->err_stat = 1;
                    goto out;
                }
                obj1type = oinfo1.type;
                trav_info_add(info1_obj, obj1fullname, obj1type);
            }
            else if (src_linfo1.type == H5L_TYPE_SOFT)
            {
                obj1type = H5TRAV_TYPE_LINK;
                trav_info_add(info1_obj, obj1fullname, obj1type);
            }
            else if (src_linfo1.type == H5L_TYPE_EXTERNAL)
            {
                obj1type = H5TRAV_TYPE_UDLINK;
                trav_info_add(info1_obj, obj1fullname, obj1type);
            }
        }

        /*----------------------------------------------------------
         * check if obj2 is root, group, single object or symlink
         */
        if(!HDstrcmp(obj2fullname, "/"))
        {
            obj2type = H5TRAV_TYPE_GROUP;
        }
        else
        {
            /* check if link itself exist */
            if(H5Lexists(file2_id, obj2fullname, H5P_DEFAULT) <= 0) 
            {
                parallel_print ("Object <%s> could not be found in <%s>\n", obj2fullname, fname2);
                options->err_stat = 1;
                goto out;
            }
            /* get info from link */
            if(H5Lget_info(file2_id, obj2fullname, &src_linfo2, H5P_DEFAULT) < 0) 
            {
                parallel_print("Unable to get link info from <%s>\n", obj2fullname);
                goto out;
            }

            info2_lp = info2_obj;

            /* 
             * check the type of specified path for hard and symbolic links
             */
            if(src_linfo2.type == H5L_TYPE_HARD)
            {
                /* optional data pass */
                info2_obj->opts = (diff_opt_t*)options;

                if(H5Oget_info_by_name(file2_id, obj2fullname, &oinfo2, H5P_DEFAULT) < 0)
                {
                    parallel_print("Error: Could not get file contents\n");
                    options->err_stat = 1;
                    goto out;
                }
                obj2type = oinfo2.type;
                trav_info_add(info2_obj, obj2fullname, obj2type);
            }
            else if (src_linfo2.type == H5L_TYPE_SOFT)
            {
                obj2type = H5TRAV_TYPE_LINK;
                trav_info_add(info2_obj, obj2fullname, obj2type);
            }
            else if (src_linfo2.type == H5L_TYPE_EXTERNAL)
            {
                obj2type = H5TRAV_TYPE_UDLINK;
                trav_info_add(info2_obj, obj2fullname, obj2type);
            }
        }           
    }
    /* if no object specified */
    else
    {
        /* set root group */
        obj1fullname = (char*)HDcalloc(2, sizeof(char));
        HDstrcat(obj1fullname, "/");
        obj2fullname = (char*)HDcalloc(2, sizeof(char));
        HDstrcat(obj2fullname, "/");
    }

    options->cmn_objs = 1; /* eliminate warning */

    /*---------------------------------------------
     * check for following symlinks 
     */
    if (options->follow_links)
    {
        /* pass how to handle printing warning to linkinfo option */
        if(print_warn(options))
            trg_linfo1.opt.msg_mode = trg_linfo2.opt.msg_mode = 1;

        /*-------------------------------
         * check symbolic link (object1)
         */
        l_ret = H5tools_get_symlink_info(file1_id, obj1fullname, &trg_linfo1, TRUE);
        /* dangling link */
        if (l_ret == 0)
        {
            if (options->no_dangle_links)
            {
                /* gangling link is error */
                if(options->m_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", obj1fullname);
                options->err_stat = 1;
                goto out;
            }
            else
            {
                if(options->m_verbose)
                    parallel_print("obj1 <%s> is a dangling link.\n", obj1fullname);
                nfound++;
                print_found(nfound);
                goto out;
            }
        }
        else if(l_ret < 0) /* fail */
        {
            parallel_print ("Object <%s> could not be found in <%s>\n", obj1fullname, fname1);
            options->err_stat = 1;
            goto out;
        }
        else if(l_ret != 2) /* symbolic link */
            obj1type = trg_linfo1.trg_type;

        /*-------------------------------
         * check symbolic link (object2)
         */
        l_ret = H5tools_get_symlink_info(file2_id, obj2fullname, &trg_linfo2, TRUE);
        /* dangling link */
        if (l_ret == 0)
        {
            if (options->no_dangle_links)
            {
                /* gangling link is error */
                if(options->m_verbose)
                    parallel_print("Warning: <%s> is a dangling link.\n", obj2fullname);
                options->err_stat = 1;
                goto out;
            }
            else
            {
                if(options->m_verbose)
                    parallel_print("obj2 <%s> is a dangling link.\n", obj2fullname);
                nfound++;
                print_found(nfound);
                goto out;
            }
        }
        else if(l_ret < 0) /* fail */ 
        {
            parallel_print ("Object <%s> could not be found in <%s>\n", obj2fullname, fname2);
            options->err_stat = 1;
            goto out;
        }
        else if(l_ret != 2)  /* symbolic link */
            obj2type = trg_linfo2.trg_type;
    } /* end of if follow symlinks */


    /* if both obj1 and obj2 are group */
    if (obj1type == H5TRAV_TYPE_GROUP && obj2type == H5TRAV_TYPE_GROUP)
    {
        /* 
         * traverse group1 
         */
        trav_info_init(fname1, file1_id, &info1_grp);
        /* optional data pass */
        info1_grp->opts = (diff_opt_t*)options;

        if(h5trav_visit(file1_id,obj1fullname,TRUE,TRUE,
                        trav_grp_objs,trav_grp_symlinks, info1_grp) < 0)
        {
            parallel_print("Error: Could not get file contents\n");
            options->err_stat = 1;
            goto out;
        }
        info1_lp = info1_grp;

        /* 
         * traverse group2 
         */
        trav_info_init(fname2, file2_id, &info2_grp);
        /* optional data pass */
        info2_grp->opts = (diff_opt_t*)options;

        if(h5trav_visit(file2_id,obj2fullname,TRUE,TRUE,
                        trav_grp_objs,trav_grp_symlinks, info2_grp) < 0)
        {
            parallel_print("Error: Could not get file contents\n");
            options->err_stat = 1;
            goto out;
        } /* end if */
        info2_lp = info2_grp;


#ifdef H5_HAVE_PARALLEL
        if(g_Parallel)
        {
            if((HDstrlen(fname1) > MAX_FILENAME) || 
               (HDstrlen(fname2) > MAX_FILENAME))
            {
                fprintf(stderr, "The parallel diff only supports path names up to %d characters\n", MAX_FILENAME);
                MPI_Abort(MPI_COMM_WORLD, 0);
            } /* end if */

            HDstrcpy(filenames[0], fname1);
            HDstrcpy(filenames[1], fname2);

            /* Alert the worker tasks that there's going to be work. */
            for(i = 1; i < g_nTasks; i++)
                MPI_Send(filenames, (MAX_FILENAME * 2), MPI_CHAR, i, MPI_TAG_PARALLEL, MPI_COMM_WORLD);
        } /* end if */
#endif
        build_match_list (obj1fullname, info1_lp, obj2fullname, info2_lp, 
                         &match_list, options);
        nfound = diff_match(file1_id, obj1fullname, info1_lp, 
                            file2_id, obj2fullname, info2_lp, 
                            match_list, options); 
    }
    else
    {
#ifdef H5_HAVE_PARALLEL
        if(g_Parallel)
            /* Only single object diff, parallel workers won't be needed */
            phdiff_dismiss_workers();
#endif

        nfound = diff_compare(file1_id, fname1, obj1fullname, info1_lp,
                              file2_id, fname2, obj2fullname, info2_lp,
                              options);
    }

out:
#ifdef H5_HAVE_PARALLEL
    if(g_Parallel)
        /* All done at this point, let tasks know that they won't be needed */
        phdiff_dismiss_workers();
#endif
    /* free buffers in trav_info structures */
    if (info1_obj)
        trav_info_free(info1_obj);
    if (info2_obj)
        trav_info_free(info2_obj);

    if (info1_grp)
        trav_info_free(info1_grp);
    if (info2_grp)
        trav_info_free(info2_grp);

    /* free buffers */
    if (obj1fullname)
        HDfree(obj1fullname);
    if (obj2fullname)
        HDfree(obj2fullname);

    /* free link info buffer */
    if (trg_linfo1.trg_path)
        HDfree(trg_linfo1.trg_path);
    if (trg_linfo2.trg_path)
        HDfree(trg_linfo2.trg_path);

    /* close */
    H5E_BEGIN_TRY
    {
        H5Fclose(file1_id);
        H5Fclose(file2_id);
    } H5E_END_TRY;

    return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_match
 *
 * Purpose: 
 *  Compare common objects in given groups according to table structure. 
 *  The table structure has flags which can be used to find common objects 
 *  and will be compared. 
 *  Common object means same name (absolute path) objects in both location.
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Modifications: Jan 2005 Leon Arber, larber@uiuc.edu
 *    Added support for parallel diffing
 *
 * Pedro Vicente, pvn@hdfgroup.org, Nov 4, 2008
 *    Compare the graph and make h5diff return 1 for difference if
 * 1) the number of objects in file1 is not the same as in file2
 * 2) the graph does not match, i.e same names (absolute path)
 * 3) objects with the same name are not of the same type
 *-------------------------------------------------------------------------
 */
hsize_t diff_match(hid_t file1_id, const char *grp1, trav_info_t *info1,
                   hid_t file2_id, const char *grp2, trav_info_t *info2,
                   trav_table_t *table, diff_opt_t *options)
{
    hsize_t      nfound = 0;
    unsigned     i;

    char * grp1_path = "";
    char * grp2_path = "";
    char * obj1_fullpath = NULL;
    char * obj2_fullpath = NULL;


    /* 
     * if not root, prepare object name to be pre-appended to group path to
     * make full path
     */
    if (HDstrcmp (grp1, "/"))
        grp1_path = grp1;
    if (HDstrcmp (grp2, "/"))
        grp2_path = grp2;

    /*-------------------------------------------------------------------------
    * regarding the return value of h5diff (0, no difference in files, 1 difference )
    * 1) the number of objects in file1 must be the same as in file2
    * 2) the graph must match, i.e same names (absolute path)
    * 3) objects with the same name must be of the same type
    *-------------------------------------------------------------------------
    */     
       
    /* not valid compare nused when --exclude-path option is used */
    if (!options->exclude_path)
    {
        /* number of different objects */
        if ( info1->nused != info2->nused )
        {
            options->contents = 0;
        }
    }
    
    /* objects in one file and not the other */
    for( i = 0; i < table->nobjs; i++)
    {
        if( table->objs[i].flags[0] != table->objs[i].flags[1] )
        {
            options->contents = 0;
            break;
        }
    }

    /* objects with the same name but different HDF5 types */
    for( i = 0; i < table->nobjs; i++) 
    {
        if ( table->objs[i].flags[0] && table->objs[i].flags[1] )
        {
            if ( table->objs[i].type != table->objs[i].type )
            {
                options->contents = 0;
            }
        }
    }

    /*-------------------------------------------------------------------------
    * do the diff for common objects
    *-------------------------------------------------------------------------
    */
#ifdef H5_HAVE_PARALLEL
    {
    char *workerTasks = (char*)HDmalloc((g_nTasks - 1) * sizeof(char));
    int n;
    int busyTasks = 0;
    struct diffs_found nFoundbyWorker;
    struct diff_args args;
    int havePrintToken = 1;
    MPI_Status Status;

    /*set all tasks as free */
    HDmemset(workerTasks, 1, (g_nTasks - 1));
#endif

    for(i = 0; i < table->nobjs; i++)
    {
        if( table->objs[i].flags[0] && table->objs[i].flags[1])
        {
            /* make full path for obj1 */
            obj1_fullpath = (char*)HDcalloc (strlen(grp1_path) + strlen (table->objs[i].name) + 1, sizeof (char));
            HDstrcpy(obj1_fullpath, grp1_path);
            HDstrcat(obj1_fullpath, table->objs[i].name);

            /* make full path for obj2 */
            obj2_fullpath = (char*)HDcalloc (strlen(grp2_path) + strlen (table->objs[i].name) + 1, sizeof (char));
            HDstrcpy(obj2_fullpath, grp2_path);
            HDstrcat(obj2_fullpath, table->objs[i].name);

            options->cmn_objs = 1;
            if(!g_Parallel)
            {
                nfound += diff(file1_id, obj1_fullpath,
                               file2_id, obj2_fullpath, 
                               options, table->objs[i].type);
            } /* end if */
#ifdef H5_HAVE_PARALLEL
            else
            {
                int workerFound = 0;

                h5diffdebug("beginning of big else block\n");
                /* We're in parallel mode */
                /* Since the data type of diff value is hsize_t which can
                * be arbitary large such that there is no MPI type that
                * matches it, the value is passed between processes as
                * an array of bytes in order to be portable.  But this
                * may not work in non-homogeneous MPI environments.
                */

                /*Set up args to pass to worker task. */
                if(HDstrlen(obj1_fullpath) > 255 || 
                   HDstrlen(obj2_fullpath) > 255)
                {
                    printf("The parallel diff only supports object names up to 255 characters\n");
                    MPI_Abort(MPI_COMM_WORLD, 0);
                } /* end if */

                HDstrcpy(args.name1, obj1_fullpath);
                HDstrcpy(args.name2, obj2_fullpath);
                args.options = *options;
                args.type = table->objs[i].type;

                h5diffdebug2("busyTasks=%d\n", busyTasks);
                /* if there are any outstanding print requests, let's handle one. */
                if(busyTasks > 0)
                {
                    int incomingMessage;

                    /* check if any tasks freed up, and didn't need to print. */
                    MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &incomingMessage, &Status);

                    /* first block*/
                    if(incomingMessage)
                    {
                        workerTasks[Status.MPI_SOURCE - 1] = 1;
                        MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                        nfound += nFoundbyWorker.nfound;
                        options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                        busyTasks--;
                    } /* end if */

                    /* check to see if the print token was returned. */
                    if(!havePrintToken)
                    {
                        /* If we don't have the token, someone is probably sending us output */
                        print_incoming_data();

                        /* check incoming queue for token */
                        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

                        /* incoming token implies free task. */
                        if(incomingMessage) {
                            workerTasks[Status.MPI_SOURCE - 1] = 1;
                            MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                            nfound += nFoundbyWorker.nfound;
                            options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                            busyTasks--;
                            havePrintToken = 1;
                        } /* end if */
                    } /* end if */

                    /* check to see if anyone needs the print token. */
                    if(havePrintToken)
                    {
                        /* check incoming queue for print token requests */
                        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &incomingMessage, &Status);
                        if(incomingMessage)
                        {
                            MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &Status);
                            MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);
                            havePrintToken = 0;
                        } /* end if */
                    } /* end if */
                } /* end if */

                /* check array of tasks to see which ones are free.
                * Manager task never does work, so freeTasks[0] is really
                * worker task 0. */
                for(n = 1; (n < g_nTasks) && !workerFound; n++)
                {
                    if(workerTasks[n-1])
                    {
                        /* send file id's and names to first free worker */
                        MPI_Send(&args, sizeof(args), MPI_BYTE, n, MPI_TAG_ARGS, MPI_COMM_WORLD);

                        /* increment counter for total number of prints. */
                        busyTasks++;

                        /* mark worker as busy */
                        workerTasks[n - 1] = 0;
                        workerFound = 1;
                    } /* end if */
                } /* end for */

                h5diffdebug2("workerfound is %d \n", workerFound);
                if(!workerFound)
                {
                    /* if they were all busy, we've got to wait for one free up
                     *  before we can move on.  If we don't have the token, some
                     * task is currently printing so we'll wait for that task to
                     * return it.
                     */

                    if(!havePrintToken)
                    {
                        while(!havePrintToken)
                        {
                            int incomingMessage;

                            print_incoming_data();
                            MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);
                            if(incomingMessage)
                            {
                                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                                havePrintToken = 1;
                                nfound += nFoundbyWorker.nfound;
                                options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                                /* send this task the work unit. */
                                MPI_Send(&args, sizeof(args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS, MPI_COMM_WORLD);
                            } /* end if */
                        } /* end while */
                    } /* end if */
                    /* if we do have the token, check for task to free up, or wait for a task to request it */
                    else
                    {
                        /* But first print all the data in our incoming queue */
                        print_incoming_data();
                        MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
                        if(Status.MPI_TAG == MPI_TAG_DONE)
                        {
                            MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                            nfound += nFoundbyWorker.nfound;
                            options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                            MPI_Send(&args, sizeof(args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS, MPI_COMM_WORLD);
                        } /* end if */
                        else if(Status.MPI_TAG == MPI_TAG_TOK_REQUEST)
                        {
                            int incomingMessage;

                            MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &Status);
                            MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);

                            do
                            {
                                MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

                                print_incoming_data();
                            } while(!incomingMessage);

                            MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                            nfound += nFoundbyWorker.nfound;
                            options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                            MPI_Send(&args, sizeof(args), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_ARGS, MPI_COMM_WORLD);
                        } /* end else-if */
                        else
                        {
                            printf("ERROR: Invalid tag (%d) received \n", Status.MPI_TAG);
                            MPI_Abort(MPI_COMM_WORLD, 0);
                            MPI_Finalize();
                        } /* end else */
                    } /* end else */
                } /* end if */
            } /* end else */
#endif /* H5_HAVE_PARALLEL */
            if (obj1_fullpath)
                HDfree (obj1_fullpath);
            if (obj2_fullpath)                
                HDfree (obj2_fullpath);
        } /* end if */
    } /* end for */
    h5diffdebug("done with for loop\n");

#ifdef H5_HAVE_PARALLEL
    if(g_Parallel)
    {
        /* make sure all tasks are done */
        while(busyTasks > 0)
        {
            MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
            if(Status.MPI_TAG == MPI_TAG_DONE)
            {
                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                nfound += nFoundbyWorker.nfound;
                options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                busyTasks--;
            } /* end if */
            else if(Status.MPI_TAG == MPI_TAG_TOK_RETURN)
            {
                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_DONE, MPI_COMM_WORLD, &Status);
                nfound += nFoundbyWorker.nfound;
                options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                busyTasks--;
                havePrintToken = 1;
            } /* end else-if */
            else if(Status.MPI_TAG == MPI_TAG_TOK_REQUEST)
            {
                MPI_Recv(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD, &Status);
                if(havePrintToken)
                {
                    int incomingMessage;

                    MPI_Send(NULL, 0, MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);

                    do {
                        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

                        print_incoming_data();
                    } while(!incomingMessage);

                    MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                    nfound += nFoundbyWorker.nfound;
                    options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                    busyTasks--;
                } /* end if */
                /* someone else must have it...wait for them to return it, then give it to the task that just asked for it. */
                else
                {
                    int source = Status.MPI_SOURCE;
                    int incomingMessage;

                    do
                    {
                        MPI_Iprobe(MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &incomingMessage, &Status);

                        print_incoming_data();
                    } while(!incomingMessage);


                    MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, MPI_ANY_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                    nfound += nFoundbyWorker.nfound;
                    options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                    busyTasks--;
                    MPI_Send(NULL, 0, MPI_BYTE, source, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD);
                } /* end else */
            } /* end else-if */
            else if(Status.MPI_TAG == MPI_TAG_TOK_RETURN)
            {
                MPI_Recv(&nFoundbyWorker, sizeof(nFoundbyWorker), MPI_BYTE, Status.MPI_SOURCE, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD, &Status);
                nfound += nFoundbyWorker.nfound;
                options->not_cmp = options->not_cmp | nFoundbyWorker.not_cmp;
                busyTasks--;
                havePrintToken = 1;
            } /* end else-if */
            else if(Status.MPI_TAG == MPI_TAG_PRINT_DATA)
            {
                char  data[PRINT_DATA_MAX_SIZE + 1];
                HDmemset(data, 0, PRINT_DATA_MAX_SIZE + 1);

                MPI_Recv(data, PRINT_DATA_MAX_SIZE, MPI_CHAR, Status.MPI_SOURCE, MPI_TAG_PRINT_DATA, MPI_COMM_WORLD, &Status);

                printf("%s", data);
            } /* end else-if */
            else
            {
                printf("ph5diff-manager: ERROR!! Invalid tag (%d) received \n", Status.MPI_TAG);
                MPI_Abort(MPI_COMM_WORLD, 0);
            } /* end else */
        } /* end while */

        for(i = 1; i < g_nTasks; i++)
            MPI_Send(NULL, 0, MPI_BYTE, i, MPI_TAG_END, MPI_COMM_WORLD);

        /* Print any final data waiting in our queue */
        print_incoming_data();
    } /* end if */
    h5diffdebug("done with if block\n");

    free(workerTasks);
    }
#endif /* H5_HAVE_PARALLEL */

    /* free table */
    if (table)
        trav_table_free(table);

    return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff_compare
 *
 * Purpose: get objects from list, and check for the same type
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 * Date: May 9, 2003
 *
 * Programmer: Jonathan Kim
 *  - add following links feature (Feb 11,2010)
 *-------------------------------------------------------------------------
 */

hsize_t diff_compare(hid_t file1_id,
                     const char *file1_name,
                     const char *obj1_name,
                     trav_info_t *info1,
                     hid_t file2_id,
                     const char *file2_name,
                     const char *obj2_name,
                     trav_info_t *info2,
                     diff_opt_t *options)
{
    int     f1 = 0;
    int     f2 = 0;
    hsize_t nfound = 0;
    ssize_t i,j;
    int l_ret;
    int is_dangle_link1 = 0;
    int is_dangle_link2 = 0;
    const char *obj1name = obj1_name;
    const char *obj2name = obj2_name;

    /* local variables for diff() */
    h5trav_type_t obj1type, obj2type;

    /* to get link info */
    h5tool_link_info_t linkinfo1;
    h5tool_link_info_t linkinfo2;

    /* init link info struct */
    HDmemset(&linkinfo1, 0, sizeof(h5tool_link_info_t));
    HDmemset(&linkinfo2, 0, sizeof(h5tool_link_info_t));

    i = h5trav_getindex (info1, obj1name);
    j = h5trav_getindex (info2, obj2name);

    if (i == -1)
    {
        parallel_print ("Object <%s> could not be found in <%s>\n", obj1name,
            file1_name);
        f1 = 1;
    }
    if (j == -1)
    {
        parallel_print ("Object <%s> could not be found in <%s>\n", obj2name,
            file2_name);
        f2 = 1;
    }
    if (f1 || f2)
    {
        options->err_stat = 1;
        return 0;
    }
    /* use the name with "/" first, as obtained by iterator function */
    obj1name = info1->paths[i].path;
    obj2name = info2->paths[j].path;

    obj1type = info1->paths[i].type;
    obj2type = info2->paths[j].type;

    /*-----------------------------------------------------------------
     * follow link option, compare with target object 
    */
    if (options->follow_links)
    {
        /* pass how to handle printing warning to linkinfo option */
        if(print_warn(options))
            linkinfo1.opt.msg_mode = linkinfo2.opt.msg_mode = 1;

        /*------------------------------------------------------------
         * Soft links
         *------------------------------------------------------------*/

        /*--------------------------
         * if object1 soft link   */
        if (obj1type == H5TRAV_TYPE_LINK)
        {
            /* get type of target object */
            l_ret = H5tools_get_symlink_info(file1_id, obj1name, &linkinfo1, TRUE);
            /* dangling link */
            if (l_ret == 0)
            {
                if (options->no_dangle_links)
                {
                    /* gangling link is error */
                    if(options->m_verbose)
                        parallel_print("Warning: <%s> is a dangling link.\n", obj1name);
                    options->err_stat = 1;
                    goto out;
                }
                else
                    is_dangle_link1 = 1;
            }
            /* fail */
            else if(l_ret < 0)
            {
                options->err_stat = 1;
                goto out;
            }
            else /* OK */
            {
                /* target type for diff() */
                obj1type = linkinfo1.trg_type;
            }
        }
        
        /*-----------------------------
         * if object2 is soft link   */
        if (obj2type == H5TRAV_TYPE_LINK)
        {
            /* get type target object */
            l_ret = H5tools_get_symlink_info(file2_id, obj2name, &linkinfo2, TRUE);
            /* dangling link */
            if (l_ret == 0)
            {
                if (options->no_dangle_links)
                {
                    /* gangling link is error */
                    if(options->m_verbose)
                        parallel_print("Warning: <%s> is a dangling link.\n", obj2name);
                    options->err_stat = 1;
                    goto out;
                }
                else
                    is_dangle_link2=1;
            }
            /* fail */
            else if(l_ret < 0)
            {
                options->err_stat = 1;
                goto out;
            }
            else /* OK */
            {
                /* target type for diff() */
                obj2type = linkinfo2.trg_type;
            }
        }

        /*------------------------------------------------------------
         * External links
         *------------------------------------------------------------*/

        /*--------------------------------
         * if object1 is external link  */
        if (obj1type == H5TRAV_TYPE_UDLINK)
        {
            /* get type and name of target object */
            l_ret = H5tools_get_symlink_info(file1_id, obj1name, &linkinfo1, TRUE);
            /* dangling link */
            if (l_ret == 0)
            {
                if (options->no_dangle_links)
                {
                    /* gangling link is error */
                    if(options->m_verbose)
                        parallel_print("Warning: <%s> is a dangling link.\n", obj1name);
                    options->err_stat = 1;
                    goto out;
                }
                else
                    is_dangle_link1 = 1;
            }
            /* fail */
            else if(l_ret < 0)
            {
                options->err_stat = 1;
                goto out;
            }
            else /* OK */
            {
                /* for external link */
                if(linkinfo1.linfo.type == H5L_TYPE_EXTERNAL)
                    obj1type = linkinfo1.trg_type;
            }
        }

        /*--------------------------------
         * if object2 is external link  */
        if (obj2type == H5TRAV_TYPE_UDLINK)
        {
            /* get type and name of target object */
            l_ret = H5tools_get_symlink_info(file2_id, obj2name, &linkinfo2, TRUE);
            /* dangling link */
            if (l_ret == 0)
            {
                if (options->no_dangle_links)
                {
                    /* gangling link is error */
                    if(options->m_verbose)
                        parallel_print("Warning: <%s> is a dangling link.\n", obj2name);
                    options->err_stat = 1;
                    goto out;
                }
                else
                    is_dangle_link2 = 1;
            }
            /* fail */
            else if(l_ret < 0)
            {
                options->err_stat = 1;
                goto out;
            }
            else /* OK */
            {
                /* for external link */
                if(linkinfo2.linfo.type == H5L_TYPE_EXTERNAL)
                    obj2type = linkinfo2.trg_type;
            }
        }
        /* found dangling link */
        if (is_dangle_link1 || is_dangle_link2)
            goto out;
    } /* end of follow_links */
    
    /* objects are not the same type */
    if (obj1type != obj2type)
    {
        if (options->m_verbose||options->m_list_not_cmp)
        {
            parallel_print("<%s> is of type %s and <%s> is of type %s\n",
            obj1name, get_type(obj1type), 
            obj2name, get_type(obj2type));
        }
        options->not_cmp=1;
        goto out;
    }

    nfound = diff(file1_id, obj1name,
                  file2_id, obj2name,
                  options, obj1type);

out:
    /*-------------------------------
     * handle dangling link(s) */
    /* both obj1 and obj2 are dangling links */
    if(is_dangle_link1 && is_dangle_link2)
    {
        if(print_objname(options, nfound))
        {
            do_print_objname("dangling link", obj1name, obj2name, options);
            print_found(nfound);
        }
    }
    /* obj1 is dangling link */
    else if (is_dangle_link1)
    {
        if(options->m_verbose)
           parallel_print("obj1 <%s> is a dangling link.\n", obj1name);
        nfound++;
        if(print_objname(options, nfound))
            print_found(nfound);
    }
    /* obj2 is dangling link */
    else if (is_dangle_link2)
    {
        if(options->m_verbose)
            parallel_print("obj2 <%s> is a dangling link.\n", obj2name);
        nfound++;
        if(print_objname(options, nfound))
            print_found(nfound);
    }

    /* free link info buffer */
    if (linkinfo1.trg_path)
        HDfree(linkinfo1.trg_path);
    if (linkinfo2.trg_path)
        HDfree(linkinfo2.trg_path);

    return nfound;
}


/*-------------------------------------------------------------------------
 * Function: diff
 *
 * Purpose: switch between types and choose the diff function
 * TYPE is either
 *  H5G_GROUP         Object is a group
 *  H5G_DATASET       Object is a dataset
 *  H5G_TYPE          Object is a named data type
 *  H5G_LINK          Object is a symbolic link
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 * Date: May 9, 2003
 *
 * Programmer: Jonathan Kim
 *  - add following links feature (Feb 11,2010)
 *-------------------------------------------------------------------------
 */

hsize_t diff(hid_t file1_id,
              const char *path1,
              hid_t file2_id,
              const char *path2,
              diff_opt_t * options,
              h5trav_type_t type)
{
    hid_t   type1_id = (-1);
    hid_t   type2_id = (-1);
    hid_t   grp1_id = (-1);
    hid_t   grp2_id = (-1);
    int     ret;
    int     is_dangle_link1 = 0;
    int     is_dangle_link2 = 0;
    hsize_t nfound = 0;


    /* to get link info */
    h5tool_link_info_t linkinfo1;
    h5tool_link_info_t linkinfo2;

    /*init link info struct */
    HDmemset(&linkinfo1,0,sizeof(h5tool_link_info_t));
    HDmemset(&linkinfo2,0,sizeof(h5tool_link_info_t));

    /* pass how to handle printing warnings to linkinfo option */
    if(print_warn(options))
        linkinfo1.opt.msg_mode = linkinfo2.opt.msg_mode = 1;

    /* 
     * Get target object info for obj1 and obj2 and check dangling links.
     * (for hard-linked-objects, because diff() only get the obj1's type, 
     *  so obj2's type should be check here when diff() is called from 
     *  diff_match() for same-named objects with dangling link only one side.)
     */

    /* target object1 - get type and name */
    ret = H5tools_get_symlink_info(file1_id, path1, &linkinfo1, TRUE);
    /* dangling link */
    if (ret == 0)
    {
        if (options->no_dangle_links)
        {
            /* gangling link is error */
            if(options->m_verbose)
                parallel_print("Warning: <%s> is a dangling link.\n", path1);
            goto out;
        }
        else
            is_dangle_link1 = 1;
    }
    else if (ret < 0)
        goto out;

    /* target object2 - get type and name */
    ret = H5tools_get_symlink_info(file2_id, path2, &linkinfo2, TRUE);
    /* dangling link */
    if (ret == 0)
    {
        if (options->no_dangle_links)
        {
            /* gangling link is error */
            if(options->m_verbose)
                parallel_print("Warning: <%s> is a dangling link.\n", path2);
            goto out;
        }
        else
            is_dangle_link2 = 1;
    }
    else if (ret < 0)
        goto out;
                
    /* found dangling link */
    if (is_dangle_link1 || is_dangle_link2)
        goto out2;

    switch(type)
    {
       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_DATASET
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_DATASET:
			/* verbose (-v) and report (-r) mode */
            if(options->m_verbose || options->m_report)
            {
                do_print_objname("dataset", path1, path2, options);
                nfound = diff_dataset(file1_id, file2_id, path1, path2, options);
                print_found(nfound);
            }
            /* quiet mode (-q), just count differences */
            else if(options->m_quiet)
            {
                nfound = diff_dataset(file1_id, file2_id, path1, path2, options);
            }
			/* the rest (-c, none, ...) */
            else
            {
                nfound = diff_dataset(file1_id, file2_id, path1, path2, options);
                /* print info if compatible and difference found  */
                if (!options->not_cmp && nfound)
                {
                    do_print_objname("dataset", path1, path2, options);
                    print_found(nfound);	
                }
            }
            break;

       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_NAMED_DATATYPE
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_NAMED_DATATYPE:
            if((type1_id = H5Topen2(file1_id, path1, H5P_DEFAULT)) < 0)
                goto out;
            if((type2_id = H5Topen2(file2_id, path2, H5P_DEFAULT)) < 0)
                goto out;

            if((ret = H5Tequal(type1_id, type2_id)) < 0)
                goto out;

            /* if H5Tequal is > 0 then the datatypes refer to the same datatype */
            nfound = (ret > 0) ? 0 : 1;

            if(print_objname(options,nfound))
                do_print_objname("datatype", path1, path2, options);

            /* always print the number of differences found in verbose mode */
            if(options->m_verbose)
                print_found(nfound);

            /*-----------------------------------------------------------------
             * compare attributes
             * the if condition refers to cases when the dataset is a 
             * referenced object
             *-----------------------------------------------------------------
             */
            if(path1)
                nfound += diff_attr(type1_id, type2_id, path1, path2, options);

            if(H5Tclose(type1_id) < 0)
                goto out;
            if(H5Tclose(type2_id) < 0)
                goto out;
            break;

       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_GROUP
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_GROUP:
            if(print_objname(options, nfound))
                do_print_objname("group", path1, path2, options);

            /* always print the number of differences found in verbose mode */
            if(options->m_verbose)
                print_found(nfound);

            if((grp1_id = H5Gopen2(file1_id, path1, H5P_DEFAULT)) < 0)
                goto out;
            if((grp2_id = H5Gopen2(file2_id, path2, H5P_DEFAULT)) < 0)
                goto out;

            /*-----------------------------------------------------------------
             * compare attributes
             * the if condition refers to cases when the dataset is a 
             * referenced object
             *-----------------------------------------------------------------
             */
            if(path1)
                nfound += diff_attr(grp1_id, grp2_id, path1, path2, options);

            if(H5Gclose(grp1_id) < 0)
                goto out;
            if(H5Gclose(grp2_id) < 0)
                goto out;
            break;


       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_LINK
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_LINK:
            {
            ret = HDstrcmp(linkinfo1.trg_path, linkinfo2.trg_path);

            /* if the target link name is not same then the links are "different" */
            nfound = (ret != 0) ? 1 : 0;

            if(print_objname(options, nfound))
                do_print_objname("link", path1, path2, options);

            if (options->follow_links)
            {
                /* objects are not the same type */
                if (linkinfo1.trg_type != linkinfo2.trg_type)
                {
                    if (options->m_verbose||options->m_list_not_cmp)
                    {
                        parallel_print("<%s> is of type %s and <%s> is of type %s\n", path1, get_type(linkinfo1.trg_type), path2, get_type(linkinfo2.trg_type));
                    }
                    options->not_cmp=1;
                    goto out;
                }

                /* call self to compare target object */
                nfound += diff(file1_id, path1, 
                               file2_id, path2, 
                               options, linkinfo1.trg_type);
            }

            /* always print the number of differences found in verbose mode */
            if(options->m_verbose)
                print_found(nfound);

            }
            break;

       /*----------------------------------------------------------------------
        * H5TRAV_TYPE_UDLINK
        *----------------------------------------------------------------------
        */
        case H5TRAV_TYPE_UDLINK:
            {
            /* Only external links will have a query function registered */
            if(linkinfo1.linfo.type == H5L_TYPE_EXTERNAL && linkinfo2.linfo.type == H5L_TYPE_EXTERNAL) 
            {
                /* If the buffers are the same size, compare them */
                if(linkinfo1.linfo.u.val_size == linkinfo2.linfo.u.val_size) 
                {
                    ret = HDmemcmp(linkinfo1.trg_path, linkinfo2.trg_path, linkinfo1.linfo.u.val_size);
                }
                else
                    ret = 1;

                /* if "linkinfo1.trg_path" != "linkinfo2.trg_path" then the links
                 * are "different" extlinkinfo#.path is combination string of 
                 * file_name and obj_name
                 */
                nfound = (ret != 0) ? 1 : 0;

                if(print_objname(options, nfound))
                    do_print_objname("external link", path1, path2, options);

                if (options->follow_links)
                {
                    /* objects are not the same type */
                    if (linkinfo1.trg_type != linkinfo2.trg_type)
                    {
                        if (options->m_verbose||options->m_list_not_cmp)
                        {
                            parallel_print("<%s> is of type %s and <%s> is of type %s\n", path1, get_type(linkinfo1.trg_type), path2, get_type(linkinfo2.trg_type));
                        }
                        options->not_cmp=1;
                        goto out;
                    }

                    nfound = diff(file1_id, path1,  
                                  file2_id, path2, 
                                  options, linkinfo1.trg_type);
                } 
            } /* end if */
            else 
            {
                /* If one or both of these links isn't an external link, we can only
                 * compare information from H5Lget_info since we don't have a query
                 * function registered for them.
                 *
                 * If the link classes or the buffer length are not the
                 * same, the links are "different"
                 */
                if((linkinfo1.linfo.type != linkinfo2.linfo.type) || 
                   (linkinfo1.linfo.u.val_size != linkinfo2.linfo.u.val_size))
                    nfound = 1;
                else
                    nfound = 0;

                if (print_objname (options, nfound))
                    do_print_objname ("user defined link", path1, path2, options);
            } /* end else */

            /* always print the number of differences found in verbose mode */
            if(options->m_verbose)
                print_found(nfound);
            }
            break;

        default:
            if(options->m_verbose)
                parallel_print("Comparison not supported: <%s> and <%s> are of type %s\n",
                    path1, path2, get_type(type) );
            options->not_cmp = 1;
            break;
     }

    /* free link info buffer */
    if (linkinfo1.trg_path)
        HDfree(linkinfo1.trg_path);
    if (linkinfo2.trg_path)
        HDfree(linkinfo2.trg_path);

    return nfound;

out:
    options->err_stat = 1;

out2:
    /*-----------------------------------
     * handle dangling link(s) 
     */
    /* both path1 and path2 are dangling links */
    if(is_dangle_link1 && is_dangle_link2)
    {
        if(print_objname(options, nfound))
        {
            do_print_objname("dangling link", path1, path2, options);
            print_found(nfound);
        }
    }
    /* path1 is dangling link */
    else if (is_dangle_link1)
    {
        if(options->m_verbose)
           parallel_print("obj1 <%s> is a dangling link.\n", path1);
        nfound++;
        if(print_objname(options, nfound))
            print_found(nfound);
    }
    /* path2 is dangling link */
    else if (is_dangle_link2)
    {
        if(options->m_verbose)
            parallel_print("obj2 <%s> is a dangling link.\n", path2);
        nfound++;
        if(print_objname(options, nfound))
            print_found(nfound);
    }

    /* free link info buffer */
    if (linkinfo1.trg_path)
        HDfree(linkinfo1.trg_path);
    if (linkinfo2.trg_path)
        HDfree(linkinfo2.trg_path);

    /* close */
    /* disable error reporting */
    H5E_BEGIN_TRY {
        H5Tclose(type1_id);
        H5Tclose(type2_id);
        H5Gclose(grp1_id);
        H5Tclose(grp2_id);
        /* enable error reporting */
    } H5E_END_TRY;

    return nfound;
}

