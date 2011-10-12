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


/* This file contains the definition of functions required to build h5perf in
 * STANDALONE mode.
 * Created: Christian Chilan, 2005/5/18.
 */

#include "pio_perf.h"


/** From h5tools_utils.c **/

/* global variables */
int   nCols = 80;

/* ``get_option'' variables */
int         opt_err = 1;    /*get_option prints errors if this is on */
int         opt_ind = 1;    /*token pointer                          */
const char *opt_arg;        /*flag argument (or value)               */


int
get_option(int argc, const char **argv, const char *opts, const struct long_options *l_opts)
{
    static int sp = 1;    /* character index in current token */
    int opt_opt = '?';    /* option character passed back to user */

    if (sp == 1) {
        /* check for more flag-like tokens */
        if (opt_ind >= argc || argv[opt_ind][0] != '-' || argv[opt_ind][1] == '\0') {
            return EOF;
        } else if (HDstrcmp(argv[opt_ind], "--") == 0) {
            opt_ind++;
            return EOF;
        }
    }

    if (sp == 1 && argv[opt_ind][0] == '-' && argv[opt_ind][1] == '-') {
        /* long command line option */
        const char *arg = &argv[opt_ind][2];
        int i;

        for (i = 0; l_opts && l_opts[i].name; i++) {
            size_t len = HDstrlen(l_opts[i].name);

            if (HDstrncmp(arg, l_opts[i].name, len) == 0) {
                /* we've found a matching long command line flag */
                opt_opt = l_opts[i].shortval;

                if (l_opts[i].has_arg != no_arg) {
                    if (arg[len] == '=') {
                        opt_arg = &arg[len + 1];
                    } else if (opt_ind < (argc - 1) && argv[opt_ind + 1][0] != '-') {
                        opt_arg = argv[++opt_ind];
                    } else if (l_opts[i].has_arg == require_arg) {
                        if (opt_err)
                            HDfprintf(stderr,
                                    "%s: option required for \"--%s\" flag\n",
                                    argv[0], arg);

                        opt_opt = '?';
                    }
                } else {
                    if (arg[len] == '=') {
                        if (opt_err)
                            HDfprintf(stderr,
                                    "%s: no option required for \"%s\" flag\n",
                                    argv[0], arg);

                        opt_opt = '?';
                    }

                    opt_arg = NULL;
                }

                break;
            }
        }

        if (l_opts[i].name == NULL) {
            /* exhausted all of the l_opts we have and still didn't match */
            if (opt_err)
                HDfprintf(stderr, "%s: unknown option \"%s\"\n", argv[0], arg);

            opt_opt = '?';
        }

        opt_ind++;
        sp = 1;
    } else {
        register char *cp;    /* pointer into current token */

        /* short command line option */
        opt_opt = argv[opt_ind][sp];

        if (opt_opt == ':' || (cp = strchr(opts, opt_opt)) == 0) {

            if (opt_err)
                HDfprintf(stderr, "%s: unknown option \"%c\"\n",
                        argv[0], opt_opt);

            /* if no chars left in this token, move to next token */
            if (argv[opt_ind][++sp] == '\0') {
                opt_ind++;
                sp = 1;
            }

            return '?';
        }

        if (*++cp == ':') {
            /* if a value is expected, get it */
            if (argv[opt_ind][sp + 1] != '\0') {
                /* flag value is rest of current token */
                opt_arg = &argv[opt_ind++][sp + 1];
            } else if (++opt_ind >= argc) {
                if (opt_err)
                    HDfprintf(stderr,
                            "%s: value expected for option \"%c\"\n",
                            argv[0], opt_opt);

                opt_opt = '?';
            } else {
                /* flag value is next token */
                opt_arg = argv[opt_ind++];
            }

            sp = 1;
        } else {
            /* set up to look at next char in token, next time */
            if (argv[opt_ind][++sp] == '\0') {
                /* no more in current token, so setup next token */
                opt_ind++;
                sp = 1;
            }

            opt_arg = NULL;
        }
    }

    /* return the current flag character found */
    return opt_opt;
}


void
print_version(const char *progname)
{
    printf("%s: Version %u.%u.%u%s%s\n",
           progname, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE,
           H5_VERS_SUBRELEASE[0] ? "-" : "", H5_VERS_SUBRELEASE);
}



/** From h5test.c **/

#ifdef H5_HAVE_PARALLEL
MPI_Info    h5_io_info_g=MPI_INFO_NULL;/* MPI INFO object for IO */
#endif

int
h5_set_info_object(void)
{
    char	*envp;			/* environment pointer */
    int		ret_value=0;

    /* handle any MPI INFO hints via $HDF5_MPI_INFO */
    if ((envp = getenv("HDF5_MPI_INFO")) != NULL){
        char *next, *valp;


        valp = envp = next = HDstrdup(envp);

        /* create an INFO object if not created yet */
        if (h5_io_info_g == MPI_INFO_NULL)
            MPI_Info_create(&h5_io_info_g);

        do {
            size_t len;
            char *key_val, *endp, *namep;

            if (*valp == ';')
                valp++;

            /* copy key/value pair into temporary buffer */
            len = strcspn(valp, ";");
            next = &valp[len];
            key_val = calloc(1, len + 1);

            /* increment the next pointer past the terminating semicolon */
            if (*next == ';')
                ++next;

            namep = HDstrncpy(key_val, valp, len);

            /* pass up any beginning whitespaces */
            while (*namep && (*namep == ' ' || *namep == '\t'))
                namep++;

            /* eat up any ending white spaces */
            endp = &namep[strlen(namep) - 1];

            while (endp && (*endp == ' ' || *endp == '\t'))
                *endp-- = '\0';

            /* find the '=' */

            valp = HDstrchr(namep, '=');

            if (valp != NULL) {     /* it's a valid key/value pairing */
                char *tmp_val = valp + 1;

                /* change '=' to \0, move valp down one */
                *valp-- = '\0';

                /* eat up ending whitespace on the "key" part */
                while (*valp == ' ' || *valp == '\t')
                    *valp-- = '\0';

                valp = tmp_val;

                /* eat up beginning whitespace on the "value" part */
                while (*valp == ' ' || *valp == '\t')
                    *valp++ = '\0';

                /* actually set the darned thing */
                if (MPI_SUCCESS != MPI_Info_set(h5_io_info_g, namep, valp)) {
                    printf("MPI_Info_set failed\n");
                    ret_value = -1;
                }
            }

            valp = next;
            HDfree(key_val);
        } while (next && *next);

        HDfree(envp);
    }

    return ret_value;
}


void
h5_dump_info_object(MPI_Info info)
{
    char	key[MPI_MAX_INFO_KEY+1];
    char	value[MPI_MAX_INFO_VAL+1];
    int  	flag;
    int		i, nkeys;

    printf("Dumping MPI Info Object(%d) (up to %d bytes per item):\n", (int)info,
	MPI_MAX_INFO_VAL);
    if (info==MPI_INFO_NULL){
	printf("object is MPI_INFO_NULL\n");
    }
    else {
	MPI_Info_get_nkeys(info, &nkeys);
	printf("object has %d items\n", nkeys);
	for (i=0; i<nkeys; i++){
	    MPI_Info_get_nthkey(info, i, key);
	    MPI_Info_get(info, key, MPI_MAX_INFO_VAL, value, &flag);
	    printf("%s=%s\n", key, value);
	}

    }
}
