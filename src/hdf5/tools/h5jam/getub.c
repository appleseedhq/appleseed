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

#include <stdio.h>
#include <fcntl.h>

#ifdef H5_HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "H5private.h"
#include "h5tools_utils.h"

void parse_command_line (int argc, const char *argv[]);

#define TRUE 1
#define FALSE 0

/* Name of tool */
#define PROGRAMNAME "getub"
char *nbytes = NULL;

static const char *s_opts = "c:";	/* add more later ? */
static struct long_options l_opts[] = {
  {"c", require_arg, 'c'},	/* input file */
  {NULL, 0, '\0'}
};

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message
 *
 * Return:      void
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (const char *prog)
{
  fflush (stdout);
  fprintf (stdout, "usage: %s -c nb file] \n", prog);
  fprintf (stdout, "           print first 'nb' byts of file to stdoug.\n");
}

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parse the command line for the h5dumper.
 *
 * Return:      Success:
 *
 *              Failure:    Exits program with EXIT_FAILURE value.
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
parse_command_line (int argc, const char *argv[])
{
  int opt = FALSE;

  /* parse command line options */
  while ((opt = get_option (argc, argv, s_opts, l_opts)) != EOF)
    {
      switch ((char) opt)
	{
	case 'c':
	  nbytes = HDstrdup (opt_arg);
	  break;
	case '?':
	default:
	  usage (h5tools_getprogname());
	  exit (EXIT_FAILURE);
	}
    }

  if (argc <= opt_ind)
    {
      error_msg("missing file name\n");
      usage (h5tools_getprogname());
      exit (EXIT_FAILURE);
    }
}

int
main (int argc, const char *argv[])
{
  int fd;
  unsigned int size;
  char *filename;
  long res;
  char *buf;

  h5tools_setprogname(PROGRAMNAME);
  h5tools_setstatus(EXIT_SUCCESS);

  parse_command_line (argc, argv);

  if (nbytes == NULL)
    {
      /* missing arg */
      error_msg("missing size\n");
      usage (h5tools_getprogname());
      exit (EXIT_FAILURE);
    }
  if (argc <= (opt_ind))
    {
      error_msg("missing file name\n");
      usage (h5tools_getprogname());
      exit (EXIT_FAILURE);
    }
  filename = HDstrdup (argv[opt_ind]);

  size = 0;
  res = sscanf (nbytes, "%u", &size);
  if (res == EOF)
    {
      /* fail */
      error_msg("missing file name\n");
      usage (h5tools_getprogname());
      exit (EXIT_FAILURE);
    }

  fd = HDopen (filename, O_RDONLY, 0);
  if (fd < 0)
    {
      error_msg("can't open file %s\n", filename);
      exit (EXIT_FAILURE);
    }

  buf = malloc ((unsigned)(size + 1));
  if (buf == NULL)
    {
      HDclose (fd);
      exit (EXIT_FAILURE);
    }

  res = HDread (fd, buf, (unsigned)size);

  if (res < (long)size)
    {
      if (buf)
	free (buf);
      HDclose (fd);
      exit (EXIT_FAILURE);
    }

  HDwrite (1, buf, (unsigned)size);

  if (buf)
    free (buf);
  HDclose (fd);
  return (EXIT_SUCCESS);
}
