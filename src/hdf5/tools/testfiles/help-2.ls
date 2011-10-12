#############################
 output for 'h5ls -w80 -help'
#############################
usage: h5ls [OPTIONS] [OBJECTS...]
  OPTIONS
   -h, -?, --help  Print a usage message and exit
   -a, --address   Print raw data address.  If dataset is contiguous, address
                   is offset in file of beginning of raw data. If chunked,
                   returned list of addresses indicates offset of each chunk.
                   Must be used with -v, --verbose option.
                   Provides no information for non-dataset objects.
   -d, --data      Print the values of datasets
   --enable-error-stack
                   Prints messages from the HDF5 error stack as they occur.
   --follow-symlinks
                   Follow symbolic links (soft links and external links)
                   to display target object information.
                   Without this option, h5ls identifies a symbolic link
                   as a soft link or external link and prints the value
                   assigned to the symbolic link; it does not provide any
                   information regarding the target object or determine
                   whether the link is a dangling link.
   --no-dangling-links
                   Must be used with --follow-symlinks option;
                   otherwise, h5ls shows error message and returns an exit
                   code of 1. 
                   Check for any symbolic links (soft links or external links)
                   that do not resolve to an existing object (dataset, group,
                   or named datatype).
                   If any dangling link is found, this situation is treated
                   as an error and h5ls returns an exit code of 1.
   -f, --full      Print full path names instead of base names
   -g, --group     Show information about a group, not its contents
   -l, --label     Label members of compound datasets
   -r, --recursive List all groups recursively, avoiding cycles
   -s, --string    Print 1-byte integer datasets as ASCII
   -S, --simple    Use a machine-readable output format
   -wN, --width=N  Set the number of columns of output
   -v, --verbose   Generate more verbose output
   -V, --version   Print version number and exit
   --vfd=DRIVER    Use the specified virtual file driver
   -x, --hexdump   Show raw data in hexadecimal format

  OBJECTS
    Each object consists of an HDF5 file name optionally followed by a
    slash and an object name within the file (if no object is specified
    within the file then the contents of the root group are displayed).
    The file name may include a printf(3C) integer format such as
    "%05d" to open a file family.

  Deprecated Options
    The following options have been deprecated in HDF5. While they remain
    available, they have been superseded as indicated and may be removed
    from HDF5 in the future. Use the indicated replacement option in all
    new work; where possible, existing scripts, et cetera, should also be
    updated to use the replacement option.

   -E or --external   Follow external links.
                      Replaced by --follow-symlinks.
   -e, --errors       Show all HDF5 error reporting
                      Replaced by --enable-error-stack.
