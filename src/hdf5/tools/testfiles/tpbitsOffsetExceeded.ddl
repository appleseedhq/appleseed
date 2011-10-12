#############################
Expected output for 'h5dump -d /DS08BITS -M 64,1 packedbits.h5'
#############################
usage: h5dump [OPTIONS] file
  OPTIONS
     -h, --help           Print a usage message and exit
     -n, --contents       Print a list of the file contents and exit
     -B, --superblock     Print the content of the super block
     -H, --header         Print the header only; no data is displayed
     -A, --onlyattr       Print the header and value of attributes
     -i, --object-ids     Print the object ids
     -r, --string         Print 1-byte integer datasets as ASCII
     -e, --escape         Escape non printing characters
     -V, --version        Print version number and exit
     -a P, --attribute=P  Print the specified attribute
     -d P, --dataset=P    Print the specified dataset
     -y, --noindex        Do not print array indices with the data
     -p, --properties     Print dataset filters, storage layout and fill value
     -f D, --filedriver=D Specify which driver to open the file with
     -g P, --group=P      Print the specified group and all members
     -l P, --soft-link=P  Print the value(s) of the specified soft link
     -o F, --output=F     Output raw data into file F
     -b B, --binary=B     Binary file output, of form B
     -t P, --datatype=P   Print the specified named datatype
     -w N, --width=N      Set the number of columns of output. A value of 0 (zero)
                          sets the number of columns to the maximum (65535).
                          Default width is 80 columns.
     -m T, --format=T     Set the floating point output format
     -q Q, --sort_by=Q    Sort groups and attributes by index Q
     -z Z, --sort_order=Z Sort groups and attributes by order Z
     -M L, --packedbits=L Print packed bits as unsigned integers, using mask
                          format L for an integer dataset specified with
                          option -d. L is a list of offset,length values,
                          separated by commas. Offset is the beginning bit in
                          the data value and length is the number of bits of
                          the mask.
     -R, --region         Print dataset pointed by region references
     -x, --xml            Output in XML using Schema
     -u, --use-dtd        Output in XML using DTD
     -D U, --xml-dtd=U    Use the DTD or schema at U
     -X S, --xml-ns=S      (XML Schema) Use qualified names n the XML
                          ":": no namespace, default: "hdf5:"
                          E.g., to dump a file called `-f', use h5dump -- -f
     --enable-error-stack Prints messages from the HDF5 error stack as they
                          occur.

 Subsetting is available by using the following options with a dataset
 attribute. Subsetting is done by selecting a hyperslab from the data.
 Thus, the options mirror those for performing a hyperslab selection.
 One of the START, COUNT, STRIDE, or BLOCK parameters are mandatory if you do subsetting.
 The STRIDE, COUNT, and BLOCK parameters are optional and will default to 1 in
 each dimension. START is optional and will default to 0 in each dimension.

      -s START,  --start=START    Offset of start of subsetting selection
      -S STRIDE, --stride=STRIDE  Hyperslab stride
      -c COUNT,  --count=COUNT    Number of blocks to include in selection
      -k BLOCK,  --block=BLOCK    Size of block in hyperslab
  START, COUNT, STRIDE, and BLOCK - is a list of integers the number of which are equal to the
        number of dimensions in the dataspace being queried

  D - is the file driver to use in opening the file. Acceptable values
        are "sec2", "family", "split", "multi", "direct", and "stream". Without
        the file driver flag, the file will be opened with each driver in
        turn and in the order specified above until one driver succeeds
        in opening the file.
  F - is a filename.
  P - is the full path from the root group to the object.
  N - is an integer greater than 1.
  T - is a string containing the floating point format, e.g '%.3f'
  U - is a URI reference (as defined in [IETF RFC 2396],
        updated by [IETF RFC 2732])
  B - is the form of binary output: NATIVE for a memory type, FILE for the
        file type, LE or BE for pre-existing little or big endian types.
        Must be used with -o (output file) and it is recommended that
        -d (dataset) is used. B is an optional argument, defaults to NATIVE
  Q - is the sort index type. It can be "creation_order" or "name" (default)
  Z - is the sort order type. It can be "descending" or "ascending" (default)

  Examples:

  1) Attribute foo of the group /bar_none in file quux.h5

     	h5dump -a /bar_none/foo quux.h5

  2) Selecting a subset from dataset /foo in file quux.h5

      h5dump -d /foo -s "0,1" -S "1,1" -c "2,3" -k "2,2" quux.h5

  3) Saving dataset 'dset' in file quux.h5 to binary file 'out.bin'
        using a little-endian type

      h5dump -d /dset -b LE -o out.bin quux.h5

  4) Display two packed bits (bits 0-1 and bits 4-6) in the dataset /dset

      h5dump -d /dset -M 0,1,4,3 quux.h5

h5dump error: Packed Bit offset value(64) must be between 0 and 63
