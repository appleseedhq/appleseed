#############################
Expected output for 'h5dump -n tfcontents1.h5'
#############################
HDF5 "tfcontents1.h5" {
FILE_CONTENTS {
 datatype   /#5616
 group      /
 dataset    /dset
 dataset    /dset3 -> /dset
 dataset    /dset4 -> /dset
 dataset    /dsetmytype2
 ext link   /extlink -> fname oname
 group      /g1
 dataset    /g1/dset1 -> /dset
 group      /g1/g1.1
 dataset    /g1/g1.1/dset2 -> /dset
 group      /g2 -> /g1/g1.1
 link       /mylink -> mylink
 datatype   /mytype
 link       /softlink -> /dset
 unknown type of UD link /udlink -> ???
 }
}
