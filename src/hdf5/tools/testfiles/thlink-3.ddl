#############################
Expected output for 'h5dump -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5'
#############################
HDF5 "thlink.h5" {
DATASET "/g1/g1.1/dset3" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
   DATA {
   (0): 0, 1, 2, 3, 4
   }
}
DATASET "/g1/dset2" {
   HARDLINK "/dset1"
}
DATASET "/dset1" {
   HARDLINK "/dset1"
}
}
