#############################
Expected output for 'h5dump -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5'
#############################
HDF5 "thlink.h5" {
DATASET "/g1/dset2" {
   DATATYPE  H5T_STD_I32BE
   DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
   DATA {
   (0): 0, 1, 2, 3, 4
   }
}
DATASET "/dset1" {
   HARDLINK "/dset1"
}
DATASET "/g1/g1.1/dset3" {
   HARDLINK "/dset1"
}
}
