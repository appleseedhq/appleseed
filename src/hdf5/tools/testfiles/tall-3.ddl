#############################
Expected output for 'h5dump -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5'
#############################
HDF5 "tall.h5" {
DATASET "/g2/dset2.1" {
   DATATYPE  H5T_IEEE_F32BE
   DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
   DATA {
   (0): 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9
   }
}
SOFTLINK "/g1/g1.2/g1.2.1/slink" {
   LINKTARGET "somevalue"
}
}
