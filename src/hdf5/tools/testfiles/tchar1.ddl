#############################
Expected output for 'h5dump -r tchar.h5'
#############################
HDF5 "tchar.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_STD_I8LE
      DATASPACE  SIMPLE { ( 308 ) / ( 308 ) }
      DATA {
         "Four score and seven years ago our forefathers brought forth on this "
         "continent a new nation, conceived in liberty and dedicated to the pro"
         "position that all men are created equal. Now we are engaged in a grea"
         "t civil war, testing whether that nation or any nation so conceived a"
         "nd so dedicated can long endure."
      }
   }
}
}
