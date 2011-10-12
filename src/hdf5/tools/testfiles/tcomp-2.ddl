#############################
Expected output for 'h5dump -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5'
#############################
HDF5 "tcompound.h5" {
DATATYPE "/type1" H5T_COMPOUND {
   H5T_STD_I32BE "int_name";
   H5T_IEEE_F32BE "float_name";
}
DATATYPE "/type2" H5T_COMPOUND {
   H5T_ARRAY { [4] H5T_STD_I32BE } "int_array";
   H5T_ARRAY { [5][6] H5T_IEEE_F32BE } "float_array";
}
DATATYPE "/group1/type3" H5T_COMPOUND {
   H5T_STD_I32BE "int";
   H5T_IEEE_F32BE "float";
}
}
