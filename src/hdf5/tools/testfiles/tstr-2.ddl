#############################
Expected output for 'h5dump tstr2.h5'
#############################
HDF5 "tstr2.h5" {
GROUP "/" {
   GROUP "g1" {
      DATASET "dset1" {
         DATATYPE  H5T_STRING {
               STRSIZE 50;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
         DATA {
         (0): "This is row 0 of type H5T_STR_NULLTERM of",
         (1): "This is row 1 of type H5T_STR_NULLTERM of",
         (2): "This is row 2 of type H5T_STR_NULLTERM of",
         (3): "This is row 3 of type H5T_STR_NULLTERM of",
         (4): "This is row 4 of type H5T_STR_NULLTERM of",
         (5): "This is row 5 of type H5T_STR_NULLTERM of",
         (6): "This is row 6 of type H5T_STR_NULLTERM of",
         (7): "This is row 7 of type H5T_STR_NULLTERM of",
         (8): "This is row 8 of type H5T_STR_NULLTERM of",
         (9): "This is row 9 of type H5T_STR_NULLTERM of"
         }
         ATTRIBUTE "attr1" {
            DATATYPE  H5T_STRING {
                  STRSIZE 11;
                  STRPAD H5T_STR_NULLTERM;
                  CSET H5T_CSET_ASCII;
                  CTYPE H5T_C_S1;
               }
            DATASPACE  SIMPLE { ( 3 ) / ( 3 ) }
            DATA {
            (0): "0123456789", "abcdefghij", "ABCDEFGHIJ"
            }
         }
      }
   }
   GROUP "g2" {
      DATASET "dset2" {
         DATATYPE  H5T_STRING {
               STRSIZE 50;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
         DATA {
         (0): "This is row 0 of type H5T_STR_NULLTERM of string ",
         (1): "This is row 1 of type H5T_STR_NULLTERM of string ",
         (2): "This is row 2 of type H5T_STR_NULLTERM of string ",
         (3): "This is row 3 of type H5T_STR_NULLTERM of string ",
         (4): "This is row 4 of type H5T_STR_NULLTERM of string ",
         (5): "This is row 5 of type H5T_STR_NULLTERM of string ",
         (6): "This is row 6 of type H5T_STR_NULLTERM of string ",
         (7): "This is row 7 of type H5T_STR_NULLTERM of string ",
         (8): "This is row 8 of type H5T_STR_NULLTERM of string ",
         (9): "This is row 9 of type H5T_STR_NULLTERM of string "
         }
      }
   }
   GROUP "g3" {
      DATASET "dset3" {
         DATATYPE  H5T_STRING {
               STRSIZE 50;
               STRPAD H5T_STR_NULLPAD;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
         DATA {
         (0): "This is row 0 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000",
         (1): "This is row 1 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000",
         (2): "This is row 2 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000",
         (3): "This is row 3 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000",
         (4): "This is row 4 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000",
         (5): "This is row 5 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000",
         (6): "This is row 6 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000",
         (7): "This is row 7 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000",
         (8): "This is row 8 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000",
         (9): "This is row 9 of type H5T_STR_NULLPAD of\000\000\000\000\000\000\000\000\000\000"
         }
      }
   }
   GROUP "g4" {
      DATASET "dset4" {
         DATATYPE  H5T_STRING {
               STRSIZE 50;
               STRPAD H5T_STR_NULLPAD;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
         DATA {
         (0): "This is row 0 of type H5T_STR_NULLPAD of string ar",
         (1): "This is row 1 of type H5T_STR_NULLPAD of string ar",
         (2): "This is row 2 of type H5T_STR_NULLPAD of string ar",
         (3): "This is row 3 of type H5T_STR_NULLPAD of string ar",
         (4): "This is row 4 of type H5T_STR_NULLPAD of string ar",
         (5): "This is row 5 of type H5T_STR_NULLPAD of string ar",
         (6): "This is row 6 of type H5T_STR_NULLPAD of string ar",
         (7): "This is row 7 of type H5T_STR_NULLPAD of string ar",
         (8): "This is row 8 of type H5T_STR_NULLPAD of string ar",
         (9): "This is row 9 of type H5T_STR_NULLPAD of string ar"
         }
      }
   }
   GROUP "g5" {
      DATASET "dset5" {
         DATATYPE  H5T_STRING {
               STRSIZE 50;
               STRPAD H5T_STR_SPACEPAD;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
         DATA {
         (0): "This is row 0 of type H5T_STR_SPACEPAD of         ",
         (1): "This is row 1 of type H5T_STR_SPACEPAD of         ",
         (2): "This is row 2 of type H5T_STR_SPACEPAD of         ",
         (3): "This is row 3 of type H5T_STR_SPACEPAD of         ",
         (4): "This is row 4 of type H5T_STR_SPACEPAD of         ",
         (5): "This is row 5 of type H5T_STR_SPACEPAD of         ",
         (6): "This is row 6 of type H5T_STR_SPACEPAD of         ",
         (7): "This is row 7 of type H5T_STR_SPACEPAD of         ",
         (8): "This is row 8 of type H5T_STR_SPACEPAD of         ",
         (9): "This is row 9 of type H5T_STR_SPACEPAD of         "
         }
      }
   }
   GROUP "g6" {
      DATASET "dset6" {
         DATATYPE  H5T_STRING {
               STRSIZE 50;
               STRPAD H5T_STR_SPACEPAD;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
         DATA {
         (0): "This is row 0 of type H5T_STR_SPACEPAD of string a",
         (1): "This is row 1 of type H5T_STR_SPACEPAD of string a",
         (2): "This is row 2 of type H5T_STR_SPACEPAD of string a",
         (3): "This is row 3 of type H5T_STR_SPACEPAD of string a",
         (4): "This is row 4 of type H5T_STR_SPACEPAD of string a",
         (5): "This is row 5 of type H5T_STR_SPACEPAD of string a",
         (6): "This is row 6 of type H5T_STR_SPACEPAD of string a",
         (7): "This is row 7 of type H5T_STR_SPACEPAD of string a",
         (8): "This is row 8 of type H5T_STR_SPACEPAD of string a",
         (9): "This is row 9 of type H5T_STR_SPACEPAD of string a"
         }
      }
   }
}
}
