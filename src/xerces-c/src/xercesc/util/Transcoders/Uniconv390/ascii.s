*
* Licensed to the Apache Software Foundation (ASF) under one or more
* contributor license agreements.  See the NOTICE file distributed with
* this work for additional information regarding copyright ownership.
* The ASF licenses this file to You under the Apache License, Version 2.0
* (the "License"); you may not use this file except in compliance with
* the License.  You may obtain a copy of the License at
* 
*      http://www.apache.org/licenses/LICENSE-2.0
* 
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
* implied. See the License for the specific language governing
* permissions and limitations under the License.
*

*
* $Id: ascii.s 568078 2007-08-21 11:43:25Z amassari $
*

TROTASC  CSECT
TROTASC  RMODE ANY
         USING   *,15
         STM     14,12,12(13)  Save register content
         USING   PARMS,1       R1 = parameters
         L       6,INPUT       R6 = input data
         L       4,LENGTH@     
         L       10,LENGTH@    R10 = @ of length
         L       9,0(4)        R9 = length of input data
         L       4,STOP
         L       0,0(4)        R0 = Stop Character
         LR      4,6           Store input data @ in R4
         LA      5,1
         L       8,OUTPUT      R8 = output data
         L       11,FLAG       R11 = @ of FLAG
         L       1,TBL         R1 = Translate table @
RETR     TROT    8,6   Translate input pointed to output buffer pointed
         BO      RETR          Not done yet
         BZ      END           Branch to END if done
         ST      5,0(11)       FLAG = 1 none-ASCII character found
         B       END           Branch to END if Stop Char found
END      SR      6,4           Number of characters processed
         ST      6,0(10)       Store number of characters to Length
         L       14,12(,13)    Restore the registers
         LM      2,12,28(13)
         BALR    1,14          Return
PARMS    DSECT
INPUT    DS A
OUTPUT   DS A
LENGTH@  DS A
TBL      DS A
STOP     DS A
FLAG     DS A
   END
