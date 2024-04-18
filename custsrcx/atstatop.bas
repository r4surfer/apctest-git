        REM *************************************************************~
            *                                                           *~
            *  Program Name      - ATSTATOP                             *~
            *  Creation Date     - 06/11/2109                           *~
            *  Written By        - Ricky Beane                          *~
            *                                                           *~
            *  Description       - Open file to status check            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *************************************************************
        dim filename$8

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ATSTTSAD ! PlyGem Atlas Audit on Status Updates     *~
            *************************************************************~
            
               select #1, "ATSTTSAD",                                    ~
                        varc,     indexed,  recsize =  255,              ~
                        keypos =    1, keylen =  106         
                        
            filename$ = "ATSTTSAD" 
            call "EWDOPEN" (#1, filename$, err%)


           end