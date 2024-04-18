        REM *************************************************************~
            *                                                           *~
            *  Special Note - Used by Programs (EWDPLN58) and (AWDPLN60)*~
            *                                                           *~
            *  Program Name      - GENREAD                              *~
            *  Creation Date     - 05/16/2016                           *~
            *  Last Modified Date-                                      *~
            *  Last Modified By  - Christie Norman                      *~
            *                                                           *~
            *  Description       - Subroutine to read gencdoes          *~
            *  Defined Errors                                           *~
            *    err% = 1% -> no data found                             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *05/16/2016! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

         sub "GENREAD"   (table$,        /* Table To Read          IN  */~
                          genkey$,       /* Key to Read            IN  */~
                          descr1$,       /* Description Return     OUT */~
                          codeLen$,      /* Max Allowable Code Len OUT */~
                          descr2$,       /* Description Return     OUT */~
                          descr3$,       /* Description Return     OUT */~
                          err%)          /* Errors                 OUT */


         dim table$9,                    /* Table To Read              */~
             genkey$15,                  /* GENCODES Key to Read       */~
             descr1$30,                  /* Description                */~
             codeLen$2,                  /* Code Length                */~
             descr2$30,                  /* Description                */~
             descr3$30,                  /* Description                */~
             readkey$30                  /* Generic Gencodes readkey   */

        dim f2%(32%),                    /* = 0 if the file is open    */~
            f1%(32%),                    /* = 1 if READ was successful */~
            fs%(32%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32%)20                 /* Text from file opening     */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! System Master Code Table Files           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


           mat f1% = zer
           err% = 99%
           if beenHereBefore% = 1% then goto NoFileOpen
            beenHereBefore% = 1%
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),  0%, rslt$(1%))

NoFileOpen:

           gosub readGENCODES
           goto endSub

           readGENCODES
             init(" ") readkey$, descr1$, codeLen$, descr2$, descr3$
             str(readkey$,1%,9%)   = table$
             str(readkey$,10%,15%) = genkey$
             read #1,key = readkey$, using L00001, descr1$, codeLen$, descr2$, ~
                               descr3$,  eod goto noGENCODES

L00001:       FMT POS(25), CH(30), CH(02), CH(30), CH(30)

              err% = 0%
           return
           noGENCODES
              err% = 1%               /* No Code Found */
           return

        endSub
        end



