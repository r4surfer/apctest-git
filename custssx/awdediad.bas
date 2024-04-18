        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDEDIAD                             *~
            *  Creation Date     - 06/08/2018                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Ricky Beane                          *~
            *                                                           *~
            *  Description       - Utility program to insert the EDI    *~
            *                      Customer and Code in GENCODES.       *~
            *                                                           *~
            *      errmsg$       - 0 successfully inserted              *~
            *                      1 already exits/update code          *~
            *                      2 error                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *************************************************************
        
        sub "AWDEDIAD"   (edi_cust$,     /* New EDI Customer Number    */~
                          edi_code$,     /* New EDI Customer Code      */~
                          errmsg$)       /* Error Code                 */
                          
        dim currdate$6,                  /* Current date               */~
            edi_cust$10,                 /* EDI customer               */~
            edi_code$30,                 /* EDI customer code          */~
            errmsg$1,                    /* Error message code         */~
            r_rec$128,                   /* Gencodes file record       */~
            readkey$24                   /* Key of Gencodes file       */

        dim f2%(45%),                    /* = 0 if the file is open    */~
            fs%(45%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(45%)20                 /* Text from file opening     */

            mat f2% = con
            mat fs% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
REM    initialize
         init(" ") currdate$
         currdate$ = date 
     
REM------------------------------------------------------------------------
REM             M A I N                                                   -
REM------------------------------------------------------------------------
REM    main
      
        errmsg$ = "2"              /* initialize */
        
        gosub gen_record
 
        goto exit_program

REM------------------------------------------------------------------------
REM     Check if exists, then write to GENCODES                           -
REM------------------------------------------------------------------------

        gen_record
            init(" ") readkey$, r_rec$
            
            str(readkey$,1%,9%)   = "CLNTCODEX"
            str(readkey$,10%,15%) = edi_cust$
            read #1,hold,key = readkey$, using GENCODES, r_rec$,    ~
                 eod goto gencodes_no_data

GENCODES:         FMT CH(128)

               errmsg$ = "1"         /* Exists */

            if str(r_rec$,25%,30%) = edi_code$ then goto L01000
            
            delete #1 
            
            str(r_rec$,25%,30%) = edi_code$
            put #1, using GENCODES, r_rec$
            
            write #1
               
            goto L01000

gencodes_no_data:
            
            str(r_rec$,1%,9%)   = "CLNTCODEX"
            str(r_rec$,10%,15%) = edi_cust$
            str(r_rec$,25%,30%) = edi_code$
            
            put #1, using GENCODES, r_rec$  
            
            write #1, eod goto L01000 
            errmsg$ = "0"          /* Added successfully */

L01000: return  

REM------------------------------------------------------------------------
REM       E X I T    P R O G R A M                                        -
REM------------------------------------------------------------------------

        exit_program
            close #1
        end






















