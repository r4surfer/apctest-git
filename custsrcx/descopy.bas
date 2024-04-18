REM         +---------------------------------------------------------------+
REM         | fix status code in apcplnor & sc with max stat in dt          |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            nc_rec$128,                  /* Detail Record              */~
            ud_key$48,                   /* Detail Record              */~
            ud_rec$112,                  /* Detail Record              */~
            dt_rec$256,                  /* Detail Record              */~
	    new_key$24,                  /*                            */~ 
            wt_key$18,                   /* APCPLWT readkey            */~
            warranty$8,                  /* warranty                   */~
            part$25                      /* Part number                */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            

        dim                              /* (AWD083)                   */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            field5$1                     /* New Part Field 5 FOAM      */

        dim schema$8                     /* Schema (AWD088)            */
           
	dim scr_nbr%                  /* Screen Number AWD076 */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$32, apc1$41                   /* (EWD055) */
                                                            /* (EWD060) */
                                                            /* (EWD066) */
                                                            /* (EWD068) */
                                                            /* (EWD072) */
                                                            /* (AWD077) */
                                                            /* (AWD082) */ 
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AWDBAYBW !                                          *~
            * #2  ! APCPLNUD !                                          *~
            * #3  ! APCPLNDT !                                          *~
            * #4  ! APCPLNWT ! get warentee #                           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "AWDBAYBW",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   29

            select #2,  "APCPLNUD",                                      ~
                        varc,     indexed,  recsize = 112,               ~
                        keypos =   35, keylen =   48,                    ~
                        alt key  1, keypos =    1, keylen =  49

            select #5,  "APCPLNWT",                                      ~
                         varc,    indexed,  recsize =  128,              ~
                         keypos =    1, keylen =  8,                     ~
                         alt key 1, keypos =  9, keylen = 10, dup,       ~
                             key 2, keypos =  9, keylen = 18

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),0%, rslt$(5%))

            mat f1% = zer

	     init(hex(00)) ud_key$
L01000:      /* main loop */
              read #2, key 0% > ud_key$, using L50780, ud_rec$,          ~
                                            eod goto L56890
              ud_key$ = str(ud_rec$,35,48)
REM       1 - 18 bar  
REM      19 - 21 dept 
REM      22 - 23 proc 
REM      24 - 29 date    PD(11,1)
REM      30 - 37 time  
REM      38 - 42 load  
REM      43 - 44 shift 
REM      45 - 46 status
REM      47 - 49 userid   
REM      50 - 50 transmitted flag
REM      51 - 128 filler

             init(" ") warranty$,part$,warranty$,wt_key$
             wt_key$ = str(ud_rec$,16,18)
             read #5, key 2 = wt_key$, using wt_fmt, warranty$, part$, ~
                             eod goto no_wt_rec

wt_fmt:            FMT CH(08), POS(27), CH(25)
no_wt_rec:
 
             init(" ") nc_rec$             
             str(nc_rec$,01%,18%)  = str(ud_rec$,16,18)  /* barcode */
             str(nc_rec$,19%,05%)  = str(ud_rec$,40,05)  /* dept/proc */
             str(nc_rec$,24%,06%)  = str(ud_rec$,34,06)  /* date    */
             str(nc_rec$,30%,18%)  = str(ud_rec$,82,08)  /* time    */
             str(nc_rec$,38%,05%)  = str(ud_rec$,59,05)  /* load    */
             str(nc_rec$,43%,04%)  = str(ud_rec$,45,04)  /* shft/st */
             str(nc_rec$,47%,03%)  = str(ud_rec$,90,03)  /* userid  */
             str(nc_rec$,50%,1%)   = "S" 
             str(nc_rec$,51%,25%)  = part$
             str(nc_rec$,76%,8%)   = warranty$

             write #1, using L50760, nc_rec$, eod goto L01010

L01010:      goto L01000

L50760:     FMT CH(128)
L50780:     FMT CH(112)
L56890:      end



