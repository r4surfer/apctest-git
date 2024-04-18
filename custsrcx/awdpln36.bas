        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN36                             *~
            *  Creation Date     - 09/20/06                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  - Paul Williams                        *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                                                           *~
            *                      This program only creates the ARI    *~
            *                      master and lines data to import!!    *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for Shanes Reports              *~
            *                      AWDARIMS - ARIMASTR                  *~
            *                      AWDARILS - ARILINES                  *~
            *                                                           *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Spec. Comm (Screen 1) -                                  *~
            *                         PF(10) Create Delimited File.     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/20/06 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 01/03/07 ! (AWD001) Mod to add open invoice balacne ! CMG *~
            * 03/03/16 ! SR73147  Mod to add new extract fields & ! PWW *~
            *          !          consolidate NC & TX into one    !     *~
            *          !          execution of this program.      !     *~
            *04/16/2019! (CR1993) PlyGem new tax rate, HD Met and ! RDB *~
            *          !   HD discount                            !     *~
            *************************************************************

        dim                              /* (CATEGORY) - FILE          */~
            readkey$100,                 /* Readkey                    */~
            fields$(500%)256,            /* Generic Fields             */~
            fields42$(2%)250,           /* Field #42                   */~
            num_fields(500%)            /* Generic Fields             */ 
            
            
        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            cnt$28,                      /* Screen Display             */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            userid$3,                    /* Current User Id            */~
            readdate$10                  /* Read date                  */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim comma$1,                     /* Comma for EWDAPPSN File    */~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */

       dim                               /* (AWD001)                   */~
            asofu$6,                     /* As of Date                 */~
            bal$15,                      /* Balance                    */~
            bals(3),                     /* Balances - Statutory       */~
            cbals(3),                    /* balances                   */~
            cdate$6,                     /* ARMCBLNC - RETURNED        */~
            currency$4,                  /* Currency RETURNED          */~
            cur_billto$9                 /* Current Bill To            */

       dim orainv_key$18,                /* Orainv Key                 */~
           orainv_rec$256,               /* Orainv Rec                 */~
           cuscode$9,                    /* Customer Code              */~
           invoice$8,                    /* Invoice Number             */~
           invoice$(31)8,                /* invoices                   */~
           customer$(31)9                /* customer                   */
           
        dim schema$8                     /* Schema                     */           

        dim vtoc$22                      /* (SR73147)                  */
/*SR73147 + */
        dim oradescr_key$11,             /* ORADESC2 Read Key          */~
            oradescr_rec$(4%)256,        /* ORADESC2 Record            */~
            flds38$3,flds41$200,flds42$(2%)250,flds43$5,flds44$50,flds45$25
/*SR73147 - */

        REM *************************************************************

            mat f2% = con
            mat fs% = zer
            rslt$() = " "

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            
            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24            

            select #6,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  16,                     ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup

/*SR73147*/ select #16,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  16,                     ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup

            select #7,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  19,                     ~
                        alt key  1, keypos =   46, keylen =  19, dup      
                        
/*SR73147*/ select #17,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  19,                     ~
                        alt key  1, keypos =   46, keylen =  19, dup      
                        

            select #8,  "ARMTRIAL",                                     ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =  1,   keylen = 21                                                 

/*SR73147*/ select #18,  "ARMTRIAL",                                     ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =  1,   keylen = 21                                                 

            select #9, "CUSTOMER",                                       ~
                        varc, indexed, recsize = 1200,                   ~
                        keypos = 1, keylen =   9,                        ~
                        alt key  1, keypos =  10, keylen =  30, dup,     ~
                            key  2, keypos = 424, keylen =   9, dup,     ~
                            key  3, keypos = 771, keylen =   9, dup,     ~
                            key  4, keypos = 780, keylen =   9, dup

/*SR73147*/ select #19, "CUSTOMER",                                       ~
                        varc, indexed, recsize = 1200,                   ~
                        keypos = 1, keylen =   9,                        ~
                        alt key  1, keypos =  10, keylen =  30, dup,     ~
                            key  2, keypos = 424, keylen =   9, dup,     ~
                            key  3, keypos = 771, keylen =   9, dup,     ~
                            key  4, keypos = 780, keylen =   9, dup

            select #10, "ARMTBCEX",                                     ~
                        varc,     indexed,  recsize = 100,              ~
                        keypos =    5, keylen = 21,                     ~
                        alt key  1, keypos =  1, keylen =  25

/*SR73147*/ select #20, "ARMTBCEX",                                     ~
                        varc,     indexed,  recsize = 100,              ~
                        keypos =    5, keylen = 21,                     ~
                        alt key  1, keypos =  1, keylen =  25

            select #11,  "SYSFILE2",                                    ~
                        varc,     indexed,  recsize = 500,              ~
                        keypos =  1,   keylen = 20 
/*SR73147*/ select #21,  "SYSFILE2",                                    ~
                        varc,     indexed,  recsize = 500,              ~
                        keypos =  1,   keylen = 20 


            select #12, "ARMTBCRC",                                     ~
                        varc,     indexed,  recsize = 100,              ~
                        keypos =    5, keylen = 21,                     ~
                        alt key  1, keypos =  1, keylen =  25

/*SR73147*/ select #22, "ARMTBCRC",                                     ~
                        varc,     indexed,  recsize = 100,              ~
                        keypos =    5, keylen = 21,                     ~
                        alt key  1, keypos =  1, keylen =  25

            select #13, "ORAINV",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24     

/*SR73147*/ select #23, "ORAINV",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24     


            select #25, "AWDARIMS",                                      ~
                        varc,     indexed, recsize = 1664,               ~
                        keypos = 1,    keylen = 19
                        
/*SR73147   select #26, "AWDARILS",                                      ~
                        varc,     indexed, recsize = 496,                ~
                        keypos = 1,    keylen = 23                      */
                        
/*SR73147 + Minimun Record size = 406+815=1221  we'll go with 1300    */
            select #26, "AWDARILS",                                      ~
                        varc,     indexed, recsize = 1300,                ~
                        keypos = 1,    keylen = 23
                        
            select #14, "ORADESC2",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =   1,   keylen = 11
                        
            select #24, "ORADESC2",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =   1,   keylen = 11
/*SR73147 - */
REM            call "SHOSTAT" ("Initialization")

            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            
/*SR73147   filename$ = "ARIMASTR"   call "EWDOPEN" (#6, filename$, err%) ~
            if err% <> 0% then gosub open_error                           ~
            filename$ = "ARILINES"   call "EWDOPEN" (#7, filename$, err%) ~
            if err% <> 0% then gosub open_error                           ~
            filename$ = "ARMTRIAL"   call "EWDOPEN" (#8, filename$, err%) ~
            if err% <> 0% then gosub open_error                           ~
            filename$ = "CUSTOMER"   call "EWDOPEN" (#9, filename$, err%) ~
            if err% <> 0% then gosub open_error                           ~
            filename$ = "ARMTBCEX"   call "EWDOPEN" (#10, filename$, err%)~
            if err% <> 0% then gosub open_error                           ~
            filename$ = "SYSFILE2"   call "EWDOPEN" (#11, filename$, err%)~
            if err% <> 0% then gosub open_error                           ~
            filename$ = "ARMTBCRC"   call "EWDOPEN" (#12, filename$, err%)~
            if err% <> 0% then gosub open_error                           ~
                                                                          ~
            filename$ = "ORAINV"   call "EWDOPEN" (#13, filename$, err%)  ~
            if err% <> 0% then gosub open_error                          */

/*SR73147 + */
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARIMASTR" 
               library$  = "APCDATA" 
               chno% = 6%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARIMASTR" 
               library$  = "NEDATA" 
               chno% = 16%                     
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARILINES" 
               library$  = "APCDATA" 
               chno% = 7%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARILINES" 
               library$  = "NEDATA" 
               chno% = 17%
               gosub open_special
                                    
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARMTRIAL"
               library$  = "APCDATA" 
               chno% = 8%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARMTRIAL"
               library$  = "NEDATA" 
               chno% = 18%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "CUSTOMER"
               library$  = "APCDATA" 
               chno% = 9%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "CUSTOMER"
               library$  = "NEDATA" 
               chno% = 19%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARMTBCEX" 
               library$  = "APCDATA" 
               chno% = 10%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARMTBCEX" 
               library$  = "NEDATA" 
               chno% = 20%                     
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "SYSFILE2" 
               library$  = "APCDATA" 
               chno% = 11%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "SYSFILE2" 
               library$  = "NEDATA" 
               chno% = 21%
               gosub open_special
                                    
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARMTBCRC"
               library$  = "APCDATA" 
               chno% = 12%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARMTBCRC"
               library$  = "NEDATA" 
               chno% = 22%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ORAINV"
               library$  = "APCDATA" 
               chno% = 13%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ORAINV"
               library$  = "NEDATA" 
               chno% = 23%
               gosub open_special

               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ORADESC2"
               library$  = "APCDATA" 
               chno% = 14%
               gosub open_special
               
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ORADESC2"
               library$  = "NEDATA" 
               chno% = 24%
               gosub open_special

/*SR73147 - */
            
            mat f1% = zer


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


            call "EXTRACT" addr("ID", userid$)
            date$ = date
            
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #4, schema_err%)            

            call "DATEFMT" (date$)
            mat num_fields     = zer

REM 0% = extract entire file and 1% = only new daily data
            extract% = 1%


             file$   = "AWDARIMS"
             ff% = 25%  
            volume$ = "CARLO2"
             gosub open_file

             file$   = "AWDARILS"
             ff% = 26%  
            volume$ = "CARLO2"
             gosub open_file


          init(" ") customer$(), invoice$()
          customer$(1) = "TST0001"	:     invoice$(1) = "02245795"
          customer$(2) = "TST0001"	:     invoice$(2) = "02245796"
          customer$(3) = "TST0001"	:     invoice$(3) = "02245797"
          customer$(4) = "TST0001"	:     invoice$(4) = "02245798"
          customer$(5) = "TST0002"	:     invoice$(5) = "02245799"
          customer$(6) = "TST0002"	:     invoice$(6) = "02245800"
          customer$(7) = "TST0002"	:     invoice$(7) = "02245801"
          customer$(8) = "TST0002"	:     invoice$(8) = "02245802"
          customer$(9) = "TST0003"	:     invoice$(9) = "02245803"
          customer$(10) = "TST0003"	:     invoice$(10) = "02245804"
          customer$(11) = "TST0003"	:     invoice$(11) = "02245805"
          customer$(12) = "TST0003"	:     invoice$(12) = "02245806"
          customer$(13) = "TST0004"	:     invoice$(13) = "02245807"
          customer$(14) = "TST0004"	:     invoice$(14) = "02245808"
          customer$(15) = "TST0004"	:     invoice$(15) = "02245809"
          customer$(16) = "TST0004"	:     invoice$(16) = "02245810"
          customer$(17) = "TST0005"	:     invoice$(17) = "02245811"
          customer$(18) = "TST0005"	:     invoice$(18) = "02245812"
          customer$(19) = "TST0005"	:     invoice$(19) = "02245813"
          customer$(20) = "TST0005"	:     invoice$(20) = "02245814"
          customer$(21) = "TST0006"	:     invoice$(21) = "02245815"
          customer$(22) = "TST0006"	:     invoice$(22) = "02245816"
          customer$(23) = "TST0006"	:     invoice$(23) = "02245817"
          customer$(24) = "TST0006"	:     invoice$(24) = "02245818"
          customer$(25) = "AB0337"	:     invoice$(25) = "02245822"
          customer$(26) = "BA0014"	:     invoice$(26) = "02245823"
          customer$(27) = "LO2620"	:     invoice$(27) = "02245825"
          customer$(28) = "NO0055"	:     invoice$(28) = "02245821"
          customer$(29) = "NO0069"	:     invoice$(29) = "02245820"
          customer$(30) = "SO0200"	:     invoice$(30) = "02245824"
          customer$(31) = "TST0001"	:     invoice$(31) = "02245819"

             RCNT = 0:CCNT = 0

             ff1% = 6%
             ff2% = 7%
             ff3% = 8%
             ff4% = 9%
             ff5% = 10%
             ff6% = 11%
             ff7% = 12%
             ff8% = 13%
             ff9% = 14%
             ncntx$ = "NC "
             
         begin_files_analysis
             gosub initialize_variables
             if extract% = 0% then gosub files_analysis
             if extract% = 1% then gosub files_analysis_daily
             if ff1% > 6% then goto exit_program
             ff1% = 16%
             ff2% = 17%
             ff3% = 18%
             ff4% = 19%
             ff5% = 20%
             ff6% = 21%
             ff7% = 22%
             ff8% = 23%
             ff9% = 24%
             ncntx$ = "NTX "
             goto begin_files_analysis

               goto exit_program

REM   END OF AUTOMATION


        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        files_analysis
            comma$ = "|"

            init(" ") readkey$, fields$()
            readkey$ = all(hex(00))

            init(" ") readkey$, fields$()
            readkey$ = all(hex(00))



            gosub create_mast
            gosub create_lines

        return



        files_analysis_daily
            comma$ = "|"

            init(" ") readkey$, fields$()
            readkey$ = all(hex(00))

        file_analysis_daily_next
            gosub read_orainv
               if rec% = 0% then goto daily_done
REM         for i% = 1% to 31%


REM         cuscode$ = customer$(i%)
REM         invoice$ = invoice$(i%)

            cuscode$ = str(orainv_rec$,8 ,9)
            invoice$ = str(orainv_rec$,17,8)


            str(readkey$,1,9)  = cuscode$
            str(readkey$,10,8) = invoice$

            rec2% = 0%
            gosub create_mast

/*pwww      if rec2% = 0% then goto file_analysis_daily_next  */
            str(readkey$,1,9)  = cuscode$    
            str(readkey$,10,8) = invoice$

            gosub create_lines



            gosub update_orainv
              goto file_analysis_daily_next

REM            next i%
daily_done
        return


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$, library$, volume$, readdate$

        return

        REM *************************************************************~
            *************************************************************


        
        create_mast
REM             init(" ") readkey$, file$
             rec%, cnt% = 0%


             gosub read_mast
                   goto L67010
        create_mast_nxt
             gosub read_mast_nxt
L67010:      if rec% <> 1% then goto mast_done
        
REM             gosub build_file
               gosub calc_invoice_balance   /* (AWD001) */
               gosub lookup_current_billto

               gosub write_upload_mast
               goto create_mast_nxt
        return
        mast_done
        return

        read_orainv
             rec% = 0%
             init(" ") orainv_key$                     
             orainv_key$ = "S"                                              
/*           orainv_key$ = "R"  t e m p ! !!                              */
             read #ff8%, key > orainv_key$, using orainv_fmt, orainv_rec$,  ~
                                                      eod goto no_orainv

orainv_fmt:         FMT CH(256)

                    orainv_key$ = str(orainv_rec$,7,18)

                    if str(orainv_key$,1,1) <> "S" then goto no_orainv       
/*  t e m p ! !     if str(orainv_key$,1,1) <> "R" then goto no_orainv     */
          goto L61000
          
          RCNT = RCNT + 1
          if MOD(RCNT,1000) <> 0 then goto L61000
          convert RCNT to RCNT$, pic (00000000)
          convert CCNT to CCNT$, pic (00000000)
          call "SHOSTAT" (ncntx$ & " Read... " & RCNT$ & " Wrote... " & CCNT$)
          
L61000:   


REM                 invoice$ = str(orainv_key$,17,8)
                    rec% = 1%

        no_orainv
        return                

        update_orainv
             read #ff8%, hold, key = orainv_key$, using orainv_fmt,     ~
                                  orainv_rec$, eod goto no_updte_orainv


                    str(orainv_rec$,7,1)  = "T"                         
/*                  str(orainv_rec$,7,1)  = "R"      pwww t e m p      */
                    str(orainv_rec$,36,6) = date

                 delete #ff8%

                 write #ff8%, using orainv_fmt, orainv_rec$,           ~
                                             eod goto no_updte_orainv   

        no_updte_orainv
        return                

        
        read_mast

            mat num_fields = zer
            rec% = 0%

            if extract% = 0% then                       ~
            read #ff1%, key > readkey$, eod goto read_mast_done ~
            else                                        ~
            read #ff1%, key = readkey$, eod goto read_mast_done
        
                goto L67100
        read_mast_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #ff1%, eod goto read_mast_done

L67100:         cnt% = cnt% + 1%
REM            goto L67155

            if extract% = 0% then goto L67155
               get #ff1%, using mastr_fmt, readkey$
mastr_fmt:          FMT CH(17)

                 if str(readkey$,10,8) <> invoice$ then ~
                                      goto read_mast_done

            goto L67155
            if mod(cnt%,50%) <> 0% then goto L67155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L67155:
                get #ff1%, using L67110, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), fields$(12%),~
                                      fields$(13%), fields$(14%),~
                                      fields$(15%), fields$(16%),~
                                      fields$(17%), fields$(18%),~
                                      fields$(19%), fields$(20%),~
                                      fields$(21%), num_fields(22%),~
                                      num_fields(23%), fields$(24%),~
                                      fields$(25%), num_fields%(26%),~
                                      fields$(27%), fields$(28%),~
                                      fields$(29%), fields$(30%),~
                                      fields$(31%), fields$(32%),~
                                      fields$(33%), fields$(34%),~
                                      fields$(35%), fields$(36%),~
                                      fields$(37%), fields$(38%),~
                                      fields$(39%), fields$(40%),~
                                      fields$(41%), fields$(42%),~
                                      fields$(43%), fields$(44%),~
                                      fields$(45%), fields$(46%),~
                                      fields$(47%), fields$(48%),~
                                      num_fields(49%), num_fields(50%),~
                                      num_fields(51%), num_fields(52%),~
                                      num_fields(53%), num_fields(54%),~
                                      num_fields(55%), fields$(56%),~
                                      fields$(57%), num_fields(58%), ~
                                      num_fields(59%), num_fields(60%), ~
                                      fields$(61%), fields$(62%), ~
                                      num_fields%(63%), fields$(64%), ~
                                      fields$(65%), fields$(66%), ~
                                      fields$(67%), num_fields(68%), ~
                                      num_fields(69%), fields$(70%),~
                                      fields$(71%), fields$(72%),~
                                      fields$(73%), fields$(74%),~
                                      fields$(75%), fields$(76%),~
                                      fields$(77%), fields$(78%),~
                                      fields$(79%), num_fields(80%),~
                                      num_fields(81%),              ~
/* CR1993 */                          fields$(82%), num_fields(83%),~
                                      num_fields(84%)



L67110:           FMT CH(09), CH(08), CH(16), CH(16), CH(03),CH(30),    ~
                      CH(30), CH(30), CH(30), CH(30), CH(30), CH(30),   ~
                      CH(30), CH(30), CH(30), CH(30), CH(30), CH(06),   ~
                      CH(20), CH(20), CH(06), PD(15,4), PD(15,4),     ~
                      CH(20), CH(04), POS(513), BI(01), POS(516), ~
                      CH(04), CH(01), CH(06), CH(06), CH(06), CH(06),   ~
                      CH(03), CH(20), CH(20), CH(20), CH(20), CH(20),   ~
                      CH(20), CH(20), CH(20), CH(20), CH(20), CH(09),   ~
                      CH(09), CH(09), CH(09), CH(09), PD(15,4), PD(15,4),~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4), ~
                      CH(09), CH(12), CH(03), CH(10), PD(15,4), CH(01), ~
                      CH(09), BI(04), CH(01), CH(01), CH(01), CH(20),   ~
                      PD(15,4), POS(1168), PD(15,4), POS(1408), CH(06),~
                      POS(1588), CH(06), POS(1768), CH(02), CH(09), CH(04),~
                      CH(09), CH(09), CH(08), CH(01), CH(01), PD(14,4),  ~
                      PD(14,4), POS(1844), CH(20), PD(14,4), PD(14,4)           
/* CR1993 add pgpo, hdhem, hdidx */
            
            convert num_fields(26%) to fields$(26%), pic(########0)
            convert num_fields(63%) to fields$(63%), pic(########0)

   
            convert num_fields(22%) to fields$(22%), pic(-#######0.00##)
            convert num_fields(23%) to fields$(23%), pic(-#######0.00##)
            convert num_fields(49%) to fields$(49%), pic(-#######0.00##)
            convert num_fields(50%) to fields$(50%), pic(-#######0.00##)
            convert num_fields(51%) to fields$(51%), pic(-#######0.00##)
            convert num_fields(52%) to fields$(52%), pic(-#######0.00##)
            convert num_fields(53%) to fields$(53%), pic(-#######0.00##)   
            convert num_fields(54%) to fields$(54%), pic(-#######0.00##)   
            convert num_fields(55%) to fields$(55%), pic(-#######0.00##)   
            convert num_fields(60%) to fields$(60%), pic(-#######0.00##)   
            convert num_fields(68%) to fields$(68%), pic(-#######0.00##)   
            convert num_fields(69%) to fields$(69%), pic(-#######0.00##)   
            convert num_fields(80%) to fields$(80%), pic(-#######0.00##)   
            convert num_fields(81%) to fields$(81%), pic(-#######0.00##) 
/* CR1993 add hdhem, hdidx */            
            convert num_fields(83%) to fields$(83%), pic(-#######0.00##)
            convert num_fields(84%) to fields$(84%), pic(-#######0.00##)

            convert fields$(80%) to num_fields(80%), data goto bad_esc

correct_esc:

            call "DATFMTC" (fields$(18%), date%, fields$(18%))            
            call "DATFMTC" (fields$(29%), date%, fields$(29%))
            call "DATFMTC" (fields$(30%), date%, fields$(30%))
            call "DATFMTC" (fields$(31%), date%, fields$(31%))
            call "DATFMTC" (fields$(32%), date%, fields$(32%))
            call "DATFMTC" (fields$(70%), date%, fields$(70%))
            call "DATFMTC" (fields$(71%), date%, fields$(71%))                     
/*SR73147*/ awd_so$ = fields$(4%)

            rec% = 1%            
        read_mast_done
        return


        write_upload_mast
            write #25, using L67400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$,      ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$,      ~
                              fields$(14%), comma$, fields$(15%),~
                              comma$, fields$(16%), comma$,      ~
                              fields$(17%), comma$, fields$(18%),~
                              comma$, fields$(19%), comma$,      ~
                              fields$(20%), comma$, fields$(21%),~
                              comma$, fields$(22%), comma$,      ~ 
                              fields$(23%), comma$, fields$(24%),~
                              comma$, fields$(25%), comma$,      ~
                              fields$(26%), comma$, fields$(27%),~
                              comma$, fields$(28%), comma$,      ~
                              fields$(29%), comma$, fields$(30%),~
                              comma$, fields$(31%), comma$,      ~
                              fields$(32%), comma$, fields$(33%),~
                              comma$, fields$(34%), comma$,      ~
                              fields$(35%), comma$, fields$(36%),~
                              comma$, fields$(37%), comma$,      ~
                              fields$(38%), comma$, fields$(39%),~
                              comma$, fields$(40%), comma$,      ~
                              fields$(41%), comma$, fields$(42%),~
                              comma$, fields$(43%), comma$,      ~
                              fields$(44%), comma$, fields$(45%),~
                              comma$, fields$(46%), comma$,      ~
                              fields$(47%), comma$, fields$(48%),~
                              comma$, fields$(49%), comma$,      ~
                              fields$(50%), comma$, fields$(51%),~
                              comma$, fields$(52%), comma$,      ~
                              fields$(53%), comma$, fields$(54%),~
                              comma$, fields$(55%), comma$,      ~
                              fields$(56%), comma$, fields$(57%),~
                              comma$, fields$(58%), comma$,      ~
                              fields$(59%), comma$, fields$(60%),~
                              comma$, fields$(61%), comma$,      ~
                              fields$(62%), comma$, fields$(63%),~
                              comma$, fields$(64%), comma$,      ~
                              fields$(65%), comma$, fields$(66%),~
                              comma$, fields$(67%), comma$,      ~
                              fields$(68%), comma$, fields$(69%),~
                              comma$, fields$(70%), comma$,      ~
                              fields$(71%), comma$, fields$(72%),~
                              comma$, fields$(73%), comma$,      ~
                              fields$(74%), comma$, fields$(75%),~
                              comma$, fields$(76%), comma$,      ~
                              fields$(77%), comma$, fields$(78%),~
                              comma$, fields$(79%), comma$,      ~
                              fields$(80%), comma$, fields$(81%),~
                              comma$, bal$, comma$, cur_billto$, ~
                              comma$, "S", ~
/*SR73147 */                  comma$, " ", comma$, "0", comma$,  ~
/* CR1993 */                  fields$(82%), comma$, fields$(83%),~
                              comma$, fields$(84%), comma$,      ~
                              eod goto crash


 
L67400:           FMT CH(09), CH(01), CH(08), CH(01), CH(16), CH(01),    ~
                  CH(16), CH(01), CH(03), CH(01), CH(30), CH(01), CH(30),~
                  CH(01), CH(30), CH(01), CH(30), CH(01), CH(30), CH(01),~
                  CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), CH(30),~
                  CH(01), CH(30), CH(01), CH(30), CH(01), CH(30), CH(01),~
                  CH(10), CH(01), CH(20), CH(01), CH(20), CH(01), CH(06),~
                  CH(01), CH(14), CH(01), CH(14), CH(01), CH(20), CH(01),~
                  CH(04), CH(01), CH(09), CH(01), CH(04), CH(01), CH(01),~
                  CH(01), CH(10), CH(01), CH(10), CH(01), CH(10), CH(01),~
                  CH(10), CH(01), CH(03), CH(01), CH(20), CH(01), CH(20),~
                  CH(01), CH(20), CH(01), CH(20), CH(01), CH(20), CH(01),~
                  CH(20), CH(01), CH(20), CH(01), CH(20), CH(01), CH(20),~
                  CH(01), CH(20), CH(01), CH(09), CH(01), CH(09), CH(01),~
                  CH(09), CH(01), CH(09), CH(01), CH(09), CH(01), CH(14),~
                  CH(01), CH(14), CH(01), CH(14), CH(01), CH(14), CH(01),~
                  CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), CH(09),~
                  CH(01), CH(12), CH(01), CH(03), CH(01), CH(10), CH(01),~
                  CH(14), CH(01), CH(01), CH(01), CH(09), CH(01), CH(09),~
                  CH(01), CH(01), CH(01), CH(01), CH(01), CH(01), CH(01),~
                  CH(20), CH(01), CH(14), CH(01), CH(14), CH(01), CH(10),~
                  CH(01), CH(10), CH(01), CH(02), CH(01), CH(09), CH(01),~
                  CH(04), CH(01), CH(09), CH(01), CH(09), CH(01), CH(08),~
                  CH(01), CH(01), CH(01), CH(01), CH(01), CH(14), CH(01),~
                  CH(14), CH(01), CH(14), CH(01), CH(09), CH(01),        ~
/*SR73147 */      CH(01), CH(01), CH(01), CH(01), CH(01), CH(01),        ~
/* CR1993 */      CH(20), CH(01), CH(14), CH(01), CH(14), CH(01)
            

                  CCNT  = CCNT  + 1
                  rec2% = 1%
        return

bad_esc:

        num_fields(80%), num_fields(81%) = 0.00
        convert num_fields(80%) to fields$(80%), pic(-#######0.00##)   
        convert num_fields(81%) to fields$(81%), pic(-#######0.00##)   

           goto correct_esc

        
        
        create_lines
             rec%, cnt% = 0%
             gosub read_lines
                   goto L68010
        create_lines_nxt
             gosub read_lines_nxt
L68010:      if rec% <> 1% then goto lines_done
         
REM             gosub build_file
               gosub write_upload_lines
               goto create_lines_nxt
        return
        lines_done
        return

        read_lines

            mat num_fields = zer
            rec% = 0%
            read #ff2%, key > readkey$, eod goto read_lines_done
                goto L68100
        read_lines_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #ff2%, eod goto read_lines_done

L68100:         cnt% = cnt% + 1%
REM            goto L68155
            if extract% = 0% then goto L68155
               get #ff2%, using lines_fmt, readkey$
lines_fmt:          FMT CH(20)
                  if str(readkey$,10,8) <> invoice$ then goto read_lines_done


            goto L68155
            if mod(cnt%,50%) <> 0% then goto L68155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L68155:
                get #ff2%, using L68110, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), num_fields(8%),~
                                      num_fields(9%), num_fields(10%), ~
                                      num_fields(11%), fields$(12%),~
                                      fields$(13%), num_fields(14%),~
                                      num_fields(15%), num_fields(16%),~
                                      num_fields(17%), num_fields(18%),~
                                      fields$(19%), fields$(20%),~
                                      fields$(21%), fields$(22%),~
                                      num_fields%(23%), fields$(24%),~
                                      fields$(25%), num_fields(26%),~
                                      fields$(27%), fields$(28%),~
                                      fields$(29%), num_fields(30%),~
                                      num_fields(31%), fields$(32%),~
                                      num_fields(33%), num_fields%(34%),~
                                      fields$(35%), fields$(36%), fields$(37%)




L68110:           FMT CH(09), CH(08), CH(03), CH(03), CH(25), CH(32), ~
                      CH(04), PD(15,4), PD(15,4), PD(15,4), PD(15,4), ~
                      CH(04), CH(04), PD(15,7), PD(15,4), PD(15,4),   ~
                      PD(14,4), PD(14,4), CH(01), CH(09), CH(09),     ~
                      CH(06), BI(04), CH(03), CH(06), POS(377),       ~ 
                      PD(15,4), POS(617), CH(01), CH(08), CH(04),    ~
                      PD(15,4), PD(15,4), CH(01), PD(15,4), BI(04), ~
                      CH(10), CH(10), CH(09)
                      
/*SR73147 + */
            str(oradescr_key$,1%,8%) = awd_so$
            str(oradescr_key$,9%,3%) = fields$(24%)
            read #ff9%, key = oradescr_key$, eod goto skip_oradesc2

            get #ff9%, using L00100, oradescr_rec$()
L00100:           FMT 4*CH(256)
L00110:          FMT PD(15,4)
L00120:          FMT BI(2)
            fields$(38%) = str(oradescr_rec$(),12%,3%)    /*WW_LINE */
            flds38$ = str(oradescr_rec$(),12%,3%)    /*WW_LINE */
            get str(oradescr_rec$(),55%,8%) using L00110, widthes
            get str(oradescr_rec$(),63%,8%) using L00110, heightes  
            convert widthes  to fields$(39%), pic(##0.0###)
            convert heightes to fields$(40%), pic(##0.0###)

            fields$(41%) = str(oradescr_rec$(),71%,200%)  /*L1_DESC */
            flds41$ = str(oradescr_rec$(),71%,200%)  /*L1_DESC */
/*          fields42$() = str(oradescr_rec$(),271%,500%)    L2_DESC */
            fields$(42%) = str(oradescr_rec$(),321%,200%)  /*L2_DESC */
            flds42$ = str(oradescr_rec$(),321%,500%)  /*L2_DESC */
            get str(oradescr_rec$(),821%,2%) using L00120, l1_linetypeid%
            convert l1_linetypeid%  to fields$(43%), pic(#0) /* L1_LINETYPEID*/
            flds43$ = fields$(43%)/*L1_LINETYPEID*/
            fields$(44%) = str(oradescr_rec$(),823%,50%)  /* L1_MUTYPE */
            flds44$ = str(oradescr_rec$(),823%,50%)  /* L1_MUTYPE */
            fields$(45%) = str(oradescr_rec$(),873%,25%)  /* L3_MULLTYPE */
            flds45$ = str(oradescr_rec$(),873%,25%)  /* L3_MULLTYPE */
        skip_oradesc2
/*SR73147 - */
            convert num_fields(23%) to fields$(23%), pic(########0)
            convert num_fields(34%) to fields$(34%), pic(########0)            
            
            convert num_fields(8%) to fields$(8%), pic(-#######0.00##)
            convert num_fields(9%) to fields$(9%), pic(-#######0.00##)
            convert num_fields(10%) to fields$(10%), pic(-#######0.00##)
            convert num_fields(11%) to fields$(11%), pic(-#######0.00##)
            convert num_fields(14%) to fields$(14%), pic(-####0.00#####)
            convert num_fields(15%) to fields$(15%), pic(-#######0.00##)
            convert num_fields(16%) to fields$(16%), pic(-#######0.00##)
            convert num_fields(17%) to fields$(17%), pic(-#######0.00##)
            convert num_fields(18%) to fields$(18%), pic(-#######0.00##)
            convert num_fields(26%) to fields$(26%), pic(-#######0.00##)
            convert num_fields(30%) to fields$(30%), pic(-#######0.00##)
            convert num_fields(31%) to fields$(31%), pic(-#######0.00##)
            convert num_fields(33%) to fields$(33%), pic(-#######0.00##)

            rec% = 1%            
        read_lines_done
        return


        write_upload_lines
            write #26, using L68400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$,      ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$,      ~
                              fields$(14%), comma$, fields$(15%),~
                              comma$, fields$(16%), comma$,      ~
                              fields$(17%), comma$, fields$(18%),~
                              comma$, fields$(19%), comma$,      ~
                              fields$(20%), comma$, fields$(21%),~
                              comma$, fields$(22%), comma$,      ~ 
                              fields$(23%), comma$, fields$(24%),~
                              comma$, fields$(25%), comma$,      ~
                              fields$(26%), comma$, fields$(27%),~
                              comma$, fields$(28%), comma$,      ~
                              fields$(29%), comma$, fields$(30%),~
                              comma$, fields$(31%), comma$,      ~
                              fields$(32%), comma$, fields$(33%),~
                              comma$, fields$(34%), comma$,      ~
                              fields$(35%), comma$, fields$(36%),~
                              comma$, fields$(37%), comma$,      ~
                              fields$(38%), comma$, fields$(39%),~
                              comma$, fields$(40%), comma$,      ~
                              fields$(41%), comma$, fields$(42%),~
                              comma$, fields$(43%), comma$,      ~
                              fields$(44%), comma$, fields$(45%),~
                              comma$, eod goto crash

L68400:           FMT CH(09), CH(01), CH(08), CH(01), CH(03), CH(01),~
                      CH(03), CH(01), CH(25), CH(01), CH(32), CH(01),~
                      CH(04), CH(01), CH(14), CH(01), CH(14), CH(01),~
                      CH(14), CH(01), CH(14), CH(01), CH(04), CH(01),~
                      CH(04), CH(01), CH(14), CH(01), CH(14), CH(01),~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01),~
                      CH(01), CH(01), CH(09), CH(01), CH(09), CH(01),~
                      CH(06), CH(01), CH(09), CH(01), CH(03), CH(01),~
                      CH(06), CH(01), CH(14), CH(01), CH(01), CH(01),~
                      CH(08), CH(01), CH(04), CH(01), CH(14), CH(01),~
                      CH(14), CH(01), CH(01), CH(01), CH(14), CH(01),~
                      CH(09), CH(01), CH(10), CH(01), CH(10), CH(01),~
                      CH(09), CH(01), CH(03), CH(01), CH(08), CH(01),~
                      CH(08), CH(01), CH(200), CH(01), CH(200), CH(01),~
                      CH(05), CH(01), CH(50), CH(01), CH(25), CH(01)
                      
        return

        
        
        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        open_file
            init(" ") library$, volume$
            library$        = "ORAFILES"
            volume$ = "CARLO2"                                            
/*SR73147   if schema% = 2% then volume$ = "NE2"                        */


             open nodisplay #ff%, output, space = 100%,                  ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

/*(AWD001) - BEGIN */       
        calc_invoice_balance
           coneqv, conunt = 0.00
           mat cbals = zer
           asofu$  = date
           
/* fields$(56%) = bill to customer */
/* fields$(57%) = settlement code  */
           call "ARMCBLNC" (fields$(56%), fields$(57%), asofu$, 10%, ~
                            "N", #ff3%, #ff4%, bals(), #ff5%, #ff6%, #ff7%,~
                            currency$, cdate$, coneqv,conunt, cbals())

/* Current Invoice Balance */
               convert bals(3%) to bal$, pic(-#######0.00##)
               

        return
/*(AWD001) - END */       

        lookup_current_billto
             init(" ") cur_billto$
             read #ff4%, key = fields$(1%), using cur_billtofmt, cur_billto$, ~
                                 eod goto currentbillto_done

cur_billtofmt:         FMT POS(780), CH(09)


        currentbillto_done
        return

/*SR73147 + */
        open_special
               x% = 1%
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #chno%,                                   ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$
               return
/*SR73147 - */

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
REM            call "SHOSTAT" ("One Moment Please")

            end


            file_error
               err% = 2%
 
            end


        crash
          stop
          stop
          
