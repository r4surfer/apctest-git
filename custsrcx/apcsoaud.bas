        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    SSSS   OOO    AAA   U   U  TTTTT   *~
            *  A   A  P   P  C   C  S      O   O  A   A  U   U    T     *~
            *  AAAAA  PPPP   C       SSS   O   O  AAAAA  U   U    T     *~
            *  A   A  P      C   C      S  O   O  A   A  U   U    T     *~
            *  A   A  P       CCC   SSSS    OOO   A   A   UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCAUDIT - Summary Audit of all Sales Orders Entered      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/06/91 ! New APC Audit Program                    ! RHH *~
            *          !                                          !     *~
            * 11/06/97 ! Reflect 60403 Revision Code              ! DJD *~
            *          !                                          !     *~
            *************************************************************~

        dim                                                              ~
            aud_key$25,                  /* PRIMARY KEY                */~
            aud_key1$19,                 /* ALTERNATE KEY              */~
            aud_dte$8,                   /* Date of Sales Order        */~
            sav_dte$6,                   /* Save Date                  */~
            aud_so$8,                    /* Sales Order Number         */~
            aud_cust$9,                  /* Customer Code              */~
            sav_cust$9,                  /* Customer Code              */~
            aud_ln$2,                    /* Sales Order Line Item      */~
            aud_part$25,                 /* Sales Order Part No.       */~
            aud_qty$10,                  /* Line Item Qty              */~
            aud_amt$10,                  /* Line Item Amount           */~
            aud_tot1$10,                 /* Total Quantity for Date    */~
            aud_tot2$10,                 /* Total Amount for Date      */~
            aud_qty$(10)10,              /*                            */~
            aud_amt$(10)10,              /*                            */~
            aud_qty(10),                 /*                            */~
            aud_amt(10),                 /*                            */~
            cust_name$30,                /* CUSTOMER NAME - SHIP TO    */~
            cust_city$18,                /* CUSTOMER CITY - SHIP TO    */~
            cust_st$2,                   /* CUSTOMER STATE- SHIP TO    */~
            aud_userid$3,                /* USERID                     */~
            aud_desc$20,                 /* COMMENT                    */~
            aud_filler$11,               /* FILLER AREA                */~
            compname$60,                 /* Company Name               */~
            date$8,                      /* Run Date                   */~
            plowkey$50,                  /* Plow Key                   */~
            runtime$8                    /* REPORT RUN TIME            */

        dim f2%(10),                     /* = 0 if the file is open    */~
            f1%(10),                     /* = 1 if READ was successful */~
            axd$4,                       /* Alt Indices Byte           */~
            rslt$(10)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/06/97 - Sales Order Audit Report      "
        REM *************************************************************

            mat f2% = con
                     /* The variable F2%() should not be modified.     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCSOAUD ! Sales Order Audit File.                  *~
            * #2  ! CUSTOMER ! CUSTOMER MASTER FILE.                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCSOAUD",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =    7, keylen =  19


            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30,         ~
                            key 2,  keypos = 424,  keylen =   9,         ~
                            key 3,  keypos = 771,  keylen =   9,         ~
                            key 4,  keypos = 780,  keylen =   9

            u3% = 2%
            call "ASKUSER" (u3%, "** DAILY SALES ORDER AUDIT REPORT **", ~
                            "Press PF-16 to Continue with this Report",  ~
                            "-- OR --",                                  ~
                            "Press RETURN to Cancel and Return to Menu")
            if u3% <> 16% then exit_program

            call "OPENFILE" (#1, "SHARE", f2%(1), rslt$(1), axd$)
            call "OPENFILE" (#2, "SHARE", f2%(2), rslt$(2), axd$)
            if f2%(1) = 0% then L09000
                u3% = 2%
                call "ASKUSER" (u3%,"**DAILY SALES ORDER AUDIT REPORT**",~
                                "Unable to open report file (APCSOAUD);",~
                                "Either is in use or doesn't exist.",    ~
                                "Press RETURN to return to menu...")
                goto exit_program


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            print page
            call "SHOSTAT" ("Printing 'DAILY' Sales Order Audit Rpt.")

            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (runtime$)
            call "COMPNAME" (12%, compname$, f1%(2))
            call "SETPRNT" ("APCSO1", " ", 0%, 0%)
            select printer (134)
            line% = 857%
            aud_tot1, aud_tot2 = 0.0  : changes% = 0%
            sav_dte$ = " "
            aud_key$ = all(hex(00))
            for i% = 1% to 10%
                aud_qty(i%) = 0.0
                aud_amt(i%) = 0.0
            next i%

        REM *************************************************************~
            *                P R I N T    R E P O R T                   *~
            *-----------------------------------------------------------*~
            * List all changes found in report file.                    *~
            *************************************************************

        report_loop
            read #1,key > aud_key$, eod goto L10090
            goto L10120
L10090:         gosub report_totals
                goto  clear_file

L10120:  init(" ") aud_dte$, aud_so$, aud_cust$, aud_ln$, aud_part$,     ~
                   aud_qty$, aud_amt$, aud_userid$, aud_desc$
         aud_qty, aud_amt = 0.0

         get #1 using L10200, aud_dte$, aud_cust$, aud_so$, aud_ln$,      ~
                             aud_part$, aud_qty, aud_amt, aud_userid$,   ~
                             aud_desc$, aud_filler$

L10200:   FMT CH(6), CH(9), CH(8), CH(2), CH(25), 2*PD(14,4), CH(3),     ~
              CH(20)
            str(aud_key$,1%,6%)  = aud_dte$
            str(aud_key$,7%,9%)  = aud_cust$
            str(aud_key$,16%,8%) = aud_so$
            str(aud_key$,24%,2%) = aud_ln$

            if sav_dte$ <> " " then goto L10290
               sav_dte$ = str(aud_dte$,1%,6%)
L10290:     if sav_dte$ = aud_dte$ then goto L10340
               sav_dte$ = str(aud_dte$,1%,6%)
               gosub report_totals
               gosub page_heading

L10340:     call "DATEFMT" (aud_dte$)
            convert aud_qty to aud_qty$, pic(###,###.##)
            convert aud_amt to aud_amt$, pic(###,###.##)

            if line% > 55% then gosub page_heading
            gosub lookup_cust
            print using L11170, aud_dte$, aud_so$, aud_cust$, aud_ln$,    ~
                              aud_part$, aud_qty$, aud_amt$, aud_userid$,~
                              aud_desc$
            if sav_cust$ = aud_cust$ then goto L10430
               sav_cust$ = aud_cust$
               print using L11190, cust_name$, cust_city$, cust_st$
               line% = line% + 1%

L10430:     print
            line% = line% + 2%
                                         /* ONLY UPDATE NEW S.O. */
            if str(aud_desc$,12%,5%) <> "ADDED" then goto L10540
              aud_tot1 = round( aud_tot1 + aud_qty, 2)
              aud_tot2 = round( aud_tot2 + aud_amt, 2)
              inc% = 4%
              convert str(aud_part$,1%,1%) to inc%, data goto L10500

L10500:       if len(aud_part$) < 19 then inc% = 4%
              aud_qty(inc% + 1%) = round(aud_qty(inc% + 1%) + aud_qty,2)
              aud_amt(inc% + 1%) = round(aud_amt(inc% + 1%) + aud_amt,2)

L10540:     changes% = changes% + 1%
            goto report_loop


            report_totals
               convert aud_tot1 to aud_tot1$,pic(###,###.##)

               convert aud_tot2 to aud_tot2$,pic(###,###.##)
               for i% = 1% to 10%
                  convert aud_qty(i%) to aud_qty$(i%),pic(###,###.##)
                  convert aud_amt(i%) to aud_amt$(i%),pic(###,###.##)
               next i%

               gosub page_heading

               print using L11210
               print
               print using L11230, aud_qty$(1%), aud_amt$(1%)
               print using L11240, aud_qty$(2%), aud_amt$(2%)
               print using L11250, aud_qty$(3%), aud_amt$(3%)
               print using L11260, aud_qty$(4%), aud_amt$(4%)
               print using L11270, aud_qty$(5%), aud_amt$(5%)
               print using L11280, aud_qty$(6%), aud_amt$(6%)
               print using L11290, aud_qty$(7%), aud_amt$(7%)
               print using L11300, aud_qty$(8%), aud_amt$(8%)
               print using L11310, aud_qty$(9%), aud_amt$(9%)
               print using L11320, aud_qty$(10%), aud_amt$(10%)
               print using L11330
               print using L11340, aud_tot1$, aud_tot2$
               aud_tot1, aud_tot2 = 0.0
               for i% = 1% to 10%
                 aud_qty(i%), aud_amt(i%) = 0.0
               next i%
            return


            page_heading
                sav_cust$ = " "
                page% = page% + 1%
                line% = 7%
                print page
                print using L11050, date$, runtime$, compname$
                print using L11080, page%
                print
                print using L11110
                print using L11140
                print
                return




L11050: %RUN DATE: ######## ########                 ####################~
        ~########################################                    APCSO~
        ~AUD
L11080: %                                            D A I L Y   S A L E ~
        ~S   A U D I T   R E P O R T                              PAGE: ##~
        ~##
L11110: %ORD DATE  SALES ORD   CUSTOMER    LINE   <----- PART NUMBER ----~
        ~->     QUANTITY     LINE AMT   USER ID   <---- COMMENTS ---->

L11140: %--------  ---------   ---------   ----   -----------------------~
        ~--   ----------   ----------   -------   --------------------

L11170: %########   ########   #########    ##    #######################~
        ~##   ##########   ##########     ###     ####################
L11190: %                      SHIP TO: ##############################  C~
        ~ITY: ##################  STATE: ##
L11210: %TOTALS FOR DATE (NEW ORDERS)   S.O  QUANTITIES     S.O AMOUNTS

L11230: % (0) ALUMINUM FRAM SCREEN         ##########        ##########
L11240: % (1) STORM WINDOW                 ##########        ##########
L11250: % (2) STORM DOORS                  ##########        ##########
L11260: % (3) PATIO DOORS                  ##########        ##########
L11270: % (4) COMPONENT PARTS              ##########        ##########
L11280: % (5) ALUMINUM PRIME WINDOWS       ##########        ##########
L11290: % (6) VINYL REPLACEMENT WINDOWS    ##########        ##########
L11300: % (7) VINYL PRIME WINDOWS          ##########        ##########
L11310: % (8) VINYL CASEMENT WINDOWS       ##########        ##########
L11320: % (9) BAYS AND BOW WINDOWS         ##########        ##########
L11330: %                                  ----------        ----------
L11340: %                                  ##########        ##########

        clear_file
            call "SHOSTAT" ("REPORT COMPLETED")
            if changes% > 0% then L11440
              call "ASKUSER" (2%, "** DAILY SALES ORDER AUDIT REPORT **",~
                                "There are No Sales Orders on File.",    ~
                                " ", "Press any PF Key to exit.")
                goto exit_program

L11440:     u3% = 2%
            call "ASKUSER" (u3%, "** DAILY SALES ORDER AUDIT REPORT **", ~
                     "Enter PF-8 to CLEAR Audit Report File,",           ~
                     "- OR -", "Enter RETURN to NOT CLEAR File.")
            if u3%  = 0% then exit_program
            if u3% <> 8% then L11440
                call "SHOSTAT" ("Clearing Audit Report File")
                plowkey$ = hex(00)
                call "DELETE" (#1, plowkey$, 0%)




        lookup_cust                           /* Look Up Customer Info */
            read #2,key = aud_cust$, using L60040, cust_name$,            ~
                                 cust_city$, cust_st$, eod goto L60050
L60040:        FMT POS(253), CH(30), POS(403), CH(18), CH(2)
L60050: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" ("APCSO1", " ", 0%, 1%)
            end
