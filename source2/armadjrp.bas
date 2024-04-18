        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M   AAA   DDDD   JJJJJ  RRRR   PPPP    *~
            *  A   A  R   R  MM MM  A   A  D   D     J   R   R  P   P   *~
            *  AAAAA  RRRR   M M M  AAAAA  D   D     J   RRRR   PPPP    *~
            *  A   A  R  R   M   M  A   A  D   D  J  J   R  R   P       *~
            *  A   A  R   R  M   M  A   A  DDDD    JJ    R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMADJRP - Prints A/R Adjustments Report.                 *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/17/86 ! Original                                 ! ERN *~
            * 07/26/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            billto$9,                    /* Bill-to Customer Code      */~
            discdue$(2)8,                /* Discount Due Date          */~
            discpct$(2)8,                /* Cash Disc %, Was/Is        */~
            change_date$8,               /* Date Changed               */~
            compname$60,                 /* Company Name               */~
            date$8,                      /* Run Date                   */~
            grace$(2)2,                  /* Grace Days                 */~
            name$30,                     /* Bill-to Name               */~
            netdue$(2)8,                 /* Net Due Date               */~
            plowkey$50,                  /* Plow Key                   */~
            po$(2)16,                    /* PO                         */~
            runtime$8,                   /* Report Run Time            */~
            stlmnt$14,                   /* Settlement Number          */~
            userid$3                     /* User who made changes      */~

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            axd$4,                       /* Alt Indices Byte           */~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con
                     /* The variable F2%() should not be modified.     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMADJRP ! A/R Adjustments Report File              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARMADJRF",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  28

            u3% = 2%
            call "ASKUSER" (u3%, "** A/R ADJUSTMENTS REPORT **",         ~
                            "Press PF-16 to Continue with this Report",  ~
                            "-- OR --",                                  ~
                            "Press RETURN to Cancel and Return to Menu")
            if u3% <> 16% then exit_program

            call "OPENFILE" (#1, "IO   ", f2%(1), rslt$(1), axd$)
            if f2%(1) = 0% then L09000
                u3% = 2%
                call "ASKUSER" (u3%, "** A/R ADJUSTMENTS REGISTER **",   ~
                                "Unable to open report file (ARMADJRF);",~
                                "Either is in use or doesn't exist.",    ~
                                "Press RETURN to return to menu...")
                goto exit_program


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            print page
            call "SHOSTAT" ("Printing A/R ADJUSTMENTS REGISTER")

            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (runtime$)
            call "COMPNAME" (12%, compname$, f1%(2))
            call "SETPRNT" ("ARM002", " ", 0%, 0%)
            select printer (134)
            line% = 857%


        REM *************************************************************~
            *                P R I N T    R E P O R T                   *~
            *-----------------------------------------------------------*~
            * List all changes found in report file.                    *~
            *************************************************************

        report_loop
            call "READNEXT" (#1, f1%(1))
            if f1%(1) = 1% then L10130
                gosub report_totals
                goto  clear_file

L10130:     get #1 using L10180, billto$, stlmnt$, name$, change_date$,   ~
                                userid$, discpct$(1), discdue$(1),       ~
                                grace$(1), netdue$(1), po$(1),           ~
                                discpct$(2), discdue$(2), grace$(2),     ~
                                netdue$(2), po$(2)
L10180:         FMT CH(9), CH(12), XX(7), CH(30), CH(6), CH(3),          ~
                    CH(6), CH(8), CH(2), CH(8), CH(16),                  ~
                    CH(6), CH(8), CH(2), CH(8), CH(16)
            stlmnt$ = str(stlmnt$,,8) & "-" & str(stlmnt$,9,2) & "-" &   ~
                                              str(stlmnt$, 11)
            call "DATEFMT" (change_date$)
            call "DATEFMT" (discdue$(1))
            call "DATEFMT" (netdue$(1))
            call "DATEFMT" (discdue$(2))
            call "DATEFMT" (netdue$(2))
            changes% = changes% + 1%
            if discpct$(1) = discpct$(2) then init (" ")discpct$()
            if discdue$(1) = discdue$(2) then init (" ")discdue$()
            if grace$  (1) = grace$  (2) then init (" ")grace$()
            if netdue$ (1) = netdue$ (2) then init (" ")netdue$()
            if po$     (1) = po$     (2) then init (" ")po$()
            if line% > 55% then gosub page_heading
            print using L10820, billto$, name$, stlmnt$, change_date$,    ~
                               userid$,                                  ~
                               discpct$(1), discdue$(1), grace$(1),      ~
                               netdue$(1), po$(1)
            print using L10850, discpct$(2), discdue$(2), grace$(2),      ~
                               netdue$(2), po$(2)
            print
            line% = line% + 3%
            goto report_loop


            report_totals
                if changes% = 0% then return
                     print
                     print using L10880, changes%
                     print
                     print "** END OF REPORT **"
                     return


            page_heading
                page% = page% + 1%
                line% = 7%
                print page
                print using L10670, date$, runtime$, compname$
                print using L10700, page%
                print
                print using L10730
                print using L10760
                print using L10790
                return




L10670: %RUN DATE: ######## ########                 ####################~
        ~########################################             ARMADJRP-CRC~
        ~002
L10700: %                                                  A / R   A D J ~
        ~U S T M E N T S   R E G I S T E R                        PAGE: ##~
        ~##
L10730: % BILL-TO                                                 CHANGES~
        ~ MADE          CASH   DISCOUNT  GRACE    NET

L10760: %CUSTOMER  CUSTOMER NAME                  SETTLEMENT NO.     ON  ~
        ~  BY          DISC %  DUE DATE   DAYS  DUE DATE  CUSTOMER PO

L10790: %--------- ------------------------------ --------------  -------~
        ~- ---         ------  --------  -----  --------  ----------------

L10820: %######### ############################## ##############  #######~
        ~# ###   FROM  ######  ########    ##   ########  ################

L10850: %                                                                ~
        ~          TO  ######  ########    ##   ########  ################

L10880: %NUMBER OF CHANGES LISTED: #####



        clear_file
            call "SHOSTAT" ("REPORT COMPLETED")
            if changes% > 0% then L11000
                call "ASKUSER" (2%, "** A/R ADJUSTMENTS REPORT **",      ~
                                "There are No Adjustments on file.",     ~
                                " ", "Press any PF Key to exit.")
                goto exit_program

L11000:     u3% = 2%
            call "ASKUSER" (u3%, "** A/R ADJUSTMENTS REPORT **",         ~
                     "Enter PF-8 to CLEAR Adjustment's Report File,",    ~
                     "- OR -", "Enter RETURN to NOT CLEAR File.")
            if u3%  = 0% then exit_program
            if u3% <> 8% then L11000
                call "SHOSTAT" ("Clearing Adjustment's Report File")
                plowkey$ = hex(00)
                call "DELETE" (#1, plowkey$, 0%)


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" ("ARM002", " ", 0%, 1%)
            end
