        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSSS  L      M   M  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  S      L      MM MM    I    NN  N  P   P  U   U    T     *~
            *   SSSS  L      M M M    I    N N N  PPPP   U   U    T     *~
            *       S L      M   M    I    N   N  P      U   U    T     *~
            *   SSSS  LLLLL  M   M  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SLMINPUT - Manage Salesmen codes and commission rate      *~
            *            structure.  Commissions maintained in a        *~
            *            'GENCODES' style- no start over on rates!      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/29/86 ! ORIGINAL                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            addr$(3)30,                  /* Address                    */~
            ctgy$4,                      /* Part Category Code         */~
            code$(15)29,                 /* Category or Part           */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor Location for Edit   */~
            date$8,                      /* Date for Screen Display    */~
            descr$(15)30,                /* Ctgy, Part Description     */~
            dfac$(15)1,                  /* Description FACs           */~
            edtmessage$79,               /* Edit Screen Message        */~
            eof$3,                       /* End-of-File Flag           */~
            errormsg$79,                 /* Error Message              */~
            fac1$(15)1, fac2$(15)1,      /* Column FACs                */~
            fac3$(15)1,                  /*                            */~
            from$35,                     /* From Salesman for copy     */~
            hdr1$30, hdr2$22,            /* Column Headings            */~
            hdr3$5 , hdr4$18,            /*                            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Input Message              */~
            lfac$(15)1,                  /* Regular Facs               */~
            line2$79,                    /* Second Screen Line         */~
            name$30,                     /* Salesman Name              */~
            part$25,                     /* Part Number                */~
            pf5$14 , pf8$26 , pf12$18,   /* PF Key Descriptions        */~
            pf14$18, pf16$16,            /*                            */~
            pfdescr$(3,2)79,             /* PF Key Descriptions        */~
            pfkeys$(2)17,                /* PF Keys (HEX)              */~
            phone$10,                    /* Phone Number               */~
            phonefmt$12,                 /* Phone Number- formatted    */~
            plowkey$50,                  /* Plow Key                   */~
            plowkey1$50,                 /* 2nd level plow key         */~
            rate$(15)5,                  /* Commission Rates           */~
            runtime$8,                   /* Report run time            */~
            salesman$4,                  /* Salesman Code              */~
            to$(15)1,                    /* What to apply Rate to code */~
            todescr$(15)16,              /* What to apply Rate to descr*/~
            todescrs$(5)16,              /* Apply to descriptors       */~
            vf$200                       /* Variable Fields            */

        dim f1%(5)                       /* = 1 IF READ WAS SUCCESSFUL */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard costs to 12            "


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SLMMASTR ! Salesmen Master File                     *~
            * #2  ! SLMMAST2 ! Salesmen Commision Structure             *~
            * #3  ! CATEGORY ! Part Category File                       *~
            * #4  ! HNYMASTR ! Parts Master File                        *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1 , "SLMMASTR",                                      ~
                        varc, indexed,                                   ~
                        recsize = 600,                                   ~
                        keypos =    1,  keylen = 4

            select #2 , "SLMMAST2",                                      ~
                        varc, indexed,                                   ~
                        recsize = 100,                                   ~
                        keypos =    1,  keylen = 33

            select #3 , "CATEGORY",                                      ~
                        varc, indexed,                                   ~
                        recsize = 200,                                   ~
                        keypos =    1,  keylen = 4

            select #4 , "HNYMASTR",                                      ~
                        varc, indexed,                                   ~
                        recsize = 900,                                   ~
                        keypos =      1, keylen =  25,                   ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos =  90, keylen = 4, dup

        call "SHOSTAT" ("Opening Files, One Moment Please.")
            call "OPENCHCK" (#1, u3%, 0%, 100%, " ")
            call "OPENCHCK" (#2, u3%, 0%, 200%, " ")
            call "OPENCHCK" (#3, u3%, 0%,   0%, " ")
            call "OPENCHCK" (#4, u3%, 0%,   0%, " ")


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$  = date  :  call "DATEFMT" (date$)

            pfdescr$(1,1) = "(2)First Screen            (3)Copy Rates" & ~
                            " from another Salesman (13)Instructions"
            pfdescr$(2,1) = "(5)Next Screen                          " & ~
                            "                       (15)Print Screen"
            pfdescr$(3,1) = "(8)Move to Item Entered   (12)Delete ALL" & ~
                            " Rates                 (16)Return      "

            pfdescr$(1,2) = "(ENTER)SAVE DATA                        " & ~
                            "                       (13)Instructions"
            pfdescr$(2,2) = "(1)Start Line Over        (12)Delete Lin" & ~
                            "e                      (15)Print Screen"
            pfdescr$(3,2) = "                                        " & ~
                            "                                       "

            pfkeys$(1) = hex(ff0203ff05ffff08ffffff0c0dff0f1000)
            pfkeys$(2) = hex(0102ffffffffff08ffffff0c0dff0fff00)

            hdr1$ = "Category Code -or- Part Code"
            hdr2$ = "Description (Short)"
            hdr3$ = "Rate%"
            hdr4$ = "  Apply Rate to"

            str(line2$,62) = "SLMINPUT: " & str(cms2v$,,8)

            todescrs$(1) = "Gross Extension "
            todescrs$(2) = "Netted Extension"
            todescrs$(3) = "Standard Cost   "
            todescrs$(4) = "Margin per Gross"
            todescrs$(5) = "Margin per Net'd"

            call "COMPNAME" (12%, company$, u3%)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * INPUT MODE for first screen.                              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, salesman$, name$, addr$(), ~
                      phone$, str(line2$,,60), vf$

            for fieldnr% = 1% to 4%
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10210
L10140:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 14 and fieldnr% = 1 then print_report
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10140
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
L10210:         next fieldnr%


*        Do Input for Variable Fields
            str(line2$,,60) = "Salesman: " & salesman$ & ", " & name$
            call "VFINPSUB" ("SLMMASTR", "I", "Manage Salesman File",    ~
                             str(line2$,,60), "NN", vf$, keyhit%)
            if keyhit% = 1% then inputmode

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for the first screen.      *~
            *************************************************************
        editmode
            str(line2$,,60) = "Salesman: " & salesman$ & ", " & name$
            errormsg$, inpmessage$ = " "
L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then       edit_vfs
                  if keyhit%  =  8 then       manage_rates
                  if keyhit%  = 12 then gosub delete_salesman
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
L11130:     fieldnr% = cursor%(1) - 5%
                if fieldnr% = 4% or fieldnr% = 5% then fieldnr% = 3%
                if fieldnr% = 6%                  then fieldnr% = 4%
            if fieldnr% < 2% or fieldnr% > 4% then L11060

L11180:     gosub'051(fieldnr%)
            gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11180
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11180
                  temp% = cursor%(1) - 5%
                  if temp% = 4% or temp% = 5% then temp% = 3%
                  if temp% = 6%               then temp% = 4%
                  if fieldnr% = temp% then L11060 else L11130


        edit_vfs   /* Do Edit for Variable Fields  */
            call "VFINPSUB" ("SLMMASTR", "E", "Manage Salesman File",    ~
                             str(line2$,,60), "YN", vf$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then editmode
            if keyhit% = 16% then datasave
            goto editmode

        delete_salesman
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "D E L E T E",                     ~
                            "Enter PF-16 to DELETE Salesman", "-OR-",    ~
                            "Press RETURN to CANCEL delete.")
            if keyhit1% <> 16% then return
                call "DELETE" (#1, salesman$, 4%)  /* Master File      */
                call "DELETE" (#2, salesman$, 4%)  /* Commission Rates */
                return clear all
                goto inputmode


        REM *************************************************************~
            *       M A N A G E   C O M M I S S I O N   R A T E S       *~
            *-----------------------------------------------------------*~
            *  Manages rate structure ala 'GENCDSIN'.                   *~
            *************************************************************
        manage_rates
            gosub datasave2    /* Save Salesman data                   */
            gosub load_first   /* Load up first screen of rates        */
            str(line2$,,60) = "Salesman: " & salesman$ & ", " & name$

        screen_loop
            errormsg$ = " "
L12110:     gosub'102(0%)      /* Get what is to be done               */
                if keyhit%  =  2 then gosub load_first
                if keyhit%  =  3 then       copy_rates
                if keyhit%  =  5 then gosub load_next
                if keyhit%  =  8 then gosub load_stated
                if keyhit%  = 12 then gosub delete_all_rates
                if keyhit%  = 16 then editmode
                if keyhit% <>  0 then L12110
            errormsg$ = " "
            l% = cursor%(1) - 5%
            if l% <  1% or l% > last% then L12110
            if l% <> 1% then L12420       /* Change code already input  */

*        Add -or- Change a specifically mentioned code- Edit data 1st.
            if code$(1) <> " " then L12260
                errormsg$ = "Both Category and Part cannot be blank."
                goto L12110
L12260:     ctgy$ = str(code$(1),,4) : part$ = str(code$(1),5)
            if ctgy$ = " " or part$ = " " then L12300
                errormsg$ = "Can not enter both Category and Part."
                goto L12110
L12300:     if ctgy$ = " " then L12360
            /* Validate Category Code    */
                if ctgy$ <> "****" then L12320
                     code$(l%) = ctgy$
                     goto L12390
L12320:         call "GETCODE" (#3, ctgy$, " ", 0%, 0.30, f1%(3))
                if f1%(3) = 1% then code$(l%) = ctgy$
                if f1%(3) = 1% then L12390
                     errormsg$ = "Category not on file."  : goto L12110
            /* Validate Part Code        */
L12360:         call "GETCODE" (#4, part$, " ", 0%, 0.32, f1%(4))
                if f1%(4) = 1% then code$(l%) = "    " & part$
                if f1%(4) = 1% then L12390
                     errormsg$ = "Part Code not on file." : goto L12110
L12390:     gosub load_specific

*        Get entry and then test data.
L12420:     gosub'052(l%)      /* Set FACs for line entry              */
L12430:     gosub'102(l%)      /* Get field entries.                   */
                if keyhit%  =  1 then gosub startover_line
                if keyhit%  = 12 then gosub delete_rate
                if keyhit% <>  0 then L12430
            gosub'152(l%)      /* Edit fields                          */
                if errormsg$ <> " "  then L12430
                gosub save_rate
                goto screen_loop


        copy_rates   /* Copy Commission rates from another Salesman    */
            from$ = hex(00)
            errormsg$ = hex(06) & "Select Salesman to copy from"
            call "GETCODE" (#1, from$, errormsg$, 0%, .3, f1%(1))
            if f1%(1) = 0% or from$ = salesman$ then screen_loop
            errormsg$ = "Copying Rates from Salesman " & from$ & ", " &  ~
                        str(errormsg$,,30)
            print page
            call "SHOSTAT" (errormsg$)

         copy_loop
            call "PLOWNEXT" (#2, from$, 4%, f1%(2))
            if f1%(2) = 1% then L12750
                gosub load_first
                goto screen_loop
L12750:     put #2 using L12760, salesman$
L12760:         FMT CH(4)
            write #2, eod goto L12780
L12780:     goto copy_loop


        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            * --------------------------------------------------------- *~
            * Print Salesman Listing.  Printing of commision rates is   *~
            * optional at best.                                         *~
            *************************************************************
        print_report
L14070:     keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "REPORT OPTIONS",                  ~
                            "(1)Do not print -- Return to editing.",     ~
                            "(2)Print Report (No Commission Rates)",     ~
                            "(3)Print Report (w/ Commission Rates)" )
            if keyhit1% = 1% then inputmode
            if keyhit1% = 2% or keyhit1% = 3% then L14150 else L14070

L14150:     call "TIME" (runtime$)
            call "SETPRNT" ("SLM001", " ", 0%, 0%)
            call "SHOSTAT" ("PRINTING SALESMAN LISTING")
            page% = 0%  :  line% = 857%
            plowkey$ = hex(00)
            select printer(134)

        report_loop
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then end_report

            get #1 using L14270, salesman$, name$, addr$(), phone$
L14270:         FMT CH(4), CH(30), 3*CH(30), CH(10)
            section% = 0%
            if line% > 51% then gosub page_heading
            phonefmt$ = str(phone$,,3) & "-" & str(phone$,4,3) & "-" &   ~
                        str(phone$, 7)
            print using L15020, salesman$, name$, addr$(1), phonefmt$
            print using L15050, addr$(2)
            print using L15050, addr$(3)
            print
            line% = line% + 4%
            if keyhit1% <> 3% then report_loop     /* No Rates         */

            section% = 1%
            if line% > 51% then gosub page_heading else gosub rate_heading
            plowkey1$ = str(salesman$) & hex(00)
            ratecount% = 0%
          rate_loop
            call "PLOWNEXT" (#2, plowkey1$, 4%, f1%(2))
            if f1%(2) = 1% then L14480
                if ratecount% = 0% then print using L15170
                if ratecount% = 0% then line% = line% + 1%
                print
                line% = line% + 1%
                goto report_loop
L14480:     get #2 using L14490, code$(1), rate, to$(1)
L14490:         FMT XX(4), CH(29), PD(14,4), CH(1)
            l% = 1%  :  gosub describe_code
            convert rate to rate$(1), pic(##.00)
            if line% > 56% then gosub page_heading
            print using L15140, str(code$(1),,4), str(code$(1),5),        ~
                               descr$(1), rate$(1), todescr$(1)
            line% = line% + 1%
            ratecount% = 1%
            goto rate_loop


        page_heading
            page% = page% + 1%
            line% = 6%
            print page
            print using L14900, date$, runtime$, company$
            print using L14930, page%
            print
            print using L14960
            print using L14990
            if section% = 0% then return

        rate_heading
            print
            print using L15080
            print using L15110
            line% = line% + 3%
            return


        end_report
            print
            print "END OF REPORT"
            close printer
            call "SETPRNT" ("SLM001", " ", 0%, 1%)
            select ws
            goto inputmode


L14900: %RUN DATE: ######## ########            #########################~
        ~###################################               SLMINPUT-SLM001

L14930: %                                                         SALESMA~
        ~N LISTING                                             PAGE: ###  ~

L14960: %CODE  SALESMAN NAME                   ADDRESS                   ~
        ~      PHONE NUMBER

L14990: %----  ------------------------------  --------------------------~
        ~----  ------------

L15020: %####  ##############################  ##########################~
        ~####  ############

L15050: %                                      ##########################~
        ~####

L15080: %        COMMISSION RATES:  CATEGORY PART CODE                 DE~
        ~SCRIPTION                    %-RATE  RATE APPLIED TO

L15110: %                           -------- ------------------------- --~
        ~---------------------------  ------  ----------------

L15140: %                             ####   ######################### ##~
        ~###########################   ##.##  ################

L15170: %                           NO RATES DEFINED


        REM *************************************************************~
            *                  D A T A   S A V E                        *~
            * --------------------------------------------------------- *~
            * Control writing of the salesman data to SLMMASTR.         *~
            *************************************************************
        datasave
            gosub save_data
            goto inputmode

        datasave2    /* Executed prior to managing commissions         */
            print at (04,02), "Saving Salesman Record...."
            gosub save_data
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES Fields for the Page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20150,         /* Salesman Code    */~
                                    L20190,         /* Name             */~
                                    L20230,         /* Address          */~
                                    L20270          /* Phone Number     */

                     return

L20150
*        Default/Enable for SALESMAN CODE
            inpmessage$ = "Enter Saleman Code."
            return

L20190
*        Default/Enable for NAME
            inpmessage$ = "Enter Salesman's Name."
            return

L20230
*        Default/Enable for ADDRESS
            inpmessage$ = "Enter Salesman's Address."
            return

L20270
*        Default/Enable for PHONE NUMBER
            inpmessage$ = "Enter Salesman's Phone Number."
            return

        REM *************************************************************~
            *        S E T   F A C S   F O R   E N T R Y                *~
            *-----------------------------------------------------------*~
            *  Set FACs on for line modification.                       *~
            *************************************************************

        deffn'052(l%)
            init (hex(8c)) fac1$(), fac2$(), fac3$(), dfac$()
            fac1$(l%), dfac$(l%) = hex(ac)   /* Code and Descrs        */
            fac2$(l%), fac3$(l%) = hex(82)   /* Rate and Apply-to      */
          return

        REM *************************************************************~
            *          S T A R T   O V E R  -  L I N E                  *~
            *-----------------------------------------------------------*~
            *  Abort line modfification. Gives operator a second chance.*~
            *  Note that if code is displayed on screen we must reload  *~
            *  so that it's values revert to what they were before they *~
            *  we modified.                                             *~
            *************************************************************
        startover_line
L29090:     keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "* ABORT CURRENT ENTRY *",         ~
                            "PRESS (1) TO RETURN TO DISPLAY", "- OR -",  ~
                          "PRESS (ENTER) TO ABORT CHANGES MADE TO LINE")

            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29090

            return clear        /* Enter- Abort Changes       */
            if l% <>  1% then L29210
                gosub clear_first
                goto screen_loop
L29210:     code$(1) = code$(2)
            gosub load_stated
            goto screen_loop


        startover    /* Regular Type for Regular Type Screen           */
L29280:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3%  = 1% then return
            if u3% <> 0% then L29280
                return clear all
                goto inputmode


L30000: REM *************************************************************~
            *          L O A D   S A L E S M A N   R E C O R D          *~
            * --------------------------------------------------------- *~
            * Loads Salesman record from SLMMASTR.                      *~
            *************************************************************

            call "READ100" (#1, salesman$, f1%(1))
            if f1%(1) = 0% then return
                get #1 using L30150, salesman$, name$, addr$(), phone$,   ~
                                    vf$
                return

L30150:     FMT CH( 4),                  /* Salesman Code              */~
                CH(30),                  /* Name                       */~
                3*CH(30),                /* Address                    */~
                CH(10),                  /* Phone Number               */~
                CH(200)                  /* Variable Fields            */

        REM *************************************************************~
            *           L O A D   R A T E   R O U T I N E S             *~
            * --------------------------------------------------------- *~
            * Move a screen's worth of data into display arrays.        *~
            *************************************************************

        load_first        /* Load from top of file                     */
            plowkey$ = str(salesman$) & hex(00)
            goto load_rates

        load_stated       /* Load from operator specified code         */
            plowkey$ = str(salesman$) & code$(1) & hex(00)
            gosub clear_first
            goto load_rates

        load_next         /* Load from last code listed                */
            if eof$ <> "YES" then L31190
                errormsg$ = "ALREADY AT END OF FILE."
                return
L31190:     if last% = 1% then load_first
            plowkey$ = str(salesman$) & code$(last%) & hex(00)

        load_rates
            errormsg$ = " "
            last% = 1%
            eof$  = "NO"
            init (" ") code$(), descr$(), rate$(), to$(), todescr$()

          load_rates_loop
            call "PLOWNEXT" (#2, plowkey$, 4%, f1%(2))
            if f1%(2) = 1% then L31330
                eof$ = "YES"
                return
L31330:     last% = last% + 1%  :  l% = last%
            gosub get_record   /* Load record and format for display   */
          if l% < 15% then load_rates_loop else return


        load_specific     /* Get specific code from file               */
                          /* If new, supply defaults.                  */
            call "READ100" (#2, str(salesman$) & str(code$(l%)), f1%(2))
            if f1%(2) <> 1% then L31440
                gosub get_record
                return
L31440:   /* Not on file-- setup with defaults               */
            descr$(l%), rate$(l%), to$(l%), todescr$(l%) = " "
            gosub describe_code
         return


        get_record   /* Get record from buffer and format for display  */
            get #2 using L31570, code$(l%), rate, to$(l%)
            convert rate to rate$(l%), pic(##.##)
            gosub describe_code
        return


L31570:         FMT  XX( 4),             /* Salesman Code              */~
                     CH(29),             /* Code (Category / Part )    */~
                     PD(14,4),           /* Commission Rate            */~
                     CH( 1)              /* Applt To Code              */


        describe_code     /* Fill in descriptions for codes            */
            ctgy$ = str(code$(l%),,4)
            part$ = str(code$(l%), 5)
            if ctgy$ <> "****" then L31660
                descr$(l%) = "All Not Mentioned"
                goto L31710
L31660:     if ctgy$ <> " " then                                         ~
                call "DESCRIBE" (#3, ctgy$, descr$(l%), 0%, f1%(3))
            if part$ <> " " then                                         ~
                call "DESCRIBE" (#4, part$, descr$(l%), 0%, f1%(4))

L31710: describe_apply_to
            todescr$(l%) = " "
            if to$(l%)   = " " then return
            convert to$(l%) to to%, data goto L31760
            todescr$(l%) = todescrs$(to%)
L31760:     return


        REM *************************************************************~
            *      S A V E   S A L E S M A N    R E C O R D             *~
            * --------------------------------------------------------- *~
            * Writes Salesman data to SLMMASTR.                         *~
            *************************************************************
        save_data
            call "READ101" (#1, salesman$, f1%(1))
            put #1 using L32130, salesman$, name$, addr$(), phone$, vf$,  ~
                                " ", " "
            if f1%(1) = 1% then rewrite #1 else write #1
            return


L32130:     FMT CH( 4),                  /* Salesman Code              */~
                CH(30),                  /* Name                       */~
                3*CH(30),                /* Address                    */~
                CH(10),                  /* Phone Number               */~
                CH(200),                 /* Variable Fields            */~
                CH(255), CH(11)          /* Filler (blanks)            */


        REM *************************************************************~
            *           S A V E   R A T E   O N    F I L E              *~
            * --------------------------------------------------------- *~
            *  Update file with code maintained (L%). After save, if    *~
            *  Code was on line 1 we redo screen with that code on top. *~
            *************************************************************
        save_rate
            call "READ101" (#2, str(salesman$) & str(code$(l%)), f1%(2))
            rate = 0 : convert rate$(l%) to rate, data goto L33080
L33080:     put #2 using L33260, salesman$, code$(l%), rate, to$(l%), " "
            if f1%(2) = 0 then write #2 else rewrite #2

         /* Now make sure that code added/changed appears on screen.   */
         /* Also accessed by DELETE_RATE routine.                      */
         screen_align
            if l%   <> 1 then L33200
            if code$(l%) > code$(last%) and last% < 15% then L33200
            if code$(l%) < code$(2) or code$(l%) > code$(last%) then L33220


L33200:     code$(1) = code$(2)      /* Keeps the screen the same  */
                goto L33230
L33220:     code$(1) = code$(l%)     /* Put modified code on top   */
L33230:     gosub load_stated        /* Start display at CODE$(1)  */
          return

L33260:         FMT  CH( 4),             /* Salesman Code              */~
                     CH(29),             /* Code (Category / Part)     */~
                     PD(14,4),           /* Commission Rate            */~
                     CH( 1),             /* Applied To                 */~
                     CH(48)              /* Blanks                     */


        REM *************************************************************~
            *             D E L E T E   R A T E ( S )                   *~
            *-----------------------------------------------------------*~
            *  Delete a Rate or all Rates after operator's approval.    *~
            *************************************************************
        delete_rate
L34060:     errormsg$ = "Item: " & code$(l%) & " (" & descr$(l%) & ")"
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "**** DELETE CODE ****",           ~
                            errormsg$,                                   ~
                            "(ENTER) RETURN -- DO NOT DELETE",           ~
                            "(PF 16) DELETE ITEM INDICATED  ")
            errormsg$ = " "
            if keyhit1%  =  0% then return
            if keyhit1% <> 16% then L34060
                call "DELETE" (#2, str(salesman$) & code$(l%), 33%)
L34160:         code$(1) = code$(l%)
                gosub screen_align
                return clear
                goto screen_loop


        delete_all_rates
L34230:     errormsg$ = "Salesman: " & salesman$ & " " & name$
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "**** DELETE RATES ****",          ~
                            errormsg$,                                   ~
                            "(ENTER) RETURN -- DO NOT DELETE",           ~
                            "(PF 16) DELETE ALL RATES       ")
            errormsg$ = " "
            if keyhit1%  =  0% then return
            if keyhit1% <> 16% then L34230

                call "DELETE" (#2, salesman$, 4%)    /* Bye Bye */
                goto L34160

        REM *************************************************************~
            *             M A I N   S C R E E N                         *~
            * --------------------------------------------------------- *~
            * INPUT AND EDIT for the main screen.                       *~
            *************************************************************

            deffn'101(fieldnr%)                    /* INPUT Mode       */
                init(hex(8c)) lfac$()
                pf5$, pf8$, pf12$, pf14$, pf16$ = " "
                if fieldnr%  = 1% then pf16$ = "(16)Exit Program"
                if fieldnr%  = 1% then pf14$ = "(14)Print Listing"
                goto L40240

            deffn'111(fieldnr%)                    /* EDIT Mode        */
                init(hex(8c)) lfac$()
                pf5$, pf8$, pf12$, pf14$, pf16$ = " "
                if fieldnr% <> 0% then L40240
                     init(hex(86)) lfac$()
                     pf5$  = "(5)Next Screen"
                     pf8$  = "(8)Manage Commission Rates"
                     pf12$ = "(12)Delete Code"
                     pf16$ = "(16)Save Data"
                     inpmessage$ = " "

L40240:     on fieldnr%  gosub       L40330,        /* Salesman Code    */~
                                     L40300,        /* Name             */~
                                     L40300,        /* Address          */~
                                     L40300         /* Phone Number     */
            goto L40400

L40300:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40330:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40400: accept                                                           ~
            at (01,02), "Manage Salesmen Master File",                   ~
            at (01,66), "Today: ",                                       ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Salesman Code",                                 ~
            at (06,30), fac(lfac$( 1)), salesman$               , ch(04),~
            at (07,02), "Salesman's Name",                               ~
            at (07,30), fac(lfac$( 2)), name$                   , ch(30),~
            at (08,02), "Salesman's Address",                            ~
            at (08,30), fac(lfac$( 3)), addr$(1)                , ch(30),~
            at (09,30), fac(lfac$( 3)), addr$(2)                , ch(30),~
            at (10,30), fac(lfac$( 3)), addr$(3)                , ch(30),~
            at (11,02), "Phone Number",                                  ~
            at (11,30), fac(lfac$( 4)), str(phone$,1,3)         , ch(03),~
            at (11,36), fac(lfac$( 4)), str(phone$,4,3)         , ch(03),~
            at (11,42), fac(lfac$( 4)), str(phone$,7,4)         , ch(04),~
            at (11,34), "-",   at (11,40), "-",                          ~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (22,20), fac(hex(8c)), pf5$ ,                             ~
            at (23,20), fac(hex(8c)), pf8$ ,                             ~
            at (22,45), fac(hex(8c)), pf12$,                             ~
            at (24,40), fac(hex(8c)), pf14$,                             ~
            at (22,65), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), fac(hex(8c)), pf16$,                             ~
               keys(hex(000105080c0d0e0f10)),                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40770
                  call "MANUAL" ("SLMINPUT")
                  goto L40400

L40770:        if keyhit% <> 15 then L40810
                  call "PRNTSCRN"
                  goto L40400

L40810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *        D A T A   E N T R Y   S C R E E N                  *~
            *-----------------------------------------------------------*~
            *  Combination Display and Manage Screen.                   *~
            *                                                           *~
            *  There are essentially 2 entry points --                  *~
            *     line =  0 - Get from operator what to do. The code    *~
            *                 field on the 'wild line' is the only      *~
            *                 modifiable field.                         *~
            *     line <> 0 - Get entries for the line.                 *~
            *************************************************************
        deffn'102(l%)
            if l% <> 0% then L42240

*        Case 1. Get what is to be done.
            init (hex(86)) fac1$()  :  fac1$(1) = hex(81) /* Cat/Part  */
            init (hex(84)) fac2$()                        /* Rates     */
            init (hex(8c)) fac3$()                        /* Apply To  */
            init (hex(84)) dfac$()                        /* Descrs    */
            mode% = 1%
            edtmessage$ = "Enter Code to Add/Change or position curs" &  ~
                          "or.  Leave blank to see codes on file."
            goto L42280

L42240
*        Case 2. Modify line L%.
            edtmessage$ = "Modify field(s) then press (ENTER) to save."
            mode% = 2%

L42280: accept                                                           ~
          at(01,02), "Manage Salesman's Commission Rate Structure",      ~
          at(01,66), "Today: ",                                          ~
          at(01,73), fac(hex(8c)), date$                        , ch(08),~
          at(02,02), fac(hex(ac)), line2$                       , ch(79),~
          at(04,02), fac(hex(94)), errormsg$,                            ~
                                                                         ~
          at(05,02), fac(hex(ac)), str(hdr1$),                           ~
          at(05,33), fac(hex(ac)), str(hdr2$),                           ~
          at(05,56), fac(hex(ac)), str(hdr3$),                           ~
          at(05,62), fac(hex(ac)), str(hdr4$),                           ~
                                                                         ~
          at(06,02), fac(fac1$( 1)), str(code$( 1),,4),                  ~
          at(07,02), fac(fac1$( 2)), str(code$( 2),,4),                  ~
          at(08,02), fac(fac1$( 3)), str(code$( 3),,4),                  ~
          at(09,02), fac(fac1$( 4)), str(code$( 4),,4),                  ~
          at(10,02), fac(fac1$( 5)), str(code$( 5),,4),                  ~
          at(11,02), fac(fac1$( 6)), str(code$( 6),,4),                  ~
          at(12,02), fac(fac1$( 7)), str(code$( 7),,4),                  ~
          at(13,02), fac(fac1$( 8)), str(code$( 8),,4),                  ~
          at(14,02), fac(fac1$( 9)), str(code$( 9),,4),                  ~
          at(15,02), fac(fac1$(10)), str(code$(10),,4),                  ~
          at(16,02), fac(fac1$(11)), str(code$(11),,4),                  ~
          at(17,02), fac(fac1$(12)), str(code$(12),,4),                  ~
          at(18,02), fac(fac1$(13)), str(code$(13),,4),                  ~
          at(19,02), fac(fac1$(14)), str(code$(14),,4),                  ~
          at(20,02), fac(fac1$(15)), str(code$(15),,4),                  ~
                                                                         ~
          at(06,07), fac(fac1$( 1)), str(code$( 1), 5),                  ~
          at(07,07), fac(fac1$( 2)), str(code$( 2), 5),                  ~
          at(08,07), fac(fac1$( 3)), str(code$( 3), 5),                  ~
          at(09,07), fac(fac1$( 4)), str(code$( 4), 5),                  ~
          at(10,07), fac(fac1$( 5)), str(code$( 5), 5),                  ~
          at(11,07), fac(fac1$( 6)), str(code$( 6), 5),                  ~
          at(12,07), fac(fac1$( 7)), str(code$( 7), 5),                  ~
          at(13,07), fac(fac1$( 8)), str(code$( 8), 5),                  ~
          at(14,07), fac(fac1$( 9)), str(code$( 9), 5),                  ~
          at(15,07), fac(fac1$(10)), str(code$(10), 5),                  ~
          at(16,07), fac(fac1$(11)), str(code$(11), 5),                  ~
          at(17,07), fac(fac1$(12)), str(code$(12), 5),                  ~
          at(18,07), fac(fac1$(13)), str(code$(13), 5),                  ~
          at(19,07), fac(fac1$(14)), str(code$(14), 5),                  ~
          at(20,07), fac(fac1$(15)), str(code$(15), 5),                  ~
                                                                         ~
          at(06,33), fac(dfac$( 1)),             descr$( 1),             ~
          at(07,33), fac(dfac$( 2)),             descr$( 2),             ~
          at(08,33), fac(dfac$( 3)),             descr$( 3),             ~
          at(09,33), fac(dfac$( 4)),             descr$( 4),             ~
          at(10,33), fac(dfac$( 5)),             descr$( 5),             ~
          at(11,33), fac(dfac$( 6)),             descr$( 6),             ~
          at(12,33), fac(dfac$( 7)),             descr$( 7),             ~
          at(13,33), fac(dfac$( 8)),             descr$( 8),             ~
          at(14,33), fac(dfac$( 9)),             descr$( 9),             ~
          at(15,33), fac(dfac$(10)),             descr$(10),             ~
          at(16,33), fac(dfac$(11)),             descr$(11),             ~
          at(17,33), fac(dfac$(12)),             descr$(12),             ~
          at(18,33), fac(dfac$(13)),             descr$(13),             ~
          at(19,33), fac(dfac$(14)),             descr$(14),             ~
          at(20,33), fac(dfac$(15)),             descr$(15),             ~
                                                                         ~
          at(06,56), fac(fac2$( 1)),            rate$( 1),               ~
          at(07,56), fac(fac2$( 2)),            rate$( 2),               ~
          at(08,56), fac(fac2$( 3)),            rate$( 3),               ~
          at(09,56), fac(fac2$( 4)),            rate$( 4),               ~
          at(10,56), fac(fac2$( 5)),            rate$( 5),               ~
          at(11,56), fac(fac2$( 6)),            rate$( 6),               ~
          at(12,56), fac(fac2$( 7)),            rate$( 7),               ~
          at(13,56), fac(fac2$( 8)),            rate$( 8),               ~
          at(14,56), fac(fac2$( 9)),            rate$( 9),               ~
          at(15,56), fac(fac2$(10)),            rate$(10),               ~
          at(16,56), fac(fac2$(11)),            rate$(11),               ~
          at(17,56), fac(fac2$(12)),            rate$(12),               ~
          at(18,56), fac(fac2$(13)),            rate$(13),               ~
          at(19,56), fac(fac2$(14)),            rate$(14),               ~
          at(20,56), fac(fac2$(15)),            rate$(15),               ~
                                                                         ~
          at(06,62), fac(fac3$( 1)),              to$( 1),               ~
          at(07,62), fac(fac3$( 2)),              to$( 2),               ~
          at(08,62), fac(fac3$( 3)),              to$( 3),               ~
          at(09,62), fac(fac3$( 4)),              to$( 4),               ~
          at(10,62), fac(fac3$( 5)),              to$( 5),               ~
          at(11,62), fac(fac3$( 6)),              to$( 6),               ~
          at(12,62), fac(fac3$( 7)),              to$( 7),               ~
          at(13,62), fac(fac3$( 8)),              to$( 8),               ~
          at(14,62), fac(fac3$( 9)),              to$( 9),               ~
          at(15,62), fac(fac3$(10)),              to$(10),               ~
          at(16,62), fac(fac3$(11)),              to$(11),               ~
          at(17,62), fac(fac3$(12)),              to$(12),               ~
          at(18,62), fac(fac3$(13)),              to$(13),               ~
          at(19,62), fac(fac3$(14)),              to$(14),               ~
          at(20,62), fac(fac3$(15)),              to$(15),               ~
                                                                         ~
          at(06,64), fac(dfac$( 1)),           todescr$( 1),             ~
          at(07,64), fac(dfac$( 2)),           todescr$( 2),             ~
          at(08,64), fac(dfac$( 3)),           todescr$( 3),             ~
          at(09,64), fac(dfac$( 4)),           todescr$( 4),             ~
          at(10,64), fac(dfac$( 5)),           todescr$( 5),             ~
          at(11,64), fac(dfac$( 6)),           todescr$( 6),             ~
          at(12,64), fac(dfac$( 7)),           todescr$( 7),             ~
          at(13,64), fac(dfac$( 8)),           todescr$( 8),             ~
          at(14,64), fac(dfac$( 9)),           todescr$( 9),             ~
          at(15,64), fac(dfac$(10)),           todescr$(10),             ~
          at(16,64), fac(dfac$(11)),           todescr$(11),             ~
          at(17,64), fac(dfac$(12)),           todescr$(12),             ~
          at(18,64), fac(dfac$(13)),           todescr$(13),             ~
          at(19,64), fac(dfac$(14)),           todescr$(14),             ~
          at(20,64), fac(dfac$(15)),           todescr$(15),             ~
                                                                         ~
          at(21,02), fac(hex(a4)), edtmessage$,                          ~
          at(22,02), fac(hex(8c)), pfdescr$(1,mode%)            , ch(79),~
          at(23,02), fac(hex(8c)), pfdescr$(2,mode%)            , ch(79),~
          at(24,02), fac(hex(8c)), pfdescr$(3,mode%)            , ch(79),~
                                                                         ~
                keys(str(pfkeys$(mode%))),                               ~
                key (keyhit%)

               if keyhit% <> 13 then L43480
                  call "MANUAL" ("SLMINPUT")
                  goto L42280

L43480:        if keyhit% <> 15 then L43520
                  call "PRNTSCRN"
                  goto L42280

L43520:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())

            if keyhit%  <>  0%      then return
            if mode%    <>  1%      then return
            if code$(1) <> " "      then return
            if cursor%(1) - 5 <> 1% then return
*        Execute Plowcode Routine to find entries on file.
                plowkey$ = str(salesman$) & code$(1)
                call "PLOWCODE" (#2, plowkey$, " ", 4%, 0, f1%(2))
                if f1%(2) = 0% then L42280
                     code$(1) = str(plowkey$, 5)
                     return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Tests Data for the items on Page 1 (Saleman Info).        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* Salesman Code    */~
                                    L50250,         /* Name             */~
                                    L50300,         /* Address          */~
                                    L50340          /* Phone Number     */
                  return

L50140
*        Test Data for TAX CODE
            if salesman$ <> " " then L50200
                call "GETCODE" (#1, salesman$, " ", 0%, 0.30, f1%(1))
                if f1%(1) <> 0% then L50200
                     errormsg$ = hex(00)
                     return
L50200:     gosub L30000
            if f1%(1) = 0% then return
                return clear all
                goto editmode

L50250
*        Test Data for NAME
            if name$ <> " " then return
                errormsg$ = "Sorry, Name Can't Be Blank"
                return

L50300
*        Test Data for ADDRESS
            return

L50340
*        Test Data for PHONE NUMBER
            return

        REM *************************************************************~
            *                T E S T   D A T A                          *~
            *-----------------------------------------------------------*~
            *  Edit data fields for rate's line entered.                *~
            *************************************************************

        deffn'152(l%)
            init (hex(8c)) dfac$(), fac1$(), fac2$(), fac3$()
            errormsg$ = " "

*        Test COMMISSION RATE
            if rate$(l%) = " " then rate$(l%) = "0"
            convert rate$(l%) to rate, data goto L51130 : goto L51160
L51130:         errormsg$ = "INVALID ENTRY FOR COMMISSION RATE."
L51140:         fac2$(l%) = hex(82)
                return
L51160:     if rate >= 0 and rate <= 99.99 then L51200
                errormsg$ = "COMMISSION RATE MUST BE 0 - 99.99"
                goto L51140
L51200:     convert rate to rate$(l%), pic(##.00)


*        Test APPLIED TO
            convert to$(l%) to to%, data goto L51350 :  goto L51390
L51350:         errormsg$ = "1=Gross Ext, 2= Netted Ext, 3=Std Cost, " & ~
                            "4=Margin per Gross, 5=Margin per Netted"
                fac3$(l%) = hex(82)
                return
L51390:     if to% < 1% or to% > 5% then L51350
                gosub describe_apply_to
                return


        REM *************************************************************~
            *        M I S C.  S U B - R O U T I N E S                  *~
            *************************************************************

        clear_first       /* Clear array bucket 1                      */
            code$(1), descr$(1), rate$(1), to$(1), todescr$(1) = " "
        return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
