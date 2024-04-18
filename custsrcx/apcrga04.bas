        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA04                             *~
            *  Creation Date     - 10/23/95                             *~
            *  Last Modified Date- 10/18/99                             *~
            *  Description       - This Program Creates the RGA         *~
            *                      Analysis Reports.                    *~
            *                                                           *~
            *  Special Comments  - (APCRGA4B) - By Customer             *~
            *                      (APCRGA5B) - By Reason Code          *~
            *                      (APCRGA6B) - By Salesman             *~
            *                      (APCRGA7B) - By Production Date      *~
            *                      (APCRGA8B) - By Status Code          *~
            *                      (APCRGA9B) - By Auth. Userid         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/23/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 04/04/96 ! Change Reason Code to 3 bytes (Complaint)! JBF *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID To 60403      ! DJD *~
            * 02/26/98 ! Modified for Y2K Compliance              ! DJD *~
            * 10/18/99 ! (EWD001) Fix Date Problems wrong changes ! RHH *~
            *          !    Made.                                  !     *~
            *************************************************************
        dim                                                              ~
            readkey$24,                  /* GENCODES Read Key          */~
            custkey$9,                   /* CUSTOMER Read Key          */~
            saleskey$4,                  /* SLMMASTR Read Key          */~
            cms_key$3,                   /* USERLCMS Read Key          */~
            rpttype$1, rpttype_d$25,     /* Report Type and Description*/~
            rpt_value$9,                 /* Customer, Reason, Salesman */~
            rpt_sum$1, rpt_sum_d$15,     /* Summary or Detail          */~
            beg_date$10,                 /* Begin Date Range (EWD001)  */~
            end_date$10,                 /* End   Date Range (EWD001)  */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            today$6,                     /* Today's Date               */~
            userid$3                     /* Current User Id            */~

        dim f2%(7%),                     /* = 0 if the file is open    */~
            f1%(7%),                     /* = 1 if READ was successful */~
            fs%(7%),                     /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(7%)20                  /* Text from file opening     */

	dim beg_datec$10,                /* Century Begin Date	       */~
	    end_datec$10		 /* Century end date	       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 RGA Analysis Reports         "

        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! GENERAL SYSTEM CODES FILE                *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #6  ! SLMMASTR ! Salesman Master File                     *~
            * #7  ! USERLCMS ! Caelus User ID Master File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #6,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #7,  "USERLCMS",                                      ~
                        varc,     indexed,  recsize =   600,             ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =    4, keylen =  30, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENOLIB" (#7, "SHARE", f2%(7%),     rslt$(7%), axd$)

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62%) = "APCRGA04: " & str(cms2v$,,8%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then L10250

L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then L10220

L10160:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                               if enabled% = 1% then L10120
                               if fieldnr% = 1% then L10090

                          goto L10160
L10220:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0%                   then L10120

L10250:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10120

            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 14% then gosub dataput
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then editpg1

L11130:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            if fieldnr% = lastfieldnr%        then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% = 0%  then editpg1

L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  = 1%  then gosub startover
                if keyhit% <> 0%  then L11190

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then L11190
                lastfieldnr% = fieldnr%

            goto L11130

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************
        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************
        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28100
            inpmessage$ =  edtmessage$
        return

L28100
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
                if fieldnr% <> 2% then return
                   if rpttype$ = "4" then inpmessage$ =                  ~
                          "Enter the a Valid Reason Code(3) or (ALL) ?"
            return

        scrn1_msg  :  data                                               ~
         "Enter Report By Type ?                                       ",~
         "Enter the Valid Customer/Reason Code/Salesman/ID or (ALL) ?  ",~
         "Enter an (S) for Summary Report Only, (D) for Detail Report ?",~
         "Enter Beginning Date Range ?                                 ",~
         "Enter Ending Date Range ?                                    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, custkey$, rpttype$,        ~
                      rpt_value$, readkey$, rpt_sum$, rpt_sum_d$,        ~
                      rpttype_d$, beg_date$, end_date$, cms_key$,	 ~
		      beg_datec$, end_datec$
        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************
        startover
            u3% = 2%
            call "STARTOVR" (u3%)

            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            convert rpttype$ to rpttype%, data goto L31160

            on rpttype% gosub by_customer,                               ~
                              by_reason_code,                            ~
                              by_salesman,                               ~
                              by_production_date,                        ~
                              by_status_code,                            ~
                              by_auth_id
        return clear all
        goto inputmode
L31160: return

        by_customer
            call "APCRGA4B" (#1, #2, rpt_value$, rpt_sum$)

        return

        by_reason_code
            call "APCRGA5B" (#1, #2, rpt_value$, rpt_sum$)

        return

        by_salesman
            call "APCRGA6B" (#1, #2, #6, rpt_value$, rpt_sum$)

        return

        by_production_date                       /* (EWD001)           */
            if str(beg_datec$,1%,3%) = "ALL" then beg_date$ = beg_datec$

            if str(end_datec$,1%,3%) = "ALL" then end_date$ = end_datec$

            call "APCRGA7B" (#1, #2, beg_date$, end_date$, rpt_value$,   ~
                             rpt_sum$)
                                                 /* (EWD001)           */
        return

        by_status_code
            call "APCRGA8B" (#1, #2, rpt_value$, rpt_sum$)

        return

        by_auth_id
            call "APCRGA9B" (#1, #2, #7, rpt_value$, rpt_sum$)

        return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1

            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L40200,         /* Report Type       */     ~
                              L40190,         /* Report Value      */     ~
                              L40190,         /* Summary/Detail    */     ~
                              L40200,         /* Start Date Range  */     ~
                              L40200          /* End   Date Range  */
            goto L40220

                lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:         lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40200:         lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
                at (01,02),                                              ~
                     "APC Building Products   -  RGA Analysis Reports",  ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                at (02,02), fac(hex(8c)),   line2$              , ch(79),~
                at (03,02), fac(hex(94)),   errormsg$           , ch(79),~
                                                                         ~
                at (05,02), "Report Type (1-6)  :",                      ~
                at (05,25), fac(lfac$(1%)), rpttype$            , ch(01),~
                at (05,40), fac(hex(84)),   rpttype_d$          , ch(25),~
                                                                         ~
                at (06,02), "Specified Value    :",                      ~
                at (06,25), fac(lfac$(2%)), rpt_value$          , ch(09),~
                                                                         ~
                at (07,02), "(S)ummary/(D)etail :",                      ~
                at (07,25), fac(lfac$(3%)), rpt_sum$            , ch(01),~
                at (07,40), fac(hex(84)),   rpt_sum_d$          , ch(15),~
                                                                         ~
                at (08,02), "Beginning Date     :",                      ~
                at (08,25), fac(lfac$(4%)), beg_datec$          , ch(10),~
                                                                         ~
                at (09,02), "Ending Date        :",                      ~
                at (09,25), fac(lfac$(5%)), end_datec$          , ch(10),~
                                                                         ~
                at (11,24), "**********************************",        ~
                at (12,24), "*   Report Types:                *",        ~
                at (13,24), "*   1) Customer                  *",        ~
                at (14,24), "*   2) Reason Code               *",        ~
                at (15,24), "*   3) Salesman                  *",        ~
                at (16,24), "*   4) Production Date (Range)   *",        ~
                at (17,24), "*   5) Status Code               *",        ~
                at (18,24), "*   6) Authorizing Userid        *",        ~
                at (19,24), "**********************************",        ~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15% then L40670
                     call "PRNTSCRN" : goto L40220

L40670:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

        return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)

            if fieldnr% = 1% then L40840
                str(pf$(3%),64%) = " "     : str(pfkeys$,16%,1%) = hex(ff)
L40840:     if fieldnr% > 1% then L40860
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%)  = hex(ff)
L40860: return

L40880: if fieldnr% > 0% then L40980  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Print Report"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
        return

L40980:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,                /* Report Type     */~
                              L50330,                /* Report Value    */~
                              L50930,                /* Summary/Detail  */~
                              L51040,                /* Beg. Date Range */~
                              L51240                 /* End  Date Range */
        return

L50140: REM Test for Report Type                  RPTTYPE$
            if rpttype$ >= "1" and rpttype$ <= "6" then L50190
                errormsg$ = "Report Type Must be 1 thru 6 ?"
                rpttype$, rpttype_d$ = " "
                goto L50310
L50190:     if  rpttype$   = "1" then                                    ~
                rpttype_d$ = "RGA's by Customer       "
            if  rpttype$   = "2" then                                    ~
                rpttype_d$ = "RGA's by Reason Code    "
            if  rpttype$   = "3" then                                    ~
                rpttype_d$ = "RGA's by Salesman       "
            if  rpttype$   = "4" then                                    ~
                rpttype_d$ = "RGA's by Production Date"
            if  rpttype$   = "5" then                                    ~
                rpttype_d$ = "RGA's by Status Code    "
            if  rpttype$   = "6" then                                    ~
                rpttype_d$ = "RGA's by Auth. Userid   "
L50310: return

L50330: REM Test for Report Value                 RPT_VALUE$
            if rpt_value$ <> " "             then L50360
               str(rpt_value$,1%,3%) = "ALL"
L50360:     if str(rpt_value$,1%,3%) = "ALL" then L50910

            if rpttype$ <> "6"               then L50480
                cms_key$            = all(hex(20))
                str(cms_key$,1%,3%) = str(rpt_value$,1%,3%)
                read #7,key = cms_key$, eod goto L50440

                goto L50910
L50440:         errormsg$  = "(Error) - Invalid Auth. Userid"
                rpt_value$ = " "
                goto L50910

L50480:     if rpttype$ <> "5"               then L50590
                readkey$ = all(hex(20))
                str(readkey$,1%,9%)   = "APC  RGA1"
                str(readkey$,10%,15%) =  rpt_value$
                read #1,key = readkey$, eod goto L50550

                goto L50910
L50550:         errormsg$  = "(Error) - Invalid Status Code."
                rpt_value$ = " "
                goto L50910

L50590:     if rpttype$ <> "4"               then L50630
                                          /* Check Reason Code */   
                goto L50735               /* (EWD001) - Fix    */

L50630:     if rpttype$ <> "3"               then L50730
                saleskey$  = " "
                saleskey$  = str(rpt_value$,1%,4%)
                read #6,key = saleskey$, eod goto L50690

                goto L50910
L50690:         errormsg$  = "(Error) - Invalid Salesman Code."
                rpt_value$ = " "
                goto L50910

L50730:     if rpttype$ <> "2"                then L50840
L50735:         readkey$ = all(hex(20))             /* (EWD001)    */
                str(readkey$,1%,9%)   = "COMPLAINT"
                str(readkey$,10%,15%) =  rpt_value$
                read #1,key = readkey$, eod goto L50800

                goto L50910
L50800:         errormsg$  = "(Error) - Invalid Reason Code."
                rpt_value$ = " "
                goto L50910

L50840:     custkey$ = " "
            custkey$ = rpt_value$
            read #2,key = custkey$, eod goto L50890

            goto L50910
L50890:     errormsg$  = "(Error) - Invalid Customer Code."
            rpt_value$ = " "
L50910: return

L50930: REM - Summary or Detail
            if rpt_sum$ <> " " then L50960
               rpt_sum$ =  "D"
L50960:     if rpt_sum$ <> "D" and  rpt_sum$   <> "S" then L51000
            if rpt_sum$ =  "D" then rpt_sum_d$ =  "Detail Report  "
            if rpt_sum$ =  "S" then rpt_sum_d$ =  "Summary Only   "
            goto L51020
L51000:     errormsg$   = "(Error) - Invalid Entry, Either S or D ?"
            rpt_sum$, rpt_sum_d$ = " "
L51020: return

L51040: REM - Beginning Date Range            /* (EWD001) - Rewrite */
            if rpttype$ <> "4" then beg_datec$ = "N/A"
            if beg_datec$ <> " " then goto L51050
L51045:        str(beg_datec$,1%,10%) = "ALL       "
               goto L51220

L51050:     if str(beg_datec$,1%,3%) = "N/A" then L51220
            if str(beg_datec$,1%,3%) = "ALL" then goto L51045

            today$ = date
            call "DATEOKC" (beg_datec$, date%, errormsg$)
                if date% = 0%               then L51200
            beg_date$ = beg_datec$
            call "DATUFMTC" (beg_date$)

            if str(beg_date$,1%,6%) > today$     then L51200
            goto L51220
L51200:     errormsg$ = "(Error) - Invalid Beginning Date ?"
            init(" ") beg_datec$, end_datec$, beg_date$, end_date$
L51220: return

L51240: REM - Ending Date Range               /* (EWD001) - rewrite  */       
            if rpttype$ <> "4" then end_datec$ = "N/A"
            if end_datec$ <> " " then goto L51250
L51245:        str(end_datec$,1%,10%) = "ALL       "
               str(beg_datec$,1%,10%) = "ALL       "
               goto L51420

L51250:     if str(end_datec$,1%,3%) = "N/A" then L51420
            if str(end_datec$,1%,3%) = "ALL" then goto L51245
            
            call "DATEOKC" (end_datec$, date%, errormsg$)
            if date% = 0% then L51400

            end_date$ = end_datec$
            call "DATUFMTC" (end_date$)
            if str(end_date$,1%,6%) > today$ or                      ~
               str(end_date$,1%,6%) < str(beg_date$,1%,6%) then L51400 
                return
L51400:     errormsg$ = "(Error) - Invalid Ending Date ?"
            init(" ") end_date$, end_datec$
L51420:     return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************
        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
