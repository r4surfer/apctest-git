        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC   U   U  TTTTT  IIIII  L                     *~
            *  W   W  C   C  U   U    T      I    L                     *~
            *  W   W  C      U   U    T      I    L                     *~
            *  W W W  C   C  U   U    T      I    L                     *~
            *   W W    CCC    UUU     T    IIIII  LLLLL                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCUTIL   - DISPLAY WORK CENTER UTILIZATION ON CRT. SHOWS  *~
            *            3 DIGITS FOR EACH PERCENTAGE ON EACH W/C DAY.  *~
            *            OVER 999% IS DISPLAYED AS ###, TOTALLY FREE    *~
            *            CAPACITY SHOWS AS ..., TOTALLY UNAVAILABLE IS  *~
            *            DISPLAYED AS ***.  ALSO PRINTS TWO REPORTS.    *~
            *************************************************************~
            * NOTE - We had to really trim arrays and duplicate usage   *~
            *        on some variables to get the segment 2 size down   *~
            *        to a usable figure.                                *~
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
            * 08/05/83 ! ORIGINAL                                 ! HES *~
            * 10/11/87 ! Now Shows up to 300 Work centers         ! HES *~
            * 09/14/89 ! Major Cleanup/Rebuild.  Now shows usage  !     *~
            *          ! percentages to 3 places.                 ! DAW *~
            * 11/19/90 ! Modified array variable handling for     ! MJB *~
            *          !  BASIC 4.3                               !     *~
            * 02/24/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 10/28/92 ! PRR12011 Dim AVGU%(17%,2%).  Taking      ! SID *~
            *          ! Un-Available Days into account when      !     *~
            *          ! calculating the average percentage of WC !     *~
            *          ! utilization.(ie Weekend, Holiday etc..)  !     *~
            * 01/25/93 ! Standardized report headings.            ! MLJ *~
            * 08/29/96 ! Millie date conversion                   ! DER *~
            *          ! Deviate from standard, Dates used YYMMDD !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            avg$3,                       /* Temporary Average Holder   */~
            avgu%(17%,2%),               /* Ave. Utilization Percent   */~
            begin_date$10,               /* DISPLAY WORK FIELD FOR DATE*/~
            blankdate$8,                 /* blank unfmt date           */~
            calander$(490)6,             /* DATES FOR DISPLAY          */~
            ccyymmdd$8,                  /* used to get YYMMDD         */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            centers$(300,490)3,          /* DATA STORAGE FOR DISPLAY   */~
            company$60,                  /* Company Name               */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            daysa%(490),                 /* DATA STORAGE FOR AVAIL DAYS*/~
            daysu%(490),                 /* DATA STORAGE FOR USED DAYS */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            end_date$10,                 /* SELECT VARIABLE            */~
            ending_date$10,              /* DISPLAY WORK FIELD FOR DATE*/~
            eddt_yymmdd$6,               /* end date form YYMMDD       */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fc$(300)1,                   /* FACS FOR WC SELECT SCREEN  */~
            heading$(17)5,               /* REPORT ONE HEADING DATES   */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            inst$(2)79,                  /* DISPLAY ARRAY FOR FOOTINGS */~
            line$(14)79,                 /* DISPLAY ARRAY              */~
            line2$79,                    /* Screen Display Line        */~
            lfac$(25)1,                  /* DISPLAY ATRIBUTES          */~
            mnth$2,                      /* WORK VARIABLE              */~
            pf1$17,                      /* INSTRUCTION FOR SELCT DSPLY*/~
            pf4$17,                      /* INSTRUCTION FOR SELCT DSPLY*/~
            pf5$17,                      /* INSTRUCTION FOR SELCT DSPLY*/~
            pf12$17,                     /* INSTRUCTION FOR SELCT DSPLY*/~
            print$(17)6,                 /* FOR REPORT ONE             */~
            printline$132,               /* FOR REPORT TWO             */~
            report$40,                   /* NAME OF SELECTED REPORT    */~
            runtime$8,                   /* SYSTEM TIME                */~
            start_date$10,               /* SELECT VARIABLE            */~
            stdt_yymmdd$6,               /* start date form YYMMDD     */~
            search%(2),                  /* WORK VARIABLE FOR SEARCHES */~
            select$(300)1,               /* STORES WC SELECTED         */~
            tempdate$8,                  /* temp used in date format   */~
            title$(16)79,                /* DISPLAY ARRAY FOR HEADINGS */~
                                         /* ALSO USED IN SEVERAL PLACES*/~
                                         /* AS TEMP STORAGE(SAVES CORE)*/~
                                                                         ~
            tstcal1$8,                   /* test for calandar          */~
            tstcal2$8,                   /* test for calandar two      */~
            wcname$30,                   /* NAME OF CENTRE             */~
            wc$(300)4,                   /* DATA STORAGE FOR DISPLAY   */~
            year$2                       /* WORK VARIABLE              */~

        dim f2%(02),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(02),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(02)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(02)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * #02 ! CALMASTR ! CALANDAR MASTER  FILE                    *~
            *************************************************************~

            select #01, "WCMASTR", varc, indexed, recsize = 2024,        ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #02, "CALMASTR", varc, indexed, recsize = 1962,       ~
                       keypos = 1, keylen = 2

            call "SHOSTAT" ("Preparing For Work Center Usage Mapping")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))
            if f2%(1) <> 0 then L65000
            if f2%(2) <> 0 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            maxallowed% = 300%  /* must be multiples of 50, also */
                            /* TITLE$() must have eq # of total bytes */

            ret% = 0%
            call "COMPNAME" (12%, company$, ret%)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

             inst$(1) = "1)Re-Select Centers    4)See Prev Centers    6)S~
        ~ee Prev Days    15)Print Screen"
             inst$(2) = "2)Show Legend          5)See More Centers    7)S~
        ~ee More Days    16)Exit Program"
            str(line2$,62) = "  WCUTIL: " & str(cms2v$,,8)

            init(" ") calander$()     /* Load Planning Calender */
            call "READ100" (#02,"10",f1%(02))
              if f1%(02)=0 then L65000  /* IF NOT THERE, SORRY CHARLIE */
                 get #02, using L09230, str(calander$ (),1)
            call "READ100" (#02, "11", f1%(02))
              if f1%(02)=0 then L65000
                 get #02, using L09230, str(calander$ (),1471)
L09230:     FMT XX(2), CH(1470)

            for eye% = 1% to 490%
               tempdate$ = str(calander$(), 1%+((eye%-1%)*6%), 6%)
               call "DATEFMT" (tempdate$, ccyymmdd%, ccyymmdd$)
               str(calander$(), 1%+((eye%-1%)*6%), 6%) = str(ccyymmdd$, 3%, 6%)
            next eye%

            tempdate$ = date
            call "DATEFMT" (tempdate$, ccyymmdd%, ccyymmdd$)

            search calander$() = str(ccyymmdd$, 3%, 6%) to search%() step 6
                if search%(1) = 0 then L10000
            first% = (search%(1) - 1)/6

L10000: REM *************************************************************~
            *                       F I R S T                           *~
            *       S E L E C T   C E N T E R S   S C R E E N           *~
            *                                                           *~
            * GET CENTERS TO INCLUDE ON DISPLAY                         *~
            *************************************************************

        select_centers_for_display
            gosub read_centers
            if wc$() = " " then L65000
            pf16$ = " 16)Exit Program"
            pf1$ = "RETURN To Accept"
            pf12$ = "12)Print Report"
            which$ = "display."
            b% = 0%
L10122:     inpmessage$ = "To INCLUDE a Work Center, Mark The Box."
L10130:
L10140:     gosub L40000  /* Select Workcenters Screen */
            if keyhit% = 12 then  wc$() = " "
            if keyhit% = 12 then  select_report
            if keyhit% = 16 then L65000
            if keyhit% <> 2 then L10170
                init ("X") str(select$(),,hits%)
                inpmessage$ = "To EXCLUDE a Work Center, replace the 'X' ~
        ~with a SPACE"
                goto L10140
L10170:     if keyhit% <> 0 then L10130
            if select$() = " " then L10122
            pf12$ = " "
            gosub calculate_status

        REM *************************************************************~
            *                       D I S P L A Y                       *~
            *                   U T I L I Z A T I O N                   *~
            *             AND GIVE OPTION TO PRINT HARD COPY.           *~
            *************************************************************

        show_them
            if wc$() = " " then select_centers_for_display
            title$() , mnth$, year$ = " "
            str(title$(1),5,1), str(title$(1),79) = "Y"
            str(title$(2),5,1), str(title$(2),79) = "Y"
            str(title$(3),5,1), str(title$(3),79) = "M"
            str(title$(4),5,1), str(title$(4),79) = "M"
            str(title$(5),5,1), str(title$(5),79) = "D"
            str(title$(6),5,1), str(title$(6),79) = "D"
            work1% = 0
            for x3% = 1 to 14
              strt% = 6%
              work% = 0
              line$(x3%) = wc$(row% + x3%)
              for u3% = 1 to 18
                work% = work% + 1
                if column% + work% > 490 then L11456
                if row% + x3% > hits% then L11260
                  if x3% = 1 then work1% = work1% + 1
                  str(line$(x3%), strt%,   4) =                          ~
                       centers$(row% + x3%, column% + work% )
L11260:           on x3% goto L11280,,L11360,,L11440,L11440    /* HEADING */
                         goto L11456
L11280:           if str(calander$(column% + work%) , x3%, 2) = year$    ~
                         then L11456
                         year$ = str(calander$(column% + work%), x3%, 2)
                         str(title$(x3%), strt%+2%,4) =                  ~
                         str(calander$(column% + work%), x3%, 1)
                         str(title$(x3% +1), strt%+2%,4) =               ~
                         str(calander$(column% + work%), x3% + 1, 1)
                         goto L11456
L11360:           if str(calander$(column% + work%) , x3%, 2) = mnth$    ~
                         then L11456
                         mnth$ = str(calander$(column% + work%), x3%, 2)
                         str(title$(x3%), strt%+2%,4) =                  ~
                         str(calander$(column% + work%), x3%, 1)
                         str(title$(x3% +1), strt%+2%,4) =               ~
                         str(calander$(column% + work%), x3% + 1, 1)
                         goto L11456
L11440:           if strt% < 74% then str(title$(x3%), strt%+2%,4) =     ~
                      str(calander$(column% + work%), x3%, 1) else       ~
                      str(title$(x3%), strt%+2%,2) =                     ~
                      str(calander$(column% + work%), x3%, 1)
L11456:           strt% = strt% + 4%
                next u3%
            next x3%
            gosub L40500  /* Display Screen */
            if keyhit%  = 16 then  L65000
            if keyhit%  = 6 then  column% = max(0, column% - 12)
            if keyhit%  = 7 then  column% = min(470, column% + 12)
            if keyhit%  = 4 then  row% = max(0, row% - 13)
            if keyhit%  = 5 then  row% = min(max(hits%-14,0), row% + 13)
            if keyhit%  = 1 then  L11660
            if keyhit%  = 2 then  gosub L41000     /* SHOW LEGEND */
            goto show_them

L11660:     call "SHOSTAT" ("Re-building Selection Table, One Moment Plea~
        ~se")
            goto select_centers_for_display

        REM -------------------------------------------------------------~
            ----------------- WHICH REPORT ????????? --------------------~
            -------------------------------------------------------------~

        select_report
           gosub L41500  /* Report selection screen */
           if keyhit%  <> 1 and keyhit% <> 2 and pf12$ <> " " then       ~
                                               select_centers_for_display
           if keyhit%  = 1 or keyhit% = 2 then L12060
           init (" ") wc$()
           go to select_centers_for_display
L12060:       which% = keyhit%
           report$ = "Report One, 1 Work Center Per Page"
           if which% = 2 then report$ = "Report Two, Up To 50 Centers Per~
        ~ Page"
            call "SETPRNT" ("W/C003", " ", 0%, 0%)
            select printer
            runtime$ = " "
            call "TIME" (runtime$)

        REM -------------------------------------------------------------~
            ************* GET CENTERS TO INCLUDE ON REPORT **************~
            -------------------------------------------------------------~

            pf16$ = " 16)Prev Screen"
            pf1$  = "1)Prev Screen"
            pf12$ = " "
            which$ = "Report."
            rslt$() = wc$()
            keyhit% = 0
            gosub read_centers
L13061:     inpmessage$ = " "
L13070:     gosub L40000 /* Select Workcenters to include screen */
            if keyhit% <> 16 and keyhit% <> 1 then L13110
               wc$() = rslt$()
               goto select_report
L13110:     if keyhit% <> 2 then L13120
                init ("X") str(select$(),,hits%)
                inpmessage$ = "To EXCLUDE a Work Center, replace the 'X' ~
        ~with a SPACE"
                goto L13070
L13120:     if keyhit% <> 0 then L13061
            if select$() = " " then L13061
            gosub calculate_status

        REM -------------------------------------------------------------~
            ******** GET DATE RANGE SPECIFICATIONS FOR REPORT ***********~
            ********          (STANDARD INPUT LOGIC)          ***********~
            -------------------------------------------------------------~

            errormsg$ = " "
            inpmessage$ = "Enter Starting and Ending Dates to Include on ~
        ~Report"
            begin_date$   = calander$(1)
               call "DATFMTC" (begin_date$)
            ending_date$  = calander$(490)
               call "DATFMTC" (ending_date$)

            for fieldnr% = 1 to  2
L15070:         gosub'102(fieldnr%)  /* Display and Accept Screen */
                      if keyhit%  = 16 then select_centers_for_display
                      if keyhit%  =  1 then select_report
                      if keyhit% <>  0 then L15070
                gosub'152(fieldnr%)  /* Validate Data */
                      if errormsg$ <> " " then L15070
                next fieldnr%
        REM EDIT
L15150:     gosub'112(0%)            /* Display and Edit Screen */
                  gosub call_screen
                  if keyhit%  =  1 then select_report
                  if keyhit%  = 16 then L15290
                  if keyhit% <>  0 then L15150
                  fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  2 then L15150

L15220:     gosub'112(fieldnr%)      /* Display and Edit Screen */
                  gosub call_screen
                  if keyhit%  =  1 then select_report
                  if keyhit% <>  0 then L15220
            gosub'152(fieldnr%)      /* Validate Data */
                  if errormsg$ <> " " then L15220
            goto L15150

L15290:     on which% goto print_one, print_two    /* THE CROSSROADS */

                      REM ===============================================
        read_centers  /*  = RE-FILLS WC$(), RETAINING PREV. SELECTIONS */
                      REM ===============================================
            hits% = 0                 /* A LOWCASE X IS PUT INTO THE BOX*/
            key$ = hex(00)            /* FOR THE USER IF THE CENTER HAD */
                                      /* PREVIOUSLY BEEN SELECTED.      */
*          TITLE$() = WC$()
            init(" ")  select$(), wc$()
L16080:         call "PLOWNEXT" (#1, key$, 0%, f2%(1))
                  if f2%(1) = 0 or hits% = maxallowed% then L16160
                     hits% = hits% + 1
                     search title$() = str(key$,,4) to search%() step 4
*                   IF SEARCH%(1) <> 0 THEN SELECT$(HITS%) = "x"
                     wc$(hits%) = key$
                goto L16080
L16160: return

                         REM =============================================
        calculate_status  /* CALCS THE PLANNING STATUS OF SLCTD CENTERS */
                         REM =============================================
           call "SHOSTAT" ("Retrieving data and calculating status of the~
        ~ selected centers.")
           init(" ") centers$()
           work%, hits%, row% = 0
           column% = first%
L17070:    work% = work% + 1
           if work% > maxallowed% then L17420
           if select$(work%) = " " then L17070
           call "READ100" (#1, wc$(work%), f2%(1))
           if f2%(1) = 0 then L17070
           get #1 using L35100, wc$, wcname$, daysa%(), daysu%()
           hits% = hits% + 1
           wc$(hits%) = wc$
           for u3% = 1 to 490
             if daysa%(u3%) = 0 then   /* DAY IS NOT AVAILABLE */        ~
               centers$(hits%, u3%) = all("*")  else                     ~
                 centers$(hits%, u3%) = "..."  /* DAY IS AVAILABLE */
             if daysu%(u3%) = 0 then L17390          /* DAY NOT PLANNED */
             if daysa%(u3%) <> 0 then L17300
             centers$(hits%, u3%) = "@@@" /* UNAVL DAY WAS PLANNED */
             goto L17390
L17300:      x1 = daysu%(u3%)             /* GET PERCENTAGE PLANNED */
             x2 = daysa%(u3%)
             x3 = (x1 / x2) *100
             convert round(x3,0) to centers$(hits%,u3%), pic(###)
L17390:    next u3%
           goto L17070

L17420:    if hits% < maxallowed% then str(wc$(), hits% * 4 + 1) = " "
        return

        REM !============================================================~
            !==!      EVERYTHING BELOW HERE IS THE REPORT LOGIC.      !==~
            !==!      (Except for the standard program sections)      !==~
            !============================================================~

        print_one
            gosub tell_them
        REM ****** LETS FIND THE MONTHS TO INCLUDE ON THE HEADING ******
            init(" ") heading$()
            work% = 1
            work_date$ = stdt_yymmdd$
            heading$(1) = str(work_date$,,2) &"/"& str(work_date$,3,2)
            search calander$() = str(work_date$,,6) to cursor%()
            if cursor%(1) = 0 then print_done
L19072:        search str(calander$(), cursor%(1) ) <>                   ~
                             str(work_date$,,4) to search%() step 6
            if search%(1) = 0 then L19200
               search%(1) = search%(1) + cursor%(1) - 1
            tstcal1$ = str(calander$(), search%(1), 4) & "01"
            call "DATECONV" (tstcal1$)
            tstcal2$ = str(eddt_yymmdd$,1%,4%) & "01"
            call "DATECONV" (tstcal2$)
            if str(tstcal1$, 1%, 4%) > str(tstcal2$, 1%, 4%) then L19200

            /* if str(calander$(), search%(1), 4) > str(eddt_yymmdd$,,4) then L19200 */

            work% = work% + 1
            heading$(work%) = str(calander$(), search%(1), 2) & "/" &    ~
                              str(calander$(), search%(1)+2, 2)
            if work% = 17 then L19200
            work_date$ = str(calander$(), search%(1), 6)
            cursor%(1) = search%(1)
            goto L19072

        REM ***** WE ARE ALL READY, SO LETS GO ... ******

L19200:     work% = 1
L19208:     wc$ = wc$(work%)
            if wc$ = " " then print_done
            call "READ100" (#1, wc$, f1%(1))
              if f1%(1) = 0 then L19568
                 get #1 using L35100, wc$, wcname$
                 gosub form_control_one
                 print wc$ ; "    " ; wcname$

          mat avgu% = zer : avg$ = " "
          for x3% = 1 to 31
            print$() = " "
            for u3% = 1 to 17
              if heading$(u3%) = " " then L19384
              work_date$ = str(heading$(u3%),,2) & str(heading$(u3%),4,2)
              convert x3% to str(work_date$,5,2), pic (00)
              search calander$() = str(work_date$,,6) to search%()
              if search%(1) = 0 then L19384
              search%(1) = (search%(1) + 5)/6
              str(print$(u3%),3 ,3) = centers$(work%, search%(1))
              if centers$(work%,search%(1)) = "***" then L19384
              if centers$(work%,search%(1)) = "@@@" then L19384
              avg% = 0%
              convert centers$(work%,search%(1)) to avg%, data goto L19368
L19368:       avgu%(u3%, 1%) = avgu%(u3%, 1%) + avg% /* Accumulate Avgs */
              avgu%(u3%, 2%) = avgu%(u3%, 2%) + 1%   /* Number of Days  */
L19384:     next u3%
            tran(print$(), hex(202e)) replacing
            print using L19768, -x3%, print$(1), print$(2), print$(3),    ~
            print$(4), print$(5), print$(6), print$(7), print$(8),       ~
            print$(9), print$(10), print$(11), print$(12), print$(13),   ~
            print$(14), print$(15), print$(16), print$(17), -x3%
          next x3%
          for x3% = 1% to 17%
            avg$ = " " : avg% = 0%
            if avgu%(x3%, 2%) <> 0% then avg%=avgu%(x3%,1%)/avgu%(x3%,2%)
            convert avg% to avg$, pic(###)
            if heading$(x3%) = " " then avg$ = " "
            str(print$(x3%),3,3) = avg$
          next x3%
          print using L19792, print$(1), print$(2), print$(3), print$(4), ~
          print$(5), print$(6), print$(7), print$(8), print$(9),         ~
          print$(10), print$(11), print$(12), print$(13), print$(14),    ~
          print$(15), print$(16), print$(17)
          print skip (06)
          print using L19888
          print using L19896
          print using L19904
          print using L19912
          print using L19928
          if work% = maxallowed% then print_done
L19568:   work% = work% + 1
          goto L19208

          goto select_report

        form_control_one
            edtmessage$ = " One Workcenter Per Page, Up To 17 Months Visi~
        ~bility"
            page% = page% + 1
            print page
            print using L19816, date$, runtime$, company$
            print using L19840, edtmessage$, page%
            print using L19864
            print using L19768," ", heading$(1), heading$(2), heading$(3),~
            heading$(4), heading$(5), heading$(6), heading$(7),          ~
            heading$(8), heading$(9), heading$(10), heading$(11),        ~
            heading$(12), heading$(13), heading$(14), heading$(15),      ~
            heading$(16), heading$(17)
            print using L19864
            edtmessage$ = " "
        return
        %....!....1....!....2....!....3....!....4....!....5....!....6....~
        ~!....7....!....8....!....9....!....0....!....1....!....2....!....~
        ~3..

L19768: %    -## #####  #####  #####  #####  #####  #####  #####  #####  ~
        ~#####  #####  #####  #####  #####  #####  #####  #####  ##### ##-

L19792: %Average #####  #####  #####  #####  #####  #####  #####  #####  ~
        ~#####  #####  #####  #####  #####  #####  #####  #####  #####

L19816: %RUN ########   ########               ##########################~
        ~#################################                   WCUTIL:W/C003

L19840: %                                      ##########################~
        ~#################################                     PAGE:  ####

L19864: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--
L19888: %SYMBOLS LEGEND
L19896: %==============
L19904: %BLANK  Means Day Is Completely Available (Unplanned)
L19912: %  ***  Means Day Is Un-Available  (No Original Available, Weeken~
        ~d, Or Holiday)
L19928: %       The Numbers Represent The Percentages Of Planned Utilizat~
        ~ion For That Day.  Numbers Over 100 Indicate A Potential Problem

        %  @@@  Means Day Is NOT Available, but Capacity has been Planned

        print_two
          gosub tell_them
          search calander$() = stdt_yymmdd$ to search%()
          search%(1) = max(1, search%(1) )
          pcolumn%, savep% = (search%(1) + 5) /6
          search calander$() = eddt_yymmdd$ to search%()
          search%(1) = max(1, search%(1) )
          plimit% = (search%(1) + 5) /6
          for prow% = 0 to maxallowed%-5% step 50 /* CLOSE ENOUGH!! */
            page% = 0
            if prow% > hits% then L27140
L20030:     gosub form_control_two
            for x3% = 1 to 50
              if wc$(prow% + x3%) = " " then L27100 /* Forget Days Loop */
              printline$ = wc$(prow% + x3%)
              str(printline$,129,4) = wc$(prow% + x3%)
              strt% = 29%
              for u3% = 1 to 25  /* Only room now for 25 days across */
                if prow%+x3% > hits% or pcolumn%+u3% > plimit% then L27062
                str(printline$, strt%, 4) =                              ~
                str(centers$(prow% + x3%, pcolumn% + u3%),,3) & " "
L27062:         strt% = strt% + 4%
              next u3%
              tran(printline$, hex(2a0b202e)) replacing
              wcname$ = "??????"
              call "READ100" (#1, wc$(prow% + x3%), f1%(1))
              if f1%(1) = 0 then L27087 /* Bad Workcenter, Skip GET */
              get #1 using L35100, wc$, wcname$
L27087:       str(printline$, 6,22) = wcname$
              print printline$
L27100:     next x3%
            if pcolumn% + u3% > plimit% then L27131
            pcolumn% = pcolumn% + 25
            goto L20030

L27131:     pcolumn% = savep%
L27140:   next prow%
          goto print_done

        form_control_two
            edtmessage$ = "Up To 50 Workcenters Per Page, Up To 25 Days V~
        ~isibility Per Page"
            page% = page% + 1
            print page
            print using L19816, date$, runtime$, company$
            convert (prow%+50)/50 to str(page$,,2), pic(##)
            str(page$,3 ,1) = "."
            str(page$,4,1) = bin(page% + 64, 1) /*SECTION IDENTENTIFIER */
            print using L19840, edtmessage$, page$
            print using L19864
            edtmessage$, mnth$, year$ = " "
            for x3% = 1 to 5
               printline$, title$() = " "
               strt% = 31%
               for u3% = 1 to 25
                  if pcolumn% + u3% > plimit% then L27512
                  on x3% goto L27320,L27512,L27400,L27512,L27480
L27320:           if str(calander$(pcolumn% + u3%) , x3%, 2) = year$     ~
                         then L27512
                         year$ = str(calander$(pcolumn% + u3%), x3%, 2)
                         str(printline$, strt%,      1) =                ~
                         str(calander$(pcolumn% + u3%), x3%, 1)
                         str(title$( ), strt%,      1) =                 ~
                         str(calander$(pcolumn% + u3%), x3% + 1, 1)
                         goto L27512
L27400:           if str(calander$(pcolumn% + u3%  ) , x3%, 2) = mnth$   ~
                         then L27512
                         mnth$ = str(calander$(pcolumn% + u3%), x3%, 2)
                         str(printline$, strt%,      1) =                ~
                         str(calander$(pcolumn% + u3%), x3%, 1)
                         str(title$(), strt%,      1) =                  ~
                         str(calander$(pcolumn% + u3%), x3% + 1, 1)
                         goto L27512
L27480:           str(printline$, strt%,      1) =                       ~
                  str(calander$(pcolumn% + u3%), x3%, 1)
                  str(title$(), strt%,      1) =                         ~
                  str(calander$(pcolumn% + u3%), x3% + 1, 1)
L27512:           strt% = strt% + 4%
               next u3%
               on x3% goto  ,L27620, L27560, L27620, L27580
               str(printline$,27,1), str(title$(),27,1),                 ~
               str(printline$,130,1), str(title$(),130,1) = "Y"
               goto L27600
L27560:        str(printline$,27,1), str(title$(),27,1),                 ~
               str(printline$,130,1), str(title$(),130,1) = "M"
               goto L27600
L27580:        str(printline$,27,1), str(title$(),27,1),                 ~
               str(printline$,130,1), str(title$(),130,1) = "D"
L27600:        print printline$
               print str(title$(),,132)
L27620:     next x3%
            print using L19864
        return

        REM ******** Some trivial subs *******

        tell_them
            call "SHOSTAT" ("Printing Requested Data")
            page% = 0
        return

        print_done
            runtime$ = " "
            call "TIME" (runtime$)
            print
            print "***** End of Report @ " & runtime$ & " *****"
            close printer
            call "SETPRNT" ("W/C003", " ", 0%, 1%)
            goto select_centers_for_display
        return

        call_screen
            call "GETSCRN" ("C", " ", cursor%(), 0%)
            return

        pf1315
            if keyhit% <> 13% then L28580
                call "MANUAL" ("WCUTIL  ")
                keyhit% = 15%
                return
L28580:     if keyhit% <> 15% then return
                call "PRNTSCRN"
                return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35100: FMT                      /* FILE: WCMASTR                      */~
            XX(1),               /* Status indicator for level 2 plann */~
            CH(4),               /* work-center code                   */~
            XX(1),               /* Type - used generically for specia */~
            CH(30),              /* work centre description            */~
            XX(14),              /* normal hours per day               */~
            XX(3),               /* frequency of PM's for a given work */~
            XX(6),               /* date of last prev maintenance in a */~
            490*BI(2),           /* amount of wc capacity available    */~
            490*BI(2),           /* amount of wc capacity used         */~
            XX(5)                /* filler for rest of record or inter */~

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

L40000: REM *************************************************************~
            *     S E L E C T    C E N T E R S   T O   I N C L U D E    *~
            *                                                           *~
            * ALLOWS FOR SPECIFIC SELECTION OF CENTERS TO INCLUDE       *~
            *************************************************************

            init (hex(8c)) fc$()
            pf4$ = "4)PREV CENTERS" : pf5$ = "5)NEXT CENTERS"
            if hits% <= b% + 100% then pf5$ = " "
            if b% = 0% then pf4$ = " "

            for u3% = 1 to maxallowed%
              if wc$(u3%) <> " " then fc$(u3%) = hex(81)
            next u3%

L40028:     accept                                                       ~
            at(01,02), "Select Work Centers To Be Included",             ~
            at(01,66), "Today:",                                         ~
            at(01,73), fac(hex(8c)), date$                   ,    ch(08),~
            at(02,02), fac(hex(ac)), line2$                  ,    ch(79),~
            at(03,02),  "Type any non-blank character into the box, and t~
        ~he corresponding center",                                        ~
            at (04,02), "will be included on the", fac(hex(8c)), which$, ~
                                                                         ~
        at(08,02),fac(fc$(b%+01)),select$(b%+01),fac(hex(8c)),wc$(b%+01),~
        at(08,12),fac(fc$(b%+02)),select$(b%+02),fac(hex(8c)),wc$(b%+02),~
        at(08,22),fac(fc$(b%+03)),select$(b%+03),fac(hex(8c)),wc$(b%+03),~
        at(08,32),fac(fc$(b%+04)),select$(b%+04),fac(hex(8c)),wc$(b%+04),~
        at(08,42),fac(fc$(b%+05)),select$(b%+05),fac(hex(8c)),wc$(b%+05),~
        at(08,52),fac(fc$(b%+06)),select$(b%+06),fac(hex(8c)),wc$(b%+06),~
        at(08,62),fac(fc$(b%+07)),select$(b%+07),fac(hex(8c)),wc$(b%+07),~
        at(08,72),fac(fc$(b%+08)),select$(b%+08),fac(hex(8c)),wc$(b%+08),~
        at(09,02),fac(fc$(b%+09)),select$(b%+09),fac(hex(8c)),wc$(b%+09),~
        at(09,12),fac(fc$(b%+10)),select$(b%+10),fac(hex(8c)),wc$(b%+10),~
        at(09,22),fac(fc$(b%+11)),select$(b%+11),fac(hex(8c)),wc$(b%+11),~
        at(09,32),fac(fc$(b%+12)),select$(b%+12),fac(hex(8c)),wc$(b%+12),~
        at(09,42),fac(fc$(b%+13)),select$(b%+13),fac(hex(8c)),wc$(b%+13),~
        at(09,52),fac(fc$(b%+14)),select$(b%+14),fac(hex(8c)),wc$(b%+14),~
        at(09,62),fac(fc$(b%+15)),select$(b%+15),fac(hex(8c)),wc$(b%+15),~
        at(09,72),fac(fc$(b%+16)),select$(b%+16),fac(hex(8c)),wc$(b%+16),~
        at(10,02),fac(fc$(b%+17)),select$(b%+17),fac(hex(8c)),wc$(b%+17),~
        at(10,12),fac(fc$(b%+18)),select$(b%+18),fac(hex(8c)),wc$(b%+18),~
        at(10,22),fac(fc$(b%+19)),select$(b%+19),fac(hex(8c)),wc$(b%+19),~
        at(10,32),fac(fc$(b%+20)),select$(b%+20),fac(hex(8c)),wc$(b%+20),~
        at(10,42),fac(fc$(b%+21)),select$(b%+21),fac(hex(8c)),wc$(b%+21),~
        at(10,52),fac(fc$(b%+22)),select$(b%+22),fac(hex(8c)),wc$(b%+22),~
        at(10,62),fac(fc$(b%+23)),select$(b%+23),fac(hex(8c)),wc$(b%+23),~
        at(10,72),fac(fc$(b%+24)),select$(b%+24),fac(hex(8c)),wc$(b%+24),~
        at(11,02),fac(fc$(b%+25)),select$(b%+25),fac(hex(8c)),wc$(b%+25),~
        at(11,12),fac(fc$(b%+26)),select$(b%+26),fac(hex(8c)),wc$(b%+26),~
        at(11,22),fac(fc$(b%+27)),select$(b%+27),fac(hex(8c)),wc$(b%+27),~
        at(11,32),fac(fc$(b%+28)),select$(b%+28),fac(hex(8c)),wc$(b%+28),~
        at(11,42),fac(fc$(b%+29)),select$(b%+29),fac(hex(8c)),wc$(b%+29),~
        at(11,52),fac(fc$(b%+30)),select$(b%+30),fac(hex(8c)),wc$(b%+30),~
        at(11,62),fac(fc$(b%+31)),select$(b%+31),fac(hex(8c)),wc$(b%+31),~
        at(11,72),fac(fc$(b%+32)),select$(b%+32),fac(hex(8c)),wc$(b%+32),~
        at(12,02),fac(fc$(b%+33)),select$(b%+33),fac(hex(8c)),wc$(b%+33),~
        at(12,12),fac(fc$(b%+34)),select$(b%+34),fac(hex(8c)),wc$(b%+34),~
        at(12,22),fac(fc$(b%+35)),select$(b%+35),fac(hex(8c)),wc$(b%+35),~
        at(12,32),fac(fc$(b%+36)),select$(b%+36),fac(hex(8c)),wc$(b%+36),~
        at(12,42),fac(fc$(b%+37)),select$(b%+37),fac(hex(8c)),wc$(b%+37),~
        at(12,52),fac(fc$(b%+38)),select$(b%+38),fac(hex(8c)),wc$(b%+38),~
        at(12,62),fac(fc$(b%+39)),select$(b%+39),fac(hex(8c)),wc$(b%+39),~
        at(12,72),fac(fc$(b%+40)),select$(b%+40),fac(hex(8c)),wc$(b%+40),~
        at(13,02),fac(fc$(b%+41)),select$(b%+41),fac(hex(8c)),wc$(b%+41),~
        at(13,12),fac(fc$(b%+42)),select$(b%+42),fac(hex(8c)),wc$(b%+42),~
        at(13,22),fac(fc$(b%+43)),select$(b%+43),fac(hex(8c)),wc$(b%+43),~
        at(13,32),fac(fc$(b%+44)),select$(b%+44),fac(hex(8c)),wc$(b%+44),~
        at(13,42),fac(fc$(b%+45)),select$(b%+45),fac(hex(8c)),wc$(b%+45),~
        at(13,52),fac(fc$(b%+46)),select$(b%+46),fac(hex(8c)),wc$(b%+46),~
        at(13,62),fac(fc$(b%+47)),select$(b%+47),fac(hex(8c)),wc$(b%+47),~
        at(13,72),fac(fc$(b%+48)),select$(b%+48),fac(hex(8c)),wc$(b%+48),~
        at(14,02),fac(fc$(b%+49)),select$(b%+49),fac(hex(8c)),wc$(b%+49),~
        at(14,12),fac(fc$(b%+50)),select$(b%+50),fac(hex(8c)),wc$(b%+50),~
        at(14,22),fac(fc$(b%+51)),select$(b%+51),fac(hex(8c)),wc$(b%+51),~
        at(14,32),fac(fc$(b%+52)),select$(b%+52),fac(hex(8c)),wc$(b%+52),~
        at(14,42),fac(fc$(b%+53)),select$(b%+53),fac(hex(8c)),wc$(b%+53),~
        at(14,52),fac(fc$(b%+54)),select$(b%+54),fac(hex(8c)),wc$(b%+54),~
        at(14,62),fac(fc$(b%+55)),select$(b%+55),fac(hex(8c)),wc$(b%+55),~
        at(14,72),fac(fc$(b%+56)),select$(b%+56),fac(hex(8c)),wc$(b%+56),~
        at(15,02),fac(fc$(b%+57)),select$(b%+57),fac(hex(8c)),wc$(b%+57),~
        at(15,12),fac(fc$(b%+58)),select$(b%+58),fac(hex(8c)),wc$(b%+58),~
        at(15,22),fac(fc$(b%+59)),select$(b%+59),fac(hex(8c)),wc$(b%+59),~
        at(15,32),fac(fc$(b%+60)),select$(b%+60),fac(hex(8c)),wc$(b%+60),~
        at(15,42),fac(fc$(b%+61)),select$(b%+61),fac(hex(8c)),wc$(b%+61),~
        at(15,52),fac(fc$(b%+62)),select$(b%+62),fac(hex(8c)),wc$(b%+62),~
        at(15,62),fac(fc$(b%+63)),select$(b%+63),fac(hex(8c)),wc$(b%+63),~
        at(15,72),fac(fc$(b%+64)),select$(b%+64),fac(hex(8c)),wc$(b%+64),~
        at(16,02),fac(fc$(b%+65)),select$(b%+65),fac(hex(8c)),wc$(b%+65),~
        at(16,12),fac(fc$(b%+66)),select$(b%+66),fac(hex(8c)),wc$(b%+66),~
        at(16,22),fac(fc$(b%+67)),select$(b%+67),fac(hex(8c)),wc$(b%+67),~
        at(16,32),fac(fc$(b%+68)),select$(b%+68),fac(hex(8c)),wc$(b%+68),~
        at(16,42),fac(fc$(b%+69)),select$(b%+69),fac(hex(8c)),wc$(b%+69),~
        at(16,52),fac(fc$(b%+70)),select$(b%+70),fac(hex(8c)),wc$(b%+70),~
        at(16,62),fac(fc$(b%+71)),select$(b%+71),fac(hex(8c)),wc$(b%+71),~
        at(16,72),fac(fc$(b%+72)),select$(b%+72),fac(hex(8c)),wc$(b%+72),~
        at(17,02),fac(fc$(b%+73)),select$(b%+73),fac(hex(8c)),wc$(b%+73),~
        at(17,12),fac(fc$(b%+74)),select$(b%+74),fac(hex(8c)),wc$(b%+74),~
        at(17,22),fac(fc$(b%+75)),select$(b%+75),fac(hex(8c)),wc$(b%+75),~
        at(17,32),fac(fc$(b%+76)),select$(b%+76),fac(hex(8c)),wc$(b%+76),~
        at(17,42),fac(fc$(b%+77)),select$(b%+77),fac(hex(8c)),wc$(b%+77),~
        at(17,52),fac(fc$(b%+78)),select$(b%+78),fac(hex(8c)),wc$(b%+78),~
        at(17,62),fac(fc$(b%+79)),select$(b%+79),fac(hex(8c)),wc$(b%+79),~
        at(17,72),fac(fc$(b%+80)),select$(b%+80),fac(hex(8c)),wc$(b%+80),~
        at(18,02),fac(fc$(b%+81)),select$(b%+81),fac(hex(8c)),wc$(b%+81),~
        at(18,12),fac(fc$(b%+82)),select$(b%+82),fac(hex(8c)),wc$(b%+82),~
        at(18,22),fac(fc$(b%+83)),select$(b%+83),fac(hex(8c)),wc$(b%+83),~
        at(18,32),fac(fc$(b%+84)),select$(b%+84),fac(hex(8c)),wc$(b%+84),~
        at(18,42),fac(fc$(b%+85)),select$(b%+85),fac(hex(8c)),wc$(b%+85),~
        at(18,52),fac(fc$(b%+86)),select$(b%+86),fac(hex(8c)),wc$(b%+86),~
        at(18,62),fac(fc$(b%+87)),select$(b%+87),fac(hex(8c)),wc$(b%+87),~
        at(18,72),fac(fc$(b%+88)),select$(b%+88),fac(hex(8c)),wc$(b%+88),~
        at(19,02),fac(fc$(b%+89)),select$(b%+89),fac(hex(8c)),wc$(b%+89),~
        at(19,12),fac(fc$(b%+90)),select$(b%+90),fac(hex(8c)),wc$(b%+90),~
        at(19,22),fac(fc$(b%+91)),select$(b%+91),fac(hex(8c)),wc$(b%+91),~
        at(19,32),fac(fc$(b%+92)),select$(b%+92),fac(hex(8c)),wc$(b%+92),~
        at(19,42),fac(fc$(b%+93)),select$(b%+93),fac(hex(8c)),wc$(b%+93),~
        at(19,52),fac(fc$(b%+94)),select$(b%+94),fac(hex(8c)),wc$(b%+94),~
        at(19,62),fac(fc$(b%+95)),select$(b%+95),fac(hex(8c)),wc$(b%+95),~
        at(19,72),fac(fc$(b%+96)),select$(b%+96),fac(hex(8c)),wc$(b%+96),~
        at(20,02),fac(fc$(b%+97)),select$(b%+97),fac(hex(8c)),wc$(b%+97),~
        at(20,12),fac(fc$(b%+98)),select$(b%+98),fac(hex(8c)),wc$(b%+98),~
        at(20,22),fac(fc$(b%+99)),select$(b%+99),fac(hex(8c)),wc$(b%+99),~
        at(20,32),fac(fc$(b%+100)),select$(b%+100),fac(hex(8c)),         ~
                                                             wc$(b%+100),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf1$,                         ~
               at (24,02), "2)To Include All",                           ~
               at (23,24), fac(hex(8c)),   pf4$,                         ~
               at (24,24), fac(hex(84)),   pf5$,                         ~
               at (23,45), fac(hex(8c)),   pf12$,                        ~
               at (24,45), "13)Instructions",                            ~
               at (23,66), "15)Print Screen",                            ~
               at (24,65), fac(hex(8c)),   pf16$,                        ~
                                                                         ~
               keys(hex(00010204050c0d0f10)),                            ~
               key (keyhit%)

               if keyhit% <> 4 then L40280
                  b% = max(0%, b% - 100%)
                  goto L40000

L40280:        if keyhit% <> 5 then L40296
                  b% = min(maxallowed% - 100%, b% + 100%)
                  goto L40000

L40296:        gosub pf1315
               if keyhit% = 15% then L40028
               return

L40500: REM *************************************************************~
            *       U T I L I Z A T I O N           D I S P L A Y       *~
            *                                                           *~
            * WC UTILIZATION  DISPLAY SCREEN.                           *~
            *************************************************************

L40530:     accept                                                       ~
               at (01,02), "Work Center Utilization Summary",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(84)), title$(1)              , ch(79),~
               at (03,02), fac(hex(84)), title$(2)              , ch(79),~
               at (04,02), fac(hex(84)), title$(3)              , ch(79),~
               at (05,02), fac(hex(84)), title$(4)              , ch(79),~
               at (06,02), fac(hex(84)), title$(5)              , ch(79),~
               at (07,02), fac(hex(a4)), title$(6)              , ch(79),~
               at (08,02), fac(hex(8c)), line$(01)              , ch(79),~
               at (09,02), fac(hex(8c)), line$(02)              , ch(79),~
               at (10,02), fac(hex(8c)), line$(03)              , ch(79),~
               at (11,02), fac(hex(8c)), line$(04)              , ch(79),~
               at (12,02), fac(hex(8c)), line$(05)              , ch(79),~
               at (13,02), fac(hex(8c)), line$(06)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(07)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(08)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(09)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (23,02), fac(hex(8c)), inst$(1)               , ch(79),~
               at (24,02), fac(hex(8c)), inst$(2)               , ch(79),~
                                                                         ~
               keys(hex(000102040506070d0f10)),                          ~
               key (keyhit%)

               gosub pf1315
               if keyhit% = 15% then L40530
               return

L41000: REM *************************************************************~
            *                 L E D G E N D                             *~
            *                                                           *~
            * EXPLAINS CODES USED ON UTILIZATION SCREEN                 *~
            *************************************************************
            inpmessage$ = " "
L41060:     accept                                                       ~
               at (01,02), "Work Center Utilization Display Legend",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                 , ch(08), ~
               at (02,02), fac(hex(ac)), line2$                , ch(79), ~
               at (07,02), "...  Means Day is Completely Available.",    ~
                                                                         ~
               at (09,02), "***  Means Day is Not Available, Such as Week~
        ~ends & Holidays.",                                               ~
                                                                         ~
               at (11,02), "@@@  Means Day is Not Available, but Capacity~
        ~ has been Planned.",                                             ~
                                                                         ~
               at (13,02), "The Numbers Represent the Percentages of Plan~
        ~ned Capacity ",                                                  ~
               at (14,06), "Utilization for that Day",                   ~
                                                                         ~
                                                                         ~
               at (22,02), fac(hex(ac)), inpmessage$,             ch(79),~
               at (23,02), "RETURN)Return to Display",                   ~
               at (23,65), "(13)Instructions",                           ~
               at (24,65), "(15)Print Screen",                           ~
                                                                         ~
               keys(hex(000d0f10)),                                      ~
               key (keyhit%)

               gosub pf1315
               if keyhit% = 15% then L41060
               return

L41500: REM *************************************************************~
            *     R E P O R T    S E L E C T   S C R E E N              *~
            *                                                           *~
            * SUB-MENU TO DETERMINE WICH REPORT TO PERFORM              *~
            *************************************************************

              errormsg$ =  "             USE THE PF KEYS TO SELECT THE CO~
        ~RRESPONDING OPTION"

L41540:     accept                                                       ~
               at (01,02), "Work Center Utilization Reports",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                 , ch(08), ~
               at (02,02), fac(hex(ac)), line2$                , ch(79), ~
               at (07,12), "01)   One Work Center per Page, Optionally In~
        ~cludes up ",                                                     ~
               at (08,18), "to All 490 Days on the One Page",            ~
               at (10,12), "02)   50 Work Centers per Page, Shows 25 Days~
        ~ per Page",                                                      ~
               at (11,18), "up To 20 Sections will be Printed to Cover Al~
        ~l the",                                                          ~
               at (12,18), "Requested Days",                             ~
               at (14,12), "16)   OR (RETURN) will Return You to Select W~
        ~rk Ctrs to be Included",                                         ~
               at (20,02), fac(hex(a4)), errormsg$,                      ~
         at (23,02), "      Both Reports Allow A Range Of Centers And/or ~
        ~Days To Be Specified",                                           ~
                                                                         ~
               keys(hex(0001020f10)),                                    ~
               key (keyhit%)

               errormsg$ = " "
               gosub pf1315
               if keyhit% = 15% then L41540
               return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   REPORT  1        *~
            *                                                           *~
            * INPUTS SELECTIONS FOR REPORT 1                            *~
            *************************************************************

            deffn'102(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub                                      ~
                                    L42585,         /* FIRST DATE       */~
                                    L42585          /* LAST DATE        */
                     if errormsg$ <> " " then print at (1,1); bell
                     goto L42620

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42585:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42620:     accept                                                       ~
               at (01,02), "Work Center Utilization Report",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), report$                , ch(40),~
               at (04,02), fac(hex(84)), errormsg$              , ch(79),~
               at (06,02), "Starting Date",                              ~
               at (06,30), fac(lfac$( 1)), begin_date$          , ch(10),~
               at (07,02), "Ending Date",                                ~
               at (07,30), fac(lfac$( 2)), ending_date$         , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,55), "(13)Instructions",                           ~
               at (23,55), "(15)Print Screen",                           ~
               at (24,55), "(16)Return To Display Mode",                 ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               gosub pf1315
               if keyhit% = 15% then L42620
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   REPORT  1         *~
            *                                                           *~
            * SCREEN FOR EDITING SELECTIONS FOR REPORT 1                *~
            *************************************************************

            deffn'112(fieldnr%)
                  init(hex(84)) lfac$()
                     if fieldnr% > 0% then gosub L43070 /* All Are Same */
                  if errormsg$ <> " " then print at (1,1); bell
                  goto L43105

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43070:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43105:     accept                                                       ~
               at (01,02), "Work Center Utilization Report",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), report$                , ch(40),~
               at (04,02), fac(hex(84)), errormsg$              , ch(79),~
               at (06,02), "Starting Date",                              ~
               at (06,30), fac(lfac$( 1)), begin_date$          , ch(10),~
               at (07,02), "Ending Date",                                ~
               at (07,30), fac(lfac$( 2)), ending_date$         , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Print Report",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               gosub pf1315
               if keyhit% = 15% then L43105
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub                                      ~
                                    L51210,         /* START_DATE$      */~
                                    L51270          /* END_DATE$        */
                     return

L51210:     REM TEST DATA FOR START_DATE$
                call "DATEOKC" (begin_date$, u3%, errormsg$)
                if errormsg$ <> " " then return
                start_date$ = begin_date$
                call "DATUFMTC" (start_date$)
                tempdate$ = start_date$
                call "DATEFMT" (tempdate$, ccyymmdd%, ccyymmdd$)
                stdt_yymmdd$ = str(ccyymmdd$, 3%, 6%)
                search calander$() = stdt_yymmdd$ to search%()
                if end_date$ = " " or end_date$ = blankdate$ then L51335
                if start_date$ <= end_date$ then L51335
                errormsg$ = "START DATE CANNOT BE LATER THAN ENDING DATE"
                return

L51270:     REM TEST DATA FOR END_DATE$
                call "DATEOKC" (ending_date$, u3%, errormsg$)
                if errormsg$ <> " " then return
                end_date$ = ending_date$
                call "DATUFMTC" (end_date$)
                tempdate$ = end_date$
                call "DATEFMT" (tempdate$, ccyymmdd%, ccyymmdd$)
                eddt_yymmdd$ = str(ccyymmdd$, 3%, 6%)
                if start_date$ <= end_date$ then L51330
                errormsg$ = "START DATE CANNOT BE LATER THAN ENDING DATE"
                return
L51330:         search calander$() = eddt_yymmdd$ to search%()
L51335:         if errormsg$ <> " " then return
                if search%(1) = 0 then errormsg$ =                       ~
                "DATE IS NOT WITHIN THE PLANNED CALANDER ("& calander$(1)~
                & " THRU " & calander$(490) & "). PLEASE RE-ENTER."
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")

            end
