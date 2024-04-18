        REM *************************************************************~
            *                                                           *~
            *  W   W  H   H  IIIII   CCC   H   H  PPPP   EEEEE  RRRR    *~
            *  W   W  H   H    I    C   C  H   H  P   P  E      R   R   *~
            *  W   W  HHHHH    I    C      HHHHH  PPPP   EEEE   RRRR    *~
            *  W W W  H   H    I    C   C  H   H  P      E      R  R    *~
            *   W W   H   H  IIIII   CCC   H   H  P      EEEEE  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WHICHPER - SIMILAR TO WHICHMON.  TAKES DATE PASSED AND    *~
            *            COMPARES IT TO THE G/L PERIOD OPEN (FROM       *~
            *            THE G/L FISCAL DATES RECORD IN SYSFILE2)       *~
            *            THEN SEES IF THIS DATE IS VALID TO POST TO.    *~
            *            STANDARD CRITERIA IS THAT DATE MUST BE IN THE  *~
            *            CURRENT PERIOD OPEN, THE PERIOD IMMEDIATELY    *~
            *            FOLLOWING, OR THE IMEDIATE PRECEEDING PERIOD.  *~
            *            WHAT IS ACCEPTABLE AND WHAT IS NOT AS FAR AS   *~
            *            HOW THE DATE PASTED COMPARES TO THE CURRENT    *~
            *            G/L PERIOD OPEN CAN BE MODIFIED HERE.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/10/83 ! ORIGINAL                                 ! HES *~
            * 10/11/90 ! Replaced ACCEPT with call to GETPARM     ! KAB *~
            *************************************************************

            sub "WHICHPER" (#1, date$, whichmonth%)

            dim date$8,                  /* MODULE DATE COMING IN      */~
                temp$8,                  /* MODULE DATE COMING IN      */~
                f1%(1),                  /* RECORD ON FILE STATUS FLAG */~
                u$(17)8                  /* FICAL DATE STRUCTURE       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************
            whichmonth% = 0
            u$() = " "
            temp$ = date$
            call "DATEOK" (temp$, f1%(1), str(u$(),,30))
            if u$() <> " " then end
            call "DATUNFMT" (temp$)

                call "READ100" (#1, "FISCAL DATES", f1%(1))
                      if f1%(1) <> 0 then L04410
        /*      ACCEPT  AT(1,2), "ERROR IN G/L FISCAL DATE STRUCTURE - EX~
        IT IMMEDIATELY AND RECTIFY"
                END */
                   call "GETPARM" addr("I ", "A",                        ~
                                       "WHICHPER", " ", "0001", "WCHPER",~
                   2%,                                                   ~
                   "Error in G/L Fiscal Date Structure.        ",    44%,~
                   "Press ENTER to acknowledge and EXIT Program",    44%)
                   end

L04410:         get #1, using L04510 , periods%, u$(), monthopen%
L04510:         FMT XX(20), BI(2), 17*CH(8), BI(2)

                temp% = 0
                for u3% = 1 to 17
                  if u$(u3%) = " " then L05110
                     if temp$ < u$(u3%) then temp% = u3% - 1
                     if temp$ < u$(u3%) then L05120
L05110:         next u3%
L05120:         if u3% = 1 then end
                if temp% = 0 then temp% = 17

                if temp% = 13 and periods% = 12 then temp% = 12
                if monthopen% = temp%     then L05510
                if monthopen% = temp% + 1 then L05510
                if monthopen% = temp% - 1 then L05510
           if monthopen% = 12 and periods% = 12 and temp% = 14 then L05510
           if monthopen% = 14 and periods% = 12 and temp% = 12 then L05510
                end
L05510:         whichmonth% = temp%
                end
