REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
    *                                                           *~
    *   GGG   EEEEE  N   N  FFFFF  M   M  TTTTT                 *~
    *  G      E      NN  N  F      MM MM    T                   *~
    *  G GGG  EEEE   N N N  FFFF   M M M    T                   *~
    *  G   G  E      N  NN  F      M   M    T                   *~
    *   GGG   EEEEE  N   N  F      M   M    T                   *~
    *                                                           *~
    *-----------------------------------------------------------*~
    * GENFMT   - GENERATES A BASIC FORMAT STATEMENT FROM THE    *~
    *            PASSED PARENT STRUCTURE ARRAY, STARTING AT THE *~
    *            PASSED BASIC LINE NUMBER.                      *~
    *            MAXIMUM NUMBER OF FIELDS IS MAXFIELDS%         *~
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
    * 04/26/83 ! ORIGINAL                                 ! ECR *~
    * 07/24/84 ! MODIFIED FOR NEW S.E.S. FILES/UTILITIES  ! LDJ *~
    * 04/23/96 ! Removed line numbers from generated code ! ERN *~
    * 07/29/97 ! Corrected output file format label       ! DER *~
    CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "GENFMT" (#1, #4, parent$, record$(), maxfields%)

        dim                                                              ~
            fmtdescr$50,                 /* FIELD DESCRIPTION          */~
            fmt$13,                      /* FORMAT & ","               */~
            out$255,                     /* LINE BUFFER                */~
            outline$255,                 /* LINE TO WRITE OUT          */~
            parent$16,                   /* PARENT CODE                */~
            parntfmt$22,                 /* format label               */~
            pos$4,                       /* file format position       */~
            record$(1001)256,            /* PARENT COMPONENTS ARRAY    */~
            t$1,                         /* tilde or blank             */~
            tilde$1,                     /* tilde                      */~
            txtfmt$3                     /* Literal FMT or 3 spaces    */


REM *************************************************************~
    *                  Release Version ID Section               *~
    *************************************************************
        dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
REM *************************************************************

        t$        = " "
        tilde$    = hex(7E)
        txtfmt$   = "fmt"    /* for first write, 3 spaces thereafter */
        pos%      = 1%
        t$        = tilde$

        parntfmt$ = parent$ & "_fmt:"

        put out$, using L20070, parntfmt$, parent$, t$
        gosub '180(0%)

        for f% = 1% to maxfields%               /* FORMATS          */
            fmtdescr$ = " "
            fmt$      = str(record$(f%),176%, 12%)
            if fmt$ = " " then get_nxt_fld

            /* Change format from uppercase to lowercase */
            for tolower% = 1% to 12%
                if str(fmt$, tolower%, 1%) >= hex(41) and ~
                   str(fmt$, tolower%, 1%) <= hex(5A) then ~
                   str(fmt$, tolower%, 1%) = add hex(20)
            next tolower%


            call "READ100" (#1, str(record$(f%), 1%, 16%), f1%)
            if f1% = 0% then get_position

            get #1 using L10075, fmtdescr$
L10075:         fmt pos(41), ch(50)

          get_position
            /* store position of field */
            convert pos% to pos$, pic(####)

            /* get length of current field */
            l% = 1%
            convert str(record$(f%), 188%, 4%) to l%, data goto pos_calc
            /* calculate new position */
          pos_calc
            pos% = pos% + l%

            if f% <> maxfields% then fmt$ = fmt$ & "," ~
                                else t$   = " "
            put out$, using L20110, txtfmt$, pos$, fmt$, ~
                                    str(fmtdescr$,1,40), t$
            gosub'180(10%)

            /* finished with txtfmt$ as FMT so clear it */
            txtfmt$ = " "

          get_nxt_fld
        next f%

        put out$, using L20140, " "              /* EXTRA LINE       */
        gosub'180(-1%)

        put out$, using L20140, " "              /* EXTRA LINE       */
        gosub'180(-1%)

        end

REM WRITE THE PROGRAM LINE, INCREMENT LINE$ ACCRDING TO TEMP%
deffn'180(temp%)
        init(" ") outline$
        str(outline$, 1%) = out$
        write #4, using L10250, outline$
L10250:     fmt ch(255)
        out$  = " "
        temp% = temp%
return


REM *************************************************************~
    *             F O R M A T   S T A T E M E N T S             *~
    *                                                           *~
    * FORMAT STATEMENTS FOR ALL THE CODE GENERATION ROUTINES    *~
    * WILL BE FOUND HERE.                                       *~
    *************************************************************
L20070: %#####################          /* FILE: ########                          */#
L20110: %### pos(####), #############   /* ########################################*/#
L20140: %#
