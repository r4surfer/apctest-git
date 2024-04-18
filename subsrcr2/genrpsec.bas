        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   EEEEE  N   N  RRRR   PPPP    SSS   EEEEE   CCC    *~
            *  G      E      NN  N  R   R  P   P  S      E      C   C   *~
            *  G GGG  EEEE   N N N  RRRR   PPPP    SSS   EEEE   C       *~
            *  G   G  E      N  NN  R   R  P          S  E      C   C   *~
            *   GGG   EEEEE  N   N  R   R  P       SSS   EEEEE   CCC    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GENRPSEC - Subroutine to generate the generate report     *~
            *             section for the report program generator      *~
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
            * 03/08/88 ! Original                                 ! MJB *~
            * 03/31/93 ! PRR 12751 Standard Page 0 FAC removal    ! JIM *~
            * 07/11/97 ! Changed for UNIX. No Line Numbers.       ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "GENRPSEC" (mode%, #3, outfile$)
                             /* Mode = 1 for extract data section    */
        dim outfile$8,       /* Mode = 2 for generate report section */~
            tilde$1,         /* The Tilde Character                  */~
            writeLn$255      /* Temporary Work Variable              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto    L00392
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L00392: REM *************************************************************
            tilde$ = hex(7e)
            if mode% = 2% then goto gen_report

*       * 1st generate the EXTRACT_DATA Section
            put writeLn$, using  L02080 , " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L02100 , " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L02120 , " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L02140 , " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L02160 , " "
            gosub WriteToFile
            put writeLn$, using  L02190 , " "
            gosub WriteToFile
            put writeLn$, using  L02210 , " "
            gosub WriteToFile
            put writeLn$, using  L02230 , " "
            gosub WriteToFile
            put writeLn$, using  L02260 , " "
            gosub WriteToFile
            put writeLn$, using  L02280 , " "
            gosub WriteToFile
            put writeLn$, using  L02300 , " "
            gosub WriteToFile

            end

*       * Now generate the GENERATE_REPORT Section
        gen_report
            put writeLn$, using  L01100, " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L01120, " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L01140, " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L01160, " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L01180, " "
            gosub WriteToFile
            put writeLn$, using  L01210, " "    /* Gen Report 30000 Section */
            gosub WriteToFile
            put writeLn$, using  L01230, " "
            gosub WriteToFile
            put writeLn$, using  L01240, " "
            gosub WriteToFile
            put writeLn$, using  L01250, " "
            gosub WriteToFile
            put writeLn$, using  L01270, " "
            gosub WriteToFile
            put writeLn$, using  L01290, " "
            gosub WriteToFile
            put writeLn$, using  L01310, " "
            gosub WriteToFile
            put writeLn$, using  L01330, " "
            gosub WriteToFile
            put writeLn$, using  L01350, " "
            gosub WriteToFile
            put writeLn$, using  L01370, " "
            gosub WriteToFile
            put writeLn$, using  L01390, " "
            gosub WriteToFile
            put writeLn$, using  L01410, " "
            gosub WriteToFile
            put writeLn$, using  L01430, " "
            gosub WriteToFile
            put writeLn$, using  L01450, " "
            gosub WriteToFile
            put writeLn$, using  L01470, " "
            gosub WriteToFile
            put writeLn$, using  L01490, " "
            gosub WriteToFile
            put writeLn$, using  L01510, " "
            gosub WriteToFile
            put writeLn$, using  L01530, " "
            gosub WriteToFile
            put writeLn$, using  L01550, " "
            gosub WriteToFile
            put writeLn$, using  L01570, " "
            gosub WriteToFile
            put writeLn$, using  L01590, " ", outfile$
            gosub WriteToFile
            put writeLn$, using  L01610, " "
            gosub WriteToFile
            put writeLn$, using  L01630, " "
            gosub WriteToFile
            put writeLn$, using  L01650, " "
            gosub WriteToFile
            put writeLn$, using  L01670, " "
            gosub WriteToFile
            put writeLn$, using  L01690, " "
            gosub WriteToFile
            put writeLn$, using  L01710, " "
            gosub WriteToFile
            put writeLn$, using  L01730, " "
            gosub WriteToFile
            put writeLn$, using  L01750, " "
            gosub WriteToFile
            put writeLn$, using  L01752, " "
            gosub WriteToFile
            put writeLn$, using  L01754, " "
            gosub WriteToFile
            put writeLn$, using  L01756, " "
            gosub WriteToFile
            put writeLn$, using  L01770, " "
            gosub WriteToFile
            put writeLn$, using  L01790, " "
            gosub WriteToFile
            put writeLn$, using  L01810, " "
            gosub WriteToFile
            put writeLn$, using  L01830, " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L01860, " "
            gosub WriteToFile
            put writeLn$, using  L01880, " "
            gosub WriteToFile
            put writeLn$, using  L01900, " "
            gosub WriteToFile
            put writeLn$, using  L01920, " "
            gosub WriteToFile
            put writeLn$, using  L01940, " ", tilde$
            gosub WriteToFile
            put writeLn$, using  L01970, " "
            gosub WriteToFile
            put writeLn$, using  L01990, " "
            gosub WriteToFile
            put writeLn$, using  L02010, " "
            gosub WriteToFile
            put writeLn$, using  L02030, " "
            gosub WriteToFile

            end

WriteToFile
      write #3 using write_fmt, writeLn$
      writeLn$ = " "
         return

write_fmt: %#############################################################~
###################


*       * These are the Image statements for the generate report section
l01100: %#   REM ************************************************************* #
l01120: %#       *       G e n e r a t e   R e p o r t   S e c t i o n       * #
l01140: %#       *-----------------------------------------------------------* #
l01160: %#       * Main section for report generation.                       * #
l01180: %#       *************************************************************
l01210: %#     generate_report
l01230: %#         select printer(134)
l01240: %#         time$ = " "  :  call "TIME" (time$)
l01250: %#         call "SETPRNT" ("RPTID", " ", 0%, 0%)
l01270: %#         pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
l01290: %#         if lcntr% > 56% then gosub page_head
L01310: %#
L01330: %** Report Generation Logic goes here.#
L01350: %#
l01370: %#     end_report                /* Report Ending Routine */
l01390: %#         print skip(2)
l01410: %#         print using FinalLine /* End of Report Message */

l01430: %#         close printer

l01450: %#         call "SETPRNT" (" ", " ", 0%, 1%)

l01470: %#         goto inputmode

L01490: %#                                                               ~

l01510: %#     page_head              /* Page Heading Print Routine */

l01530: %#         pcntr% = pcntr% + 1%

l01550: %#         if pcntr% = 0% then gosub print_params

l01570: %#         print page        /* Top of Form */

l01590: %#         print using Title1, date$, time$, company$, "########"

l01610: %#         print using L60110, rpttitle$, pcntr%

l01630: %#         print

l01650: %#         lcntr% = 3%

l01670: %#         return

L01690: %#                                                               ~

l01710: %#     print_params           /* Print Page Zero */

l01730: %#         print page

l01750: %#FindHex: i% = pos(str(i$()) > hex(7f))

l01752: %#         if i% = 0% then ParmPrt

l01754: %#             str(i$(), i%, 1%) = hex(20)

l01756: %#             goto FindHex

L01770: %#ParmPrt: print using Pg0Ln, rpttitle$

l01790: %#         print skip(3)

l01810: %#         print tab(26);

l01830: %#         print "------------------------- Report Selection Para~
        ~meters #

L01860: %--------------------------"#

l01880: %#         print

l01900: %#         for x% = 6% to 17% : print tab(26); i$(x%) : next x%

l01920: %#         print tab(26);

l01940: %#         print "-----------------------------------------------~
        ~-------#

L01970: %--------------------------"#

l01990: %#         pcntr% = pcntr% + 1%

l02010: %#         return

L02030: %#                                                               ~


*       * These are the Image statements for the extract data section

l02080: %#   rem ************************************************************* #
L02100: %#       *           E x t r a c t   R e p o r t   D a t a           * #
L02120: %#       *-----------------------------------------------------------* #
L02140: %#       * Data Extraction section for report.                       * #
L02160: %#       *************************************************************
l02190: %#     extract_data
L02210: %#
L02230: %** Insert Logic here for data extraction and workfile creation.#
L02260: %#
l02280: %#         goto generate_report
L02300: %#
