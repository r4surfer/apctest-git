        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  DDDD   DDDD    CCC   TTTTT  L       GGG   EEEEE  N   N   *~
            *  D   D  D   D  C   C    T    L      G      E      NN  N   *~
            *  D   D  D   D  C        T    L      G GGG  EEEE   N N N   *~
            *  D   D  D   D  C   C    T    L      G   G  E      N  NN   *~
            *  DDDD   DDDD    CCC     T    LLLLL   GGG   EEEEE  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DDCTLGEN - Creates a Catalog Import file from the file    *~
            *            definition passed by the calling program.      *~
            *            Displays screen allowing user to change field  *~
            *            and/or key names before creating catalog file. *~
            *            Output Generated is in Easysoft Fixed Width    *~
            *            Text Format. Caller is responsible for         *~
            *            Creating and closing the output file.          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/21/83 ! ORIGINAL                                 ! ECR *~
            * 07/30/84 ! EXTENSIVE MODIFICATIONS FOR NEW S.E.S.   ! LDJ *~
            * 11/12/84 ! CHANGED DISPLAY CODE FOR PD FIELDS FROM  ! LDJ *~
            *          !   0 TO 1 - CAUSED A MINOR BUG IN REPORT. !     *~
            * 02/11/85 ! FIXED MORE BUGS IN CONTROL FILE DEFAULTS ! LDJ *~
            *          !  (MUST HAVE STARTED W/BAD OR OLD VERSION)!     *~
            * 08/08/85 ! Modified to translate all embedded blanks! LDJ *~
            *          !   within a field name to dashes "-".     !     *~
            * 04/10/86 ! Many Changes.  Fixed Bugs & Added ability! LDJ *~
            *          !   to Generate EZQUERY control files.     !     *~
            *          !   (Actually Rewrote & Renamed the Turkey)!     *~
            * 07/22/86 ! Bug Fix.  non alpha field names in Inquir! LDJ *~
            * 09/09/86 ! Bug Fix.  Max PD field size was 10,now 15! LDJ *~
            * 11/11/87 ! Removed TYPE$() argument & RENAMED.      ! LDJ *~
            * 04/18/88 ! Corrected problem with EZQUERY - did not ! LDJ *~
            *          ! like duplicate ALIAS NAMES.  Also changed!     *~
            *          ! all embedded blanks & special characters !     *~
            *          ! in an alias name to dashes "-".  Also    !     *~
            *          ! reduced max alias name to 30 characters  !     *~
            *          ! in response to new EZQUERY feature (bug).!     *~
            * 02/15/93 ! PRR 12167 force FILEORG = 'I'ndexed.     ! JIM *~
            * 08/25/97 ! Rewrite to generate Easysoft ODBC Catalog! LDJ *~
            *          ! Import Files under UNIX/NT.              !     *~
            * 09/30/97 ! Bug Fixes                                ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

        sub "DDCTLGEN" (#3, parent$, library$, version$, parent_type$,   ~
                        max%, reclen$, record$(), errorno%)

        dim                                                              ~
            cat_fields$(512)19,          /* Control File Field Names   */~
            data_type$8,                 /* SES Data Usage Type        */~
            datatype$64,                 /* Easysoft Data Type         */~
            date_format$64,              /* Easysoft Date / Time Format*/~
            decimal_places$10,           /* SES/Easysoft Decimal/Scale */~
            extlen$10,                   /* External Length of Field   */~
            fmt$2,                       /* Decoded Format             */~
            format$12,                   /* Current Format             */~
            intlen$10,                   /* Internal Length of Field   */~
            library$6,                   /* S.E.S. Library             */~
            offset$10,                   /* Easysoft Field Offset Pos  */~
            p%(2),                       /* Search Receiver            */~
            parent$16,                   /* Parent Code                */~
            parent_type$8,               /* Parent Code Element Type   */~
            reclen$4,                    /* Record Length of File      */~
            temp$19,                     /* Miscell Work Variable      */~
            version$6                    /* Relationship Version no.   */

            dim record$(1)256            /*Components Array from Caller*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************


        REM *************************************************************~
            *     G E N E R A T E   A   C A T A L O G   F I L E         *~
            *-----------------------------------------------------------*~
            *   Actual Catalog File Generation begins here.             *~
            *************************************************************
            errorno% = 0
            gosub Write_Catalog_Info
            end


Write_Catalog_Info

*       ******** Write The File Header Record *********


                write #3, using L19500,                                   ~
                     "FILE",             /* ASCII Zeros                */~
                     parent$,            /* File Name                  */~
                     "INDEXED",          /* File Organization          */~
                     "FIXED",            /* Record Type                */~
                     reclen$,            /* Record Length              */~
                     " ", hex(0d)        /* Filler                     */


L19500:         FMT                      /* File Definition            */~
                     CH(7),              /* Record Type                */~
                     CH(64),             /* File Name                  */~
                     CH(16),             /* File Organization          */~
                     CH(16),             /* Record Type                */~
                     CH(10),             /* Record Length              */~
                     CH(223),            /* Filler                     */~
                     CH(1)               /* EOR                        */


*       *********** Now Write the Field Descriptor Records *************
        o% = 0%             /* Offset position in record area    */
        f% = 1%             /* Field Counter                     */
        for i% = 1% to max%
            get record$(i%) using L39460,                                 ~
                            /* FILE: INTDOC02.FIELDS                   */~
            component$,     /* Data Dictionary Field Name              */~
            format$,        /* BASIC Field Storage Format              */~
            internal_len ,  /* Internal field element length           */~
            external_len ,  /* External field length                   */~
            occurs,         /* Number of occurences of field element   */~
            data_type$,     /* data Type                               */~
            decimal_places$ /* number of decimal places                */

            /* For internal Testing  */
*           call "ASKUSER" (A%,component$,format$,data_type$,decimal_places$)

            if format$ = " " or data_type$ = " " then next_i
            call "SPCESMSH" (format$, 0%)
            star% = pos(format$ = "*")
            format% = pos(str(format$, star% + 1%) > " ") + star%
            if format% = 0% then next_i
            fmt$ = str(format$, format%, 2%)
            if str(format$,format%+2%,1%) <> "(" then next_i


            if fmt$ = "CH" or fmt$ = "XX"                                   then datatype$ = "STRING"
            if fmt$ = "PD"                                                  then datatype$ = "PACKED"
            if fmt$ = "BI" and internal_len = 1                             then datatype$ = "UINTEGER1"
            if fmt$ = "BI" and internal_len = 2                             then datatype$ = "UWORD"
            if fmt$ = "BI" and internal_len = 3                             then datatype$ = "UINT3"
            if fmt$ = "BI" and internal_len = 4                             then datatype$ = "LONG"
            if fmt$ = "CH" and data_type$ = "DATE"                          then datatype$ = "DATE"
            if fmt$ = "CH" and data_type$ = "DATE" and internal_len = 6     then date_format$ = "YYMMDD"
            if fmt$ = "CH" and data_type$ = "DATE" and internal_len = 8     then date_format$ = "YYYYMMDD"
            if fmt$ = "CH" and data_type$ = "YEAR"                          then datatype$ = "DATE"
            if fmt$ = "CH" and data_type$ = "YEAR" and internal_len >=4     then date_format$ = "YYYY"
            if fmt$ = "CH" and data_type$ = "YEAR" and internal_len = 2     then date_format$ = "YY"
            if fmt$ = "CH" and data_type$ = "YEARMON"                       then datatype$ = "DATE"
            if fmt$ = "CH" and data_type$ = "YEARMON" and internal_len >=6  then date_format$ = "YYYYMM"
            if fmt$ = "CH" and data_type$ = "YEARMON" and internal_len = 4  then date_format$ = "YYMM"
            if fmt$ = "PD" and data_type$ = "DATE"                          then datatype$ = "SPD-DATE"
            if fmt$ = "BI" and internal_len = 4 and data_types$ = "DATE"    then datatype$ = "BINDATE"
            if fmt$ = "BI" and internal_len = 4 and data_types$ = "DATE"    then date_format$ = "YYYYMMDD"
            if fmt$ = "BI" and internal_len = 4 and data_types$ = "YEAR"    then date_format$ = "YYYY"
            if fmt$ = "BI" and internal_len = 4 and data_types$ = "YEARMON" then date_format$ = "YYYYMM"
            if fmt$ = "BI" and internal_len = 4 and data_types$ = "TIME"    then datatype$ = "BINTIME"
            if fmt$ = "BI" and internal_len = 4 and data_types$ = "TIME"    then date_format$ = "hhmmsscc"
            if fmt$ = "BI" and internal_len = 3 and data_types$ = "TIME"    then datatype$ = "CDS-TIME3"
            if fmt$ = "BI" and internal_len = 3 and data_types$ = "TIME"    then date_format$ = "hhmmss"
            if fmt$ = "BI" and internal_len = 2 and data_types$ = "TIME"    then datatype$ = "BINTIME-2-U"
            if fmt$ = "BI" and internal_len = 2 and data_types$ = "TIME"    then date_format$ = "hhmm"
            if datatype$ = "STRING"                                         then external_len = internal_len
            if datatype$ = "PACKED"                                         then external_len = internal_len * 2 - 1
            if datatype$ = "UINTEGER1"                                      then external_len = 3
            if datatype$ = "UWORD"                                          then external_len = 5
            if datatype$ = "UINT3"                                          then external_len = 8
            if datatype$ = "LONG"                                           then external_len = 10
            if datatype$ = "DATE"                                           then external_len = len(date_format$)
            if datatype$ = "SPD-DATE"                                       then external_len = 15 /*?*/
            if datatype$ = "SPD-DATE"                                       then decimal_places$ = "0"
            if datatype$ = "BINDATE"                                        then external_len = 10
            if datatype$ = "BINTIME"                                        then external_len = 10
            if datatype$ = "CDS-TIME3"                                      then external_len = 9
            if datatype$ = "BINTIME-2-U"                                    then external_len = 5
            call "CONVERT" (internal_len,-0.0001,intlen$)
            call "CONVERT" (external_len,-0.0001,extlen$)

            dupe% = 1%               /* Duplicate Fields Counter   */
            temp$ = component$
            tran(temp$," ! % ^ & * ( ) _ + = [ ] ' ; : . > , < / ? - # $ @AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz")replacing
                tran(temp$, hex(2022))replacing /*  double quotes */
                if temp$ = " " then next_i
                call "SPCESMSH" (temp$, 1%)  /* remove extra spaces */
                call "STRING" addr("LJ", temp$, 19%)
                tran(str(temp$,,len(temp$)),"_ ")replacing
            l2% = min(17%,len(temp$)+1%)
            if f% = 1% then cat_fields$(f%) = temp$                  ~
                       else gosub check_for_duplicate_code
            for j = 1 to occurs
               convert o% to offset$, pic(##########)
               call "STRING" addr("LJ", offset$, 10%)
               o% = o% + internal_len
               if j < 2 then L21370
               temp$ = cat_fields$(f%-1%)
               gosub check_for_duplicate_code

L21370:        write #3, using L21680,                                   ~
                     "FIELD",            /* Record Type                */~
                     parent$,            /* File Name                  */~
                     cat_fields$(f%),    /* Field Name                 */~
                     datatype$,          /* Easysoft Data Type         */~
                     offset$,            /* Offset Position in Record  */~
                     intlen$,            /* Internal Length            */~
                     extlen$,            /* Precision                  */~
                     decimal_places$,    /* Decimal Places             */~
                     "0",                /* Encryption Flag            */~
                     date_format$,       /* Date/Time Format           */~
                     " ", hex(0d)        /* Default Value              */
                f% = f% + 1%
            next j
L21680:          FMT                                                     ~
                     CH(7),              /* Record Type                */~
                     CH(64),             /* File Name                  */~
                     CH(64),             /* Field Name                 */~
                     CH(64),             /* Data Type                  */~
                     CH(10),             /* Offset Position in Record  */~
                     CH(10),             /* Internal Length            */~
                     CH(10),             /* Precision / Max Num Size   */~
                     CH(10),             /* Scale / Nbr Decimal Places */~
                     CH(1),              /* Encrypted                  */~
                     CH(64),             /* Date / Time Format         */~
                     CH(32),             /* Default Value           )  */~
                     CH(1)               /* EOR                        */

next_i: next i%
        return

check_for_duplicate_code
        search str(cat_fields$(),,(f%-1%)*19%) = str(temp$,,19%) to p%() step 19%
        if p%(1%) = 0% then L13200
        REM *** Deal with Duplicate Field Name ***
        dupe% = dupe% + 1%        /* Increment for Next Test    */
        convert dupe% to str(temp$,l2%,3%),pic(000)
        goto check_for_duplicate_code
L13200: cat_fields$(f%) = temp$
return

L39460: FMT                 /* FILE: INTDOC02.FIELDS                   */~
            CH(16),         /* Data Dictionary Field Name              */~
            POS(176),       /* Position for Field DESCTEXT             */~
            CH(12),         /* BASIC Field Storage Format              */~
            POS(222),       /* Position for Field Internal Length      */~
            BI(2),          /* Internal len                            */~
            POS(241),                                                    ~
            BI(2),          /* External length                         */~
            BI(2),          /* Occurrences                             */~
            CH(8),          /* Data Type                               */~
            XX(2),          /* Storage Type                            */~
            CH(1)           /* Decimal Places                          */

