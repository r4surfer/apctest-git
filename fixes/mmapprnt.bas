REM *THISPROGRAMWASGENERATEDUSINGGENPGMPROGRAMWHICHISPROPRIETARY*~
    *                                                           *~
    *  M   M  M   M   AAA   PPPP   PPPP   RRRR   N   N  TTTTT   *~
    *  MM MM  MM MM  A   A  P   P  P   P  R   R  NN  N    T     *~
    *  M M M  M M M  AAAAA  PPPP   PPPP   RRRR   N N N    T     *~
    *  M   M  M   M  A   A  P      P      R   R  N  NN    T     *~
    *  M   M  M   M  A   A  P      P      R   R  N   N    T     *~
    *                                                           *~
    *-----------------------------------------------------------*~
    * MMAPPRNT - Print all the contents of the file 'MDATMAP'.  *~
    *            Creates a conversion report.                   *~
    *-----------------------------------------------------------*~
    *  This program contains valuable trade secrets and         *~
    *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
    *  embodying substantial creative efforts and confidential  *~
    *  information.  Unauthorized use, copying, decompiling,    *~
    *  translating, disclosure, or transfer of it is prohibited.*~
    *  Copyright (c) 1996, an unpublished work by CAELUS,       *~
    *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
    *-----------------------------------------------------------*~
    *                  M O D I F I C A T I O N S                *~
    *---WHEN---+----------------WHAT----------------------+-WHO-*~
    * 07/24/96 ! Original                                 ! DXL *~
    * 09/03/97 ! Added Reverse (1000000) to xor           ! DXL *~
    PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVED

    Dim                                                              ~
        d_file$16,                   /* File name                  */~
        d_key$30,                    /* Record Key                 */~
        d_ver$6,                     /* SES Version                */~
        d_pos%(100),                 /* Date field Starting Pos    */~
        d_fmt%(100),                 /* Date Conversion Fmt        */~
        d_name$(100)16,              /* Date Field Name            */~
        date$8,                      /* Date for screen display    */~
        errormsg$79,                 /* Error message              */~
        filler$100,                  /* Filler                     */~
        footer$(14)132,              /* Footer Image               */~
        plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
        rep_date$60,                 /* Report Date                */~
        rep_comp_name$60,            /* Report Title-Company Name  */~
        rep_title$60,                /* Report Title-Report Name   */~
        userid$3                     /* Current User Id            */

    dim axd$(64)4,                   /* Alternate key block        */~
        f2%(64),                     /* = 0 if the file is open    */~
        f1%(64),                     /* = 1 if READ was successful */~
        rslt$(64)20                  /* Text from file opening     */

rem *************************************************************~
    *                  Release Version ID Section               *~
    *************************************************************
    dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
rem *************************************************************

    mat f2% = con

REM *************************************************************~
    *                  S E L E C T   F I L E S                  *~
    *-----+----------+------------------------------------------*~
    *File#!  PRName  ! D E S C R I P T I O N                    *~
    *-----+----------+------------------------------------------*~
    * #01 ! MMDATMAP ! Data file for millie data conversion sys *~
    *************************************************************~
    *       File Selection and Open Calls                       *~
    *************************************************************

   select #01, "MMDATMAP",                                 ~
          VARC,     INDEXED,  recsize = 2000,              ~
          keypos =    1, keylen =  46                      ~

   call "SHOSTAT" ("Opening Database, One Moment Please")

   call "OPENOLIB" (#1, "SHARE", f2%(1%), rslt$(1%), axd$(1%))

   call "SETPRNT" ( " ", "MMAP", 0%, 0% )
   select printer

REM *************************************************************~
    *                I N I T I A L I Z A T I O N                *~
    *-----------------------------------------------------------*~
    * Initializes information necessary for program.            *~
    *************************************************************
       gosub initialize_variables

       call "EXTRACT" addr("ID", userid$)
       date$ = date
       call "DATEFMT" (date$)

       call "DATE" addr ("HL", rep_date$)
       call "SPCESMSH" ( rep_date$, 2% )

       rep_title$ = "Date Conversion Map Report"
       call "FMTTITLE" ( rep_title$, " ", 2% )

       call "COMPNAME" (10%, rep_comp_name$, 0%)
       call "FMTTITLE" ( rep_comp_name$, " ", 2% )

       pg_len% = 66%
       pg_num% = 0%
       line_num% = 0%
       bottom_margin% = 7%

       footer$(01%) =  "00 Not a date."
       footer$(02%) =  "01 YYMMDD to CCYYMMDD           CH(6) to PD(11,1)"
       footer$(03%) =  "02 CCYYMMDD to CCYYMMDD         CH(8) to PD(11,1)"
       footer$(04%) =  "03 YY to CCYY                   CH(2) to BI(2)"
       footer$(05%) =  "04 YYMM to CCYYMM               CH(4) to BI(4)"
       footer$(06%) =  "05 MM/DD/YY to CCYYMMDD         CH(8) to PD(11,1)"
       footer$(07%) =  "06 Reverse Date (1000000) to xor(ffffffff)    "
       footer$(08%) =  "07 YY to CCYY                   BI(4) to BI(4)"
       footer$(09%) =  "08 YYMMDD to CCYYMMDD           BI(4) to BI(4)"
       footer$(10%) =  "09 Reverse Date (xor)           CH(6) to PD(11,1)"
       footer$(11%) =  "10 Reverse Date (999999)        CH(6) to PD(11,1)"
       footer$(12%) =  "11 Reverse Date (1000000)       CH(6) to PD(11,1)"
       footer$(13%) =  "12 Variable Fields              CH(6) to PD(11,1)"
       footer$(14%) =  "13  YYMM  to CCYYMM             CH(6) to CH(6)"

       footer_len% = 1% + (dim( footer$(),1%) / 2%)
       footer_len% = footer_len% + bottom_margin%

REM *************************************************************~
    *                  M A I N   L O O P                        *~
    *-----------------------------------------------------------*~
    * Read Rec, Print Rec ...                                   *~
    *************************************************************
    gosub print_header
    call "SHOSTAT" ( "Printing Date Conversion Map Report" )

main_loop
    plowkey$ = d_file$
    str(plowkey$, 17%, 30%) = d_key$
    call "READ102" ( #1, plowkey$, f1%(1%) )
    if f1%(1%) = 0% then exit_program
    gosub dataload
    gosub print_loop
    goto main_loop

REM *************************************************************~
    *                 P R I N T   L O O P                       *~
    *-----------------------------------------------------------*~
    * Prints each Date Fmt on file.                             *~
    *************************************************************

print_loop
    for field_count% = 1% to 100%
         if d_pos%(field_count%)  = 0% or   ~
            d_fmt%(field_count%)  = 0% or   ~
            d_name$(field_count%) = " " then loop_next

         convert d_pos%(field_count%) to d_pos$, pic(####)
         convert d_fmt%(field_count%) to d_fmt$, pic(00)

         on print_descr% gosub        ~
                          rec_desc,   ~
                          blank_desc, ~
                          rec_cont
         print_descr% = 2%

         line_num% = line_num% + 1%

         gosub header_check
loop_next
    next field_count%

    return

REM *************************************************************~
    *                 H E A D E R   C H E C K                   *~
    *-----------------------------------------------------------*~
    * Checks if we need a new page.                             *~
    *************************************************************

header_check
    if (line_num% + footer_len%) < pg_len% then return
       gosub print_footer
       gosub print_header
       print_descr% = 3%
    return

rem *************************************************************~
    *                  P R I N T   H E A D E R                  *~
    *-----------------------------------------------------------*~
    * Prints header, increment page number ...                  *~
    *************************************************************
print_header
    pg_num% = pg_num% + 1%
    convert pg_num% to pg_num$, PIC(####)
    call "SPCSMASH"(pg_num$)
    print using header1_fmt, rep_date$, rep_comp_name$, "MMAPPRNT"
    print using header2_fmt, rep_title$, "Page:", pg_num$
    print
    print "                                                    SES    Rec. ~
          ~Date                     "
    print "File Name        Record Key                     Brk Ver.   Len. ~
          ~Field Name       Pos. Fmt"
    print "---------------- ------------------------------ --- ------ ---- ~
          ~---------------- ---- ---"
    line_num%, header_len% = 6%
    return
rem *************************************************************~
    *                  P R I N T   F O O T E R                  *~
    *-----------------------------------------------------------*~
    * Prints footer, advances the form ...                      *~
    *************************************************************
print_footer
    print skip (1% + (pg_len% - footer_len%) - line_num%)
    line_num% = line_num% + (1% + (pg_len% - footer_len%) - line_num%)

    print "Legend: Fmt"
    line_num% = line_num% + 1%

    f% = footer_len% - bottom_margin% - 1%

    for count% = 1% to f%
         print using footer_fmt, footer$(count%), footer$(count% + f%)
         line_num% = line_num% + 1%
    next count%

    print page

    return
rem *************************************************************~
    * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
    *-----------------------------------------------------------*~
    * Initializes all defined screen variables to blank         *~
    *************************************************************
initialize_variables
       INIT(" ") errormsg$, ~
                 date$, filler$, d_file$, d_key$, d_ver$, d_name$()
       d_break%, d_len%, pg_num%, line_num%, footer_len%, pg_len% = 0%
       print_descr%, field_count%, rec_count% = 0%
       return

REM *************************************************************~
    *           L O A D   D A T A   F R O M   F I L E           *~
    *-----------------------------------------------------------*~
    * Loads data from File Record Area into Program Variables.  *~
    *************************************************************
dataload
     if line_num% = header_len% or print_descr% = 1% then read_rec
     print
     line_num% = line_num% + 1
     gosub header_check
read_rec
     print_descr% = 1%
     get #1, using MMDATMAP_fmt, d_file$, d_key$, d_break%, d_ver$, d_len%, ~
             d_pos%(), d_fmt%(), d_name$(), filler$

     convert d_break% to d_break$, PIC(###)
     convert d_len% to d_len$, PIC(####)
     rec_count% = rec_count% + 1%
     return

REM *************************************************************~
    *        F O R M A T    S T A T E M E N T S                 *~
    *-----------------------------------------------------------*~
    * FORMAT Statements for Data Files.                         *~
    *************************************************************

MMDATMAP_fmt: FMT         /* FILE: MMDATMAP                          */~
          CH(16),         /* Name of the file to convert.            */~
          CH(30),         /* Holds Key value for current record or He*/~
          BI(2),          /* Key length for current record           */~
          CH(6),          /* SES Version                             */~
          BI(2),          /* Length of the records to convert        */~
          100*BI(2),      /* Start position for each date field      */~
          100*BI(1),      /* Code for the conversion for each date fi*/~
          100*CH(16),     /* Name of each date field to be converted */~
          CH(44)          /* Unused Space                            */~

rem *************************************************************~
    *         A C T U A L   P R I N T   R O U T I N E S         *~
    *-----------------------------------------------------------*~
    * This is where the real print work happens.                *~
    *************************************************************

rec_desc
       print using rec_desc_fmt,   d_file$,                 ~
                                   d_key$,                  ~
                                   d_break$,                ~
                                   d_ver$,                  ~
                                   d_len$,                  ~
                                   d_name$(field_count%),   ~
                                   d_pos$,                  ~
                                   d_fmt$
       return

blank_desc
       print using blank_desc_fmt, d_name$(field_count%),   ~
                                   d_pos$,                  ~
                                   d_fmt$
       return

rec_cont
       print using rec_cont_fmt,   d_file$,                 ~
                                   "Cont'd",                ~
                                   d_name$(field_count%),   ~
                                   d_pos$,                  ~
                                   d_fmt$
       return
REM *************************************************************~
    *               F O R M A T   S T A T E M E N T S           *~
    *-----------------------------------------------------------*~
    * Formats for each type of print line.                      *~
    *************************************************************

rec_desc_fmt:   FMT CH(16), XX(01), CH(30), XX(01), CH(03), XX(01), CH(6), ~
                    XX(01), CH(04), XX(01), CH(16), XX(01), CH(04), ~
                    XX(01), CH(02)
blank_desc_fmt: FMT XX(64), CH(16), XX(01), CH(04), XX(01), CH(02)
rec_cont_fmt:   FMT CH(16), XX(01), CH(46), XX(01), CH(16), XX(01), CH(04), ~
                    XX(01), CH(02)
header1_fmt:    FMT CH(36), CH(88), CH(8)
header2_fmt:    FMT XX(36), CH(86), CH(6), CH(4)
footer_fmt:     FMT CH(65), CH(50)
rec_count_fmt:  FMT CH(71), PIC(####), CH(13)
rem THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAEUS,INC~
    *                          E X I T                          *~
    *-----------------------------------------------------------*~
    * Terminates execution (files closed automatically).        *~
    *-----------------------------------------------------------*~
    *  This program contains valuable trade secrets and         *~
    *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
    *  embodying substantial creative efforts and confidential  *~
    *  information.  Unauthorized use, copying, decompiling,    *~
    *  translating, disclosure, or transfer of it is prohibited.*~
    *  Copyright (c) 1996, an unpublished work by CAELUS,       *~
    *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
    CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

exit_program

       for x1% = 1% to 3%
          print
          line_num% = line_num% + 1%
          gosub header_check
       next x1%

       print using rec_count_fmt, "End of Report", rec_count%, " Records Read"
       gosub header_check

       gosub print_footer
       close printer
       call "SETPRNT" ( " ", "MMAP", 0%, 1% )

       end
