REM *THISPROGRAMWASGENERATEDUSINGGENPGMPROGRAMWHICHISPROPRIETARY*~
    *                                                           *~
    *  M   M  M   M   AAA   PPPP   IIIII  N   N  PPPP           *~
    *  MM MM  MM MM  A   A  P   P    I    NN  N  P   P          *~
    *  M M M  M M M  AAAAA  PPPP     I    N N N  PPPP           *~
    *  M   M  M   M  A   A  P        I    N  NN  P              *~
    *  M   M  M   M  A   A  P      IIIII  N   N  P              *~
    *                                                           *~
    *-----------------------------------------------------------*~
    * MMAPINP  - Lets you Create, Review, Update, or Delete date*~
    *            format information in "MDATMAP". This file will*~
    *            be used by "MMDATFIX" to convert date fields in*~
    *            'old style' data files to the specified format.*~
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
    * 07/01/96 ! Original                                 ! DXL *~
    * 09/25/96 ! Modified screen titles                   ! DER *~
    * 09/26/96 ! Modified PLOWCODE to use description     ! DER *~
    *          !   area and vars only                     !     *~
    * 10/09/96 ! Screen change Break% to Record key length! DER *~
    * 09/03/97 ! Added Reverse (1000000) to xor           ! DXL *~
    PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVED

    Dim                                                              ~
        cursor%(2),                  /* Cursor location for edit   */~
        data_rec_break$3,            /* Break%                     */~
        data_rec_key$30,             /* Record Key                 */~
        data_rec_len$4,              /* SES Record Length          */~
        data_rec_name$16,            /* File Name:                 */~
        data_rec_ver$6,              /* SES Version                */~
        data_rec_descr$50,           /* SES File Description       */~
        date$8,                      /* Date for screen display    */~
        date_field_fmt$(110)50,      /* Date format                */~
        date_field_fmt%(100),        /* Date format                */~
        date_field_name$(110)16,     /* Field Name                 */~
        date_field_nam2$(100)16,     /* Field Name                 */~
        date_field_num$(110)16,      /* Field Number               */~
        date_field_start$(110)4,     /* Start POS                  */~
        date_field_start%(100),      /* Start POS                  */~
        date_fmt_len%(20),           /* Date Field Lengths (BYTES) */~
        descr$80,                    /* Description for PLOWCODE   */~
        descr_map(12),               /* Desc. Field Map. (PLOWCODE)*/~
        edtmessage$79,               /* Edit screen message        */~
        edtmessage2$79,              /* Edit screen message        */~
        errormsg$79,                 /* Error message              */~
        i$(24)80,                    /* Screen Image               */~
        inpmessage$79,               /* Informational Message      */~
        incl_excl(4),                /* for PLOWCODE               */~
        incl_excl$(4)20,             /* for PLOWCODE               */~
        header$(5)80,                /* for PLOWCODE               */~
        lfac$(110)1,                 /* Field Attribute Characters */~
        line2$79,                    /* Screen Line #2             */~
        pf$(3)79,                    /* PF Screen Literals         */~
        pfkeyS$32,                   /* PF Key Hex Values          */~
        plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
        scr2_col$(3)50,              /* Column titles for screen 2 */~
        userid$3,                    /* Current User Id            */~
        work_str$50                  /* Temp. String               */

    dim                                                              ~
        axd$(64)4,                   /* AXD Pointer from FILEOPEN  */~
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
    * #02 ! WORK     ! Temporary Work File                      *~
    * #03 ! INTDOC01 !                                          *~
    *************************************************************~
    *       File Selection and Open Calls                       *~
    *************************************************************

   call "SHOSTAT" ("Opening Database, One Moment Please")

   select #01, "MMDATMAP",                                 ~
          VARC,     INDEXED,  recsize = 2000,              ~
          keypos = 1,    keylen = 46                       ~

   select #02, "WORK",                                     ~
          VARC,     INDEXED,  recsize = 60,                ~
          keypos = 1,    keylen = 2                        ~

   select #03, "INTDOC01",                                 ~
          VARC,     INDEXED,  recsize = 1000,              ~
          keypos = 25,   keylen = 16                       ~

   call "WORKOPN2" (#02, "OUTPT", 20%, f2%(2%))

   restore line = work_file_data
work_file_loop:
        read work_str$
        if work_str$ = " " then work_loop_end
        write #02 using WORK_fmt, work_str$, " "
        goto work_file_loop
work_file_data
   data "00 Not a date.",~
        "01 YYMMDD to CCYYMMDD           CH(6) to PD(11,1)",~
        "02 CCYYMMDD to CCYYMMDD         CH(8) to PD(11,1)",~
        "03 YY to CCYY                   CH(2) to BI(2)",~
        "04 YYMM to CCYYMM               CH(4) to BI(4)",~
        "05 YY/MM/DD to CCYYMMDD         CH(8) to PD(11,1)",~
        "06 Reverse Date (1000000) to xor(ffffffff)    ",~
        "07 YY to CCCYY                   BI(4) to BI(4)",~
        "08 YYMMDD to CCYYMMDD           BI(4) to BI(4)",~
        "09 Reverse Date (xor)           CH(6) to PD(11,1)",~
        "10 Reverse Date (999999)        CH(6) to PD(11,1)",~
        "11 Reverse Date (1000000)       CH(6) to PD(11,1)",~
        "12 Variable Field               CH(6) to PD(11,1)",~
        "13  YYMM  TO CCYYMM             CH(6) to CH(6)",~
        "                                       "
work_loop_end

   call "OPENOLIB" (#01, "SHARE", f2%(1%), rslt$(1%), axd$(1%))
   call "WORKOPN2" (#02, "INPUT", 0%, f2%(2%) )
   call "OPENOLIB" (#03, "SHARE", f2%(3%), rslt$(3%), axd$(3%))

REM *************************************************************~
    *                I N I T I A L I Z A T I O N                *~
    *-----------------------------------------------------------*~
    * Initializes information necessary for program.            *~
    *************************************************************
       date_fmt_len%( 1%) = 6%:   date_fmt_len%( 2%) = 8%
       date_fmt_len%( 3%) = 4%:   date_fmt_len%( 4%) = 4%
       date_fmt_len%( 5%) = 8%:   date_fmt_len%( 6%) = 6%
       date_fmt_len%( 7%) = 4%:   date_fmt_len%( 8%) = 4%
       date_fmt_len%( 9%) = 6%:   date_fmt_len%(10%) = 6%
       date_fmt_len%(11%) = 6%:   date_fmt_len%(12%) = 200%
       date_fmt_len%(13%) = 6%:   date_fmt_len%(14%) = 9999%
       date_fmt_len%(15%) = 9999%:   date_fmt_len%(16%) = 9999%
       date_fmt_len%(17%) = 9999%:   date_fmt_len%(18%) = 9999%
       date_fmt_len%(19%) = 9999%:   date_fmt_len%(20%) = 9999%

       scr2_col$(1%) = "Start POS"
       scr2_col$(2%) = "Date Format"
       scr2_col$(3%) = "Field Name"

       call "EXTRACT" addr("ID", userid$)
       date$ = date
       call "DATEFMT" (date$)
       edtmessage$  = "To Modify Displayed Values, Position Cursor" & ~
                      " to Desired Value & Press (RETURN)."

       edtmessage2$ = "To Modify Displayed Values Press (RETURN)."

       str(line2$,62%) = "MMAPINP : " & STR(CMS2V$,,8)

       for o% = 1% to 100%
           convert o% to date_field_num$(o%), pic (###)
       next o%

       o% = 1                              /* First date field format */
REM *************************************************************~
    *       I N P U T   M O D E   M A I N   P R O G R A M       *~
    *-----------------------------------------------------------*~
    * Handles normal input for data entry screens.              *~
    *************************************************************

inputmode
       gosub initialize_variables
       from_input% = 1%

       for fieldnr% = 1% TO  5%
inp1a:     gosub'051(fieldnr%)              /* Default - Enables */
           if enabled% = 0% then inp1e
inp1b:     gosub'101(fieldnr%, 1%)          /* Display - Accept  */
               if keyhit%  =  1% then gosub startover
               if keyhit% <>  4% then       inp1d
inp1c:             fieldnr% = max(1%, fieldnr% - 1%)
                   gosub'051(fieldnr%)
                   if enabled% = 1% then inp1b
                   if fieldnr% = 1% then inp1a
                   goto inp1c
inp1d:
               if keyhit% = 12% then gosub delete_curr_rec
               if keyhit% <> 14% then inp1da
                  gosub inquiry
                  if f1%(1%) = 1% then editpg1
inp1da
               if keyhit% = 16% and fieldnr% = 1% then exit_program
               if keyhit% <> 0%                   then inp1b
inp1e:     gosub'151(fieldnr%)      /* Edit Field for Valid Entry */
               if errormsg$ <> " " then inp1b
       next fieldnr%

       o% = 1                              /* First date field format */
       if f1%(1%) = 0% then inp2_begin     /* New record information. */
       gosub dataload                      /* Selected record exists. */
       goto editpg2

inp2_begin
       for fieldnr% = 1% TO 30%
           c% = mod(fieldnr%, 3%)
           if c% = 0% then c% = 3%
           if c% <> 1% then inp2a
           if o% + 9% >= 100% then inp2a
               if (o% + 9%) > o% + (fieldnr% / 3%) then inp2a
                     o% = o% + 1%
                     fieldnr% = fieldnr% - 3%
inp2a:     gosub'052(c%)                    /* Default - Enables */
           if enabled% = 0% then inp2e
inp2b:     gosub'102(fieldnr%, 1%)          /* Display - Accept  */
               if keyhit%  =  1% then gosub startover
               if keyhit%  =  2% or ~
                  keyhit%  =  3% or ~
                  keyhit%  =  6% or ~
                  keyhit%  =  7% then gosub offset_calc
               if keyhit% <>  4% then       inp2d
inp2c:             fieldnr% = max(1%, fieldnr% - 1%)
                   c% = mod(fieldnr%, 3%)
                   if c% = 0% then c% = 3%
                   gosub'052(c%)
                   if enabled% = 1% then inp2b
                   if fieldnr% = 1% then inp2a
                   goto inp2c
inp2d:         if mod(fieldnr%, 3%) = 1% and ~
                  ( keyhit% = 16% or ~
                  date_field_start$(1% + fieldnr% / 3%) = " " ) then ~
                       editpg2
               if keyhit% <> 0%                   then inp2b
inp2e:     gosub'152(fieldnr%)      /* Edit Field for Valid Entry */
               if errormsg$ <> " " then inp2b
       next fieldnr%

REM *************************************************************~
    *        E D I T   M O D E   M A I N   P R O G R A M        *~
    *-----------------------------------------------------------*~
    * Handles operation of EDIT MODE for data entry screens.    *~
    *************************************************************

editpg1
       errormsg$ = " "
       lastfieldnr%, from_input% = 0%
       gosub'101(0%, 2%)           /* Display Screen - No Entry */
           if keyhit%  =  1% then gosub startover
           if keyhit%  =  2% then       editpg2
           if keyhit%  = 12% then gosub delete_curr_rec
           if keyhit%  = 16% then       datasave
           if keyhit% <>  0% then       editpg1
edt1a:
       fieldnr% = cursor%(1%) - 5%
       if fieldnr% < 1% or fieldnr% >  5% then editpg1
       if fieldnr% = lastfieldnr%         then editpg1
       gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
           if enabled% =  0%   then editpg1
edt1b: gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
           if kehit%   =  1%   then gosub startover
           if keyhit% <>  0%   then       edt1b
       gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
           if errormsg$ <> " " then edt1b
           lastfieldnr% = fieldnr%
           goto edt1a

editpg2
       lastfieldnr%, from_input% = 0%
       gosub'102(0%, 2%)           /* Display Screen - No Entry */
           if keyhit%  =  1% then gosub startover
           if keyhit%  =  9% then       editpg1
           if keyhit%  = 16% then       datasave
           if keyhit%  <  2% or ~
              keyhit%  >  7% then last_key_chk
                 gosub offset_calc
                 goto editpg2
last_key_chk
           if keyhit% <>  0% then       editpg2
           if keyhit% = 0% then         edt2a

edt2a: fieldnr% = o%               /* First line  number on screen*/
       if cursor%(1%) < 6% then    /* Cursor must be in edit area */ ~
          editpg2
edt2b: gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
           if kehit%   =  1%   then gosub startover
           if keyhit% <>  0%   then       edt2b
       gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
           if errormsg$ <> " " then edt2b
           goto editpg2

REM *************************************************************~
REM *            C A L C    S C R E E N   O F F S E T           *~
REM *-----------------------------------------------------------*~
REM * Offset for the detail lines. First, Last, Up, Down.       *~
REM *************************************************************~

offset_calc:
        on keyhit% gosub , o_first, o_last, o_prev, o_next, o_down, o_up

        if o% > 100% then o% = 100%

        if o% < 1% then o% = 1%
        return

o_first:
        o% = 1%
        return

o_last:
        o% = 100%
        return

o_next:
        o% = o% + 8%
        return

o_prev:
        o% = o% - 8%
        return

o_up:
        o% = o% - 1%
        return

o_down:
        o% = o% + 1%
        return

REM *************************************************************~
    *             S A V E   D A T A   O N   F I L E             *~
    *-----------------------------------------------------------*~
    * Saves data on file after INPUT/EDITING.                   *~
    *************************************************************

datasave
       gosub dataput
       goto inputmode

REM *************************************************************~
    *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
    *-----------------------------------------------------------*~
    * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
    *************************************************************

deffn'051(fieldnr%)
       enabled% = 1%
       on fieldnr% gosub df101,         /* File Name              */~
                         df102,         /* Record Key             */~
                         df103,         /* Break%                 */~
                         df104,         /* SES Version            */~
                         df105          /* SES Record Length      */
       return
df101: rem Def/Enable File Name                   data_rec_name$
       if from_input% <> 1% then enabled% = 0%
       return

df102: rem Def/Enable Record Key                  data_rec_key$
       if from_input% <> 1% then enabled% = 0%
       return

df103: rem Def/Enable Break%                      data_rec_break$
       return

df104: rem Def/Enable SES Version                 data_rec_ver$
       return

df105: rem Def/Enable SES Record Length           data_rec_len$
       return

REM *************************************************************~
    *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
    *-----------------------------------------------------------*~
    * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
    *************************************************************

deffn'052(fieldnr0%)
       c% = mod( fieldnr0%, 3%)
       if c% = 0% then c% = 3%
       enabled% = 1%
       on c%        gosub df201,         /* Start POS              */~
                          df202,         /* Date format            */~
                          df203          /* Field Name             */
       return
df201: rem Def/Enable Start POS                   date_field_start$(1)
       return

df202: rem Def/Enable Date format                 date_field_fmt$(1)
       return

df203: rem Def/Enable Field Name                  date_field_name$(1)
       return

rem *************************************************************~
    *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
    *-----------------------------------------------------------*~
    * Initializes Variable Field Input Messages.                *~
    *************************************************************

deffn'050(scrnr%, fieldnr0%)
       if fieldnr0% <> 0% then im000
           inpmessage$ = edtmessage$
       if scrnr% = 2% then ~
           inpmessage$ = edtmessage2$
           return

im000: rem Define the Input Message for the Screen/Field Indicated
       if scrnr% = 1% then restore line = scrn1_msg, fieldnr0%
       if scrnr% = 2% then restore line = scrn2_msg, 1%
       read inpmessage$      /* Read Input Message */
       return

scrn1_msg  :  data                                        ~
       "Enter File Name   or '?' for list                      ",~
       "Enter Record Key  or '?' for list                      ",~
       "Enter Record Key Length                                ",~
       "Enter SES Version                                      ",~
       "Enter SES Record Length                                "

scrn2_msg  :  data                                        ~
       "Start POS#, Date FMT#, Field Name                    "

rem *************************************************************~
    * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
    *-----------------------------------------------------------*~
    * Initializes all defined screen variables to blank         *~
    *************************************************************
initialize_variables
       INIT(" ") errormsg$, inpmessage$,                            ~
                 data_rec_break$, data_rec_key$, data_rec_len$,     ~
                 data_rec_name$, data_rec_ver$, data_rec_descr$,    ~
                 date_field_fmt$(), date_field_name$(),             ~
                 date_field_name$(), date_field_start$(),           ~
                 filler$, header$(), incl_excl$()
       return

REM *************************************************************~
    * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
    *-----------------------------------------------------------*~
    * Gives User the ability to START OVER when s/he wants to   *~
    * or will return User back to where they were.  Must push   *~
    * two buttons to start over for safety.                     *~
    *************************************************************

startover
       u3% = 2%
       call "STARTOVR" (U3%)
       if u3% = 1% then return
           return clear all
           goto inputmode

REM *************************************************************~
    *           L O A D   D A T A   F R O M   F I L E           *~
    *-----------------------------------------------------------*~
    * Loads data from File Record Area into Program Variables.  *~
    *************************************************************
dataload
       errormsg$ = " "
       get #01 using MMDATMAP_fmt, ~
                              data_rec_name$, ~
                              data_rec_key$, ~
                              data_rec_break%, ~
                              data_rec_ver$, ~
                              data_rec_len%, ~
                              date_field_start%(), ~
                              date_field_fmt%(), ~
                              date_field_nam2$(), ~
                              filler$

       convert data_rec_break% to data_rec_break$, pic(###)
       convert data_rec_len% to data_rec_len$, pic(####)

       for x1% = 1% to 100%
          if date_field_start%(x1%) = 0% and ~
             date_field_fmt%(x1%) = 0% and ~
             date_field_nam2$(x1%) = " " then dataload_loop_out
          date_field_name$(x1%) = date_field_nam2$(x1%)
          convert date_field_start%(x1%) to date_field_start$(x1%), pic(####)
          convert date_field_fmt%(x1%) to date_field_fmt$(x1%), pic(00)
          call "GETCODE" ( #02, ~
                     str(date_field_fmt$(x1%), 1%, 2%),~
                     str(date_field_fmt$(x1%), 3%, 48%),~
                     0%,~
                     0.48,~
                     f1%(2%) )
dataload_loop_out
       next x1%

       plowkey$ = data_rec_name$
       call "READ100" (#03, plowkey$, f1%(3%))
       if f1%(3%) = 0% then dataload_out
       get #03 using INTDOC01_fmt, data_rec_descr$

dataload_out
       return

REM *************************************************************~
    *          S T U F F   D A T A   I N T O   F I L E          *~
    *-----------------------------------------------------------*~
    * Stuffs data from Program Variables into File Record Area. *~
    *************************************************************
dataput
       call "LINSMASH" (date_field_start$())
       call "LINSMASH" (date_field_fmt$())
       call "LINSMASH" (date_field_name$())

       convert data_rec_break$ to data_rec_break%
       convert data_rec_len$ to data_rec_len%

       for x1% = 1% to 100%
          date_field_start%(x1%), ~
          date_field_fmt%(x1%) = 0%
          if date_field_start$(x1%) <> " " then ~
             convert date_field_start$(x1%) to date_field_start%(x1%)
          if date_field_fmt$(x1%) <> " " then ~
             convert str( date_field_fmt$(x1%), 1%, 2%) to date_field_fmt%(x1%)
          date_field_nam2$(x1%) = date_field_name$(x1%)
       next x1%

       plowkey$ = data_rec_name$
       str( plowkey$, 17%, 30% ) = data_rec_key$

       call "READ101" (#01, plowkey$, f1%(1%))
       put #01, using MMDATMAP_fmt, data_rec_name$,      ~
                                    data_rec_key$,       ~
                                    data_rec_break%,     ~
                                    data_rec_ver$,       ~
                                    data_rec_len%,       ~
                                    date_field_start%(), ~
                                    date_field_fmt%(),   ~
                                    date_field_nam2$(),  ~
                                    " "
       if f1%(1%) = 1% then goto re_put
       write #1
       goto dataput_out
re_put
       rewrite #1

dataput_out
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
          CH(44)          /* Unused Space                            */

WORK_fmt: FMT             /* FILE: WORK                              */~
          CH(50),         /* Descrip. of conversion. - 1st 2 chrs NUM*/~
          CH(10)          /* Unused Space                            */

INTDOC01_fmt: FMT         /* FILE: INTDOC02                          */~
          POS(41),        /* Skip Riff-Raff                          */~
          CH(50)          /* File Description                        */

rem *************************************************************~
    *               S C R E E N   P A G E   1                   *~
    *-----------------------------------------------------------*~
    * Document Input and Edit Screen.                           *~
    *************************************************************

deffn'101(fieldnr%, edit%)
       gosub'050(1%, fieldnr%)
       gosub set_pf1
       if fieldnr% > 0% then init(hex(8C)) lfac$()                ~
                        else init(hex(86)) lfac$()
       on fieldnr% gosub upr1 ,         /* File Name         */   ~
                         upr1 ,         /* Record Key        */   ~
                         num1 ,         /* Break%            */   ~
                         upr1 ,         /* SES Version       */   ~
                         num1           /* SES Record Length */
       goto scr1a

          lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
upr1:     lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
num1:     lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

scr1a: accept                                                    ~
       at (01,02),                                               ~
          "Database Conversion - File Identification",           ~
       at (01,66), "Today:",                                     ~
       at (01,73), fac(hex(8C)), date$                  , CH(08),~
       at (02,02), fac(hex(AC)), line2$                 , CH(79),~
       at (04,02), fac(hex(94)), errormsg$              , CH(79),~
                                                                 ~
       at (06,02), "File Name:",                                 ~
       at (06,30), fac(lfac$( 1)), data_rec_name$       , CH(16),~
                                                                 ~
       at (06,48), fac(hex(8C)), data_rec_descr$        , CH(30),~
                                                                 ~
       at (07,02), "Record Key",                                 ~
       at (07,30), fac(lfac$( 2)), data_rec_key$        , CH(30),~
                                                                 ~
       at (08,02), "Record Key Length",                          ~
       at (08,30), fac(lfac$( 3)), data_rec_break$      , CH(03),~
                                                                 ~
       at (09,02), "SES Version",                                ~
       at (09,30), fac(lfac$( 4)), data_rec_ver$        , CH(06),~
                                                                 ~
       at (10,02), "SES Record Length",                          ~
       at (10,30), fac(lfac$( 5)), data_rec_len$        , CH(04),~
                                                                 ~
       at (21,02), fac(hex(A4)),   inpmessage$          , CH(79),~
       at (22,02), fac(hex(8C)),   pf$(1)               , CH(79),~
       at (23,02), fac(hex(8C)),   pf$(2)               , CH(79),~
       at (24,02), fac(hex(8C)),   pf$(3)               , CH(79),~
                                                                 ~
              keys(pfkeys$), key(keyhit%)

       if keyhit% <> 13% then scr1b
           call "MANUAL" ("MMAPINP ") : goto scr1a

scr1b: if keyhit% <> 15% then scr1c
           call "PRNTSCRN" : goto scr1a

scr1c: close WS
       call "SCREEN" addr ("C", U3%, "I", i$(), cursor%())
       return

set_pf1
       if edit% = 2% then scr1f     /*  Input Mode             */
           pf$(1) = "(1)Start Over                                                  ~
(13)Instructions"
           pf$(2) = "                 (4)Previous Field                             ~
(15)Print Screen"
           pf$(3) = "                                    (14)Inquiry                ~
(16)Exit Program"
           pfkeys$ = hex(01FFFF04FFFFFFFFFFFFFFFF0D0E0F1000)
           if fieldnr% = 1% then scr1d
               str(pf$(3%),64%)    = " "  :  str(pfkeys$,16,1) = hex(FF)
scr1d:     if fieldnr% > 1% then scr1e
               str(pf$(2%),18%,19%) = " "  :  str(pfkeys$, 4,1) = hex(FF)
scr1e:     return

scr1f: if fieldnr% > 0% then scr1g  /*  Edit Mode - Select Fld */
           pf$(1) = "(1)Start Over                                                  ~
(13)Instructions"
           pf$(2) = "(2)Date Fields                      (12)Delete                 ~
(15)Print Screen"
           pf$(3) = "                                                               ~
(16)Save Data   "
           pfkeys$ = HEX(0102FFFFFFFFFFFFFFFFFF0C0DFF0F1000)
       return
scr1g                              /*  Edit Mode - Enabled    */
       pf$(1) = "(1)Start Over                                                  ~
(13)Instructions"
       pf$(2) = "                                                               ~
(15)Print Screen"
       pf$(3) = "                                                                        ~
       "
       pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0DFF0FFF00)
       return

rem *************************************************************~
    *               S C R E E N   P A G E   2                   *~
    *-----------------------------------------------------------*~
    * Document Input and Edit Screen.                           *~
    *************************************************************

deffn'102(fieldnr%, edit%)
       gosub'050(2%, fieldnr%)
             /***************************************************/
             /* fieldnr% will be the the actual field number on */
             /* the screen in input mode, but will be the line  */
             /* offset of the first edit line in edit mode.     */
             /***************************************************/
       gosub set_pf2
       if fieldnr% > 0% then init(hex(8C)) lfac$()                ~
                        else init(hex(86)) lfac$()
       if fieldnr% = 0 then scr2a
       if edit% <> 1% ~
           then set_edit                /* input mode        */
       c% = mod( fieldnr%, 3%)
       if c% = 0% then c% = 3%
       on c% gosub       num2i ,         /* Start POS         */   ~
                         num2i ,         /* Date format       */   ~
                         upl2i           /* Field Name        */
       goto scr2a

set_edit                                /* edit mode         */
       for fieldnr% = o% to o% + 9%
       if fieldnr% > 100% then end_set_edit
           for c% = 1% to 3%
           on c% gosub       num2e ,         /* Start POS         */   ~
                             num2e ,         /* Date format       */   ~
                             upl2e           /* Field Name        */
           next c%
end_set_edit
       next fieldnr%
       goto scr2a

upl2i:     lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
num2i:     lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */
upl2e:     lfac$(c% + (fieldnr% - o%) * 3%) = hex(80):  return  /* Up / Low   */
           lfac$(c% + (fieldnr% - o%) * 3%) = hex(81):  return  /* Upper Only */
num2e:     lfac$(c% + (fieldnr% - o%) * 3%) = hex(82):  return  /* Numeric    */

scr2a: accept                                                    ~
       at (01,02),                                               ~
          "Database Conversion - Date Field Identification",     ~
       at (01,66), "Today:",                                     ~
       at (01,73), fac(hex(8C)), date$                  , CH(08),~
       at (02,02), fac(hex(AC)), line2$                 , CH(79),~
       at (04,02), fac(hex(94)), errormsg$              , CH(79),~
                                                                 ~
       at (05,06), fac(hex(AC)), scr2_col$(1),            CH( 5),~
       at (05,12), fac(hex(AC)), scr2_col$(2),            CH(50),~
       at (05,63), fac(hex(AC)), scr2_col$(3),            CH(16),~
                                                                 ~
                                                                 ~
       at (06+0%,02), fac(hex(8C)),   date_field_num$(o%+0%)   , CH(03),~
       at (06+1%,02), fac(hex(8C)),   date_field_num$(o%+1%)   , CH(03),~
       at (06+2%,02), fac(hex(8C)),   date_field_num$(o%+2%)   , CH(03),~
       at (06+3%,02), fac(hex(8C)),   date_field_num$(o%+3%)   , CH(03),~
       at (06+4%,02), fac(hex(8C)),   date_field_num$(o%+4%)   , CH(03),~
       at (06+5%,02), fac(hex(8C)),   date_field_num$(o%+5%)   , CH(03),~
       at (06+6%,02), fac(hex(8C)),   date_field_num$(o%+6%)   , CH(03),~
       at (06+7%,02), fac(hex(8C)),   date_field_num$(o%+7%)   , CH(03),~
       at (06+8%,02), fac(hex(8C)),   date_field_num$(o%+8%)   , CH(03),~
       at (06+9%,02), fac(hex(8C)),   date_field_num$(o%+9%)   , CH(03),~
                                                                        ~
       at (06+0%,06), fac(lfac$( 1%)), date_field_start$(o%+0%), CH(04),~
       at (06+1%,06), fac(lfac$( 4%)), date_field_start$(o%+1%), CH(04),~
       at (06+2%,06), fac(lfac$( 7%)), date_field_start$(o%+2%), CH(04),~
       at (06+3%,06), fac(lfac$(10%)), date_field_start$(o%+3%), CH(04),~
       at (06+4%,06), fac(lfac$(13%)), date_field_start$(o%+4%), CH(04),~
       at (06+5%,06), fac(lfac$(16%)), date_field_start$(o%+5%), CH(04),~
       at (06+6%,06), fac(lfac$(19%)), date_field_start$(o%+6%), CH(04),~
       at (06+7%,06), fac(lfac$(22%)), date_field_start$(o%+7%), CH(04),~
       at (06+8%,06), fac(lfac$(25%)), date_field_start$(o%+8%), CH(04),~
       at (06+9%,06), fac(lfac$(28%)), date_field_start$(o%+9%), CH(04),~
                                                                        ~
       at (06+0%,12), fac(lfac$(02%)), date_field_fmt$(o%+0%),   CH(02),~
       at (06+1%,12), fac(lfac$(05%)), date_field_fmt$(o%+1%),   CH(02),~
       at (06+2%,12), fac(lfac$(08%)), date_field_fmt$(o%+2%),   CH(02),~
       at (06+3%,12), fac(lfac$(11%)), date_field_fmt$(o%+3%),   CH(02),~
       at (06+4%,12), fac(lfac$(14%)), date_field_fmt$(o%+4%),   CH(02),~
       at (06+5%,12), fac(lfac$(17%)), date_field_fmt$(o%+5%),   CH(02),~
       at (06+6%,12), fac(lfac$(20%)), date_field_fmt$(o%+6%),   CH(02),~
       at (06+7%,12), fac(lfac$(23%)), date_field_fmt$(o%+7%),   CH(02),~
       at (06+8%,12), fac(lfac$(26%)), date_field_fmt$(o%+8%),   CH(02),~
       at (06+9%,12), fac(lfac$(29%)), date_field_fmt$(o%+9%),   CH(02),~
                                                                        ~
       at (06+0%,15), fac(hex(8C)), str(date_field_fmt$(o%+0%),3%,), CH(47),~
       at (06+1%,15), fac(hex(8C)), str(date_field_fmt$(o%+1%),3%,), CH(47),~
       at (06+2%,15), fac(hex(8C)), str(date_field_fmt$(o%+2%),3%,), CH(47),~
       at (06+3%,15), fac(hex(8C)), str(date_field_fmt$(o%+3%),3%,), CH(47),~
       at (06+4%,15), fac(hex(8C)), str(date_field_fmt$(o%+4%),3%,), CH(47),~
       at (06+5%,15), fac(hex(8C)), str(date_field_fmt$(o%+5%),3%,), CH(47),~
       at (06+6%,15), fac(hex(8C)), str(date_field_fmt$(o%+6%),3%,), CH(47),~
       at (06+7%,15), fac(hex(8C)), str(date_field_fmt$(o%+7%),3%,), CH(47),~
       at (06+8%,15), fac(hex(8C)), str(date_field_fmt$(o%+8%),3%,), CH(47),~
       at (06+9%,15), fac(hex(8C)), str(date_field_fmt$(o%+9%),3%,), CH(47),~
                                                                        ~
       at (06+0%,63), fac(lfac$(03%)), date_field_name$(o%+0%)  , CH(16),~
       at (06+1%,63), fac(lfac$(06%)), date_field_name$(o%+1%)  , CH(16),~
       at (06+2%,63), fac(lfac$(09%)), date_field_name$(o%+2%)  , CH(16),~
       at (06+3%,63), fac(lfac$(12%)), date_field_name$(o%+3%)  , CH(16),~
       at (06+4%,63), fac(lfac$(15%)), date_field_name$(o%+4%)  , CH(16),~
       at (06+5%,63), fac(lfac$(18%)), date_field_name$(o%+5%)  , CH(16),~
       at (06+6%,63), fac(lfac$(21%)), date_field_name$(o%+6%)  , CH(16),~
       at (06+7%,63), fac(lfac$(24%)), date_field_name$(o%+7%)  , CH(16),~
       at (06+8%,63), fac(lfac$(27%)), date_field_name$(o%+8%)  , CH(16),~
       at (06+9%,63), fac(lfac$(30%)), date_field_name$(o%+9%)  , CH(16),~
                                                                 ~
       at (21,02), fac(hex(A4)),   inpmessage$          , CH(79),~
       at (22,02), fac(hex(8C)),   pf$(1)               , CH(79),~
       at (23,02), fac(hex(8C)),   pf$(2)               , CH(79),~
       at (24,02), fac(hex(8C)),   pf$(3)               , CH(79),~
                                                                 ~
              keys(pfkeys$), key(keyhit%)

       if keyhit% <> 13% then scr2b
           call "MANUAL" ("MMAPINP ") : goto scr2a

scr2b: if keyhit% <> 15% then scr2c
           call "PRNTSCRN" : goto scr2a

scr2c: close WS
       call "SCREEN" addr ("C", U3%, "I", i$(), cursor%())
       return

set_pf2
       if edit% = 2% then scr2f     /*  Input Mode             */
           pf$(1) = "(1)Start Over                                                  ~
(13)Instructions"
           pf$(2) = "                 (4)Previous                                   ~
(15)Print Screen"
           pf$(3) = "                                                               ~
(16)Done        "
           pfkeys$ = hex(01FFFF04FFFFFFFFFFFFFFFF0DFF0F1000)
           if mod( fieldnr%, 3%) = 1% then scr2d
               str(pf$(3%),64%)    = " "  :  str(pfkeys$,16%,1%) = hex(FF)
scr2d:     if mod( fieldnr%, 3%) <> 1% then scr2e
               str(pf$(2%),18%,26%) = " "  :  str(pfkeys$, 4%,1%) = hex(FF)
scr2e:     return

scr2f: if fieldnr% > 0% then scr2g  /*  Edit Mode - Select Fld */
           pf$(1) = "(1)Start Over                       (6)Down                    ~
(13)Instructions"
           pf$(2) = "(2)First         (4)Previous        (7)Up                      ~
(15)Print Screen"
           pf$(3) = "(3)Last          (5)Next            (9)Header                  ~
(16)Save Data   "
       pfkeys$ = HEX(01020304050607FF09FFFFFF0DFF0F1000)

       if o% > 1% then scr2fl      /*  First format line      */
           pf$(2) = "                                                               ~
(15)Print Screen"
       pfkeys$ = HEX(01FF03FF0506FFFF09FFFFFF0DFF0F1000)
scr2fl:

       if o% < 100% then scr2fz     /*  Last format line       */
           pf$(1) = "(1)Start Over                                                  ~
(13)Instructions"
           pf$(3) = "                                    (9)Header                  ~
(16)Save Data   "
       pfkeys$ = HEX(0102FF04FFFF07FF09FFFFFF0DFF0F1000)
scr2fz
       return

scr2g                              /*  Edit Mode - Enabled    */
       pf$(1) = "(1)Start Over                                                  ~
(13)Instructions"
       pf$(2) = "                                                               ~
(15)Print Screen"
       pf$(3) = "                                                                        ~
       "
       pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0DFF0FFF00)
       return

REM *************************************************************~
    *                     T E S T   D A T A                     *~
    *-----------------------------------------------------------*~
    * Test data for the items on Screen 1.                      *~
    *************************************************************

deffn'151(fieldnr%)
       errormsg$ = " "
           on fieldnr% gosub td101,         /* File Name              */~
                             td102,         /* Record Key             */~
                             td103,         /* Break%                 */~
                             td104,         /* SES Version            */~
                             td105          /* SES Record Length      */
       return
td101: rem Test for File Name                    data_rec_name$
           if data_rec_name$ <> " " then td101_chk2
               errormsg$ = "Enter a File Name"
               goto td101_out
td101_chk2
           if str(data_rec_name$, 1%, 1%) <> "?" then td101_out
               gosub inquiry
           if str(data_rec_name$, 1%, 1%) <> "?" then td101_ok
               errormsg$ = "Enter a File Name"
               goto td101_out
td101_ok
           return clear all
           goto editpg1
td101_out
       return

td102: rem Test for Record Key                   data_rec_key$
           if data_rec_key$ <> " " then td102_chk2
               goto td102_chk3
td102_chk2
           if str(data_rec_key$, 1%, 1%) <> "?" then td102_chk3
               gosub inquiry
           if str(data_rec_key$, 1%, 1%) <> "?" then td102_ok
               errormsg$ = "Enter a Record Key"
               goto td102_out
td102_chk3
           plowkey$ = data_rec_name$
           str(plowkey$, 17%, 30%) = data_rec_key$
           call "READ100" (#01, plowkey$, f1%(1%))
           if f1%(1%) = 0% then td102_out
           gosub dataload
td102_ok
           return clear all
           goto editpg1
td102_out
       return

td103: rem Test for Break%                       data_rec_break$
       if data_rec_break$ = " " then td103_num
       convert data_rec_break$ to data_rec_break%, data goto td103_num
       if data_rec_break% >= 0% then td103_out
td103_num
           errormsg$ = "Enter a positive number, or zero."
td103_out
       return

td104: rem Test for SES Version                  data_rec_ver$
       return

td105: rem Test for SES Record Length            data_rec_len$
       if data_rec_len$ = " " then td105_num
       convert data_rec_len$ to data_rec_len%, data goto td105_num
       if data_rec_len% >= 0% then td105_break
td105_num
           errormsg$ = "Enter a positive number, or zero."
           goto td105_out
td105_break
       if data_rec_len% >= data_rec_break% then td105_out
           errormsg$ = "Enter a number greater than break%"
td105_out
       return

REM *************************************************************~
    *                     T E S T   D A T A                     *~
    *-----------------------------------------------------------*~
    * Test data for the items on Screen 2.                      *~
    *************************************************************

deffn'152(fieldnr0%)
       errormsg$ = " "
       if from_input% <> 1% then tdfscreen

	   c% = mod(fieldnr0%, 3%)          /* input mode check       */
	   if c% = 0% then c% = 3%
	   fieldnr0% = o% + (fieldnr0% - 1%) / 3%
	   convert fieldnr0% to fieldnr0$, pic(###)
	   on c%       gosub td201,         /* Start POS              */~
			     td202,         /* Date format            */~
			     td203          /* Field Name             */
       return

tdfscreen                                   /* full screen check      */
       for fieldnr0% = o% to o% + 9%
	   convert fieldnr0% to fieldnr0$, pic(###)
	   for c1% = 1% to 3%

	       if fieldnr0% > 100% or ~
		  errormsg$ <> " " then tdf_loop_out

	       on c1%  gosub td201,         /* Start POS              */~
			     td202,         /* Date format            */~
			     td203          /* Field Name             */

tdf_loop_out
	   next c1%
       next fieldnr0%
       return

td201: rem Test for Start POS                    date_field_start$(1)
       if date_field_start$(fieldnr0%) <> " " then td201_test2
	   if date_field_fmt$(fieldnr0%) <> " " or ~
	      date_field_name$(fieldnr0%) <> " " then td201_num
	   goto td201_out                   /* Blank record           */
td201_test2
       convert date_field_start$(fieldnr0%) to ~
	       date_field_start%(fieldnr0%), data goto td201_num
       if date_field_start%(fieldnr0%) > data_rec_len% then td201_2big
       if date_field_start%(fieldnr0%) < 1% then td201_num
       goto td201_out
td201_2big
       errormsg$ = "#" & fieldnr0$ & "'s starting pos. is " ~
		 & "bigger than record length (" & data_rec_len$ & ")"
       goto td201_out
td201_num
       errormsg$ = "Enter a number for #" & fieldnr0$ & "'s start position"
td201_out
       return

td202: rem Test for Date format                  date_field_fmt$(1)
          if date_field_start$(fieldnr0%) = " " and ~
             date_field_fmt$  (fieldnr0%) = " " and ~
             date_field_name$ (fieldnr0%) = " " then td202_out
          call "GETCODE" (#02,~
                     str(date_field_fmt$(fieldnr0%), 1%, 2%),~
                     str(date_field_fmt$(fieldnr0%), 3%, 48%),~
                     0%,~
                     0.48,~
                     f1%(2%) )
          if f1%(2%) = 0% then td202_err
          convert str( date_field_fmt$(fieldnr0%), 1%, 2%) to fmtlen%
          if fmtlen% = 0% then td202_out
          if date_field_start%(fieldnr0%) > ~
             (data_rec_len% - date_fmt_len%(fmtlen%) + 1%) then td202_2big
          goto td202_out
td202_2big
       errormsg$ = "#" & fieldnr0$ & "'s starting pos. dosn't leave " ~
		 & "room for date. (Record length =" & data_rec_len$ & ")"
       goto td202_out
td202_err
          errormsg$ = "Enter a valid conversion format for #" ~
              & fieldnr0$
td202_out
       return

td203: rem Test for Field Name                   date_field_name$(1)
       if date_field_name$(fieldnr0%) <> " " then td203_out
           if date_field_fmt$(fieldnr0%) <> " " or ~
              date_field_start$(fieldnr0%) <> " " then ~
                  errormsg$ = "Enter a name for #" & fieldnr0$
td203_out
       return

REM **************************************************************~
    *         D E L E T E   C U R R E N T   R E C O R D          *~
    *------------------------------------------------------------*~
    *  Deletes the current file record from MMDATINP.            *~
    **************************************************************
delete_curr_rec
    keyhit% = 2% :           /* put the message at the bottom */
    call "ASKUSER" (keyhit%, "*** Delete this record? ***",~
                             "Press Return to Delete this record",~
                             "- OR -",~
                             "Press PF1 to cancel delete")
    if keyhit% = 1% then return
    if keyhit% <> 0% then delete_curr_rec
    plowkey$ = data_rec_name$
    str(plowkey$, 17%, 30%) = data_rec_key$
    call "READ101" (#01, plowkey$, f1%(1%))
    if f1%(1%) = 1% then call "DELETE" (#01, plowkey$, 48%)
    return clear
    goto inputmode


REM **************************************************************~
    *          I N Q U I R Y   F O R   A   R E C O R D           *~
    *------------------------------------------------------------*~
    *  Uses PLOWCODE to show nearest filename/key combinations.  *~
    **************************************************************
inquiry
           descr_map(01) =  01.16 : descr_map(02) =  0003.00  /*File Name*/
           descr_map(03) =  17.30 : descr_map(04) =  0022.00  /*Rec Key  */
           descr_map(05) =  47.02 : descr_map(06) = -0055.04  /*Brk Len  */
           descr_map(07) =  55.02 : descr_map(08) = -0060.04  /*File Size*/
           descr_map(11) =  49.06 : descr_map(12) =  0065.00  /*SES Ver  */
           descr_map(09) = -41.50 : descr_map(10) =  1008.00  /*File Desc*/

                                /*  1234567890123456   */
           header$(1) = "    " & "File/Desc(below)" ~
                      & "   " & "Record Key                    " ~
                      & "  " & "Break" ~
                      & " "  & "Size" ~
                      & " "  & "SES Ver"

           plowkey$ = data_rec_name$
           str(plowkey$, 17%, 30%) = data_rec_key$
           mat incl_excl = ZER
           call "PLOWCODE" (#01,             ~
                            plowkey$,        ~
                            descr$,          ~
                            9000%,           ~
                            0.70,            ~
                            f1%(1%),         ~
                            header$(),       ~
                            0.3,           ~
                            0.0050,               ~
                            incl_excl(),     ~
                            incl_excl$(),    ~
                            "D", "Y",        ~
                            #03,             ~
                            descr_map())
           if f1%(1%) = 0% then inquiry_end
           gosub dataload
inquiry_end
           return
rem THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
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

       call "FILEBGON" (#02)

       end
