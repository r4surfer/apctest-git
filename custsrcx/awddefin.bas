rem *THISPROGRAMWASGENERATEDUSINGGENPGMPROGRAMWHICHISPROPRIETARY*~
    *                                                           *~
    *                                                           *~
    *                                                           *~
    *                                                           *~
    *                                                           *~
    *                                                           *~
    *                                                           *~
    *-----------------------------------------------------------*~
    * AWDDEFIN -                                                *~
    *-----------------------------------------------------------*~
    *  This program contains valuable trade secrets and         *~
    *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
    *  embodying substantial creative efforts and confidential  *~
    *  information.  Unauthorized use, copying, decompiling,    *~
    *  translating, disclosure, or transfer of it is prohibited.*~
    *  Copyright (c) 2006, an unpublished work by CAELUS,       *~
    *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
    *-----------------------------------------------------------*~
    *                  M O D I F I C A T I O N S                *~
    *---WHEN---+----------------WHAT----------------------+-WHO-*~
    * 03/12/06 ! Original                                 ! CMG *~
    * 04/14/17 ! Add NcPlan TxPlan ncendpln58 txendpln58  ! RDB *~
    * 05/08/17 ! Add user ID to table                     ! RDB *~
    PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVED

    dim                                                              ~
        blankdate$8,                 /* Millennium comparisons     */~
        cursor%(2),                  /* Cursor location for edit   */~
        date$8,                      /* Date for screen display    */~
        dwarrant$8,                  /* Dallas warranty            */~
        edtmessage$79,               /* Edit screen message        */~
        errormsg$79,                 /* Error message              */~
        i$(24)80,                    /* Screen Image               */~
        inpmessage$79,               /* Informational Message      */~
        lfac$(20)1,                  /* Field Attribute Characters */~
        line2$79,                    /* Screen Line #2             */~
        pf$(3)79,                    /* PF Screen Literals         */~
        pfkeyS$32,                   /* PF Key Hex Values          */~
        swarrant$8,                  /* Stock Warranty             */~
        userid$3,                    /* Current User Id            */~
        warranty$8,                  /* Warranty                   */~
        mfgid$8,                     /* Mfg ID                     */~
        ncplan$1,                    /* NC PLan Indicator          */~
        txplan$1,                    /* TX Plan Indictor           */~
        ncendpln58$1,                /* NC End Plan Indicator      */~
        txendpln58$1,                /* TX End Plan Indicator      */~
        ncuserid$3,                  /* NC user ID                 */~
        txuserid$3,                  /* TX user ID                 */~
        ncendid$3,                   /* NC end user ID             */~
        txendid$3                    /* TX end user ID             */

    dim readkey$100,                 /* Generic Readkey            */~
        descr$56,                    /* Generic Description        */~
        descr$(50%)56,               /* Generic Descriptions       */~
        fields$(50%)                 /* Array of Fields            */

    dim f2%(64),                     /* = 0 if the file is open    */~
        f1%(64),                     /* = 1 if READ was successful */~
        fs%(64),                     /* = 1 if file open, -1 if it */~
                                     /*   doesn't exist, or 0 if   */~
                                     /*   not yet checked (OPENCHCK*/~
        rslt$(64)20                  /* Text from file opening     */

rem *************************************************************~
    *                  Release Version ID Section               *~
    *************************************************************
    dim cms2v$50
    cms2v$ = "01.00.00 12/31/99 Pre-Release Version            "
rem *************************************************************

    mat f2% = con

rem *************************************************************~
    *                  S E L E C T   F I L E S                  *~
    *-----+----------+------------------------------------------*~
    *File#!  PRName  ! File Description                         *~
    *-----+----------+------------------------------------------*~
    * #01 ! AWDDEFIN ! New Number Starting Number definions     *~
    *************************************************************~
    * File Selection and Open Calls                             *~
    *************************************************************

    select #01, "AWDDEFIN",                                      ~
           varc,   indexed,       recsize =  64,                 ~
                   keypos =  1,   keylen  =   8

   call "SHOSTAT" ("Opening Files, One Moment Please")

   call "OPENCHCK" (#01, fs%(01%), f2%(01%), 100%, rslt$(01%))


rem *************************************************************~
    *                I N I T I A L I Z A T I O N                *~
    *-----------------------------------------------------------*~
    * Initializes information necessary for program.            *~
    *************************************************************
    call "EXTRACT" addr("ID", userid$)
    date$ = date
    call "DATEFMT" (date$)
    /* Blank date - Millennium compliant comparisons */
    blankdate$ = " "
    call "DATUFMTC" (blankdate$)

    /* True and False for clarity */
    true%   = 1%
    false%  = 0%

    edtmessage$  = "To Modify Displayed Values, Position Cursor" & ~
                   " to Desired Value & Press (RETURN)."

    str(line2$,62%) = "        : " & STR(CMS2V$,,8)


    init(" ") fields$()

    field_max% = 12%

    fields$(1%) = "WARRANTY"
    fields$(2%) = "DWARRANT"
    fields$(3%) = "SWARRANT"
    fields$(4%) = "MFGID"
    fields$(5%) = "NCPLAN"
    fields$(6%) = "NCUSERID"
    fields$(7%) = "TXPLAN"
    fields$(8%) = "TXUSERID"
    fields$(9%) = "NCENDPLN58"
    fields$(10%) = "NCENDID"
    fields$(11%) = "TXENDPLN58"
    fields$(12%) = "TXENDID"

rem *************************************************************~
    *       I N P U T   M O D E   M A I N   P R O G R A M       *~
    *-----------------------------------------------------------*~
    * Handles normal input for data entry screens.              *~
    *************************************************************

inputmode
       gosub initialize_variables
       gosub dataload

       for fieldnr% = 1% TO  field_max%
inp1a:     gosub'051(fieldnr%)              /* Default - Enables */
           if enabled% = false%  then inp1e
inp1b:     gosub'101(fieldnr%, false%)      /* Display - Accept  */
               if keyhit%  =  1% then gosub startover
               if keyhit% <>  4% then       inp1d
inp1c:             fieldnr% = max(1%, fieldnr% - 1%)
                   gosub'051(fieldnr%)
                   if enabled% = true% then inp1b
                   if fieldnr% = 1%    then inp1a
                   goto inp1c
inp1d:         if keyhit% = 16% and fieldnr% = 1% then exit_program
               if keyhit% <> 0%                   then inp1b
inp1e:     gosub'151(fieldnr%)      /* Edit Field for Valid Entry */
               if errormsg$ <> " " then inp1b
       next fieldnr%


rem *************************************************************~
    *        E D I T   M O D E   M A I N   P R O G R A M        *~
    *-----------------------------------------------------------*~
    * Handles operation of EDIT MODE for data entry screens.    *~
    *************************************************************

editpg1
       lastfieldnr% = 0%
       gosub'101(0%, true%)        /* Display Screen - No Entry */
           if keyhit%  =  1% then gosub startover
           if keyhit%  = 16% then       datasave
           if keyhit% <>  0% then       editpg1
edt1a: fieldnr% = cursor%(1%) - 5%
       if fieldnr% < 1% OR fieldnr% >  field_max% then editpg1
       if fieldnr% = lastfieldnr%         then editpg1
       gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
           if enabled% =  false%   then editpg1
edt1b: gosub'101(fieldnr%, true%)  /* Display & Accept Screen     */
           if keyhit%  =  1%   then gosub startover
           if keyhit% <>  0%   then       edt1b
       gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
           if errormsg$ <> " " then edt1b
           lastfieldnr% = fieldnr%
           goto edt1a


rem *************************************************************~
    *             S A V E   D A T A   O N   F I L E             *~
    *-----------------------------------------------------------*~
    * Saves data on file after INPUT/EDITING.                   *~
    *************************************************************

datasave

    for i% = 1% to field_max%
       gosub dataput
    next i%


    goto inputmode


rem *************************************************************~
    *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
    *-----------------------------------------------------------*~
    * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
    *************************************************************

deffn'051(fieldnr%)
    enabled% = true%
    on fieldnr% gosub df101,         /* Warranty               */~
                      df201,         /* Dallas Warranty        */~
                      df301,         /* Stock Warrany          */~
                      df401,         /* MFG ID                 */~
                      df501,         /* NC PLAN                */~
                      df601,         /* NC USER ID             */~
                      df701,         /* TX PLAN                */~
                      df801,         /* TX USER ID             */~
                      df901,         /* NC END PLAN 58         */~
                      df111,         /* NC END USER ID         */~
                      df121,         /* TX END PLAN 58         */~
                      df131          /* TX END USER ID         */
return

df101   /* Def/Enable Warranty                    warranty$           */
return
df201   /* Def/Enable Dallas Warranty             dwarrant$           */
return
df301   /* Def/Enable Stock  Warranty             swarrant$           */
return
df401   /* Def/Enable Mfg ID                         mfgid$           */
return
df501   /* Def/Enable NC PLAN                       ncplan$           */
return
df601   /* Def/Enable NC USER ID                    ncuserid$         */
return
df701   /* Def/Enable TX PLAN                       txplan$           */
return
df801   /* Def/Enable TX USER ID                    txuserid$         */
return
df901   /* Def/Enable NC END PLAN 58                ncendpln58$       */
return
df111   /* Def/Enable NC END PLAN USER ID           ncendid$          */
return
df121   /* Def/Enable TX END PLAN 58                txendpln58$       */
return
df131   /* Def/Enable TX END PLAN USER ID           txendid$          */
return

rem *************************************************************~
    *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
    *-----------------------------------------------------------*~
    * Initializes Variable Field Input Messages.                *~
    *************************************************************

deffn'050(scrnr%, fieldnr%)
       if fieldnr% <> 0% then im000
           inpmessage$ = edtmessage$
return


im000: rem Define the Input Message for the Screen/Field Indicated
       if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
       read inpmessage$      /* Read Input Message */
return


scrn1_msg  :  data                                        ~
       "Enter Warranty                                        ",~
       "Enter Dallas Warranty                                 ",~
       "Enter Stock Warranty                                  ",~
       "Enter MFG ID                                          ",~
       "Enter NC Plan Indicator                               ",~
       "Enter NC User ID                                      ",~
       "Enter TX Plan Indicator                               ",~
       "Enter TX User ID                                      ",~
       "Enter NC End Plan 58 Indicated                        ",~
       "Enter NC End Plan 58 User ID                          ",~
       "Enter TX End Plan 58 Indicator                        ",~
       "Enter TX End Plan 58 User ID                          "
 
rem *************************************************************~
    * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
    *-----------------------------------------------------------*~
    * Initializes all defined screen variables to blank         *~
    *************************************************************
initialize_variables
    init(" ") errormsg$, inpmessage$,                            ~
              readkey$, warranty$, descr$, descr$(), dwarrant$,  ~
              swarrant$, mfgid$, ncplan$, txplan$, ncendpln58$,  ~
              txendpln58$, ncuserid$, txuserid$, ncendid$, txendid$

return


rem *************************************************************~
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


rem *************************************************************~
    *           L O A D   D A T A   F R O M   F I L E           *~
    *-----------------------------------------------------------*~
    * Loads data from File Record Area into Program Variables.  *~
    *************************************************************
dataload

    for i% = 1% to field_max%
        gosub load_data
    next i%

    
         
return

load_data

    init(" ") readkey$, descr$
    readkey$ = fields$(i%)

    read #1, key = readkey$, eod goto no_load_data


         get #1, using FMT_02, descr$
FMT_02:        FMT XX(08), CH(56)

         gosub load_scr_field

no_load_data
return

load_scr_field

    on i% gosub field_1, field_2, field_3, field_4, field_5, ~
                field_6, field_7, field_8, field_9, field_10, ~ 
                field_11, field_12

return


field_1

 warranty$ = str(descr$,1%,8%)

return

field_2
 dwarrant$ = str(descr$,1%,8%)

return
field_3
 swarrant$ = str(descr$,1%,8%)

return
field_4
 mfgid$ = str(descr$,1%,8%)

return

field_5
 ncplan$ = str(descr$,1%,1%)

return

field_6
 ncuserid$ = str(descr$,1%,3%)

return

field_7
 txplan$ = str(descr$,1%,1%)

return

field_8
 txuserid$ = str(descr$,1%,3%)

return

field_9
 ncendpln58$ = str(descr$,1%,1%)

return

field_10
 ncendid$ = str(descr$,1%,3%)

return

field_11
 txendpln58$ = str(descr$,1%,1%)

return

field_12
 txendid$ = str(descr$,1%,3%)

return

rem *************************************************************~
    *          S T U F F   D A T A   I N T O   F I L E          *~
    *-----------------------------------------------------------*~
    * Stuffs data from Program Variables into File Record Area. *~
    *************************************************************
dataput
     init(" ") readkey$
     readkey$ = fields$(i%)

     read #1, hold, key = readkey$, eod goto not_defined

             delete #1
not_defined

      put #1, using  FMT_01, str(readkey$,1%,8%), ~
                             descr$(i%)


FMT_01:         FMT CH(08), CH(56)

      write #1
     
return


rem *************************************************************~
    *   G E N E R I C    S C R E E N    S U B R O U T I N E S   *~
    *-----------------------------------------------------------*~
    * uprlowr       - FAC Upper or Lower Case                   *~
    * upronly       - FAC Upper Case Only                       *~
    * numeric       - FAC Numeric                               *~
    * man_or_prtscr - Manual or Print Screen                    *~
    * get_screen    - Get screen and cursor image               *~
    *************************************************************

uprlowr
    lfac$(fieldnr%) = hex(80)
return


upronly
    lfac$(fieldnr%) = hex(81)
return


numeric
    lfac$(fieldnr%) = hex(82)
return


man_or_prtscr
    if keyhit% = 13% then call "MANUAL" ("        ") ~
                     else call "PRNTSCRN"
return


get_screen
    close WS
    call "SCREEN" addr ("C", U3%, "I", i$(), cursor%())
return


rem *************************************************************~
    *               S C R E E N   P A G E   1                   *~
    *-----------------------------------------------------------*~
    * Document Input and Edit Screen.                           *~
    *************************************************************

deffn'101(fieldnr%, edit%)
    gosub'050(1%, fieldnr%)
    if edit% = false% then gosub setpf_pg1_inp ~
                      else gosub setpf_pg1_edt

    if fieldnr% > 0% then init(hex(8C)) lfac$()                ~
                     else init(hex(86)) lfac$()

    on fieldnr% gosub numeric,         /* Warranty          */~
                      upronly,         /* DWarrant          */~
                      upronly,         /* SWarrant          */~
                      numeric,         /* Mfg ID            */~
                      numeric,         /* NCPLAN            */~
                      upronly,         /* ncuserid          */~      
                      numeric,         /* TXPLAN            */~
                      upronly,         /* txuserid          */~
                      numeric,         /* NCENDPLN58        */~
                      upronly,         /* ncendid           */~
                      numeric,         /* txendpln58        */~
                      upronly          /* txenid            */

  scr1_dsply
    accept                                                       ~
       at (01,02),                                               ~
          "Define Various Beginning Numbers",                    ~
       at (01,66), "Today:",                                     ~
       at (01,73), fac(hex(8C)), date$                  , CH(08),~
       at (02,02), fac(hex(AC)), line2$                 , CH(79),~
       at (04,02), fac(hex(94)), errormsg$              , CH(79),~
                                                                 ~
       at (06,02), "Warranty",                                   ~
       at (06,30), fac(lfac$( 1)), warranty$            , CH(08),~
                                                                 ~
       at (07,02), "Darranty",                                   ~
       at (07,30), fac(lfac$( 2)), dwarrant$            , CH(08),~
                                                                 ~
       at (08,02), "Sarranty",                                   ~
       at (08,30), fac(lfac$( 3)), swarrant$            , CH(08),~
                                                                 ~
       at (09,02), "MFG ID  ",                                   ~
       at (09,30), fac(lfac$( 4)), mfgid$               , CH(08),~
                                                                 ~
       at (10,02), "NC Plan ",                                   ~
       at (10,30), fac(lfac$( 5)), ncplan$              , CH(01),~
                                                                 ~  
       at (10,35), "NC User ID",                                 ~
       at (10,57), fac(lfac$( 6)), ncuserid$            , CH(03),~
                                                                 ~  
       at (11,02), "TX Plan ",                                   ~
       at (11,30), fac(lfac$( 7)), txplan$              , CH(01),~
                                                                 ~
       at (11,35), "TX User ID ",                                ~
       at (11,57), fac(lfac$( 8)), txuserid$            , CH(03),~
                                                                 ~
       at (12,02), "NC End Plan 58 ",                            ~
       at (12,30), fac(lfac$( 9)), ncendpln58$          , CH(01),~
                                                                 ~
       at (12,35), "NC End 58 User ID ",                         ~
       at (12,57), fac(lfac$(10)), ncendid$             , CH(03),~
                                                                 ~
       at (13,02), "TX End Plan 58  ",                           ~
       at (13,30), fac(lfac$(11)), txendpln58$          , CH(01),~
                                                                 ~
       at (13,35), "TX End 58 User ID  ",                        ~
       at (13,57), fac(lfac$(12)), txendid$             , CH(03),~
                                                                 ~                                                                 
       at (21,02), fac(hex(A4)),   inpmessage$          , CH(79),~
       at (22,02), fac(hex(8C)),   pf$(1%)              , CH(79),~
       at (23,02), fac(hex(8C)),   pf$(2%)              , CH(79),~
       at (24,02), fac(hex(8C)),   pf$(3%)              , CH(79),~
                                                                 ~
       keys(pfkeys$), key(keyhit%)

    if keyhit% = 13% or keyhit% = 15% then gosub man_or_prtscr
    if keyhit% = 13% or keyhit% = 15% then goto  scr1_dsply

    gosub get_screen
return


setpf_pg1_inp
    /* Input Mode */
    pf$(1%) = "(1)Start Over                                                  (13)Instructions"
    pf$(2%) = "                 (4)Previous Field                             (15)Print Screen"
    pf$(3%) = "                                                               (16)Exit Program"
    pfkeys$ = hex(01FFFF04FFFFFFFFFFFFFFFF0DFF0F1000)

    /* Field one, turn off previous field, otherwise turn off exit */
    if fieldnr% = 1% then str(pf$(2%), 18%, 26%) = " " ~
                     else str(pf$(3%), 64%, 16%) = " "
    if fieldnr% = 1% then str(pfkeys$,  4%,  1%) = hex(FF) ~
                     else str(pfkeys$, 16%,  1%) = hex(FF)
return


setpf_pg1_edt
    /* Edit Mode */
    if fieldnr% > 0% then setpf_pg1_edtfld

    /* Edit Mode - no field selected */
    pf$(1%) = "(1)Start Over                                                  (13)Instructions"
    pf$(2%) = "                                                               (15)Print Screen"
    pf$(3%) = "                                                               (16)Save Data   "
    pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0DFF0F1000)
    goto setpf_pg1_edtexit

  setpf_pg1_edtfld
    pf$(1%) = "(1)Start Over                                                  (13)Instructions"
    pf$(2%) = "                                                               (15)Print Screen"
    pf$(3%) = "                                                                               "
    pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0DFF0FFF00)

  setpf_pg1_edtexit
return




rem *************************************************************~
    *                     T E S T   D A T A                     *~
    *-----------------------------------------------------------*~
    * Test data for the items on Screen 1.                      *~
    *************************************************************

deffn'151(fieldnr%)
    errormsg$ = " "
    on fieldnr% gosub td101,         /* Warranty               */~
                      td201,         /* Dallas Warranty        */~
                      td301,         /* Stock  Warranty        */~
                      td401,         /* MFG ID                 */~
                      td501,         /* NC Plan                */~
                      td601,         /* NC User ID             */~            
                      td701,         /* TX Plan                */~
                      td801,         /* TX User ID             */~
                      td901,         /* NC End Plan 58         */~
                      td111,         /* NC End Plan 58 User ID */~
                      td121,         /* TX End Plan 58         */~
                      td131          /* TX End Plan 58 User ID */
  
return

td101   /* Test for Warranty                     warranty$    */
     init(" ") descr$(1%)  :  warranty% = 0

     convert warranty$ to warranty%, data goto er101

     convert warranty% to descr$(1%), pic(00000000)
return
er101:
     errormsg$ = "Invaild Warranty Number??" 

     return

td201 /* test for Dallas Warranty              dwarrant$           */
     init(" ") descr$(2%)  :  warranty% = 0
     str(descr$(2%),1,1) = "D"

     convert str(dwarrant$,2,7) to warranty%, data goto er201

     convert warranty% to str(descr$(2%),2,7), pic(0000000)
return
er201:
     errormsg$ = "Invaild Dallas Warranty Number??" 

     return      

td301 /* test for Stock  Warranty              swarrant$           */
     init(" ") descr$(3%)  :  warranty% = 0
     str(descr$(3%),1,1) = "S"

     convert str(swarrant$,2,7) to warranty%, data goto er301

     convert warranty% to str(descr$(3%),2,7), pic(0000000)
return
er301:
     errormsg$ = "Invaild Stock  Warranty Number??" 

     return

td401   /* Test for Mfg ID                          mfgid$           */
     init(" ") descr$(4%)  :  mfgid% = 0

     convert mfgid$ to mfgid%, data goto er401

     convert mfgid% to descr$(4%), pic(00000000)
return
er401:
     errormsg$ = "Invaild MFG ID Number??" 

     return
  
td501   /* Test for NC Plan                         ncplan$          */
     init(" ") descr$(5%)  :  ncplan% = 0%

     convert ncplan$ to ncplan% , data goto er501
     if ncplan$ < "0" or ncplan$ > "1"  then goto er501

     convert ncplan% to descr$(5%), pic(0)
return
er501:
     errormsg$ = "Invaild NC Plan Indicator??" 

     return
     
td601   /* Test for NC User ID */
     init(" ") descr$(6%)  

     descr$(6%) = ncuserid$
return

td701   /* Test for TX Plan                         txplan$          */
     init(" ") descr$(7%)  :  txplan% = 0

     convert txplan$ to txplan% , data goto er701
     if txplan$ < "0" or txplan$ > "1"  then goto er701

     convert txplan% to descr$(7%), pic(0)
return
er701:
     errormsg$ = "Invaild TX Plan Indicator??" 

     return
     
td801   /* Test for TX User ID */
     init(" ") descr$(8%)  

     descr$(8%) = txuserid$
return

td901   /* Test for NC End Plan 58                  ncendpln58$     */
     init(" ") descr$(9%)  :  ncendpln58% = 0

     convert ncendpln58$ to ncendpln58% , data goto er901
     if ncendpln58$ < "0" or ncendpln58$ > "1"  then goto er901

     convert ncendpln58% to descr$(9%), pic(0)
return
er901:
     errormsg$ = "Invaild NC End Plan 58 Indicator??" 

     return

td111   /* Test for NC End Plan 58 User ID */
     init(" ") descr$(10%)  

     descr$(10%) = ncendid$
return
 
td121   /* Test for TX End Plan 58                   txendpln58$   */
     init(" ") descr$(11%)  :  txendpln58% = 0

     convert txendpln58$ to txendpln58% , data goto er121
     if txendpln58$ < "0" or txendpln58$ > "1"  then goto er121

     convert txendpln58% to descr$(11%), pic(0)
return
er121:
     errormsg$ = "Invaild TX End Plan 58 Indicator??" 

     return

td131   /* Test for TX End Plan 58 User ID */
     init(" ") descr$(12%)  

     descr$(12%) = txendid$
return

rem  THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAEUS,INC~
    *                          E X I T                          *~
    *-----------------------------------------------------------*~
    * Terminates execution (files closed automatically).        *~
    *-----------------------------------------------------------*~
    *  This program contains valuable trade secrets and         *~
    *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
    *  embodying substantial creative efforts and confidential  *~
    *  information.  Unauthorized use, copying, decompiling,    *~
    *  translating, disclosure, or transfer of it is prohibited.*~
    *  Copyright (c) 2006, an unpublished work by CAELUS,       *~
    *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
    CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

exit_program  

    end   
    
    
    