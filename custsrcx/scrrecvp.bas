        REM *************************************************************~
            *  Program Name      - SCRRECVP                             *~
            *  Creation Date     - 09/23/2019                           *~
            *                                                           *~
            *  Description       - This Program prints the screen       *~
            *                      label for receiving of outsourced    *~
            *                      screen by the production barcode.    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *09/23/2019! (CR2259) New Program                     ! RDB *~
            *11/20/2019! (CR2345) Add already complete status chk ! RDB *~
            *************************************************************
        dim revision$5          : revision$ = " 1.01"
        
        dim                              /* FILE = APCPLNDT            */~
            dt_part$25,                  /* Prod/Comp Seq. (0) or (1)  */~
            sub_part$20                  /* DESCRIPTION                */
            
        dim                              /* FILE - (APCPLNW1)          */~
            wrk_key$53,                  /* WRK PRIMARY KEY            */~
            wd$7,                        /* Actual Width               */~
            ht$6                         /* Actual Height              */

        dim                              /* (Program Variables)        */~
            hdr$40, msg$(3%)79,          /* ASKUSER TEXT               */~
            screen_dte$8,                /* Screen Comp Date Formated  */~
            screen_dte1$8,               /* Screen Comp Date Unformated*/~
            scr_dte$8,                   /* Screen Completion Date FORM*/~
            scr_dte1$8,                  /* Screen Comp. Date Unform   */~
            scr_barcode$18,              /* Screen production barcode  */~
            wandchar$1,                  /* Screeen scan advance char  */~
            dept$3,                      /* Screen Department Code     */~
            scr_msg$30,                  /* Screen - Report Selection  */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            cdate$8,                     /* Current date               */~
            time$4,                      /* Current time               */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            fld$(3%)30,                  /* Field Text                 */~
            prev_bar$18,                 /* Previously Scanned         */~
            tt_unit$25,                  /* Scan counter               */~
            userid$3                     /* Current User Id            */
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            specialmull$1,               /* Special Mull Flag          */~
            subpart$20                   /* Subpart                    */

        dim f2%(20%),                    /* = 0 if the file is open    */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
            rslt$(20%)20                 /* Text from file opening     */

        dim                              /* FILE - (NEW LABEL VARS)    */~
            model$3,                     /* MODEL CODE                 */~
            width$10,                    /* WIDTH                      */~
            height$10                    /* HEIGHT                     */

       dim  num$3,                       /* Label Day of Week          */~
            colorcd$2, a$256, b$256,                                     ~
            screen$3

        dim dd$(90%)255, bb$(90%)255

        dim rec$(2)256, xx$(90%)255, yy$(90%)255 ,lbl$(40)252,            ~
            file$8,                      /* Lbl Print File             */~
            library$8, batch$5,          /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6                     /* DISK VOLUME = CARLOS       */
            
        dim schema$8                     /* Schema (AWD004)            */    

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = "Receiving Screen Processing Utility  "
            pname$ = "SCRRECVP - Rev:" & revision$

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1 ! AWDPLNSR ! Screen Production File                    *~
            * #2 ! AWDSCHSR ! Flex screen hold file                     *~
            * #3 ! GENCODES ! Master System Table File                  *~
            * #4 ! APCPLNDT ! Production Master Detail File             *~
            * #5 ! MFGSCR   ! Print File for AES Screen Labels          *~
            * #6 ! BCKSUBPT ! Sub part file                             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
 
            select #1, "AWDPLNSR",                                       ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =   42, keylen =   12,                    ~
                        alt key  1, keypos =    7, keylen =  47,         ~
                            key  2, keypos  = 163, keylen =  13,         ~
                            key  3, keypos =   1, keylen =  53,          ~
                            key  4, keypos = 205, keylen =  12, dup

            select #2, "AWDSCHSR",               /*FLEX SCREEN DATA */   ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    10, keylen =  18,                    ~
                        alt key  1, keypos =    1, keylen =  27

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #4,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
                            
            select #5,  "MFGSC2", varc, consec, recsize = 256

            select #6,  "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup


            call "SHOSTAT" ("Opening Files, One Moment Please")
 
            filename$ = "AWDPLNSR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDSCHSR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            cdate$ = date
            call "DATEFMT" (date$)
            time$ = time
            fs$ = "^FS"
            been_here% = 0%
           
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)
                      
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************


        inputmode
            gosub initialize_variables

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 1%
            edit% = 2%
            fieldnr% = cursor%(1%)
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

REM     begin_process

            gosub load_label
            gosub print_labels

            close #5
            call "LINK" addr(script$, lb1%, lb2%)
            call "FILEBGON" (#5)

        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then goto L00010
                inpmessage$ = edtmessage$
                init(" ") scr_barcode$ 
                return
L00010:
        return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_barcode$,               ~
                      scr_dte$, scr_dte1$, hld_key$,                      ~
                      screen_dte$, screen_dte1$
            lb1% = 0% : lb2% = 0%
            if keyhit% <> 1% then gosub open_file
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              scrnr% = 1%
              gosub'050(scrnr%, fieldnr%)
              tt_unit$ = "Scanned Units [ XXXXXX ]"
              convert tt_unit% to str(tt_unit$,17%,6%), pic(######)
              gosub set_screen

              gosub L00200
              goto L00230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L00200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L00230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,29), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(25),~
               at (05,30), fac(lfac$(1%)), scr_barcode$         , ch(18),~
               at (05,50), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
               at (05,52), fac(hex(84)), fld$(2%)               , ch(04),~
               at (05,57), fac(hex(84)), prev_bar$              , ch(18),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_screen
            lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
            fld$(1%)      = "Barcode Number To Scan  :"
            fld$(2%)      = "Prv:"
            
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
        return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            gosub L10150
            
            return

L10150:
REM Skip print if screen the same barcode
            if scr_barcode$ = prev_bar$ then goto L10800
            
REM Production barcode is a FLEX screen order
            sr_key$ = scr_barcode$
            read #2, key = sr_key$, using L10100, sr_rec$(), eod goto L10900
L10100:        FMT 2*CH(256)

REM Match planned barcode and pull warranty number from dt_ref
            str(dt_key$, 1%,18%) = scr_barcode$
            str(dt_key$,19%, 3%) = str(sr_rec$(),55%, 3%)
            str(dt_key$,22%, 2%) = "01"
            read #4, key = dt_key$, using L10105, dt_ref$, eod goto L10910
L10105:        FMT POS(96), CH(08)

REM Confirm the screen record exists for the order 
            str(wrk_key$, 1%, 8%) = dt_ref$
            str(wrk_key$, 9%, 1%) = "0"
            str(wrk_key$,10%, 3%) = "000"
            read #1, key > wrk_key$, using AWDPLNSR, rec$(), eod goto L10920
              if dt_ref$ <> str(rec$(),42%, 8%) then goto L10920
              hld_key$ = str(rec$(),42%,12%)
              
        return

L10800:     errormsg$ = "Same barcode scanned, no label printed - Check Packing Slip for duplicate label "
            init(" ") scr_code$, scr_msg$, scr_barcode$
        return
        
L10900:     errormsg$ = "Not a FLEX screen                              "
            gosub error_prompt
            init(" ") scr_code$, scr_msg$
        return
        
L10910:     errormsg$ = "Window not Planned, Contact Planning Dept      "
            gosub error_prompt
            init(" ") scr_code$, scr_msg$
        return
        
L10920:     errormsg$ = "Screen Batch not created, Contact Planning Dept"
            gosub error_prompt
            init(" ") scr_code$, scr_msg$
        return


/* AWDPLNSR
          1 -   6 PD  DATE
          7 -   7 C   PROCESS
          8 -  13 PD  DATE1
         14 -  33 C   BATCH
         34 -  38 C   BATCH #
         39 -  41 C   DEPT
         42 -  50 C   BAR
         51 -  53 C   NUM
         54 -  58 C   SEQ
         59 -  61 C   MODEL
         62 -  62 C   COLOR
         63 -  63 C   SC
         64 -  64 C   TYPE
         65 -  65 C   HF
         66 -  73 PD  WIDTH DEC
         74 -  81 PD  HEIGHT DEC
         82 -  89 PD  CB LEN
         90 -  97 PD  CB LOC
         98 - 107 C   WIDTH FRAC
        108 - 117 C   HEIGHT FRAC
        118 - 127 C   CB LEN FRAC
        128 - 137 C   CB LOC FRAC
        138 - 162 C   PART
        163 - 163 C   ST
        164 - 172 C   BAR1
        173 - 175 C   NUM1
        176 - 176 C   PULL
        177 - 184 C   S.O.
        185 - 194 C   WD
        195 - 204 C   HT     */
 
        print_labels
          call "SHOSTAT" ("One Moment Please, generating Labels")
          init(hex(00)) wrk_key$
          st_chk% = 0%
   
          wrk_key$ = hld_key$
          read #1, key = wrk_key$, hold, using AWDPLNSR, ~
                     rec$(), eod goto end_label
            goto skip_read
read_next:
          read #1, hold, using AWDPLNSR, rec$(), eod goto end_label
skip_read:
AWDPLNSR: FMT 2*CH(256)
        
          if dt_ref$ <> str(rec$(),042,8) then goto end_label 
   
          if str(rec$(),7,1) <> "1" then goto read_next     /* skip remake */
          if str(rec$(),163, 1) = "2" then gosub complt_reprint  /* CR2345 */
          if st_chk% = 1%  then goto read_next                   /* CR2345 */
          
          colorcd$ = "NA"
          if str(rec$(),62,1) = "2" then colorcd$ = "WH"
          if str(rec$(),62,1) = "6" then colorcd$ = "AL"
          if str(rec$(),62,1) = "7" then colorcd$ = "CY"
          if str(rec$(),62,1) = "3" then colorcd$ = "BZ"
          if str(rec$(),62,1) = "A" then colorcd$ = "NO"
          if str(rec$(),62,1) = "B" then colorcd$ = "HO"
          if str(rec$(),62,1) = "E" then colorcd$ = "CH"
          if str(rec$(),62,1) = "F" then colorcd$ = "CE"
          if str(rec$(),62,1) = "G" then colorcd$ = "LO"
          if str(rec$(),62,1) = "H" then colorcd$ = "DO"
          if str(rec$(),62,1) = "I" then colorcd$ = "ER"
          if str(rec$(),62,1) = "J" then colorcd$ = "EG"
          if str(rec$(),62,1) = "K" then colorcd$ = "EY"
          if str(rec$(),62,1) = "L" then colorcd$ = "ET"
          if str(rec$(),62,1) = "M" then colorcd$ = "EC"
          if str(rec$(),62,1) = "N" then colorcd$ = "EN"
          if str(rec$(),62,1) = "O" then colorcd$ = "EB"
          if str(rec$(),62,1) = "P" then colorcd$ = "EZ"
          if str(rec$(),62,1) = "5" then colorcd$ = "BK"
          if str(rec$(),62,1) = "4" then colorcd$ = "BK"
   
          screen$ = "  "
          gosub read_screen_desc
          if code% = 1% then screen$ = str(desc$,1%,3%)
   
          model$  = str(rec$(),059,3)
          bar$    = str(rec$(),042,9)
          num$    = str(rec$(),051,3)
          seq$    = str(rec$(),054,5)
          width$  = str(rec$(),098,10)
          height$ = str(rec$(),108,10)
          cb_loc$ = " "
          cb_len$ = " "
          wd$     = str(rec$(),185,10)
          ht$     = str(rec$(),195,10)
REM          type$   = str(rec$(),064,1)
          batch$  = str(rec$(),34,5)
   
/* if style is 3SL, print 2 labels; already generated in screen file*/        
          so_inv$  = str(scr_barcode$,1%,8%)
          item_no$ = str(scr_barcode$,9%,2%)        
          gosub lookup_sub_part
     
          gosub print_label_to_file
          
REM        if style$ = "3SL" then gosub print_label_to_file
        
          prev_bar$ = scr_barcode$
          tt_unit% = tt_unit% + 1%
          
          str(rec$(),163, 1) = "2"
          str(rec$(),205, 5) = cdate$
          str(rec$(),210, 4) = time$
          str(rec$(),214, 3) = "FLX"
     
          rewrite #1, using AWDPLNSR, rec$()        
          
          goto read_next

end_label:
        return

        load_label
    /* positioning is 200/inch in both the X & Y axis */
        /* make sure you use font 0 (zero) not O          */

           init(" ") yy$(), dd$(), bb$()

           yy$( 1%) = "^JO"                          /* This format is used*/
           yy$( 2%) = "^XA^EG^XZ"
           yy$( 3%) = "^XA"
           yy$( 4%) = "^PMN"
           yy$( 5%) = "^MNY"
           yy$( 6%) = "^MMTN"                          /* Back Feed Off */
           yy$( 7%) = "^MTT"
           yy$( 8%) = "^MD0"
           yy$( 9%) = "^LH0,0"
           yy$(10%) = "^LL2400"
           yy$(11%) = "^PR4"                          /* (AWD002)            */
           yy$(12%) = "^JMA^XB"                    

           yy$(13%) = "^FO516,25"
           yy$(14%) = "01^BY2^BCN,72,N,N^FD>:" /* Fieldnr% = 01 barcode 245960981^FS */
           yy$(15%) = "^FT583,119"
           yy$(16%) = "^CI0"
           yy$(17%) = "01^A0N,23,32^FD"        /* Fiendnr% = 01 barcode 245960981^FS */
           yy$(18%) = "^FT34,40"
           yy$(19%) = "02^A0N,34,35^FD"        /* Fieldnr% = 02 Model Color Scrn Seq S23  AL  EHS      00010^FS */
           yy$(20%) = "^FT34,154"
           yy$(21%) = "^A0N,34,38^FD(DO NOT REMOVE)^FS"
           yy$(22%) = "^FT34,78"
           yy$(23%) = "03^A0N,34,22^FD"        /*CL  20  1/2XXX  C  15  1/8XXX^FS */
           yy$(24%) = "^FT34,116"
           yy$(25%) = "04^A0N,34,27^FD"        /*20  1/2XXX X 30  1/4XXX^FS */
           yy$(26%) = "^FT504,154"
           yy$(27%) = "05^A0N,34,33^FD A:"     /*45  5/8 X 34  1/4^FS */
           yy$(28%) = "^FT339,154"
           yy$(29%) = "06^A0N,34,40^FDDept. "  /*015^FS */
           yy$(30%) = "^FT389,40"
           yy$(31%) = "08^A0N,34,22^FD"       /*05/19/14^FS */
           yy$(32%) = "^FT324,78"
           yy$(33%) = "07^A0N,34,29^FD"        /*WIRE 1234  ^FS  */
           yy$(34%) = "^FT327,116"
           yy$(35%) = "09^A0N,34,24^FD"        /*SO12345678 ^FS */
           yy$(36%) = "^PQ1,0,1,Y"
           yy$(37%) = "^XZ"

      return

      print_label_to_file
        init (" ") lbl$()
    
        lbl$(01) = bar$ & fs$
        lbl$(02) = model$ & "  " & colorcd$ & "  " & screen$ & "      " &~
                   seq$ & "  " & fs$
        lbl$(03) = " CL  " & cb_loc$ & "  C  " & cb_len$ & fs$
        lbl$(04) = width$ & "X" & height$ & fs$
        lbl$(05) = wd$ & "X" & ht$ & fs$
        lbl$(08) = " " & scr_dte$ & fs$
    
        dept$ = str(rec$(),39,03)
    
        lbl$(06) = dept$  & fs$
        xx$() = yy$()
        sub_part$ = str(rec$(),278,20)
        dt_part$ = str(rec$(),138,25)
     
        lbl$(07) = "        " & fs$
        if str(dt_part$,11,1) = "D" then lbl$(07) = "   LOCK  " & fs$  
REM Subpart door hardware field                                      
                                                                     
        if str(sub_part$,4,1) = "3" then lbl$(07) = "   CSTL  " & fs$  
        if str(sub_part$,4,1) = "D" then lbl$(07) = "   CSTL  " & fs$  
        if str(sub_part$,4,1) = "E" then lbl$(07) = "   CSTL  " & fs$  
        if str(sub_part$,4,1) = "P" then lbl$(07) = "   CSTL  " & fs$  
        if str(sub_part$,4,1) = "Q" then lbl$(07) = "   CSTL  " & fs$  
        if str(sub_part$,4,1) = "R" then lbl$(07) = "   CSTL  " & fs$  
        if str(sub_part$,4,1) = "S" then lbl$(07) = "   CSTL  " & fs$  
        if str(sub_part$,4,1) = "U" then lbl$(07) = "   CSTL  " & fs$  
REM CR 986 for setting new screen mesh, CR1198 for new colors 
        if str(sub_part$,4,1) = "3" or str(sub_part$,4,1) = "D" ~
          or str(sub_part$,4,1) = "E"  or str(sub_part$,4,1) = "P" ~
          or str(sub_part$,4,1) = "Q"  or str(sub_part$,4,1) = "R" ~
          or str(sub_part$,4,1) = "S"  or str(sub_part$,4,1) = "U" ~
            then gosub combine_with_mesh
        if str(sub_part$,4,1) <> "3" and str(sub_part$,4,1) <> "D" ~
          and str(sub_part$,4,1) <> "E"  and str(sub_part$,4,1) <> "P" ~
          and str(sub_part$,4,1) <> "Q"  and str(sub_part$,4,1) <> "R" ~
          and str(sub_part$,4,1) <> "S"  and str(sub_part$,4,1) <> "U" ~      
            then gosub mesh_only
        lbl$(09%) = str(rec$(),177%,08%) & fs$
              
      nbr_lines% = 0%
read_loop:
        init(" ") a$
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */
        l_len% = len(lbl$(ln%))            /* Find Length of Data Element*/
                                           /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%    /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)
      skip_data
                                           /* (AWD002)                  */
        if nbr_lines% = 1% and been_here% = 1% then                        ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

        if nbr_lines% = 1% and been_here% > 1% then                        ~
                               goto read_loop


        gosub print_line
                                           /* (AWD002)                 */
        if a$ = "^XZ" then end_process       /* Last Line */

                                           /* (AWD002)                 */
        goto read_loop

       end_process
          been_here% = been_here% + 1%
       return

       print_line
                                                 
            write #5, using L25030, b$, eod goto L25030
L25030: FMT CH(256)
       return
        
        open_file
            library$        = "APCDATA "
            volume$         = "CARLOS"
            if schema% = 2% then library$ = "NEDATA "
            if schema% = 2% then volume$  = "NE   "     
            file$           = "MFGSC2"
            script$         = "MFGSC2"
            if schema% = 2% then script$ = "NTXSC2"    
            
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L31100
               gosub file_exists
               if comp% <> 16% then goto exit_program
                  call "FILEBGON" (#5)

L31100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        read_screen_desc
           code% = 0%
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "SCREEN"
           str(readkey$,10%,15%) = str(rec$(),63%,1%)
           read #3,key = readkey$, using L12070, desc$, eod goto L12090
L12070:       FMT POS(25), CH(30)
           code% = 1%
L12090: return
        
        lookup_sub_part
          init(" ") bcksubpt_rec$, flds$(), info_flds$(), specialmull$, ~
                    subpart$, series$, style$

          flag$ = "0"                  /* Sales Order Info         */
          pgm$  = "1"
          err1% = 0%

          convert so_inv$ to so_inv%, data goto convert_alpha
          convert so_inv% to so_inv$, pic(00000000)
          goto order_converted

convert_alpha:
          convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
          convert so_inv% to str(so_inv$,2%,7%), pic(0000000)

order_converted:
          convert item_no$ to item_no%, data goto sub_part2
sub_part2:
          convert item_no% to item_no$, pic(###)

          call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                            pgm$,          /* Calling Program 0=BCKUPDTE */~
                                           /* 1=Any Other 2=Delete       */~
                                           /* 3=Invoice                  */~
                            so_inv$,       /* SO or Invoice Num to lookup*/~
                            item_no$,      /* Item Number                */~
                            bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                           /* pass in else pass out      */~
                            flds$(),       /* Part Number Fields         */~
                            info_flds$(),  /* Information Fields         */~
                            #6,            /* BCKSUBPT File              */~
                            err1%)         /* Error Code                 */

            if err1% <> 0% then                                ~
                    str(bcksubpt_rec$,48%,20%) = "00000000000000000000"

            init(" ") specialmull$, subpart$
            specialmull$ = str(bcksubpt_rec$,152%,1%)
            subpart$     = str(bcksubpt_rec$,48%,20%)
            series$  = str(bcksubpt_rec$,169%,16%) 
            style$   = str(bcksubpt_rec$,185%,10%) 
        return
        
        file_exists
          comp% = 2%
          hdr$ = "***  Barcode Label ***"
          msg$(1%) = "       The File (MFGSC2) Already Exists.         "
          msg$(2%) = "       New  B A R C O D E   L a b e l s          "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

/* CR2345  screen already scanned complete, ask if user wants to print label */
        complt_reprint
          comp% = 2%
          hdr$     = "** Screen Barcode Already Scanned Complete **  "
          msg$(1%) = "                                               "
          msg$(2%) = "    To print the label  - Press Any PF Key     "
          msg$(3%) = "    To *NOT* print the label - Press PF(4)     "
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
          if comp% = 4% then st_chk% = 1%
        return

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
        
        combine_with_mesh             /* CR 986 Combine new mesh with costal */
           if str(sub_part$,15,1) = "2" then lbl$(07) = "WIRE CSTL" & fs$
           if str(sub_part$,15,1) = "3" then lbl$(07) = "HVY CSTL " & fs$ 
           if str(sub_part$,15,1) = "4" then lbl$(07) = "PET CSTL " & fs$ 
           /* CR1856 */
           if str(sub_part$,15,1) = "5" then lbl$(07) = "SOLR CSTL" & fs$
           if str(sub_part$,15,1) = "9" then lbl$(07) = "CLR CSTL " & fs$ 
     
        return
        
        mesh_only                    /* CR 986 Set new mesh  */
           if str(sub_part$,15,1) = "2" then lbl$(07) = "   WIRE  " & fs$
           if str(sub_part$,15,1) = "3" then lbl$(07) = "   HVY   " & fs$ 
           if str(sub_part$,15,1) = "4" then lbl$(07) = "   PET   " & fs$ 
           /* CR1856 */
           if str(sub_part$,15,1) = "5" then lbl$(07) = "   SOLAR " & fs$
           if str(sub_part$,15,1) = "9" then lbl$(07) = "   CLR   " & fs$ 
     
        return
        
        open_error                            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
        
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program

            close #1 : close #2 : close #3 : close #4 : close #6
            close #5

            call "FILEBGON" (#5)
        end
