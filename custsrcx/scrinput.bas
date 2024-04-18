        REM *************************************************************~
            *                                                           *~
            *   SSSS   CCC   RRRR   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  S      C   C  R   R    I    NN  N  P   P  U   U    T     *~
            *   SSS   C      RRRR     I    N N N  PPPP   U   U    T     *~
            *      S  C   C  R  RR    I    N  NN  P      U   U    T     *~
            *  SSSS    CCC   R   R  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *************************************************************~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *04/21/2009! Original                                 ! CMG	*~
            *************************************************************


        dim                                                              ~
            hdr$40, msg$(3%)79,          /* ASKUSER TEXT               */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(40%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */ 

        dim                                                              ~
            scr_num$10,                  /* Sequence Number            */~
            scr_model$3,                 /* Screen Model               */~
            scr_mdl$(100)3,              /* Screen Model Array         */~
            model_desc$30,               /* Model Desc                 */~
            scr_color$1,                 /* Screen Color               */~ 
            color_desc$30,               /* Color Desc                 */~
            scr_type$1,                  /* Screen Type Roll or Ext    */~
            scr_hf$1,                    /* Screen Half or Full        */~
            scr_width$10,                /* Screen Width               */~
            scr_height$10,               /* Screen Height              */~
            scr_qty$10,                  /* Screen Quantity            */~
            scr_min$10,                  /* Screen Min Inv Quantity    */~
            scr_desc$30                  /* Screen Item Descr          */

        dim inv_key$19,                  /* SCRINV readkey             */~
            inv_rec$(4)253               /* SCRINV record              */

        dim                                                              ~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            descr$30                     /* Use for GENCODES Look-Up   */


        dim                                                              ~
            idx$(3200)14,               /* Order Index Array          */~
            didx%(15),                  /* Display Index              */~
            inv_data$(3200)150,         /* Inv Data                   */~
            inv_qty(3200)               /* Inv Qty                    */


        dim                                                              ~
            f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = "        Screen Inventory Utility  "
            pname$ = "SCRINPUT "

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
            * #1 ! SCRINV    ! Screen Inventory Master File             *~
            * #2 ! SCRMDL   ! Screen Inventory Model Master File        *~
            * #3 ! GENCODES ! Master System Table File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SCRINV"  ,                                      ~
                        varc,     indexed,  recsize = 1012,              ~
                        keypos = 11,    keylen = 19,                     ~
                        alt key  1, keypos =    1, keylen =  10

            select #2,  "SCRINMDL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 13,                      ~
                        alt key  1, keypos =   11, keylen =  3, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%), 100%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),   0%, rslt$(3%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


            siz% = dim(idx$(),1) /* Number of elements in arrays  */
            m%   = siz% - 15%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            call "ALLFREE"
            gosub initialize_variables

            for fieldnr% = 1% to 10%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  7% then goto  inputmode2
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%
            goto editpg1

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            fieldnr% = 0%
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 10% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120



        inputmode2
            goto load_screen


            gosub initialize_variables

            for fieldnr% = 1% to  1%
L20100:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L20220
L20120:         gosub'102(fieldnr%)       /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L20200
                         fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L20120
                         if fieldnr% = 1% then L20100
                         goto L10150
L20200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L20120
L20220:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L20120
            next fieldnr%

load_screen:
            gosub load_qty_screen
            l% = 0%

        editpg2
           pf$(1) = "(1)Start Over                                     "&~
                    "                             "
           pf$(2) = "(2)First (4)Prev                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "(3)Last  (5)Next                                  "&~
                    "             (16)Save Data   "
           pfkeys$ = hex(0102030405ffffffffffffffffffff10ffffff00)
           if l% <> 0% then L11200
                str(pf$(2), 1,9) = " " : str(pfkeys$,2,1) = hex(ff)
                str(pf$(2),10,7) = " " : str(pfkeys$,4,1) = hex(ff)
                str(pf$(2),18,7) = " " : str(pfkeys$,6,1) = hex(ff)
L11200:    if l% + 15% < maxlines% then L11240
                str(pf$(3), 1,9) = " " : str(pfkeys$,3,1) = hex(ff)
                str(pf$(3),10,7) = " " : str(pfkeys$,5,1) = hex(ff)
                str(pf$(3),18,7) = " " : str(pfkeys$,7,1) = hex(ff)
L11240:     inpmessage$ = "Enter Model and Press (RETURN) to update " & ~
                          "Inventory Numbers"

        gosub'103
          if keyhit%  =  1 then gosub startover
          if keyhit%  =  2 then l% = 0%
          if keyhit%  =  3 then l% = max(0%, min(m%, maxlines% - 15%))
          if keyhit%  =  4 then l% = max(0%, l% - 15%)
          if keyhit%  =  5 then l% = l% + 15%
          if keyhit%  = 16 then gosub datasave
          if keyhit% <>  0 then       editpg2
        goto editpg2


        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return


        deffn'052(fieldnr%)
            enabled% = 1%
        return


        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Record Number or blank for Entry.                      ",~
         "Enter Color Code.                                            ",~
         "Enter Screen Type 'R'oll Form or 'E'xtruded.                 ",~
         "Enter Screen Size 'H'alf or 'F'ull.                          ",~
         "Enter Screen Width in Decimal                                ",~
         "Enter Screen Height in Decimal                               ",~
         "Enter Description                                            ",~ 
         "Enter Inventory Quantity                                     ",~
         "Enter Minimum Inventory Quantity                             ",~
         "Enter the Applicable Models for the Associated Inventory Sizes"

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L29110
                inpmessage$ = edtmessage$
                return

L29110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter Model Number.                                          "



        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") scr_model$, model_desc$, scr_color$, color_desc$,~
                      scr_type$, scr_hf$, scr_width$, scr_height$,     ~
                      scr_desc$, scr_qty$, scr_num$, scr_mdl$(),       ~
                      scr_min$, errormsg$

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
            dataload 
              load% = 0%
              init(" ") inv_key$, inv_rec$()

              str(inv_key$,1,1) = scr_color$
              str(inv_key$,2,1) = scr_type$
              str(inv_key$,3,1) = scr_hf$

              put str(inv_key$,4,8), using pd_fmt, scr_width
pd_fmt:            FMT PD(14,4)

              put str(inv_key$,12,8), using pd_fmt, scr_height

              read #1,key = inv_key$, eod goto no_screen

              get #1, using inv_write, scr_num$,        /* Scr Number  */~
                                       scr_color$,      /* Color       */~
                                       scr_type$,       /* Type        */~ 
                                       scr_hf$,         /* Half Full   */~ 
                                       scr_width,       /* Width       */~ 
                                       scr_height,      /* Height      */~
                                       scr_desc$,       /* Item Desc   */~
                                       scr_qty%



                   convert scr_width to scr_width$,pic(####0.0000)

                   convert scr_height to scr_height$,pic(####0.0000)
                   
                   convert scr_qty% to scr_qty$, pic(########0)

                   convert scr_min% to scr_qty$, pic(########0)

                   gosub load_models
              load% = 1%
            no_screen
            return

            dataload_num
              load%, scr_num%, scr_min% = 0%
              init(" ") inv_key$, inv_rec$()

              convert scr_num$ to scr_num%, data goto no_screen_num

              convert scr_num% to scr_num$, pic(0000000000)

              str(inv_key$,1,10) = scr_num$


              read #1,key 1% = inv_key$, eod goto no_screen_num

              get #1, using inv_write, scr_num$,        /* Scr Number  */~
                                       scr_color$,      /* Color       */~
                                       scr_type$,       /* Type        */~ 
                                       scr_hf$,         /* Half Full   */~ 
                                       scr_width,       /* Width       */~ 
                                       scr_height,      /* Height      */~
                                       scr_desc$,       /* Item Desc   */~
                                       scr_qty%,        /* Init Qty    */~
                                       scr_min%         /* Min INv Qty */


                   convert scr_width to scr_width$,pic(####0.0000)

                   convert scr_height to scr_height$,pic(####0.0000)

                   convert scr_qty% to scr_qty$, pic(########0)

                   convert scr_min% to scr_min$, pic(########0)
                 
                   gosub load_models
              load% = 1%
            no_screen_num
            return


            load_models
               init(" ") inv_key$, scr_mdl$()
               cnt% = 0%
               str(inv_key$,1,10) = scr_num$

            mdl_next
               read #2, key > inv_key$, using mdl_fmt, inv_key$,  ~
                                                   eod goto mdl_done
mdl_fmt:              FMT CH(13)
                 if str(scr_num$,1,10) <> str(inv_key$,1,10) ~
                                       then goto mdl_done
                      
                 cnt% = cnt% + 1%
                 get #2, using mdl_write, scr_num$,         /* Scr Num */~
                                          scr_mdl$(cnt%)    /* Scr mdl */
                      goto mdl_next
            mdl_done
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************
            datasave
               gosub check_qty
               if check% <> 1% then return

               update% = 1%
               for u% = 1% to maxlines%
                  init(" ") scr_color$,scr_type$,scr_hf$, ~
                            scr_desc$, scr_num$
                  scr_width, scr_height = 0.00
                  scr_qty% = 0%

                  scr_num$   = str(inv_data$(u%),1,10)
                  scr_color$ = str(inv_data$(u%),12,1)
                  scr_type$  = str(inv_data$(u%),14,1)
                  scr_hf$    = str(inv_data$(u%),16,1)

                  convert str(inv_data$(u%),18,10) to scr_width

                  convert str(inv_data$(u%),30,10) to scr_height

                  convert str(inv_data$(u%),42,9) to scr_qty%

                  scr_desc$  = str(inv_data$(u%),52,30)
                    gosub dataput
               next u%
             update% = 0%
            return clear all
            goto inputmode

            check_qty
              check%, qty% = 0%
              for q% = 1% to maxlines%
                  
                  convert str(inv_data$(q%),42,9) to qty%, data goto bad_qty


              next q%
              check% = 1%
            return
bad_qty:
              errormsg$ = "Invalid Quantity in Number " & str(inv_data$(q%),1,10)
            return

            dataput
              if scr_num$ = "NEW" then gosub assign_number

              init(" ") inv_key$, inv_rec$()

              str(inv_key$,1,1) = scr_color$
              str(inv_key$,2,1) = scr_type$
              str(inv_key$,3,1) = scr_hf$

              put str(inv_key$,4,8), using pd_fmt, scr_width

              put str(inv_key$,12,8), using pd_fmt, scr_height

              read #1,hold,key = inv_key$, eod goto write_inv

                       delete #1
            write_inv
              put #1, using inv_write, scr_num$,        /* Scr Number  */~
                                       scr_color$,      /* Color       */~
                                       scr_type$,       /* Type        */~ 
                                       scr_hf$,         /* Half Full   */~ 
                                       scr_width,       /* Width       */~ 
                                       scr_height,      /* Height      */~
                                       scr_desc$,       /* Item Desc   */~
                                       scr_qty%,       /* Inital Inv Qt*/~
                                       scr_min%         /* Min Inv Qty */

              write #1, eod goto inv_write_err
              if update% = 1% then return

              gosub update_models

            return clear all
            goto inputmode
            inv_write_err
               init(" ") errormsg$
               errormsg$ = "(ERROR) - Unable to update SCRINV!"
            return clear all
            goto inputmode

            update_models
             for i% = 1 to 100%
               if scr_mdl$(i%) = " " then goto next_mdl
   
               init(" ") inv_key$ 
               str(inv_key$,1,10) = scr_num$
               str(inv_key$,11,3) = scr_mdl$(i%)
 
               read #2, hold, key = inv_key$, eod goto write_mdl
                      delete #2

            write_mdl
              put #2, using mdl_write, scr_num$,        /* Scr Number   */~
                                       scr_mdl$(i%)     /* Scr Mdl      */

              write #2, eod goto mdl_write_err

next_mdl:
             next i%

            return clear all
            goto inputmode
            mdl_write_err
               init(" ") errormsg$
               errormsg$ = "(ERROR) - Unable to update SCRINMDL!"
            return clear all
            goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
inv_write:     FMT CH(10),                          /* Record Number   */~
                   CH(01),                          /* Color           */~
                   CH(01),                          /* Type            */~
                   CH(01),                          /* Half or Full    */~
                   PD(14,4),                        /* Width           */~
                   PD(14,4),                        /* Height          */~
                   CH(30),                          /* Item Desc       */~
                   BI(4),                           /* Inital Qty      */~
                   BI(4)                            /* Min Qty         */

mdl_write:     FMT CH(10),                          /* Record Number   */~ 
                   CH(03)                           /* Model Number    */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,         /* Model             */   ~
                                L40200,         /* Color             */   ~
                                L40200,         /* Type              */   ~
                                L40200,         /* Half or Full      */   ~
                                L40210,         /* Width             */   ~
                                L40210,         /* Height            */   ~ 
                                L40200,         /* Description       */   ~
                                L40210,         /* Inital Item Qty   */   ~
                                L40210,         /* Min Inv Qty       */   ~
                                L40200          /* Models            */

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Record Number (Item)    :",                  ~
               at (04,30), fac(lfac$(1%)), scr_num$             , ch(10),~
               at (05,02), "Color                   :",                  ~
               at (05,30), fac(lfac$(2%)), scr_color$           , ch(01),~
               at (05,40), fac(hex(84)), color_desc$            , ch(30),~
               at (06,02), "Type R or E             :",                  ~
               at (06,30), fac(lfac$(3%)), scr_type$            , ch(01),~
               at (07,02), "Half or Full            :",                  ~
               at (07,30), fac(lfac$(4%)), scr_hf$              , ch(01),~
               at (08,02), "Width Dec               :",                  ~
               at (08,30), fac(lfac$(5%)), scr_width$           , ch(10),~
               at (09,02), "Height Dec              :",                  ~
               at (09,30), fac(lfac$(6%)), scr_height$          , ch(10),~
               at (10,02), "Description             :",                  ~
               at (10,30), fac(lfac$(7%)), scr_desc$            , ch(30),~
               at (11,02), "Quantity                :",                  ~
               at (11,30), fac(lfac$(8%)), scr_qty$             , ch(09),~
                                                                         ~
               at (12,02), "Min Inventory Level     :",                  ~
               at (12,30), fac(lfac$(9%)), scr_min$             , ch(09),~
                                                                         ~
               at (13,02), "Models                  :",                  ~
               at (13,30), fac(lfac$(10%)), scr_mdl$(1%)        , ch(03),~
               at (13,35), fac(lfac$(10%)), scr_mdl$(2%)        , ch(03),~
               at (13,40), fac(lfac$(10%)), scr_mdl$(3%)        , ch(03),~
               at (13,45), fac(lfac$(10%)), scr_mdl$(4%)        , ch(03),~
               at (13,50), fac(lfac$(10%)), scr_mdl$(5%)        , ch(03),~
               at (13,55), fac(lfac$(10%)), scr_mdl$(6%)        , ch(03),~
               at (13,60), fac(lfac$(10%)), scr_mdl$(7%)        , ch(03),~
               at (13,65), fac(lfac$(10%)), scr_mdl$(8%)        , ch(03),~
                                                                         ~
               at (14,30), fac(lfac$(10%)), scr_mdl$(9%)        , ch(03),~
               at (14,35), fac(lfac$(10%)), scr_mdl$(10%)       , ch(03),~
               at (14,40), fac(lfac$(10%)), scr_mdl$(11%)       , ch(03),~
               at (14,45), fac(lfac$(10%)), scr_mdl$(12%)       , ch(03),~
               at (14,50), fac(lfac$(10%)), scr_mdl$(13%)       , ch(03),~
               at (14,55), fac(lfac$(10%)), scr_mdl$(14%)       , ch(03),~
               at (14,60), fac(lfac$(10%)), scr_mdl$(15%)       , ch(03),~
               at (14,65), fac(lfac$(10%)), scr_mdl$(16%)       , ch(03),~
                                                                         ~
               at (15,30), fac(lfac$(10%)), scr_mdl$(17%)       , ch(03),~
               at (15,35), fac(lfac$(10%)), scr_mdl$(18%)       , ch(03),~
               at (15,40), fac(lfac$(10%)), scr_mdl$(19%)       , ch(03),~
               at (15,45), fac(lfac$(10%)), scr_mdl$(20%)       , ch(03),~
               at (15,50), fac(lfac$(10%)), scr_mdl$(21%)       , ch(03),~
               at (15,55), fac(lfac$(10%)), scr_mdl$(22%)       , ch(03),~
               at (15,60), fac(lfac$(10%)), scr_mdl$(23%)       , ch(03),~
               at (15,65), fac(lfac$(10%)), scr_mdl$(24%)       , ch(03),~
                                                                         ~
               at (16,30), fac(lfac$(10%)), scr_mdl$(25%)       , ch(03),~
               at (16,35), fac(lfac$(10%)), scr_mdl$(26%)       , ch(03),~
               at (16,40), fac(lfac$(10%)), scr_mdl$(27%)       , ch(03),~
               at (16,45), fac(lfac$(10%)), scr_mdl$(28%)       , ch(03),~
               at (16,50), fac(lfac$(10%)), scr_mdl$(29%)       , ch(03),~
               at (16,55), fac(lfac$(10%)), scr_mdl$(30%)       , ch(03),~
               at (16,60), fac(lfac$(10%)), scr_mdl$(31%)       , ch(03),~
               at (16,65), fac(lfac$(10%)), scr_mdl$(32%)       , ch(03),~
                                                                         ~
               at (17,30), fac(lfac$(10%)), scr_mdl$(33%)       , ch(03),~
               at (17,35), fac(lfac$(10%)), scr_mdl$(34%)       , ch(03),~
               at (17,40), fac(lfac$(10%)), scr_mdl$(35%)       , ch(03),~
               at (17,45), fac(lfac$(10%)), scr_mdl$(36%)       , ch(03),~
               at (17,50), fac(lfac$(10%)), scr_mdl$(37%)       , ch(03),~
               at (17,55), fac(lfac$(10%)), scr_mdl$(38%)       , ch(03),~
               at (17,60), fac(lfac$(10%)), scr_mdl$(39%)       , ch(03),~
               at (17,65), fac(lfac$(10%)), scr_mdl$(40%)       , ch(03),~
                                                                         ~
               at (18,30), fac(lfac$(10%)), scr_mdl$(41%)       , ch(03),~
               at (18,35), fac(lfac$(10%)), scr_mdl$(42%)       , ch(03),~
               at (18,40), fac(lfac$(10%)), scr_mdl$(43%)       , ch(03),~
               at (18,45), fac(lfac$(10%)), scr_mdl$(44%)       , ch(03),~
               at (18,50), fac(lfac$(10%)), scr_mdl$(45%)       , ch(03),~
               at (18,55), fac(lfac$(10%)), scr_mdl$(46%)       , ch(03),~
               at (18,60), fac(lfac$(10%)), scr_mdl$(47%)       , ch(03),~
               at (18,65), fac(lfac$(10%)), scr_mdl$(48%)       , ch(03),~
                                                                         ~
               at (19,30), fac(lfac$(10%)), scr_mdl$(49%)       , ch(03),~
               at (19,35), fac(lfac$(10%)), scr_mdl$(50%)       , ch(03),~
               at (19,40), fac(lfac$(10%)), scr_mdl$(51%)       , ch(03),~
               at (19,45), fac(lfac$(10%)), scr_mdl$(52%)       , ch(03),~
               at (19,50), fac(lfac$(10%)), scr_mdl$(53%)       , ch(03),~
               at (19,55), fac(lfac$(10%)), scr_mdl$(54%)       , ch(03),~
               at (19,60), fac(lfac$(10%)), scr_mdl$(55%)       , ch(03),~
               at (19,65), fac(lfac$(10%)), scr_mdl$(56%)       , ch(03),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40600
                  gosub display_models
                  goto L40230

L40600:        if keyhit% <> 15 then goto L40640
                  call "PRNTSCRN"
                  goto L40230

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1

        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (7)Enter Quantities    " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (9)Display Models      " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffff07ff09ffffffffff0f1000)
            if fieldnr% = 1% then L40790
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40790:     if fieldnr% > 1% then L40810
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40920  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (7) Enter Models       " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (9)Display Models      " &       ~
                      "                       (16)Data Save   "
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
            return
L40920:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                 (9)Display Models      " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffff09ffffffffffffff00)
            return



        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2 
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              on fieldnr% gosub L40300          /* Model             */  

              goto L40330


                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40300:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */


L40330:     accept                                                       ~
            at (01,02), "Screen Inventory Management       ",            ~
            at (01,67), "Today",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,20), fac(hex(a4)), apc$                      , ch(41),~
            at (03,02), fac(hex(94)), errormsg$                 , ch(79),~
            at (04,02), "Model                   :",                     ~
            at (04,30), fac(lfac$(1%)), scr_model$              , ch(03),~
            at (04,40), fac(hex(84)), model_desc$               , ch(30),~
                                                                         ~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1)                  , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2)                  , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3)                  , ch(79),~
                keys(pfkeys$), key(keyhit%)


REM         if keyhit% <> 0% then goto L40350
REM                  goto L40330

L40350:

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
           pf$(1) = "(1)Start Over                                     "&~
                    "                             "
           pf$(2) = "(2)First (4)Prev                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "(3)Last  (5)Next                                  "&~
                    "             (16)Exit Program"
           pfkeys$ = hex(0102030405ffffffffffffffffffff10ffffff00)
           if l% <> 0% then L41200
                str(pf$(2), 1,9) = " " : str(pfkeys$,2,1) = hex(ff)
                str(pf$(2),10,7) = " " : str(pfkeys$,4,1) = hex(ff)
                str(pf$(2),18,7) = " " : str(pfkeys$,6,1) = hex(ff)
L41200:    if l% + 15% < maxlines% then L41240
                str(pf$(3), 1,9) = " " : str(pfkeys$,3,1) = hex(ff)
                str(pf$(3),10,7) = " " : str(pfkeys$,5,1) = hex(ff)
                str(pf$(3),18,7) = " " : str(pfkeys$,7,1) = hex(ff)
L41240:     inpmessage$ = "Enter Model and Press (RETURN) to update " & ~
                          "Inventory Numbers"

        return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103
            init (hex(8c)) lfac$()
            mat didx% = con  :  mat didx% = (m%) * didx%
            for dl% = 1% to 15%          /* DL% = Display Line */
                c% = dl% + l%            /* IDX$() Element     */
                if c% > maxlines% then L41150
                     get idx$(c%) using L41130, didx%(dl%)
L41130:                   FMT XX(10), BI(4)
                     lfac$(dl%) = hex(81)
L41150:     next dl%


L41330:     accept                                                       ~
            at (01,02), "Screen Inventory Management       ",            ~
            at (01,67), "Today",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,20), fac(hex(a4)), apc$                      , ch(41),~
            at (03,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (04,04), fac(hex(84)) ,  str(inv_data$(didx%( 1)),1,40),  ~
            at (05,04), fac(hex(84)) ,  str(inv_data$(didx%( 2)),1,40),  ~
            at (06,04), fac(hex(84)) ,  str(inv_data$(didx%( 3)),1,40),  ~
            at (07,04), fac(hex(84)) ,  str(inv_data$(didx%( 4)),1,40),  ~
            at (08,04), fac(hex(84)) ,  str(inv_data$(didx%( 5)),1,40),  ~
            at (09,04), fac(hex(84)) ,  str(inv_data$(didx%( 6)),1,40),  ~
            at (10,04), fac(hex(84)) ,  str(inv_data$(didx%( 7)),1,40),  ~
            at (11,04), fac(hex(84)) ,  str(inv_data$(didx%( 8)),1,40),  ~
            at (12,04), fac(hex(84)) ,  str(inv_data$(didx%( 9)),1,40),  ~
            at (13,04), fac(hex(84)) ,  str(inv_data$(didx%(10)),1,40),  ~
            at (14,04), fac(hex(84)) ,  str(inv_data$(didx%(11)),1,40),  ~
            at (15,04), fac(hex(84)) ,  str(inv_data$(didx%(12)),1,40),  ~
            at (16,04), fac(hex(84)) ,  str(inv_data$(didx%(13)),1,40),  ~
            at (17,04), fac(hex(84)) ,  str(inv_data$(didx%(14)),1,40),  ~
            at (18,04), fac(hex(84)) ,  str(inv_data$(didx%(15)),1,40),  ~
                                                                         ~
            at (04,45), fac(lfac$( 1)), str(inv_data$(didx%( 1)),42,9),  ~
            at (05,45), fac(lfac$( 2)), str(inv_data$(didx%( 2)),42,9),  ~
            at (06,45), fac(lfac$( 3)), str(inv_data$(didx%( 3)),42,9),  ~
            at (07,45), fac(lfac$( 4)), str(inv_data$(didx%( 4)),42,9),  ~
            at (08,45), fac(lfac$( 5)), str(inv_data$(didx%( 5)),42,9),  ~
            at (09,45), fac(lfac$( 6)), str(inv_data$(didx%( 6)),42,9),  ~
            at (10,45), fac(lfac$( 7)), str(inv_data$(didx%( 7)),42,9),  ~
            at (11,45), fac(lfac$( 8)), str(inv_data$(didx%( 8)),42,9),  ~
            at (12,45), fac(lfac$( 9)), str(inv_data$(didx%( 9)),42,9),  ~
            at (13,45), fac(lfac$(10)), str(inv_data$(didx%(10)),42,9),  ~
            at (14,45), fac(lfac$(11)), str(inv_data$(didx%(11)),42,9),  ~
            at (15,45), fac(lfac$(12)), str(inv_data$(didx%(12)),42,9),  ~
            at (16,45), fac(lfac$(13)), str(inv_data$(didx%(13)),42,9),  ~
            at (17,45), fac(lfac$(14)), str(inv_data$(didx%(14)),42,9),  ~
            at (18,45), fac(lfac$(15)), str(inv_data$(didx%(15)),42,9),  ~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1)                  , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2)                  , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3)                  , ch(79),~
                keys(pfkeys$), key(keyhit%)


            if keyhit% <> 0% then goto L41350
                  goto L41330
L41350:

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return




        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Scr Number            */ ~
                              L50340,         /* Color                 */ ~
                              L50550,         /* Type                  */ ~
                              L50750,         /* Half or Full          */ ~
                              L50930,         /* Width                 */ ~
                              L51250,         /* Height                */ ~
                              L51500,         /* Description           */ ~
                              L52000,         /* Qty                   */ ~
                              L52400,         /* Min Inv Qty           */ ~
                              L52500          /* Model Array           */
            return

L50150: REM SCR NUMBER                            SCR_NUM$
            if scr_num$ <> " " then gosub dataload_num
            if scr_num$ <> " " and load% = 0% then goto L50290
            if load% = 1% then fieldnr% = 10%
            load% = 0%

            if scr_num$ = " " then scr_num$ = "NEW"

            
        return
L50290:     errormsg$ = "(Error) - Invalid Record?"
            gosub error_prompt
            init(" ") scr_num$
        return

L50340: REM COLOR                                 SCR_COLOR$, COLOR_DESC$
            if scr_color$ = " " then goto L50500
            gosub lookup_color
            if descr$ = " " then goto L50500

            color_desc$ = descr$
        return
L50500:    errormsg$ = "(Error) - Invalid Color?"
           gosub error_prompt
           init(" ") scr_color$, color_desc$
        return

L50550: REM TYPE                                  SCR_TYPE$             
            if scr_type$ = " " then goto L50700

            if scr_type$ <> "E" and scr_type$ <> "R" then ~
                            goto L50700
        return
L50700:     errormsg$ = "(Error) Invalid Screen Type? "
            gosub error_prompt
            init(" ") scr_type$
        return

L50750: REM Half or Full                        SCR_HF$                 
            if scr_hf$ = " " then goto L50880

            if scr_hf$ <> "H" and scr_hf$ <> "F" then ~
                            goto L50880
        return
L50880:    errormsg$ = "(Error) Invalid Half or Full?"
           gosub error_prompt
           init(" ") scr_hf$
        return

L50930: REM WIDTH DECIMAL                       SCR_WIDTH$
           scr_width = 0.00

           convert scr_width$ to scr_width, data goto L51150

           convert scr_width to scr_width$, pic(####0.0000)
        return
L51150:    errormsg$ = "(Error) - Invalid Screen Width?"
           init(" ") scr_width$
           scr_width = 0.00
        return

L51250: REM HEIGHT DECIMAL                       SCR_HEIGHT$
           scr_height = 0.00

           convert scr_height$ to scr_height, data goto L51350

           convert scr_height to scr_height$, pic(####0.0000)


             gosub dataload
             if load% = 0% then return

                 fieldnr% = 9%

        return
L51350:    errormsg$ = "(Error) - Invalid Screen Height?"
           init(" ") scr_height$
           scr_height = 0.00
        return
L51500: REM DESCRIPTION                          SCR_DESC$

        return

L52000: REM INITAL INVENTORY QUANTITY            SCR_QTY$
           scr_qty% = 0%
           convert scr_qty$ to scr_qty%,data goto L52100

           convert scr_qty% to scr_qty$, pic(########0)
        return
L52100:    errormsg$ = "(Error) - Invalid Screen Inventory Quantity?"
           init(" ") scr_qty$
           scr_qty% = 0%
        return
L52400: REM MINIMUM INVENTORY QUANTITY            SCR_MIN$
           scr_min% = 0%
           convert scr_min$ to scr_min%,data goto L52410

           convert scr_min% to scr_min$, pic(########0)
        return
L52410:    errormsg$ = "(Error) - Invalid Minimum Screen Inventory Quantity?"
           init(" ") scr_min$
           scr_min% = 0%
        return
L52500: REM SCREEN MODEL ARRAY                  SCR_MDL$()
        return


        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53150          /* Model                 */ 

            return

L53150: REM MODEL                                 SCR_MODEL$, MODEL_DESC$
            if scr_model$ = " " then goto L53290
            gosub lookup_model
            if descr$ = " " then goto L53290

            model_desc$ = descr$
        return
L53290:     errormsg$ = "(Error) - Invalid Model?"
            gosub error_prompt
            init(" ") scr_model$, model_desc$
        return



        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        assign_number
           init(" ") readkey$, descr$             :    seq% = 0%
           str(readkey$,1,9)   = "SEQUENCE"
           str(readkey$,10,15) = "SCRINPUT"
      
           read #3, hold, key = readkey$, using L51910, descr$, ~
                                                eod goto no_seq

                   convert str(descr$,1,10) to seq%, data goto no_seq

                   seq% = seq% + 1%

                   convert seq% to scr_num$,pic(0000000000)

                   descr$ = scr_num$

                   put #3, using L51910, descr$
                   rewrite #3

        return
        no_seq
           errormsg$ = "(ERROR)-Cannot assign sequence Number-GENCODES ~
                       ~SEQUENCE!"
           gosub error_prompt
        return


        display_models
           readkey$ = " "
           str(readkey$,1%,9%) = "MODEL    "
           descr$ =hex(06) & "Model Information"
           call "PLOWCODE" (#3, readkey$, descr$, 9%, .30, f1%(2%))
        return

        lookup_model
           init(" ") readkey$, descr$

           str(readkey$,1%,9%) = "MODEL"
           str(readkey$,10%,15%) = scr_model$
           read #3,key = readkey$, using L51910, descr$, ~ 
                                      eod goto no_model
L51910:       FMT POS(25), CH(30)
        no_model
        return

        lookup_color
           init(" ") readkey$, descr$

           str(readkey$,1%,9%) = "COLOR"
           str(readkey$,10%,15%) = scr_color$
           read #3,key = readkey$, using L51910, descr$, ~ 
                                      eod goto no_color

        no_color
        return


        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        load_qty_screen 
            maxlines%,s% = 0%
            init(" ") inv_data$(), inv_key$, inv_rec$(), scr_width$, ~
                 scr_height$, scr_qty$
            mat inv_qty = zer
            scr_width, scr_height = 0.00
            scr_qty% = 0%
            inv_key$ = scr_model$

            read #1, key > inv_key$, using qty_fmt, inv_rec$(),           ~ 
                                                     eod goto qty_done
qty_fmt:           FMT 4*CH(253)
               goto qty_scr_first
        qty_scr_next
            read #1, using qty_fmt, inv_rec$(), eod goto qty_done

qty_scr_first:
REM              if str(inv_rec$(),1,3) <> scr_model$ then goto qty_done

              maxlines%, s% = s% + 1%

                                                /* record Num */
              str(inv_data$(s%),  1, 10) = str(inv_rec$(),1,10)
                                                /* Color      */
              str(inv_data$(s%), 12,  1) = str(inv_rec$(),11,1)
                                                /* Type       */
              str(inv_data$(s%), 14,  1) = str(inv_rec$(),12,1)
                                                /* Half Full  */
              str(inv_data$(s%), 16,  1) = str(inv_rec$(),13,1)
                                                /* Width      */
              get str(inv_rec$(),14,8), using pd_fmt, scr_width

              convert scr_width to scr_width$, pic(####0.0000)
              str(inv_data$(s%), 18, 10) = scr_width$
                                                /* Height     */
              get str(inv_rec$(),22,8), using pd_fmt, scr_height

              convert scr_height to scr_height$, pic(####0.0000)
              str(inv_data$(s%), 30, 10) = scr_height$


              get str(inv_rec$(),60%,4) using bi_fmt, scr_qty%
bi_fmt:              FMT BI(04)

              convert scr_qty% to scr_qty$,pic(########0)
                                                /* Quantity    */
              str(inv_data$(s%), 42, 9) = scr_qty$
                                                /* Description */
              call "SPCESMSH" (str(inv_data$(s%),42,9),0%)

              str(inv_data$(s%), 52,30) = str(inv_rec$(),30,30)

              put idx$(s%) using idx_fmt, str(inv_rec$(),1,10), s%
idx_fmt:             FMT CH(10), BI(4)

                   goto qty_scr_next

        qty_done
        return



        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

        end
        
        
            
