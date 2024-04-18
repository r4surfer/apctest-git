rem *********************************************************************~
    *                                                                   *~
    *  Program Name      - APCPRWFB                                     *~
    *  Creation Date     - 05/15/98                                     *~
    *  Last Modified Date- 07/29/2014                                   *~
    *  Description       - Determines if a part number is Wood          *~
    *                      Surround or Factory Mull and, if so,         *~
    *                      determines the particulars that are          *~
    *                      needed for pricing that product.             *~
    *                                                                   *~
    *                                                                   *~
    *-------------------------------------------------------------------*~
    *                      M O D I F I C A T I O N S                    *~
    *---WHEN---+----------------WHAT------------------------------+-WHO-*~
    * 05/15/98 ! Original                                         ! ERN *~
    * 10/07/98 ! (EWD001) - Bug fix. WS on Ctg/Orl not pricing.   ! BWS *~
    * 09/06/02 ! (EWD002) - Fix for Special Shapes Grid Codes.    ! CMG *~
    * 06/26/06 ! (AWD003) - Mod for Triple Mulls                  ! CMG *~
    *07/29/2014! (AWD004) - Mod for u-channel & mull clips        ! CMG *~
    ********************************************************************

    sub "APCPRWFB" (part$,              /* IN  MFG Part Number          */~
                    apcwood_code$,      /* OUT WS/FM code from part #   */~
                    apcwood_descr$,     /* OUT Uncensored APC WOOD descr*/~
                    wsfm_code$,         /* OUT APC WOOD before the "-"  */~
                    wsfm_descr$,        /* OUT APC WOOD  after the "-"  */~
                    instruction$,       /* OUT Manufacturing instruction*/~
                    nbr_this_model$,    /* OUT Qty for pricing (PR2SB)  */~
                    nbr_windows$,       /* OUT Number of windows        */~
                    top_window$,        /* OUT Def of window(s) on top  */~
                                        /*       " ", H, T, S           */~
                    top_nbr_windows$,   /* OUT Number of windows on top */~
                                        /*       " " if no window on top*/~
/* (AWD004) */      nbr_uchannel%,      /* Number of U-channel Clips    */~
/* (AWD004) */      nbr_mclips%,        /* Number of Mull Clips         */~
                    sp_mull%,           /* (AWD003) 0% or 1% for SPEC MULL*/~
                    debug%(),           /* IN  Level (2) - 00=Off,01=On */~
                    #2,                 /* IN  GENCODES File Channel    */~
                    ret_code%,          /* OUT 0% = OK; NOT WS or FM    */~
                                        /*     1% = Wood Surr/Fact Mull */~
                                        /*    >1% = Error               */~
                    ret_descr$)         /* OUT Text descr of ret_code   */
                                        

    dim                                 /* ARGUMENTS                    */~
        apcwood_code$3,                 /* WS/FM code from part number  */~ 
        apcwood_descr$32,               /* APC WOOD description         */~
	      sp_descr$32,                    /* Special Mull Descr (AWD003)  */~
        debug%(2%),                     /* Debug Switches               */~
        gc$32,                          /* GENCODES desciption          */~
        hinge$2,                        /* Hinge code                   */~
        instruction$2,                  /* Manufacturing Instruction    */~
                                        /*    WS, FM, FP, CP, SW, CW    */~
        n(5),                           /* SEARCH Results Array (EWD001)*/~
        nbr_this_model$1,               /* Qty for pricing              */~
        nbr_windows$1,                  /* Number of windows on bottom  */~
        part$25,                        /* MFG Part Number              */~
        ret_descr$50,                   /* Descr text of ret_code%      */~
        table_code$5,                   /* APCPR1SB arg                 */~
        top_window$1,                   /* Definition of window on top  */~
                                        /*  " " = none,   H = half rnd  */~
                                        /*   T  = Transom S = Special   */~
        top_nbr_windows$1,              /* Number of windows on top     */~ 
        wsfm_code$30,                   /* APC WOOD descr before the "-"*/~
        wsfm_descr$30                   /* APC WOOD descr after  the "-"*/

    dim                                 /* DEBUG VARIABLES              */~
        apc$40,                         /* Screen Title                 */~
        inp$79, pfkeys$32,              /* Input prompt                 */~
        ret_descr$50,                   /* Alpha return code            */~
        ret_descrs$(10)50


rem   ********************************************************************
rem   I N I T I A L I Z A T I O N S
rem   ********************************************************************

        init (" ") apcwood_code$, apcwood_descr$, wsfm_code$, wsfm_descr$, ~
                   instruction$, nbr_windows$, top_window$, top_nbr_windows$, ~
                   ret_descr$, sp_descr$

        ret_code% = 99%

        nbr_this_model$  = "0"
        nbr_windows$     = "0"
        top_nbr_windows$ = "0"


     /* Index = ret_code% + 1%      */
        ret_descrs$( 1) = "Not WS/FM: is a Bay/Bow"
        ret_descrs$( 2) = "Not WS/FM: Part number LT 22 chars"
        ret_descrs$( 3) = "Not WS/FM: Cottage-Oriel, part len <> 25"
        ret_descrs$( 4) = "ERR: WS/FM Code not found in APC WOOD table"
        ret_descrs$( 5) = "ERR: APC WOOD Entry: No appropriate dash found"
        ret_descrs$( 6) = "ERR: APC WOOD entry: unable to parse"
        ret_descrs$( 7) = "ERR: Old APC WOOD code"


rem   ****************************************************************
rem   M a i n   L i n e   R o u t i n e
rem   ****************************************************************

rem   If model number is 9xx (Bay/Bow) then part is NOT WS/FM.
        ret_code% = 0% : ret_descr% = 1%
        if str(part$,1%,1%) = "9" then goto EXIT

rem   If part number is shorter than 22 characters then NOT WS/FM.
        ret_code% = 0% : ret_descr% = 2%
        if len(part$) < 22% then goto EXIT

rem   If hinge code >= "70" & <= "95" (ctg-orl) then part must be 25 chars
        ret_code% = 0% : ret_descr% = 3%
        if str(part$,9%,2%) >= "70" and str(part$,9%,2%) <= "95" and   ~
            len(part$) <> 25% then EXIT         /*^EWD001^*/

rem   Now extract the code from the part number...
        if len(part$) = 22% then apcwood_code$ = str(part$,20%,3%)     ~
                            else apcwood_code$ = str(part$,23%,3%)

rem   Make sure not Special Shape                           /* (EWD002) */
        if str(part$,7%,2%) > "99" and apcwood_code$ < "A00" then EXIT

rem   Make sure this is not an old Code
        ret_code% = 2% : ret_descr% = 7%
REM        if apcwood_code$ < "A00" then EXIT               /* (EWD002) */
        if apcwood_code$ < "A00" and str(part$,7%,2%) < "A0" then EXIT


rem   Validate / Get description of code in GENCODES "APC WOOD"
        table1%     = 0%
        table2%     = 9%                    /* GENCODES: "APC WOOD"   */
        table_code$ = apcwood_code$

        call "APCPR1SB" (table1%, table2%, table_code$, apcwood_descr$,  ~
                         dash_pos%, #2, found%)

        ret_code%    = 2% : ret_descr% = 4%
        if found%    = 0% then EXIT
        ret_code%    = 2% : ret_descr% = 5%
        if dash_pos% < 4% then EXIT


rem   Parse this puppy... Assumes "CCC- DDDDDDDD" i.e., blank after dash
        ret_code%   = 2% : ret_descr% = 6%
        wsfm_code$  = str(apcwood_descr$, 1%, dash_pos% - 1%)
        wsfm_descr$ = str(apcwood_descr$, dash_pos% + 2%)


     /* Code should be in the format CC[#][T[#]]                            */
        instruction$ = str(wsfm_code$,1,2)      /* WS, FM, FP, CP, SW, CW   */

     /* If wood surround on half, octagon, or special then only 1 window    */
        if pos("HSO" = str(wsfm_code$,3,1)) = 0% THEN L100 
            nbr_windows$    = "1"
            nbr_windows%    =  1%
            nbr_this_model$ = "1"
            nbr_this_model% =  1%
            goto TOP

     /* 3rd position should be number of windows...                         */         
L100:   nbr_windows$ = str(wsfm_code$,3,1)
        convert nbr_windows$ to nbr_windows%, data goto EXIT

     /* Now figure the nbr of windows for pricing.  Only single hung and    */
     /* double hung can have a count > 1.                                   */

        nbr_this_model$ = "1"
        nbr_this_model% =  1%

        if nbr_windows% = 1% then goto top

        /* Read GENCODES "MODEL" to determine if we're SH or DH.  No -> 1%  */
        table1%     = 0%
        table2%     = 1%                    /* GENCODES: "MODEL"   */
        table_code$ = str(part$,1%,3%)

        call "APCPR1SB" (table1%, table2%, table_code$, gc$, dash_pos%, #2, found%)

        if found% = 0% or dash_pos% < 3% then goto TOP

            gc$ = str(gc$,1%,dash_pos% - 1%)
            gc$ = str(gc$, len(gc$)-1%,2%)
            if gc$ = "SH" or gc$ = "DH" then goto L110
                goto TOP

        /* Still here?  Look at Hinge Code and see if any of these guys are XX */
L110:   hinge$ = str(part$,9%,2%)
        table1%     = 0%                                  /*EWD001 - Begin */
        table2%     = 5%                    /* GENCODES: "HINGE"   */
        table_code$ = hinge$        

        call "APCPR1SB" (table1%, table2%, table_code$, gc$, dash_pos%, #2, found%)
        if found% = 0% or dash_pos% < 4% then goto L120
        gc$ = str(gc$,1%,dash_pos% - 1%)

        cnt% = 1%
        mat n = zer                         /*  TWIN  */
        search gc$ = "XX" to n()
        if n(1) <> 0 then cnt% = 2%
        mat n = zer                         /* TRIPLE */
        search gc$ = "XXX" to n()
        if n(1) <> 0 then cnt% = 3%
        mat n = zer                         /*  QUAD  */
        search gc$ = "XXXX" to n()
        if n(1) <> 0 then cnt% = 4%
        mat n = zer                         /* No 5Xs */
        search gc$ = "XXXXX" to n()
        if n(1) <> 0 then cnt% = 1%
      
 
        nbr_this_model% = cnt%
                                                          /* EWD001 -  End */

L120:   convert nbr_this_model% to nbr_this_model$, pic(#)

TOP: /* See if there is a top and, if so, number of windows on top          */
        if len(wsfm_code$) < 4% then goto DONE
            top_window$ = str(wsfm_code$,4,1)
            top_nbr_windows$ = "1"
            top_nbr_windows% =  1%
            if len(wsfm_code$) < 5% then goto DONE
                convert str(wsfm_code$,5%,1%) to top_nbr_windows%, data goto DONE
                top_nbr_windows$ = str(wsfm_code$,5%,1%)


rem   Made it.  Clean up a bit.
DONE:   ret_code% = 1%
/* (AWD004) u-channel and mull clip changes */
      p% = pos(apcwood_descr$ = "*")
      if p% = 0% then goto bad_mClips

      convert str(apcwood_descr$,(p%+2%),1%) to nbr_uchannel%, data goto bad_uChannel

bad_uChannel:

      convert str(apcwood_descr$,(p%+4%),1%) to nbr_mclips%, data goto bad_mClips

bad_mClips:
/* (\AWD004) */

/* (AWD003) */
rem   Validate / Get description of code in GENCODES "PRICESPEC"
        table1%     = 15%
        table2%     = 0%                    /* GENCODES: "PRICESPEC"  */
        table_code$ = apcwood_code$

        call "APCPR1SB" (table1%, table2%, table_code$, sp_descr$,  ~
                         dash_pos%, #2, found%)

        if found% = 0% then sp_mull% = 0%
	      if found% > 0% then sp_mull% = 1%


rem   Now get out of Dodge on a fast horse...
EXIT:   if ret_code% = 0% then                                          ~
            ret_descr$ = "Part is not Wood Surround or Factory Mull"
        if ret_code% = 1% then                                          ~
            ret_descr$ = "Part is Wood Surround or Factory Mull"
        if ret_code% > 1% then                                          ~
            ret_descr$ = ret_descrs$(ret_descr%)

        gosub display_debug

        end




rem     *********************************************************************
rem     D E B U G   S T U F F
rem     *********************************************************************

rem     *--------------------------*
        display_debug
rem     *--------------------------*
           if debug%(2%) = 0% then return
              
           gosub set_pf1

SCR000:    accept                                                        ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
                                                                         ~
               at (04,02), "Part Number",                                ~
               at (04,30), fac(hex(8c)), part$                  , ch(25),~
               at (04,57), "->",                                         ~
               at (04,60), fac(hex(8c)), wsfm_code$             , ch(03),~
                                                                         ~
               at (06,02), "APC WOOD Code",                              ~
               at (06,30), fac(hex(8c)), apcwood_code$          , ch(20),~
               at (07,02), "         Description",                       ~
               at (07,30), fac(hex(8c)), apcwood_descr$         , ch(40),~
                                                                         ~
               at (09,02), "Mfg Instruction",                            ~
               at (09,30), fac(hex(8c)), instruction$           , ch(02),~
               at (10,02), "Pricing Number of Windows",                  ~
               at (10,30), fac(hex(8c)), nbr_this_model$        , ch(01),~
                                                                         ~
               at (12,02), "Number of Windows",                          ~
               at (12,30), fac(hex(8c)), nbr_windows$           , ch(01),~
                                                                         ~
               at (14,02), "Top Window",                                 ~
               at (14,30), fac(hex(8c)), top_window$            , ch(03),~
                                                                         ~
               at (15,02), "Number of Top Windows",                      ~
               at (15,30), fac(hex(8c)), top_nbr_windows$       , ch(01),~
                                                                         ~
               at (17,02), "Number of U-Channel",                        ~
               at (17,30), fac(hex(8c)), nbr_uchannel%,          pic(0), ~
                                                                         ~
               at (18,02), "Number of M-Clips  ",                        ~
               at (18,30), fac(hex(8c)), nbr_mclips%,            pic(0), ~
                                                                         ~
                                                                         ~
               at (20,02), "Return Code",                                ~
               at (20,30), fac(hex(8c)), ret_code$              , ch(10),~
               at (21,02), "Return Code Descr",                          ~
               at (21,30), fac(hex(8c)), ret_descr$             , ch(50),~
                                                                         ~
               at (24,02), fac(hex(a4)),   inp$                 , ch(79),~
                                                                         ~
                    keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto SCR100
                  call "PRNTSCRN"
                  goto SCR000

SCR100:        return  
               

        set_pf1
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            inp$ = "Press <Return> To Continue, or PF(15) to Print the Screen."
               apc$ = "*Interpret Part for WS/FM Scr -05/15/98*"
               pname$ = "APCPRWFB - Rev: 01.02"
               date$ = date
               call "DATEFMT" (date$)

rem     Set screen variables...
        convert ret_code% to ret_code$, pic(0)
        return




