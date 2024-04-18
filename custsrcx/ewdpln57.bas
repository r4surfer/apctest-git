        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN57                             *~
            *  Creation Date     - 07/31/98                             *~
            *  Last Modified Date- 01/01/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Analysis of Glass Re-Makes turn      *~
            *                      around time. Calc the time from when *~
            *                      glass was scanned as a Re-Make until *~
            *                      it was completed.                    *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/31/98 ! (New) Program                            ! RHH *~
            * 08/10/98 ! Mod add rma_reason$ Skip Codes 26 or     ! RHH *~
            *          !   Greater.                               !     *~
            * 12/16/99 ! (EWD001) Calc re-make load               ! TBM *~
            * 10/12/00 ! (EWD002) Make remake load half hour up to! CMG *~
            *          !    six hours then one hour increments    !     *~
            * 04/18/02 ! (EWD003) Make change to company name     ! TLM *~
            * 04/21/04 ! (EWD004) Mod for received glass panels   ! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mod for sub part          ! CMG *~
            * 08/23/06 ! (AWD005) mod for late percentage         ! CMG *~
            *12/18/2007! (AWD006) mod for average turn.           ! DES *~
            *************************************************************

        dim                                                              ~
            rm_key$33,                   /* Re-Make Glass Record       */~
            rm_time$8,                   /* Time Re-Make Entered       */~
            rm_dte$6,                    /* Date Remake Last Entered   */~
                                         /* (EWD001)                   */~       
            cnt$28,                      /* For Analysis               */~
            title$38,                    /* Analysis Title and Time    */~
            beg_dte$6, beg_date$10,      /* Beg/End Delivery Date      */~
            end_dte$6, end_date$10,      /*                            */~
            x$10,                        /* Date Buffer                */~
            rma_key$18, rma_date$8,      /* Primary Analysis Key       */~
            rma_comp$4,                  /* Completed HHMM             */~
            rma_reason$2,                /* Re-Make Reason code        */~ 
            h1$8,                        /* (EWD001) "Rmk Date"        */~
            h2$5,                        /* "Total"  (EWD002)          */~
            h3$4,                        /* " 0.5"                     */~ 
            h4$4,                        /* " 1.0"                     */~
            h5$4,                        /* " 1.5"                     */~
            h6$4,                        /* " 2.0"                     */~
            h7$4,                        /* " 2.5"                     */~
            h8$4,                        /* " 3.0"                     */~
            h9$4,                        /* " 3.5"                     */~
            h10$4,                       /* " 4.0"                     */~
            h11$4,                       /* " 4.5"                     */~
            h12$4,                       /* " 5.0"                     */~
            h13$8,                       /* "AVG TURN" (AWD006)        */~
            h31$4,                       /* " 5.5"                     */~ 
            h41$4,                       /* " 6.0"                     */~
            h51$4,                       /* " 7.0"                     */~
            h61$4,                       /* " 8.0"                     */~
            h71$4,                       /* " 9.0"                     */~
            h81$4,                       /* "10.0"                     */~
            h91$4,                       /* "11.0"                     */~
            h101$4,                      /* "12.0"                     */~
            h111$4,                      /* "13.0"                     */~
            h121$4,                      /* "+-Hr"     (EWD002)        */~
            h131$8,                      /* " OnTime " (EWD001)        */~
            h141$8,                      /* "Summary "                 */~
            option$18,                   /*                            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            dsp_msg$79,                  /* Screen Display Message     */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim l_date$(50%)8,               /* Re-Make Glass Date Buckets */~
            l_d%(50%,48%), xx%(48%),     /* Re-Make Load Buckets (1-48)*/~
            l_d$(50%,48%)3,              /* Load Bucket Display        */~
            tt%(3%), tt$(3%)4, rhh$8,    /* Daily Shift Totals         */~
            company$30,                  /* Report                     */~
            rpt_time$8,                  /* Report                     */~
            l_r%(50%,20%),               /* re_make Glass Buckets(1-20)*/~
                                         /*   for each date            */~
            l_rt(50%,2%),                /* Tot Product-Dept Ea Load   */~
            l_r$(50%,20%)4,              /* Display Buckets            */~
            l_rt$(50%,2%)8,              /* Display Date Total Buckets */~
            l_sm(23%),                   /* Summary Display Buckets    */~
            l_sm$(23%)8                  /* "          "       "       */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim cnt_line(50), sum_line(50), l_avg$(50)4   /* AWD006   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21 
            apc$   = "(EWD) Analysis of Re-Make Glass    "
            pname$ = "EWDPLN57 - Rev: R7.00"

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
            * #1  ! APCPLNGA ! (New) Glass Remake audit file            *~
            * #2  ! APCPLNGR ! Glass Re-Make File                       *~
            * #3  !          !                                          *~
            * #4  ! GENCODES ! Master Code Table Files                  *~
            * #5  ! APCPLNGV ! Glass Receving File              (EWD004)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNGA",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  1,   keylen = 18,                      ~
                        alt key 1, keypos =  7, keylen = 12

            select #2,  "APCPLNGR",                                      ~
/*PAR000*/              varc,     indexed,  recsize = 384,               ~
                        keypos = 22,   keylen = 12,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33,             ~
                            key 3, keypos = 13, keylen = 21

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

                                                           /*  (EWD004)  */
            select #5,  "APCPLNGV",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  1,   keylen = 18,                      ~
                        alt key 1, keypos =  7, keylen = 12

            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "APCPLNGA" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
                                                            /*  (EWD004)  */
            filename$ = "APCPLNGV" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
REM            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
REM            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
REM            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)            /* (EWD003) */
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            h1$ = "Rmk Date"                /* (EWD002) Begin */
            h2$ = "Total"
            h3$ = " 0.5" 
            h4$ = " 1.0"
            h5$ = " 1.5"
            h6$ = " 2.0"
            h7$ = " 2.5"
            h8$ = " 3.0"
            h9$ = " 3.5"
            h10$= " 4.0" 
            h11$= " 4.5"
            h12$= " 5.0"
            h13$ = "Avg Turn"                /* (AWD006) */
            h31$= " 5.5"
            h41$= " 6.0"
            h51$= " 7.0"
            h61$= " 8.0"
            h71$= " 9.0"
            h81$= "10.0"
            h91$= "11.0"
            h101$="12.0"
            h111$="13.0"                       /* (EWD002) End   */
            h121$= "+-Hr" 
            h131$= " OnTime "
            h141$= "Summary "
                                               /*  xx%( 1%) =  0%  */
            for i% = 1% to 48%                 /*  xx%( 2%) = 60%  */
                xx%(i%) = (i% * 60%) - 60%     /*  xx%( 3%) = 120% */
            next i%                            /*  xx%( 4%) = 180% */
                                               /*  xx%( 5%) = 240% */
                                               /*  Etc.            */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
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

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************
                                                 /* Analyize Scanning  */ 
        process_data                             /* of Re-Make Glass   */
            ff% = 1%                             /*  (EWD004)          */
            if keyhit% = 11% then ff% = 5%       /*  (EWD004)          */
            call "SHOSTAT" ("Analyizing Re-Make Glass")
            init(" ") rma_key$
                               
            tot = 0.0  : tot1 = 0.0
            cnt$ = "Remake Records  [ xxxxxxxx ]" 
            cnt% = 0% : dd_max% = 0%             /* Production Remake  */
            str(rma_key$,1%,6%) = beg_dte$       /* records Beginning  */
                                                 /* at Production Date */
        process_data_ga                          /*   (EWD004)         */
            read #ff%,key > rma_key$, using L19000, rma_key$, rma_comp$,   ~
                                  rma_reason$, eod goto process_data_done
L19000:        FMT CH(18), POS(30), CH(4), POS(37), CH(2)
            cnt% = cnt% + 1%
            if mod(cnt%,25%) <> 0 then goto L19005
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,27);hex(84);cnt$;

L19005:        if str(rma_key$,1%,6%) > end_dte$ then goto Process_data_done
                  rma_reason% = 0% 
                  convert rma_reason$ to rma_reason%, data goto L19008

L19008:           if rma_reason% > 10% then goto process_data_ga
                  init(" ") rma_date$ 
                  rma_date$ = str(rma_key$,1%,6%)
                  call "DATEFMT" (rma_date$)        /* (EWD001)     */
                  if dd_max% = 0% then goto L19010
                  for dd% = 1% to dd_max%
                      if l_date$(dd%) = rma_date$ then goto L19015
                  next dd%
L19010:           dd_max% = dd_max% + 1%
                  dd% = dd_max%
                  l_date$(dd%) = rma_date$

L19015:           gosub analyize_data
                  goto process_data_ga
            
        process_data_done
            call "SHOSTAT" ("Formatting Analysis Data")
            for dd% = 1% to dd_max%
                for kk% = 1% to 20%
                    convert l_r%(dd%,kk%) to l_r$(dd%,kk%), pic(####)
                next kk%

                convert l_rt(dd%,1%) to l_rt$(dd%,1%), pic(#####)
                                             
                x = 0.0
                if l_rt(dd%,1%) = 0 then goto L19020
                   x = round( (l_rt(dd%,2%) / l_rt(dd%,1%)) * 100.0, 4)

L19020:         convert x to str(l_rt$(dd%,2%),1%,7%), pic(###.###)
                str(l_rt$(dd%,2%),8%,1%) = "%"
/* <AWD006> */
	    avg_line = sum_line(dd%) / cnt_line(dd%)
            convert avg_line to l_avg$(dd%), pic(#0.0)
/* </AWD006> */
            next dd%
            convert l_sm(1%) to l_sm$(1%), pic(#####) /* Total Units  */
               
            x = 0.0 : y = 0.0          
            for ii% = 2% to 21%             /* Hourly Buckets (10)    */
                convert l_sm(ii%) to l_sm$(ii%), pic(####)
                                            /* Sum (5) Hrs or Less    */
                                            /* (AWD005)           */
                                            /* bucket 5 is 10 on screen */
                if ii% < 12% then y = y + l_sm(ii%)   /* (EWD002) */
            next ii%
            
            if l_sm(1%) = 0 then goto L19025     /* Total Plant Units */
               x = round( (y / l_sm(1%) ) * 100.0, 4)

L19025:     convert x to l_sm$(22%), pic(###.###)
            str(l_sm$(22%),8%,1%) = "%"

/* <AWD006> */
	    avg_turn = sum_turn / cnt_turn
            convert avg_turn to l_sm$(23), pic(#0.0)
	    str(l_sm$(23),5,4) = " hrs"
/* </AWD006> */

        gosub display_summary
        return

        analyize_data                            /* Calc How Long to  */ 
               hr% = 0% : mm% = 0%               /* Complete Remake   */
               t_hr_mn% = 0%
               convert str(rma_comp$,1%,2%) to hr%, data goto L20020

L20020:        convert str(rma_comp$,3%,2%) to mm%, data goto L20030

L20030:        t_hr_mn% = (hr% * 60%) + mm%      /* Total Minutes to  */
               hh% = 1%                          /* 1st (1) 1/2 Hour */
	       tmp_sum  = hr% + (mm% / 60.0)
	       sum_turn = sum_turn + hr% + (mm% / 60.0)
	       cnt_turn = cnt_turn + 1                
	       sum_line(dd%) = sum_line(dd%) + hr% + mm%/60
	       cnt_line(dd%) = cnt_line(dd%) + 1             
               if t_hr_mn% <= 30% then hh% = 1%  /* (EWD002) Begin */
                                                 /* 2nd (2) 1 Hour    */ 
               if t_hr_mn% > 30% and t_hr_mn% <=  60% then hh% = 2%
                                                 /* 3rd (3) 1 1/2 Hour  */
               if t_hr_mn% > 60% and t_hr_mn% <=  90% then hh% = 3%
                                                 /* 4th (4) 2 Hour  */
               if t_hr_mn% > 90% and t_hr_mn% <= 120% then hh% = 4% 
                                                 /* 5th (5) 2 1/2 Hours */
               if t_hr_mn% > 120% and t_hr_mn% <= 150% then hh% = 5%
                                                 /* 6th (6) 3 Hours */
               if t_hr_mn% > 150% and t_hr_mn% <= 180% then hh% = 6%
                                                 /* 7th (7) 3 1/2 Hours */
               if t_hr_mn% > 180% and t_hr_mn% <= 210% then hh% = 7%
                                                 /* 8th (8) 4 Hours */
               if t_hr_mn% > 210% and t_hr_mn% <= 240% then hh% = 8%
                                                 /* 9th (9) 4 1/2 Hours */
               if t_hr_mn% > 240% and t_hr_mn% <= 270% then hh% = 9%
                                                 /* 10th (10) 5 Hours*/
               if t_hr_mn% > 270% and t_hr_mn% <= 300% then hh% = 10%
                                                 /* 11th (11) 5 1/2 Hours*/
               if t_hr_mn% > 300% and t_hr_mn% <= 330% then hh% = 11%
                                                 /* 12th (12) 6 Hours*/
               if t_hr_mn% > 330% and t_hr_mn% <= 360% then hh% = 12%
                                                 /* 13th (13) 7 Hours*/
               if t_hr_mn% > 360% and t_hr_mn% <= 420% then hh% = 13%
                                                 /* 14th (14) 8 Hours*/
               if t_hr_mn% > 420% and t_hr_mn% <= 480% then hh% = 14%
                                                 /* 15th (15) 9 Hours*/
               if t_hr_mn% > 480% and t_hr_mn% <= 540% then hh% = 15%
                                                 /* 16th (16) 10 Hours*/
               if t_hr_mn% > 540% and t_hr_mn% <= 600% then hh% = 16%
                                                 /* 17th (17) 11 Hours*/
               if t_hr_mn% > 600% and t_hr_mn% <= 660% then hh% = 17%
                                                 /* 18th (18) 12 Hours*/
               if t_hr_mn% > 660% and t_hr_mn% <= 720% then hh% = 18%
                                                 /* 19th (19) 13 Hours*/
               if t_hr_mn% > 720% and t_hr_mn% <= 780% then hh% = 19%
                                                 /* 20th (20) 14+Hours*/  
               if t_hr_mn% > 780% then hh% = 20%  /* (EWD002) End   */

               l_r%(dd%,hh%) = l_r%(dd%,hh%) + 1%
                                                 /* Date Totals        */
               l_rt(dd%,1%) = l_rt(dd%,1%) + 1.0 /* Total remakes for  */
                                                 /* the day            */

                                                 /* Date on-time totals*/
                                                 /* (EWD001) - Key Mod */
                                                 /* (EWD002)  */
                                                 /* (AWD005) - 5 hours */
                                                 /* 11 b/c half hours...*/

               if hh% < 11% then l_rt(dd%,2%) = l_rt(dd%,2%) + 1.0
                                                 /* Total remakes 6 hrs*/

                                                 /* Totals for all Dates*/
               tot = tot + 1.0                   /* Total remakes for  */
                                                 /* Date Run           */

                                                 /* (AWD005) - 5 hours */
                                                 /* 11 b/c half hours...*/
               if hh% < 11% then tot1 = tot1 + 1.0 /* (EWD002)  */
                                                 /* Total remakes for  */
                                                 /* Date run within    */ 
                                                 /* (6) Hour Criteria  */

                                                 /* hh%- 2% thru 21%   */                               
              l_sm(hh% + 1%) = l_sm(hh% + 1%) + 1.0 /*   one(1) hour   */
                                                 /*      Buckets       */
              l_sm(1%)  = l_sm(1%) + 1.0         /* Total Remake Units */

                                                 /* (22%) Plant %      */
        return 
   
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
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Beginning Re-Make Date?                        ",~
         "Enter a Valid Ending Re-Make Date?                           "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_dte$, beg_date$, x$,  ~
                      end_dte$, end_date$, l_r$(), l_rt$(), l_date$(),  ~
                      l_sm$()

            mat l_r% = zer
            mat l_rt = zer
            mat l_sm = zer
/* <AWD006> */
            sum_turn = 0.0
            cnt_turn = 0.0
            mat sum_line = zer 
            mat cnt_line = zer 
/* </AWD006> */
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
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

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
              on fieldnr% gosub L40160,          /* beg_date$          */~
                                L40160,          /* end_date$          */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Beginning Re-Make Date:",                    ~
               at (03,25), fac(lfac$(1%)), beg_date$            , ch(10),~
                                                                         ~
               at (04,02), "Ending Re-Make Date    :",                   ~
               at (04,25), fac(lfac$(2%)), end_date$            , ch(10),~
                                                                         ~   
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 10% then goto L40200
                  gosub process_data
                  goto L40190

L40200:        if keyhit% <> 11% then goto L40300
                  gosub process_data
                  goto L40190
                                                /* (EWD001)           */     
L40300:        if keyhit% <> 14% then goto L40400
                  gosub remake_load_analysis
                  goto L40190
                                                /* (EWD001)           */
L40400:        if keyhit% <> 15 then goto L40410
                  call "PRNTSCRN"
                  goto L40190

L40410:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(2),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
                                      /*  (EWD004)               */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (14)Process Load"
            pf$(2) = "                                        " &        ~
                     "(11) Received Panels   (15)Print Screen"
            pf$(3) = "                 (10)Process Data       " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0a0bffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_summary
L41000:     gosub set_pf2   
            accept                                                       ~
               at (01,02), fac(hex(84)), option$                , ch(18),~
               at (01,23), fac(hex(a4)), title$                 , ch(38),~
               at (01,65), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
                                                                         ~
               at (03,02), fac(hex(a4))  , h1$                  , ch(08),~
               at (03,11), fac(hex(a4))  , h2$                  , ch(05),~
               at (03,17), fac(hex(a4))  , h3$                  , ch(04),~
               at (03,22), fac(hex(a4))  , h4$                  , ch(04),~
               at (03,27), fac(hex(a4))  , h5$                  , ch(04),~
               at (03,32), fac(hex(a4))  , h6$                  , ch(04),~
               at (03,37), fac(hex(a4))  , h7$                  , ch(04),~
               at (03,42), fac(hex(a4))  , h8$                  , ch(04),~      
               at (03,47), fac(hex(a4))  , h9$                  , ch(04),~
               at (03,52), fac(hex(a4))  , h10$                 , ch(04),~
               at (03,57), fac(hex(a4))  , h11$                 , ch(04),~
               at (03,62), fac(hex(a4))  , h12$                 , ch(04),~
               at (03,70), fac(hex(a4))  , h13$                 , ch(08),~
                                                                         ~
               at (04,02), fac(hex(84))  , l_date$(k% + 1%)     , ch(08),~
               at (04,11), fac(hex(84))  , l_rt$(k% + 1%,1%)    , ch(05),~
               at (04,17), fac(hex(84))  , l_r$(k% + 1%, 1%)    , ch(04),~
               at (04,22), fac(hex(84))  , l_r$(k% + 1%, 2%)    , ch(04),~
               at (04,27), fac(hex(84))  , l_r$(k% + 1%, 3%)    , ch(04),~
               at (04,32), fac(hex(84))  , l_r$(k% + 1%, 4%)    , ch(04),~
               at (04,37), fac(hex(84))  , l_r$(k% + 1%, 5%)    , ch(04),~
               at (04,42), fac(hex(84))  , l_r$(k% + 1%, 6%)    , ch(04),~
               at (04,47), fac(hex(84))  , l_r$(k% + 1%, 7%)    , ch(04),~
               at (04,52), fac(hex(84))  , l_r$(k% + 1%, 8%)    , ch(04),~
               at (04,57), fac(hex(84))  , l_r$(k% + 1%, 9%)    , ch(04),~
               at (04,62), fac(hex(84))  , l_r$(k% + 1%, 10%)   , ch(04),~
               at (04,72), fac(hex(84))  , l_avg$(k% + 1%)      , ch(04),~
                                                                         ~
               at (05,17), fac(hex(a4))  , h31$                 , ch(04),~
               at (05,22), fac(hex(a4))  , h41$                 , ch(04),~
               at (05,27), fac(hex(a4))  , h51$                 , ch(04),~
               at (05,32), fac(hex(a4))  , h61$                 , ch(04),~
               at (05,37), fac(hex(a4))  , h71$                 , ch(04),~
               at (05,42), fac(hex(a4))  , h81$                 , ch(04),~      
               at (05,47), fac(hex(a4))  , h91$                 , ch(04),~
               at (05,52), fac(hex(a4))  , h101$                , ch(04),~
               at (05,57), fac(hex(a4))  , h111$                , ch(04),~
               at (05,62), fac(hex(a4))  , h121$                , ch(04),~
               at (05,70), fac(hex(84))  , h131$                , ch(08),~      
                                                                         ~
               at (06,17), fac(hex(84))  , l_r$(k% + 1%, 11%)    , ch(04),~
               at (06,22), fac(hex(84))  , l_r$(k% + 1%, 12%)    , ch(04),~
               at (06,27), fac(hex(84))  , l_r$(k% + 1%, 13%)    , ch(04),~
               at (06,32), fac(hex(84))  , l_r$(k% + 1%, 14%)    , ch(04),~
               at (06,37), fac(hex(84))  , l_r$(k% + 1%, 15%)    , ch(04),~
               at (06,42), fac(hex(84))  , l_r$(k% + 1%, 16%)    , ch(04),~
               at (06,47), fac(hex(84))  , l_r$(k% + 1%, 17%)    , ch(04),~
               at (06,52), fac(hex(84))  , l_r$(k% + 1%, 18%)    , ch(04),~
               at (06,57), fac(hex(84))  , l_r$(k% + 1%, 19%)    , ch(04),~
               at (06,62), fac(hex(84))  , l_r$(k% + 1%, 20%)    , ch(04),~
               at (06,70), fac(hex(84))  , l_rt$(k% + 1%,2%)    , ch(08),~      
                                                                         ~
               at (07,02), fac(hex(a4))  , h1$                  , ch(08),~
               at (07,11), fac(hex(a4))  , h2$                  , ch(05),~
               at (07,17), fac(hex(a4))  , h3$                  , ch(04),~
               at (07,22), fac(hex(a4))  , h4$                  , ch(04),~
               at (07,27), fac(hex(a4))  , h5$                  , ch(04),~
               at (07,32), fac(hex(a4))  , h6$                  , ch(04),~
               at (07,37), fac(hex(a4))  , h7$                  , ch(04),~
               at (07,42), fac(hex(a4))  , h8$                  , ch(04),~      
               at (07,47), fac(hex(a4))  , h9$                  , ch(04),~
               at (07,52), fac(hex(a4))  , h10$                 , ch(04),~
               at (07,57), fac(hex(a4))  , h11$                 , ch(04),~
               at (07,62), fac(hex(a4))  , h12$                 , ch(04),~
               at (07,70), fac(hex(a4))  , h13$                 , ch(08),~
                                                                         ~
               at (08,02), fac(hex(84))  , l_date$(k% + 2%)     , ch(08),~
               at (08,11), fac(hex(84))  , l_rt$(k% + 2%,1%)    , ch(05),~
               at (08,17), fac(hex(84))  , l_r$(k% + 2%, 1%)    , ch(04),~
               at (08,22), fac(hex(84))  , l_r$(k% + 2%, 2%)    , ch(04),~
               at (08,27), fac(hex(84))  , l_r$(k% + 2%, 3%)    , ch(04),~
               at (08,32), fac(hex(84))  , l_r$(k% + 2%, 4%)    , ch(04),~
               at (08,37), fac(hex(84))  , l_r$(k% + 2%, 5%)    , ch(04),~
               at (08,42), fac(hex(84))  , l_r$(k% + 2%, 6%)    , ch(04),~
               at (08,47), fac(hex(84))  , l_r$(k% + 2%, 7%)    , ch(04),~
               at (08,52), fac(hex(84))  , l_r$(k% + 2%, 8%)    , ch(04),~
               at (08,57), fac(hex(84))  , l_r$(k% + 2%, 9%)    , ch(04),~
               at (08,62), fac(hex(84))  , l_r$(k% + 2%, 10%)   , ch(04),~
               at (08,72), fac(hex(84))  , l_avg$(k% + 2%)      , ch(04),~
                                                                         ~
               at (09,17), fac(hex(a4))  , h31$                 , ch(04),~
               at (09,22), fac(hex(a4))  , h41$                 , ch(04),~
               at (09,27), fac(hex(a4))  , h51$                 , ch(04),~
               at (09,32), fac(hex(a4))  , h61$                 , ch(04),~
               at (09,37), fac(hex(a4))  , h71$                 , ch(04),~
               at (09,42), fac(hex(a4))  , h81$                 , ch(04),~      
               at (09,47), fac(hex(a4))  , h91$                 , ch(04),~
               at (09,52), fac(hex(a4))  , h101$                , ch(04),~
               at (09,57), fac(hex(a4))  , h111$                , ch(04),~
               at (09,62), fac(hex(a4))  , h121$                , ch(04),~
               at (09,70), fac(hex(84))  , h131$                , ch(08),~      
                                                                         ~
               at (10,17), fac(hex(84))  , l_r$(k% + 2%, 11%)    , ch(04),~
               at (10,22), fac(hex(84))  , l_r$(k% + 2%, 12%)    , ch(04),~
               at (10,27), fac(hex(84))  , l_r$(k% + 2%, 13%)    , ch(04),~
               at (10,32), fac(hex(84))  , l_r$(k% + 2%, 14%)    , ch(04),~
               at (10,37), fac(hex(84))  , l_r$(k% + 2%, 15%)    , ch(04),~
               at (10,42), fac(hex(84))  , l_r$(k% + 2%, 16%)    , ch(04),~
               at (10,47), fac(hex(84))  , l_r$(k% + 2%, 17%)    , ch(04),~
               at (10,52), fac(hex(84))  , l_r$(k% + 2%, 18%)    , ch(04),~
               at (10,57), fac(hex(84))  , l_r$(k% + 2%, 19%)    , ch(04),~
               at (10,62), fac(hex(84))  , l_r$(k% + 2%, 20%)    , ch(04),~
               at (10,70), fac(hex(84))  , l_rt$(k% + 2%, 2%)    , ch(08),~      
                                                                         ~
               at (11,02), fac(hex(a4))  , h1$                  , ch(08),~
               at (11,11), fac(hex(a4))  , h2$                  , ch(05),~
               at (11,17), fac(hex(a4))  , h3$                  , ch(04),~
               at (11,22), fac(hex(a4))  , h4$                  , ch(04),~
               at (11,27), fac(hex(a4))  , h5$                  , ch(04),~
               at (11,32), fac(hex(a4))  , h6$                  , ch(04),~
               at (11,37), fac(hex(a4))  , h7$                  , ch(04),~
               at (11,42), fac(hex(a4))  , h8$                  , ch(04),~      
               at (11,47), fac(hex(a4))  , h9$                  , ch(04),~
               at (11,52), fac(hex(a4))  , h10$                 , ch(04),~
               at (11,57), fac(hex(a4))  , h11$                 , ch(04),~
               at (11,62), fac(hex(a4))  , h12$                 , ch(04),~
               at (11,70), fac(hex(a4))  , h13$                 , ch(08),~
                                                                         ~
               at (12,02), fac(hex(84))  , l_date$(k% + 3%)     , ch(08),~
               at (12,11), fac(hex(84))  , l_rt$(k% + 3%,1%)    , ch(05),~
               at (12,17), fac(hex(84))  , l_r$(k% + 3%, 1%)    , ch(04),~
               at (12,22), fac(hex(84))  , l_r$(k% + 3%, 2%)    , ch(04),~
               at (12,27), fac(hex(84))  , l_r$(k% + 3%, 3%)    , ch(04),~
               at (12,32), fac(hex(84))  , l_r$(k% + 3%, 4%)    , ch(04),~
               at (12,37), fac(hex(84))  , l_r$(k% + 3%, 5%)    , ch(04),~
               at (12,42), fac(hex(84))  , l_r$(k% + 3%, 6%)    , ch(04),~
               at (12,47), fac(hex(84))  , l_r$(k% + 3%, 7%)    , ch(04),~
               at (12,52), fac(hex(84))  , l_r$(k% + 3%, 8%)    , ch(04),~
               at (12,57), fac(hex(84))  , l_r$(k% + 3%, 9%)    , ch(04),~
               at (12,62), fac(hex(84))  , l_r$(k% + 3%, 10%)   , ch(04),~
               at (12,72), fac(hex(84))  , l_avg$(k% + 3%)      , ch(04),~
                                                                         ~
               at (13,17), fac(hex(a4))  , h31$                 , ch(04),~
               at (13,22), fac(hex(a4))  , h41$                 , ch(04),~
               at (13,27), fac(hex(a4))  , h51$                 , ch(04),~
               at (13,32), fac(hex(a4))  , h61$                 , ch(04),~
               at (13,37), fac(hex(a4))  , h71$                 , ch(04),~
               at (13,42), fac(hex(a4))  , h81$                 , ch(04),~      
               at (13,47), fac(hex(a4))  , h91$                 , ch(04),~
               at (13,52), fac(hex(a4))  , h101$                , ch(04),~
               at (13,57), fac(hex(a4))  , h111$                , ch(04),~
               at (13,62), fac(hex(a4))  , h121$                , ch(04),~
               at (13,70), fac(hex(84))  , h131$                , ch(08),~      
                                                                         ~
               at (14,17), fac(hex(84))  , l_r$(k% + 3%, 11%)    , ch(04),~
               at (14,22), fac(hex(84))  , l_r$(k% + 3%, 12%)    , ch(04),~
               at (14,27), fac(hex(84))  , l_r$(k% + 3%, 13%)    , ch(04),~
               at (14,32), fac(hex(84))  , l_r$(k% + 3%, 14%)    , ch(04),~
               at (14,37), fac(hex(84))  , l_r$(k% + 3%, 15%)    , ch(04),~
               at (14,42), fac(hex(84))  , l_r$(k% + 3%, 16%)    , ch(04),~
               at (14,47), fac(hex(84))  , l_r$(k% + 3%, 17%)    , ch(04),~
               at (14,52), fac(hex(84))  , l_r$(k% + 3%, 18%)    , ch(04),~
               at (14,57), fac(hex(84))  , l_r$(k% + 3%, 19%)    , ch(04),~
               at (14,62), fac(hex(84))  , l_r$(k% + 3%, 20%)    , ch(04),~
               at (14,70), fac(hex(84))  , l_rt$(k% + 3%, 2%)    , ch(08),~      
                                                                         ~
                                                                         ~
               at (15,02), fac(hex(a4))  , h1$                  , ch(08),~
               at (15,11), fac(hex(a4))  , h2$                  , ch(05),~
               at (15,17), fac(hex(a4))  , h3$                  , ch(04),~
               at (15,22), fac(hex(a4))  , h4$                  , ch(04),~
               at (15,27), fac(hex(a4))  , h5$                  , ch(04),~
               at (15,32), fac(hex(a4))  , h6$                  , ch(04),~
               at (15,37), fac(hex(a4))  , h7$                  , ch(04),~
               at (15,42), fac(hex(a4))  , h8$                  , ch(04),~      
               at (15,47), fac(hex(a4))  , h9$                  , ch(04),~
               at (15,52), fac(hex(a4))  , h10$                 , ch(04),~
               at (15,57), fac(hex(a4))  , h11$                 , ch(04),~
               at (15,62), fac(hex(a4))  , h12$                 , ch(04),~
               at (15,70), fac(hex(a4))  , h13$                 , ch(08),~
                                                                         ~
               at (16,02), fac(hex(84))  , l_date$(k% + 4)     , ch(08),~
               at (16,11), fac(hex(84))  , l_rt$(k% + 4%,1%)    , ch(05),~
               at (16,17), fac(hex(84))  , l_r$(k% + 4%, 1%)    , ch(04),~
               at (16,22), fac(hex(84))  , l_r$(k% + 4%, 2%)    , ch(04),~
               at (16,27), fac(hex(84))  , l_r$(k% + 4%, 3%)    , ch(04),~
               at (16,32), fac(hex(84))  , l_r$(k% + 4%, 4%)    , ch(04),~
               at (16,37), fac(hex(84))  , l_r$(k% + 4%, 5%)    , ch(04),~
               at (16,42), fac(hex(84))  , l_r$(k% + 4%, 6%)    , ch(04),~
               at (16,47), fac(hex(84))  , l_r$(k% + 4%, 7%)    , ch(04),~
               at (16,52), fac(hex(84))  , l_r$(k% + 4%, 8%)    , ch(04),~
               at (16,57), fac(hex(84))  , l_r$(k% + 4%, 9%)    , ch(04),~
               at (16,62), fac(hex(84))  , l_r$(k% + 4%, 10%)   , ch(04),~
               at (16,72), fac(hex(84))  , l_avg$(k% + 4%)      , ch(04),~
                                                                         ~
               at (17,17), fac(hex(a4))  , h31$                 , ch(04),~
               at (17,22), fac(hex(a4))  , h41$                 , ch(04),~
               at (17,27), fac(hex(a4))  , h51$                 , ch(04),~
               at (17,32), fac(hex(a4))  , h61$                 , ch(04),~
               at (17,37), fac(hex(a4))  , h71$                 , ch(04),~
               at (17,42), fac(hex(a4))  , h81$                 , ch(04),~      
               at (17,47), fac(hex(a4))  , h91$                 , ch(04),~
               at (17,52), fac(hex(a4))  , h101$                , ch(04),~
               at (17,57), fac(hex(a4))  , h111$                , ch(04),~
               at (17,62), fac(hex(a4))  , h121$                , ch(04),~
               at (17,70), fac(hex(84))  , h131$                , ch(08),~      
                                                                         ~
               at (18,17), fac(hex(84))  , l_r$(k% + 4%, 11%)    , ch(04),~
               at (18,22), fac(hex(84))  , l_r$(k% + 4%, 12%)    , ch(04),~
               at (18,27), fac(hex(84))  , l_r$(k% + 4%, 13%)    , ch(04),~
               at (18,32), fac(hex(84))  , l_r$(k% + 4%, 14%)    , ch(04),~
               at (18,37), fac(hex(84))  , l_r$(k% + 4%, 15%)    , ch(04),~
               at (18,42), fac(hex(84))  , l_r$(k% + 4%, 16%)    , ch(04),~
               at (18,47), fac(hex(84))  , l_r$(k% + 4%, 17%)    , ch(04),~
               at (18,52), fac(hex(84))  , l_r$(k% + 4%, 18%)    , ch(04),~
               at (18,57), fac(hex(84))  , l_r$(k% + 4%, 19%)    , ch(04),~
               at (18,62), fac(hex(84))  , l_r$(k% + 4%, 20%)    , ch(04),~
               at (18,70), fac(hex(84))  , l_rt$(k% + 4%, 2%)    , ch(08),~      
                                                                         ~
                                                                         ~
               at (19,02), fac(hex(a4))  , h141$                , ch(08),~
               at (19,11), fac(hex(a4))  , l_sm$(1%)            , ch(05),~
               at (19,17), fac(hex(a4))  , l_sm$(2%)            , ch(04),~
               at (19,22), fac(hex(a4))  , l_sm$(3%)            , ch(04),~
               at (19,27), fac(hex(a4))  , l_sm$(4%)            , ch(04),~
               at (19,32), fac(hex(a4))  , l_sm$(5%)            , ch(04),~
               at (19,37), fac(hex(a4))  , l_sm$(6%)            , ch(04),~
               at (19,42), fac(hex(a4))  , l_sm$(7%)            , ch(04),~
               at (19,47), fac(hex(a4))  , l_sm$(8%)            , ch(04),~
               at (19,52), fac(hex(a4))  , l_sm$(9%)            , ch(04),~
               at (19,57), fac(hex(a4))  , l_sm$(10%)           , ch(04),~
               at (19,62), fac(hex(a4))  , l_sm$(11%)           , ch(04),~
               at (19,70), fac(hex(a4))  , l_sm$(23%)           , ch(08),~      
                                                                         ~
               at (20,17), fac(hex(a4))  , l_sm$(12%)           , ch(04),~
               at (20,22), fac(hex(a4))  , l_sm$(13%)           , ch(04),~
               at (20,27), fac(hex(a4))  , l_sm$(14%)           , ch(04),~
               at (20,32), fac(hex(a4))  , l_sm$(15%)           , ch(04),~
               at (20,37), fac(hex(a4))  , l_sm$(16%)           , ch(04),~
               at (20,42), fac(hex(a4))  , l_sm$(17%)           , ch(04),~
               at (20,47), fac(hex(a4))  , l_sm$(18%)           , ch(04),~
               at (20,52), fac(hex(a4))  , l_sm$(19%)           , ch(04),~
               at (20,57), fac(hex(a4))  , l_sm$(20%)           , ch(04),~
               at (20,62), fac(hex(a4))  , l_sm$(21%)           , ch(04),~
               at (20,70), fac(hex(a4))  , l_sm$(22%)           , ch(08),~      
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L41040             /* First    */
L41020:           k% = 0%
                  goto L41000

L41040:        if keyhit% <> 3% then goto L41080             /* Last      */
L41060:           x% = int(val_max% / 4%)
                  k% = (x%*4%)
                  goto L41000

L41080:        if keyhit% <> 4% then goto L41100             /* Previous */
                  if k% < 5% then goto L41020
                  k% = k% - 4%
                  if k% <= 1% then goto L41020
                  goto L41000

L41100:        if keyhit% <> 5% then goto L41120             /* Next     */
                  k% = k% + 4%
                  if k% < val_max% then goto L41000
                  goto L41060

L41120:        if keyhit% <> 15 then goto L41140
                  call "PRNTSCRN"
                  goto L41000

L41140:        if keyhit% <> 16% then goto L41000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode

        set_pf2
            rhh = 0.0             /* (EWD002) Change dsp_msg */
            dsp_msg$=               /* (AWD005)  */                               ~
              "Total Plant <5 Hr or Less> Re_Make Pcnt [xxxxxxx%] Re_Make Units [xxxxxx]"
            if tot > 0 then                                                           ~
               rhh = round((tot1 / tot) * 100, 4)
            convert rhh to str(dsp_msg$,42%,7%), pic(###.###)
            convert tot to str(dsp_msg$,67%,6%), pic(######)
                                      
            option$ = "[Re-Make Analysis]"
            title$ = "(Analysis) Display of Re-Makes by Date"
            pageno$ = "Page: XX of XX"             /* k% = Array Values */

            val_max% = dd_max%
            if val_max% > (50% - 4%) then val_max% = 50% - 4%
                                                        /* Display Max */
            yy% = ( val_max% / 4% ) + 1%
            xx% = (k% / 4%) + 1%
            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */

            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "
            pf$(2) = "(3)Last                                 " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous                             " &        ~
                     "                       (16)Exit Display" 
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            gosub check_screen
            return

        check_screen
            if val_max% > 4% then goto L41160
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41160:      if k% >= 4% then goto L41180
                gosub no_first
                gosub no_prev
L41180:      if (k% + 4%) <= val_max% then goto L41200
                gosub no_last
L41200:      if k% <= (val_max% - 4%) then goto L41220
                gosub no_next
L41220: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50170,                 /* Beg Date     */  ~
                              L50260,                 /* End Date     */
            return

L50170: REM Beginning Remake Date                    BEG_DTE$, BEG_DATE$
            if beg_date$ <> " " then goto L50180
               beg_date$ = date
 
L50180:     date% = 0%
            call "DATEOKC" (beg_date$, date%, errormsg$)
            if date% = 0% then goto L50190
            x$ = beg_date$
            call "DATUFMTC"(x$)
            beg_dte$ = str(x$,1%,6%)
        return
L50190:     init(" ") beg_dte$, beg_date$, x$
            errormsg$ = "(Error) Invalid Begining Re-make Date?"
            gosub error_prompt
        return 

L50260: REM Ending Re-Make Date                      END_DTE$, END_DATE$
            if end_date$ <> " " then goto L50300
               end_date$ = beg_date$
L50300:     date% = 0%
            call "DATEOKC" (end_date$, date%, errormsg$)
            if date% = 0% then goto L50310
            x$ = end_date$
            call "DATUFMTC"(x$)
            end_dte$ = str(x$,1%,6%)
            if end_dte$ < beg_dte$ then goto L50310
        return
L50310:     init(" ") end_dte$, end_date$, x$
            errormsg$ = "(Error) Invalid Ending Re-make Date?"
            gosub error_prompt
        return


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                                      /* Report Header */
L55000: %!########@########                                    ##########~
        ~#####################                                  EWDPLN57:!

L55010: %!User Id: ###                                       ############~
        ~##################                                   Page: #####!

L55020: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L55030: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55040: %!Date:######## 00:00-07:59(####)! ### ! ### ! ### ! ### ! ### ! ~
        ~### ! ### ! ### ! ### ! ### ! ### ! ### ! ### ! ### ! ### ! ### !

L55050: %!              08:00-15:59(####)! ### ! ### ! ### ! ### ! ### ! ~
        ~### ! ### ! ### ! ### ! ### ! ### ! ### ! ### ! ### ! ### ! ### !

L55060: %!              16:00-24:00(####)! ### ! ### ! ### ! ### ! ### ! ~
        ~### ! ### ! ### ! ### ! ### ! ### ! ### ! ### ! ### ! ### ! ### !


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

                                                   /* (EWD001) - Begin */
        remake_load_analysis
            call "SHOSTAT" ("Analyizing Re-Make Glass")

            cnt% = 0% : dd_max% = 0%
            init(" ") rm_key$, l_date$(), l_d$(), cnt$
            cnt$ = "Glass Rec's Checked [xxxxxx]"
            mat l_d% = zer
            rm_key$ = all(hex(00))
            str(rm_key$,1%,6%) = beg_dte$          /* Set Start Date   */
        remake_load_anal_nxt
            read #2,key 2% > rm_key$, using L60000 , rm_key$, rm_time$,  ~
                                  rm_dte$, eod goto remake_load_anal_done
L60000:        FMT CH(33), POS(44), CH(8), CH(6)
            cnt% = cnt% + 1%
            if mod(cnt%,500%) <> 0 then goto L60005
               convert cnt% to str(cnt$,22%,6%), pic(######)
               print at(02,27);hex(84);cnt$;

L60005:        if str(rm_key$,1%,6%) > end_dte$ then                        ~
                                              goto remake_load_anal_done
               rm_num$ = str(rm_key$,32%,2%)
               rm_num% = 0%
               convert rm_num$ to rm_num%, data goto L60010
L60010:
               if rm_num% < 1% then goto remake_load_anal_nxt
                                              /* Only Process Remakes */
                  init(" ") rma_date$ 
                  rma_date$ = rm_dte$         /* Date of Remake       */
                  call "DATEFMT" (rma_date$)          /* (EWD001)     */
                  if dd_max% = 0% then goto L60020
                  for dd% = 1% to dd_max%
                      if l_date$(dd%) = rma_date$ then goto L60030
                  next dd%
L60020:           dd_max% = dd_max% + 1%
                  if dd_max% > 50% then dd_max% = 50%
                  dd% = dd_max%
                  l_date$(dd%) = rma_date$
                                                   /* Set Month     */
L60030:           hr% = 0% : mn% = 0% : ss% = 0%   /* bucket        */
                                                   /* 24 Hour Clock */   
                  convert str(rm_time$,1%,2%) to hr%, data goto L60040
L60040:                                            /* Hours         */
 
                  convert str(rm_time$,3%,2%) to mn%, data goto L60050
L60050:                                            /* Minutes       */
 
                  convert str(rm_time$,5%,2%) to ss%, data goto L60060
L60060:                                            /* Seconds       */
                                          
                  if ss% > 30% then mn% = mn% + 1% /* Add (1) Min   */
                                                   /* Round Up      */
                  tot_min% = (60% * hr%) + mn%

                  for slot% = 1% to 48%
                      if tot_min% >= xx%(slot%) then goto L60070
                         goto L60080
L60070:           next slot%
                  slot% = 49%
L60080:           slot% = slot% - 1% 
                                                     /* (1) to (48) */
                  l_d%(dd%,slot%) = l_d%(dd%,slot%) + 1%

                  goto remake_load_anal_nxt
        remake_load_anal_done
            if dd_max% = 0% then goto L60100 
            for i% = 1% to dd_max%
                for j% = 1% to 48%
                    convert l_d%(i%,J%) to l_d$(i%,j%), pic(###)
                next j%
            next i%
            gosub print_report
         
L60100: return clear all
        goto inputmode

        prt_header
            if lcnt% <> 99% then print using L55020
            init(" ") rpt_time$
            call "TIME" (rpt_time$)
            print page
            page_no% = page_no% + 1%
            print using L55020
            print using L55000, date$, rpt_time$, company$
            print using L55010, userid$, title$, page_no%
            lcnt% = 3%
        return

        prt_detail
            if lcnt% > 55% then gosub prt_header
               mat tt% = zer
               for j% = 1% to 48%
                   if j% <= 16% then tt%(1%) = tt%(1%) + l_d%(i%,j%)
                   if j% >= 17% and j% <= 32% then tt%(2%) = tt%(2%) + l_d%(i%,j%)
                   if j% >= 33% then tt%(3%) = tt%(3%) + l_d%(i%,j%)
               next j%

               for j% = 1% to 3%
                   convert tt%(j%) to tt$(j%), pic(####)
           
               next j%

               print using L55030
               print using L55040, l_date$(i%), tt$(1%), l_d$(i%,1%), l_d$(i%,2%),~
                                   l_d$(i%,3%), l_d$(i%,4%), l_d$(i%,5%),~ 
                                   l_d$(i%,6%), l_d$(i%,7%), l_d$(i%,8%),~
                                 l_d$(i%,9%), l_d$(i%,10%), l_d$(i%,11%),~
                                 l_d$(i%,12%), l_d$(i%,13%), l_d$(i%,14%),~
                                 l_d$(i%,15%), l_d$(i%,16%)

               print using L55050, tt$(2%), l_d$(i%,17%), l_d$(i%,18%), l_d$(i%,19%),~
                                 l_d$(i%,20%), l_d$(i%,21%), l_d$(i%,22%), ~ 
                                 l_d$(i%,23%), l_d$(i%,24%), l_d$(i%,25%), ~
                                 l_d$(i%,26%), l_d$(i%,27%), l_d$(i%,28%), ~
                                 l_d$(i%,29%), l_d$(i%,30%), l_d$(i%,31%), ~
                                 l_d$(i%,32%)

               print using L55060, tt$(3%), l_d$(i%,33%), l_d$(i%,34%), l_d$(i%,35%),~
                                 l_d$(i%,36%), l_d$(i%,37%), l_d$(i%,38%), ~ 
                                 l_d$(i%,39%), l_d$(i%,40%), l_d$(i%,41%), ~
                                 l_d$(i%,42%), l_d$(i%,43%), l_d$(i%,44%), ~
                                 l_d$(i%,45%), l_d$(i%,46%), l_d$(i%,47%), ~
                                 l_d$(i%,48%)

               lcnt% = lcnt% + 4%
        return

        print_report
            call "SHOSTAT" ("Creating Report")

REM            company$ = "Ellison Windows and Doors"
            title$ = " Glass Re-Make Load Analysis  "
            lcnt% = 99% : page_no% = 0%
            call "SETPRNT" ("EWDLOAD", " ", 0%, 0%)
            select printer (134)

            for i% = 1% to dd_max%
                gosub prt_detail
            next i%

            print using L55020
            call "SETPRNT" ("EWDLOAD", " ", 0%, 1%)
        return

                                                                           
        open_error                                                            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
        
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end



