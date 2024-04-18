*       ****************************************************************~
*                            ( R e p o r t s )                         *~
*                                                                      *~
*                           ( As of 08/01/93 - RHH )                   *~
*        APCCUTMM - MATERIAL REQUIREMENT REPORT FOR THE SPECIFIED      *~
*                   PRODUCTION PERIOD.                                 *~
*                                                                      *~
*       ****************************************************************~
*       03/31/98  ERN  Y2K modifications                               *~
*       ****************************************************************

*       NOTE: Dates are passed in unformatted, therefore packed

        sub "APCCUTMM" (scr_dte1$,       /* Starting Production Date  */ ~
                        scr_dte2$,       /* Ending Production Date    */ ~
                        scr_prod$,       /* Product Line              */ ~
                        scrap,           /* SCRAP PERCENTAGE          */ ~
                        k_max%,          /* Total Number of Extrusions*/ ~
                        ext$(),          /* Raw Material Part Numbers */ ~
                        units(),         /* Raw Material Total Units  */ ~
                        lngth$(),        /* Extrusion Length an Type  */ ~
                        #2,              /* (GENCODES) TABLE FILE     */ ~
                        #3,              /* (HNYMASTR) Part Master    */ ~
                        #4 )             /* (HNYQUAN ) QUANTITIES     */

        dim scr_dte1$10,                 /* Beginning Prod Date        */~
            scr_dte2$10,                 /* Ending Prod Date           */~
            scr_prod$1,                  /* Product Line for Report    */~
            readkey$50,                  /* Code Table Key             */~
            ext$(300)10,                 /* Raw Material Part Number   */~
            units(300),                  /* Raw Material Total Units   */~
            lngth$(300)5,                /* Raw Material Length / Type */~
            lngth$4,                     /* EXTRUSION LENGTH           */~
            scrap$7, rhh$7,              /* SCRAP PERCENTAGE           */~
            units$12,                    /* TOTAL UNITS (INCH) - SCRAP */~
            inv_on_hand$12,              /* HNYQUAN Value for Part     */~
            t_units$12,                  /* CALC REQUIRED QTY NEEDED   */~
            dtl_desc$32,                 /*                            */~
            dtl_part$25,                 /* Part Number                */~
            dept$32,                     /* Department Description     */~
            date$8,                      /* CURRENT DATE               */~
            rpt_time$8,                  /* CURRENT TIME               */~
            scr_msg$40                   /* MESSAGE/TITLE              */

            scr_msg$ = "    Materials Requirements Report For   "
            call "SHOSTAT" ("Printing Materials Report ... ")
            date$ = date
            call "DATEFMT" (date$)
            call "TIME" (rpt_time$)

            call "DATFMTC" (scr_dte1$)
            call "DATFMTC" (scr_dte2$)
            convert scrap to scrap$, pic(###.##-)

            scrap = round(scrap / 100.0, 4)

            readkey$ = all(hex(00))
            readkey$ = "DEPTCODE " & scr_prod$
            call "DESCRIBE" (#2, readkey$, dept$, 0%, x%)
            if x% = 0% then dept$ = "          UNDEFINED             "

            pageno% = 0% : lcnt% = 99%
            call "SETPRNT" ("APCC", " ", 0%, 0%)
            select printer (134)

            for i% = 1% to k_max%
                dtl_part$ = ext$(i%)
                gosub lookup_scrap
                if rhh = 0 then rhh = scrap

                lngth$    = str(lngth$(i%),1%,4%)
                lngth = 1.0                        /* Extrusion Length */
                convert lngth$ to lngth, data goto L00700
L00700:
                x = units(i%) * rhh                /* Calc Scrap Units */
                units = round( units(i%) + x, 4)   /* Add in Scrap */

                typ$ = str(lngth$(i%),5%,1%)
                t_units = units                   /* Default -(I)nches */
                if typ$ = "F" then t_units = (units / 12.0 )   /* Feet */
                units = round(t_units, 2)         /* Display for Feet  */
                convert units to units$, pic(####,###.##-)
                                                  /* Convert Extrusion */
                t_units = round( t_units / lngth, 2)   /* length       */
                convert t_units to t_units$, pic(####,###.##-)

                gosub calc_hnyquan_data
                gosub detail_a
            next i%

            print using L01080
            call "SETPRNT" ("APCC", " ", 0%, 1%)
        goto exit_program                          /* Finished         */


L00920: %!Raw Material!<--------- Description -------->!!LNGT!I/F!Total  ~
        ~Feet !Piece's Needed!!! Feet On_Hand !<- Issued ->!<--- Net --->!
L00940: %!------------!--------------------------------!!----!---!-------~
        ~-----!--------------!!!--------------!------------!-------------!
L00960: %! ########## !################################!!####! # !#######~
        ~#####! ############ !!! ############ !            !             !


        detail_a                                       /* C1$(), C2$() */
             if lcnt% > 58% then gosub header
             print using L00940
             print using L00960   , dtl_part$, dtl_desc$, lngth$, typ$,     ~
                                 units$, t_units$, inv_on_hand$
             lcnt% = lcnt% + 2%
        return

L01080: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L01110: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L01140: %! ######## @ ########                         ##################~
        ~######################                               Page: #### !

L01170: %! Begining Production Day: ##########                           ~
        ~                                                                !

L01200: %! Ending Production Day  : ##########  Scrap Percentage:####### ~
        ~                                                                !

L01230: %!                                                 ##############~
        ~##################                                              !


        header
          if lcnt% <> 99% then print using L01080
          pageno% = pageno% + 1%
          print page
          print using L01080
          print using L01140  , date$, rpt_time$, scr_msg$, pageno%
          print using L01170  , scr_dte1$
          print using L01200  , scr_dte2$, scrap$
          print using L01230  , dept$
          print using L01110
          print using L00920
          lcnt% = 7%
        return

        calc_hnyquan_data
          readkey$ = all(hex(00)) : inv_on_hand = 0.0
          str(readkey$,1%,25%) = dtl_part$
          read #4,key > readkey$, using L01490   , readkey$, on_hand,      ~
                                                          eod goto L01530
          goto L01500
        calc_next
          read #4, using L01490   , readkey$, on_hand
L01490:       FMT POS(17), CH(44), POS(69), PD(14,4)
L01500:   if str(readkey$,1%,25%) <> dtl_part$ then goto L01530
             inv_on_hand = round(inv_on_hand + on_hand, 2)
             goto calc_next
L01530:   convert inv_on_hand to inv_on_hand$, pic(####,###.##-)
          gosub get_description
        return

        get_description
           read #3,key = dtl_part$, using L01600   , dtl_desc$,            ~
                                                         eod goto L01620
L01600:        FMT POS(26), CH(32)
        return
L01620:    dtl_desc$ = " "
        return

        lookup_scrap
           rhh = 0.0
           readkey$ = " "
           str(readkey$,1%,9%)   = "MRP SCRAP"
           str(readkey$,10%,15%) = ext$(i%)
           read #2,key = readkey$, using L01710, rhh$, eod goto L01740
L01710:       FMT POS(25), CH(7)
           convert rhh$ to rhh, data goto L01740

L01740: return

        exit_program

        end

