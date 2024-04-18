        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPR1SB                             *~
            *  Creation Date     - 12/06/93                             *~
            *  Last Modified Date- 06/26/06                             *~
            *  Description       - This Subroutine if for Looking Up    *~
            *                      Table Entries For both the Price     *~
            *                      System and the MFG Part.             *~
            *  Tables for Pricing- (PRICE 000) - Master Catalog Table   *~
            *                      (PRICE 001) - Catalog Method Table   *~
            *                      (PRICE 002) - Std/Spc Reference Codes*~
            *                      (PRICE 003) - Std/Spc Calc Codes     *~
            *                      (PRICE 004) - Field Descriptions     *~
            *                      (PRICE 005) - Field Definitions      *~
            *                      (PRICE 006) - Price/Value Formats    *~
            *                      (PRICE 007) - Price/Value Calc Method*~
            *                      (PRICE 008) - Key Calculation Method *~
            *                      (PRICE 009) - Key Unit Conversion    *~
            *                      (PRICE 010) - Special Calc. Product  *~
            *                      (PRICE 011) - Prc Cross-Reference Cod*~
            *                      (PRICE 012) - Factory Mull Pricing   *~
            *                      (PRICE 015) - Debug Display On/Off   *~
	    *                      (PRICESPEC) - Specialty Pricing Trip *~
            *  Tables for MFG    - (MODEL    ) - Model Code Table       *~
            *                      (COLOR    ) - Color Code Table       *~
            *                      (GLASS    ) - Glass Code Table       *~
            *                      (LITING   ) - Liting Code Table      *~
            *                      (HINGE    ) - Hinge Code Table       *~
            *                      (SCREEN   ) - Screen Code Table      *~
            *                      (LOCKS    ) - Locks Code Table       *~
            *                      (PRICECUST) - Catalog - Method Calc  *~
            *                      (APC WOOD ) - Prc Cross-Reference Cod*~
            *                      (PRICE 025) - WJB Casement STD 4 9/16*~
            *                      (PRICE 026) - WJB Casement CST 4 9/16*~
            *                      (PRICE 027) - WJB Casement STD 6 9/16*~
            *                      (PRICE 028) - WJB Casement CST 6 9/16*~
            *                      (PRICE 029) - WJB Multiple STD 4 9/16*~
            *                      (PRICE 030) - WJB Multiple CST 4 9/16*~
            *                      (PRICE 031) - WJB Multiple STD 6 9/16*~
            *                      (PRICE 032) - WJB Multiple CST 6 9/16*~
            *  Special Comments  -                                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/04/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 03/31/97 ! Mod to Update for Current Pricing        ! RHH *~
            * 10/31/97 ! Check for Upgrade to R6.04.03            ! RHH *~
            * 11/01/99 ! (EWD001) Mods for Wood Jamb              ! RHH *~ 
            * 06/26/06 ! (AWD002) Mods for speicality triple mull ! CMG *~ 
            *************************************************************

            sub "APCPR1SB" ( tab_1%,     /* Non-Zero Lookup Price Codes*/~
                             tab_2%,     /* Non_Zero Lookup MFG Codes  */~
                             tab_val$,   /* Table Lookup 'Value'       */~
                             tab_desc$,  /* Code Description           */~
                             p%,         /* Position of 1st '-'        */~
                             #1,         /* GENCODES - File            */~
                             tab_rec% )  /* 0% = No, 1% = Found        */

        dim                              /* (GENCODES) Code Tables   . */~
            readkey$24,                  /* Code Table Lookup Key      */~
            sav_key$9,                   /* Code Table name            */~
            tab_val$5, tab_desc$32       /* Code Value and Description */
                                         /* (EWD001) Wood Jamb         */
        if tab_2% < 21% then goto L00500
           tab_2% = tab_2% - 11%         /* (EWD001) Start with 10 - 13*/
           tab_1% = 0%
                                         /* (EWD001)                   */ 
L00500: if tab_2% = 0% then goto L00580     /* Get Value from Code Desc*/

           on tab_2% gosub t_model, t_color, t_glass, t_liting, t_hinge, ~
                           t_screen, t_locks, t_cat_method, t_wood_fact, ~
                           t_wood1, t_wood2, t_wood3, t_wood4, t_wood5,  ~
                           t_wood6, t_wood7, t_wood8

                                         /* (EWD001) New Routines      */
              goto exit_program
L00580: on tab_1% gosub t_1, t_2, t_3, t_4, t_5, t_6, t_7, t_8, t_9,t_10,~
                        t_11, t_12, t_13, t_14, t_15 /* (AWD002) */
           goto exit_program

        t_10                               /* Lookup Master Catalog    */
            sav_key$ = "PRICE 000"
            goto L01300
        t_1                                /* Lookup Catalog Method    */
            sav_key$ = "PRICE 001"
            goto L01300
        t_2                                /* Lookup Reference Code Def*/
            sav_key$ = "PRICE 002"
            goto L01300
        t_3                                /* Lookup Std/Spc Ref Calc  */
            sav_key$ = "PRICE 003"
            goto L01300
        t_4                                /* Lookup Field Descriptions*/
            sav_key$ = "PRICE 004"
            goto L01300
        t_5                                /* Lookup Field Definitions */
            sav_key$ = "PRICE 005"
            goto L01300
        t_6                                /* Lookup Price/Value Format*/
            sav_key$ = "PRICE 006"
            goto L01300
        t_7                                /* Lookup Price/Value Calc  */
            sav_key$ = "PRICE 007"
            goto L01300
        t_8                                /* Lookup (Key) Calc Method */
            sav_key$ = "PRICE 008"
            goto L01300
        t_9                                /* Lookup (Key) Unit Convert*/
            sav_key$ = "PRICE 009"
            goto L01300
        t_11                               /* Lookup Special Calc Prod.*/
            sav_key$ = "PRICE 010"
            goto L01300
        t_12                               /* Price Cross-Ref. Codes   */
            sav_key$ = "PRICE 011"
            goto L01300
        t_13                               /* Factory Mulls Pricing    */
            sav_key$ = "PRICE 012"
            goto L01300
        t_14                               /* Debug Display On/Off     */
            sav_key$ = "PRICE 015"
            goto L01300
        t_15                               /* Debug Display On/Off     */
	    sav_key$ = "PRICESPEC"         /* Special Triple Mulls     */
            goto L01300                    /* (AWD002)                 */
        t_model                            /* Lookup MODEL CODE   (1)  */
            sav_key$ = "MODEL    "
            goto L01300
        t_color                            /* Lookup COLOR CODE   (2)  */
            sav_key$ = "COLOR    "
            goto L01300
        t_glass                            /* Lookup GLASS CODE   (3)  */
            sav_key$ = "GLASS    "
            goto L01300
        t_liting                           /* Lookup LITING CODE  (4)  */
            sav_key$ = "LITING   "
            goto L01300
        t_hinge                            /* Lookup HINGE CODE   (5)  */
            sav_key$ = "HINGE    "
            goto L01300
        t_screen                           /* Lookup SCREEN CODE  (6)  */
            sav_key$ = "SCREEN   "
            goto L01300
        t_locks                            /* Lookup LOCKS CODE   (7)  */
            sav_key$ = "LOCKS    "
            goto L01300
        t_cat_method                       /* Lookup LOCKS CODE   (8)  */
            sav_key$ = "PRICECUST"
            goto L01300
        t_wood_fact
            sav_key$ = "APC WOOD "         /* Wood/Factory Mull Codes  */
            goto L01300
                                           /* (EWD001)                 */
        t_wood1                            /* WJB Casement STD  4 9/16 */
            sav_key$ = "PRICE 025"
            goto L01300
        t_wood2                            /* WJB Casement CST  4 9/16 */
            sav_key$ = "PRICE 026"
            goto L01300
        t_wood3                            /* WJB Casement STD  6 9/16 */
            sav_key$ = "PRICE 027"
            goto L01300
        t_wood4                            /* WJB Casement CST  6 9/16 */
            sav_key$ = "PRICE 028"
            goto L01300
        t_wood5                            /* WJB Multiple STD  4 9/16 */
            sav_key$ = "PRICE 029"
            goto L01300
        t_wood6                            /* WJB Multiple CST  4 9/16 */
            sav_key$ = "PRICE 030"
            goto L01300
        t_wood7                            /* WJB Multiple STD  6 9/16 */
            sav_key$ = "PRICE 031"
            goto L01300
        t_wood8                            /* WJB Multiple CST  6 9/16 */
            sav_key$ = "PRICE 032"
            goto L01300
                                           /* (EWD001)                 */
L01300:     tab_rec% = 0%
            init(" ") tab_desc$, readkey$
            str(readkey$,1%,9%)   = sav_key$
            str(readkey$,10%,15%) = tab_val$
            read #1,key = readkey$, using L01350 , tab_desc$,eod goto L01380
L01350:       FMT POS(25), CH(32)
            p% = pos(tab_desc$ = "-")
            tab_rec% = 1%
L01380: return

        exit_program
        end

