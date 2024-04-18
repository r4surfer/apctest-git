        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCCST0B                             *~
            *  Creation Date     - 07/21/95                             *~
            *  Last Modified Date- 10/31/05                             *~
            *  Description       - Subroutine to Calculate the Cost and *~
            *                      Return all Material and Pricing      *~
            *                      Information.                         *~
            *                                                           *~
            *  Special Note      - PF(9) Key from Calling Programs Sets *~
            *                      CALC% flag = 99%, to Display the     *~
            *                      Debug Screen Upon Exiting.           *~
            *                                                           *~
            *  Where Used        - APCCST05, APCCST06, APCCST08,        *~
            *                      APCCST09, EWDSLS00, APCSALES,        *~
            *                      APCSALE1                             *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *      Routines - (APCCST5B) - Sub to Calculate the Direct  *~
            *           OLD - (APCCSTLB)   and Indirect Labor Cost      *~
            *                              Assoc. with a MFG Part.      *~
            *                     Tables - 'CAPACITY ', 'COST 01LB'     *~
            *                              'COST 02LB', 'COST 03LB'     *~
            *                              'COST 04LB', 'COST 05LB'     *~
            *                                                           *~
            *                 (APCCST1B) - Sub to Calculate the Primary,*~
            *           OLD - (APCCSTRM)   Misc., and Grid/Liting       *~
            *                              Material Cost Associated     *~
            *                              with a MFG Part. Scrap Cost  *~
            *                              is also Calculated.          *~
            *           OLD - (APCCSTSB)   (APCCST9B - Used )           *~
            *                     Tables - 'COST 03LT', 'COST 01SP'     *~
            *                              'COST 01PD' = Liting Pads    *~
            *                                                           *~
            *                 (APCCST9B) - Calc the Unit Cost Assoc.    *~
            *                              with a Raw Mat'l Part No.    *~
            *                                                           *~
            *                 (APCCST2B) - Calc the Glass and Screen    *~
            *           OLD - (APCCSTGS)   Cost of Mat'ls for a MFG     *~
            *                              Part. Scrap Cost also Calc.  *~
            *                     Tables - 'COST 01GM', 'COST 01GS'     *~
            *                              'COST 02SC', 'COST 01SP'     *~
            *                                                           *~
            *                 (APCCST3B) - Calc the Cost of Locks and   *~
            *           OLD - (APCCSTKK)   Keeper Rail for MFG Part     *~
            *                     Tables - 'COST 06LK'                  *~
            *                               Note - Lock and Keeper Raw  *~
            *                                      Material             *~
            *                            - 'COST 07LK'                  *~
            *                               Note - Lock and Keeper      *~
            *                                      Screws               *~
            *                                                           *~
            *                 (APCCST4B) - Calc. the Cost of Hardware   *~
            *           OLD - (APCCSTHH)   and Packaging. When doing    *~
            *                              Hardware also Calc the Cost  *~
            *                              of Balcance Tubes.           *~
            *                     Tables - 'COST 05BL'                  *~
            *                                                           *~
            *           NEW - (APCCST6B) - New Costmate Utility for     *~
            *                              Exceptions                   *~
            *                                                           *~
            *                                                           *~
            *===========================================================*~
            *                                                           *~
            *  LAB()- Assoc. labor Cost      TC()- Assoc. Mat'l Cost    *~
            *   1% = Dir Prod Line Labor      1% = Mat'l Vinyl          *~
            *   2% = Dir Glass labor          2% = Mat'l Misc           *~
            *   3% = Dir Screen Labor         3% = Mat'l Total          *~
            *   4% = Ind Mat'l Labor          4% = Glass Mat'l Vinyl    *~
            *   5% = Ind Staging Labor        5% = Glass Mat'l Misc     *~
            *   6% = Ind Loading Labor        6% = Glass Mat'l Total    *~
            *   7% = Std Indirect Labor       7% = Scrn Mat'l Vinyl     *~
            *   8% = Total Dir + Ind          8% = Scrn Mat'l Misc      *~
            *   9% = Tot Labor Overhead       9% = Scrn Mat'l Total     *~
            *  10% = Total Labor             10% = Lock Mat'l Vinyl     *~
            *                                11% = Lock Mat'l Misc      *~
            *  AVG_PAY() - Avg Pay           12% = Lock Mat'l Total     *~
            *  UPH() - Unit per Manhour      13% = Hardware Mat'l Vinyl *~
            *    1% = Mfg Direct             14% = Hardware Mat'l Misc  *~
            *    4% = Glass Direct           15% = Hardware Mat'l Total *~
            *    5% = Screen Direct          16% = Packaging Mat'l Vinyl*~
            *    7% = Material Indirect      17% = Packaging Mat'l Misc *~
            *    8% = Staging Indirect       18% = Packaging Mat'l Total*~
            *    9% = Loading Indirect       19% = Total Freight Amount *~
            *                                20% = Total Vinyl Discount *~
            *                                21% = Total Cost Wood Surr *~
            *===========================================================*~
            *                                                           *~
            *  RM_MAT()- Mat'l Cost          RM_MATS()- Mat'l Scrap Cost*~
            *    1% = Vinyl Mat'l Cost         1% = Vinyl Mat'l Scrap   *~
            *    2% = Misc Mat'l Cost          2% = Misc Mat'l Scrap    *~
            *    3% = Total Mat'l Cost         3% = Total Mat'l Scrap   *~
            *    4% = Vinyl Glass Cost         4% = Vinyl Glass Scrap   *~
            *    5% = Misc Glass Cost          5% = Misc Glass Scrap    *~
            *    6% = Total Glass Cost         6% = Total Glass Scrap   *~
            *    7% = Vinyl Scrn Cost          7% = Vinyl Scrn Cost     *~
            *    8% = Misc Scrn Cost           8% = Misc Scrn Cost      *~
            *    9% = Total Scrn Cost          9% = Total Scrn Cost     *~
            *   10% = Vinyl Lock Cost                                   *~
            *   11% = Misc Lock Cost                                    *~
            *   12% = Total Lock Cost                                   *~
            *   13% = Vinyl Hdr Cost                                    *~
            *   14% = Misc Hdr Cost                                     *~
            *   15% = Total Hdr Cost                                    *~
            *   16% = Vinyl Pck Cost                                    *~
            *   17% = Misc Pck Cost                                     *~
            *   18% = Total Pck Cost                                    *~
            *===========================================================*~
            *                                                           *~
            *  TT()- Cost Total Buckets      Type Values                *~
            *    1% = Total Vinyl Cost         1% = Inv Raw Materials   *~
            *    2% = Total Vinyl Adj Cost     2% = Glass Materials     *~
            *    3% = Total Glass Cost         3% = Screen Materials    *~
            *    4% = Total Screen Cost        4% = Lock(s) Materials   *~
            *    5% = Total Hardware Cost      5% = Hardware Materials  *~
            *    6% = Total Packaging Cost     6% = Packaging Materials *~
            *    7% = Total Misc Cost          7% = ( Available )       *~
            *    8% = Total Material Cost                               *~
            *    9% = Total Materail Adj                                *~
            *   10% = Total Product Cost                                *~
            *                                                           *~
            *===========================================================*~
            *                                                           *~
            *   ? = Type Value ( 1 thru 7 )                             *~
            *  ?? = Number of Entries ( 1 thru 50 )                     *~
            *                                                           *~
            *  TMP$(?%,??%)                 Store all Raw Mat'l Part No *~
            *  TMC(?%,??%)                  Assoc. Cut Inches In Dec.   *~
            *  TMCT(?%,??%)                 Assoc. Total Cost Raw Mat'l *~
            *  TMU%(?%,??%)                 Assoc. Unit of Measure      *~
            *  TMD$(?%,??%)                 Assoc. Raw Mat'l Desc       *~
            *  TMUC(?%,??%)                 Assoc. Raw Mat'l Unit Cost  *~
            *  TMSI(?%,??%),                Assoc. Scrap Inches Decimal *~
            *  TMSC(?%,??%),                Assoc. Scrap Mat'l Cost     *~
            *  TMEQ$(?%,??%),               Assoc. Calc Type Eq. No.    *~
            *  TMPH$(?%,??%),               Assoc. Phantom Designature  *~
            *  TCNT%(?%),                   Assoc. Count for Each Type  *~
            *                                                           *~
            *===========================================================*~
            *                                                           *~
            *   FF%() - File Channels                                   *~
            *    1% = (APCCUTEQ)            10% = (APCCSTLR)            *~
            *    2% = (HNYMASTR)            11% = (CPRPRICE)            *~
            *    3% = (HNYQUAN )            12% = (CUSTOMER)            *~
            *    4% = (GENCDSIN)            13% = (APCPCMST)            *~
            *    5% = (AMTBOMCD)            14% = (APCSKUNO)            *~
            *    6% =                       15% = (APCPCMSK)            *~
            *    7% = (APCEMPLY)            16% = (APCPCMSD)            *~
            *    8% = (APCEQUAT)            19% = (APCPLNDP)            *~
            *    9% = (APCCSTHP)                                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/21/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 09/15/95 ! Mod to Calc Freight Cost. TC(19%) and the! RHH *~
            *          !    Total Vinyl Discount Amount. TC(20%). !     *~
            * 10/02/95 ! Mod to Calc Wood Surround and Factory    ! RHH *~
            *          !    Mull Cost TC(21%). Also Turn on       !     *~
            *          !    Costmate Utility.                     !     *~
            * 06/01/96 ! Mod to Utility for New Debug Subroutine. ! RHH *~
            *          !    'APCPLNDD'                            !     *~
            * 11/08/97 ! Mod to Labor Subroutine (APCCST5B) add . ! RHH *~
            *          !    another file (APCPLNDP)               !     *~
            * 03/27/98 ! Y2K COMPLIANT                            ! DJD *~
            * 10/31/05 ! (AWD001) CR347 Mod for Sub Part          ! CMG *~
            *************************************************************

            sub "APCCST0B" ( calc%,      /* Calculation Method         */~
                             part$,      /* MFG Part Number            */~
                             partno1$,   /* MFG Sub Part Number        */~
                             adj_mat,    /* Cost Adjustment Dollars    */~
                             tmp$(),     /* Raw Mat'l Part Numbers     */~
                             tmc(),      /* Raw Mat'l Cut Inches in Dec*/~
                             tmct(),     /* Raw Mat'l Costs            */~
                             tmu%(),     /* Raw Mat'l Calc Unit of Meas*/~
                             tmd$(),     /* Raw Mat'l Descriptions     */~
                             tmuc(),     /* Raw Mat'l Unit Cost        */~
                             tmsi(),     /* Raw Mat'l Scrap Inches Dec */~
                             tmsc(),     /* Raw Mat'l Scrap Cost       */~
                             tmeq$(),    /* Calc Type and Equations    */~
                             tmph$(),    /* Phantom Designature        */~
                             tcnt%(),    /* Raw Mat'l Type Counts      */~
                             lb_typ$,    /* Labor Type (A) or (S)tand  */~
                             lab(),      /* Labor Costs (1 thru 10)    */~
                             avg_pay(),  /* Avg Pay per Dept           */~
                             uph(),      /* Avg Units Per Manhour Dept */~
                             tc(),       /* Material Costs (1 thru 20) */~
                             tt(),       /* Total Cost Buckets         */~
                             rm_mat(),   /* Material Costs (1 thru 10) */~
                             rm_mats(),  /* Mat'l Scrap Costs(1 thru 9)*/~
                             cuscode$,   /* Customer Code for Pricing  */~
                             pc(),       /* 35 Prices                  */~
                             price,      /* Calc. Price for customer   */~
                             #1,         /*   (APCCUTEQ)               */~
                             #2,         /*   (HNYMASTR)               */~
                             #3,         /*   (HNYQUAN )               */~
                             #4,         /*   (GENCDSIN)               */~
                             #5,         /*   (AMTBOMCD)               */~
                             #7,         /*   (APCEMPLY)               */~
                             #8,         /*   (APCEQUAT)               */~
                             #9,         /*   (APCCSTHP)               */~
                             #10,        /*   (APCCSTLR)               */~
                             #11,        /*   (CPRPRICE)               */~
                             #12,        /*   (CUSTOMER)               */~
                             #13,        /*   (APCPCMST)               */~
                             #22,        /*   (AWDPCMST)               */~
                             #14,        /*   (APCSKUNO)               */~
                             #15,        /*   (APCPCMSK)               */~
                             #16,        /*   (APCPCMSD)               */~
                             #17,        /*   (APCCSTEX)               */~
                             #18,        /*   (APCSTOCK)               */~
                             #19,        /*   (APCPLNDP)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */

        dim                              /* Costing Variables          */~
            part$25,                     /* MFG Part Number            */~
            partno1$20,                  /* SUb part number 1 (AWD001) */~
            tmp$(7%,50%)25,              /* Store all Raw Mat'l Part No*/~
            tmc(7%,50%),                 /* Assoc. Cut Inches In Dec.  */~
            tmct(7%,50%),                /* Assoc. Total Cost Raw Mat'l*/~
            tmu%(7%,50%),                /* Assoc. Unit of Measure     */~
            tmd$(7%,50%)32,              /* Assoc. Raw Mat'l Desc      */~
            tmuc(7%,50%),                /* Assoc. Raw Mat'l Unit Cost */~
            tmsi(7%,50%),                /* Assoc. Scrap Inches Decimal*/~
            tmsc(7%,50%),                /* Assoc. Scrap Mat'l Cost    */~
            tmeq$(7%,50%)3,              /* Assoc. Calc Typ and Eq No. */~
            tmph$(7%,50%)5,              /* Assoc. Phantom Designature */~
            tcnt%(7%),                   /* Assoc. Count for Each Type */~
            lb_typ$1,                    /* LAB TYPE (S)tandard (A)ctua*/~
            lab(10%),                    /* Breakdown of Labor Cost    */~
            avg_pay(15%),                /* Avg Pay By Dept            */~
            uph(15%),                    /* Avg Unit Per Manhour Dept  */~
            tc(25%),                     /* Total Cost's               */~
            tt(25%),                     /* Cost Total Buckets         */~
            rm_mat(20%),                 /* Total Vinyl,Misc, Mat      */~
            rm_mats(20%),                /* Total Vinyl,Misc, Mat Scrap*/~
            apc_err%(20%),               /* Store Error Code each Modul*/~
            rm_raw$(100%)14,             /* Inv. Raw Materil Part No.  */~
            rm_part$(100%)10,            /* Inventory Raw Mat No.      */~
            rm_cuts(100%),               /* Total Inches in Decimal    */~
            rm_cost(100%),               /* Total Cost Raw Material    */~
            rm_desc$(100%)32,            /* Raw Material Description   */~
            rm_unit(100%),               /* Raw Mat'l Unit Cost        */~
            rm_cuts_s(100%),             /* Total Inch Desc. Scrap     */~
            rm_cost_s(100%),             /* Total Cost Raw Mat Scrap   */~
            rm_eq$(100%)3,               /* Calc Type and Equation No. */~
            rm_ph$(100%)5,               /* Phantom Designatures       */~
            gs_qty(9%),                  /* G/S Material Units         */~
            gs_raw$(9%)25,               /* G/S Material Parts         */~
            gs_desc$(9%)32,              /* G/S Material Description   */~
            gs_cost(9%),                 /* Raw Materal Cost           */~
            gs_units%(100%),             /* Raw Material Unit Measure  */~
            gs_qty_s(9%),                /* G/S Material Scrap         */~
            gs_cost_s(9%),               /* G/S Material Cost Scrap    */~
            cst_typ$1,                   /* 0 = Hardware, 1 = Package  */~
            wood_code$3,                 /* Wood Surround Code         */~
            readkey$24,                  /* Gencodes Key               */~
            value$10,                    /* Wood Surround Cost         */~
            area$1                       /* Costmate Assign Area       */

        dim                              /* Pricing Variables          */~
            size$1, sel$2,               /* (O)pening, (E)xact         */~
            pc(36%),                     /* Calc Dealer Price Catalog  */~
            upd$1,                       /* Update Prices Y or N       */~
            cuscode$9,                   /* Customer Code              */~
            ref$(30%)2,                  /* Ref Type Codes Catalog     */~
            ref1$(30%)2,                 /* Ref Type Codes Spec. Cat.  */~
            ref_p(30%),                  /* Ref Prices APC CAtalog     */~
            ref_p1(30%)                  /* Ref Prices Spec. Cat.      */

        REM - Main Line
              mat tmc     = zer : mat tmct    = zer : mat tmu%     = zer
              mat tmsi    = zer : mat tmsc    = zer : mat tcnt%    = zer
              mat lab     = zer : mat tc      = zer : mat tt       = zer
              mat rm_mat  = zer : mat rm_mats = zer : mat apc_err% = zer
              mat tmuc    = zer : mat avg_pay = zer : mat uph      = zer

              init (" ") tmp$(), tmd$(), tmeq$(), tmph$()
              gosub calc_cost
              gosub calc_wood_surround
              gosub calc_price
              goto sub_exit

        calc_cost
           debug% = 0%
           if calc% < 99% then goto L02820   /* From Calling Program     */
              calc% = 0% : debug% = 1%

L02820:    if calc% = 0% or calc% = 1% then gosub calc_labor
                                            /* LAB(8%) - DIRECT/IND    */
                                            /* LAB(9%) - OVERHEAD      */
           if calc% = 0% or calc% = 2% then gosub calc_material
                                            /* (1) - TC(1%) - Vinyl    */
                                            /* (2) - TC(2%) - Misc     */
                                            /* (3) - TC(3%) - Total    */
           typ% = 0%
           if calc% = 0% or calc% = 3% then gosub calc_glass
                                            /* (1) - TC(4%) - Vinyl Gls*/
                                            /* (2) - TC(5%) - Misc  Gls*/
                                            /* (3) - TC(6%) - Total Gls*/
           typ% = 1%
           if calc% = 0% or calc% = 4% then gosub calc_screen
                                            /* (1) - TC(7%) - Vinyl Scr*/
                                            /* (2) - TC(8%) - Misc  Scr*/
                                            /* (3) - TC(9%) - Total Scr*/
           if calc% = 0% or calc% = 5% then gosub calc_locks
                                            /* (1) - TC(10%) - Vinyl Lk*/
                                            /* (2) - TC(11%) - Misc  Lk*/
                                            /* (3) - TC(12%) - Total Lk*/
           cst_typ$ = "0"
           if calc% = 0% or calc% = 6% then gosub calc_hardware
                                            /* (1) - TC(13%) - Vinyl Hd*/
                                            /* (2) - TC(14%) - Misc  Hd*/
                                            /* (3) - TC(15%) - Total Hd*/
           cst_typ$ = "1"
           if calc% = 0% or calc% = 7% then gosub calc_hardware
                                            /* (1) - TC(16%) - Vinyl Pk*/
                                            /* (2) - TC(17%) - Misc  Pk*/
                                            /* (3) - TC(18%) - Total Pk*/
        REM - Total Vinyl Cost
            tt(1%) = round( tc(1%) + tc(4%) + tc(7%) + tc(10%) + tc(13%) ~
                            + tc(16%), 4)
        REM - Total Vinyl Adjusted Cost        TC(20%) Vinyl Discount Amt
            tt(2%) = round(tt(1%) - tc(20%), 4)
        REM - Total Glass Cost
            tt(3%) = round( tc(5%), 4)
        REM - Total Screen Cost
            tt(4%) = round( tc(8%), 4)
        REM Total Hardware Cost
            tt(5%) = round( tc(11%) + tc(14%), 4)
        REM Total Packaging Cost
            tt(6%) = round( tc(17%), 4 )
        REM Total Misc Cost
            tt(7%) = round( tc(2%), 4)
        REM Total Material Cost
            tt(8%) = round(tt(2%) + tt(3%) + tt(4%) + tt(5%) + tt(6%) +  ~
                           tt(7%), 2)
                                /* LAB(8%) = Total Direct/Indirect Lab */
                                /* LAB(9%) = Total Over Head           */
        REM Total Cost of Material with Adjustment
           tt(9%) = round(tt(8%) + adj_mat, 2)

        REM Total Cost of Product
           tt(10%) = round(tt(9%) + lab(8%) + lab(9%), 2)
                                            /* Note - TC(19) Freight   */
                                            /*      - TC(20) V Disc    */
                                            /*      - TC(21) W/F Mull  */
                                            /*  Not Included in TT(10) */

        return

        calc_labor
           err% = 0%
           call "APCCST5B" (part$,            /* MFG PART NUMBER       */~
                            lb_typ$,          /* A=Actual,S=Standard   */~
                            lab(1%),          /* Direct Labor Prod Line*/~
                            lab(2%),          /* Direct   Glass Labor  */~
                            lab(3%),          /* Direct   Screen Labor */~
                            lab(4%),          /* Indirect Material Lab */~
                            lab(5%),          /* Indirect Staging Labor*/~
                            lab(6%),          /* Indirect Loading Labor*/~
                            lab(7%),          /**Standard Indirec Labor*/~
                            lab(8%),          /**Total of Direct+Indire*/~
                            lab(9%),          /* Labor Overhead        */~
                            lab(10%),         /* Total Labor           */~
                            avg_pay(),        /* Avg Pay by Dept       */~
                            uph(),            /* Avg UPH by Dept       */~
                            #7,               /* (APCEMPLY) - FILE     */~
                            #4,               /* (GENCODES) - FILE     */~
                            #10,              /* (APCCSTLR) - FILE     */~
                            #19,              /* (APCPLNDP) - FILE     */~
                            err%     )        /* 0% = OK               */
           if err% <> 0% then apc_err%(1%) = 1%         /* Labor Error */
        return

        calc_material
           err% = 0%
           call "APCCST1B" (part$,            /* MFG PART NUMBER       */~
                            rm_part$(),       /* Inv Raw Mat'l Part No.*/~
                            rm_cuts(),        /* Total Inches in Decima*/~
                            rm_cost(),        /* Total Cost Raw Mat'l  */~
                            rm_desc$(),       /* Raw Mat'l Description */~
                            gs_units%(),      /* UNITS OF MEASURE      */~
                            rm_unit(),        /* Raw Material Unit Cost*/~
                            rm_mat(1%),       /* Vinyl Material Cost   */~
                            rm_mat(2%),       /* Misc Material Cost    */~
                            rm_mat(3%),       /* Total Material Cost   */~
                            rm_cnt%,          /* Raw Material Count    */~
                            rm_cuts_s(),      /* Total Inch Decima SCRA*/~
                            rm_cost_s(),      /* Total Cst Raw Mat SCRA*/~
                            rm_mats(1%),      /* Vinyl Mat'l Cost Scrap*/~
                            rm_mats(2%),      /* Misc Mat'l Cost Scrap */~
                            rm_mats(3%),      /* Raw Mat Tot Cost SCRAP*/~
                            rm_eq$(),         /* Save Calc Type and Eq */~
                            rm_ph$(),         /* Save Phantom Designat */~
                            rm_frt,           /* Calc. Freight Cost    */~
                            rm_vinyl_d,       /* Calc. Vinyl Disc. Amt */~
                            #1,               /* (APCCUTEQ) - FILE     */~
                            #4,               /* (GENCODES) - FILE     */~
                            #2,               /* (HNYMASTR) - FILE     */~
                            #3,               /* (HNYQUAN ) - FILE     */~
                            #5,               /* (AMTBOMCD) - FILE     */~
                            #18,              /* (APCSTOCK) - FILE     */~
                            err%      )       /* 0% = OK               */
           area$ = "1"
           rm_vinyl = rm_mat(1%)         /* Raw Mat'l Vinyl Cost      */
           rm_misc  = rm_mat(2%)         /* Raw Mat'l Misc. Cost      */
           rm_total = rm_mat(3%)         /* Raw Material Total Cost   */
           rm_vinyl_s = rm_mats(1%)      /* Vinyl Mat'l Cost Scrap    */
           rm_misc_s  = rm_mats(2%)      /* Misc Mat'l Cost Scrap     */
           rm_total_s = rm_mats(3%)      /* Raw Mat Tot Cost SCRAP     */

           gosub calc_costmate

           rm_mat(1%)       = rm_vinyl   /* Raw Mat'l Vinyl Cost      */
           rm_mat(2%)       = rm_misc    /* Raw Mat'l Misc. Cost      */
           rm_mat(3%)       = rm_total   /* Raw Material Total Cost   */
           rm_mats(1%)      = rm_vinyl_s /* Vinyl Mat'l Cost Scrap    */
           rm_mats(2%)      = rm_misc_s  /* Misc Mat'l Cost Scrap     */
           rm_mats(3%)      = rm_total_s /* Raw Mat Tot Cost SCRAP    */

           tc(1%) = rm_mat(1%) + rm_mats(1%)         /* Total Mat Vinyl*/
           tc(2%) = rm_mat(2%) + rm_mats(2%)         /* Total Mat Misc */
           tc(3%) = rm_mat(3%) + rm_mats(3%)         /* Total Mat Total*/
           tc(19%) = tc(19%) + rm_frt                /* Total Freight  */
           tc(20%) = tc(20%) + rm_vinyl_d            /* Tot Vinyl Disc */
           if rm_cnt% = 0% then goto L04320
           for i% = 1% to rm_cnt%
               tmp$(1%,i%) = rm_part$(i%)   /*Inv Raw Material Part No */
               tmc(1%,i%)  = rm_cuts(i%)    /* Cut Inchec in Deciaml   */
               tmct(1%,i%) = rm_cost(i%)    /* Tot Cost Inv Raw Mat'l  */
               tmu%(1%,i%) = gs_units%(i%)  /* Units of Measure Code   */
               tmd$(1%,i%) = rm_desc$(i%)   /* Inv Raw Mat'l Desc      */
               tmuc(1%,i%) = rm_unit(i%)    /* Raw Mat'l Unit Cost     */
               tmsi(1%,i%) = rm_cuts_s(i%)  /* Scrap Inches In Decimal */
               tmsc(1%,i%) = rm_cost_s(i%)  /* Scrap Cost for Raw Mat'l*/
               tmeq$(1%,i%) = rm_eq$(i%)    /* Calc Type and Eq. No.   */
               tmph$(1%,i%) = rm_ph$(i%)    /* Save Phantom Designature*/
           next i%
L04320:    tcnt%(1%) = rm_cnt%

           if err% <> 0% then apc_err%(2%) = 2%      /* Material Error */
           if x_err% <> 0% then apc_err%(12%) = 12%  /* Mat'l Costmate */
        return

        calc_glass
        calc_screen
           ii%, err%, gs_cnt% = 0%
           if typ% = 1% then ii% = 3%
           call "APCCST2B" (part$,            /* MFG PART NUMBER       */~
                            typ%,             /* 0% = Glass,1% = Screen*/~
                            gs_qty(),         /* G/S Mat'l Units (1-8) */~
                            gs_raw$(),        /* Raw Mat'l Parts (1-8) */~
                            gs_desc$(),       /* Raw Mat'l Descr. (1-8)*/~
                            gs_cost(),        /* Raw Mat'l Cost (1-8)  */~
                            gs_units%(),      /* RAW Mat'l U. M. (1-8) */~
                            rm_unit(),        /* Raw Mat'l Unit Cost   */~
                            rm_mat(4%+ii%),   /* Vinyl Raw Mat'l Cost  */~
                            rm_mat(5%+ii%),   /* Misc. Raw Mat'l Cost  */~
                            rm_mat(6%+ii%),   /* Total Raw Mat'l Cost  */~
                            gs_cnt%,          /* G/S No. Raw Mat'l Item*/~
                            gs_qty_s(),       /* G/S Units Mat'l Scrap */~
                            gs_cost_s(),      /* G/S Scrap Cost Mat'l  */~
                            rm_mats(4%+ii%),  /* Vinyl Raw Mat'l Scrap */~
                            rm_mats(5%+ii%),  /* Misc. Raw Mat'l Scrap */~
                            rm_mats(6%+ii%),  /* G/S Total Cost Scrap  */~
                            rm_eq$(),         /* Calc Type & Eq. No.   */~
                            rm_ph$(),         /* Save Phantom Number   */~
                            rm_frt,           /* Calc. Freight Cost    */~
                            rm_vinyl_d,       /* Calc. Vinyl Disc. Amt */~
                            #8,               /* (APCEQUAT) - File     */~
                            #5,               /* (AMTBOMCD) - File     */~
                            #4,               /* (GENCODES) - File     */~
                            #3,               /* (HNYQUAN ) - File     */~
                            #2,               /* (HNYMASTR) - File     */~
                            err%    )         /* 0% = Ok, Non 0% = Err */

           if typ% <> 0% then goto L04910
              tc(4%)  = rm_mat(4%) + rm_mats(4%)
              tc(5%)  = rm_mat(5%) + rm_mats(5%)
              tc(6%)  = rm_mat(6%) + rm_mats(6%)
              tc(19%) = tc(19%) + rm_frt             /* Total Freight  */
              tc(20%) = tc(20%) + rm_vinyl_d         /* Tot Vinyl Disc */
              if err% <> 0% then apc_err%(3%) = 3%      /* Glass Error */
              for i% = 1% to gs_cnt%
               tmp$(2%,i%) = gs_raw$(i%)    /*Inv Raw Material Part No */
               tmc(2%,i%)  = gs_qty(i%)     /* Cut Inchec in Deciaml   */
               tmct(2%,i%) = gs_cost(i%)    /* Tot Cost Inv Raw Mat'l  */
               tmu%(2%,i%) = gs_units%(i%)  /* Units of Measure Code   */
               tmd$(2%,i%) = gs_desc$(i%)   /* Inv Raw Mat'l Desc      */
               tmuc(2%,i%) = rm_unit(i%)    /* Raw Mat'l Unit Cost     */
               tmsi(2%,i%) = gs_qty_s(i%)   /* Scrap Inches In Decimal */
               tmsc(2%,i%) = gs_cost_s(i%)  /* Scrap Cost for Raw Mat'l*/
               tmeq$(2%,i%) = rm_eq$(i%)    /* Calc Type and Eq. No.   */
               tmph$(2%,i%) = rm_ph$(i%)    /* Save Phantom Designature*/
              next i%
              tcnt%(2%) = gs_cnt%
              return
L04910:    tc(7%)  = rm_mat(7%) + rm_mats(7%)
           tc(8%)  = rm_mat(8%) + rm_mats(8%)
           tc(9%)  = rm_mat(9%) + rm_mats(9%)
           tc(19%) = tc(19%) + rm_frt                /* Total Freight  */
           tc(20%) = tc(20%) + rm_vinyl_d            /* Tot Vinyl Disc */
           if err% <> 0% then apc_err%(4%) = 4%        /* Screen Error */
              for i% = 1% to gs_cnt%
               tmp$(3%,i%) = gs_raw$(i%)    /*Inv Raw Material Part No */
               tmc(3%,i%)  = gs_qty(i%)     /* Cut Inchec in Deciaml   */
               tmct(3%,i%) = gs_cost(i%)    /* Tot Cost Inv Raw Mat'l  */
               tmu%(3%,i%) = gs_units%(i%)  /* Units of Measure Code   */
               tmd$(3%,i%) = gs_desc$(i%)   /* Inv Raw Mat'l Desc      */
               tmuc(3%,i%) = rm_unit(i%)    /* Raw Mat'l Unit Cost     */
               tmsi(3%,i%) = gs_qty_s(i%)   /* Scrap Inches In Decimal */
               tmsc(3%,i%) = gs_cost_s(i%)  /* Scrap Cost for Raw Mat'l*/
               tmeq$(3%,i%) = rm_eq$(i%)    /* Calc Type and Eq. No.   */
               tmph$(3%,i%) = rm_ph$(i%)    /* Save Phantom Designature*/
              next i%
              tcnt%(3%) = gs_cnt%
        return
        calc_locks
           err% = 0%
           call "APCCST3B" (part$,            /* MFG PART NUMBER       */~
                            rm_part$(),       /* Inv Raw Mat'l Part No.*/~
                            rm_cuts(),        /* Total Inches in Decima*/~
                            rm_cost(),        /* Total Cost Raw Mat'l  */~
                            rm_desc$(),       /* Raw Mat'l Description */~
                            gs_units%(),      /* Units of Measure      */~
                            rm_unit(),        /* Raw Mat'l Unit Cost   */~
                            rm_mat(10%),      /* Raw Mat'l Cost Vinyl  */~
                            rm_mat(11%),      /* Raw Mat'l Cost Vinyl  */~
                            rm_mat(12%),      /* Raw Material Tot Cost */~
                            rm_cnt%,          /* Raw Material Count    */~
                            rm_frt,           /* Calc. Freight Cost    */~
                            rm_vinyl_d,       /* Calc. Vinyl Disc. Amt */~
                            #4,               /* (GENCODES) - FILE     */~
                            #2,               /* (HNYMASTR) - FILE     */~
                            #3,               /* (HNYQUAN ) - FILE     */~
                            err%      )       /* 0% = OK               */

           tc(10%) = rm_mat(10%)              /* Total Cost Locks Vinyl*/
           tc(11%) = rm_mat(11%)              /* Total Cost Locks Misc */
           tc(12%) = rm_mat(12%)              /* Total Cost Locks      */
           tc(19%) = tc(19%) + rm_frt                /* Total Freight  */
           tc(20%) = tc(20%) + rm_vinyl_d            /* Tot Vinyl Disc */
           if err% <> 0% then apc_err%(5%) = 5%         /* Locks Error */
           for i% = 1% to rm_cnt%
               tmp$(4%,i%) = rm_part$(i%)   /*Inv Raw Material Part No */
               tmc(4%,i%)  = rm_cuts(i%)    /* Cut Inchec in Deciaml   */
               tmct(4%,i%) = rm_cost(i%)    /* Tot Cost Inv Raw Mat'l  */
               tmu%(4%,i%) = gs_units%(i%)  /* Units of Measure Code   */
               tmd$(4%,i%) = rm_desc$(i%)   /* Inv Raw Mat'l Desc      */
               tmuc(4%,i%) = rm_unit(i%)    /* Raw Material Unit Cost  */
               tmeq$(4%,i%) = "NA "         /* Calc Type and Eq. No.   */
               tmph$(4%,i%) = "Wired"       /* Save Phantom Designature*/
           next i%
           tcnt%(4%) = rm_cnt%
        return

        calc_hardware
           ii%, err% = 0%
           if cst_typ$ = "1" then ii% = 3%
        call "APCCST4B" (part$,          /* MFG Part Number           */ ~
                        cst_typ$,        /* '0'=Hardware,'1'=Packaging*/ ~
                        rm_raw$(),       /* Inv. Raw Material Part    */ ~
                        rm_cuts(),       /* Total Inches in Decimal   */ ~
                        rm_cost(),       /* Total Cost Raw Mat. Part  */ ~
                        rm_desc$(),      /* Raw Material Description  */ ~
                        gs_units%(),     /* RAW MAT'L UNITS OF MEASURE*/ ~
                        rm_unit(),       /* Raw Mat'l Unit Cost       */ ~
                        rm_mat(13%+ii%), /* Raw Mat'l Vinyl Cost      */ ~
                        rm_mat(14%+ii%), /* Raw Mat'l Misc. Cost      */ ~
                        rm_mat(15%+ii%), /* Raw Material Total Cost   */ ~
                        rm_cnt%,         /* Raw Material Count        */ ~
                        rm_cuts_s(),     /* Total Inch Decima SCRA     */~
                        rm_cost_s(),     /* Total Cst Raw Mat SCRA     */~
                        rm_mats(13%+ii%),/* Vinyl Mat'l Cost Scrap     */~
                        rm_mats(14%+ii%),/* Misc Mat'l Cost Scrap      */~
                        rm_mats(15%+ii%),/* Raw Mat Tot Cost SCRAP     */~
                        rm_eq$(),        /* Save Calc Type and Eq      */~
                        rm_ph$(),        /* Save Phantom Designat      */~
                        rm_frt,          /* Calc. Freight Cost         */~
                        rm_vinyl_d,      /* Calc. Vinyl Disc. Amt      */~
                        #9,              /* (APCCSTHP) - File         */ ~
                        #4,              /* (GENCODES) - File         */ ~
                        #3,              /* (HNYQUAN ) - File         */ ~
                        #2,              /* (HNYMASTR) - File         */ ~
                        err%    )        /* 0% = Ok, Non 0% = Error   */
           if cst_typ$ = "0" then area$ = "5"                            ~
                             else area$ = "6"
           rm_vinyl = rm_mat(13%+ii%)    /* Raw Mat'l Vinyl Cost      */
           rm_misc  = rm_mat(14%+ii%)    /* Raw Mat'l Misc. Cost      */
           rm_total = rm_mat(15%+ii%)    /* Raw Material Total Cost   */
           rm_vinyl_s = rm_mats(13%+ii%) /* Vinyl Mat'l Cost Scrap    */
           rm_misc_s  = rm_mats(14%+ii%) /* Misc Mat'l Cost Scrap     */
           rm_total_s = rm_mats(15%+ii%) /* Raw Mat Tot Cost SCRAP     */

           gosub calc_costmate

           rm_mat(13%+ii%)  = rm_vinyl   /* Raw Mat'l Vinyl Cost      */
           rm_mat(14%+ii%)  = rm_misc    /* Raw Mat'l Misc. Cost      */
           rm_mat(15%+ii%)  = rm_total   /* Raw Material Total Cost   */
           rm_mats(13%+ii%) = rm_vinyl_s /* Vinyl Mat'l Cost Scrap    */
           rm_mats(14%+ii%) = rm_misc_s  /* Misc Mat'l Cost Scrap     */
           rm_mats(15%+ii%) = rm_total_s /* Raw Mat Tot Cost SCRAP    */

           if cst_typ$ <> "0" then goto L06170
              tc(13%) = rm_mat(13%)
              tc(14%) = rm_mat(14%)
              tc(15%) = rm_mat(15%)
              tc(19%) = tc(19%) + rm_frt             /* Total Freight  */
              tc(20%) = tc(20%) + rm_vinyl_d         /* Tot Vinyl Disc */
              if err% <> 0% then apc_err%(6%) = 6%   /* Hardware Error */
              if x_err% <> 0% then apc_err%(16%) = 16%  /* Costmate */
              for i% = 1% to rm_cnt%
               tmp$(5%,i%) = rm_raw$(i%)    /*Inv Raw Material Part No */
               tmc(5%,i%)  = rm_cuts(i%)    /* Cut Inchec in Deciaml   */
               tmct(5%,i%) = rm_cost(i%)    /* Tot Cost Inv Raw Mat'l  */
               tmu%(5%,i%) = gs_units%(i%)  /* Units of Measure Code   */
               tmd$(5%,i%) = rm_desc$(i%)   /* Inv Raw Mat'l Desc      */
               tmuc(5%,i%) = rm_unit(i%)    /* Raw Mat'l Unit Cost     */
               tmeq$(5%,i%) = "NA "         /* Calc Type and Eq. No.   */
               tmph$(5%,i%) = "Wired"       /* Save Phantom Designature*/
              next i%
              tcnt%(5%) = rm_cnt%
              return
L06170:    tc(16%) = rm_mat(16%)
           tc(17%) = rm_mat(17%)
           tc(18%) = rm_mat(18%)
           tc(19%) = tc(19%) + rm_frt                /* Total Freight  */
           tc(20%) = tc(20%) + rm_vinyl_d            /* Tot Vinyl Disc */
           if err% <> 0% then apc_err%(7%) = 7%     /* Packaging Error */
           if x_err% <> 0% then apc_err%(17%) = 17%  /*Costmate Package*/

           for i% = 1% to rm_cnt%
               tmp$(6%,i%) = rm_raw$(i%)    /*Inv Raw Material Part No */
               tmc(6%,i%)  = rm_cuts(i%)    /* Cut Inchec in Deciaml   */
               tmct(6%,i%) = rm_cost(i%)    /* Tot Cost Inv Raw Mat'l  */
               tmu%(6%,i%) = gs_units%(i%)  /* Units of Measure Code   */
               tmd$(6%,i%) = rm_desc$(i%)   /* Inv Raw Mat'l Desc      */
               tmuc(6%,i%) = rm_unit(i%)    /* Raw Mat'l Unit Cost     */
               tmeq$(6%,i%) = "NA "         /* Calc Type and Eq. No.   */
               tmph$(6%,i%) = "Wired"       /* Save Phantom Designature*/
           next i%
           tcnt%(6%) = rm_cnt%
        return 
        calc_price                  /* Get Correct Size and Price      */
          mat pc     = zer
          mat ref_p  = zer
          mat ref_p1 = zer
          if calc% <> 0% and calc% <> 8% then return
          p1    = 0.0      /* Spc Customer Price, Dealer Catalog Always*/
          sp%   = 0%       /* (0%) Dealer Price Catalog (Only)         */
                           /* (1%) Spc Customer Catalog Price in (P1)  */
                           /* (2%) Spc Customer EDI Price in     (P1)  */
                           /* Note - Information from 'CUS PRICE' Table*/
          size$ = "E"      /* Always Exact Size                        */
          upd$ = "N"       /* Do Not Update Price Sheets               */
          err%  = 0%       /* Clear Error Flag                         */
          call "APCPRSUB" ( part$,       /* Part Number                */~
                            partno1$,    /* Part Number 1  (AWD001)    */~
                            size$,       /* (O)pen,(E)xact,(F)No Deduct*/~
                            pc(),        /* Calc Dealer Price Catalog  */~
                            p1,          /* Special Customer Price     */~
                            sp%,         /* Special Price Code 0,1,2   */~
                            upd$,        /* Update Price Sheet Y or N  */~
                            cuscode$,    /* Customer Code              */~
                            err%,        /* Error Return Codes         */~
                            ref$(),      /* Ref. Type Codes Catalog    */~
                            ref1$(),     /* Ref. Type Codes Spec Cat.  */~
                            ref_p(),     /* Ref Prices APC Catalog     */~
                            ref_p1(),    /* Ref Prices Special Cat.    */~
			    " ",  ~
			    " ",  ~
			    " ",  ~
			    " ",  ~
			    " ",  ~
                            #13,         /* Channel of (APCPCMST) File */~
                            #22,         /* Channel of (AWDPCMST) File */~
                            #15,         /* Channel of (APCPCMSK) File */~
                            #16,         /* Channel of (APCPCMSD) File */~
                            #4,          /* Channel of (GENCODES) File */~
                            #11,         /* Channel of (CPRPRICE) File */~
                            #12,         /* Channel of (CUSTOMER) File */~
                            #14       )  /* Channel of (APCSKUNO) File */
                                         /* After Call PART$ Always    */
                                         /* has Exact Size             */
           if err% <> 0% then apc_err%(8%) = 8%       /* Pricing Error */
           srce1% = 0% : cd$ = "0"
           read #12,key = cuscode$, using L06740, cd$, eod goto L06750
L06740:       FMT POS(525), CH(1)
L06750:    if cd$ >= "A" and cd$ <= "Z" then                             ~
              srce1% = val(cd$) - 64%           /* (01) Thru (26) */     ~
              else  srce1% = val(cd$) - 21%     /* (27) Thru (36) */
           price = pc(srce1%)             /* Cat. Price with Cust Disc*/
           if sp% > 0% and p1 > 0.1 then price = p1  /* Special Price */
        return

        calc_wood_surround
            tc(21%) = 0.0 : value$ = "0.0"
            if len(part$) < 22 then goto L06960
            if str(part$,1%,1%) = "9" then goto L06960
            if len(part$) = 22 then wood_code$ = str(part$,20%,3%)       ~
                               else wood_code$ = str(part$,23%,3%)
            if wood_code$ = "000" then goto L06960
               readkey$ = " " : value$ = "0.0"
               str(readkey$,1%,9%) = "COST WOOD"
               str(readkey$,10%,3%) = wood_code$
               read #4,key = readkey$, using L06930 ,value$, eod goto L06940
L06930:           FMT POS(25), CH(10)
L06940:        convert value$ to tc(21%), data goto L06950
L06950:                                    /* Fixed Cost Wood Surround */
L06960: return

        calc_costmate                       /* Exception Utility       */
           x_err% = 0%
           call "APCCST6B" ( area$,         /* Area To Assign Def      */~
                             part$,         /* MFG Part Number         */~
                            rm_raw$(),      /* Inv Raw Material Part No*/~
                            rm_cuts(),      /* Total Inchec in Decimal */~
                            rm_cost(),      /* Total Cost Rsw Mat'l Par*/~
                            rm_desc$(),     /* Raw Material Description*/~
                            gs_units%(),    /* Raw Mat'l Units of Meas */~
                            rm_unit(),      /* Raw Mat'l Unit Cost     */~
                            rm_vinyl,       /* Raw Mat'l Vinyl Cost    */~
                            rm_misc,        /* Raw Mat'l Misc Cost     */~
                            rm_total,       /* Raw Mat'l Total Cost    */~
                            rm_cnt%,        /* Raw Mat'l Count (Pieces)*/~
                            rm_cuts_s(),    /* Tot Inch Decimal Scrap  */~
                            rm_cost_s(),    /* Tot Cost Raw Mat'l Scrap*/~
                            rm_vinyl_s,     /* Raw Mat'l Vinyl Scrap Co*/~
                            rm_misc_s,      /* Raw Mat'l Misc Scrap Cos*/~
                            rm_total_s,     /* Raw Mat'l Tot Cost Scrap*/~
                            rm_eq$(),       /* Save Type and Eq. No.   */~
                            rm_ph$(),       /* Save Eq. Phantom Code   */~
                            rm_frt,         /* Save Freight Cost       */~
                            rm_vinyl_d,     /* Save Vinyl Discount Amt */~
                             #1,            /*   (APCCUTEQ)            */~
                             #2,            /*   (HNYMASTR)            */~
                             #3,            /*   (HNYQUAN)             */~
                             #4,            /*   (GENCODES)            */~
                             #5,            /*   (AMTBOMCD)            */~
                             #7,            /*   (APCEMPLY)            */~
                             #8,            /*   (APCEQUAT)            */~
                             #9,            /*   (APCCSTHP)            */~
                             #10,           /*   (APCCSTLR)            */~
                             #17,           /*   (APCCSTEX)            */~
                             #18,           /*   (APCSTOCK)            */~
                             x_err% )       /* 0% = Ok, 1% = Error     */
        return

        sub_exit
           if debug% = 0% then end

           call "APCPLNDD" ( sel$,       /* Debug Screen Selection     */~
                             cuscode$,   /* Customer Code for Pricing  */~
                             part$,      /* MFG Part Number            */~
                             lab(),      /* Labor Costs (1 thru 10)    */~
                             avg_pay(),  /* Avg Pay per Dept           */~
                             uph(),      /* Avg Units Per Manhour Dept */~
                             tc(),       /* Material Costs (1 thru 20) */~
                             tt(),       /* Total Cost Buckets         */~
                             rm_mat(),   /* Material Costs (1 thru 10) */~
                             rm_mats(),  /* Mat'l Scrap Costs(1 thru 9)*/~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */

        end
