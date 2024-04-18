        REM *************************************************************~
            *  ( Used in 'AWDPLN64'                                     *~
            *                                                           *~
            *  Program Name      - AWDPLA64                             *~
            *  Creation Date     - 04/12/2012                           *~
            *  Last Modified Date- 01/31/2019                           *~
            *  Written By        - Christie Sanders                     *~
            *  Description       - This Program returns information for *~
            *                      NFRC                                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *04/12/2012! New Program for (AWD) - Last Mod Date    ! CMS *~
            *10/01/2014! (AWD001)For BBG set thickness to 1.0000  ! PWW *~
            *01/31/2019! (CR1908)Mod for WindowNation Foam Sash   ! CMN *~
            *06/26/2023! CR3339 Duralite logic changes            ! RDB *~
            *************************************************************
        sub "AWDPLA64"   (nf_part$,          /* Mfg Part Number         */~
                          nf_subpart$,       /* Subpart Number          */~
                          ufactor$,          /* Ufactor       - out     */~
                          sheat$,            /* SolarHeat     - out     */~
                          vtranmit$,         /* Visible Trans - out     */~
                          cpdnumber$,        /* CPD Number    - out     */~
                          warehouse$,        /* Warehouse     - out     */~
                          series$,           /* Series        - out     */~
                          style$,            /* Style         - out     */~
                          spacercode$,       /* SpacerCode    - out     */~
                          igthickness$,      /* IGThickness   - out     */~
                          pane1$,            /* Pane1         - out     */~
                          pane2$,            /* Pane2         - out     */~
                          pane3$,            /* Pane3         - out     */~
                          gridcode$,         /* GridCode      - out     */~
                          gridsize$,         /* Gridsize      - out     */~
                          gapfill1$,         /* Gapfill1      - out     */~
                          gapfill2$,         /* GapFill2      - out     */~
                          framecode$,        /* FrameCode     - out     */~
                          sashcode$,         /* SashCode      - out     */~
                          #1,                /* NFRCDATA                */~
                          #2,                /* NFRCMDL                 */~
                          #3,                /* NFRCGLS                 */~
                          #4,                /* Gencodes                */~
                          err%)              /* Return Error Code       */

        dim                                                               ~
            nf_part$25,                      /* Mfg Part Number         */~
            nf_subpart$20,                   /* Subpart Number          */~
            ufactor$10,                      /* Ufactor - out           */~
            sheat$10,                        /* SolarHeat - out         */~
            vtranmit$10,                     /* Visible Trans - out     */~
            cpdnumber$30                     /* CPD Number - out        */

        dim                                                               ~
            nfrcdata_key0$104,               /* NFRCDATA Key0           */~
            nfrcmdl_key0$20,                 /* NFRCMDL key0            */~
            nfrcmdl_sav0$20,                 /* NFRCMDL key0            */~
            nfrcgls_key0$7,                  /* NFRCGLS Key0            */~
            warehouse$4,                     /* Warehouse               */~
            series$10,                       /* Series                  */~
            style$10,                        /* Style                   */~
            spacercode$10,                   /* SpacerCode              */~
            igthickness$10,                  /* IG THICKNESS            */~
            pane1$5,                         /* Pane1                   */~
            pane2$5,                         /* Pane2                   */~
            pane3$5,                         /* Pane3                   */~
            pane5_32_1$5,                    /* Pane1 5_32              */~
            pane5_32_2$5,                    /* Pane2 5_32              */~
            pane5_32_3$5,                    /* Pane3 5_32              */~
            gridcode$5,                      /* Grid Code               */~
            gridsize$10,                     /* Grid Size               */~
            gapfill1$10,                     /* GapFill1                */~
            gapfill2$10,                     /* GapFill2                */~
            framecode$5,                     /* FrameCode               */~
            foamframe$5,                     /* FoamFrameCode           */~
            sashcode$5,                      /* SashCode                */~
            foamsash$5                       /* FoamSash (CR1908)       */

        dim model$5,                         /* Model Number            */~
            gls$2,                           /* Glass Code              */~
            liting$2,                        /* Litting Code            */~
            foam$1,                          /* Foam                    */~
            extgrdtype$1,                    /* Ext Grid Type           */~
            extgrdsize$1,                    /* Ext Grdi Size           */~
            sp_intercept$1,                  /* Spacer Code             */~
            intercept$2,                     /* Intercept               */~
            subpartigthk$1,                  /* Subpart IG Thickness    */~
            sashacc$1,                       /* Sash Accessory (CR1908) */~
            gridtype$1                       /* Grid Type CR3339        */

        dim gen_key0$24,                     /* Gencodes Key            */~
            desc$30                          /* Gencodes Description    */

        dim logit$256                        /* LogIt message           */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) NFRC Data Lookup                  "
            pname$ = "AWDPLA64 - Rev: R1.00"


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! NFRCDATA ! NFRC data from corp                      *~
            * #2  ! NFRCMDL  ! Model to Corp Series                     *~
            * #3  ! NFRCGLS  ! Glass Data                               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            init(" ") warehouse$, series$, style$, spacercode$,          ~
                 igthickness$, pane1$, pane2$, pane3$, gridcode$,        ~
                 gridsize$, gapfill1$, gapfill2$, framecode$, foamframe$,~
                 sashcode$, ufactor$, sheat$, vtranmit$, cpdnumber$,     ~
                 pane5_32_1$, pane5_32_2$, pane5_32_3$, subpartigthk$,   ~
                 foamsash$, gridtype$
                 
            err%, surface3% = 0%


            if len(nf_part$) < 19% then err% = 99%
            warehouse$ = "AWD"
            model$ = str(nf_part$,1%,3%)
            gls$   = str(nf_part$,5%,2%)
            liting$= str(nf_part$,7%,2%)
            foam$  = str(nf_subpart$,5%,1%)
            extgrdtype$   = str(nf_subpart$,8%,1%)
            extgrdsize$   = str(nf_subpart$,9%,1%)
            sp_intercept$ = str(nf_subpart$,17%,1%)
            subpartigthk$ = str(nf_subpart$,14%,1%)
            sashacc$      = str(nf_subpart$,16%,1%)          /*(CR1908)*/
            if gls$ >= "3A" and gls$ <= "3N" then surface3% = 1%            

            gridcode$ = "N"
            if liting$ <> "00" then gridcode$ = "G"
            if str(liting$,1%,1%) >= "A" and str(liting$,2%,1%) = "0" ~
                                                   then gridcode$ = "N"
REM            IF GRIDCODE$ <> "N" AND EXTGRDTYPE$ = "1" THEN GRIDCODE$ = "S"
            if gridcode$ <> "N" and extgrdtype$ = "1" and  ~
                      extgrdsize$ = "1" then gridcode$ = "S"
/*AWD001*/  if str(nf_subpart$,1%,1%) = "4" then gridcode$ = "N"

            gridsize$ = "0.0000"
            if gridcode$ = "G" then gridsize$ = "0.7500"
            if gridcode$ = "S" then gridsize$ = "1.5000"

            if err% <> 0% then goto exit_sub
        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *-----------------------------------------------------------*~
            * Processes data from calling program to produce key.       *~
            *************************************************************
        REM BUILD_KEY

        gosub check_triple
         if err% <> 0% then goto exit_sub
        gosub lookup_series
         if err% <> 0% then goto exit_sub
        gosub lookup_gls
         if err% <> 0% then goto exit_sub
        gosub lookup_intercept
         if err% <> 0% then goto exit_sub
        gosub lookup_shape  /* If shape use duraseal A8-S */

        gosub lookup_spacer
         if err% <> 0% then goto exit_sub
        gosub lookup_nfrc

        goto exit_sub


        lookup_series
         init(" ") nfrcmdl_key0$
         str(nfrcmdl_key0$,1%,3%) = model$
REM STR(NFRCMDL_KEY0$,6%,2%) = "VY"
REM IF FOAM$ = "3" THEN STR(NFRCMDL_KEY0$,4%,2%) = "VF"
REM IF FOAM$ = "4" THEN STR(NFRCMDL_KEY0$,4%,2%) = "VF"
REM IF FOAM$ = "3" THEN STR(NFRCMDL_KEY0$,4%,2%) = FOAMFRAME$
REM IF FOAM$ = "4" THEN STR(NFRCMDL_KEY0$,4%,2%) = FOAMFRAME$

         nfrcmdl_sav0$ = str(nfrcmdl_key0$,1%,20%)
         read #2, key > nfrcmdl_key0$, using NFRCMDL_SAV_FMT, nfrcmdl_key0$, ~
                                                         eod goto no_model

REM IF STR(NFRCMDL_KEY0$,1%,05%) <> STR(NFRCMDL_SAV0$,1%,05%) THEN ~
                                             GOTO NO_MODEL

         if str(nfrcmdl_key0$,1%,03%) <> str(nfrcmdl_sav0$,1%,03%) then ~
                                             goto no_model

                                                                                          
            get #2, using NFRCMDL_FMT, warehouse$,                ~
                                       series$,                   ~
                                       style$,                    ~
                                       model$,                    ~
                                       framecode$,                ~
                                       foamframe$,                ~
                                       sashcode$,                 ~
                                       igthickness,               ~
                                       sdligthickness,            ~
                                       triplethickness,           ~
                                       foamsash$  /* (CR1908) */

          gosub checkForcedFoam
          if series$ = "8300" and ffoam% = 0% and wn% = 0% then goto noFoam                            
            if foam$ = "3" then framecode$ = foamframe$
            if foam$ = "4" then framecode$ = foamframe$
noFoam:
/* CR3339 */
          if series$ = "8300" and ffoam% = 1% and gridcode$ = "N" then ~
                framecode$ = "VF"
                
          if sashacc$ = "5" or sashacc$ = "6" then sashcode$ = foamsash$ 
/*AWD001*/if str(nf_subpart$,1%,1%) = "4" then igthickness = 1.0
             
          convert igthickness to igthickness$, pic(##0.0000)
REM IF EXTGRDTYPE$ = "1" THEN        /* SDL GRID */         ~
             CONVERT SDLIGTHICKNESS TO IGTHICKNESS$, PIC(##0.0000)
          if extgrdtype$ = "1" and extgrdsize$ = "1" then /*SDLGRID*/ ~
             convert sdligthickness to igthickness$, pic(##0.0000)
          if triple% = 1% then             /*TRIPLE PANE*/        ~
             convert triplethickness to igthickness$, pic(##0.0000)
          call "STRING" addr("LJ", igthickness$, 8%)
        return
        no_model
          err% = 1%             /* No Model Information */
        return

        NFRCMDL_SAV_FMT: FMT POS(25), CH(15)
        NFRCMDL_FMT:   FMT CH(04), CH(10), CH(10), CH(05), CH(05), CH(05), ~
                           CH(05), PD(14,4), PD(14,4), PD(14,4), CH(05)


        lookup_gls
          init(" ") nfrcgls_key0$
          str(nfrcgls_key0$,1%,2%) = gls$
          str(nfrcgls_key0$,3%,5%) = "T"
        lookup_gls_nxt
          read #3, key = nfrcgls_key0$, eod goto no_gls
             get #3, using NFRCGLS_FMT, pane1$,                   ~
                                        pane2$,                   ~
                                        pane3$,                   ~
                                        gapfill1$,                ~
                                        gapfill2$,                ~
                                        pane5_32_1$,              ~
                                        pane5_32_2$,              ~
                                        pane5_32_3$


             if subpartigthk$ <> "5" then goto not_5_32
                pane1$ = pane5_32_1$
                pane2$ = pane5_32_2$
                pane3$ = pane5_32_3$

not_5_32:
             if pane1$ = "?" then goto glass_unknown
             if len(pane1$) > 2 then goto not_gls_code
                str(nfrcgls_key0$,1%,2%) = pane1$
                goto lookup_gls_nxt
        not_gls_code
        return
        no_gls
          err% = 2%            /* No Glass Code Information */
        return
        glass_unknown
          err% = 5%
        return

        NFRCGLS_FMT:   FMT POS(28), CH(05), CH(05), CH(05), CH(10), CH(10), ~
                            CH(05), CH(05), CH(05)

        lookup_intercept
          intercept% = 1%
          if sp_intercept$ <> "0" then goto subpartIntercept
          init(" ") gen_key0$, desc$
          str(gen_key0$,1%,9%)  = "INTERCEPT"
          str(gen_key0$,10%,3%) = model$
          str(gen_key0$,13%,2%) = gls$
          read #4, key = gen_key0$, using GENCODES_FMT, desc$, ~
                                           eod goto no_gls_intercept

             convert str(desc$,1%,2%) to intercept%, data goto intercept_done
             goto intercept_done
        no_gls_intercept
          init(" ") gen_key0$, desc$
          str(gen_key0$,1%,9%)  = "INTERCEPT"
          str(gen_key0$,10%,3%) = model$
          str(gen_key0$,13%,2%) = str(gls$,1%,1%) & "*"
          read #4, key = gen_key0$, using GENCODES_FMT, desc$, ~
                                            eod goto no_intercept_all
             convert str(desc$,1%,2%) to intercept%, data goto intercept_done
             goto intercept_done
        no_intercept_all
          init(" ") gen_key0$, desc$
          str(gen_key0$,1%,9%)  = "INTERCEPT"
          str(gen_key0$,10%,3%) = model$
          str(gen_key0$,13%,2%) = "**"
          read #4, key = gen_key0$, using GENCODES_FMT, desc$, ~
                                            eod goto intercept_done
             convert str(desc$,1%,2%) to intercept%, data goto intercept_done
             goto intercept_done

        intercept_done
          convert intercept% to intercept$,pic(00)
        return
        no_intercept
          err% = 3%
        return
        subpartIntercept
          convert sp_intercept$ to intercept%, data goto no_intercept
          goto intercept_done


        lookup_shape                         /* If shape then 02 spacer */
          init(" ") gen_key0$, desc$
          str(gen_key0$,1%,9%)  = "PLAN SHAP"
          str(gen_key0$,10%,3%) = model$
          read #4, key = gen_key0$, eod goto shape_done
/* CR3338 shapes default to duralite spacer */
              sp_intercept$ = "3"
              intercept$    = "03"
              intercept%    = 3%
        shape_done
        return

        check_triple
          triple% = 0%
          init(" ") gen_key0$, desc$
          str(gen_key0$,1%,9%)  = "PLANTRIPL"
          str(gen_key0$,10%,2%) = gls$
          read #4, key = gen_key0$, using GENCODES_FMT, desc$, ~
                                            eod goto triple_done
              triple% = 1%
        triple_done
        return

        lookup_spacer
          init(" ") gen_key0$, desc$
          str(gen_key0$,1%,9%)  = "SPACER"
          str(gen_key0$,10%,1%) = str(intercept$,2%,1%)
          read #4, key = gen_key0$, using GENCODES_FMT, desc$, ~
                                            eod goto no_spacer

             spacercode$ = str(desc$,21%,10%)
        return
        no_spacer
          err% = 4%                      /* No Spacer Information */
        return
        GENCODES_FMT: FMT POS(25), CH(30)

        lookup_nfrc
          init(" ") nfrcdata_key0$
          str(nfrcdata_key0$,1%,4%)   = warehouse$
          str(nfrcdata_key0$,5%,10%)  = series$
          str(nfrcdata_key0$,15%,10%) = style$
          str(nfrcdata_key0$,25%,10%) = spacercode$
          str(nfrcdata_key0$,35%,10%) = igthickness$
          str(nfrcdata_key0$,45%,5%)  = pane1$
          str(nfrcdata_key0$,50%,5%)  = pane2$
          str(nfrcdata_key0$,55%,5%)  = pane3$
          str(nfrcdata_key0$,60%,5%)  = gridcode$
          str(nfrcdata_key0$,65%,10%) = gridsize$
          str(nfrcdata_key0$,75%,10%) = gapfill1$
          str(nfrcdata_key0$,85%,10%) = gapfill2$
          str(nfrcdata_key0$,95%,5%)  = framecode$
          str(nfrcdata_key0$,100%,5%) = sashcode$

          read #1, key = nfrcdata_key0$, eod goto no_nfrc
             get #1, using NFRCDATA_FMT, cpdnumber$,              ~
                                         ufactor,                 ~
                                         sheat,                   ~
                                         vtranmit

             convert ufactor to ufactor$, pic(######0.00)

             convert sheat to sheat$, pic(######0.00)

             convert vtranmit to vtranmit$, pic(######0.00)

        return
        no_nfrc
          err% = 10%           /* No NFRC Information       */
          init(" ") logit$
REM          logit$ = "ERROR-10|KEY|" & nfrcdata_key0$ & "|"
REM          call "LOGFILE_NFRC" (logit$)
        return
        NFRCDATA_FMT:  FMT POS(105), CH(30), PD(14,4), PD(14,4), PD(14,4)
        
        
        checkForcedFoam
          ffoam% = 0%
          wn% = 0%              /* Window Nation Flag */
          if model$ = "268" then goto windowNation
          if model$ = "S19" then goto windowNation
          if model$ = "S20" then goto windowNation
          if surface3% = 0% then return
REM          if gridcode$ = "N" then return  
          if gridcode$ <> "N" then return      /* CR3339 */
          if series$ <> "8300" then return
REM          if (style$ <> "DH" and style$ <> "SL") then return
          if style$ <> "DH" then return         /* CR3339 */
            ffoam% = 1%                          
        return
        windowNation
         wn% = 1%
        return
         
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_sub

        end


