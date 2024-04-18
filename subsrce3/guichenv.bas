        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGGG  U   U  IIIII   CCC   H   H  EEEEE  N   N  V   V   *~
            *  G      U   U    I    C      H   H  E      NN  N  V   V   *~
            *  G GGG  U   U    I    C      HHHHH  EEEE   N N N  V   V   *~
            *  G   G  U   U    I    C      H   H  E      N  NN   V V    *~
            *   GGG    UUU   IIIII   CCC   H   H  EEEEE  N   N    V     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GUICHENV - This routine will load the GUI environment     *~
            *            options for a specified name (program, routine,*~
            *            etc).  This includes the TOOLBAR, HOTSPOTS, and*~
            *            TEMPLATES.  If any of the environment files    *~
            *            for the specified name are not found the       *~
            *            current environment file remains in effect     *~
            *            unless the LOAD_DEFAULTS% flag is true, in     *~
            *            which case the following defaults may be used; *~
            *                 TOOLBAR = cmsprogs.tbf                    *~
            *                 HOTSPOT = cmscontr.hsf                    *~
            *                 TEMPLATE= cmsprogs.key                    *~
            *            If user definition has turned off display of   *~
            *            toolbar or button bar in CMSUSRIN then they    *~
            *            will not be loaded.                            *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1995, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/28/95 ! Original                                 ! LDJ *~
            * 01/28/97 ! Increase WAIT period to correct WAN      ! LDJ *~
            *          ! Latency Problem.                         !     *~
            * 03/04/97 ! Added Show Toolbar, Buttonbar Options.   ! LDJ *~
            *          ! Substituted GETCMD for WSXIO.            !     *~
            * 06/18/97 ! Changed BBARSETUP api command from 0 to 3! LDJ *~
            * 07/08/97 ! Attempt to reduce number of API messages ! LDJ *~
            *          ! sent by comparing last function name to  !     *~
            *          ! current one.                             !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "GUICHENV" (function$, load_defaults%, tbar$, bbar$)

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
cms_start
            call "NARGS" addr(nargs%)   /* test to see if extra args   */

        dim                                                              ~
            bbar_name$16,                /* Name of ButtonBar to load  */~
            cdir$32,                     /* PC Files directory         */~
            function$16,                 /* Program/subroutine name    */~
            hspot_name$16,               /* Name of Hotspot to load    */~
            last_bbar$16,                /* Last ButtonBar loaded      */~
            last_hspot$16,               /* Last Hotspot file loaded   */~
            last_tbar$16,                /* Last ToolBar file loaded   */~
            return$2,                    /* keyboard input             */~
            show_tbar$1,                 /* 'Y' to load and show tbar  */~
            show_bbar$1,                 /* 'N' to not show button bar */~
            tbar_name$16,                /* Name of Toolbar to Load    */~
            uw$1                         /* CoStar MAGIC Character     */


        REM *** Are we in GUI Land ? ***
            call "CHECKGUI" addr(gui%)
            if gui% = 0% then exit_routine
            uw$ = hex(7f)
            if nargs% > 2% then show_tbar$ = tbar$ else show_tbar$="Y"
            if nargs% > 3% then show_bbar$ = bbar$ else show_bbar$="Y"
            cdir$ = "." & hex(5c) & "caelus" & hex(5c) /* PC Files Dir */
            on load_defaults% + 1% gosub replace_if_there,               ~
                                         replace_with_defaults
            call "SENDCMD" (uw$ & "UWBBARSETUP,3" & uw$)    /* Disable Popup */
            goto exit_routine

        replace_if_there     /* Replace Template and Toolbar */
          if show_bbar$ = "Y" then ~
            call "SENDCMD" (uw$ & "UWBUTTONBAR" & cdir$ & function$ &    ~
              ".key,1" & uw$)  /* Load display utility button bar*/
          if show_tbar$ = "Y" then ~
            call "SENDCMD" (uw$ & "UWTOOLBAR" & cdir$ &   function$ &    ~
                     ".tbf" & uw$)    /* Load new Toolbar if there */
            call "SENDCMD" (uw$ & "UWHOTSPOT" & cdir$ &   function$ &    ~
                     ".hsf" & uw$)    /* Load hotspot file - if there */
            return

        replace_with_defaults
            REM *** Hide Screen Cursor ***
            call "SENDCMD" (uw$ & "UWSETUP3,1" & uw$)
*       *****************************************************************
*        Check for Template / Button Bar
            if show_bbar$ = "N" then Check_4_Toolbar
            call "SENDCMD" (uw$ & "UWSCRIPTsdk/fexist.scr," & cdir$ &    ~
                            function$ & ".key" & uw$)
            gosub get_gui_macro_result
            if return_value% = 0% then  /* file not found */             ~
               bbar_name$ = "cmsprogs" else bbar_name$ = function$
            if bbar_name$ = last_bbar$ then Check_4_Toolbar
            call "SENDCMD" (uw$ & "UWBUTTONBAR" & cdir$ & bbar_name$ &  ~
              ".key,1" & uw$)
            last_bbar$ = bbar_name$

Check_4_Toolbar
*        Check for Toolbar
            if show_tbar$ = "N" then Check_4_Hotspot
            call "SENDCMD" (uw$ & "UWSCRIPTsdk/fexist.scr," & cdir$ &    ~
                            function$ & ".tbf" & uw$)
            gosub get_gui_macro_result
            if return_value% = 0% then   /* file not found */            ~
               tbar_name$ = "cmsprogs" else tbar_name$ = function$
            if tbar_name$ = last_tbar$ then Check_4_Hotspot
            call "SENDCMD" (uw$ & "UWTOOLBAR" & cdir$ & tbar_name$ &    ~
              ".tbf" & uw$)
            last_tbar$ =tbar_name$

Check_4_Hotspot
*        Check for HotSpot
            call "SENDCMD" (uw$ & "UWSCRIPTsdk/fexist.scr," & cdir$ &    ~
                            function$ & ".hsf" & uw$)
            gosub get_gui_macro_result
            if return_value% = 0% then   /* file not found */            ~
               hspot_name$ = "CMSCONTR" else hspot_name$ = function$
            if hspot_name$ = last_hspot$ then Restore_Cursor
            call "SENDCMD" (uw$ & "UWHOTSPOT" & cdir$ & hspot_name$ &      ~
                     ".hsf" & uw$)
            last_hspot$ = hspot_name$
*       *****************************************************************
Restore_Cursor
            REM *** Restore Screen Cursor ***
            call "SENDCMD" (uw$ & "UWSETUP3,0" & uw$)
            return

        get_gui_macro_result
            call "GETCMD" (30%,guy%,return$)    /* wait upto 30 seconds*/
            if guy% <> 0% then L13400           /* No ENTER Received */
            convert return$ to return_value%, data goto L13400
            return

L13400:     return_value% = 0%
            return

        exit_routine
            end
