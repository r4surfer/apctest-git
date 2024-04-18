        REM *************************************************************~
            *                                                           *~
            *   GGG   EEEEE  N   N  RRRR     SSS    CCC    SSS   BBBB   *~
            *  G      E      NN  N  R   R   S      C   C  S      B   B  *~
            *  G GGG  EEE    N N N  RRRR     SSS   C       SSS   BBBB   *~
            *  G   G  E      N  NN  R  R        S  C   C      S  B   B  *~
            *   GGG   EEEEE  N   N  R   R    SSS    CCC    SSS   BBBB   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GENRSCSB - Subroutine used by the Caelus Report Program   *~
            *            Generator to generate the screen I-O sections  *~
            *            of a program. (Note; split out of GENRPPGM     *~
            *            because of compiler limitations - too big!).   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/08/88 ! Original - Cloned from SCRNSUB           ! MJB *~
            * 07/11/97 ! Changed for Unix, no line numbers.       ! DXL *~
            *************************************************************

            sub "GENRSCSB" (#1, prompt$(), alias$(), variable$(),        ~
                                datatype$(), fieldlen$(), maxfields%,    ~
                                outfile$)

        dim                                                              ~
            alias$(65)18,                /* Field alias names          */~
            datatype$(65)1,              /* Data type for field        */~
            fieldlen$(65)2,              /* Field length               */~
            length$3,                    /* Length - Tempory Work Var. */~
            line$6,                      /* Line number for code gen   */~
            outfile$8,                   /* Generated program name     */~
            prompt$(65)27,               /* Prompts for input fields   */~
            promst$50,                   /* Prompt name for writing    */~
            quote$1,                     /* Double Quote Character     */~
            temp$3,                      /* Temporary Work Variable    */~
            tilde$1,                     /* The Tilde Character        */~
            title$50,                    /* Screen Title               */~
            variable$(65)19,             /* Variable names to use      */~
            writeLn$255                  /* Work Variable              */

        REM *************************************************************~
            *        G E N E R A T E   I N P U T   S C R E E N S        *~
            *                                                           *~
            * GENERATES INPUT SCREENS.                                  *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

            title$ = "Input Report Selection Criteria"
            tilde$ = hex(7e)
            quote$ = hex(22)

        REM *************************************************************
            gosub'180(0%)
            put writeLn$, using L02840 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L02870 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L02900 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L02930 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L02960 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03240 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L02990 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03010 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03030 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03050 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03080 , line$
            gosub WriteToFile

            for line% = 1 to maxfields%
                if datatype$(line%) = "1" then offset% = 1%
                if datatype$(line%) = "2" then offset% = 2%
                if datatype$(line%) = "3" then offset% = 3%

                if maxfields% <> 1% then L00940
                   gosub'180(-offset%)
                   put writeLn$, using L03130, " ", line$, alias$(line%)
                   gosub WriteToFile
                   goto L01070
L00940:         if line% <> 1% then L00990
                   gosub'180(-offset%)
                   put writeLn$, using L03100, " ", line$, alias$(line%), tilde$
                   gosub WriteToFile
                   goto L01070
L00990:         if line% <> maxfields% then L01040
                   gosub'180(-offset%)
                   put writeLn$, using L03190, " ", line$, alias$(line%)
                   gosub WriteToFile
                   goto L01070
L01040:         gosub'180(-offset%)
                put writeLn$, using L03160, " ", line$, alias$(line%), tilde$
                gosub WriteToFile
L01070:     next line%

            gosub'180(0%)
            put writeLn$, using L03220 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03240 , line$
            gosub WriteToFile
            gosub'180(1%)
            put writeLn$, using L03260 , line$
            gosub WriteToFile
            gosub'180(1%)
            put writeLn$, using L03290 , line$
            gosub WriteToFile
            gosub'180(1%)
            put writeLn$, using L03320 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03240 , line$
            gosub WriteToFile

        REM Now generate screen header part.
            gosub'180(0%)
            put writeLn$, using L03350 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03380 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            promst$ = title$ & quote$ & ","
            put writeLn$, using L03410 , line$, promst$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03440 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03470 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03500 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            gosub'180(0%)
            put writeLn$, using L03530 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03830 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03560 , line$, tilde$
            gosub WriteToFile

        REM Now generate screen fields.
            for line% = 1% to maxfields%
                convert fieldlen$(line%) to length%
                convert length% to length$, pic(##)
                tran(length$, "0 ") replacing
                gosub'180(0%)
                convert line% + 6% to temp$, pic(##)
                tran(str(temp$, 1%, 2%), "0 ") replacing
                temp$ = temp$ & ","
                put writeLn$, using L03830 , line$, tilde$
                gosub WriteToFile
                gosub'180(0%)
                promst$ = prompt$(line%) & quote$ & ","
                put writeLn$, using L03590 , temp$, promst$, tilde$
                gosub WriteToFile
                gosub'180(0%)
                put writeLn$, using L03620 , line$, temp$, line%,              ~
                                       variable$(line%), length$, tilde$
                gosub WriteToFile
                gosub'180(0%)
                put writeLn$, using L03650 , line$, temp$, line%,              ~
                                       variable$(line%+12), length$, tilde$
                gosub WriteToFile
            next line%

        REM Now generate screen trailer.
            gosub'180(0%)
            put writeLn$, using L03830 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03680 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03740 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03770 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03800 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03830 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03860 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03980 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03880 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03900 , line$, outfile$, begin%
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03980 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03940 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03960 , line$, begin%
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03240 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04000 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04020 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04040 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03240 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04060 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04080 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04100 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04130 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04150 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04180 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04200 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04230 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04250 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04290 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04310 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04340 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04360 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04390 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03240 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04410 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04430 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04460 , line$
            gosub WriteToFile
            gosub setlo

            gosub'180(0%)
            put writeLn$, using L04730 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04760 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04780 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04810 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04830 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04860 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04880 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04910 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04930 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04950 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L03240 , line$
            gosub WriteToFile

        end

        setlo                 /* For Page #1 */
            gosub'180(0%)
            put writeLn$, using L04480 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04540 , line$
            gosub WriteToFile
            gosub'180(0%)

            put writeLn$, using L04560 , line$, tilde$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04630 , line$
            gosub WriteToFile
            gosub'180(0%)
            put writeLn$, using L04650 , line$
            gosub WriteToFile
            return

        REM *************************************************************~
            *  I M A G E   S T A T E M E N T S   F O R   S C R E E N    *~
            *************************************************************

l02840: %#     rem ******************************************************~
        ~*******#

l02870: %#         *               S c r e e n   P a g e   1             ~
        ~      *#

l02900: %#         *-----------------------------------------------------~
        ~------*#

l02930: %#         * Document Input and Edit Screen.                     ~
        ~      *#

l02960: %#         ******************************************************~
        ~*******

l02990: %#     deffn'101(fieldnr%, edit%)
l03010: %#           gosub'050(fieldnr%)
l03030: %#           gosub set_pf1
l03050: %#           if fieldnr% > 0% then init(hex(8c)) lfac$()               #
l03080: %#                            else init(hex(86)) lfac$()
l03100: %#           on fieldnr% gosub #####,        /* ################# */   #
l03130: %#           on fieldnr% gosub #####         /* ################# */
l03160: %#                             #####,        /* ################# */   #
l03190: %#                             #####         /* ################# */
l03220: %#           goto Accept1
l03240: %#
l03260: %#####:          lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
l03290: %#####:          lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
l03320: %#####:          lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */
l03350: %Accept1:# accept                                                ~
        ~       #

l03380: %#            at (01,02),                                        ~
        ~       #

l03410: %#               "##########################################     ~
        ~       #

l03440: %#            at (01,66), "Today:",                              ~
        ~       #

l03470: %#            at (01,73), fac(hex(8c)), date$                  , ~
        ~ch(08),#

l03500: %#            at (02,02), fac(hex(ac)), line2$                 , ~
        ~ch(79),#

l03530: %#            at (04,02), fac(hex(94)), errormsg$              , ~
        ~ch(79),#

l03560: %#            at (06,30), fac(hex(ac)),   columnttl$           , ~
        ~ch(51),#

l03590: %             at (###02), "##################################### ~
        ~       #

l03620: %#            at (###30), fac(lfac$(##)), ###################  , ~
        ~ch(##),#

l03650: %#            at (###56), fac(lfac$(##)), ###################  , ~
        ~ch(##),#

l03680: %#            at (21,02), fac(hex(a4)),   inpmessage$          , ~
        ~ch(79),#

        %#            at (21,02), fac(hex(a4)),   edtmessage$          , ~
        ~ch(79),#

l03740: %#            at (22,02), fac(hex(8c)),   pf$(1)               , ~
        ~ch(79),#

l03770: %#            at (23,02), fac(hex(8c)),   pf$(2)               , ~
        ~ch(79),#

l03800: %#            at (24,02), fac(hex(8c)),   pf$(3)               , ~
        ~ch(79),#

L03830: %#                                                               ~
        ~       #

l03860: %#            keys(pfkeys$), key(keyhit%)

l03880: %#            if keyhit% <> 13% then Acc02                       ~

l03900: %#               call "MANUAL" ("########") : goto Accept1       ~

        %#                                                               ~

l03940: %#Acc02:      if keyhit% <> 15% then Acc03                       ~

l03960: %#               call "PRNTSCRN" : goto Accept1                  ~

L03980: %#                                                               ~

l04000: %#Acc03:      close ws                                           ~

l04020: %#            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())~

l04040: %#            return                                             ~

l04060: %#     set_pf1

l04080: %#     if edit% = 2% then Spf04     /*  Input Mode             */

L04100: %#         pf$(1%) = "(1)Start Over                           " & ~
        ~       #

L04130: %#                  "                       (13)Instructions"

L04150: %#         pf$(2%) = "                 (4)Previous Field      " & ~
        ~       #

L04180: %#                  "                       (15)Print Screen"

L04200: %#         pf$(3%) = "                                        " & ~
        ~       #

L04230: %#                  "                       (16)Exit Program"

l04250: %#         pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)

        %#         return

l04290: %#         if fieldnr% = 1% then Spf02

l04310: %#             str(pf$(3%),64%)    = " "  :  str(pfkeys$,16,1) = he~
        ~x(ff)

l04340: %#         if fieldnr% > 1% then Spf03

l04360: %#Spf02:       str(pf$(2%),18%,26%) = " "  :  str(pfkeys$, 4,1) = he~
        ~x(ff)

l04390: %#Spf03:   return

l04410: %#Spf04: if fieldnr% > 0% then Spf05  /*  Edit Mode - Select Fld */

L04430: %#         pf$(1%) = "(1)Start Over                           " & ~
        ~       #

L04460: %#                  "                       (13)Instructions"

L04480: %#         pf$(2%) = "                                        " & ~
        ~       #

        %#         pf$(2%) = "                 (4)Previous Screen     " & ~
        ~       #

L04540: %#                  "                       (15)Print Screen"

L04560: %#         pf$(3%) = "                                        " & ~
        ~       #

        %#         pf$(3%) = "                 (5)Next Screen         " & ~
        ~       #


L04630: %#                  "                       (16)Print Report"

l04650: %#         pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)

        %#         pfkeys$ = hex(01ffffff05ffffffffffffff0dff0f1000)

        %#         pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)

        %#         pfkeys$ = hex(01ffff0405ffffffffffffff0dff0f1000)

l04730: %#         return


L04760: %#Spf05                             /*  Edit Mode - Enabled    */

L04780: %#         pf$(1%) = "(1)Start Over                           " & ~
        ~       #

L04810: %#                  "                       (13)Instructions"

L04830: %#         pf$(2%) = "                                        " & ~
        ~       #

L04860: %#                  "                       (15)Print Screen"

L04880: %#         pf$(3%) = "                                        " & ~
        ~       #

L04910: %#                  "                                       "

l04930: %#         pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)

l04950: %#         return


        REM Set LINE$ with the specified increment.
            deffn'180(temp%)
                line$ = " "
                if temp% = 0% then return

                if temp% > 0% then new_label
                convert -temp% to line$, pic(00)
                go to create_label

new_label
                linenumber% = linenumber% + 1%
                convert linenumber% to line$, pic(00)

create_label
                line$ = "Scr" & line$
            return

WriteToFile
       write #1 using file_fmt, writeLn$
       writeLn$ = " "
          return

file_fmt: %############################################################~
####################
