*       ****************************************************************~
*                           ( As of 11/17/97 - RHH Check R6.04.03 )    *~
*        APCPAUSE - Automatic Program Delay. Start Operation at the    *~
*                   Specified Date and Time.                           *~
*                                                                      *~
*       ****************************************************************~
*       03/31/98  ERN  Y2K modifications                               *~
*       ****************************************************************

             sub "APCPAUSE" (apc%,       /* 0% = Start, 1% = Exit  */    ~
                             prog$ )     /* Program Name              */ ~

        dim cursor%(2%),                 /* Field Positions           */ ~
            i$(24%)80,                   /* Screen Images             */ ~
            pfkeys$32,                   /* PF Keys                   */ ~
            errormsg$79, tic$31,         /* Error Messages            */ ~
            disp_msg$79, date$10,        /* Prompt Message            */ ~
            start_time$5, save_time$5,   /* Start Time                */ ~
            start_date$10, save_date$10, /* Start Date                */ ~
            prog$8                       /* Delayed Program Name      */


            time% = 0%
            mat cursor% = zer
        REM Main Line Program
            start_date$ = date
            call "DATEOKC" (start_date$, date%, errormsg$ )
            save_date$ = start_date$
            save_date% = date%
            date$ = start_date$
            tic$, start_time$ = " "
            str(start_time$,1%,4%) = time
            call "TIMEOK" (start_time$, time%, errormsg$)
            save_time$ = start_time$
        REM Display Entry Screen and Enter Date and Time

        input_data
            start_date$ = save_date$
            gosub entry_screen
            errormsg$ = " "
            if keyhit% = 16% then goto exit_program
                                                         /* Edit  Date */
            call "DATEOKC" (start_date$, date% , errormsg$)
            if date% = 0% then goto input_data
            if date% >= save_date% then goto L00470
               date% = save_date%
               start_date$ = save_date$

L00470:     save_date% = date%
            save_date$ = start_date$
                                                         /* Check Time */
            call "TIMEOK" (start_time$, time%, errormsg$ )
            if errormsg$ = " " then goto L00550
               start_time$ = save_time$
               goto input_data

L00550:     gosub display_screen

        display_data
            if keyhit% = 16% then goto exit_program
                                                         /* Check Date */
            call "PAUSE" addr(1000%)
            gosub mouse
            tic% = tic% + 1%
            str(tic$,tic%,2%) = "->"
            if tic% < 30% then goto L00680
               tic% = 1%
               print at(19,26);hex(84);"                                "
               str(tic$,1%,31%) = "->"
L00680:     print at(19,26);hex(84);tic$
            save_date$ = date
            call "DATEOKC" (save_date$, date%, errormsg$)
            if save_date% = date% then goto check_time
               goto display_data

        check_time
            save_time$ = " "
            str(save_time$,1%,4%) = time
            call "TIMEOK" (save_time$, time%, errormsg$ )
            if save_time$ >= start_time$ then goto exit_program

            goto display_data


        entry_screen
            disp_msg$ =                                                  ~
         "Enter Beginning Date and Time and Press <Return>, PF(16) Exit."
            pfkeys$ = hex(01ffffffffffffffffffffffffffff1000)
            u3% = 0%
            accept                                                       ~
               at (01,23),                                               ~
                  "APC Building Products Program Delay",                 ~
               at (02,66), "Today:",                                     ~
               at (02,73), fac(hex(8c)), date$                  , ch(10),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (10,02), "Starting Date  :",                           ~
               at (10,20), fac(hex(81)), start_date$            , ch(10),~
                                                                         ~
               at (12,02), "Starting Time  :",                           ~
               at (12,20), fac(hex(81)), start_time$            , ch(05),~
                                                                         ~
               at (23,02), fac(hex(a4)),   disp_msg$            , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        display_screen

            display                                                      ~
               at (09,15),                                               ~
                  "***************************************************", ~
               at (10,15),                                               ~
                  "**   Delay Start of ( xxxxxxxx )                 **", ~
               at (11,15),                                               ~
                  "**                                               **", ~
               at (12,15),                                               ~
                  "**   Starting Date :     xxxxxxxxxx              **", ~
               at (13,15),                                               ~
                  "**                                               **", ~
               at (14,15),                                               ~
                  "**   Starting Time :     xxxxx                   **", ~
               at (15,15),                                               ~
                  "**                                               **", ~
               at (16,15),                                               ~
                  "**              ( C O U N T I N G )              **", ~
               at (17,15),                                               ~
                  "***************************************************", ~
               at (10,37), prog$,                                        ~
               at (12,40), start_date$,                                  ~
               at (14,40), start_time$

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        mouse
          cl% = 10%
          for rw% = 9% to 19%
              print at(rw%-1%,cl%);hex(84);"  ";                         ~
                    at(rw%,cl%);hex(84);"->";
          next rw%
          for cl% = 10% to 26%
              print at(rw%,cl% - 2%);hex(84);"  ";                       ~
                    at(rw%,cl%);hex(84);"->";
          next cl%
        return

        exit_program
            apc% = 0%
            if keyhit% = 16% then apc% = 1%
        end
