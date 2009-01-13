      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      *  プログラム修正履歴
      *              @@@@-高嶋    06/10/21  自費追加対応
      *              @@@@-門間    08/11/04  1枚対応
      ********************************************************************
       01  HC17.
              03  HC17-SYSYMD      PIC X(22).
              03  HC17-SRYYMD      PIC X(22).
              03  HC17-PAGE        PIC X(3).
              03  HC17-MIDASI      PIC X(10).
              03  HC17-JOBMSG      PIC X(60).
              03  HC17-NYUGAIKBN   PIC X(4).
              03  HC17-SRYKA       PIC X(20).
              03  HC17-TJIHI-G     OCCURS  10 TIMES.
                05  HC17-TJIHI         PIC X(8).
      *--- 08/11/04 LINE ADD START MONMA -----------------------
      *        03  HC17-RENNUM-G    OCCURS  30 TIMES.
              03  HC17-RENNUM-G    OCCURS  21 TIMES.
      *--- 08/11/04 LINE ADD END   MONMA -----------------------
                05  HC17-RENNUM        PIC X(7).
      *--- 08/11/04 LINE ADD START MONMA -----------------------
      *        03  HC17-PTNUM-G     OCCURS  30 TIMES.
              03  HC17-PTNUM-G     OCCURS  21 TIMES.
      *--- 08/11/04 LINE ADD END   MONMA -----------------------
                05  HC17-PTNUM         PIC X(8).
      *--- 08/11/04 LINE ADD START MONMA -----------------------
      *        03  HC17-NAME-G      OCCURS  30 TIMES.
              03  HC17-NAME-G      OCCURS  21 TIMES.
      *--- 08/11/04 LINE ADD END   MONMA -----------------------
                05  HC17-NAME          PIC X(18).
      *--- 08/11/04 LINE ADD START MONMA -----------------------
      *        03  HC17-HKNTEN-G    OCCURS  30 TIMES.
              03  HC17-HKNTEN-G    OCCURS  21 TIMES.
      *--- 08/11/04 LINE ADD END   MONMA -----------------------
                05  HC17-HKNTEN        PIC X(7).
      *--- 08/11/04 LINE ADD START MONMA -----------------------
      *        03  HC17-MONEY-G     OCCURS  16 TIMES.
              03  HC17-MONEY-G     OCCURS  20 TIMES.
      *          05  HC17-MONEY-OCC     OCCURS  30 TIMES.
                05  HC17-MONEY-OCC     OCCURS  21 TIMES.
                  07  HC17-MONEY           PIC X(7).
      *        03  HC17-SYUEKI-G    OCCURS  30 TIMES.
              03  HC17-SYUEKI-G    OCCURS  21 TIMES.
      *--- 08/11/04 LINE ADD END   MONMA -----------------------
                05  HC17-SYUEKI        PIC X(8).
              03  HC17-G-HKNTEN    PIC X(7).
      *--- 08/11/04 1枚対応  START MONMA -----------------------
      *        03  HC17-G-MONEY-TBL OCCURS  16 TIMES.
              03  HC17-G-MONEY-TBL OCCURS  20 TIMES.
      *--- 08/11/04 1枚対応  END   MONMA -----------------------
                05  HC17-G-MONEY       PIC X(7).
      *--- 08/11/04 LINE ADD START MONMA -----------------------
      *        03  HC17-SEIKYU-G    OCCURS  30 TIMES.
              03  HC17-SEIKYU-G    OCCURS  21 TIMES.
                05  HC17-SEIKYU        PIC X(8).
      *        03  HC17-RYOSYU-G    OCCURS  30 TIMES.
              03  HC17-RYOSYU-G    OCCURS  21 TIMES.
                05  HC17-RYOSYU        PIC X(8).
              03  HC17-G-SYUEKI    PIC X(8).
      *--- 08/11/04 LINE ADD END   MONMA -----------------------
              03  HC17-GG-HKNTEN   PIC X(8).
      *--- 08/11/04 1枚対応  START MONMA -----------------------
      *        03  HC17-GG-MONEY-TBL OCCURS  16 TIMES.
              03  HC17-GG-MONEY-TBL    OCCURS  20 TIMES.
      *--- 08/11/04 1枚対応  END   MONMA -----------------------
                05  HC17-GG-MONEY      PIC X(8).
              03  HC17-GG-SYUEKI   PIC X(9).
      *--- 08/11/04 LINE ADD START MONMA -----------------------
              03  HC17-G-SEIKYU    PIC X(8).
              03  HC17-G-RYOSYU    PIC X(8).
              03  HC17-GG-SEIKYU   PIC X(9).
              03  HC17-GG-RYOSYU   PIC X(9).
      *--- 08/11/04 LINE ADD END   MONMA -----------------------
