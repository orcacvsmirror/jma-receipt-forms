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
      *              @@@@-高嶋    06/10/28  項目追加対応
      *              @@@@-田中    08/03/25  H20年4月改正対応
      *              @@@@-田中    08/11/05  帳票枚数を1枚に変更
      ********************************************************************
       01  D600.
              03  D600-JOBMSG         PIC X(60).
              03  D600-PAGE           PIC X(3).
              03  D600-SYSYMD         PIC X(22).
              03  D600-MIDASI         PIC X(10).
              03  D600-NYUHEN-YMD     PIC X(22).
              03  D600-NYUGAIKBN      PIC X(4).
              03  D600-SRYKA          PIC X(20).
              03  D600-NYUINRYO       PIC X(8).
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *        03  D600-TRYO           PIC X(12).
              03  D600-TRYO           PIC X(8).
      *        03  D600-RENNUM-TBL                 OCCURS  30 TIMES.
              03  D600-RENNUM-TBL                 OCCURS  21 TIMES.
      *--- 2008/11/05  CHANGE  END -------------------------------------
                  05  D600-RENNUM     PIC X(7).
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *        03  D600-PTNUM-TBL                  OCCURS  30 TIMES.
              03  D600-PTNUM-TBL                  OCCURS  21 TIMES.
      *--- 2008/11/05  CHANGE  END -------------------------------------
                  05  D600-PTNUM      PIC X(8).
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *        03  D600-NAME-TBL                   OCCURS  30 TIMES.
              03  D600-NAME-TBL                   OCCURS  21 TIMES.
      *--- 2008/11/05  CHANGE  END -------------------------------------
                  05  D600-NAME       PIC X(18).
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *        03  D600-SEX-TBL                    OCCURS  30 TIMES.
              03  D600-SEX-TBL                    OCCURS  21 TIMES.
      *--- 2008/11/05  CHANGE  END -------------------------------------
                  05  D600-SEX        PIC X(2).
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *        03  D600-AGE-TBL                    OCCURS  30 TIMES.
              03  D600-AGE-TBL                    OCCURS  21 TIMES.
      *--- 2008/11/05  CHANGE  END -------------------------------------
                  05  D600-AGE        PIC X(3).
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *        03  D600-HKNKBN-TBL                 OCCURS  30 TIMES.
              03  D600-HKNKBN-TBL                 OCCURS  21 TIMES.
      *--- 2008/11/05  CHANGE  END -------------------------------------
                  05  D600-HKNKBN     PIC X(2).
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *        03  D600-HKNS-TBL                   OCCURS  30 TIMES.
              03  D600-HKNS-TBL                   OCCURS  21 TIMES.
      *--- 2008/11/05  CHANGE  END -------------------------------------
                  05  D600-HKNS       PIC X(8).
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *        03  D600-HKNTEN-TBL                 OCCURS  30 TIMES.
              03  D600-HKNTEN-TBL                 OCCURS  21 TIMES.
      *--- 2008/11/05  CHANGE  END -------------------------------------
                  05  D600-HKNTEN     PIC X(7).
      * 20080325 H20年4月改正対応STRAT tanaka
      *        03  D600-TEN-HK-TBL                 OCCURS  15 TIMES.
              03  D600-TEN-HK-TBL                 OCCURS  16 TIMES.
      * 20080325 H20年4月改正対応END
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *            05  D600-TEN-TBL                OCCURS  30 TIMES.
                  05  D600-TEN-TBL                OCCURS  21 TIMES.
      *--- 2008/11/05  CHANGE  END -------------------------------------
                      07  D600-TEN    PIC X(7).
              03  D600-G-HKNTEN       PIC X(7).
      * 20080325 H20年4月改正対応STRAT tanaka
      *        03  D600-G-TEN-HK-TBL               OCCURS  15 TIMES.
      * 20080325 H20年4月改正対応END
              03  D600-G-TEN-HK-TBL               OCCURS  16 TIMES.
                  05  D600-G-TEN      PIC X(7).
              03  D600-GG-HKNTEN      PIC X(8).
      * 20080325 H20年4月改正対応STRAT tanaka
      *        03  D600-GG-TEN-HK-TBL              OCCURS  15 TIMES.
      * 20080325 H20年4月改正対応END
              03  D600-GG-TEN-HK-TBL              OCCURS  16 TIMES.
                  05  D600-GG-TEN     PIC X(8).
      *--- 2008/11/05  CHANGE  START -----------------------------------
              03  D600-HKNFTN-TBL                 OCCURS  21 TIMES.
                  05  D600-HKNFTN    PIC X(8).
              03  D600-G-HKNFTN      PIC X(8).
              03  D600-GG-HKNFTN     PIC X(9).
              03  D600-JIHIFTN-TBL                OCCURS  21 TIMES.
                  05  D600-JIHIFTN   PIC X(8).
              03  D600-G-JIHIFTN     PIC X(8).
              03  D600-GG-JIHIFTN    PIC X(9).
              03  D600-SEIKYU-TBL                 OCCURS  21 TIMES.
                  05  D600-SEIKYU    PIC X(8).
              03  D600-G-SEIKYU      PIC X(8).
              03  D600-GG-SEIKYU     PIC X(9).
              03  D600-RYOSYU-TBL                 OCCURS  21 TIMES.
                  05  D600-RYOSYU    PIC X(8).
              03  D600-G-RYOSYU      PIC X(8).
              03  D600-GG-RYOSYU     PIC X(9).
      *--- 2008/11/05  CHANGE  END -------------------------------------
