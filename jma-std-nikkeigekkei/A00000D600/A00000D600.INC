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
      ********************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
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
              03  D600-TRYO           PIC X(8).
              03  D600-RENNUM-TBL                 OCCURS  21 TIMES.
                  05  D600-RENNUM     PIC X(7).
              03  D600-PTNUM-TBL                  OCCURS  21 TIMES.
                  05  D600-PTNUM      PIC X(8).
              03  D600-NAME-TBL                   OCCURS  21 TIMES.
                  05  D600-NAME       PIC X(18).
              03  D600-SEX-TBL                    OCCURS  21 TIMES.
                  05  D600-SEX        PIC X(2).
              03  D600-AGE-TBL                    OCCURS  21 TIMES.
                  05  D600-AGE        PIC X(3).
              03  D600-HKNKBN-TBL                 OCCURS  21 TIMES.
                  05  D600-HKNKBN     PIC X(2).
              03  D600-HKNS-TBL                   OCCURS  21 TIMES.
                  05  D600-HKNS       PIC X(8).
              03  D600-HKNTEN-TBL                 OCCURS  21 TIMES.
                  05  D600-HKNTEN     PIC X(7).
              03  D600-TEN-HK-TBL                 OCCURS  16 TIMES.
                  05  D600-TEN-TBL                OCCURS  21 TIMES.
                      07  D600-TEN    PIC X(7).
              03  D600-G-HKNTEN       PIC X(7).
              03  D600-G-TEN-HK-TBL               OCCURS  16 TIMES.
                  05  D600-G-TEN      PIC X(7).
              03  D600-GG-HKNTEN      PIC X(8).
              03  D600-GG-TEN-HK-TBL              OCCURS  16 TIMES.
                  05  D600-GG-TEN     PIC X(8).
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
