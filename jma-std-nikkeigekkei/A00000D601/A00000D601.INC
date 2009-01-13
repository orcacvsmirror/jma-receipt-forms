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
      *              @@@@-高嶋    06/10/30  基本診療項目追加対応
      *              @@@@-田中    08/03/26  H20年4月改正対応
      ********************************************************************
       01  D601.
              03  D601-JOBMSG      PIC X(60).
              03  D601-PAGE        PIC X(3).
              03  D601-SYSYMD      PIC X(22).
              03  D601-MIDASI      PIC X(10).
              03  D601-NYUHEN-YMD  PIC X(22).
              03  D601-NYUGAIKBN   PIC X(4).
              03  D601-SRYKA       PIC X(20).
              03  D601-NYUINRYO    PIC X(8).
              03  D601-TRYO        PIC X(12).
              03  D601-TJYUSHIN    PIC X(8).
              03  D601-KENSU1      PIC X(8).
              03  D601-KENSU2      PIC X(8).
              03  D601-KENSU3      PIC X(10).
              03  D601-JYUSHINCNT  PIC X(5).
              03  D601-SHKNKEN     PIC X(5).
              03  D601-RHKNKEN     PIC X(5).
              03  D601-OHKNKEN     PIC X(5).
              03  D601-GHKNTEN     PIC X(9).
      * 20080326 H20年4月改正対応START tanaka
      *        03  D601-TEN-TBL                OCCURS  15 TIMES.
              03  D601-TEN-TBL                OCCURS  16 TIMES.
      * 20080326 H20年4月改正対応END
                  05  D601-TEN     PIC X(9).
              03  D601-HKNFTN      PIC X(9).
              03  D601-JIHIFTN     PIC X(9).
              03  D601-GSEIKYU     PIC X(9).
              03  D601-GRYOSYU     PIC X(9).
