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
      *              @@@@-高嶋    06/11/14  自費、基本診療項目追加対応
      *              @@@@-田中    08/04/01  H20年4月改正対応
      ********************************************************************
       01  D134.
              03  D134-TITLE.
                05  D134-SUBTITLE   PIC X(30).
                05  D134-JOBMSG     PIC X(60).
                05  D134-PAGE       PIC X(3).
                05  D134-SYSYMD     PIC X(22).
                05  D134-FROMYMD    PIC X(22).
                05  D134-TOYMD      PIC X(22).
                05  D134-SRYKA      PIC X(20).
                05  D134-DRNAME     PIC X(30).
                05  D134-TENKI-G            OCCURS  3 TIMES.
                  07  D134-TENKI    PIC X(22).
              03  D134-DATA.
                05  D134-TDAY-G             OCCURS  15 TIMES.
                  07  D134-TDAY     PIC X(10).
                05  D134-YOSO-G             OCCURS  15 TIMES.
      * 20080401  H20年4月改正対応START tanaka
      *            07  D134-YOSO-OCC         OCCURS  37 TIMES.
                  07  D134-YOSO-OCC         OCCURS  38 TIMES.
      * 20080401  H20年4月改正対応END
                    09  D134-YOSO   PIC X(11).
