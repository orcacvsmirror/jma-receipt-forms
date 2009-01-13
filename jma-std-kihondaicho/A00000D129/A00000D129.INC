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
       01  D129.
              03  D129-TITLE.
                05  D129-SUBTITLE   PIC X(30).
                05  D129-JOBMSG     PIC X(60).
                05  D129-PAGE       PIC X(3).
                05  D129-SYSYMD     PIC X(22).
                05  D129-FRYMD      PIC X(22).
                05  D129-TOYMD      PIC X(22).
                05  D129-SRYKA      PIC X(20).
                05  D129-DRNAME     PIC X(30).
              03  D129-DATA.
                05  D129-TAGE-G         OCCURS  14 TIMES.
                  07  D129-TAGE     PIC X(12).
                05  D129-YOSO-G         OCCURS  14 TIMES.
      * 20080401 H20年4月改正対応START tanaka
      *            07  D129-YOSO-OCC     OCCURS  35 TIMES.
                  07  D129-YOSO-OCC     OCCURS  36 TIMES.
      * 20080401 H20年4月改正対応END
                    09  D129-YOSO   PIC X(11).
