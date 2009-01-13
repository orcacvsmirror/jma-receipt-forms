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
       01  D130.
              03  D130-TITLE.
                05  D130-SUBTITLE   PIC X(30).
                05  D130-JOBMSG     PIC X(60).
                05  D130-PAGE       PIC X(3).
                05  D130-SYSYMD     PIC X(22).
                05  D130-FRYMD      PIC X(22).
                05  D130-TOYMD      PIC X(22).
                05  D130-SRYKA      PIC X(20).
                05  D130-DRNAME     PIC X(30).
                05  D130-TENKI-G            OCCURS  3 TIMES.
                  07  D130-TENKI    PIC X(22).
              03  D130-DATA.
                05  D130-TAGE-G             OCCURS  14 TIMES.
                  07  D130-TAGE     PIC X(12).
                05  D130-YOSO-G             OCCURS  14 TIMES.
      * 20080401 H20年4月改正対応START tanaka
      *            07  D130-YOSO-OCC         OCCURS  37 TIMES.
                  07  D130-YOSO-OCC         OCCURS  38 TIMES.
      * 20080401 H20年4月改正対応END
                    09  D130-YOSO   PIC X(11).
