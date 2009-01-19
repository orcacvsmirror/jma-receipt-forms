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
       01  S138G2.
              03  S138G2-PAGE              PIC  X(03).
              03  S138G2-SRYYM             PIC  X(16).
              03  S138G2-SYSYMD            PIC  X(22).
              03  S138G2-NYUGAI            PIC  X(04).
              03  S138G2-SRYKANAME         PIC  X(20).
              03  S138G2-CITYKBN           PIC  X(12).
              03  S138G2-HKNJA-AREA.
                  05  S138G2-HKNJA-TBL OCCURS 55.
                      07  S138G2-HKNJANAME PIC  X(24).
                      07  S138G2-KENSU     PIC  X(04).
                      07  S138G2-NISSU     PIC  X(04).
                      07  S138G2-TENSU     PIC  X(09).
                      07  S138G2-MADOGK    PIC  X(09).
      *--- (2008/12/11) line add start monma ---------------------------
                      07  S138G2-JIHIGK    PIC  X(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                      07  S138G2-HKNGK     PIC  X(09).
                      07  S138G2-KOHGK     PIC  X(09).
                      07  S138G2-SYUEKI    PIC  X(09).
