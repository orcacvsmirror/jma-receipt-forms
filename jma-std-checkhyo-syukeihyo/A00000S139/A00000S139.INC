      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
       01  S139.
              03  S139-PAGE              PIC  X(03).
              03  S139-SRYYM             PIC  X(16).
              03  S139-SYSYMD            PIC  X(22).
              03  S139-SRYKANAME         PIC  X(20).
              03  S139-MEISAI-AREA.
                  05  S139-NYUGAI-TBL OCCURS 1.
      * 20080423 H20Ç¯4·î²þÀµÂÐ±þSTART tanaka
      *                07  S139-HKN-TBL OCCURS  16.
                      07  S139-HKN-TBL OCCURS  18.
      * 20080423 H20Ç¯4·î²þÀµÂÐ±þEND
                          09  S139-KENSU      PIC  X(04).
                          09  S139-NISSU      PIC  X(04).
                          09  S139-TENSU      PIC  X(09).
                          09  S139-MADOGK     PIC  X(09).
                          09  S139-HKNGK      PIC  X(09).
                          09  S139-KOHGK      PIC  X(09).
                          09  S139-SYUEKI     PIC  X(09).
      * 20080423 H20Ç¯4·î²þÀµÂÐ±þSTART tanaka
      *    £±¡¢¼ÒÊÝ £²¡¢¹ñÊÝ £³¡¢¸øÈñ £´¡¢ÊÝ¸±¹ç·× £µ¡¢¸å´ü¹âÎð
      *                07  S139-SYOKEI-TBL OCCURS 4.
                      07  S139-SYOKEI-TBL OCCURS 5.
      * 20080423 H20Ç¯4·î²þÀµÂÐ±þEND
                          09  S139-S-KENSU      PIC  X(04).
                          09  S139-S-NISSU      PIC  X(04).
                          09  S139-S-TENSU      PIC  X(09).
                          09  S139-S-MADOGK     PIC  X(09).
                          09  S139-S-HKNGK      PIC  X(09).
                          09  S139-S-KOHGK      PIC  X(09).
                          09  S139-S-SYUEKI     PIC  X(09).
      * 20080423 H20Ç¯4·î²þÀµÂÐ±þSTART tanaka
      *    £±¡¢¼«Èñ £²¡¢Ï«ºÒ £³¡¢¼«ÇåÀÕ £´¡¢¸ø³² £µ¡¢¹ç·×
      *                07  S139-ETC-TBL OCCURS 4.
                      07  S139-ETC-TBL OCCURS 5.
      * 20080423 H20Ç¯4·î²þÀµÂÐ±þEND
                          09  S139-ETC-KENSU      PIC  X(04).
                          09  S139-ETC-NISSU      PIC  X(04).
                          09  S139-ETC-TENSU      PIC  X(09).
                          09  S139-ETC-MADOGK     PIC  X(09).
                          09  S139-ETC-HKNGK      PIC  X(09).
                          09  S139-ETC-KOHGK      PIC  X(09).
                          09  S139-ETC-SYUEKI     PIC  X(09).
      * 20080423 H20Ç¯4·î²þÀµÂÐ±þSTART tanaka
      *    Áí¹ç·×¥¨¥ê¥¢ £± ÊÝ¸± £² ¼«Èñ £³ Ï«ºÒ £´ ¼«ÇåÀÕ £µ ¸ø³² £¶ Áí¹ç·× 
      *            05  S139-GOKEI-TBL OCCURS 5.
                  05  S139-GOKEI-TBL OCCURS 6.
      * 20080423 H20Ç¯4·î²þÀµÂÐ±þEND
                      07  S139-GKENSU      PIC  X(04).
                      07  S139-GNISSU      PIC  X(04).
                      07  S139-GTENSU      PIC  X(09).
                      07  S139-GMADOGK     PIC  X(09).
                      07  S139-GHKNGK      PIC  X(09).
                      07  S139-GKOHGK      PIC  X(09).
                      07  S139-GSYUEKI     PIC  X(09).
                      07  S139-SGNISSU     PIC  X(05).
                      07  S139-SGSEIKYU    PIC  X(09).
                      07  S139-SGFTNGK     PIC  X(09).


