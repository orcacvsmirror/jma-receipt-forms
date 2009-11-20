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
       01  S138TMP-REC.
              03  S138TMP-KEY.
                  05  S138TMP-NYUGAIKBN       PIC  X(01).
                  05  S138TMP-SRYKA           PIC  X(02).
      * 20080512 ÊÖÌá¤¬Àµ¤·¤¯°õ»ú¤µ¤ì¤Ê¤¤ÉÔ¶ñ¹ç¤ò½¤ÀµSTART tanaka
                  05  S138TMP-SRYYMKBN           PIC  X(01).
      * 20080512 ÊÖÌá¤¬Àµ¤·¤¯°õ»ú¤µ¤ì¤Ê¤¤ÉÔ¶ñ¹ç¤ò½¤ÀµEND
              03  S138TMP-KOKUHO-TBL OCCURS 14.
                  05  S138TMP-KENSU      PIC  9(04).
                  05  S138TMP-NISSU      PIC  9(04).
                  05  S138TMP-TENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *            05  S138TMP-MADOGK     PIC  9(09).
                  05  S138TMP-MADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/11) line add start monma ---------------------------
                  05  S138TMP-JIHIGK     PIC  9(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                  05  S138TMP-HKNGK      PIC  9(09).
                  05  S138TMP-KOHGK      PIC  9(09).
                  05  S138TMP-SYUEKI     PIC  9(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *            05  S138TMP-SNISSU     PIC  9(05).
                  05  S138TMP-SNISSU     PIC  9(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
                  05  S138TMP-SSEIKYU    PIC  9(09).
                  05  S138TMP-SFTNGK     PIC  9(09).
              03  S138TMP-KOH1-MAX       PIC  9(02).
      * 20080625 ÊÝ¸±¼Ô¡¢¸øÈñ¾ðÊóÎÎ°è100 ¢ª 300¤ËÊÑ¹¹START tanaka
      *        03  S138TMP-KOHI1-TBL OCCURS 100.
              03  S138TMP-KOHI1-TBL OCCURS 300.
      * 20080625 ÊÝ¸±¼Ô¡¢¸øÈñ¾ðÊóÎÎ°è100 ¢ª 300¤ËÊÑ¹¹END
                  05  S138TMP-KOH1-KEY.
                      07  S138TMP-RJNKBN       PIC  X(03).
                      07  S138TMP-KOHNUM       PIC  X(03).
                  05  S138TMP-KKENSU      PIC  9(04).
                  05  S138TMP-KNISSU      PIC  9(04).
                  05  S138TMP-KTENSU      PIC  9(09).
      * 20080625 ÊÝ¸±¼Ô¡¢¸øÈñ¾ðÊóÎÎ°è100 ¢ª 300¤ËÊÑ¹¹START tanaka
      *        03  S138TMP-HKNJA1-TBL OCCURS 100.
              03  S138TMP-HKNJA1-TBL OCCURS 300.
      * 20080625 ÊÝ¸±¼Ô¡¢¸øÈñ¾ðÊóÎÎ°è100 ¢ª 300¤ËÊÑ¹¹END
      *--- 2009/11/18 CHANGE START ----------------------------------------
      *            05  S138TMP-HKNJANUM    PIC  X(06).
                  05  S138TMP-HKNJANUM-KEY.
                      07  S138TMP-HKNJANUM    PIC  X(06).
      *--- 2009/11/18 CHANGE END ------------------------------------------
                  05  S138TMP-HKENSU      PIC  9(04).
                  05  S138TMP-HNISSU      PIC  9(04).
                  05  S138TMP-HTENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *            05  S138TMP-HMADOGK     PIC  9(09).
                  05  S138TMP-HMADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/11) line add start monma ---------------------------
                  05  S138TMP-HJIHIGK     PIC  9(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                  05  S138TMP-HHKNGK      PIC  9(09).
                  05  S138TMP-HKOHGK      PIC  9(09).
                  05  S138TMP-HSYUEKI     PIC  9(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *            05  S138TMP-HSNISSU     PIC  9(04).
                  05  S138TMP-HSNISSU     PIC  9(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
                  05  S138TMP-HSSEIKYU    PIC  9(09).
                  05  S138TMP-HSFTNGK     PIC  9(09).
              03  S138TMP-KUMIAI-TBL OCCURS 11.
                  05  S138TMP-K-KENSU      PIC  9(04).
                  05  S138TMP-K-NISSU      PIC  9(04).
                  05  S138TMP-K-TENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *            05  S138TMP-K-MADOGK     PIC  9(09).
                  05  S138TMP-K-MADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/11) line add start monma ---------------------------
                  05  S138TMP-K-JIHIGK     PIC  9(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                  05  S138TMP-K-HKNGK      PIC  9(09).
                  05  S138TMP-K-KOHGK      PIC  9(09).
                  05  S138TMP-K-SYUEKI     PIC  9(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *            05  S138TMP-K-SNISSU     PIC  9(04).
                  05  S138TMP-K-SNISSU     PIC  9(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
                  05  S138TMP-K-SSEIKYU    PIC  9(09).
                  05  S138TMP-K-SFTNGK     PIC  9(09).
              03  S138TMP-KOH2-MAX       PIC  9(02).
      * 20080625 ÊÝ¸±¼Ô¡¢¸øÈñ¾ðÊóÎÎ°è100 ¢ª 300¤ËÊÑ¹¹START tanaka
      *        03  S138TMP-KOHI2-TBL OCCURS 100.
              03  S138TMP-KOHI2-TBL OCCURS 300.
      * 20080625 ÊÝ¸±¼Ô¡¢¸øÈñ¾ðÊóÎÎ°è100 ¢ª 300¤ËÊÑ¹¹END
                  05  S138TMP-KOH2-KEY.
                      07  S138TMP-K-RJNKBN       PIC  X(03).
                      07  S138TMP-K-KOHNUM       PIC  X(03).
                  05  S138TMP-K-KKENSU      PIC  9(04).
                  05  S138TMP-K-KNISSU      PIC  9(04).
                  05  S138TMP-K-KTENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *            05  S138TMP-K-KMADOGK     PIC  9(09).
                  05  S138TMP-K-KMADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/11) line add start monma ---------------------------
                  05  S138TMP-K-KJIHIGK     PIC  9(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                  05  S138TMP-K-KHKNGK      PIC  9(09).
                  05  S138TMP-K-KKOHGK      PIC  9(09).
                  05  S138TMP-K-KSYUEKI     PIC  9(09).
      * 20080625 ÊÝ¸±¼Ô¡¢¸øÈñ¾ðÊóÎÎ°è100 ¢ª 300¤ËÊÑ¹¹START tanaka
      *        03  S138TMP-HKNJA2-TBL OCCURS 100.
              03  S138TMP-HKNJA2-TBL OCCURS 300.
      * 20080625 ÊÝ¸±¼Ô¡¢¸øÈñ¾ðÊóÎÎ°è100 ¢ª 300¤ËÊÑ¹¹END
      *--- 2009/11/18 CHANGE START ----------------------------------------
      *            05  S138TMP-K-HKNJANUM    PIC  X(06).
                  05  S138TMP-K-HKNJANUM-KEY.
                      07  S138TMP-K-HKNJANUM    PIC  X(06).
      *--- 2009/11/18 CHANGE END ------------------------------------------
                  05  S138TMP-K-HKENSU      PIC  9(04).
                  05  S138TMP-K-HNISSU      PIC  9(04).
                  05  S138TMP-K-HTENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *            05  S138TMP-K-HMADOGK     PIC  9(09).
                  05  S138TMP-K-HMADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/11) line add start monma ---------------------------
                  05  S138TMP-K-HJIHIGK     PIC  9(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                  05  S138TMP-K-HHKNGK      PIC  9(09).
                  05  S138TMP-K-HKOHGK      PIC  9(09).
                  05  S138TMP-K-HSYUEKI     PIC  9(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *            05  S138TMP-K-HSNISSU     PIC  9(04).
                  05  S138TMP-K-HSNISSU     PIC  9(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
                  05  S138TMP-K-HSSEIKYU    PIC  9(09).
                  05  S138TMP-K-HSFTNGK     PIC  9(09).
              03  S138TMP-GOKEI-TBL.
                  05  S138TMP-GKENSU      PIC  9(04).
                  05  S138TMP-GNISSU      PIC  9(04).
                  05  S138TMP-GTENSU      PIC  9(09).
      *--- (2008/12/26) line change start monma ------------------------
      *            05  S138TMP-GMADOGK     PIC  9(09).
                  05  S138TMP-GMADOGK     PIC  S9(09).
      *--- (2008/12/26) line change end   monma ------------------------
      *--- (2008/12/11) line add start monma ---------------------------
                  05  S138TMP-GJIHIGK     PIC  9(09).
      *--- (2008/12/11) line add end   monma ---------------------------
                  05  S138TMP-GHKNGK      PIC  9(09).
                  05  S138TMP-GKOHGK      PIC  9(09).
                  05  S138TMP-GSYUEKI     PIC  9(09).
      *--- 2009/08/20 CHANGE START -------------------------------------
      *            05  S138TMP-SGNISSU     PIC  9(04).
                  05  S138TMP-SGNISSU     PIC  9(05).
      *--- 2009/08/20 CHANGE END ---------------------------------------
                  05  S138TMP-SGSEIKYU    PIC  9(09).
                  05  S138TMP-SGFTNGK     PIC  9(09).
