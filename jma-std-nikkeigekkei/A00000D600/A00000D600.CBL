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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000D600.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表明細(診療費請求明細)
      *  管理者            : 
      *  作成日付    作業者       記述
      *  04/02/24    @@@@-半田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-田中    11/03/17  患者番号の編集処理を修正
      *  01.00.03    @@@@-田中    11/04/04  患者番号桁数を最大20桁に変更
      ******************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      **    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *
           COPY    "A00000D600V02.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-S               PIC 9(05).
           03  IDX-N               PIC 9(05).
           03  IDX-MAX             PIC 9(05).
           03  IDX-SUM             PIC 9(04).
      *
           03  IDX2                PIC 9(04).
           03  JIHIIDX             PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-DENPPRTYMD PIC X(08).
           03  WRK-PARA-SORTKBN    PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-RECEERR           PIC X(200).
      *
           03  WRK-PARA-DENPPRTYMDWH PIC X(22).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
      *  
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-BIRTHDAY.
            05 WRK-BIRTHY            PIC 9(04).
            05 WRK-BIRTHM            PIC 9(02).
            05 WRK-BIRTHD            PIC 9(02).
           03  WRK-NENREI            PIC 9(03).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-SRYKA             PIC X(02).
           03  WRK-FTNRATE           PIC 9(01)V9(02).
           03  WRK-KYFRATE           PIC 9(01)V9(02).
      *
           03  WRK-SYUHKN-KBN        PIC 9(01). 
           03  WRK-MONEY             PIC 9(08).
           03  WRK-JIHI              PIC 9(08).
           03  WRK-SYUEKI            PIC 9(08).
           03  WRK-JIHI2             PIC 9(08).
           03  WRK-JIHI3             PIC 9(08).
           03  WRK-JIHI4             PIC 9(08).
           03  WRK-4001-TENTANKA     PIC 9(02)V9(01).
           03  WRK-4001-KSNMONEY     PIC 9(08).
           03  WRK-RSI-MONEY1        PIC S9(09)V9(01). 
           03  WRK-RSI-MONEY2        PIC S9(09)V9(01).
      *
           03  WRK-RENNUM-X          PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9                PIC ZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZ.
           03  WRK-ZZ9               PIC ZZZZZZZ9.
           03  WRK-ZZZ               PIC ZZZZZZZZ.
           03  WRK-S9                PIC -------9.
           03  WRK-SZ                PIC -------Z.
           03  WRK-SZ9               PIC --------9.
           03  WRK-SZZ               PIC --------Z.
           03  WRK-Z9-3              PIC ZZ9.
           03  WRK-Z9-7              PIC ZZZZZZ9.
           03  WRK-ZZ-7              PIC ZZZZZZZ.
           03  WRK-Z9-8              PIC ZZZZZZZ9.
           03  WRK-S9-8              PIC -------9.
           03  WRK-SZ-8              PIC -------Z.
      *
       01  SUM-AREA                             VALUE   ZERO.
           03  SUM-HKNTEN      PIC 9(08).
           03  SUM-HKNKEN      PIC 9(08).
           03  SUM-OCC         OCCURS  14.
               05  SUM-MONEY   PIC 9(08).
               05  SUM-KEN     PIC 9(08).
           03  SUM-SEIKYU      PIC S9(08).
           03  SUM-RYOSYU      PIC S9(08).
           03  SUM-SEIKYUKEN   PIC 9(08).
           03  SUM-RYOSYUKEN   PIC 9(08).
      *
       01  SUM-G-AREA                           VALUE   ZERO.
           03  SUM-G-HKNTEN      PIC 9(09).
           03  SUM-G-OCC         OCCURS  16.
               05  SUM-G-TEN     PIC 9(09).
           03  SUM-G-HKNFTN      PIC S9(09).
           03  SUM-G-JIHIFTN     PIC S9(09).
           03  SUM-G-SEIKYU      PIC S9(09).
           03  SUM-G-RYOSYU      PIC S9(09).
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL      OCCURS  15000.
               05  WRK-SMEI-HOSPNUM         PIC 9(02).
               05  WRK-SMEI-PTID            PIC 9(10).
               05  WRK-SMEI-SORT-KEY. 
                 07  WRK-SMEI-NYUGAIKBN     PIC X(01).
                 07  WRK-SMEI-SRYKA         PIC X(02).
                 07  WRK-SMEI-DENPNUM       PIC 9(07).
                 07  WRK-SMEI-DENPEDANUM    PIC 9(02).
               05  WRK-SMEI-SORT-KEY2. 
      *--- 2010/12/07 CHANGE START -------------------------------------
      *           07  WRK-SMEI-NYUGAIKBN     PIC X(01).
      *           07  WRK-SMEI-SRYKA         PIC X(02).
                 07  WRK-SMEI-NYUGAIKBN2    PIC X(01).
                 07  WRK-SMEI-SRYKA2        PIC X(02).
      *--- 2010/12/07 CHANGE END   -------------------------------------
                 07  WRK-SMEI-PTNUM         PIC X(20).
               05  WRK-SMEI-SKYMONEY        PIC S9(07).
               05  WRK-SMEI-NYUHEN-MONEY    PIC S9(07).
               05  WRK-SMEI-NYUHEN-YMD      PIC X(08).
           03  WRK-SV-SMEI-TBL              PIC X(68). 
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    自費管理情報
           COPY  "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *    労災医療機関情報
           COPY  "CPSK4001.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者労災マスタ
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  WRK-SMEI-REC
           INITIALIZE                  SUM-G-AREA
           INITIALIZE                  SPA-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID    
                                               LNK-PRTKANRI-PRTNM    
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-DENPPRTYMD
                                               WRK-PARA-SORTKBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "A00000D600"    TO  JOB-PGID
           MOVE    "日計表明細（診療請求明細）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH 
      *
      *    対象年月日編集
           MOVE    WRK-PARA-DENPPRTYMD TO    WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PARA-DENPPRTYMDWH 
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE  WRK-PARA-DENPPRTYMD   TO  SYS-1001-EDYUKYMD
                                           SYS-1001-STYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
           ELSE    
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    労災医療機関情報
           MOVE    SPACE                   TO  SYS-4001-REC
           INITIALIZE                          SYS-4001-REC
           MOVE    "4001"                  TO  SYS-4001-KANRICD
           MOVE    "*"                     TO  SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM             TO  SYS-4001-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD     TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD       TO  SYS-4001-EDYUKYMD
                                               SYS-4001-STYUKYMD
           MOVE    SYS-4001-REC            TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
                   MOVE  MCPDATA-REC       TO  SYS-4001-REC
                   EVALUATE  SYS-4001-TENKBN
                       WHEN  "1"
                             MOVE  11.5   TO  WRK-4001-TENTANKA
                       WHEN  "2"    
                             MOVE  12     TO  WRK-4001-TENTANKA
                       WHEN  OTHER    
                             MOVE  10     TO  WRK-4001-TENTANKA
                  END-EVALUATE
           ELSE
                  MOVE  10   TO  WRK-4001-TENTANKA
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM  310-DATA-SELECT-SEC
      *
           PERFORM              UNTIL   FLG-END     =   1
      *        帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *        帳票印刷処理
               IF  FLG-GET = 1
                    PERFORM 390-PRINT-OUT-SEC
               END-IF         
           END-PERFORM
      *
      *        帳票印刷処理
           IF  FLG-GET = ZERO
              PERFORM 310-HEAD-HEN-SEC
              MOVE  "＊＊＊  対象データが１件も存在しません。  ＊＊＊"
                   TO   D600-JOBMSG
               PERFORM 390-PRINT-OUT-SEC
           END-IF         
      *      
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ選定処理
      *****************************************************************
       310-DATA-SELECT-SEC          SECTION.
      *
           MOVE 0  TO IDX-S
           PERFORM 910-DBSELECT-SEC
      *
      *    収納明細テーブルより変数配列に対象データを抽出する。
           PERFORM   UNTIL   FLG-SYUMEI  =  1

             PERFORM 900-SYUNOU-READ-SEC
             INITIALIZE   PTINF-REC 
             MOVE   SMEI-HOSPNUM   TO   PTINF-HOSPNUM
             MOVE   SMEI-PTID      TO   PTINF-PTID
             PERFORM   900-PTINF-READ-SEC
             PERFORM   900-PTNUM-READ-SEC
      *
             IF  ( WRK-PARA-DENPPRTYMD  =      SMEI-NYUHEN-YMD
               AND WRK-PARA-HOSPNUM     =      SMEI-HOSPNUM )
               AND  FLG-SYUNOU          =      ZERO
               AND  SYU-DENPJTIKBN      NOT =  "3"   
               AND  FLG-PTINF           =      ZERO
               AND  PTINF-TSTPTNUMKBN   NOT =  "1"   
      *
               IF   ( SMEI-JOUTAIKBN NOT =  "8" AND 
                      SMEI-JOUTAIKBN NOT =  "3" ) 
                    OR  SMEI-SKYMONEY NOT = 0
                      OR  SMEI-NYUHEN-MONEY NOT = 0
                    ADD 1  TO IDX-S
                    MOVE   SMEI-HOSPNUM 
                         TO    WRK-SMEI-HOSPNUM(IDX-S)
                    MOVE   SMEI-NYUGAIKBN 
                         TO    WRK-SMEI-NYUGAIKBN(IDX-S)
                    MOVE   SMEI-SRYKA 
                         TO    WRK-SMEI-SRYKA(IDX-S)
                    MOVE   SMEI-PTID 
                         TO    WRK-SMEI-PTID(IDX-S)
                    MOVE   SMEI-DENPNUM 
                         TO    WRK-SMEI-DENPNUM(IDX-S)
                    MOVE    SMEI-DENPEDANUM 
                         TO    WRK-SMEI-DENPEDANUM(IDX-S)
                    MOVE   SMEI-SKYMONEY 
                         TO    WRK-SMEI-SKYMONEY(IDX-S)
                    MOVE   SMEI-NYUHEN-MONEY 
                         TO    WRK-SMEI-NYUHEN-MONEY(IDX-S)
                    MOVE   SMEI-NYUHEN-YMD 
                         TO    WRK-SMEI-NYUHEN-YMD(IDX-S)
                    MOVE   PTNUM-PTNUM  
                         TO    WRK-SMEI-PTNUM(IDX-S)
               END-IF
             END-IF
             PERFORM 910-DBFETCH-SEC
           END-PERFORM 
           PERFORM 910-DBCLOSE-SEC
      *
           PERFORM 920-DBSELECT-SEC
      *
           PERFORM              UNTIL   FLG-SYUMEI     =   1

             PERFORM   900-SYUNOU-READ-SEC
             INITIALIZE   PTINF-REC 
             MOVE   SMEI-HOSPNUM   TO   PTINF-HOSPNUM
             MOVE   SMEI-PTID      TO   PTINF-PTID
             PERFORM   900-PTINF-READ-SEC
             PERFORM   900-PTNUM-READ-SEC
             IF  ( WRK-PARA-DENPPRTYMD  =      SMEI-NYUHEN-YMD
               AND WRK-PARA-HOSPNUM      =      SMEI-HOSPNUM )
               AND  FLG-SYUNOU          =      ZERO
               AND  SYU-DENPJTIKBN      NOT =  "3"   
               AND  FLG-PTINF           =      ZERO
               AND  PTINF-TSTPTNUMKBN   NOT =  "1"   
      *
               IF   ( SMEI-JOUTAIKBN NOT =  "8" AND 
                      SMEI-JOUTAIKBN NOT =  "3" ) 
                   OR  SMEI-SKYMONEY NOT = 0
                   OR  SMEI-NYUHEN-MONEY NOT = 0
                   ADD 1  TO IDX-S
                   MOVE   SMEI-HOSPNUM 
                         TO    WRK-SMEI-HOSPNUM(IDX-S)
                   MOVE   SMEI-NYUGAIKBN 
                         TO    WRK-SMEI-NYUGAIKBN(IDX-S)
                   MOVE   SMEI-SRYKA 
                         TO    WRK-SMEI-SRYKA(IDX-S)
                   MOVE   SMEI-PTID 
                         TO    WRK-SMEI-PTID(IDX-S)
                   MOVE   SMEI-DENPNUM 
                         TO    WRK-SMEI-DENPNUM(IDX-S)
                   MOVE    SMEI-DENPEDANUM 
                         TO    WRK-SMEI-DENPEDANUM(IDX-S)
                   MOVE   SMEI-SKYMONEY 
                         TO    WRK-SMEI-SKYMONEY(IDX-S)
                   MOVE   SMEI-NYUHEN-MONEY 
                         TO    WRK-SMEI-NYUHEN-MONEY(IDX-S)
                   MOVE   SMEI-NYUHEN-YMD 
                         TO    WRK-SMEI-NYUHEN-YMD(IDX-S)
                   MOVE   PTNUM-PTNUM  
                         TO    WRK-SMEI-PTNUM(IDX-S)
               END-IF
             END-IF
             PERFORM 920-DBFETCH-SEC
           END-PERFORM 
           PERFORM 920-DBCLOSE-SEC
      *
      *    ソート処理
           MOVE IDX-S TO IDX-MAX
      *
           IF   IDX-MAX     >    1
             MOVE  1  TO  FLG-GET
             MOVE  ZERO    TO   FLG-END
             MOVE 1  TO  IDX-S
             MOVE 2  TO  IDX-N
             PERFORM UNTIL IDX-S NOT  <  IDX-MAX
              PERFORM  UNTIL IDX-N  >  IDX-MAX
                IF  WRK-PARA-SORTKBN   =   "1"
                  IF  WRK-SMEI-SORT-KEY2(IDX-S)  >
                      WRK-SMEI-SORT-KEY2(IDX-N) 
                    MOVE WRK-SMEI-TBL(IDX-N)  TO   WRK-SV-SMEI-TBL
                    MOVE WRK-SMEI-TBL(IDX-S)  TO   WRK-SMEI-TBL(IDX-N)
                    MOVE WRK-SV-SMEI-TBL      TO   WRK-SMEI-TBL(IDX-S)
                  END-IF
                ELSE
                  IF  WRK-SMEI-SORT-KEY(IDX-S)
                               >   WRK-SMEI-SORT-KEY(IDX-N)
                    MOVE   WRK-SMEI-TBL(IDX-N)  TO   WRK-SV-SMEI-TBL
                    MOVE   WRK-SMEI-TBL(IDX-S)  TO   WRK-SMEI-TBL(IDX-N)
                    MOVE   WRK-SV-SMEI-TBL      TO   WRK-SMEI-TBL(IDX-S)
                  END-IF
                END-IF
                ADD 1  TO  IDX-N
              END-PERFORM
              ADD 1  TO  IDX-S
              COMPUTE IDX-N = IDX-S + 1
             END-PERFORM
           ELSE
             IF   IDX-MAX     =     ZERO
                  MOVE  ZERO    TO   FLG-GET
                  MOVE  1            TO   FLG-END
             ELSE
                  MOVE  1            TO  FLG-GET
                  MOVE  ZERO    TO   FLG-END
             END-IF
           END-IF
      *
           MOVE 1     TO   IDX-S
      *
           .
       310-DATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           ADD     1                   TO  CNT-PAGE
      *
           INITIALIZE                      D600      
      *
           MOVE    WRK-PARA-DENPPRTYMDWH  TO  D600-SYSYMD
      *
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  D600-PAGE
      *
           EVALUATE WRK-NYUGAIKBN
              WHEN "1"
                   MOVE "入院"         TO  D600-NYUGAIKBN
      *
              WHEN "2"
                   MOVE "外来"         TO  D600-NYUGAIKBN
      *
           END-EVALUATE
           MOVE    "入院料"       TO   D600-NYUINRYO
      *
      *    診療科名称取得
           INITIALIZE                       SYS-1005-REC
           MOVE    "1005"               TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO  SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO  SYS-1005-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD  TO  WRK-SYMD
           MOVE    WRK-PARA-DENPPRTYMD  TO  SYS-1005-EDYUKYMD
                                            SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO  MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC        TO  SYS-1005-REC
               MOVE  SYS-1005-SRYKANAME TO  D600-SRYKA
           ELSE
               MOVE  SPACE              TO  D600-SRYKA
           END-IF                                     
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO                        TO  CNT-LINE
      *
           INITIALIZE                          SUM-G-AREA
      *
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO  WRK-SRYKA
      *    帳票編集<見出し>処理
           PERFORM 310-HEAD-HEN-SEC
      *
           PERFORM   UNTIL   FLG-END     =   1
                     OR      CNT-LINE    =   21
      *    収納テーブル再読み込み
             PERFORM 901-SYUNOU-READ-SEC
             IF    FLG-SYUNOU = ZERO
      *
      *    入外区分ブレイクチェック処理
               IF WRK-SMEI-NYUGAIKBN(IDX-S)  NOT = WRK-NYUGAIKBN
      *
                 PERFORM 3001-SYOUKEI-SET-SEC
      *
      *    帳票編集<見出し>処理
                 PERFORM 390-PRINT-OUT-SEC
      *
                 INITIALIZE                          SUM-G-AREA
      *
                 MOVE    ZERO                      TO  CNT-LINE
                 MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                 MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *    帳票編集<見出し>処理
                 PERFORM 310-HEAD-HEN-SEC
               END-IF      
      *
      *    診療科ブレイクチェック処理
               IF WRK-SMEI-SRYKA(IDX-S)  NOT = WRK-SRYKA
      *
                 PERFORM 3001-SYOUKEI-SET-SEC
      *
      *    帳票編集<見出し>処理
                 PERFORM 390-PRINT-OUT-SEC
      *
                 INITIALIZE                          SUM-G-AREA
      *
                 MOVE    ZERO                      TO  CNT-LINE
                 MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                 MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *    帳票編集<見出し>処理
                 PERFORM 310-HEAD-HEN-SEC
               END-IF
      *
      *    患者情報読み込み
               INITIALIZE                        PTINF-REC
               MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTINF-HOSPNUM
               MOVE    WRK-SMEI-PTID(IDX-S)      TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
      *
               ADD     1                   TO  CNT-LINE
      *    伝票番号
               MOVE    WRK-SMEI-DENPNUM(IDX-S)
                             TO  D600-RENNUM (CNT-LINE)
      *    患者番号
               INITIALIZE   ORCSPTIDAREA
               MOVE    WRK-PARA-HOSPNUM        TO  SPTID-HOSPNUM
               MOVE    WRK-SMEI-PTID(IDX-S)    TO  SPTID-PTID
               CALL "ORCSPTID"  USING ORCSPTIDAREA
                                      SPA-AREA
               IF  SPTID-RC = ZERO
                 IF  SPTID-PTNUM(11:)  =  SPACE
                   MOVE    SPTID-PTNUM    TO  D600-PTNUM(CNT-LINE)
                 ELSE
                   MOVE    SPTID-PTNUM    TO  D600-PTNUM2(CNT-LINE)
                 END-IF
               ELSE
                 MOVE    SPACE            TO  D600-PTNUM(CNT-LINE)
               END-IF
      *    氏名
               MOVE    PTINF-NAME       TO  D600-NAME(CNT-LINE)
      *    性別
              EVALUATE  PTINF-SEX
                   WHEN  "1"
                         MOVE  "男"     TO  D600-SEX (CNT-LINE)
                   WHEN  "2"
                         MOVE  "女"     TO  D600-SEX (CNT-LINE)
              END-EVALUATE
      *    年齢
              MOVE  SYU-SRYYMD          TO  WRK-SYSYMD
              MOVE  PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
              COMPUTE WRK-NENREI        =   WRK-SYSYY
                                        -   WRK-BIRTHY
              IF    WRK-SYSMM           <   WRK-BIRTHM
                    COMPUTE WRK-NENREI  =   WRK-NENREI   -   1
              ELSE
                IF   WRK-SYSMM          =   WRK-BIRTHM
                 AND WRK-SYSDD          <   WRK-BIRTHD
                     COMPUTE WRK-NENREI =   WRK-NENREI   -   1
                END-IF 
              END-IF
              MOVE  WRK-NENREI          TO  WRK-Z9-3
              MOVE  WRK-Z9-3            TO  D600-AGE (CNT-LINE)
      *    本／家  保険組合情報
              INITIALIZE                           HKNCOMBI-REC
              MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  COMB-HOSPNUM
              MOVE    WRK-SMEI-PTID(IDX-S)     TO  COMB-PTID
              MOVE    SYU-HKNCOMBINUM          TO  COMB-HKNCOMBINUM
              PERFORM 900-HKNCOMBI-READ-SEC
      *    患者保険情報検索
              INITIALIZE                           PTHKNINF-REC
              MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTHKN-HOSPNUM
              MOVE    WRK-SMEI-PTID(IDX-S)     TO  PTHKN-PTID
              MOVE    COMB-HKNID               TO  PTHKN-HKNID
              PERFORM 900-PTHKNINF-READ-SEC
              EVALUATE  PTHKN-HONKZKKBN
                  WHEN  "1"
                        MOVE  "本"    TO  D600-HKNKBN (CNT-LINE)
                  WHEN  "2"
                        MOVE  "家"    TO  D600-HKNKBN (CNT-LINE)
              END-EVALUATE
      *    保険種別
              EVALUATE    SYU-SYUHKNNUM
                  WHEN    SPACE
                           MOVE  "公費単独" TO  D600-HKNS (CNT-LINE)
                           MOVE  3          TO  WRK-SYUHKN-KBN
                  WHEN    "980"  THRU  "989"    
                           MOVE  "自費"     TO  D600-HKNS (CNT-LINE)
                           MOVE  4          TO  WRK-SYUHKN-KBN
                  WHEN    "900"  THRU  "919"
                           MOVE  "治験"     TO  D600-HKNS(CNT-LINE)
                           MOVE  4          TO  WRK-SYUHKN-KBN
                  WHEN    WRK-CONS-ROUSAI-HKNNUM    
                           MOVE  "労災" TO  D600-HKNS (CNT-LINE)
                           MOVE  5          TO  WRK-SYUHKN-KBN
                  WHEN    WRK-CONS-JIBAI-HKNNUM
                           MOVE  "自賠責"   TO  D600-HKNS (CNT-LINE)
                           MOVE  6          TO  WRK-SYUHKN-KBN
                  WHEN    "060"
                  WHEN    "067"
                  WHEN    "068"
                  WHEN    "069"
                           MOVE  "国保"     TO  D600-HKNS (CNT-LINE)
                           MOVE  1          TO  WRK-SYUHKN-KBN
                  WHEN    WRK-CONS-KOGAI-HKNNUM
                           MOVE  "公害"   TO  D600-HKNS (CNT-LINE)
                           MOVE  7          TO  WRK-SYUHKN-KBN
                  WHEN    "039"
                  WHEN    "040"
                           MOVE  "後期高齢" TO  D600-HKNS (CNT-LINE)
                           MOVE  8          TO  WRK-SYUHKN-KBN
                  WHEN    OTHER
                           MOVE  "社保"     TO  D600-HKNS (CNT-LINE)
                           MOVE  2          TO  WRK-SYUHKN-KBN
              END-EVALUATE
      *
      *    伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
              IF     WRK-SMEI-DENPEDANUM(IDX-S)   =     1
                     IF  FLG-SYUNOU  = ZERO
                         PERFORM 320-BODY-HEN-SUB-SEC
                     END-IF
              END-IF
      *
      *    請求金額
             INITIALIZE  WRK-SZ
             MOVE    WRK-SMEI-SKYMONEY(IDX-S)   TO   WRK-SZ-8
             MOVE    WRK-SZ-8    TO   D600-SEIKYU(CNT-LINE)
             IF   WRK-SMEI-SKYMONEY(IDX-S)  NOT =  0
                   ADD   WRK-SMEI-SKYMONEY(IDX-S) TO   SUM-SEIKYU
                   ADD   1   TO   SUM-SEIKYUKEN
      *
                   ADD WRK-SMEI-SKYMONEY(IDX-S)  TO   SUM-G-SEIKYU
      *
             END-IF
      *
      *    領収金額
             INITIALIZE  WRK-SZ
             MOVE WRK-SMEI-NYUHEN-MONEY(IDX-S) TO  WRK-SZ-8
             MOVE WRK-SZ-8 TO   D600-RYOSYU  (CNT-LINE)
             IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
                   ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)
                           TO   SUM-RYOSYU
                   ADD 1   TO   SUM-RYOSYUKEN
      *
                   ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)
                          TO  SUM-G-RYOSYU
      *
             END-IF
            END-IF
      *    収納テーブル収納テーブル再読み込み
      *
      *    配列インデックスのカウントアップ
             ADD  1  TO  IDX-S
      *    配列のデータを全件出力した場合は合計表の印刷を行う為にループより脱出
             IF IDX-S    >   IDX-MAX
                MOVE    1   TO   FLG-END
             END-IF 
           END-PERFORM     
      *
           PERFORM 3001-SYOUKEI-SET-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>サブ処理
      *****************************************************************
       320-BODY-HEN-SUB-SEC      SECTION.
      *
      *    点数単価の算出
      *    保険番号情報読み込み
            PERFORM 900-HKNNUM-READ-SEC
            IF   FLG-HKNNUM  =  1
      *    該当レコードが存在しない場合は単価を10円に設定する。
                 MOVE 10     TO HKN-TENTNK
            END-IF
            IF   WRK-SYUHKN-KBN   =  5
             OR  WRK-SYUHKN-KBN   =  6
             OR  WRK-SYUHKN-KBN   =  7
      *    患者労災情報検索
                 INITIALIZE           PTRSIINF-REC
                 MOVE SYU-HOSPNUM  TO  PTRSI-HOSPNUM
                 MOVE SYU-PTID    TO  PTRSI-PTID
                 MOVE COMB-HKNID  TO  PTRSI-HKNID
                 PERFORM 900-PTRSIINF-READ-SEC
            END-IF
      *
      *    保険点数
            MOVE    ZERO                TO  SYU-TOTAL-HKNTEN
            PERFORM VARYING IDZ FROM    1   BY  1
                    UNTIL   IDZ >       16
                    MOVE    SYU-HKNTEN (IDZ)  TO  WRK-ZZ-7
                    MOVE    WRK-ZZ-7  TO  D600-TEN(IDZ CNT-LINE)
                    ADD     SYU-HKNTEN (IDZ)  TO  SYU-TOTAL-HKNTEN
      *
                    ADD     SYU-HKNTEN (IDZ)     TO  SUM-G-TEN (IDZ)
      *
            END-PERFORM
            MOVE    SYU-TOTAL-HKNTEN    TO  WRK-ZZ-7
            MOVE    WRK-ZZ-7            TO  D600-HKNTEN (CNT-LINE)
            ADD     SYU-TOTAL-HKNTEN    TO  SUM-HKNTEN
            ADD     1                   TO  SUM-HKNKEN
      *
            ADD     SYU-TOTAL-HKNTEN    TO  SUM-G-HKNTEN
      *
      *    主保険患者負担割合、主保険給付割合の算出
      *
            IF   WRK-SYUHKN-KBN   =  1
             OR  WRK-SYUHKN-KBN   =  2
             OR  WRK-SYUHKN-KBN   =  8
      *    年齢の算出
                 MOVE  SYU-SRYYMD         TO  WRK-SYSYMD
                 MOVE  PTINF-BIRTHDAY     TO  WRK-BIRTHDAY
                 COMPUTE WRK-NENREI       =   WRK-SYSYY  - WRK-BIRTHY
                 IF    WRK-SYSMM          <   WRK-BIRTHM
                       COMPUTE WRK-NENREI =   WRK-NENREI - 1
                 ELSE
                  IF   WRK-SYSMM          =   WRK-BIRTHM
                   AND WRK-SYSDD          <   WRK-BIRTHD
                       COMPUTE WRK-NENREI =   WRK-NENREI - 1
                  END-IF 
                 END-IF
      *
                 IF    SYU-SYUHKNNUM =  "060"
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   ZERO
                                  MOVE   ZERO  TO  WRK-FTNRATE 
                           WHEN   1
                                  MOVE   0.1   TO  WRK-FTNRATE 
                           WHEN   2
                                  MOVE   0.2   TO  WRK-FTNRATE 
                           WHEN   3
                                  MOVE   0.3   TO  WRK-FTNRATE 
                           WHEN   OTHER
                                  MOVE   0.3   TO  WRK-FTNRATE 
                       END-EVALUATE 
                 ELSE 
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   SPACE
                                  MOVE   0.3   TO  WRK-FTNRATE 
                           WHEN   8
                                  MOVE   0.2   TO  WRK-FTNRATE 
                           WHEN   9
                                  MOVE   0.1   TO  WRK-FTNRATE 
                           WHEN   OTHER
                                  MOVE   ZERO  TO  WRK-FTNRATE 
                       END-EVALUATE 
                 END-IF
      *
      *    三歳未満、負担割合３割以上のチェック
                 IF    WRK-NENREI         <=  2
                  AND  WRK-FTNRATE        >   0.2
                       MOVE    0.2        TO  WRK-FTNRATE
                 END-IF
                 IF  SYU-SYUHKNNUM  =  "069"  OR "068"
                     MOVE  1                   TO  WRK-FTNRATE
                 END-IF
                 COMPUTE WRK-KYFRATE   =  1 - WRK-FTNRATE 
            END-IF
      *
      *    保険負担額
               IF   WRK-SYUHKN-KBN   =  1
                OR  WRK-SYUHKN-KBN   =  2
                OR  WRK-SYUHKN-KBN   =  5
                OR  WRK-SYUHKN-KBN   =  6
                OR  WRK-SYUHKN-KBN   =  7
                OR  WRK-SYUHKN-KBN   =  8
                    EVALUATE  WRK-SYUHKN-KBN
                    WHEN  1 THRU 2
                    WHEN  8
                        IF     SYU-KOH-HKNNUM(1) =  "027"
                         COMPUTE  WRK-MONEY  =
                          (SYU-TOTAL-HKNTEN * 10) - SYU-KOH-COMPFTN(1)
                        ELSE
                         COMPUTE  WRK-MONEY  =
                          (SYU-TOTAL-HKNTEN * 10) - SYU-SYUCOMPFTN
                        END-IF
      *
                        COMPUTE WRK-MONEY =  WRK-MONEY
                                        +  SYU-RYOYOHI-SKJ  
                                        +  SYU-RYOYOHI-LIFE
      *    レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                        IF  HKN-RECEFTNDSPKBN  =  "1"
                            IF  WRK-MONEY(8:1) >   4
                                MOVE   ZERO    TO  WRK-MONEY(8:1)
                                ADD    10      TO  WRK-MONEY
                            ELSE
                                MOVE   ZERO    TO  WRK-MONEY(8:1)
                             END-IF 
                        END-IF 
                    WHEN  5
      *    労災の場合
                      IF  SYU-PTFTNRATE NOT =  ZERO
                            MOVE ZERO  TO   WRK-MONEY 
                      ELSE
                        COMPUTE  WRK-MONEY  = 
                        (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
                        COMPUTE WRK-MONEY    =    WRK-MONEY
                                             +  SYU-RSISHOSHIN-MONEY 
                                             +  SYU-RSISAISHIN-MONEY 
                                             +  SYU-RSISDO-MONEY 
                                             +  SYU-RSIETC-MONEY
                                             +  SYU-RYOYOHI-SKJ  
                                             +  SYU-RYOYOHI-LIFE
                      END-IF
                    WHEN  6
      *    自賠の場合
                        IF  SYU-PTFTNRATE NOT =  ZERO
                            MOVE ZERO  TO   WRK-MONEY 
                        ELSE
                            COMPUTE  WRK-MONEY  = 
                               SYU-RSIJIBAI-SKYMONEY
                            -  SYU-JIHI-TOTAL
                            -  SYU-JIHI-TOTAL-TAX
                            -  SYU-TOTAL-TGMONEY
                            -  SYU-TOTAL-TGMONEY-TAX
                            -  SYU-CHOSEI
                            -  SYU-YKZ-FTN
                            -  SYU-SKYMONEY-SKJ-JIHI-KEI
                            -  SYU-SKYMONEY-LIFE-JIHI-KEI
                            -  SYU-RMSAGAKU
                        END-IF
                        
      *    公害の場合
                    WHEN   7
                        COMPUTE WRK-JIHI4  =  SYU-TOTAL-TGMONEY
                                          +  SYU-TOTAL-TGMONEY-TAX
                                          +  SYU-JIHI-TOTAL
                                          +  SYU-JIHI-TOTAL-TAX
                                          +  SYU-RMSAGAKU
                                          +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                          +  SYU-SKYMONEY-LIFE-JIHI-KEI
                        IF      SYU-SRYYMD  <  "20070401"
                            COMPUTE WRK-JIHI4    =   WRK-JIHI4 
                                                +   SYU-JIHI-TAX
                        END-IF
                        COMPUTE WRK-MONEY   =   SYU-RSIJIBAI-SKYMONEY
                                                -  WRK-JIHI4
                    END-EVALUATE
                    COMPUTE WRK-MONEY  =  WRK-MONEY
                                       -  SYU-SKYMONEY-SKJ-KEI
                                       -  SYU-SKYMONEY-LIFE-KEI
                    MOVE    WRK-MONEY  TO  WRK-SZ-8
                    MOVE    WRK-SZ-8   TO  D600-HKNFTN(CNT-LINE)
      *
                    IF   WRK-MONEY  NOT =  0
                        ADD  WRK-MONEY  TO  SUM-G-HKNFTN
                    END-IF
      *
               END-IF  
      *
      *    自費負担額
               IF   WRK-SYUHKN-KBN   =  1
                OR  WRK-SYUHKN-KBN   =  2
                OR  WRK-SYUHKN-KBN   =  3
                OR  WRK-SYUHKN-KBN   =  4
                OR  WRK-SYUHKN-KBN   =  8
                OR  WRK-SYUHKN-KBN   =  5
                OR  WRK-SYUHKN-KBN   =  6
                    EVALUATE WRK-SYUHKN-KBN
                    WHEN 1 THRU 2
                    WHEN 8
      *    国保、社保の場合
                        IF     SYU-KOH-HKNNUM(1)  =  "027"
                          MOVE  SYU-KOH-COMPFTN(1)   TO  WRK-MONEY
                        ELSE
                          MOVE  SYU-SYUCOMPFTN   TO  WRK-MONEY
                        END-IF
      *    レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                        IF  WRK-MONEY(8:1) >   4
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                            ADD    10      TO  WRK-MONEY
                        ELSE
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                        END-IF 
                    WHEN 3
      *    公費単独の場合
                        COMPUTE WRK-MONEY ROUNDED     = 
                        (SYU-TOTAL-HKNTEN * HKN-TENTNK)
      *    レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                        IF  WRK-MONEY(8:1) >   4
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                            ADD    10      TO  WRK-MONEY
                        ELSE
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                        END-IF 
                        
                    WHEN 5
                        IF  SYU-PTFTNRATE  =  ZERO
                              MOVE ZERO  TO   WRK-MONEY 
                        ELSE
                          COMPUTE  WRK-MONEY  = 
                          (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
                          COMPUTE WRK-MONEY    =    WRK-MONEY
                                               +  SYU-RSISHOSHIN-MONEY 
                                               +  SYU-RSISAISHIN-MONEY 
                                               +  SYU-RSISDO-MONEY
                                               +  SYU-RSIETC-MONEY
                                               +  SYU-RYOYOHI-SKJ
                                               +  SYU-RYOYOHI-LIFE
                        END-IF
                    WHEN 6
                        IF  SYU-PTFTNRATE  =  ZERO
                            MOVE ZERO  TO   WRK-MONEY 
                        ELSE
                            COMPUTE  WRK-MONEY  = 
                               SYU-RSIJIBAI-SKYMONEY
                            -  SYU-JIHI-TOTAL
                            -  SYU-JIHI-TOTAL-TAX
                            -  SYU-TOTAL-TGMONEY
                            -  SYU-TOTAL-TGMONEY-TAX
                            -  SYU-CHOSEI
                            -  SYU-YKZ-FTN
                            -  SYU-SKYMONEY-SKJ-JIHI-KEI
                            -  SYU-SKYMONEY-LIFE-JIHI-KEI
                            -  SYU-RMSAGAKU
                        END-IF
                    END-EVALUATE
      *
                 IF WRK-SYUHKN-KBN  = 4
      *    自費の場合
                    MOVE  SYU-SKYMONEY  TO  WRK-MONEY
                 ELSE
      *    自費以外の場合
      *    自費分の集計
                     IF  SYU-SRYYMD  <=  "20070331"
                         COMPUTE WRK-JIHI    =   
                            SYU-TOTAL-TGMONEY  
                         +  SYU-TOTAL-TGMONEY-TAX  
                         +  SYU-JIHI-TOTAL
                         +  SYU-JIHI-TOTAL-TAX  
                         +  SYU-JIHI-TAX
                     ELSE
                         COMPUTE WRK-JIHI    =   
                            SYU-TOTAL-TGMONEY  
                         +  SYU-TOTAL-TGMONEY-TAX  
                         +  SYU-JIHI-TOTAL
                         +  SYU-JIHI-TOTAL-TAX  
                     END-IF
                     COMPUTE WRK-MONEY   = 
                        WRK-MONEY
                     +  WRK-JIHI
                     +  SYU-SKYMONEY-SKJ-KEI
                     +  SYU-SKYMONEY-SKJ-JIHI-KEI
                     +  SYU-SKYMONEY-LIFE-KEI
                     +  SYU-SKYMONEY-LIFE-JIHI-KEI
                     +  SYU-RMSAGAKU
                 END-IF  
                 MOVE  WRK-MONEY  TO  WRK-SZ-8
                 MOVE  WRK-SZ-8   TO  D600-JIHIFTN(CNT-LINE) 
      *
                 IF   WRK-MONEY  NOT =  0
                     ADD  WRK-MONEY  TO  SUM-G-JIHIFTN
                 END-IF
      *
               END-IF
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    小計印字処理
      *****************************************************************
       3001-SYOUKEI-SET-SEC          SECTION.
      *
      *    保険点数
           IF  SUM-G-HKNTEN  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-ZZZ
               MOVE     SUM-G-HKNTEN     TO    WRK-ZZZ
               MOVE     WRK-ZZZ         TO    D600-GG-HKNTEN
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-ZZ
               MOVE     SUM-G-HKNTEN     TO    WRK-ZZ
               MOVE     WRK-ZZ         TO    D600-G-HKNTEN
           END-IF
      *    診療種別点数（診察料〜入院料）
           PERFORM  VARYING  IDX-SUM   FROM 1  BY  1
                    UNTIL    IDX-SUM   >    16
             IF  SUM-G-TEN (IDX-SUM)   >   0
               AND IDX-SUM               =  16
                 MOVE     "療養担当"        TO   D600-TRYO
             END-IF
             IF  SUM-G-TEN (IDX-SUM)  >  999999
      *    7桁の場合
                 INITIALIZE  WRK-ZZZ
                 MOVE     SUM-G-TEN (IDX-SUM)   TO   WRK-ZZZ
                 MOVE     WRK-ZZZ   TO   D600-GG-TEN (IDX-SUM)
             ELSE
      *    6桁以下の場合
                 INITIALIZE  WRK-ZZ
                 MOVE     SUM-G-TEN (IDX-SUM)   TO   WRK-ZZ
                 MOVE     WRK-ZZ    TO   D600-G-TEN (IDX-SUM)
             END-IF
           END-PERFORM
      *    保険給付額
           IF  SUM-G-HKNFTN  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-HKNFTN     TO    WRK-SZZ
               MOVE     WRK-SZZ          TO    D600-GG-HKNFTN
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-HKNFTN     TO    WRK-SZ
               MOVE     WRK-SZ           TO    D600-G-HKNFTN
           END-IF
      *    自費負担額
           IF  SUM-G-JIHIFTN  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-JIHIFTN    TO    WRK-SZZ
               MOVE     WRK-SZZ          TO    D600-GG-JIHIFTN
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-JIHIFTN    TO    WRK-SZ
               MOVE     WRK-SZ         TO    D600-G-JIHIFTN
           END-IF
      *    請求金額
           IF  SUM-G-SEIKYU  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-SEIKYU     TO    WRK-SZZ
               MOVE     WRK-SZZ         TO    D600-GG-SEIKYU
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-SEIKYU     TO    WRK-SZ
               MOVE     WRK-SZ         TO    D600-G-SEIKYU
           END-IF
      *    領収金額
           IF  SUM-G-RYOSYU  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-RYOSYU     TO    WRK-SZZ
               MOVE     WRK-SZZ         TO    D600-GG-RYOSYU
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-RYOSYU     TO    WRK-SZ
               MOVE     WRK-SZ         TO    D600-G-RYOSYU
           END-IF
           .
       3001-SYOUKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    SYU-PTID          TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *    患者保険情報読み込み
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYU-HOSPNUM        TO  PTHKN-HOSPNUM
               MOVE    SYU-PTID          TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
           END-IF    
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                      TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM         TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
           IF   WRK-PARA-DENPPRTYMD   <   "20080401"
             MOVE    "A00000D600O.red"        TO  SPRT-PRTID
           ELSE
             MOVE    "A00000D600V02.red"        TO  SPRT-PRTID
           END-IF
           MOVE    "日計表明細（診療費請求明細）"  TO  SPRT-TITLE
           MOVE    D600                       TO  SPRT-PRTDATA
             DISPLAY " D600   =  "  D600 
           MOVE    LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC 
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *    
      *    ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報　入返金発行日で検索(入院患者)
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI-REC
      *
           MOVE    WRK-PARA-HOSPNUM     TO  SMEI-HOSPNUM
           MOVE    "1"                 TO  SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD TO  SMEI-NYUHEN-YMD
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syumei"             TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
          IF      MCP-RC              =   ZERO
                PERFORM 910-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
           END-IF        
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細情報を検索処理(入院患者)
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC            =   ZERO
               MOVE    ZERO          TO  FLG-SYUMEI
               MOVE    MCPDATA-REC   TO  SYUMEI-REC
           ELSE
               MOVE    1             TO  FLG-SYUMEI
           END-IF  
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報　入返金発行日で検索(外来患者)
      *****************************************************************
       920-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                  SYUMEI-REC
      *
           MOVE    WRK-PARA-HOSPNUM     TO  SMEI-HOSPNUM
           MOVE    "2"                 TO  SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD TO  SMEI-NYUHEN-YMD
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
          IF      MCP-RC              =   ZERO
                PERFORM 920-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
           END-IF        
           .
       920-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細情報を検索処理(外来患者)
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO  FLG-SYUMEI
               MOVE    MCPDATA-REC   TO  SYUMEI-REC
           ELSE
               MOVE    1             TO  FLG-SYUMEI
           END-IF  
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       920-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       920-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                 SYUNOU-REC
           MOVE    SMEI-HOSPNUM    TO  SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    SMEI-PTID      TO  SYU-PTID
           MOVE    SMEI-DENPNUM   TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syunou"     TO    MCP-TABLE
               MOVE    "key"            TO    MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC         =   ZERO
                   MOVE    MCPDATA-REC   TO  SYUNOU-REC
                   MOVE    ZERO       TO  FLG-SYUNOU
               ELSE
                   MOVE    1      TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1      TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       901-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                   SYUNOU-REC
      *
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)  TO  SYU-HOSPNUM
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  SYU-NYUGAIKBN
           MOVE    WRK-SMEI-PTID(IDX-S)  TO  SYU-PTID
           MOVE    WRK-SMEI-DENPNUM(IDX-S)  TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syunou"           TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SYUNOU-REC
                   MOVE    ZERO          TO  FLG-SYUNOU
               ELSE
                   MOVE    1        TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1       TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       901-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    SYU-HOSPNUM         TO  HKN-HOSPNUM  
           MOVE    SYU-SYUHKNNUM       TO  HKN-HKNNUM  
           MOVE    WRK-PARA-DENPPRTYMD TO  HKN-TEKSTYMD
           MOVE    ALL "9"             TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"      TO  MCP-FUNC
               MOVE    "tbl_hknnum"       TO  MCP-TABLE
               MOVE    "key5"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC          =  ZERO
                   MOVE    MCPDATA-REC TO  HKNNUM-REC
                   MOVE    ZERO        TO  FLG-HKNNUM
               ELSE
                   MOVE    1           TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hknnum"            TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       820-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    ZERO         TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1            TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1                TO  FLG-SYSKANRI2
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       820-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *--- 2009/03/03 ADD START ----------------------------------------
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTNUM-READ-SEC         SECTION.
      *
           INITIALIZE                    PTNUM-REC
           MOVE    SMEI-PTID             TO   PTNUM-PTID
           MOVE    PTNUM-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_ptnum"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_ptnum"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTNUM-REC
             ELSE
               INITIALIZE                PTNUM-REC
             END-IF
           ELSE
             INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_ptnum"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_pthkninf"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   MOVE    ZERO                TO  FLG-PTHKNINF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災マスター読込処理
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTRSIINF-REC
                   MOVE    ZERO        TO  FLG-PTRSIINF
               ELSE
                   MOVE    1           TO  FLG-PTRSIINF
                   INITIALIZE              PTRSIINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC      TO  HKNCOMBI-REC
                   MOVE    ZERO             TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                TO  FLG-HKNCOMBI
                   INITIALIZE               HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                    TO  FLG-HKNCOMBI
               INITIALIZE                   HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO     MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
