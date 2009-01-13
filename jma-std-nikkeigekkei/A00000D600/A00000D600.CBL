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
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000D600.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表明細(診療費請求明細)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/02/24    @@@@-半田     新規作成
      *  04/05/06    @@@@-本橋     項目名(入院料)を外来時はスペースにする
      *  04/05/20    @@@@-有村     老人保健所持時の対応修正
      *  04/08/18    @@@@-本橋     点数、金額がない患者は印字しないようにする
      *                            ページごとの小計を追加
      *  04/09/16    @@@@-本橋     MONPE化
      *  05/02/28    @@@@-鈴木     open-cobol 0.23への対応による変更
      *                            (INCファイルのみ)
      *  05/09/14    @@@@-本橋     有効期限が切られた診療科名が印字されない
      *                            不具合を修正
      *                            ORCMCPSUB → MCPSUBに変更
      *  06/08/01    @@@@-高嶋     sarge対応
      *  06/10/28    @@@@-高嶋     基本診療項目追加対応
      *  06/12/20    @@@@-高嶋     自賠責計算式変更対応
      *  06/12/22    @@@@-内田     特別療養費（068,069）を国保で扱う
      *  07/03/20    @@@@-内田     消費税内税の影響対応
      *  07/09/10    @@@@-高嶋     4.0対応
      *  08/03/25    @@@@-田中     H20年4月改正対応
      *  08/06/10    @@@@-田中     健保準拠時の自賠責の計算を修正
      *  08/11/05    @@@@-田中     帳票枚数を1枚に変更
      *****************************************************************
      *
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           COPY    "A00000D600.INC".
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *     COPY    "A00000D6001.INC".
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *****
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *=== 4.0対応 By Takashima(2007/09/10) end
           03  WRK-PARA-DENPPRTYMD PIC X(08).
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
           03  WRK-PTNUM8            PIC 9(08). 
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
      *****03  WRK-IN                PIC X(01).
      *
           03  WRK-SYUHKN-KBN        PIC 9(01). 
           03  WRK-MONEY             PIC 9(08).
           03  WRK-JIHI              PIC 9(08).
           03  WRK-SYUEKI            PIC 9(08).
           03  WRK-JIHI2             PIC 9(08).
           03  WRK-JIHI3             PIC 9(08).
      * 20080325 H20年4月改正対応STRAT tanaka
           03  WRK-JIHI4             PIC 9(08).
      * 20080325 H20年4月改正対応END
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
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *     03  SUM-G-OCC         OCCURS  11.
      * 20080325 H20年4月改正対応STRAT tanaka
      *     03  SUM-G-OCC         OCCURS  15.
           03  SUM-G-OCC         OCCURS  16.
      * 20080325 H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
               05  SUM-G-TEN     PIC 9(09).
           03  SUM-G-HKNFTN      PIC S9(09).
           03  SUM-G-JIHIFTN     PIC S9(09).
           03  SUM-G-SEIKYU      PIC S9(09).
           03  SUM-G-RYOSYU      PIC S9(09).
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL      OCCURS  15000.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         05  WRK-SMEI-HOSPID          PIC X(24).
               05  WRK-SMEI-HOSPNUM         PIC 9(02).
      *=== 4.0対応 By Takashima(2007/09/10) end
               05  WRK-SMEI-PTID            PIC 9(10).
               05  WRK-SMEI-SORT-KEY. 
                 07  WRK-SMEI-NYUGAIKBN     PIC X(01).
                 07  WRK-SMEI-SRYKA         PIC X(02).
                 07  WRK-SMEI-DENPNUM       PIC 9(07).
                 07  WRK-SMEI-DENPEDANUM    PIC 9(02).
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
      * 20080325 H20年4月改正対応STRAT tanaka
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
      * 20080325 H20年4月改正対応END
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     COPY    "ORCA-DBPATH".         
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *    収納マスタ
      * 01  BD002-REC.
      *     COPY    "CPBD002.INC".
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
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
      *
           COPY    "MCPDATA.INC".
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
      *=== 4.0対応 By Takashima(2007/09/10) start
           INITIALIZE                  SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
      *=== 4.0対応 By Takashima(2007/09/10) end
                                               WRK-PARA-DENPPRTYMD
      *=== 4.0対応 By Takashima(2007/09/10) start
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "A00000D600"    TO  JOB-PGID
           MOVE    "日計表明細（診療請求明細）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                   SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
      ***  20050914  MOTO
           MOVE  WRK-PARA-DENPPRTYMD   TO  SYS-1001-EDYUKYMD
                                           SYS-1001-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
               MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM             TO  SYS-4001-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD     TO  WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD       TO  SYS-4001-EDYUKYMD
                                               SYS-4001-STYUKYMD
      ***  20050914  MOTO
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
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *        MOVE  "＊＊＊  対象データが１件も存在しません。  ＊＊＊"
      *             TO   D6001-JOBMSG
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
               PERFORM 390-PRINT-OUT-SEC
           END-IF         
      *      
      *=== sarge対応 DEL By Takashima(2006/08/01) start
      *     PERFORM 910-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/01) end
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
      *
           PERFORM              UNTIL   FLG-SYUMEI     =   1

      * 20030707 追加 
            PERFORM 900-SYUNOU-READ-SEC
            INITIALIZE   PTINF-REC 
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE SMEI-HOSPID TO PTINF-HOSPID
            MOVE SMEI-HOSPNUM TO PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
            MOVE SMEI-PTID   TO PTINF-PTID
            PERFORM 900-PTINF-READ-SEC
      * 20030707 追加
      *    入力された日付と入返金日が一致しているかをチェック
      ***  20030707 収納明細情報抽出条件変更
      ***  収納情報にレコードが存在しないデータは抽出しない。
      ***  伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      ***  患者情報にレコードが存在しないデータは抽出しない。
      ***  テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
             IF  ( WRK-PARA-DENPPRTYMD  =      SMEI-NYUHEN-YMD
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
               AND WRK-PARA-HOSPNUM     =      SMEI-HOSPNUM )
      *=== 4.0対応 By Takashima(2007/09/10) end
               AND  FLG-SYUNOU          =      ZERO
               AND  SYU-DENPJTIKBN      NOT =  "3"   
               AND  FLG-PTINF           =      ZERO
               AND  PTINF-TSTPTNUMKBN   NOT =  "1"   
      ***  20030707 収納明細情報抽出条件変更
      *
      * 20040818 追加 MOTO  診療訂正で請求金額、入返金が０のデータは対象外
               IF   ( SMEI-JOUTAIKBN NOT =  "8" AND 
                      SMEI-JOUTAIKBN NOT =  "3" ) 
                    OR  SMEI-SKYMONEY NOT = 0
                      OR  SMEI-NYUHEN-MONEY NOT = 0
      * 20040818 追加 MOTO
                    ADD 1  TO IDX-S
      *=== 4.0対応 By Takashima(2007/09/10) start
      *              MOVE   SMEI-HOSPID 
      *                   TO    WRK-SMEI-HOSPID(IDX-S)
                    MOVE   SMEI-HOSPNUM 
                         TO    WRK-SMEI-HOSPNUM(IDX-S)
      *=== 4.0対応 By Takashima(2007/09/10) end
                    MOVE   SMEI-NYUGAIKBN 
                         TO    WRK-SMEI-NYUGAIKBN(IDX-S)
      * 20030407 追加  診療科コード
                    MOVE   SMEI-SRYKA 
                         TO    WRK-SMEI-SRYKA(IDX-S)
      * 20030407 追加  診療科コード
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
               END-IF
             END-IF
             PERFORM 910-DBFETCH-SEC
          END-PERFORM 
          PERFORM 910-DBCLOSE-SEC
      *
           PERFORM 920-DBSELECT-SEC
      *
      *    収納明細テーブルより変数配列に対象データを抽出する。
      *SYS-4001-KSNRAT
           PERFORM              UNTIL   FLG-SYUMEI     =   1

      * 20030707 追加 
             PERFORM 900-SYUNOU-READ-SEC
             INITIALIZE   PTINF-REC 
      *=== 4.0対応 By Takashima(2007/09/10) start
      *      MOVE SMEI-HOSPID TO PTINF-HOSPID
             MOVE SMEI-HOSPNUM TO PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
             MOVE SMEI-PTID   TO PTINF-PTID
             PERFORM 900-PTINF-READ-SEC
      * 20030707 追加
      *    入力された日付と入返金日が一致しているかをチェック
      ***  20030707 収納明細情報抽出条件変更
      ***  収納情報にレコードが存在しないデータは抽出しない。
      ***  伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      ***  患者情報にレコードが存在しないデータは抽出しない。
      ***  テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
             IF  ( WRK-PARA-DENPPRTYMD  =      SMEI-NYUHEN-YMD
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         AND WRK-PARA-HOSPID      =      SMEI-HOSPID )
               AND WRK-PARA-HOSPNUM      =      SMEI-HOSPNUM )
      *=== 4.0対応 By Takashima(2007/09/10) end
               AND  FLG-SYUNOU          =      ZERO
               AND  SYU-DENPJTIKBN      NOT =  "3"   
               AND  FLG-PTINF           =      ZERO
               AND  PTINF-TSTPTNUMKBN   NOT =  "1"   
      ***  20030707 収納明細情報抽出条件変更
      *
      * 20040818 追加 MOTO  診療訂正で請求金額、入返金が０のデータは対象外
               IF   ( SMEI-JOUTAIKBN NOT =  "8" AND 
                      SMEI-JOUTAIKBN NOT =  "3" ) 
                   OR  SMEI-SKYMONEY NOT = 0
                   OR  SMEI-NYUHEN-MONEY NOT = 0
      * 20040818 追加 MOTO
                   ADD 1  TO IDX-S
      *=== 4.0対応 By Takashima(2007/09/10) start
      *             MOVE   SMEI-HOSPID 
      *                   TO    WRK-SMEI-HOSPID(IDX-S)
                   MOVE   SMEI-HOSPNUM 
                         TO    WRK-SMEI-HOSPNUM(IDX-S)
      *=== 4.0対応 By Takashima(2007/09/10) end
                   MOVE   SMEI-NYUGAIKBN 
                         TO    WRK-SMEI-NYUGAIKBN(IDX-S)
      * 20030407 追加  診療科コード
                   MOVE   SMEI-SRYKA 
                         TO    WRK-SMEI-SRYKA(IDX-S)
      * 20030407 追加  診療科コード
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
               END-IF
             END-IF
             PERFORM 920-DBFETCH-SEC
          END-PERFORM 
          PERFORM 920-DBCLOSE-SEC
      *
      *  配列の最終レコード番号を設定
          MOVE IDX-S TO IDX-MAX
      *
          IF   IDX-MAX     >    1
      *    対象データが１件以上存在する場合は抽出フラグON
            MOVE  1  TO  FLG-GET
      *    対象データが１件以上存在する場合は終了フラグOFF
            MOVE  ZERO    TO   FLG-END
      *    配列に１件以上のデータが抽出されている場合はソーティング処理を行う。
      *    変数配列によるソーティング
      *    最小値より並び替えをする為にインデックスの設定
             MOVE 1  TO  IDX-S
             MOVE 2  TO  IDX-N
             PERFORM UNTIL IDX-S NOT  <  IDX-MAX
              PERFORM  UNTIL IDX-N  >  IDX-MAX
                IF WRK-SMEI-SORT-KEY(IDX-S) > 
                    WRK-SMEI-SORT-KEY(IDX-N) 
                      MOVE WRK-SMEI-TBL(IDX-N)
                           TO  WRK-SV-SMEI-TBL
                      MOVE WRK-SMEI-TBL(IDX-S)
                           TO  WRK-SMEI-TBL(IDX-N)
                      MOVE WRK-SV-SMEI-TBL 
                           TO  WRK-SMEI-TBL(IDX-S)
                END-IF
                ADD 1  TO  IDX-N
              END-PERFORM
      *    N番目の値を決定する為にインデックスの再設定
              ADD 1  TO  IDX-S
              COMPUTE IDX-N = IDX-S + 1
             END-PERFORM
          ELSE
            IF   IDX-MAX     =     ZERO
      *    対象データが１件も存在しない場合は抽出フラグOFF 
                  MOVE  ZERO    TO   FLG-GET
      *    対象データが１件も存在しない場合は終了フラグON 
                  MOVE  1            TO   FLG-END
            ELSE 
      *    対象データが１件でも存在する場合は抽出フラグオフON
                  MOVE  1            TO  FLG-GET
      *    対象データが１件でも存在する場合は終了フラグOFF 
                  MOVE  ZERO    TO   FLG-END
            END-IF
          END-IF
      *
      *  配列用員インデックスの再設定      
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
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *    INITIALIZE                      D6001
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *
           MOVE    WRK-PARA-DENPPRTYMDWH  TO  D600-SYSYMD
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *     MOVE    WRK-PARA-DENPPRTYMDWH  TO  D6001-SYSYMD
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  D600-PAGE
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *     ADD     1                   TO  CNT-PAGE
      *     MOVE    CNT-PAGE            TO  WRK-PAGE
      *     MOVE    WRK-PAGE            TO  D6001-PAGE
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *
           EVALUATE WRK-NYUGAIKBN
              WHEN "1"
                   MOVE "入院"         TO  D600-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *             MOVE "入院"         TO  D6001-NYUGAIKBN
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *
      ***  2004/05/06 @@@@-MOTOHASI 追加
                   MOVE "入院料"       TO  D600-NYUINRYO
      ***  ここまで
      *
              WHEN "2"
                   MOVE "外来"         TO  D600-NYUGAIKBN
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *             MOVE "外来"         TO  D6001-NYUGAIKBN
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *
      ***  2004/05/06 @@@@-MOTOHASI 追加
                   MOVE   SPACE       TO  D600-NYUINRYO
      ***  ここまで
      *
           END-EVALUATE
      *
      *    診療科名称取得
           INITIALIZE                       SYS-1005-REC
           MOVE    "1005"               TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO  SYS-1005-KBNCD
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM          TO  SYS-1005-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-DENPPRTYMD  TO  WRK-SYMD
      ***  20050914  MOTO
           MOVE    WRK-PARA-DENPPRTYMD  TO  SYS-1005-EDYUKYMD
                                            SYS-1005-STYUKYMD
      ***  20050914  MOTO
           MOVE    SYS-1005-REC         TO  MCPDATA-REC
           PERFORM 820-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC        TO  SYS-1005-REC
               MOVE  SYS-1005-SRYKANAME TO  D600-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *         MOVE  SYS-1005-SRYKANAME TO  D6001-SRYKA
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
           ELSE
               MOVE  SPACE              TO  D600-SRYKA
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *         MOVE  SPACE              TO  D6001-SRYKA
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
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
      *** 20040907  MOTOHASI
           INITIALIZE                          SUM-G-AREA
      *** 20040907  MOTOHASI
      *
      * 20030407 追加
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO  WRK-SRYKA
      *    帳票編集<見出し>処理
           PERFORM 310-HEAD-HEN-SEC
      * 20030407 追加
      *
           PERFORM   UNTIL   FLG-END     =   1
      *--- 2008/11/05  CHANGE  START -----------------------------------
      *               OR      CNT-LINE    =   30
                     OR      CNT-LINE    =   21
      *--- 2008/11/05  CHANGE  END -----------------------------------
      ***    収納テーブル再読み込み
             PERFORM 901-SYUNOU-READ-SEC
             IF    FLG-SYUNOU = ZERO
      *
      * 20030407 追加
      * 入外区分ブレイクチェック処理
               IF WRK-SMEI-NYUGAIKBN(IDX-S)  NOT = WRK-NYUGAIKBN
      *
      *** 20040907  MOTOHASI  小計印字
                 PERFORM 3001-SYOUKEI-SET-SEC
      *** 20040907  MOTOHASI
      *
      *          帳票編集<見出し>処理
                 PERFORM 390-PRINT-OUT-SEC
      *
      *** 20040907  MOTOHASI
                 INITIALIZE                          SUM-G-AREA
      *** 20040907  MOTOHASI
      *
                 MOVE    ZERO                      TO  CNT-LINE
                 MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                 MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *          帳票編集<見出し>処理
                 PERFORM 310-HEAD-HEN-SEC
               END-IF      
      *
      * 診療科ブレイクチェック処理
               IF WRK-SMEI-SRYKA(IDX-S)  NOT = WRK-SRYKA
      *
      *** 20040907  MOTOHASI  小計印字
                 PERFORM 3001-SYOUKEI-SET-SEC
      *** 20040907  MOTOHASI
      *
      *          帳票編集<見出し>処理
                 PERFORM 390-PRINT-OUT-SEC
      *
      *** 20040907  MOTOHASI
                 INITIALIZE                          SUM-G-AREA
      *** 20040907  MOTOHASI
      *
                 MOVE    ZERO                      TO  CNT-LINE
                 MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                 MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *          帳票編集<見出し>処理
                 PERFORM 310-HEAD-HEN-SEC
               END-IF      
      * 20030407 追加
      *
      *    患者情報読み込み       
               INITIALIZE                           PTINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    WRK-SMEI-HOSPID(IDX-S)   TO  PTINF-HOSPID
               MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTINF-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE    WRK-SMEI-PTID(IDX-S)     TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
      *    
               ADD     1                   TO  CNT-LINE
      *    伝票番号
               MOVE    WRK-SMEI-DENPNUM(IDX-S)
                             TO  D600-RENNUM (CNT-LINE)     
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *         MOVE    WRK-SMEI-DENPNUM(IDX-S)
      *                     TO  D6001-RENNUM (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *    患者番号
               INITIALIZE   ORCSPTIDAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE  WRK-PARA-HOSPID  TO  SPTID-HOSPID
               MOVE  WRK-PARA-HOSPNUM  TO  SPTID-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE  WRK-SMEI-PTID(IDX-S)    TO  SPTID-PTID
               CALL "ORCSPTID"  USING ORCSPTIDAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                SPA-1009-TBL
                                      SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF    SPTID-RC = ZERO
                      MOVE  SPTID-PTNUM TO  WRK-PTNUM8
                      MOVE  WRK-PTNUM8  TO  D600-PTNUM (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                MOVE  WRK-PTNUM8  TO  D6001-PTNUM (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
               ELSE
                      MOVE  SPACE       TO  D600-PTNUM (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                MOVE  SPACE       TO  D6001-PTNUM (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
               END-IF
      * 氏名
               MOVE    PTINF-NAME       TO  D600-NAME  (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *         MOVE    PTINF-NAME       TO  D6001-NAME  (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      * 性別
              EVALUATE  PTINF-SEX
                   WHEN  "1"
                         MOVE  "男"     TO  D600-SEX (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                   MOVE  "男"     TO  D6001-SEX (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
                   WHEN  "2"
                         MOVE  "女"     TO  D600-SEX (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                   MOVE  "女"     TO  D6001-SEX (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
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
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *        MOVE  WRK-Z9-3            TO  D6001-AGE (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *    本／家  
      *       保険組合情報
              INITIALIZE                           HKNCOMBI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *        MOVE    WRK-SMEI-HOSPID(IDX-S)   TO  COMB-HOSPID
              MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  COMB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
              MOVE    WRK-SMEI-PTID(IDX-S)     TO  COMB-PTID
              MOVE    SYU-HKNCOMBINUM          TO  COMB-HKNCOMBINUM
              PERFORM 900-HKNCOMBI-READ-SEC
      *       患者保険情報検索
              INITIALIZE                           PTHKNINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *        MOVE    WRK-SMEI-HOSPID(IDX-S)   TO  PTHKN-HOSPID
              MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTHKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
              MOVE    WRK-SMEI-PTID(IDX-S)     TO  PTHKN-PTID
              MOVE    COMB-HKNID               TO  PTHKN-HKNID
              PERFORM 900-PTHKNINF-READ-SEC
              EVALUATE  PTHKN-HONKZKKBN
                  WHEN  "1"
                        MOVE  "本"    TO  D600-HKNKBN (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                  MOVE  "本"    TO  D6001-HKNKBN (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
                  WHEN  "2"
                        MOVE  "家"    TO  D600-HKNKBN (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                  MOVE  "家"    TO  D6001-HKNKBN (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
              END-EVALUATE
      * 保険種別  
              EVALUATE    SYU-SYUHKNNUM
                  WHEN    SPACE
                           MOVE  "公費単独" TO  D600-HKNS (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                     MOVE  "公費単独" TO  D6001-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
                           MOVE  3          TO  WRK-SYUHKN-KBN
                  WHEN    "980" THRU "989"    
                           MOVE  "自費"     TO  D600-HKNS (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                     MOVE  "自費"     TO  D6001-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
                           MOVE  4          TO  WRK-SYUHKN-KBN
                  WHEN    WRK-CONS-ROUSAI-HKNNUM    
                           MOVE  "労災" TO  D600-HKNS (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                     MOVE  "労災" TO  D6001-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
                           MOVE  5          TO  WRK-SYUHKN-KBN
                  WHEN    WRK-CONS-JIBAI-HKNNUM
                           MOVE  "自賠責"   TO  D600-HKNS (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                     MOVE  "自賠責"   TO  D6001-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
                           MOVE  6          TO  WRK-SYUHKN-KBN
                  WHEN    "060"
                  WHEN    "067"
      *=== 特別療養費対応 By Uchida(2006/12/22) start
                  WHEN    "068"
                  WHEN    "069"
      *=== 特別療養費対応 By Uchida(2006/12/22) end
                           MOVE  "国保"     TO  D600-HKNS (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                     MOVE  "国保"     TO  D6001-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
                           MOVE  1          TO  WRK-SYUHKN-KBN
      * 20080325 H20年4月改正対応STRAT tanaka
                  WHEN    WRK-CONS-KOGAI-HKNNUM
                           MOVE  "公害"   TO  D600-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                     MOVE  "公害"   TO  D6001-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
                           MOVE  7          TO  WRK-SYUHKN-KBN
                  WHEN    "039"
                  WHEN    "040"
                           MOVE  "後期高齢" TO  D600-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                     MOVE  "後期高齢" TO  D6001-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
                           MOVE  8          TO  WRK-SYUHKN-KBN
      * 20080325 H20年4月改正対応END
                  WHEN    OTHER
                           MOVE  "社保"     TO  D600-HKNS (CNT-LINE)
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  DELETE  START -----------------------------------
      *                     MOVE  "社保"     TO  D6001-HKNS (CNT-LINE)
      *--- 2008/11/05  DELETE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
                           MOVE  2          TO  WRK-SYUHKN-KBN
              END-EVALUATE
      *
      ***   伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
              IF     WRK-SMEI-DENPEDANUM(IDX-S)   =     1
      ***               PERFORM 900-SYUNOU-READ-SEC
                     IF  FLG-SYUNOU  = ZERO
                         PERFORM 320-BODY-HEN-SUB-SEC
                     END-IF
              END-IF
      *
      * 請求金額
             INITIALIZE  WRK-SZ
             MOVE    WRK-SMEI-SKYMONEY(IDX-S)   TO   WRK-SZ-8
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
             MOVE    WRK-SZ-8    TO   D600-SEIKYU(CNT-LINE)
      *       MOVE WRK-SZ-8 TO   D6001-SEIKYU(CNT-LINE)
      *--- 2008/11/05  CHANGE  END -----------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
             IF   WRK-SMEI-SKYMONEY(IDX-S)  NOT =  0
                   ADD   WRK-SMEI-SKYMONEY(IDX-S) TO   SUM-SEIKYU
                   ADD   1   TO   SUM-SEIKYUKEN
      *
      *** 20040907  MOTOHASI  小計印字
                   ADD WRK-SMEI-SKYMONEY(IDX-S)  TO   SUM-G-SEIKYU
      *** 20040907  MOTOHASI  小計印字
      *
             END-IF
      *
      * 領収金額
             INITIALIZE  WRK-SZ
             MOVE WRK-SMEI-NYUHEN-MONEY(IDX-S) TO  WRK-SZ-8
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
             MOVE WRK-SZ-8 TO   D600-RYOSYU  (CNT-LINE)
      *       MOVE WRK-SZ-8 TO   D6001-RYOSYU  (CNT-LINE)
      *--- 2008/11/05  CHANGE  END -----------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
             IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
                   ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)
                           TO   SUM-RYOSYU
                   ADD 1   TO   SUM-RYOSYUKEN
      *
      *** 20040907  MOTOHASI  小計印字
                   ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)
                          TO  SUM-G-RYOSYU
      *** 20040907  MOTOHASI  小計印字
      *
             END-IF
            END-IF
      ***     収納テーブル収納テーブル再読み込み
      *
      *    配列インデックスのカウントアップ
             ADD  1  TO  IDX-S
      *    配列のデータを全件出力した場合は合計表の印刷を行う為にループより脱出
             IF IDX-S    >   IDX-MAX
                MOVE    1   TO   FLG-END
             END-IF 
           END-PERFORM     
      *
      *** 20040907  MOTOHASI  小計印字
           PERFORM 3001-SYOUKEI-SET-SEC
      *** 20040907  MOTOHASI
      *
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
      *     点数単価の算出
      *     保険番号情報読み込み
            PERFORM 900-HKNNUM-READ-SEC
            IF   FLG-HKNNUM  =  1
      ***     該当レコードが存在しない場合は単価を10円に設定する。
                 MOVE 10     TO HKN-TENTNK
            END-IF
            IF   WRK-SYUHKN-KBN   =  5
             OR  WRK-SYUHKN-KBN   =  6
      * 20080325 H20年4月改正対応STRAT tanaka
             OR  WRK-SYUHKN-KBN   =  7
      * 20080325 H20年4月改正対応END
      *          患者労災情報検索
                 INITIALIZE           PTRSIINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *           MOVE SYU-HOSPID  TO  PTRSI-HOSPID
                 MOVE SYU-HOSPNUM  TO  PTRSI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
                 MOVE SYU-PTID    TO  PTRSI-PTID
                 MOVE COMB-HKNID  TO  PTRSI-HKNID
                 PERFORM 900-PTRSIINF-READ-SEC
            END-IF
      *
      *     保険点数
            MOVE    ZERO                TO  SYU-TOTAL-HKNTEN
            PERFORM VARYING IDZ FROM    1   BY  1
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *              UNTIL   IDZ >       11
      * 20080325 H20年4月改正対応STRAT tanaka
      *              UNTIL   IDZ >       15
                    UNTIL   IDZ >       16
      * 20080325 H20年4月改正対応END
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
                    MOVE    SYU-HKNTEN (IDZ)  TO  WRK-ZZ-7
                    MOVE    WRK-ZZ-7  TO  D600-TEN(IDZ CNT-LINE)
                    ADD     SYU-HKNTEN (IDZ)  TO  SYU-TOTAL-HKNTEN
      *
      *** 20040907  MOTOHASI  小計印字
                    ADD     SYU-HKNTEN (IDZ)     TO  SUM-G-TEN (IDZ)
      *** 20040907  MOTOHASI  小計印字
      *
            END-PERFORM
            MOVE    SYU-TOTAL-HKNTEN    TO  WRK-ZZ-7
            MOVE    WRK-ZZ-7            TO  D600-HKNTEN (CNT-LINE)
            ADD     SYU-TOTAL-HKNTEN    TO  SUM-HKNTEN
            ADD     1                   TO  SUM-HKNKEN
      *
      *** 20040907  MOTOHASI  小計印字
            ADD     SYU-TOTAL-HKNTEN    TO  SUM-G-HKNTEN
      *** 20040907  MOTOHASI  小計印字
      *
      * 主保険患者負担割合、主保険給付割合の算出
      *
            IF   WRK-SYUHKN-KBN   =  1
             OR  WRK-SYUHKN-KBN   =  2
      * 20080325 H20年4月改正対応START tanaka
             OR  WRK-SYUHKN-KBN   =  8
      * 20080325 H20年4月改正対応END
      *          年齢の算出
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
      *
      *          三歳未満、負担割合３割以上のチェック
                 IF    WRK-NENREI         <=  2
                  AND  WRK-FTNRATE        >   0.2
                       MOVE    0.2        TO  WRK-FTNRATE
                 END-IF
      *=== 特別療養費対応 By Uchida(2006/12/22) start
                 IF  SYU-SYUHKNNUM  =  "069"  OR "068"
                     MOVE  1                   TO  WRK-FTNRATE
                 END-IF
      *=== 特別療養費対応 By Uchida(2006/12/22) end
                 COMPUTE WRK-KYFRATE   =  1 - WRK-FTNRATE 
            END-IF
      *
      *    保険負担額
               IF   WRK-SYUHKN-KBN   =  1
                OR  WRK-SYUHKN-KBN   =  2
                OR  WRK-SYUHKN-KBN   =  5
                OR  WRK-SYUHKN-KBN   =  6
      * 20080325 H20年4月改正対応STRAT tanaka
                OR  WRK-SYUHKN-KBN   =  7
                OR  WRK-SYUHKN-KBN   =  8
      * 20080325 H20年4月改正対応END
                    EVALUATE  WRK-SYUHKN-KBN
                    WHEN  1 THRU 2
      * 20080325 H20年4月改正対応STRAT tanaka
                    WHEN  8
      * 20080325 H20年4月改正対応END
      ***             国保、社保、後期高齢者の場合
      ***** 04/05/20 DEL 
      *                  COMPUTE WRK-MONEY ROUNDED     =
      *                  (SYU-TOTAL-HKNTEN * HKN-TENTNK) * WRK-KYFRATE
      ***** 04/05/20 INS
                        IF     SYU-KOH-HKNNUM(1) =  "027"
                         COMPUTE  WRK-MONEY  =
                          (SYU-TOTAL-HKNTEN * 10) - SYU-KOH-COMPFTN(1)
                        ELSE
                         COMPUTE  WRK-MONEY  =
                          (SYU-TOTAL-HKNTEN * 10) - SYU-SYUCOMPFTN
                        END-IF
      *****
                        COMPUTE WRK-MONEY =  WRK-MONEY
                                        +  SYU-RYOYOHI-SKJ  
      ***          レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                        IF  HKN-RECEFTNDSPKBN  =  "1"
                            IF  WRK-MONEY(8:1) >   4
                                MOVE   ZERO    TO  WRK-MONEY(8:1)
                                ADD    10      TO  WRK-MONEY
                            ELSE
                                MOVE   ZERO    TO  WRK-MONEY(8:1)
                             END-IF 
                        END-IF 
                    WHEN  5
      ***             労災の場合
      *--- 2008/11/05  ADD  START --------------------------------------
                      IF  SYU-PTFTNRATE NOT =  ZERO
                            MOVE ZERO  TO   WRK-MONEY 
                      ELSE
      *--- 2008/11/05  ADD  END ----------------------------------------
                        COMPUTE  WRK-MONEY  = 
                        (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
                        COMPUTE WRK-MONEY    =    WRK-MONEY
                                             +  SYU-RSISHOSHIN-MONEY 
                                             +  SYU-RSISAISHIN-MONEY 
                                             +  SYU-RSISDO-MONEY 
                                             +  SYU-RSIETC-MONEY
                                             +  SYU-RYOYOHI-SKJ  
      *--- 2008/11/05  ADD  START --------------------------------------
                      END-IF
      *--- 2008/11/05  ADD  END ----------------------------------------
                    WHEN  6
      ***             自賠の場合
      *=== 自賠責計算式変更対応 By Takashima(2006/12/20) start
      *                  COMPUTE  WRK-RSI-MONEY1 = 
      *                  SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA
      *                  COMPUTE  WRK-RSI-MONEY2 =
      *                  WRK-RSI-MONEY1 * (SYS-4001-KSNRATE / 100)
      *                  COMPUTE  WRK-MONEY  = 
      *                     WRK-RSI-MONEY1
      *                  +  WRK-RSI-MONEY2
      * 20080610 健保準拠時の自賠責の計算を修正START tanaka
      *               COMPUTE  WRK-MONEY ROUNDED = 
      *              (SYU-TOTAL-HKNTEN * WRK-4001-TENTANKA)
      *                  COMPUTE  WRK-MONEY ROUNDED =
      *                      ( WRK-MONEY * (SYS-4001-KSNRATE1 / 100) ) 
                        IF  SYU-PTFTNRATE NOT =  ZERO
      *                      COMPUTE  WRK-MONEY  =
      *                         WRK-MONEY
      * 20080610 健保準拠時の自賠責の計算を修正END
      *=== 自賠責計算式変更対応 By Takashima(2006/12/20) end
      *                      +  SYU-RSISHOSHIN-MONEY 
      *                      +  SYU-RSISAISHIN-MONEY 
      *                      +  SYU-RSISDO-MONEY 
      *                      +  SYU-RSIETC-MONEY
      *                      +  SYU-RYOYOHI-SKJ  
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
                        
      * 20080325 H20年4月改正対応STRAT tanaka
      *    公害の場合
                    WHEN   7
                        COMPUTE WRK-JIHI4  =  SYU-TOTAL-TGMONEY
                                          +  SYU-TOTAL-TGMONEY-TAX
                                          +  SYU-JIHI-TOTAL
                                          +  SYU-JIHI-TOTAL-TAX
                                          +  SYU-RMSAGAKU
                                          +  SYU-SKYMONEY-SKJ-JIHI-KEI
                        IF      SYU-SRYYMD  <  "20070401"
                            COMPUTE WRK-JIHI4    =   WRK-JIHI4 
                                                +   SYU-JIHI-TAX
                        END-IF
                        COMPUTE WRK-MONEY   =   SYU-RSIJIBAI-SKYMONEY
                                                -  WRK-JIHI4
      * 20080325 H20年4月改正対応END
                    END-EVALUATE
                    MOVE    WRK-MONEY  TO  WRK-SZ-8
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
                    MOVE    WRK-SZ-8   TO  D600-HKNFTN(CNT-LINE)
      *              MOVE    WRK-SZ-8   TO  D6001-HKNFTN(CNT-LINE)
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end    
      *
      *** 20040907  MOTOHASI
                    IF   WRK-MONEY  NOT =  0
                        ADD  WRK-MONEY  TO  SUM-G-HKNFTN
                    END-IF
      *** 20040907  MOTOHASI
      *
               END-IF  
      *
      *    自費負担額
      *
               IF   WRK-SYUHKN-KBN   =  1
                OR  WRK-SYUHKN-KBN   =  2
                OR  WRK-SYUHKN-KBN   =  3
                OR  WRK-SYUHKN-KBN   =  4
      * 20080325 H20年4月改正対応STRAT tanaka
                OR  WRK-SYUHKN-KBN   =  8
      * 20080325 H20年4月改正対応END
      *--- 2008/11/05  ADD  START --------------------------------------
                OR  WRK-SYUHKN-KBN   =  5
                OR  WRK-SYUHKN-KBN   =  6
      *--- 2008/11/05  ADD  END ----------------------------------------
                    EVALUATE WRK-SYUHKN-KBN
                    WHEN 1 THRU 2
      * 20080325 H20年4月改正対応STRAT tanaka
                    WHEN 8
      * 20080325 H20年4月改正対応END
      ***           国保、社保の場合
      ***** 04/05/20 DEL 
      *                  COMPUTE WRK-MONEY ROUNDED     =
      *                  (SYU-TOTAL-HKNTEN * HKN-TENTNK) * WRK-FTNRATE
      ***** 04/05/20 INS
                        IF     SYU-KOH-HKNNUM(1)  =  "027"
                          MOVE  SYU-KOH-COMPFTN(1)   TO  WRK-MONEY
                        ELSE
                          MOVE  SYU-SYUCOMPFTN   TO  WRK-MONEY
                        END-IF
      ***** 04/05/20 INS END
      ***           レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                        IF  WRK-MONEY(8:1) >   4
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                            ADD    10      TO  WRK-MONEY
                        ELSE
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                        END-IF 
                    WHEN 3
      ***           公費単独の場合
                        COMPUTE WRK-MONEY ROUNDED     = 
                        (SYU-TOTAL-HKNTEN * HKN-TENTNK)
      ***           レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                        IF  WRK-MONEY(8:1) >   4
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                            ADD    10      TO  WRK-MONEY
                        ELSE
                            MOVE   ZERO    TO  WRK-MONEY(8:1)
                        END-IF 
                        
      *--- 2008/11/05  ADD  START --------------------------------------
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
      *--- 2008/11/05  ADD  END ----------------------------------------
                    END-EVALUATE
      *
                 IF WRK-SYUHKN-KBN  = 4
      ***           自費の場合
                    MOVE  SYU-SKYMONEY  TO  WRK-MONEY
                 ELSE
      ***           自費以外の場合
      *             自費分の集計
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
                     IF  SYU-SRYYMD  <=  "20070331"
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
                         COMPUTE WRK-JIHI    =   
                            SYU-TOTAL-TGMONEY  
                         +  SYU-TOTAL-TGMONEY-TAX  
                         +  SYU-JIHI-TOTAL
                         +  SYU-JIHI-TOTAL-TAX  
                         +  SYU-JIHI-TAX
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) start
                     ELSE
                         COMPUTE WRK-JIHI    =   
                            SYU-TOTAL-TGMONEY  
                         +  SYU-TOTAL-TGMONEY-TAX  
                         +  SYU-JIHI-TOTAL
                         +  SYU-JIHI-TOTAL-TAX  
                     END-IF
      *=== 消費税内税の影響対応 ADD By Uchida(2007/03/20) end
                     COMPUTE WRK-MONEY   = 
                        WRK-MONEY
                     +  WRK-JIHI
                     +  SYU-SKYMONEY-SKJ-KEI
                     +  SYU-SKYMONEY-SKJ-JIHI-KEI
                     +  SYU-RMSAGAKU
                 END-IF  
                 MOVE  WRK-MONEY  TO  WRK-SZ-8
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
                 MOVE  WRK-SZ-8   TO  D600-JIHIFTN(CNT-LINE) 
      *           MOVE  WRK-SZ-8   TO  D6001-JIHIFTN(CNT-LINE) 
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *
      *** 20040907  MOTOHASI
                 IF   WRK-MONEY  NOT =  0
                     ADD  WRK-MONEY  TO  SUM-G-JIHIFTN
                 END-IF
      *** 20040907  MOTOHASI
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
      *        8桁の場合
               INITIALIZE  WRK-ZZZ
               MOVE     SUM-G-HKNTEN     TO    WRK-ZZZ
               MOVE     WRK-ZZZ         TO    D600-GG-HKNTEN
           ELSE
      *        7桁以下の場合
               INITIALIZE  WRK-ZZ
               MOVE     SUM-G-HKNTEN     TO    WRK-ZZ
               MOVE     WRK-ZZ         TO    D600-G-HKNTEN
           END-IF
      *    診療種別点数（診察料〜入院料）
           PERFORM  VARYING  IDX-SUM   FROM 1  BY  1
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *              UNTIL    IDX-SUM   >    11
      * 20080325 H20年4月改正対応STRAT tanaka
      *              UNTIL    IDX-SUM   >    15
                    UNTIL    IDX-SUM   >    16
             IF  SUM-G-TEN (IDX-SUM)   >   0
      *       AND IDX-SUM               =  15
               AND IDX-SUM               =  16
      * 20080325 H20年4月改正対応END
                 MOVE     "療養担当"        TO   D600-TRYO
             END-IF
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
             IF  SUM-G-TEN (IDX-SUM)  >  999999
      *          7桁の場合
                 INITIALIZE  WRK-ZZZ
                 MOVE     SUM-G-TEN (IDX-SUM)   TO   WRK-ZZZ
                 MOVE     WRK-ZZZ   TO   D600-GG-TEN (IDX-SUM)
             ELSE
      *          6桁以下の場合
                 INITIALIZE  WRK-ZZ
                 MOVE     SUM-G-TEN (IDX-SUM)   TO   WRK-ZZ
                 MOVE     WRK-ZZ    TO   D600-G-TEN (IDX-SUM)
             END-IF
           END-PERFORM
      *    保険給付額
           IF  SUM-G-HKNFTN  >  9999999
      *        8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-HKNFTN     TO    WRK-SZZ
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
               MOVE     WRK-SZZ          TO    D600-GG-HKNFTN
      *         MOVE     WRK-SZZ         TO    D6001-GG-HKNFTN
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
           ELSE
      *        7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-HKNFTN     TO    WRK-SZ
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
               MOVE     WRK-SZ           TO    D600-G-HKNFTN
      *         MOVE     WRK-SZ         TO    D6001-G-HKNFTN
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
           END-IF
      *    自費負担額
           IF  SUM-G-JIHIFTN  >  9999999
      *        8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-JIHIFTN    TO    WRK-SZZ
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
               MOVE     WRK-SZZ         TO    D600-GG-JIHIFTN
      *         MOVE     WRK-SZZ         TO    D6001-GG-JIHIFTN
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
           ELSE
      *        7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-JIHIFTN    TO    WRK-SZ
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
               MOVE     WRK-SZ         TO    D600-G-JIHIFTN
      *         MOVE     WRK-SZ         TO    D6001-G-JIHIFTN
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
           END-IF
      *    請求金額
           IF  SUM-G-SEIKYU  >  9999999
      *        8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-SEIKYU     TO    WRK-SZZ
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
               MOVE     WRK-SZZ         TO    D600-GG-SEIKYU
      *         MOVE     WRK-SZZ         TO    D6001-GG-SEIKYU
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
           ELSE
      *        7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-SEIKYU     TO    WRK-SZ
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
               MOVE     WRK-SZ         TO    D600-G-SEIKYU
      *         MOVE     WRK-SZ         TO    D6001-G-SEIKYU
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
           END-IF
      *    領収金額
           IF  SUM-G-RYOSYU  >  9999999
      *        8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-RYOSYU     TO    WRK-SZZ
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
               MOVE     WRK-SZZ         TO    D600-GG-RYOSYU
      *         MOVE     WRK-SZZ         TO    D6001-GG-RYOSYU
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
           ELSE
      *        7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-RYOSYU     TO    WRK-SZ
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) start
      *--- 2008/11/05  CHANGE  START -----------------------------------
               MOVE     WRK-SZ         TO    D600-G-RYOSYU
      *         MOVE     WRK-SZ         TO    D6001-G-RYOSYU
      *--- 2008/11/05  CHANGE  END -------------------------------------
      *=== 基本診療項目追加対応 By Takashima(2006/10/28) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SYU-HOSPID        TO  COMB-HOSPID
           MOVE    SYU-HOSPNUM        TO  COMB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SYU-PTID          TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *    患者保険情報読み込み       
               INITIALIZE                  PTHKNINF-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    SYU-HOSPID        TO  PTHKN-HOSPID
               MOVE    SYU-HOSPNUM        TO  PTHKN-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      ***  20040916 MOTOHASI MONPEに変更
      ***     MOVE    "A00000D600"        TO  SPRT-PRTID
      * 20080325 H20年4月改正対応STRAT tanaka
           IF   WRK-PARA-DENPPRTYMD   <   "20080401"
             MOVE    "A00000D600O.red"        TO  SPRT-PRTID
           ELSE
             MOVE    "A00000D600N.red"        TO  SPRT-PRTID
           END-IF
      * 20080325 H20年4月改正対応END
      ***  20040916 MOTOHASI MONPEに変更
           MOVE    "日計表明細（診療費請求明細）"  TO  SPRT-TITLE
           MOVE    D600                       TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
      ***  site-libを実行環境にする為コメントアウト
      *     MOVE    "1"          TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      SPRT-RETURN    =   ZERO
      *--- 2008/11/05  DELETE  START -----------------------------------
      **=== 基本診療項目追加対応 By Takashima   (2006/10/28) start
      *         MOVE    "A00000D6001.red"   TO  SPRT-PRTID
      *         MOVE    "日計表明細（診療費請求明細）"
      *                                     TO  SPRT-TITLE
      *         MOVE    D6001               TO  SPRT-PRTDATA
      *         MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
      *         MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
      *         MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      *         CALL    "ORCSPRT"           USING
      *                                     ORCSPRTAREA
      **=== 4.0対応 By Takashima(2007/09/10) start
      *                                     SPA-AREA
      **=== 4.0対応 By Takashima(2007/09/10) end
      *         IF      SPRT-RETURN         =   ZERO
      *             CONTINUE
      *         ELSE
      *             MOVE    "印刷ＤＢに更新できませんでした"
      *                                        TO  WRK-RECEERR
      *             PERFORM 500-ERR-HENSYU-SEC                
      *         END-IF                                        
      **=== 基本診療項目追加対応 By Takashima(2006/10/28) end
      *--- 2008/11/05  DELETE  END -----------------------------------
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
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT                        
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                   SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-PARA-HOSPID     TO  SMEI-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO  SMEI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    "1"                 TO  SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD TO  SMEI-NYUHEN-YMD
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"             TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"         TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-PARA-HOSPID     TO  SMEI-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO  SMEI-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    "2"                 TO  SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPPRTYMD TO  SMEI-NYUHEN-YMD
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"         TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUMEI-KEY4    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SMEI-HOSPID    TO  SYU-HOSPID
           MOVE    SMEI-HOSPNUM    TO  SYU-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SMEI-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    SMEI-PTID      TO  SYU-PTID
           MOVE    SMEI-DENPNUM   TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syunou"     TO    MCP-TABLE
               MOVE    "key"            TO    MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"              TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL     "MCPSUB"            USING
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    WRK-SMEI-HOSPID(IDX-S)    TO  SYU-HOSPID
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)  TO  SYU-HOSPNUM
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  SYU-NYUGAIKBN
           MOVE    WRK-SMEI-PTID(IDX-S)  TO  SYU-PTID
           MOVE    WRK-SMEI-DENPNUM(IDX-S)  TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syunou"           TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"             TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYUNOU-KEY   TO  MCP-PATH
      *     CALL     "MCPSUB"            USING
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    SYU-HOSPID          TO  HKN-HOSPID  
           MOVE    SYU-HOSPNUM         TO  HKN-HOSPNUM  
      *=== 4.0対応 By Takashima(2007/09/10) end
           MOVE    SYU-SYUHKNNUM       TO  HKN-HKNNUM  
           MOVE    WRK-PARA-DENPPRTYMD TO  HKN-TEKSTYMD
           MOVE    ALL "9"             TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"           USING
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                       MCPAREA
                                       MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                       SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"      TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *         CALL    "MCPSUB"           USING
               MOVE    "tbl_hknnum"       TO  MCP-TABLE
               MOVE    "key5"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                          MCPAREA
                                          MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                          SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL     "MCPSUB"               USING
           MOVE    "tbl_hknnum"            TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                           MCPAREA
                                           MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                           SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      ***     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      ***     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      ***     MOVE    WRK-SYSYMD           TO  ORC-DBYMD
           MOVE    "DBSELECT"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key10"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
               IF      MCP-RC           =   ZERO
                   MOVE    ZERO         TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1            TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1                TO  FLG-SYSKANRI2
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"            TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"                 USING
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                         SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                           MCPAREA
                                           MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                           SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_pthkninf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_pthkninf"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_pthkninf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"            USING
               MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptrsiinf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== 4.0対応 By Takashima(2007/09/10) start
      *         MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
               MOVE    "tbl_hkncombi"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                            MCPAREA
                                            MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                            SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== sarge対応 UP By Takashima(2006/08/01) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/01) end
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
                                        MCPDATA-REC
      *=== 4.0対応 By Takashima(2007/09/10) start
                                        SPA-AREA
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *=== 4.0対応 By Takashima(2007/09/10) end
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
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *=== 4.0対応 By Takashima(2007/09/10) end
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
      *=== 4.0対応 By Takashima(2007/09/10) start
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *=== 4.0対応 By Takashima(2007/09/10) end
                                        MCPAREA
      *=== 4.0対応 By Takashima(2007/09/10) start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *=== 4.0対応 By Takashima(2007/09/10) end
           .
       900-DBCLOSE-EXT.
           EXIT.
