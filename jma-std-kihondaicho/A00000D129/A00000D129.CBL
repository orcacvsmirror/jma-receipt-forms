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
       PROGRAM-ID.             A00000D129.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 基本台帳 外来 年齢別
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/02/09    @@@@-半田     新規作成
      *  04/03/25    @@@@-高橋     月報用に修正
      *  04/04/07    @@@@-高橋     DIAの変更に伴う修正
      *  04/06/26    @@@@-高橋     保険外自費項目が無い場合の数値欄修正
      *  04/09/22    @@@@-本橋     MONPE化
      *  04/12/06    @@@@-本橋     有効期限が設定された医師コードにおいて
      *                            シス管理より医師名が取得できない不具合を修正
      *  05/03/02    @@@@-鈴木     open-cobol 0.23への対応
      *  05/03/25    @@@@-本橋     複数診療科への対応による、
      *                            再診数カウント処理追加
      *  05/04/19    @@@@-本橋     日レセVer2.5.0 収納テーブル取得KEY追加
      *                            への対応
      *  05/09/16    @@@@-本橋     期限が切られた診療科が印字されない不具合修正
      *                            ORCMCPSUB → MCPSUBに変更
      *  05/12/30    @@@@-         ト入金とス消費税修正
      *  06/04/14    @@@@-         再診の集計の修正
      *  06/08/17    @@@@-高嶋     sarge対応
      *  06/11/14    @@@@-高嶋     自費、基本診療項目追加対応
      *  07/06/27    @@@@-吉川     初診、再診、その他振分け修正
      *  07/09/13    @@@@-田中     4.0対応,助成金額の修正,減免金額の追加
      *  07/10/11    @@@@-田中     助成金額がマイナスになってしまう不具合を修正
      *  07/10/24    @@@@-田中     入金額の重複を修正
      *  08/02/26    @@@@-田中     4.0対応漏れ修正
      *  08/04/01    @@@@-田中     H20年4月改正対応
      *  08/07/08    @@@@-田中     収納明細を更新した場合に、入金額が表示されない不具合の修正
      *  09/02/25    @@@@-門間     預り金の重複を修正
      *  09/03/04    @@@@-田中     再診、その他の件数処理を修正
      *  09/04/16    @@@@-門間     プログラムオプション対応(INIファイル)
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
      *--- プログラムオプション対応(INIファイル) 2009/04/16 DELETE START
      **    基本台帳用INIファイル
      *     SELECT  D100I-FILE      ASSIGN  D100IPARA
      *                             ORGANIZATION    SEQUENTIAL
      *                             ACCESS  MODE    SEQUENTIAL
      *                             FILE    STATUS  IS  STS-PARA.
      *--- プログラムオプション対応(INIファイル) 2009/04/16 DELETE END
      *
      *    基本台帳(外来年齢別)用対象患者ファイル
           SELECT  D129PT-FILE     ASSIGN  D129PPARA
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     D129P-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
      *--- プログラムオプション対応(INIファイル) 2009/04/16 DELETE START
      **    基本台帳用INIファイル
      * FD  D100I-FILE.
      * 01  D100I. 
      *     03  D100I-SRYCD         PIC X(09).
      *     03  FILLER              PIC X(81).
      *--- プログラムオプション対応(INIファイル) 2009/04/16 DELETE END
      *
      *    基本台帳(外来年齢別)用対象患者ファイル
       FD  D129PT-FILE.
       01  D129P. 
           03    D129P-KEY.
             05  D129P-SRYYMD      PIC X(08).
             05  D129P-PTID        PIC 9(10).
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域 
      * 20070913 4.0対応START t.tanaka
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      * 20070913 4.0対応END
      *
       01  D100IPARA.
           03  FILLER              PIC X(256) VALUE  SPACE.

      *--- プログラムオプション対応(INIファイル) 2009/04/16 DELETE START
      * 01  D100INAME.
      *     03  FILLER              PIC  X(05) VALUE  "data/".
      *     03  FILLER              PIC  X(10) VALUE  "A00000D100".
      *     03  FILLER              PIC  X(04) VALUE  ".INI".
      *--- プログラムオプション対応(INIファイル) 2009/04/16 DELETE END
      *
      *    診療日別受信患者ファイル
       01  D129PPARA.
           03  FILLER   PIC  X(05) VALUE "/tmp/".
           03  D129TPARA-FILENAME  PIC  X(12) VALUE "A00000D129PT".
           03  FILLER              PIC  X(04) VALUE ".TMP".
      *
      * 基本台帳(外来年齢別)DIA用コピー句
           COPY    "A00000D129.INC".
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
           COPY    "A00000D1291.INC".
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      *
      *****
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
      *--- 2009/03/04 ADD START ----------------------------------------
           03  FLG-JYURRK2         PIC 9(01).
      *--- 2009/03/04 ADD END ----------------------------------------
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INVALID         PIC 9(01).
           03  FLG-820SYS          PIC X(04).
           03  INP-STS             PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE          PIC 9(02).
           03  CNT-PAGE          PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY        PIC 9(02).
               05  SYS-MM        PIC 9(02).
               05  SYS-DD        PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  I1                PIC 9(04).
           03  I2                PIC 9(04).
           03  I3                PIC 9(04).
           03  I4                PIC 9(04).
           03  I5                PIC 9(04).
           03  I6                PIC 9(04).
      * 20040407 追加 Y.Takahashi
           03  I7                PIC 9(04).
      */20040407 追加
           03  IDX1              PIC 9(04).
           03  IDX2              PIC 9(04).
           03  IDX3              PIC 9(04).
           03  IDX4              PIC 9(04).
           03  IDX5              PIC 9(04).
           03  IDX6              PIC 9(04).
           03  IDX7G.    
             05    IDX7          PIC 9(02).
           03  IDX8G.
             05    IDX8          PIC 9(04).
           03  IDX               PIC 9(04).
           03  IDY               PIC 9(04).
           03  IDZ               PIC 9(04).
           03  IDX-S             PIC 9(07).
           03  IDX-N             PIC 9(07).
           03  IDX-MAX           PIC 9(07).
      *
           03  JIHIIDX           PIC 9(04).
           03  NG                PIC 9(04).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
           03  IDX9              PIC 9(04).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      *--- 2009/03/04 ADD START ----------------------------------------
           03  IDX10             PIC 9(02).
           03  IDX11             PIC 9(02).
      *--- 2009/03/04 ADD END ------------------------------------------
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH              PIC X(64).
      *
           03  WRK-RECEERR           PIC X(200).
      *
           03  WRK-SRYYMD1           PIC X(22).
           03  WRK-SRYYMD2           PIC X(22).
           03  WRK-PARA-DENPPRTYMDWH PIC X(22).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-RENNUM            PIC 9(03).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
           03  WRK-SYUHKNNUM         PIC X(03).
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
               05 WRK-BIRTHY         PIC 9(04).
               05 WRK-BIRTHM         PIC 9(02).
               05 WRK-BIRTHD         PIC 9(02).
           03  WRK-NENREI            PIC 9(02).
      *  
           03  WRK-AZUKARIYMD.
               05  WRK-AZUKARIYY     PIC 9(04).
               05  WRK-AZUKARIMM     PIC 9(02).
               05  WRK-AZUKARIDD     PIC 9(02).
      *
      *****03  WRK-IN                PIC X(01).
      *
           03  WRK-SYUHKN-KBN        PIC 9(01).
           03  WRK-INI-AZUKARICD     PIC X(09).
           03  WRK-GAITENTTLKBN      PIC 9(03).
           03  WRK-BF-MISYU          PIC S9(09).
           03  WRK-BF-AZUKARI        PIC S9(09).
           03  WRK-TEN               PIC S9(09).
           03  WRK-MONEY             PIC S9(09).
           03  WRK-JIHI              PIC S9(09).
           03  WRK-SYUEKI            PIC S9(09).
           03  WRK-RATE              PIC 9(01)V9(03).
           03  WRK-RATE2             PIC 9(03).
      *
           03  WRK-JIHI2            PIC 9(08).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      *     03  WRK-TJIHI-G       OCCURS  5.
           03  WRK-TJIHI-G       OCCURS  10.
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
               05  WRK-TJIHI        PIC X(12).
               05  WRK-TJIHI-FLG    PIC 9(01).
      *
           03  WRK-4001-TENTANKA    PIC 9(02)V9(01).
           03  WRK-RSI-MONEY1       PIC S9(09)V9(01). 
           03  WRK-RSI-MONEY2       PIC S9(09)V9(01). 
      *
           03  WRK-RENNUM-X         PIC ZZ9.
           03  WRK-PAGE             PIC ZZ9.
           03  WRK-RATE-Z9          PIC ZZ9.  
           03  WRK-Z9               PIC ZZZZZZZZ9.
           03  WRK-ZZ               PIC ZZZZZZZZZ.
           03  WRK-S9               PIC --------9.
           03  WRK-SZ               PIC --------Z.
           03  WRK-NYUGAIKBN        PIC X(01).
           03  WRK-SRYKA            PIC X(02).
           03  WRK-DRCD             PIC X(05).
           03  WRK-SRYYMD           PIC 9(08).
           03  WRK-SEL-SRYKACD      PIC X(02).
           03  WRK-SEL-SRYKANAME    PIC X(20).
           03  WRK-SEL-DRCD         PIC X(04).
           03  WRK-SEL-DRNAME       PIC X(20).
           03  DEBUG-CNT            PIC 9(03).
           03  WRK-SYS-BEDSU        PIC 9(04).
           03  WRK-SYS-BEDSUIPN     PIC 9(04).
           03  WRK-FTNRATE          PIC 9(01)V9(02).
           03  WRK-KYFRATE          PIC 9(01)V9(02).
      *
           03  WRK-TMP-YMD.
            05 WRK-TMP-YY        PIC 9(04).
            05 WRK-TMP-MM        PIC 9(02).
            05 WRK-TMP-DD        PIC 9(02).
           03  WRK-TMP-LYMD.
            05 WRK-TMP-LYY       PIC 9(04).
            05 WRK-TMP-LMM       PIC 9(02).
            05 WRK-TMP-LDD       PIC 9(02).
      *
      * 20040401 追加 Y.Takahashi 
           03  WRK-STARTYMD      PIC 9(08).
           03  WRK-LASTYMD       PIC 9(08).
           03  WRK-TABLECOL      PIC 9(02).
           03  WRK-ACCT-SDAY     PIC 9(02).
           03  WRK-ACCT-EDAY     PIC 9(02).
           03  WRK-ACCT-IN       PIC 9(04).
           03  WRK-JIHIMONEY     PIC 9(10).
           03  WRK-AZUKARI       PIC 9(10).
           03  WRK-AGE-BIRTHDAY.
            05 WRK-AGE-BIRTHY    PIC 9(04).
            05 WRK-AGE-BIRTHM    PIC 9(02).
            05 WRK-AGE-BIRTHD    PIC 9(02).
           03  WRK-AGE-SRYYMD.
            05 WRK-AGE-SRYY      PIC 9(04).
            05 WRK-AGE-SRYM      PIC 9(02).
            05 WRK-AGE-SRYD      PIC 9(02).
           03  WRK-AGE           PIC 9(04).
      ****2005/12/30  @@@@-BRAND START
           03  WRK-SMEI-NYUHEN-MONEY  PIC S9(07).
           03  WRK-SUM-SYOHI          PIC S9(07).
      ****2005/12/30  @@@@-BRAND END
      * 20071011追加 T.Tanaka
           03  WRK-GENMEN        PIC 9(09).
      * 20071011追加 
      */20040401 追加
       01  SUM-AREA.
           03         SUM-AGEMAX        PIC  9(03).
           03         SUM-SRYKA-G.    
             05       SUM-SRYKACD       PIC  X(02).
             05       SUM-SRYKANAME     PIC  X(20).
           03         SUM-DR-G.    
             05       SUM-DRCD          PIC  X(04).
             05       SUM-DRNAME        PIC  X(20).
           03         SUM-PRFLG         PIC  9(01).
      * 20040402 編集 Y.Takahashi 
           03         SUM-AGE-TBL       OCCURS   26.
             05       SUM-AGECOLMAX     PIC  9(03).
             05       SUM-AGENAME       PIC  X(12).
             05       SUM-BF-MISYU      PIC S9(11).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      *       05       SUM-YOSO-TBL      OCCURS   39.
      * 20080401 H20年4月改正対応START tanaka
      *       05       SUM-YOSO-TBL      OCCURS   47.
             05       SUM-YOSO-TBL      OCCURS   48.
      * 20080401 H20年4月改正対応END
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
               07     SUM-YOSO          PIC S9(11).
      */20040402 編集
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      * 20080401 H20年4月改正対応START tanaka
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
      * 20080401 H20年4月改正対応END
      *
           COPY    "CPSHELLTBL.INC".
      *     
      * 20070913 4.0対応START t.tanaka
      *     COPY    "ORCA-DBPATH".         
      * 20070913 4.0対応END
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
      *    職員情報
           COPY    "CPSK1010.INC".
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
      *    労災情報取得
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
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      * 20040325 追加 Y.Takahashi   
      *    請求管理マスタ
           COPY    "CPRCF010.INC".
      */20040325 追加
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
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
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
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *--- プログラムオプション対応(INIファイル) 2009/04/16 ADD START
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *--- プログラムオプション対応(INIファイル) 2009/04/16 ADD END
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "A00000D100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000D100-LNK.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF  INP-STS  =   ZERO
      ***    正常主処理
             PERFORM 200-MAIN-SEC
           ELSE
      ***    エラー処理
             PERFORM 210-ERR-SEC
           END-IF
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
      * 20070913 4.0対応START t.tanaka
           INITIALIZE                  SPA-AREA
      * 20070913 4.0対応END
      *
           PERFORM 100-DBOPEN-SEC
      * 20070913 4.0対応START t.tanaka
           MOVE    D100LNK-HOSPNUM     TO  SPA-HOSPNUM
      * 20070913 4.0対応END
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      * 20070913 4.0対応END
           MOVE    D100LNK-JOBID   TO  JOB-JOBID
           MOVE    D100LNK-SHELLID TO  JOB-SHELLID
           MOVE    "A00000D129"    TO  JOB-PGID
      *
           MOVE    "基本台帳(外来・年齢別)"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      * 20070913 4.0対応START t.tanaka
                                   SPA-AREA
      *
      *
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    D100LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      * 20070913 4.0対応END
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
           MOVE    D100LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG             TO  WRK-SYSYMDWH 
      *
      *    集計対象診療年月日１
           INITIALIZE  WRK-SYMD
           MOVE    D100LNK-YMD1        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SRYYMD1
      *
      *    集計対象診療年月日２
           INITIALIZE  WRK-SYMD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SRYYMD2 
      *
      * 20040325 追加 Y.Takahashi
           MOVE  D100LNK-YMD1    TO  WRK-STARTYMD
           MOVE  D100LNK-YMD2    TO  WRK-LASTYMD
      */20040325 追加 
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      ***  20050916  MOTO
           MOVE  D100LNK-YMD1          TO  SYS-1001-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1001-STYUKYMD
      ***  20050916  MOTO
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      * 20070913 4.0対応END
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      * 20070913 4.0対応START t.tanaka
      *         MOVE    SYS-1001-HOSPID     TO  D100LNK-HOSPID
               MOVE    SYS-1001-HOSPNUM     TO  D100LNK-HOSPNUM
      * 20070913 4.0対応END
               MOVE    SYS-1001-BEDSU      TO  WRK-SYS-BEDSU
               MOVE    SYS-1001-BEDSUIPN   TO  WRK-SYS-BEDSUIPN
           ELSE    
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
      *
      * 画面入力パラメタ値のチェック
      *
      *    集計対象か期間のチェック
           IF   D100LNK-YMD1    >   D100LNK-YMD2
                MOVE    10      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF   D100LNK-YMD1(1:6)   NOT =   D100LNK-YMD2(1:6)
                MOVE    21      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    入外区分のチェック
           IF   D100LNK-NYUGAIKBN   NOT =   "1"
            AND D100LNK-NYUGAIKBN   NOT =   "2"
                MOVE    20      TO  INP-STS 
                GO              TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    診療科コードのチェック
           IF  D100LNK-SRYKACD     NOT  =  SPACE
               INITIALIZE                      SYS-1005-REC
      * 20070913 4.0対応START t.tanaka
               MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      * 20070913 4.0対応END
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    D100LNK-SRYKACD     TO  SYS-1005-KBNCD
      * 20040325 編集 Y.Takahashi
      *         MOVE    D100LNK-YMD2        TO  WRK-SYMD
               MOVE    WRK-LASTYMD          TO  WRK-SYMD
      */20040325 編集
      ***  20050916  MOTO
               MOVE  D100LNK-YMD1          TO  SYS-1005-EDYUKYMD
               MOVE  D100LNK-YMD2          TO  SYS-1005-STYUKYMD
      ***  20050916  MOTO
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 810-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI2        =  ZERO
                       MOVE  MCPDATA-REC   TO  SYS-1005-REC
                       MOVE  SYS-1005-KBNCD(1:2)
                                           TO  WRK-SEL-SRYKACD
                       MOVE  SYS-1005-SRYKANAME1
                                           TO  WRK-SEL-SRYKANAME
               ELSE
                       MOVE  30            TO  INP-STS
                       GO                  TO  110-PARA-HENSYU-EXT
               END-IF                                     
           END-IF                                     
      *
      *    医師コードのチェック
           IF  D100LNK-DRCD        NOT  =  SPACE
               INITIALIZE                      SYS-1010-REC
      * 20070913 4.0対応START t.tanaka
               MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      * 20070913 4.0対応END
               MOVE    "1010"              TO  SYS-1010-KANRICD
               MOVE    D100LNK-DRCD        TO  SYS-1010-KBNCD
      * 20040325 編集 Y.Takahashi
      *         MOVE    D100LNK-YMD2        TO  WRK-SYMD
      ***  2004/12/06  MOTO
      ***         MOVE    WRK-LASTYMD          TO  WRK-SYMD
               MOVE    WRK-LASTYMD          TO  WRK-SYSYMD
      */20040325 編集
      ***  20050916  MOTO
               MOVE  D100LNK-YMD1          TO  SYS-1010-EDYUKYMD
               MOVE  D100LNK-YMD2          TO  SYS-1010-STYUKYMD
      ***  20050916  MOTO
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               PERFORM 810-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI2       =   ZERO
                 AND   D100LNK-DRCD(1:1)   NOT =   "0"
                       MOVE  MCPDATA-REC   TO  SYS-1010-REC
                       MOVE  SYS-1010-KBNCD(2:4)
                                           TO  WRK-SEL-DRCD
                       MOVE  SYS-1010-NAME
                                           TO  WRK-SEL-DRNAME
               ELSE
                       MOVE  40            TO  INP-STS
                       GO                  TO  110-PARA-HENSYU-EXT
               END-IF                                     
           END-IF                                     
      *
      *    外来自費情報取得
      *    区分21
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "21"                TO  SYS-1013-KBNCD
      * 20040325 編集 Y.Takahashi
      *         MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
      */20040325 編集
      ***  20050916  MOTO
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
      ***  20050916  MOTO
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(1)
               MOVE  1                 TO  WRK-TJIHI-FLG(1)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(1)
           END-IF                                     
      *  
      *    区分22
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "22"                TO  SYS-1013-KBNCD
      * 20040325 編集 Y.Takahashi
      *         MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
      */20040325 編集
      ***  20050916  MOTO
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
      ***  20050916  MOTO
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(2)
               MOVE  1                 TO  WRK-TJIHI-FLG(2)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(2)
           END-IF                                     
      *  
      *    区分23
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
            MOVE    SPA-HOSPNUM        TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "23"                TO  SYS-1013-KBNCD
      * 20040325 編集 Y.Takahashi
      *         MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
      */20040325 編集
      ***  20050916  MOTO
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
      ***  20050916  MOTO
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(3)
               MOVE  1                 TO  WRK-TJIHI-FLG(3)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(3)
           END-IF                                     
      *  
      *    区分24
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "24"                TO  SYS-1013-KBNCD
      * 20040325 編集 Y.Takahashi
      *         MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
      */20040325 編集
      ***  20050916  MOTO
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
      ***  20050916  MOTO
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(4)
               MOVE  1                 TO  WRK-TJIHI-FLG(4)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(4)
           END-IF                                     
      *
      *    区分25
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "25"                TO  SYS-1013-KBNCD
      * 20040325 編集 Y.Takahashi
      *         MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
      */20040325 編集
      ***  20050916  MOTO
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
      ***  20050916  MOTO
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(5)
               MOVE  1                 TO  WRK-TJIHI-FLG(5)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(5)
           END-IF                                     
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      *
      *    区分26
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "26"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(6)
               MOVE  1                 TO  WRK-TJIHI-FLG(6)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(6)
           END-IF                                     
      *
      *    区分27
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "27"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(7)
               MOVE  1                 TO  WRK-TJIHI-FLG(7)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(7)
           END-IF                                     
      *
      *    区分28
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "28"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(8)
               MOVE  1                 TO  WRK-TJIHI-FLG(8)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(8)
           END-IF                                     
      *
      *    区分29
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "29"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(9)
               MOVE  1                 TO  WRK-TJIHI-FLG(9)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(9)
           END-IF                                     
      *
      *    区分20
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "20"                TO  SYS-1013-KBNCD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
           MOVE  D100LNK-YMD1          TO  SYS-1013-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(10)
               MOVE  1                 TO  WRK-TJIHI-FLG(10)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(10)
           END-IF                                     
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      *
      *    労災医療機関情報
           MOVE    SPACE                TO  SYS-4001-REC
           INITIALIZE                       SYS-4001-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM          TO  SYS-4001-HOSPNUM
      * 20070913 4.0対応END 
           MOVE    "4001"               TO  SYS-4001-KANRICD
           MOVE    "*"                  TO  SYS-4001-KBNCD
      * 20040325 編集 Y.Takahashi
      *         MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    WRK-LASTYMD         TO  WRK-SYMD
      */20040325 編集
      ***  20050916  MOTO
           MOVE  D100LNK-YMD1          TO  SYS-4001-EDYUKYMD
           MOVE  D100LNK-YMD2          TO  SYS-4001-STYUKYMD
      ***  20050916  MOTO
           MOVE    SYS-4001-REC         TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
                   MOVE  MCPDATA-REC    TO  SYS-4001-REC
                   EVALUATE  SYS-4001-TENKBN
                       WHEN  "1"    
                             MOVE  11.5 TO  WRK-4001-TENTANKA
                       WHEN  "2"    
                             MOVE  12   TO  WRK-4001-TENTANKA
                       WHEN  OTHER    
                             MOVE  10   TO  WRK-4001-TENTANKA
                  END-EVALUATE
           ELSE
                  MOVE  10   TO  WRK-4001-TENTANKA
           END-IF                                     
      *
           IF   WRK-SEL-SRYKACD   =    SPACE
                MOVE     1     TO   I1 
                MOVE "99"      
                          TO   SUM-SRYKACD  
                MOVE "全科合計"
                          TO   SUM-SRYKANAME
           ELSE
                MOVE     1     TO   I1 
                MOVE WRK-SEL-SRYKACD      
                          TO   SUM-SRYKACD  
                MOVE WRK-SEL-SRYKANAME
                          TO   SUM-SRYKANAME
           END-IF
      *
           PERFORM  110-DR-PARA-HENSYU-SEC
      *
      * 20040330 削除 Y.Takahashi
      *     IF    SUM-DRCD     =  "9999"
      *      AND  SUM-SRYKACD  =  "99"
      *         MOVE     "実日数"          TO  SUM-JITUMSG
      *     END-IF
      */20040330 削除 Y.Takahashi
      *
      *--- プログラムオプション対応(INIファイル) 2009/04/16 CHANGE START
      **    基本台帳用INIファイル情報抽出
      *     INITIALIZE              ORCSMKPASSAREA
      *     MOVE  D100INAME     TO  MKPASS-IN-01
      *     CALL  "ORCSMKPASSSITE"  USING  ORCSMKPASSAREA
      *     MOVE  MKPASS-OT-01  TO  D100IPARA
      **
      *     OPEN  INPUT D100I-FILE
      *     INITIALIZE  D100I
      *     READ  D100I-FILE
      *     END-READ
      *     CLOSE       D100I-FILE
      *     IF     D100I-SRYCD   NOT =  SPACE
      *       MOVE D100I-SRYCD       TO     WRK-INI-AZUKARICD
      *     END-IF
      *
      *    プログラムオプション取込み（預り金コード）
           INITIALIZE                      ORCSPRGOPTIONAREA
           MOVE    "A00000D100"        TO  ORCSPRGOPTION-PRGID
           MOVE    "AZUKARICD"         TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           IF  ORCSPRGOPTION-OT-PARA(1:9)  NOT =  SPACE
             MOVE  ORCSPRGOPTION-OT-PARA(1:9)  TO  WRK-INI-AZUKARICD
           END-IF
      *--- プログラムオプション対応(INIファイル) 2009/04/16 CHANGE END
      *
      *    点数マスタ読込み
           MOVE   WRK-INI-AZUKARICD TO     TNS-SRYCD
      * 20080226 4.0対応漏れ修正START t.tanaka
           MOVE   SPA-HOSPNUM       TO     TNS-HOSPNUM
      * 20080226 4.0対応漏れ修正END
           PERFORM   910-TENSU-READ-SEC
           MOVE   TNS-GAITENTTLKBN  TO     WRK-GAITENTTLKBN
      *
      *    基本台帳(外来年齢別)用対象患者ファイル初期化
           OPEN  OUTPUT D129PT-FILE
           CLOSE        D129PT-FILE  
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    医師名編集処理
      *****************************************************************
       110-DR-PARA-HENSYU-SEC                   SECTION.
      *
           IF   WRK-SEL-DRCD    =        SPACE
                MOVE "9999"              TO   SUM-DRCD  
                MOVE "全医師合計"        TO   SUM-DRNAME
           ELSE
                MOVE WRK-SEL-DRCD        TO   SUM-DRCD  
                MOVE WRK-SEL-DRNAME      TO   SUM-DRNAME
           END-IF
      *     PERFORM  110-SRYMD-PARA-HENSYU-SEC
           PERFORM  110-AGE-PARA-HENSYU-SEC
      *
           .
       110-DR-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢編集処理
      *****************************************************************
       110-AGE-PARA-HENSYU-SEC                   SECTION.
      *
      * 年齢順カラム作成
           MOVE   2               TO  SUM-AGECOLMAX(1)
           MOVE   "０〜２歳"      TO  SUM-AGENAME(1)  
           MOVE   5               TO  SUM-AGECOLMAX(2)
           MOVE   "３〜５歳"      TO  SUM-AGENAME(2)  
           MOVE   9               TO  SUM-AGECOLMAX(3)
           MOVE   "６〜９歳"      TO  SUM-AGENAME(3)  
           MOVE   19              TO  SUM-AGECOLMAX(4)
           MOVE   "１０〜１９歳"  TO  SUM-AGENAME(4)  
           MOVE   29              TO  SUM-AGECOLMAX(5)
           MOVE   "２０〜２９歳"  TO  SUM-AGENAME(5)  
           MOVE   39              TO  SUM-AGECOLMAX(6)
           MOVE   "３０〜３９歳"  TO  SUM-AGENAME(6)  
           MOVE   49              TO  SUM-AGECOLMAX(7)
           MOVE   "４０〜４９歳"  TO  SUM-AGENAME(7)  
           MOVE   59              TO  SUM-AGECOLMAX(8)
           MOVE   "５０〜５９歳"  TO  SUM-AGENAME(8)  
           MOVE   69              TO  SUM-AGECOLMAX(9)
           MOVE   "６０〜６９歳"  TO  SUM-AGENAME(9)  
           MOVE   79              TO  SUM-AGECOLMAX(10)
           MOVE   "７０〜７９歳"  TO  SUM-AGENAME(10)  
           MOVE   89              TO  SUM-AGECOLMAX(11)
           MOVE   "８０〜８９歳"  TO  SUM-AGENAME(11)  
           MOVE   99              TO  SUM-AGECOLMAX(12)
           MOVE   "９０〜９９歳"  TO  SUM-AGENAME(12)  
           MOVE   999             TO  SUM-AGECOLMAX(13)
           MOVE   "１００歳〜"    TO  SUM-AGENAME(13)  
      *   
           MOVE     "計"        TO   SUM-AGENAME(14)  
           MOVE     14          TO   SUM-AGEMAX   
      *
           .
       110-AGE-PARA-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    基本台帳(外来年齢別)用対象患者ファイルOPEN
           OPEN  I-O D129PT-FILE
      *
      *    収納情報集計処理
           PERFORM 320-SYUNOU-CNT-SEC
      *
      *    基本台帳(外来年齢別)用対象患者ファイルCLOSE
           CLOSE     D129PT-FILE
      *
      *    帳票編集印刷処理
           PERFORM 330-TERM-HEN-SEC
           MOVE   1    TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面入力値阿エラー処理
      *****************************************************************
       210-ERR-SEC                SECTION.
      *
           INITIALIZE                   D129
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
           INITIALIZE                   D1291
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      *
           MOVE   "(外来・年齢別)"    TO    D129-SUBTITLE
           MOVE    WRK-SYSYMDWH   TO    D129-SYSYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
           MOVE   "(外来・年齢別)"    TO    D1291-SUBTITLE
           MOVE    WRK-SYSYMDWH       TO    D1291-SYSYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      *
           EVALUATE INP-STS 
           WHEN 10
             MOVE
             "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO    D129-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
             MOVE
             "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO    D1291-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
           WHEN 11
             MOVE
             "入力する集計対象期間の範囲は同年内で指定をして下さい"
                                  TO    D129-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
             MOVE
             "入力する集計対象期間の範囲は同年内で指定をして下さい"
                                  TO    D1291-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
           WHEN 20
             MOVE
             "入力された入外区分の値に誤りがあります"
                                  TO    D129-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
             MOVE
             "入力された入外区分の値に誤りがあります"
                                  TO    D1291-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
           WHEN 21
             MOVE
             "入力する集計対象期間の範囲は同年月内で指定をして下さい"
                                  TO    D129-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
             MOVE
             "入力する集計対象期間の範囲は同年月内で指定をして下さい"
                                  TO    D1291-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
           WHEN 30
             MOVE
             "入力された診療科コードの値に誤りがあります"
                                  TO    D129-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
             MOVE
             "入力された診療科コードの値に誤りがあります"
                                  TO    D1291-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
           WHEN 40
             MOVE
             "入力された医師コードの値に誤りがあります"
                                  TO    D129-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
             MOVE
             "入力された医師コードの値に誤りがあります"
                                  TO    D1291-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
           END-EVALUATE
      *
           PERFORM 340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報集計処理
      *****************************************************************
       320-SYUNOU-CNT-SEC      SECTION.
      *
           PERFORM  910-DBSELECT-SEC
           PERFORM  UNTIL      FLG-SYUNOU  = 1
      *
            INITIALIZE         PTINF-REC 
      * 20070913 4.0対応START t.tanaka
      *      MOVE SYU-HOSPID TO PTINF-HOSPID
            MOVE SYU-HOSPNUM TO PTINF-HOSPNUM
      * 20070913 4.0対応END
            MOVE SYU-PTID   TO PTINF-PTID
            PERFORM 900-PTINF-READ-SEC
      *
      *   集計対象年月のデータであるかをチェックする。 
            IF      SYU-SRYYMD             >=  D100LNK-YMD1
              AND   SYU-SRYYMD             <=  D100LNK-YMD2
      * 20070913 4.0対応START t.tanaka
      *        AND   SYU-HOSPID              =  D100LNK-HOSPID
              AND   SYU-HOSPNUM              =  D100LNK-HOSPNUM
      * 20070913 4.0対応END
              AND   SYU-CREATEKBN       NOT =  "3"
              AND   SYU-DENPJTIKBN      NOT =  "3"
              AND   SYU-NYUGAIKBN           =  "2"
              AND ((WRK-SEL-SRYKACD         =  SPACE)
               OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                    SYU-SRYKA               =  WRK-SEL-SRYKACD))
              AND ((WRK-SEL-DRCD            =  SPACE)
               OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                    SYU-DRCD                =  WRK-SEL-DRCD))
              AND   FLG-PTINF               =  ZERO
              AND   PTINF-TSTPTNUMKBN   NOT =  "1"  
      * 伝票作成区分 = 3(入院レセプト用ダミー)は対象外データとして取り扱う
      * 伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      * 患者情報にレコードが存在しないデータは抽出しない。
      * テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
      ***  20030704 収納情報抽出条件変更
      *
               MOVE  PTINF-BIRTHDAY   TO  WRK-AGE-BIRTHDAY
               MOVE  SYU-SRYYMD       TO  WRK-AGE-SRYYMD
               PERFORM  320-AGE-GROUPING-SEC
               IF I3  <     SUM-AGEMAX
                  IF  FLG-GET = ZERO
                       MOVE   1   TO   FLG-GET
                  END-IF
                  PERFORM 320-SYUNOU-CNT-SUB-SEC
               END-IF
            ELSE
               IF     SYU-SRYYMD              <  WRK-STARTYMD
      * 20070913 4.0対応START t.tanaka
      *          AND   SYU-HOSPID              =  D100LNK-HOSPID
                AND   SYU-HOSPNUM              =  D100LNK-HOSPNUM
      * 20070913 4.0対応END
                AND   SYU-CREATEKBN       NOT =  "3"
                AND   SYU-DENPJTIKBN      NOT =  "3"
                AND   SYU-NYUGAIKBN           =  "2"
                AND ((WRK-SEL-SRYKACD         =  SPACE)
                 OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                      SYU-SRYKA               =  WRK-SEL-SRYKACD))
                AND ((WRK-SEL-DRCD            =  SPACE)
                 OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                      SYU-DRCD                =  WRK-SEL-DRCD))
                AND   FLG-PTINF               =  ZERO
                AND   PTINF-TSTPTNUMKBN   NOT =  "1"   
      ***        範囲指定期間前未収金の集計
                   MOVE  PTINF-BIRTHDAY   TO  WRK-AGE-BIRTHDAY
                   MOVE  SYU-SRYYMD       TO  WRK-AGE-SRYYMD
                   PERFORM  320-AGE-GROUPING-SEC
                   IF I3  <     SUM-AGEMAX
                       COMPUTE  WRK-MONEY    =
                                SYU-SKYMONEY -  SYU-NYUKIN-TOTAL
                                ADD   WRK-MONEY
                                 TO  SUM-BF-MISYU(I3)
      ****2005/12/30  @@@@-BRAND  START
                    PERFORM 320-SYUMEI-CNT-SUB-SEC
      ****2005/12/30  @@@@-BRAND  END
                   END-IF
               END-IF
            END-IF
            PERFORM 910-DBFETCH-SEC
           END-PERFORM
           PERFORM 910-DBCLOSE-SEC
      *
      *    その他集計項目の加算
           PERFORM  320-ETC-CNT-SEC
      *
           .
       320-SYUNOU-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他項目集計処理
      *****************************************************************
       320-ETC-CNT-SEC      SECTION.
      *
           PERFORM  VARYING  I3     FROM 1  BY  1
                    UNTIL    I3     >=   SUM-AGEMAX
      *
      * 新患来院率
                    INITIALIZE                WRK-RATE
                                              WRK-RATE2
                    COMPUTE  WRK-RATE =
                             SUM-YOSO(I3 6) / SUM-YOSO(I3 5)
                    COMPUTE  WRK-RATE2 ROUNDED     =
                             WRK-RATE  *  100
                    MOVE     WRK-RATE2    TO  WRK-RATE-Z9
                    MOVE     WRK-RATE-Z9  TO  SUM-YOSO(I3 7)
      *
      * 平均点／人
                    INITIALIZE                WRK-TEN
                    COMPUTE  WRK-TEN  ROUNDED      =
                             SUM-YOSO (I3 8) / 
                             SUM-YOSO (I3 5)
                    MOVE     WRK-TEN  TO  SUM-YOSO (I3 9)
      *
      * 過日未収金残額
                    MOVE      SUM-BF-MISYU(I3)
                              TO  SUM-YOSO (I3 17)
                    ADD       SUM-BF-MISYU(I3)
                              TO  SUM-YOSO (SUM-AGEMAX 17)
      * 入金額
      * 入金額から当日預り金を引き去る              
                   COMPUTE SUM-YOSO(I3 18) =
                   SUM-YOSO(I3 18) -
                   SUM-YOSO(I3 20)
                   COMPUTE SUM-YOSO(SUM-AGEMAX 18) =
                   SUM-YOSO(SUM-AGEMAX 18) -
                   SUM-YOSO(I3 20)
      *
      * 自費金額から当日預り金を引き去る
                   EVALUATE WRK-GAITENTTLKBN
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      *                WHEN  1                
      *                      COMPUTE SUM-YOSO(I3 33) =
      *                      SUM-YOSO(I3 33) -
      *                      SUM-YOSO(I3 20)
      *                      COMPUTE SUM-YOSO(SUM-AGEMAX 33) =
      *                      SUM-YOSO(SUM-AGEMAX 33) -
      *                      SUM-YOSO(I3 20)
      *                WHEN  2                
      *                      COMPUTE SUM-YOSO(I3 34) =
      *                      SUM-YOSO(I3 34) -
      *                      SUM-YOSO(I3 20)
      *                      COMPUTE SUM-YOSO(SUM-AGEMAX 34) =
      *                      SUM-YOSO(SUM-AGEMAX 34) -
      *                      SUM-YOSO(I3 20)
      *                WHEN  3                
      *                      COMPUTE SUM-YOSO(I3 35) =
      *                      SUM-YOSO(I3 35) -
      *                      SUM-YOSO(I3 20)
      *                      COMPUTE SUM-YOSO(SUM-AGEMAX 35) =
      *                      SUM-YOSO(SUM-AGEMAX 35) -
      *                      SUM-YOSO(I3 20)
      *                WHEN  4                
      *                      COMPUTE SUM-YOSO(I3 36) =
      *                      SUM-YOSO(I3 36) -
      *                      SUM-YOSO(I3 20)
      *                      COMPUTE SUM-YOSO(SUM-AGEMAX 36) =
      *                      SUM-YOSO(SUM-AGEMAX 36) -
      *                      SUM-YOSO(I3 20)
      *                WHEN  5                
      *                      COMPUTE SUM-YOSO(I3 37) =
      *                      SUM-YOSO(I3 37) -
      *                      SUM-YOSO(I3 20)
      *                      COMPUTE SUM-YOSO(SUM-AGEMAX 37) =
      *                      SUM-YOSO(SUM-AGEMAX 37) -
      *                      SUM-YOSO(I3 20)
      *-----------------------------------------------------------
                      WHEN  1                
                            COMPUTE SUM-YOSO(I3 38) =
                            SUM-YOSO(I3 38) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 38) =
                            SUM-YOSO(SUM-AGEMAX 38) -
                            SUM-YOSO(I3 20)
                      WHEN  2                
                            COMPUTE SUM-YOSO(I3 39) =
                            SUM-YOSO(I3 39) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 39) =
                            SUM-YOSO(SUM-AGEMAX 39) -
                            SUM-YOSO(I3 20)
                      WHEN  3                
                            COMPUTE SUM-YOSO(I3 40) =
                            SUM-YOSO(I3 40) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 40) =
                            SUM-YOSO(SUM-AGEMAX 40) -
                            SUM-YOSO(I3 20)
                      WHEN  4                
                            COMPUTE SUM-YOSO(I3 41) =
                            SUM-YOSO(I3 41) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 41) =
                            SUM-YOSO(SUM-AGEMAX 41) -
                            SUM-YOSO(I3 20)
                      WHEN  5                
                            COMPUTE SUM-YOSO(I3 42) =
                            SUM-YOSO(I3 42) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 42) =
                            SUM-YOSO(SUM-AGEMAX 42) -
                            SUM-YOSO(I3 20)
                      WHEN  6                
                            COMPUTE SUM-YOSO(I3 43) =
                            SUM-YOSO(I3 43) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 43) =
                            SUM-YOSO(SUM-AGEMAX 43) -
                            SUM-YOSO(I3 20)
                      WHEN  7                
                            COMPUTE SUM-YOSO(I3 44) =
                            SUM-YOSO(I3 44) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 44) =
                            SUM-YOSO(SUM-AGEMAX 44) -
                            SUM-YOSO(I3 20)
                      WHEN  8                
                            COMPUTE SUM-YOSO(I3 45) =
                            SUM-YOSO(I3 45) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 45) =
                            SUM-YOSO(SUM-AGEMAX 45) -
                            SUM-YOSO(I3 20)
                      WHEN  9                
                            COMPUTE SUM-YOSO(I3 46) =
                            SUM-YOSO(I3 46) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 46) =
                            SUM-YOSO(SUM-AGEMAX 46) -
                            SUM-YOSO(I3 20)
                      WHEN  10                
                            COMPUTE SUM-YOSO(I3 47) =
                            SUM-YOSO(I3 47) -
                            SUM-YOSO(I3 20)
                            COMPUTE SUM-YOSO(SUM-AGEMAX 47) =
                            SUM-YOSO(SUM-AGEMAX 47) -
                            SUM-YOSO(I3 20)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
                   END-EVALUATE
      * 助成金額の算出              
                   COMPUTE WRK-MONEY =
                           SUM-YOSO(I3 10) +
                           SUM-YOSO(I3 11) +
      * 20070913 消費税削除START t.tanaka
      *                     SUM-YOSO(I3 14) +
      * 20070913 消費税削除END
                           SUM-YOSO(I3 15) 
      * 20071011 減免金額修正START t.tanaka
                   MOVE    SUM-YOSO(I3 12) TO WRK-GENMEN
                   COMPUTE WRK-GENMEN = WRK-GENMEN / 10
                   COMPUTE WRK-GENMEN = WRK-GENMEN * 10
      * 20071011 減免金額修正END
                          COMPUTE SUM-YOSO(I3 13) =
                           WRK-MONEY -
      * 20071011減免金額修正START t.tanaka
      *                     SUM-YOSO(I3 12) -
                           WRK-GENMEN -
      * 20071011減免金額修正END
                           SUM-YOSO(I3 16) 
                   COMPUTE SUM-YOSO(SUM-AGEMAX 13) =
                           SUM-YOSO(SUM-AGEMAX 13) +
                           SUM-YOSO(I3 13) 
           END-PERFORM
      *
      * 新患来院率(計)
      *
           INITIALIZE                WRK-RATE
                                     WRK-RATE2
           COMPUTE  WRK-RATE =
                    SUM-YOSO(SUM-AGEMAX 6) / 
                    SUM-YOSO(SUM-AGEMAX 5)
           COMPUTE  WRK-RATE2 ROUNDED     =
                    WRK-RATE     *   100
           MOVE     WRK-RATE2    TO  WRK-RATE-Z9
           MOVE     WRK-RATE-Z9  TO  SUM-YOSO(SUM-AGEMAX 7)
      *
      * 平均点／人(計)
      *
           INITIALIZE        WRK-TEN
           COMPUTE  WRK-TEN  ROUNDED      =
                    SUM-YOSO (SUM-AGEMAX 8) / 
                    SUM-YOSO (SUM-AGEMAX 5)
           MOVE     WRK-TEN
                    TO  SUM-YOSO (SUM-AGEMAX 9)
      *
      * 過日未収金残額(計)
      *     MOVE  SUM-YOSO (SUM-AGEMAX - 1 17) 
      *              TO  SUM-YOSO (SUM-AGEMAX 17)
      *
      * 
           .
       320-ETC-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報集計サブ処理
      *****************************************************************
       320-SYUNOU-CNT-SUB-SEC      SECTION.
      *
            INITIALIZE            FLG-INVALID     
            MOVE SYU-SRYYMD   TO  D129P-SRYYMD
            MOVE SYU-PTID     TO  D129P-PTID
            READ D129PT-FILE
                 KEY      IS      D129P-KEY  
                 INVALID  KEY     MOVE  1  TO FLG-INVALID
            END-READ
      *
            IF   FLG-INVALID      =     1 
      ***        診療日別対象患者ファイルに未処理のデータは患者数を集計処理を行う。
                 INITIALIZE       FLG-INVALID     
                 WRITE D129P
                        INVALID  KEY  
                        MOVE  1  TO FLG-INVALID
                 END-WRITE
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/27) start
      * 新患件数
                 PERFORM  900-JYURRK-SELECT-SEC
                 IF       FLG-JYURRK       =   ZERO
                   AND    JYURRK-SRYYMD    =   SYU-SRYYMD 
      * 新患件数としてカウントする。
                   ADD  1  TO  SUM-YOSO(I3 6)
                   ADD  1  TO  SUM-YOSO(SUM-AGEMAX 6)
                 END-IF
                 PERFORM  900-JYURRK-CLOSE-SEC
            END-IF
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/27) end
      *
      ***      受診履歴情報により初診、再診、その他件数を取得する。
      *--- 2009/03/04 CHANGE START -------------------------------------
      *     PERFORM 900-JYURRK-READ-SEC
           INITIALIZE     FLG-JYURRK2
           PERFORM   910-JYURRK-READ-SEC
      *--- 2009/03/04 CHANGE END ---------------------------------------
      *--- 預り金の重複を修正 BY Monma (2009/02/25) start
           MOVE    JYURRK-RENNUM     TO   IDX4
      *--- 預り金の重複を修正 BY Monma (2009/02/25) end
      *--- 2009/03/04 ADD START ----------------------------------------
           PERFORM  UNTIL  FLG-JYURRK  =  1
             IF  FLG-JYURRK2  =  ZERO
               PERFORM  320-JYURRK-CNT-SUB-SEC
             END-IF
      *--- 2009/03/04 ADD END ------------------------------------------
      *    当日預り金
      *    患者別に当日預り金を診療行為情報より取得する。
      * 20040401 削除 Y.Takahashi
      *             PERFORM 911-SRY-SELECT-SEC
      *             PERFORM 320-AZUKARI-GET-SUB-SEC
      */20040401 削除
      *--- 2009/03/04 ADD START ----------------------------------------
             PERFORM   320-AZUKARI-GET-SEC
             PERFORM   910-JYURRK-FETCH-SEC
           END-PERFORM
           PERFORM   910-JYURRK-CLOSE-SEC
      *--- 2009/03/04 ADD END ------------------------------------------
      *=== 初診、再診、その他振分け修正 DEL By Yoshikawa(2007/06/27) start
      *      END-IF
      *=== 初診、再診、その他振分け修正 DEL By Yoshikawa(2007/06/27) end
      *
      *各週マスタの読込
      *   保険組合情報
          INITIALIZE                           HKNCOMBI-REC
      * 20070913 4.0対応START t.tanaka
      *    MOVE    SYU-HOSPID               TO  COMB-HOSPID
          MOVE    SYU-HOSPNUM               TO  COMB-HOSPNUM
      * 20070913 4.0対応END
          MOVE    SYU-PTID                 TO  COMB-PTID
          MOVE    SYU-HKNCOMBINUM          TO  COMB-HKNCOMBINUM
          PERFORM 900-HKNCOMBI-READ-SEC
      *   患者保険情報検索
          INITIALIZE                           PTHKNINF-REC
      * 20070913 4.0対応START t.tanaka
      *    MOVE    SYU-HOSPID               TO  PTHKN-HOSPID
          MOVE    SYU-HOSPNUM               TO  PTHKN-HOSPNUM
      * 20070913 4.0対応END
          MOVE    SYU-PTID                 TO  PTHKN-PTID
          MOVE    COMB-HKNID               TO  PTHKN-HKNID
          PERFORM 900-PTHKNINF-READ-SEC
      *  保険番号情報読み込み
          PERFORM 900-HKNNUM-READ-SEC
          IF   FLG-HKNNUM  =   1
      ***      該当レコードが存在しない場合は単価を10円に設定する。
               MOVE 10     TO  HKN-TENTNK
          END-IF
          IF   SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
           OR  SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM
      * 20080401 H20年4月改正対応START tanaka
           OR  SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM
      * 20080401 H20年4月改正対応END
      *        患者労災情報検索
               INITIALIZE           PTRSIINF-REC
      * 20070913 4.0対応START t.tanaka
      *         MOVE SYU-HOSPID  TO  PTRSI-HOSPID
               MOVE SYU-HOSPNUM  TO  PTRSI-HOSPNUM
      * 20070913 4.0対応END
               MOVE SYU-PTID    TO  PTRSI-PTID
               MOVE COMB-HKNID  TO  PTRSI-HKNID
               PERFORM 900-PTRSIINF-READ-SEC
          END-IF
      *
      *  合計保険点数初期化 
                   MOVE   ZERO             TO  SYU-TOTAL-HKNTEN
      *  診療科保険点数            
                   ADD    SYU-SSU-HKNTEN   
                          TO  SUM-YOSO(I3 22)
                   ADD    SYU-SSU-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 22)
      ** 合計点数に加算
                   ADD    SYU-SSU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  指導料保険点数            
                   ADD    SYU-SDO-HKNTEN
                          TO  SUM-YOSO (I3 23)
                   ADD    SYU-SDO-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 23)
      ** 合計点数に加算
                   ADD    SYU-SDO-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  在宅料保険点数            
                   ADD    SYU-ZTK-HKNTEN
                          TO  SUM-YOSO (I3 24)
                   ADD    SYU-ZTK-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 24)
      ** 合計点数に加算
                   ADD    SYU-ZTK-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  投薬料保険点数            
                   ADD    SYU-TYK-HKNTEN
                          TO  SUM-YOSO (I3 25)
                   ADD    SYU-TYK-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 25)
      ** 合計点数に加算
                   ADD    SYU-TYK-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  注射料保険点数            
                   ADD    SYU-CSY-HKNTEN
                          TO  SUM-YOSO (I3 26)
                   ADD    SYU-CSY-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 26)
      ** 合計点数に加算FLG-SRYACT
                   ADD    SYU-CSY-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  処置料保険点数            
                   ADD    SYU-SYC-HKNTEN
                          TO  SUM-YOSO (I3 27)
                   ADD    SYU-SYC-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 27)
      ** 合計点数に加算
                   ADD    SYU-SYC-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  手術料保険点数            
                   ADD    SYU-SJT-HKNTEN
                          TO  SUM-YOSO (I3 28)
                   ADD    SYU-SJT-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 28)
      ** 合計点数に加算
                   ADD    SYU-SJT-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      **
      **  検査料保険点数            
      *             ADD    SYU-KNS-HKNTEN
      *                    TO  SUM-YOSO (I3 29)
      *             ADD    SYU-KNS-HKNTEN   
      *                    TO  SUM-YOSO(SUM-AGEMAX 29)
      *** 合計点数に加算
      *             ADD    SYU-KNS-HKNTEN
      *                    TO  SYU-TOTAL-HKNTEN
      **
      **  X線料保険点数            
      *             ADD    SYU-GZU-HKNTEN
      *                    TO  SUM-YOSO (I3 30)
      *             ADD    SYU-GZU-HKNTEN   
      *                    TO  SUM-YOSO(SUM-AGEMAX 30)
      *** 合計点数に加算
      *             ADD    SYU-GZU-HKNTEN
      *                    TO  SYU-TOTAL-HKNTEN
      **
      **  その他保険点数            
      *             ADD    SYU-ETC-HKNTEN
      *                    TO  SUM-YOSO (I3 31)
      *             ADD    SYU-ETC-HKNTEN   
      *                    TO  SUM-YOSO(SUM-AGEMAX 31)
      *** 合計点数に加算
      *             ADD    SYU-ETC-HKNTEN
      *                    TO  SYU-TOTAL-HKNTEN
      **
      **  保険適用外金額            
      *             ADD    SYU-TOTAL-TGMONEY       
      *                    TO  SUM-YOSO (I3 32)
      *             ADD    SYU-TOTAL-TGMONEY       
      *                    TO  SUM-YOSO (SUM-AGEMAX 32)
      *             ADD    SYU-TOTAL-TGMONEY-TAX
      *                    TO  SUM-YOSO (I3 32)
      *             ADD    SYU-TOTAL-TGMONEY-TAX
      *                    TO  SUM-YOSO (SUM-AGEMAX 32)
      **
      **  自費１            
      *             ADD    SYU-JIHI-1       
      *                    TO  SUM-YOSO (I3 33)
      *             ADD    SYU-JIHI-1
      *                    TO  SUM-YOSO (SUM-AGEMAX 33)
      *             ADD    SYU-JIHI-1-TAX
      *                    TO  SUM-YOSO (I3 33)
      *             ADD    SYU-JIHI-1-TAX
      *                    TO  SUM-YOSO (SUM-AGEMAX 33)
      **
      **  自費２            
      *             ADD    SYU-JIHI-2       
      *                    TO  SUM-YOSO (I3 34)
      *             ADD    SYU-JIHI-2
      *                    TO  SUM-YOSO (SUM-AGEMAX 34)
      *             ADD    SYU-JIHI-2-TAX
      *                    TO  SUM-YOSO (I3 34)
      *             ADD    SYU-JIHI-2-TAX
      *                    TO  SUM-YOSO (SUM-AGEMAX 34)
      **
      **  自費３            
      *             ADD    SYU-JIHI-3       
      *                    TO  SUM-YOSO (I3 35)
      *             ADD    SYU-JIHI-3
      *                    TO  SUM-YOSO (SUM-AGEMAX 35)
      *             ADD    SYU-JIHI-3-TAX
      *                    TO  SUM-YOSO (I3 35)
      *             ADD    SYU-JIHI-3-TAX
      *                    TO  SUM-YOSO (SUM-AGEMAX 35)
      **
      **  自費４            
      *             ADD    SYU-JIHI-4       
      *                    TO  SUM-YOSO (I3 36)
      *             ADD    SYU-JIHI-4
      *                    TO  SUM-YOSO (SUM-AGEMAX 36)
      *             ADD    SYU-JIHI-4-TAX
      *                    TO  SUM-YOSO (I3 36)
      *             ADD    SYU-JIHI-4-TAX
      *                    TO  SUM-YOSO (SUM-AGEMAX 36)
      **
      **  自費５            
      *             ADD    SYU-JIHI-5       
      *                    TO  SUM-YOSO (I3 37)
      *             ADD    SYU-JIHI-5
      *                    TO  SUM-YOSO (SUM-AGEMAX 37)
      *             ADD    SYU-JIHI-5-TAX
      *                    TO  SUM-YOSO (I3 37)
      *             ADD    SYU-JIHI-5-TAX
      *                    TO  SUM-YOSO (SUM-AGEMAX 37)
      *-----------------------------------------------------------
      *
      *  麻酔点数            
                   ADD    SYU-MSI-HKNTEN
                          TO  SUM-YOSO (I3 29)
                   ADD    SYU-MSI-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 29)
      ** 合計点数に加算
                   ADD    SYU-MSI-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  検査料保険点数            
                   ADD    SYU-KNS-HKNTEN
                          TO  SUM-YOSO (I3 30)
                   ADD    SYU-KNS-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 30)
      ** 合計点数に加算
                   ADD    SYU-KNS-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  X線料保険点数            
                   ADD    SYU-GZU-HKNTEN
                          TO  SUM-YOSO (I3 31)
                   ADD    SYU-GZU-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 31)
      ** 合計点数に加算
                   ADD    SYU-GZU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  リハ点数            
                   ADD    SYU-ETC-HKNTEN
                          TO  SUM-YOSO (I3 32)
                   ADD    SYU-ETC-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 32)
      ** 合計点数に加算
                   ADD    SYU-ETC-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  精神科専門点数            
                   ADD    SYU-SSN-HKNTEN
                          TO  SUM-YOSO (I3 33)
                   ADD    SYU-SSN-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 33)
      ** 合計点数に加算
                   ADD    SYU-SSN-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  放射線治療点数            
                   ADD    SYU-HOU-HKNTEN
                          TO  SUM-YOSO (I3 34)
                   ADD    SYU-HOU-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 34)
      ** 合計点数に加算
                   ADD    SYU-HOU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      * 20080401 H20年4月改正対応START tanaka
      *  病理診断点数            
                   ADD    SYU-BYR-HKNTEN
                          TO  SUM-YOSO (I3 48)
                   ADD    SYU-BYR-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 48)
      ** 合計点数に加算
                   ADD    SYU-BYR-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      * 20080401 H20年4月改正対応END
      *
      *  入院保険点数            
                   ADD    SYU-NYN-HKNTEN
                          TO  SUM-YOSO (I3 35)
                   ADD    SYU-NYN-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 35)
      ** 合計点数に加算
                   ADD    SYU-NYN-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  療養担当手当点数            
                   ADD    SYU-RYO-HKNTEN
                          TO  SUM-YOSO (I3 36)
                   ADD    SYU-RYO-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 36)
      ** 合計点数に加算
                   ADD    SYU-RYO-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  保険適用外金額            
                   ADD    SYU-TOTAL-TGMONEY       
                          TO  SUM-YOSO (I3 37)
                   ADD    SYU-TOTAL-TGMONEY       
                          TO  SUM-YOSO (SUM-AGEMAX 37)
                   ADD    SYU-TOTAL-TGMONEY-TAX
                          TO  SUM-YOSO (I3 37)
                   ADD    SYU-TOTAL-TGMONEY-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 37)
      *
      *  自費１            
                   ADD    SYU-JIHI-1       
                          TO  SUM-YOSO (I3 38)
                   ADD    SYU-JIHI-1
                          TO  SUM-YOSO (SUM-AGEMAX 38)
                   ADD    SYU-JIHI-1-TAX
                          TO  SUM-YOSO (I3 38)
                   ADD    SYU-JIHI-1-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 38)
      *
      *  自費２            
                   ADD    SYU-JIHI-2       
                          TO  SUM-YOSO (I3 39)
                   ADD    SYU-JIHI-2
                          TO  SUM-YOSO (SUM-AGEMAX 39)
                   ADD    SYU-JIHI-2-TAX
                          TO  SUM-YOSO (I3 39)
                   ADD    SYU-JIHI-2-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 39)
      *
      *  自費３            
                   ADD    SYU-JIHI-3       
                          TO  SUM-YOSO (I3 40)
                   ADD    SYU-JIHI-3
                          TO  SUM-YOSO (SUM-AGEMAX 40)
                   ADD    SYU-JIHI-3-TAX
                          TO  SUM-YOSO (I3 40)
                   ADD    SYU-JIHI-3-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 40)
      *
      *  自費４            
                   ADD    SYU-JIHI-4       
                          TO  SUM-YOSO (I3 41)
                   ADD    SYU-JIHI-4
                          TO  SUM-YOSO (SUM-AGEMAX 41)
                   ADD    SYU-JIHI-4-TAX
                          TO  SUM-YOSO (I3 41)
                   ADD    SYU-JIHI-4-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 41)
      *
      *  自費５            
                   ADD    SYU-JIHI-5       
                          TO  SUM-YOSO (I3 42)
                   ADD    SYU-JIHI-5
                          TO  SUM-YOSO (SUM-AGEMAX 42)
                   ADD    SYU-JIHI-5-TAX
                          TO  SUM-YOSO (I3 42)
                   ADD    SYU-JIHI-5-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 42)
      *
      *  自費６            
                   ADD    SYU-JIHI-6       
                          TO  SUM-YOSO (I3 43)
                   ADD    SYU-JIHI-6
                          TO  SUM-YOSO (SUM-AGEMAX 43)
                   ADD    SYU-JIHI-6-TAX
                          TO  SUM-YOSO (I3 43)
                   ADD    SYU-JIHI-6-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 43)
      *
      *  自費７            
                   ADD    SYU-JIHI-7       
                          TO  SUM-YOSO (I3 44)
                   ADD    SYU-JIHI-7
                          TO  SUM-YOSO (SUM-AGEMAX 44)
                   ADD    SYU-JIHI-7-TAX
                          TO  SUM-YOSO (I3 44)
                   ADD    SYU-JIHI-7-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 44)
      *
      *  自費８            
                   ADD    SYU-JIHI-8       
                          TO  SUM-YOSO (I3 45)
                   ADD    SYU-JIHI-8
                          TO  SUM-YOSO (SUM-AGEMAX 45)
                   ADD    SYU-JIHI-8-TAX
                          TO  SUM-YOSO (I3 45)
                   ADD    SYU-JIHI-8-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 45)
      *
      *  自費９            
                   ADD    SYU-JIHI-9       
                          TO  SUM-YOSO (I3 46)
                   ADD    SYU-JIHI-9
                          TO  SUM-YOSO (SUM-AGEMAX 46)
                   ADD    SYU-JIHI-9-TAX
                          TO  SUM-YOSO (I3 46)
                   ADD    SYU-JIHI-9-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 46)
      *
      *  自費１０            
                   ADD    SYU-JIHI-10       
                          TO  SUM-YOSO (I3 47)
                   ADD    SYU-JIHI-10
                          TO  SUM-YOSO (SUM-AGEMAX 47)
                   ADD    SYU-JIHI-10-TAX
                          TO  SUM-YOSO (I3 47)
                   ADD    SYU-JIHI-10-TAX
                          TO  SUM-YOSO (SUM-AGEMAX 47)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      *
      * 保険内点数(合計保険点数)
                   ADD    SYU-TOTAL-HKNTEN
                          TO  SUM-YOSO (I3 8)
                   ADD    SYU-TOTAL-HKNTEN   
                          TO  SUM-YOSO(SUM-AGEMAX 8)
      *
      * 保険内負担金              
      *
      *            主保険分負担金額
      *
                   IF      SYU-SYUCOMPTOTAL  NOT = ZERO 
                           ADD SYU-SYUCOMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD SYU-SYUCOMPTOTAL
                               TO  SUM-YOSO(SUM-AGEMAX 10)
                   ELSE IF SYU-KOH1COMPTOTAL NOT = ZERO
                           ADD SYU-KOH1COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD SYU-KOH1COMPTOTAL
                               TO  SUM-YOSO(SUM-AGEMAX 10)
                   ELSE IF SYU-KOH2COMPTOTAL NOT = ZERO
                           ADD SYU-KOH2COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH2COMPTOTAL
                               TO  SUM-YOSO(SUM-AGEMAX 10)
                   ELSE IF SYU-KOH3COMPTOTAL NOT = ZERO
                           ADD     SYU-KOH3COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH3COMPTOTAL
                               TO  SUM-YOSO(SUM-AGEMAX 10)
                   ELSE IF SYU-KOH3COMPTOTAL NOT = ZERO
                           ADD     SYU-KOH4COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH4COMPTOTAL
                               TO  SUM-YOSO(SUM-AGEMAX 10)
                   END-IF 
                   END-IF 
                   END-IF 
                   END-IF 
                   END-IF
      *
                   IF ( SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
      * 20080401 H20年4月改正対応START tanaka
      *               OR SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM )
                     OR SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM 
                     OR SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM )
      * 20080401 H20年4月改正対応END
                    AND PTRSI-JIBAISEIKBN    NOT  =  2
      *            労災初診金額金額
                        ADD  SYU-RSISHOSHIN-MONEY  
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISHOSHIN-MONEY  
                               TO  SUM-YOSO(SUM-AGEMAX 10)
      *
      *            労災再診金額金額
                        ADD  SYU-RSISAISHIN-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISAISHIN-MONEY
                               TO  SUM-YOSO(SUM-AGEMAX 10)
      *
      *            労災指導金額金額
                        ADD  SYU-RSISDO-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISDO-MONEY
                               TO  SUM-YOSO(SUM-AGEMAX 10)
      *
      *            労災その他金額金額
                        ADD  SYU-RSIETC-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSIETC-MONEY
                               TO  SUM-YOSO(SUM-AGEMAX 10)
                   END-IF
      *
      * 保険外金額
                   COMPUTE WRK-JIHI    =
                           SYU-TOTAL-TGMONEY  
                       +   SYU-TOTAL-TGMONEY-TAX  
                       +   SYU-JIHI-TOTAL
                       +   SYU-JIHI-TOTAL-TAX  
                   ADD    WRK-JIHI
                          TO  SUM-YOSO(I3 11)
                   ADD    WRK-JIHI
                          TO  SUM-YOSO(SUM-AGEMAX 11)
      * 20070913 減免金額追加START t.tanaka
      * 減免金額
                   ADD SYU-DISCOUNT-MONEY
                          TO  SUM-YOSO(I3 12)
                   ADD SYU-DISCOUNT-MONEY
                          TO  SUM-YOSO(SUM-AGEMAX 12)
      * 20070913 減免金額追加END

      ****2005/12/30  @@@@-BRAND START
      * 消費税              
      *             ADD    SYU-JIHI-TAX
      *                    TO  SUM-YOSO(I3 14)
      *             ADD    SYU-JIHI-TAX
      *                    TO  SUM-YOSO(SUM-AGEMAX 14)
      * 消費税
                   ADD    SYU-SKYMONEY-TAX-SAI
                          TO  SUM-YOSO(I3 14)
                   ADD    SYU-SKYMONEY-TAX-SAI
                          TO  SUM-YOSO(SUM-AGEMAX 14)
      ****2005/12/30  @@@@-BRAND END
      * 調整金              
                   ADD    SYU-CHOSEI
                          TO  SUM-YOSO(I3 15)
                   ADD    SYU-CHOSEI
                          TO  SUM-YOSO(SUM-AGEMAX 15)
      * 窓口請求金額              
                   ADD    SYU-SKYMONEY
                          TO  SUM-YOSO(I3 16)
                   ADD    SYU-SKYMONEY
                          TO  SUM-YOSO(SUM-AGEMAX 16)
      * 入金額(当日の診療に対する入金額)   
      * 20071024 入金額の重複を修正START T.Tanaka
      * 20080708 収納明細を更新した場合に、入金額が表示されない不具合の修正START
                   MOVE   ZERO  TO  FLG-SYUMEI
      * 20080708 収納明細を更新した場合に、入金額が表示されない不具合の修正END
                   PERFORM  320-SYUMEI-SELECT-SEC
      * 20080708 収納明細を更新した場合に、入金額が表示されない不具合の修正START
                   PERFORM  UNTIL  FLG-SYUMEI  = 1
      * 20080708 収納明細を更新した場合に、入金額が表示されない不具合の修正END
                      IF     SMEI-NYUHEN-YMD <= D100LNK-YMD2
                       AND  SMEI-NYUHEN-YMD >= D100LNK-YMD1
      * 20080708 収納明細を更新した場合に、入金額が表示されない不具合の修正START
      *                    ADD    SYU-NYUKIN-TOTAL
                          ADD    SMEI-NYUHEN-MONEY
                                 TO  SUM-YOSO(I3 18)
      *                    ADD    SYU-NYUKIN-TOTAL
                          ADD    SMEI-NYUHEN-MONEY
      * 20080708 収納明細を更新した場合に、入金額が表示されない不具合の修正END
                                 TO  SUM-YOSO(SUM-AGEMAX 18)
                      END-IF
      * 20080708 収納明細を更新した場合に、入金額が表示されない不具合の修正START
                      PERFORM  320-SYUMEI-FETCH-SEC
                   END-PERFORM 
      * 20080708 収納明細を更新した場合に、入金額が表示されない不具合の修正END
                   PERFORM  320-SYUMEI-CLOSE-SEC
      * 20071024 入金額の重複を修正END
      * 未収金残額              
                   COMPUTE     WRK-MONEY        =
                               SYU-SKYMONEY     -    SYU-NYUKIN-TOTAL
                   ADD    WRK-MONEY
                          TO  SUM-YOSO(I3 19)
                   ADD    WRK-MONEY
                          TO  SUM-YOSO(SUM-AGEMAX 19)
      *
           .
       320-SYUNOU-CNT-SUB-EXT.
           EXIT.
      *
      ****2005/12/30  @@@@-BRAND START
      *****************************************************************
      *    入金集計処理
      *****************************************************************
       320-SYUMEI-CNT-SUB-SEC      SECTION.
           MOVE  ZERO              TO  WRK-SMEI-NYUHEN-MONEY
           PERFORM  320-SYUMEI-SELECT-SEC
           PERFORM   UNTIL  FLG-SYUMEI = 1
             IF     SMEI-NYUHEN-YMD <= D100LNK-YMD2
               AND  SMEI-NYUHEN-YMD >= D100LNK-YMD1
               COMPUTE  WRK-SMEI-NYUHEN-MONEY = 
                        WRK-SMEI-NYUHEN-MONEY + SMEI-NYUHEN-MONEY
             ELSE
               COMPUTE  WRK-SMEI-NYUHEN-MONEY =
                        WRK-SMEI-NYUHEN-MONEY + 0
             END-IF
             PERFORM  320-SYUMEI-FETCH-SEC
           END-PERFORM.
      *=== sarge対応 ADD By Takashima(2006/09/16) start
           PERFORM  320-SYUMEI-CLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/16) end
           ADD  WRK-SMEI-NYUHEN-MONEY  TO  SUM-YOSO(I3 18)
           ADD  WRK-SMEI-NYUHEN-MONEY  TO  SUM-YOSO(SUM-AGEMAX 18)
           .
       320-SYUMEI-CNT-SUB-EXT.
           EXIT.
      *****************************************************************
      *   入金  SELECT
      *****************************************************************
       320-SYUMEI-SELECT-SEC           SECTION.
      * 20070913 4.0対応START t.tanaka
      *     MOVE    SYU-HOSPID          TO  SMEI-HOSPID
           MOVE    SYU-HOSPNUM          TO  SMEI-HOSPNUM
      * 20070913 4.0対応END
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYUMEI-KEY7    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"             TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                        MCPAREA
                                        MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
                PERFORM  320-SYUMEI-FETCH-SEC
           ELSE
                MOVE  1                 TO  FLG-SYUMEI
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *          PERFORM  320-SYUMEI-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       320-SYUMEI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *   入金  FETCH
      *****************************************************************
       320-SYUMEI-FETCH-SEC             SECTION.
            MOVE    "DBFETCH"           TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYUMEI-KEY7    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"             TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                        MCPAREA
                                        MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
            IF      MCP-RC              =   ZERO
                MOVE    ZERO            TO  FLG-SYUMEI
                MOVE    MCPDATA-REC     TO  SYUMEI-REC
            ELSE
                MOVE    1               TO  FLG-SYUMEI
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *          PERFORM  320-SYUMEI-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
            END-IF
            .
       320-SYUMEI-FETCH-EXT.
            EXIT.
      *****************************************************************
      *  入金  CLOSE
      *****************************************************************
       320-SYUMEI-CLOSE-SEC            SECTION.
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYUMEI-KEY7    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syumei"             TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                        MCPAREA
                                        MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
            .
       320-SYUMEI-SELECT-EXT.
            EXIT.
      ****2005/12/30  @@@@-BRAND END
      *****************************************************************
      *    受診履歴情報集計サブ処理
      *****************************************************************
       320-JYURRK-CNT-SUB-SEC      SECTION.
      *
           IF    FLG-JYURRK       = ZERO
            AND  JYURRK-NYUGAIKBN = "2"
                 MOVE JYURRK-SRYYMD(7:2)  TO  IDX7G
      * 20070913 4.0対応START t.tanaka
      *           MOVE JYURRK-HOSPID       TO  ACCT-HOSPID  
                 MOVE JYURRK-HOSPNUM       TO  ACCT-HOSPNUM  
      * 20070913 4.0対応END
                 MOVE JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
                 MOVE JYURRK-PTID         TO  ACCT-PTID
                 MOVE JYURRK-SRYKA        TO  ACCT-SRYKA
                 MOVE JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
                 MOVE "11"                TO  ACCT-SRYKBN
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/27) start
      *           PERFORM 900-SRYACCT-READ-SEC
      *  診療会計情報SELECT(KEY=3)
                 MOVE  SRYACCT-REC      TO  MCPDATA-REC
                 MOVE  "DBSELECT"       TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *           MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
                 MOVE    "tbl_sryacct"             TO  MCP-TABLE
                 MOVE    "key3"                  TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                            MCPAREA
                                            MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
                 IF      MCP-RC         =   ZERO
                     MOVE  "DBFETCH"    TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *           MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
                     MOVE    "tbl_sryacct"             TO  MCP-TABLE
                     MOVE    "key3"                  TO  MCP-PATHNAME
                     CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                            MCPAREA
                                            MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
                     IF      MCP-RC         =   ZERO
                         MOVE  MCPDATA-REC  TO  SRYACCT-REC
                         MOVE  ZERO         TO  FLG-SRYACCT
                     ELSE
                         MOVE  1            TO  FLG-SRYACCT
                         INITIALIZE             SRYACCT-REC
                     END-IF
                 ELSE
                     MOVE  1            TO  FLG-SRYACCT
                     INITIALIZE             SRYACCT-REC
                 END-IF
      *  初診データ探索処理
                 MOVE    ZERO    TO    FLG-INPUT
                 PERFORM   UNTIL   FLG-SRYACCT  =  1
                    IF  ACCT-DAY(1 IDX7)   >   ZERO
      *  初診データが見つかったら入力フラグをオン
                       MOVE  1   TO   FLG-INPUT
                    END-IF
      *  診療会計情報FETCH
                    MOVE  SRYACCT-REC     TO  MCPDATA-REC
                    MOVE  "DBFETCH"       TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *           MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
                    MOVE    "tbl_sryacct"             TO  MCP-TABLE
                    MOVE    "key3"                  TO  MCP-PATHNAME
                    CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                            MCPAREA
                                            MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
                    IF     MCP-RC     =   ZERO
                       MOVE   MCPDATA-REC   TO  SRYACCT-REC
                       MOVE   ZERO          TO  FLG-SRYACCT
                    ELSE
                       MOVE   1             TO  FLG-SRYACCT
                       INITIALIZE               SRYACCT-REC
                    END-IF
                 END-PERFORM
      *  診療会計情報CLOSE
                 MOVE  "DBCLOSECURSOR"  TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *           MOVE  PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
                 MOVE    "tbl_sryacct"             TO  MCP-TABLE
                 MOVE    "key3"                  TO  MCP-PATHNAME
                 CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                            MCPAREA
                                            MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *  入力フラグがオンの場合、診療会計終了フラグを切る
                 IF   FLG-INPUT       =   1
                    MOVE   ZERO      TO  FLG-SRYACCT
                 END-IF
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/27) end
                 IF      FLG-SRYACCT      =   ZERO
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/27) start
      *             AND   ACCT-DAY(1 IDX7) >   ZERO
                   AND   JYURRK-RENNUM       =  1
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/27) end
      *  初診の件数としてカウントする。
                         ADD  1  TO  SUM-YOSO(I3 2)
                         ADD  1  TO  SUM-YOSO(SUM-AGEMAX 2)
                         ADD  1  TO  SUM-YOSO(I3 5)
                         ADD  1  TO  SUM-YOSO(SUM-AGEMAX 5)
      *--- 2009/03/04 ADD START ----------------------------------------
                         MOVE  1 TO  FLG-JYURRK2
      *--- 2009/03/04 ADD END ------------------------------------------
                 ELSE
      ****2006/03/27 @@@@-BRAND  START
      *--- 2009/03/04 ADD + DELETE START -------------------------------
      *                MOVE   2                   TO   JYURRK-RENNUM
                      COMPUTE   JYURRK-EDANUM   =  JYURRK-EDANUM  +  1
                      MOVE   JYURRK-REC          TO   MCPDATA-REC
      *--- 2009/03/04 ADD + DELETE END ---------------------------------
                      MOVE  "DBSELECT"           TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *                MOVE PATH-TBL-JYURRK-KEY5 TO MCP-PATH
      *     CALL    "MCPSUB"             USING
                      MOVE    "tbl_jyurrk"             TO  MCP-TABLE
      *--- 2009/03/04 CHANGE START -------------------------------------
      *                MOVE   "key5"             TO   MCP-PATHNAME
                      MOVE   "key22"             TO   MCP-PATHNAME
      *--- 2009/03/04 CHANGE END ---------------------------------------
                      CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
                      IF   MCP-RC    =   ZERO
                          MOVE  ZERO            TO  FLG-JYURRK2
      *--- 2009/03/04 ADD + DELETE START -------------------------------
      *                    MOVE   JYURRK-ZAINUM1    TO   ACCT-ZAINUM
                          MOVE   "DBFETCH"         TO   MCP-FUNC
                          MOVE   "tbl_jyurrk"      TO   MCP-TABLE
                          MOVE   "key22"           TO   MCP-PATHNAME
                          CALL   "ORCDBMAIN"       USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                          IF   MCP-RC  =  ZERO
                            MOVE   ZERO            TO   FLG-JYURRK2
                          ELSE
                            MOVE   1               TO   FLG-JYURRK2
                          END-IF
      *--- 2009/03/04 ADD + DELETE END ---------------------------------
                      ELSE
                           MOVE    1             TO  FLG-JYURRK2
                      END-IF
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *                MOVE  "DBCLOSE"            TO  MCP-FUNC
                      MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *                MOVE PATH-TBL-JYURRK-KEY5 TO MCP-PATH
      *     CALL    "MCPSUB"             USING
                      MOVE    "tbl_jyurrk"             TO  MCP-TABLE
      *--- 2009/03/04 CHANGE START -------------------------------------
      *                MOVE   "key5"             TO   MCP-PATHNAME
                      MOVE   "key22"             TO   MCP-PATHNAME
      *--- 2009/03/04 CHANGE END ---------------------------------------
                      CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      ****2006/03/27 @@@@-BRAND  END
      *--- 2009/03/04 ADD START ----------------------------------------
                      PERFORM  VARYING  IDX11  FROM 1  BY 1
                               UNTIL   IDX11  >  15
                        MOVE   JYURRK-ZAINUM(IDX11)    TO   ACCT-ZAINUM
      *--- 2009/03/04 ADD END ------------------------------------------
      * 20070913 4.0対応START t.tanaka
      *                MOVE JYURRK-HOSPID TO  ACCT-HOSPID  
                        MOVE   JYURRK-HOSPNUM      TO   ACCT-HOSPNUM
      * 20070913 4.0対応END
                        MOVE   JYURRK-NYUGAIKBN    TO   ACCT-NYUGAIKBN
                        MOVE   JYURRK-PTID         TO   ACCT-PTID
                        MOVE   JYURRK-SRYKA        TO   ACCT-SRYKA
                        MOVE   JYURRK-SRYYMD(1:6)  TO   ACCT-SRYYM
                        MOVE   "12"                TO   ACCT-SRYKBN
      ****2006/03/27 @@@@-BRAND  START
      ****             PERFORM 900-SRYACCT-READ-SEC
                        MOVE    SRYACCT-REC        TO   MCPDATA-REC
                        MOVE    "DBSELECT"         TO   MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *                MOVE  PATH-TBL-SRYACCT-KEY5  TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
                        MOVE    "tbl_sryacct"      TO   MCP-TABLE
                        MOVE    "key5"             TO   MCP-PATHNAME
                        CALL    "ORCDBMAIN"        USING
      * 20070913 4.0 対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
                        IF    MCP-RC    =   ZERO
                          MOVE    "DBFETCH"       TO   MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *                MOVE  PATH-TBL-SRYACCT-KEY5  TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
                          MOVE    "tbl_sryacct"   TO   MCP-TABLE
                          MOVE    "key5"          TO   MCP-PATHNAME
                          CALL    "ORCDBMAIN"       USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
                          IF    MCP-RC   =   ZERO
                            MOVE    MCPDATA-REC  TO   SRYACCT-REC
                            MOVE    ZERO         TO   FLG-SRYACCT
                          ELSE
                            MOVE    1            TO   FLG-SRYACCT
                            INITIALIZE                SRYACCT-REC
                          END-IF
                        ELSE
                          MOVE    1              TO   FLG-SRYACCT
                          INITIALIZE                  SRYACCT-REC
                        END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *                MOVE    "DBCLOSE"        TO  MCP-FUNC
                        MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *                MOVE  PATH-TBL-SRYACCT-KEY5  TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
                        MOVE    "tbl_sryacct"    TO  MCP-TABLE
                        MOVE    "key5"           TO  MCP-PATHNAME
                        CALL    "ORCDBMAIN"      USING
      * 20070913 4.0対応END
                                                 MCPAREA
                                                 MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                                 SPA-AREA
      * 20070913 4.0対応END
      ****2006/03/27 @@@@-BRAND  END
      *--- 2009/03/04 ADD START ----------------------------------------
                        IF  IDX11  <  15
                          COMPUTE   IDX10   =   IDX11   +   1
                          IF   JYURRK-ZAINUM(IDX10)  =  ZERO
                            MOVE    16               TO   IDX11
                          END-IF
                        END-IF
                        IF   ACCT-SRYKBN   =  "12"
                          MOVE    16               TO   IDX11
                        END-IF
                      END-PERFORM
      *--- 2009/03/04 ADD END ------------------------------------------
      *
      ***  20050325  複数診療科への対応  MOTO
                      INITIALIZE               SRYACT-REC
      * 20070913 4.0対応START t.tanaka
      *                MOVE    ACCT-HOSPID     TO  SRY-HOSPID
                      MOVE    ACCT-HOSPNUM     TO  SRY-HOSPNUM
      * 20070913 4.0対応END
                      MOVE    ACCT-NYUGAIKBN  TO  SRY-NYUGAIKBN
                      MOVE    ACCT-PTID       TO  SRY-PTID
                      MOVE    ACCT-SRYKA      TO  SRY-SRYKA
                      MOVE    ACCT-SRYYM      TO  SRY-SRYYM
                      MOVE    ACCT-ZAINUM     TO  SRY-ZAINUM
                      PERFORM 900-SRYACT-SELECT-SEC
                      PERFORM 900-SRYACT-FETCH-SEC
                      PERFORM 900-SRYACT-CLOSE-SEC
      ***  20050325  複数診療科への対応  MOTO
      *
                      IF  FLG-SRYACCT          =   ZERO
                      AND ACCT-DAY(1 IDX7)     >   ZERO
      ***  20050325  複数診療科への対応  MOTO
                      AND   ( SRY-SRYCD (1)(1:3) = 
                             "112" OR "099" OR "101" )
      ***  20050325  複数診療科への対応  MOTO
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/27) start
                      AND   JYURRK-SRYKBN (1) >  "00"
      *=== 初診、再診、その他振分け修正 ADD By Yoshikawa(2007/06/27) end
      *
      *  再診の件数としてカウントする。
                         ADD  1  TO  SUM-YOSO(I3 3)
                         ADD  1  TO  SUM-YOSO(SUM-AGEMAX 3)
                         ADD  1  TO  SUM-YOSO(I3 5)
                         ADD  1  TO  SUM-YOSO(SUM-AGEMAX 5)
      *--- 2009/03/04 ADD START ----------------------------------------
                         MOVE   1   TO  FLG-JYURRK2
      *--- 2009/03/04 ADD END ------------------------------------------
                      ELSE
      *--- 2009/03/04 ADD START ----------------------------------------
                        IF  FLG-JYURRK2   =  1
      *--- 2009/03/04 ADD END ------------------------------------------
      *  その他件数としてカウントする。
                          ADD  1  TO  SUM-YOSO(I3 4)
                          ADD  1  TO  SUM-YOSO(SUM-AGEMAX 4)
                          ADD  1  TO  SUM-YOSO(I3 5)
                          ADD  1  TO  SUM-YOSO(SUM-AGEMAX 5)
      *--- 2009/03/04 ADD START ----------------------------------------
                        END-IF
      *--- 2009/03/04 ADD END ------------------------------------------
                      END-IF
                 END-IF
           END-IF
      *
      *=== 初診、再診、その他振分け修正 DEL By Yoshikawa(2007/06/27) start
      ** 新患件数
      *     PERFORM  900-JYURRK-SELECT-SEC 
      *     IF       FLG-JYURRK       =   ZERO
      *       AND    JYURRK-SRYYMD    =   SYU-SRYYMD 
      ** 新患件数としてカウントする。
      *              ADD  1  TO  SUM-YOSO(I3 6)
      *              ADD  1  TO  SUM-YOSO(SUM-AGEMAX 6)
      *     END-IF 
      *     PERFORM  900-JYURRK-CLOSE-SEC 
      *=== 初診、再診、その他振分け修正 DEL By Yoshikawa(2007/06/27) end
      *
           .           
       320-JYURRK-CNT-SUB-EXT.
           EXIT.
      *****************************************************************
      *    預り金対象者抽出処理
      *****************************************************************
       320-AZUKARI-GET-SEC      SECTION.
      *
           INITIALIZE                      SRYACT-REC
      * 20070913 4.0対応START t.tanaka
      *     MOVE   SYU-HOSPID           TO  SRY-HOSPID  
           MOVE   SYU-HOSPNUM           TO  SRY-HOSPNUM  
      * 20070913 4.0対応END
           MOVE   SYU-NYUGAIKBN        TO  SRY-NYUGAIKBN 
           MOVE   SYU-PTID             TO  SRY-PTID
           MOVE   WRK-INI-AZUKARICD    TO  SRY-SRYCD(1)
                                           SRY-SRYCD(2)
                                           SRY-SRYCD(3)
                                           SRY-SRYCD(4)
                                           SRY-SRYCD(5)
           MOVE   I3                   TO  WRK-ACCT-IN
      *    集計日範囲
           MOVE   SYU-SRYYMD(1:6)      TO  SRY-SRYYM
           MOVE   SYU-SRYYMD(7:2)      TO  WRK-ACCT-SDAY
           MOVE   SYU-SRYYMD(7:2)      TO  WRK-ACCT-EDAY
      *    
           PERFORM  911-SRY-SELECT-SEC
           PERFORM  UNTIL      FLG-SRYACT  = 1
               IF    FLG-SRYACT     =   ZERO
                AND  SRY-NYUGAIKBN  =   "2"
                AND (SUM-SRYKACD    =   SRY-SRYKA
                 OR  SUM-SRYKACD    =   "99")
                     PERFORM VARYING IDX3  FROM  1 BY 1
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      *                       UNTIL   IDX3  >  5
      *--- 預り金の重複を修正 BY Monma (2009/02/25) start
      *                       UNTIL   IDX3  >  10
                             UNTIL   IDX3  >  5
      *--- 預り金の重複を修正 BY Monma (2009/02/25) end
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
                        IF   SRY-SRYCD(IDX3)    = WRK-INI-AZUKARICD
                         AND SRY-JIHIMONEY(IDX3) > ZERO
                             MOVE SRY-JIHIMONEY(IDX3)  
                                                TO  WRK-JIHIMONEY
                             INITIALIZE             SRYACCT-REC
      * 20070913 4.0対応START t.tanaka
      *                       MOVE SRY-HOSPID    TO  ACCT-HOSPID  
                             MOVE SRY-HOSPNUM    TO  ACCT-HOSPNUM  
      * 20070913 4.0対応END
                             MOVE SRY-NYUGAIKBN TO  ACCT-NYUGAIKBN
                             MOVE SRY-PTID      TO  ACCT-PTID
                             MOVE SRY-SRYKA     TO  ACCT-SRYKA
                             MOVE SRY-SRYYM     TO  ACCT-SRYYM
                             MOVE SRY-SRYKBN    TO  ACCT-SRYKBN
                             MOVE SRY-ZAINUM    TO  ACCT-ZAINUM
                             PERFORM 900-SRYACCT-SELECT-SEC
                             PERFORM 900-SRYACCT-FETCH-SEC
                             PERFORM  UNTIL FLG-SRYACCT    =  1
                                 PERFORM 320-AZUKARI-GET-SUB-SEC
                                 PERFORM 900-SRYACCT-FETCH-SEC
                             END-PERFORM
      *=== sarge対応 ADD By Takashima(2006/09/16) start
                             PERFORM 900-SRYACCT-CLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/16) end
                        END-IF
                    END-PERFORM           
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *              PERFORM 900-SRYACCT-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
               END-IF
               PERFORM 911-SRY-FETCH-SEC
           END-PERFORM           
           PERFORM 911-SRY-CLOSE-SEC
      *
           .
       320-AZUKARI-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    預り金加算処理
      *****************************************************************
       320-AZUKARI-GET-SUB-SEC      SECTION.
      *
      *--- 預り金の重複を修正 BY Monma (2009/02/25) start
      *  PERFORM VARYING  IDX4   FROM  WRK-ACCT-SDAY  BY 1
      *             UNTIL IDX4    >    WRK-ACCT-EDAY
      *      IF  ACCT-DAY(1 IDX4) >    ZERO
      *          PERFORM VARYING  IDX5  FROM  2  BY 4
      *             UNTIL  IDX5   >    4
      *               IF  ACCT-DAY(IDX5 IDX4) > 0
      *                   INITIALIZE          JYURRK-REC
      ** 20070913 4.0対応START t.tanaka
      **                   MOVE ACCT-HOSPID     TO JYURRK-HOSPID
      *                   MOVE ACCT-HOSPNUM     TO JYURRK-HOSPNUM
      ** 20070913 4.0対応END
      *                   MOVE ACCT-PTID       TO JYURRK-PTID
      *                   MOVE ACCT-NYUGAIKBN  TO JYURRK-NYUGAIKBN
      *                   MOVE ACCT-SRYKA      TO JYURRK-SRYKA
      *                   MOVE ACCT-SRYYM      TO JYURRK-SRYYMD(1:6)
      *                   MOVE IDX4(3:2)       TO JYURRK-SRYYMD(7:2)
      *                   MOVE IDX5            TO IDX6
      *                   ADD      -1          TO IDX6
      *                   MOVE IDX6            TO JYURRK-RENNUM
      *                   MOVE ACCT-HKNCOMBI   TO JYURRK-HKNCOMBINUM
      *                   PERFORM 900-JYURRK-DRCD-READ-SEC
      *                   IF  ( WRK-SEL-DRCD     = SPACE
      *                    OR ( WRK-SEL-DRCD NOT = SPACE
      *                     AND WRK-SEL-DRCD     = JYURRK-DRCD(2:4)))
      *                    AND  FLG-JYURRK       = ZERO
      *                         COMPUTE WRK-AZUKARI =
      *                           ACCT-DAY(IDX5 IDX4) * WRK-JIHIMONEY
      *                         ADD  WRK-AZUKARI
      *                          TO  SUM-YOSO(WRK-ACCT-IN 20)
      *                         ADD  WRK-AZUKARI
      *                          TO  SUM-YOSO(SUM-AGEMAX 20)
      *                   END-IF
      *               END-IF
      *          END-PERFORM
      *      END-IF
      *  END-PERFORM
           IF  ( D100LNK-DRCD  =  SPACE
               OR ( D100LNK-DRCD NOT =  SPACE
               AND  D100LNK-DRCD  =  JYURRK-DRCD))
               AND  FLG-JYURRK  =  ZERO
             PERFORM  VARYING  IDX6  FROM 1  BY 1
                      UNTIL  IDX6  >  15
               IF  JYURRK-ZAINUM(IDX6)   =   ACCT-ZAINUM
                 COMPUTE  IDX5   =   IDX4   +   1
                 COMPUTE  WRK-AZUKARI  =  ACCT-DAY(IDX5 WRK-ACCT-SDAY)
                                       *  WRK-JIHIMONEY
                 ADD   WRK-AZUKARI   TO   SUM-YOSO(WRK-ACCT-IN 20)
                 ADD   WRK-AZUKARI   TO   SUM-YOSO(SUM-AGEMAX 20)
               END-IF
             END-PERFORM
           END-IF
      *--- 預り金の重複を修正 BY Monma (2009/02/25) end
      * 20040401 削除 Y.Takahashi
      *     PERFORM  UNTIL      FLG-SRYACT  = 1
      *
      *   集計対象のデータであるかをチェックする。 
      * 20040318  変更
      ***      IF    FLG-SRYACT     =   ZERO
      ***       AND  SRY-NYUGAIKBN  =   "2"
      ***       AND  SRY-SRYKA      =   SUM-SRYKACD
      *      IF    FLG-SRYACT     =   ZERO
      *       AND  SRY-NYUGAIKBN  =   "2"
      *       AND (SUM-SRYKACD    =   SRY-SRYKA
      *        OR  SUM-SRYKACD    =   "99")
      * 20040318  変更
      *         MOVE SRY-HOSPID       TO  ACCT-HOSPID  
      *         MOVE SRY-NYUGAIKBN    TO  ACCT-NYUGAIKBN
      *         MOVE SRY-PTID         TO  ACCT-PTID
      *         MOVE SRY-SRYKA        TO  ACCT-SRYKA
      *         MOVE SRY-SRYYM        TO  ACCT-SRYYM
      *         MOVE SRY-SRYKBN       TO  ACCT-SRYKBN
      *         MOVE SRY-ZAINUM       TO  ACCT-ZAINUM
      *         PERFORM  900-SRYACCT-READ2-SEC
      *         MOVE WRK-ACCT-SDAY    TO  IDX7
      *
      *         IF   FLG-SRYACCT               =  ZERO
      *          AND ACCT-DAY(1 IDX7)  >  ZERO
      *           PERFORM VARYING   IDX3        FROM 1  BY  1  
      *                 UNTIL     IDX3        >    5
      *            IF   SRY-SRYCD(IDX3)        =    WRK-INI-AZUKARICD
      *             AND SRY-JIHIMONEY(IDX3)    >    ZERO
      **   当日預り金
      *                ADD  SRY-JIHIMONEY(IDX3)
      *                     TO  SUM-YOSO(I3 20)
      *                ADD  SRY-JIHIMONEY(IDX3)
      *                     TO  SUM-YOSO(SUM-AGEMAX 20)
      *            END-IF
      *           END-PERFORM 
      *         END-IF
      *      END-IF
      *      PERFORM 911-SRY-FETCH-SEC
      *     END-PERFORM           
      *     PERFORM 911-SRY-CLOSE-SEC
      */20040401 削除
      *
           .
       320-AZUKARI-GET-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢インデックス判別処理
      *****************************************************************
       320-AGE-GROUPING-SEC      SECTION.
      *
      * 年齢作成
      *       COMPUTE  WRK-AGE-BIRTHDAY  =
      *                WRK-AGE-SRYYMD    -  WRK-AGE-BIRTHDAY
      *       MOVE     WRK-AGE-BIRTHDAY  TO  WRK-BIRTHDAY
      *
             COMPUTE  WRK-AGE  =
                      WRK-AGE-SRYY  -  WRK-AGE-BIRTHY
             IF       WRK-AGE-SRYM  <  WRK-AGE-BIRTHM
                 ADD  -1    TO   WRK-AGE
             END-IF
      *
             MOVE  1             TO  I3
             MOVE  SUM-AGEMAX    TO  I4
             PERFORM  UNTIL  I3  >   I4
      *                IF   SUM-AGECOLMAX(I3) >= WRK-BIRTHY
                      IF   SUM-AGECOLMAX(I3) >= WRK-AGE
                           MOVE  ZERO   TO I4
                      ELSE
                           ADD   1      TO I3
                      END-IF
             END-PERFORM
      *
           .
       320-AGE-GROUPING-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    SYU-HOSPID        TO  COMB-HOSPID
           MOVE    SYU-HOSPNUM        TO  COMB-HOSPNUM
      * 20070913 4.0対応END
           MOVE    SYU-PTID          TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *    患者保険情報読み込み       
               INITIALIZE                    PTHKNINF-REC
      * 20070913 4.0対応START t.tanaka
      *         MOVE    SYU-HOSPID        TO  PTHKN-HOSPID
               MOVE    SYU-HOSPNUM        TO  PTHKN-HOSPNUM
      * 20070913 4.0対応END
               MOVE    SYU-PTID          TO  PTHKN-PTID
               MOVE    COMB-HKNID        TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
           END-IF    
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3202-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *    患者保険情報読み込み       
            INITIALIZE                    PTHKNINF-REC
      * 20070913 4.0対応START t.tanaka
      *      MOVE    PTNYUINRRK-HOSPID TO  PTHKN-HOSPID
            MOVE    PTNYUINRRK-HOSPNUM TO  PTHKN-HOSPNUM
      * 20070913 4.0対応END
            MOVE    PTNYUINRRK-PTID   TO  PTHKN-PTID
            MOVE    COMB-HKNID        TO  PTHKN-HKNID
            PERFORM 900-PTHKNINF-READ-SEC
           .
       3202-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集サブ処理                                           *
      *****************************************************************
      * 
       330-TERM-HEN-SEC         SECTION.
      *
            INITIALIZE                   D129
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
            INITIALIZE                   D1291
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      * 帳票サブタイトル
            MOVE   "(外来・年齢別)"      TO  D129-SUBTITLE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
            MOVE   "(外来・年齢別)"      TO  D1291-SUBTITLE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      * 印字対象診療年月
            MOVE   WRK-SRYYMD1       TO  D129-FRYMD
            MOVE   WRK-SRYYMD2       TO  D129-TOYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
            MOVE   WRK-SRYYMD1       TO  D1291-FRYMD
            MOVE   WRK-SRYYMD2       TO  D1291-TOYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      * 診療科名称
            MOVE   SUM-SRYKANAME     TO  D129-SRYKA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
            MOVE   SUM-SRYKANAME     TO  D1291-SRYKA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      * 医師名称
            MOVE   SUM-DRNAME        TO  D129-DRNAME
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
            MOVE   SUM-DRNAME        TO  D1291-DRNAME
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      * 20040330 削除 Y.Takahashi
      * 実日数
      *      MOVE   SUM-JITUMSG       TO  D129-JITUMSG
      */20040330 削除 
      *
      * 帳票出力年月日
            MOVE    WRK-SYSYMDWH     TO  D129-SYSYMD  
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
            MOVE    WRK-SYSYMDWH     TO  D1291-SYSYMD  
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      * 
      * 外来の自費タイトル設定
            PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      *               UNTIL    IDX2   >    5 
      *         MOVE  WRK-TJIHI(IDX2) TO  D129-TJIHI(IDX2)
                     UNTIL    IDX2   >    10 
               MOVE  WRK-TJIHI(IDX2) TO  D1291-TJIHI(IDX2)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
            END-PERFORM
      *
            MOVE     1        TO           I3
            IF       SUM-AGEMAX      >     13
                     MOVE     14           TO   I5
            ELSE
                     MOVE     SUM-AGEMAX   TO   I5
            END-IF         
      *
      * テーブル設定
            PERFORM    UNTIL    I3     >     SUM-AGEMAX
              PERFORM  VARYING  I6     FROM  1  BY  1
                       UNTIL    I6     >     I5
                          OR    I3     >     SUM-AGEMAX
               IF    I3       =        SUM-AGEMAX
                     MOVE     "計"
                              TO  D129-TAGE(14)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
                     MOVE     "計"
                              TO  D1291-TAGE(14)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
               ELSE 
                     MOVE     SUM-AGENAME(I3)
                              TO  D129-TAGE(I6)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
                     MOVE     SUM-AGENAME(I3)
                              TO  D1291-TAGE(I6)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
               END-IF
      * 20040407 修正 Y.Takahashi
      *         PERFORM VARYING  I4     FROM  1  BY  1
      *                 UNTIL    I4     >     39
               PERFORM VARYING  I4     FROM  2  BY  1
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      *                 UNTIL    I4     >     39
      * 20080401 H20年4月改正対応START tanaka
      *                 UNTIL    I4     >     47
                       UNTIL    I4     >     48
      * 20080401 H20年4月改正対応END
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
                       COMPUTE I7  =   I4   -  1
      */20040407 修正
                       IF     I3       =     SUM-AGEMAX
                         IF   I4       =   1
                          OR  I4       =   21
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      *                    OR  I4       =   38
      *                    OR  I4       =   39
      ** 20040626 追加 Y.Takahashi 保険外自費が無い場合は数値は空白
      *                    OR ( I4               = 33 
      *                     AND WRK-TJIHI-FLG(1) = ZERO )
      *                    OR ( I4               = 34 
      *                     AND WRK-TJIHI-FLG(2) = ZERO )
      *                    OR ( I4               = 35 
      *                     AND WRK-TJIHI-FLG(3) = ZERO )
      *                    OR ( I4               = 36 
      *                     AND WRK-TJIHI-FLG(4) = ZERO )
      *                    OR ( I4               = 37 
      *                     AND WRK-TJIHI-FLG(5) = ZERO )
      **/20040626 追加
      ****                     未使用行は空白を印字する。
      ** 20040407 修正 Y.Takahashi
      **                        MOVE     SPACE
      **                                 TO  D129-YOSO(14 I4)
      *                        MOVE     SPACE
      *                                 TO  D129-YOSO(14 I7)
      **/20040407 修正
      *-----------------------------------------------------------
                          OR ( I4               = 38 
                           AND WRK-TJIHI-FLG(1) = ZERO )
                          OR ( I4               = 39 
                           AND WRK-TJIHI-FLG(2) = ZERO )
                          OR ( I4               = 40 
                           AND WRK-TJIHI-FLG(3) = ZERO )
                          OR ( I4               = 41 
                           AND WRK-TJIHI-FLG(4) = ZERO )
                          OR ( I4               = 42 
                           AND WRK-TJIHI-FLG(5) = ZERO )
                          OR ( I4               = 43 
                           AND WRK-TJIHI-FLG(6) = ZERO )
                          OR ( I4               = 44 
                           AND WRK-TJIHI-FLG(7) = ZERO )
                          OR ( I4               = 45 
                           AND WRK-TJIHI-FLG(8) = ZERO )
                          OR ( I4               = 46 
                           AND WRK-TJIHI-FLG(9) = ZERO )
                          OR ( I4               = 47 
                           AND WRK-TJIHI-FLG(10) = ZERO )
                              IF  I7   >   35
      * 20080401 H20年4月改正対応START tanaka
      *                            COMPUTE  IDX9  =   I7   -  35 
      *                            MOVE     SPACE
      *                                     TO  D1291-YOSO(14 IDX9)
                                  IF I7  =  47
                                      COMPUTE  IDX9  =   I7   -  11
                                      MOVE     SPACE
                                              TO  D129-YOSO(14 IDX9)
                                  ELSE
                                      COMPUTE  IDX9  =   I7   -  35 
                                      MOVE     SPACE
                                              TO  D1291-YOSO(14 IDX9)
                                  END-IF
      * 20080401 H20年4月改正対応END
                              ELSE
                                  MOVE     SPACE
                                           TO  D129-YOSO(14 I7)
                              END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
                         ELSE
      * 20040330 追加 Y.Takahashi
                              MOVE     SUM-YOSO(I3 I4)
                                       TO  WRK-S9
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      ** 20040407 修正 Y.Takahashi
      **                        MOVE     WRK-S9
      **                                 TO  D129-YOSO(14 I4)
      *                        MOVE     WRK-S9
      *                                 TO  D129-YOSO(14 I7)
      **/20040407 修正
      **/20040330 追加
      ** 20040330 削除 Y.Takahashi
      **                        IF   I4  =  1
      **                         AND ( SUM-DRCD   NOT  =  "9999"
      **                          OR  SUM-SRYKACD NOT  =  "99")
      **                            MOVE    SPACE
      **                                 TO  D129-YOSO(I6 I4)
      **                        ELSE
      **                            MOVE     SUM-YOSO(I3 I4)
      **                                     TO  WRK-S9
      **                            MOVE     WRK-S9
      **                                     TO  D129-YOSO(14 I4)
      **                        END-IF
      **/20040330 削除 
                              IF  I7   >   35
      * 20080401 H20年4月改正対応START tanaka
      *                            COMPUTE  IDX9  =   I7   -  35 
      *                            MOVE     WRK-S9
      *                                     TO  D1291-YOSO(14 IDX9)
                                  IF I7  =  47
                                      COMPUTE  IDX9  =   I7   -  11
                                      MOVE     WRK-S9
                                              TO  D129-YOSO(14 IDX9)
                                  ELSE
                                      COMPUTE  IDX9  =   I7   -  35 
                                      MOVE     WRK-S9
                                              TO  D1291-YOSO(14 IDX9)
                                  END-IF
      * 20080401 H20年4月改正対応END
                              ELSE
                                  MOVE     WRK-S9
                                           TO  D129-YOSO(14 I7)
                              END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
                         END-IF
                       ELSE
                         IF   I4       =   1
                          OR  I4       =   21
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      *                    OR  I4       =   38
      *                    OR  I4       =   39
      ** 20040626 追加 Y.Takahashi 保険外自費が無い場合は数値は空白
      *                    OR ( I4               = 33 
      *                     AND WRK-TJIHI-FLG(1) = ZERO )
      *                    OR ( I4               = 34 
      *                     AND WRK-TJIHI-FLG(2) = ZERO )
      *                    OR ( I4               = 35 
      *                     AND WRK-TJIHI-FLG(3) = ZERO )
      *                    OR ( I4               = 36 
      *                     AND WRK-TJIHI-FLG(4) = ZERO )
      *                    OR ( I4               = 37 
      *                     AND WRK-TJIHI-FLG(5) = ZERO )
      **/20040626 追加
      ****                     未使用行は空白を印字する。
      ** 20040407 修正 Y.Takahashi
      **                        MOVE     SPACE
      **                                 TO  D129-YOSO(I6 I4)
      *                        MOVE     SPACE
      *                                 TO  D129-YOSO(I6 I7)
      **/20040407 修正
      *-----------------------------------------------------------
                          OR ( I4               = 38 
                           AND WRK-TJIHI-FLG(1) = ZERO )
                          OR ( I4               = 39 
                           AND WRK-TJIHI-FLG(2) = ZERO )
                          OR ( I4               = 40 
                           AND WRK-TJIHI-FLG(3) = ZERO )
                          OR ( I4               = 41 
                           AND WRK-TJIHI-FLG(4) = ZERO )
                          OR ( I4               = 42 
                           AND WRK-TJIHI-FLG(5) = ZERO )
                          OR ( I4               = 43 
                           AND WRK-TJIHI-FLG(6) = ZERO )
                          OR ( I4               = 44 
                           AND WRK-TJIHI-FLG(7) = ZERO )
                          OR ( I4               = 45 
                           AND WRK-TJIHI-FLG(8) = ZERO )
                          OR ( I4               = 46 
                           AND WRK-TJIHI-FLG(9) = ZERO )
                          OR ( I4               = 47 
                           AND WRK-TJIHI-FLG(10) = ZERO )
                              IF  I7   >   35
      * 20080401 H20年4月改正対応START tanaka
      *                            COMPUTE  IDX9  =   I7   -  35 
      *                            MOVE     SPACE
      *                                     TO  D1291-YOSO(I6 IDX9)
                                  IF I7  =  47
                                      COMPUTE  IDX9  =   I7   -  11
                                      MOVE     SPACE
                                               TO  D129-YOSO(I6 IDX9)
                                  ELSE
                                      COMPUTE  IDX9  =   I7   -  35 
                                      MOVE     SPACE
                                               TO  D1291-YOSO(I6 IDX9)
                                  END-IF
      * 20080401 H20年4月改正対応END
                              ELSE
                                  MOVE     SPACE
                                           TO  D129-YOSO(I6 I7)
                              END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
                         ELSE
      * 20040330 追加 Y.Takahashi
                              MOVE     SUM-YOSO(I3 I4)
                                       TO  WRK-S9
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
      ** 20040407 修正 Y.Takahashi
      **                        MOVE     WRK-S9
      **                                 TO  D129-YOSO(I6 I4)
      *                        MOVE     WRK-S9
      *                                 TO  D129-YOSO(I6 I7)
      **/20040407 修正
      **/20040330 追加
      ** 20040325 削除 Y.Takahashi
      **                        IF   I4  =  1
      **                         AND ( SUM-DRCD   NOT  =  "9999"
      **                          OR  SUM-SRYKACD NOT  =  "99")
      **                            MOVE    SPACE
      **                                 TO  D129-YOSO(I6 I4)
      **                         ELSE
      **                            MOVE     SUM-YOSO(I3 I4)
      **                                     TO  WRK-S9
      **                            MOVE     WRK-S9
      **                                     TO  D129-YOSO(I6 I4)
      **                        END-IF
      **/20040325 削除
                              IF  I7   >   35
      * 20080401 H20年4月改正対応START tanaka
      *                            COMPUTE  IDX9  =   I7   -  35 
      *                            MOVE     WRK-S9
      *                                     TO  D1291-YOSO(I6 IDX9)
                                  IF I7  =  47
                                      COMPUTE  IDX9  =   I7   -  11
                                      MOVE     WRK-S9
                                               TO  D129-YOSO(I6 IDX9)
                                  ELSE
                                      COMPUTE  IDX9  =   I7   -  35 
                                      MOVE     WRK-S9
                                               TO  D1291-YOSO(I6 IDX9)
                                  END-IF
      * 20080401 H20年4月改正対応END
                              ELSE
                                  MOVE     WRK-S9
                                           TO  D129-YOSO(I6 I7)
                              END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
                         END-IF
                       END-IF
               END-PERFORM 
               ADD   1        TO       I3
              END-PERFORM 
      *
      *
              PERFORM 340-PRINT-OUT-SEC
              MOVE    14        TO     I5
              INITIALIZE               D129-DATA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
              INITIALIZE               D1291-DATA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) send
      *
            END-PERFORM 
      *
           .
       330-TERM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理                                               *
      *****************************************************************
      * 
       340-PRINT-OUT-SEC          SECTION.
      *
      *       帳票ページ番号
               ADD     1                   TO  CNT-PAGE
               MOVE    CNT-PAGE            TO  WRK-PAGE
               MOVE    WRK-PAGE            TO  D129-PAGE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
               ADD     1                   TO  CNT-PAGE
               MOVE    CNT-PAGE            TO  WRK-PAGE
               MOVE    WRK-PAGE            TO  D1291-PAGE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
      *
               INITIALIZE                  ORCSPRTAREA
               MOVE    "INS"               TO  SPRT-MODE
               MOVE    D100LNK-PRTKANRI-RENNUM
                                           TO  SPRT-RENNUM
               MOVE    D100LNK-PRTKANRI-TBL-KEY
                                           TO  SPRT-TBL-KEY
               MOVE    D100LNK-PRTKANRI-TBL-GROUP
                                           TO  SPRT-TBL-GROUP
               MOVE    D100LNK-PRTKANRI-SRYYM
                                           TO  SPRT-SRYYM
               MOVE    D100LNK-PRTKANRI-SKYYMD
                                           TO  SPRT-SKYYMD
               MOVE    D100LNK-PRTKANRI-SHELLID 
                                           TO  SPRT-SHELLID
               MOVE    D100LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SPRT-SHORI-RENNUM
               MOVE    D100LNK-PRTKANRI-PRIORITY
                                           TO  SPRT-PRIORITY
      ***  20040922 MOTOHASI MONPEに変更
      ***         MOVE   "A00000D129"             TO  SPRT-PRTID
      * 20080401 H20年4月改正対応START tanaka
      *           MOVE    "A00000D129.red"            TO  SPRT-PRTID
               IF  D100LNK-YMD1  <  "20080401"
                   MOVE   "A00000D129O.red"       TO  SPRT-PRTID
               ELSE
                   MOVE   "A00000D129N.red"       TO  SPRT-PRTID
               END-IF
      * 20080401 H20年4月改正対応END
      ***  20040922 MOTOHASI MONPEに変更
               MOVE    "基本台帳(外来・年齢別)"    TO  SPRT-TITLE
               MOVE    D129                    TO  SPRT-PRTDATA
               MOVE    D100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
               MOVE    D100LNK-PRTKANRI-OPID   TO  SPRT-OPID
               MOVE    D100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***  site-libを実行環境にする為コメントアウト
      ***         MOVE    "1"          TO  SPRT-SITEKBN
               CALL    "ORCSPRT"         USING
                                         ORCSPRTAREA
      * 20070913 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070913 4.0対応END
               IF  SPRT-RETURN         =   ZERO
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) start
                   MOVE   "A00000D1291.red"            TO  SPRT-PRTID
                   MOVE    "基本台帳(外来・年齢別)"    TO  SPRT-TITLE
                   MOVE    D1291                   TO  SPRT-PRTDATA
                   MOVE    D100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
                   MOVE    D100LNK-PRTKANRI-OPID   TO  SPRT-OPID
                   MOVE    D100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
                   CALL    "ORCSPRT"         USING
                                             ORCSPRTAREA
      * 20070913 4.0対応START t.tanaka
                                       SPA-AREA
      * 20070913 4.0対応END
                   IF  SPRT-RETURN         =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    "印刷ＤＢに更新できませんでした"
                                           TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC                           
                   END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/14) end
                   CONTINUE
               ELSE
                   MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC                           
               END-IF
      *
           .
       340-PRINT-OUT-EXT.
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
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      * 20070913 4.0対応END
               MOVE    D100LNK-JOBID   TO  JOB-JOBID
               MOVE    D100LNK-SHELLID TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
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
      * 20070913 4.0対応START t.tanaka
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      * 20070913 4.0対応END
           MOVE    D100LNK-JOBID   TO  JOB-JOBID
           MOVE    D100LNK-SHELLID TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
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
           INITIALIZE                          STS-AREA-DAY
           INITIALIZE                          LNK-DAY2-AREA
           MOVE    "21"                TO      LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO      LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO      WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                    STS-AREA-DAY
           INITIALIZE                    LNK-DAY7-AREA
           MOVE    "71"          TO      LNK-DAY7-IRAI
           MOVE    WRK-SYMD      TO      LNK-DAY7-YM
           CALL    "ORCSDAY"     USING   STS-AREA-DAY
                                         LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO      WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       910-DBSELECT-SEC             SECTION.
      *
           INITIALIZE                 SYUNOU-REC
      *
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      * 20070913 4.0対応START t.tanaka
      *     MOVE    D100LNK-HOSPID   TO   SYU-HOSPID
           MOVE    D100LNK-HOSPNUM   TO   SYU-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "2"              TO   SYU-NYUGAIKBN
           MOVE    "00000000"       TO   SYU-SRYYMD
      *     MOVE    D100LNK-YMD1     TO   SYU-SRYYMD
           MOVE    D100LNK-YMD2     TO   SYU-UPSRYYMD
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      ***  MOVE    "SYUNOU-ALL"   TO  ORC-DBPATH
      ***  CALL    "ORCMCPSUB"          USING
      ***                               MCPAREA
      ***                               ORCMCPAREA
      ***                               MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYUNOU-ST-KEY1    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"             TO  MCP-TABLE
           MOVE    "st_key1"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
          IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
           ELSE
               MOVE    1            TO  FLG-SYUNOU
      ***         MOVE    1            TO   FLG-END
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 910-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF        
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納情報検索
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO  MCP-FUNC
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      ***     MOVE    "SYUNOU-ALL"     TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYUNOU-ST-KEY1    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"             TO  MCP-TABLE
           MOVE    "st_key1"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
           IF      MCP-RC              =   ZERO
               MOVE    ZERO         TO  FLG-SYUNOU
               MOVE    MCPDATA-REC  TO  SYUNOU-REC
           ELSE
               MOVE    1            TO  FLG-SYUNOU
      ***         MOVE    1           TO  FLG-END
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 910-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
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
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"        TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
      ***     MOVE    "SYUNOU-ALL"     TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYUNOU-ST-KEY1    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syunou"             TO  MCP-TABLE
           MOVE    "st_key1"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
      ***     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_syskanri"             TO  MCP-TABLE
               MOVE    "key10"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み(自費情報)
      *****************************************************************
       810-SYSKANRI-READ-SEC           SECTION.
      *
      ***     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_syskanri"             TO  MCP-TABLE
               MOVE    "key10"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SYSKANRI-KEY10    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_ptinf"             TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
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
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_pthkninf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_pthkninf"             TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
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
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTHKNINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_pthkninf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者労災マスター読込処理
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptrsiinf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_ptrsiinf"             TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
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
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptrsiinf"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  TNS-YUKOSTYMD
           MOVE    ALL "9"             TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      ***     MOVE    D100LNK-PRTKANRI-SKYYMD
      ***                                 TO  ORC-DBYMD
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-TENSU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_tensu"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-TENSU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_tensu"             TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-TENSU-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_tensu"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           .
       910-TENSU-READ-EXT.
           EXIT.
      *--- 2009/03/04 CHANGE START -------------------------------------
      *****************************************************************
      *    受診履歴情報読込処理
      *****************************************************************
      * 900-JYURRK-READ-SEC         SECTION.
      **
      *     INITIALIZE                 JYURRK-REC
      ** 20070913 4.0対応START t.tanaka
      **     MOVE    SYU-HOSPID     TO  JYURRK-HOSPID  
      *     MOVE    SYU-HOSPNUM     TO  JYURRK-HOSPNUM  
      ** 20070913 4.0対応END
      *     MOVE    SYU-PTID       TO  JYURRK-PTID  
      *     MOVE    SYU-NYUGAIKBN  TO  JYURRK-NYUGAIKBN  
      *     MOVE    SYU-DENPNUM    TO  JYURRK-DENPNUM  
      **
      *     MOVE    JYURRK-REC     TO  MCPDATA-REC
      *     MOVE    "DBSELECT"     TO  MCP-FUNC
      ** 20070913 4.0対応START t.tanaka
      **     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **     CALL    "MCPSUB"             USING
      *     MOVE    "tbl_jyurrk"             TO  MCP-TABLE
      *     MOVE    "key6"                  TO  MCP-PATHNAME
      *     CALL    "ORCDBMAIN"          USING
      ** 20070913 4.0対応END
      *                                    MCPAREA
      *                                    MCPDATA-REC
      ** 20070913 4.0対応START t.tanaka
      *                                  SPA-AREA
      ** 20070913 4.0対応END
      *     IF  MCP-RC             =   ZERO
      *         MOVE    "DBFETCH"      TO  MCP-FUNC
      ** 20070913 4.0対応START t.tanaka
      **     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **     CALL    "MCPSUB"             USING
      *         MOVE    "tbl_jyurrk"             TO  MCP-TABLE
      *         MOVE    "key6"                  TO  MCP-PATHNAME
      *         CALL    "ORCDBMAIN"          USING
      ** 20070913 4.0対応END
      *                                    MCPAREA
      *                                    MCPDATA-REC
      ** 20070913 4.0対応START t.tanaka
      *                                  SPA-AREA
      ** 20070913 4.0対応END
      *         IF  MCP-RC             =   ZERO
      *             MOVE    MCPDATA-REC  TO  JYURRK-REC
      *             MOVE    ZERO         TO  FLG-JYURRK
      *         ELSE
      *             MOVE    1            TO  FLG-JYURRK
      *             INITIALIZE               JYURRK-REC
      *         END-IF
      *     ELSE
      *         MOVE    1                TO  FLG-JYURRK
      *         INITIALIZE                   JYURRK-REC
      *     END-IF
      **
      **=== sarge対応 UP By Takashima(2006/08/17) start
      **     MOVE    "DBCLOSE"           TO  MCP-FUNC
      *     MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      **=== sarge対応 UP By Takashima(2006/08/17) end
      ** 20070913 4.0対応START t.tanaka
      **     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      **     CALL    "MCPSUB"             USING
      *     MOVE    "tbl_jyurrk"             TO  MCP-TABLE
      *     MOVE    "key6"                  TO  MCP-PATHNAME
      *     CALL    "ORCDBMAIN"          USING
      ** 20070913 4.0対応END
      *                                    MCPAREA
      *                                    MCPDATA-REC
      ** 20070913 4.0対応START t.tanaka
      *                                  SPA-AREA
      ** 20070913 4.0対応END
      **
      *     .
      * 900-JYURRK-READ-EXT.
      *     EXIT.
      **      
      *****************************************************************
      *    受診履歴情報SELECT処理
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
      *
           INITIALIZE                 JYURRK-REC
           MOVE    SYU-HOSPNUM        TO   JYURRK-HOSPNUM
           MOVE    SYU-PTID           TO   JYURRK-PTID
           MOVE    SYU-NYUGAIKBN      TO   JYURRK-NYUGAIKBN
           MOVE    SYU-DENPNUM        TO   JYURRK-DENPNUM
           MOVE    JYURRK-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key6"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    ZERO                TO   FLG-JYURRK
             PERFORM  910-JYURRK-FETCH-SEC
           ELSE
             MOVE    1                TO   FLG-JYURRK
             INITIALIZE                    JYURRK-REC
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    受診履歴情報FETCH処理
      *****************************************************************
       910-JYURRK-FETCH-SEC         SECTION.
      *
           MOVE   "DBFETCH"         TO   MCP-FUNC
           MOVE   "tbl_jyurrk"      TO   MCP-TABLE
           MOVE   "key6"            TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"       USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC    TO   JYURRK-REC
           ELSE
             MOVE    1              TO   FLG-JYURRK
             INITIALIZE             JYURRK-REC
           END-IF
      *
           .
       910-JYURRK-FETCH-EXT.
           EXIT.
      *      
      *****************************************************************
      *    受診履歴情報CLOSE処理
      *****************************************************************
       910-JYURRK-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key6"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       910-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *--- 2009/03/04 CHANGE END ---------------------------------------
      *****************************************************************
      *    受診履歴情報読込処理
      *****************************************************************
       900-JYURRK-DRCD-READ-SEC         SECTION.
      *
           MOVE    JYURRK-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-JYURRK-KEY19    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key19"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"      TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-JYURRK-KEY19    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_jyurrk"             TO  MCP-TABLE
               MOVE    "key19"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
               IF  MCP-RC             =   ZERO
                   MOVE    MCPDATA-REC  TO  JYURRK-REC
                   MOVE    ZERO         TO  FLG-JYURRK
               ELSE
                   MOVE    1            TO  FLG-JYURRK
                   INITIALIZE               JYURRK-REC
               END-IF
           ELSE
               MOVE    1                TO  FLG-JYURRK
               INITIALIZE                   JYURRK-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-JYURRK-KEY19    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key19"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       900-JYURRK-DRCD-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    受信履歴情報検索
      *****************************************************************
       900-JYURRK-SELECT-SEC              SECTION.
      *
           INITIALIZE                  JYURRK-REC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    SYU-HOSPID      TO  JYURRK-HOSPID  
           MOVE    SYU-HOSPNUM      TO  JYURRK-HOSPNUM
      * 20070913 4.0対応END
           MOVE    SYU-PTID        TO  JYURRK-PTID  
           MOVE    SYU-NYUGAIKBN   TO  JYURRK-NYUGAIKBN  
           MOVE    "00000000"      TO  JYURRK-SRYYMD  
           MOVE    SYU-SRYYMD      TO  JYURRK-UPSRYYMD  
      *
           MOVE    JYURRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-JYURRK-KEY14    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key14"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
          IF      MCP-RC           =    ZERO
               PERFORM 900-JYURRK-FETCH-SEC
           ELSE
               MOVE    1           TO   FLG-JYURRK
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 900-JYURRK-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       900-JYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     受信履歴情報検索
      *****************************************************************
       900-JYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-JYURRK-KEY14    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key14"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO  FLG-JYURRK
               MOVE    MCPDATA-REC   TO  JYURRK-REC
           ELSE
               MOVE    1             TO  FLG-JYURRK
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 900-JYURRK-CLOSE-EXT
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       900-JYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-JYURRK-CLOSE-SEC                SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"        TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-JYURRK-KEY14    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key14"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       911-SRY-SELECT-SEC              SECTION.
      *
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACT-KEY7    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
          IF      MCP-RC           =    ZERO
               PERFORM 911-SRY-FETCH-SEC
           ELSE
               MOVE    1           TO   FLG-SRYACT
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 911-SRY-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       911-SRY-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為情報検索
      *****************************************************************
       911-SRY-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACT-KEY7    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    ZERO          TO  FLG-SRYACT
               MOVE    MCPDATA-REC   TO  SRYACT-REC
           ELSE
               MOVE    1             TO  FLG-SRYACT
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 911-SRY-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) start
           END-IF
           .
       911-SRY-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       911-SRY-CLOSE-SEC                SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"        TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"  TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACT-KEY7    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           .
       911-SRY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_hkncombi"             TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                  HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hkncombi"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                 HKNNUM-REC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    SYU-HOSPID     TO  HKN-HOSPID  
           MOVE    SYU-HOSPNUM     TO  HKN-HOSPNUM  
      * 20070913 4.0対応END
           MOVE    SYU-SYUHKNNUM  TO  HKN-HKNNUM  
      * 20040325 編集 Y.Takahashi
      *     MOVE    D100LNK-YMD2   TO  HKN-TEKSTYMD
           MOVE    WRK-LASTYMD    TO  HKN-TEKSTYMD
      */20040325 編集
           MOVE    ALL "9"        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hknnum"             TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_hknnum"             TO  MCP-TABLE
               MOVE    "key5"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
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
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-HKNNUM-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_hknnum"             TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"       TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"             TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_sryacct"             TO  MCP-TABLE
               MOVE    "key3"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SRYACCT-REC
                   MOVE    ZERO          TO  FLG-SRYACCT
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACCT-KEY3    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報検索
      *****************************************************************
       900-SRYACT-SELECT-SEC               SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACT-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
          IF      MCP-RC           =   ZERO
               MOVE    ZERO        TO  FLG-SRYACT
           ELSE
               MOVE    1           TO  FLG-SRYACT
           END-IF        
           .
       900-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報FETCH
      *****************************************************************
       900-SRYACT-FETCH-SEC                SECTION.
      *
           MOVE    SRYACT-REC           TO  MCPDATA-REC
           MOVE    "DBFETCH"            TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACT-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-SRYACT
               MOVE    MCPDATA-REC  TO  SRYACT-REC
           ELSE
               MOVE    1            TO  FLG-SRYACT
               INITIALIZE               SRYACT-REC
           END-IF  
           .
       900-SRYACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報 ＤＢ　クローズ処理
      *****************************************************************
       900-SRYACT-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE  "DBCLOSE"            TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACT-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryact"             TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *      
      *****************************************************************
      *    診療会計情報  検索                    20040401 Y.Takahashi
      *****************************************************************
       900-SRYACCT-SELECT-SEC         SECTION.
      *
           MOVE    SRYACCT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"       TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACCT-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    ZERO         TO  FLG-SRYACCT
           ELSE
               MOVE    1            TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報FETCH                      20040401 Y.Takahashi
      *****************************************************************
       900-SRYACCT-FETCH-SEC         SECTION.
      *
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "DBFETCH"       TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACCT-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC   TO  SRYACCT-REC
               MOVE    ZERO                    TO  FLG-SRYACCT
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
           .
       900-SRYACCT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報CLOSE                      20040401 Y.Takahashi
      *****************************************************************
       900-SRYACCT-CLOSE-SEC         SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-SRYACCT-KEY    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_sryacct"             TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       900-SRYACCT-CLOSE-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
      * 20070913 4.0対応START t.tanaka
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応START t.tanaka
                                        MCPAREA
      * 20070913 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      * 20070913 4.0対応START t.tanaka
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
      * 20070913 4.0対応START t.tanaka
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応START t.tanaka
                                        MCPAREA
      * 20070913 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      * 20070913 4.0対応START t.tanaka
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
      * 20070913 4.0対応START t.tanaka
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応START t.tanaka
                                        MCPAREA
      * 20070913 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      * 20070913 4.0対応START t.tanaka
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      * 20070913 4.0対応START t.tanaka
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応START t.tanaka
                                        MCPAREA
      * 20070913 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      * 20070913 4.0対応END
           .
       900-DBCLOSE-EXT.
           EXIT.
