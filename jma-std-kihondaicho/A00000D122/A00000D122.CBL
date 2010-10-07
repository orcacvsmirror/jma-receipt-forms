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
       PROGRAM-ID.             A00000D122.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 月計表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/03/25    @@@@-本橋   新規作成
      *  04/04/07    @@@@-高橋   DIAの変更に伴う修正
      *  04/06/26    @@@@-高橋   保険外自費項目が無い場合の数値欄修正
      *  04/09/22    @@@@-本橋   MONPE化
      *  04/12/06    @@@@-本橋   有効期限が設定された医師コードにおいて
      *                          シス管理より医師名が取得できない不具合を修正
      *  05/03/02    @@@@-鈴木   open-cobol 0.23への対応による変更
      *  05/04/19    @@@@-本橋   日レセVer2.5.0 収納テーブル取得KEY追加
      *                          への対応
      *  05/09/16    @@@@-本橋   期限が切られた診療科が印字されない不具合修正
      *                          ORCMCPSUB → MCPSUBに変更
      *  05/12/30    @@@@-劉栢   入金とス消費税修正
      *  06/08/09    @@@@-高嶋   sarge対応
      *  06/11/09    @@@@-高嶋   自費、基本診療項目追加対応
      *  07/01/23    @@@@-高嶋   終了時０ページ対応
      *  07/02/09    @@@@-内田   DB更新エラー対応（不要なDBCLOSECURSORの削除）
      *  07/06/28    @@@@-田中   生活療養費、生活負担額の修正
      *  07/07/25    @@@@-田中   errorの修正
      *  07/09/13    @@@@-田中   4.0対応,助成金額の修正,減免金額の追加
      *  07/10/11    @@@@-田中   助成金額がマイナスになってしまう不具合を修正
      *  07/10/24    @@@@-田中   入金額の重複を修正
      *  08/02/26    @@@@-田中   4.0対応漏れ修正
      *  08/03/28    @@@@-田中   H20年4月改正対応
      *  08/07/16    @@@@-田中   収納明細を更新した場合に、入金額が表示されない不具合の修正
      *  09/04/16    @@@@-門間   プログラムオプション対応(INIファイル)
      *  09/10/28    @@@@-田中   助成金額の端数処理を修正
      *  10/02/22    @@@@-門間   自賠責保険(100%)の計算式を修正
      *  10/06/29    @@@@-門間   労災・自賠責・公害保険の端数処理を修正
      *  10/08/25    @@@@-田中   労災保険を使用時に保険内負担金の計算式を修正
      *  10/09/28    @@@@-門間   預り金の集計処理を修正
      *****************************************************************
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
      *    基本台帳(入院日報)用対象患者ファイル
           SELECT  D122PT-FILE     ASSIGN  D122PPARA
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     D122P-KEY
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
      *    基本台帳(入院日報)用対象患者ファイル
       FD  D122PT-FILE.
       01  D122P. 
           03    D122P-KEY.
             05  D122P-SRYYMD      PIC X(08).
             05  D122P-PTID        PIC 9(10).
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
       01  D122PPARA.
           03  FILLER   PIC  X(05) VALUE "/tmp/".
           03  D122TPARA-FILENAME  PIC  X(12) VALUE "A00000D122PT".
           03  FILLER              PIC  X(04) VALUE ".TMP".
      *
      * 基本台帳(入院日報)DIA用コピー句
           COPY    "A00000D122.INC".
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
           COPY    "A00000D1221.INC".
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
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
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTNYUINRRK-M    PIC 9(01).
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
      * 20071024 入金額の重複を修正START T.Tanaka
      *     03  I3                PIC 9(04).
           03  I3                PIC 9(02).
      * 20071024 入金額の重複を修正END
           03  I4                PIC 9(04).
           03  I5                PIC 9(04).
           03  I6                PIC 9(04).
      * 20040407 追加 Y.Takahashi
           03  I7                PIC 9(04).
      */20040407 追加
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正START
           03  I8                PIC 9(02).
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正END
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
      **** 20050301 open-cobol 0.23への対応 SUZUKI
      *     03  IDX2              PIC 9(04).
      **** 20050301 SUZUKI
           03  JIHIIDX           PIC 9(04).
           03  NG                PIC 9(04).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
           03  IDX9              PIC 9(04).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
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
           03  WRK-NYUTENTTLKBN      PIC 9(03).
           03  WRK-BF-MISYU          PIC S9(09).
           03  WRK-BF-AZUKARI        PIC S9(09).
           03  WRK-TEN               PIC S9(09).
           03  WRK-MONEY             PIC S9(09).
           03  WRK-JIHI              PIC S9(09).
           03  WRK-SYOKUJIRYO        PIC S9(09).
           03  WRK-SYOKUJIFTN        PIC S9(09).
           03  WRK-SYUEKI            PIC S9(09).
           03  WRK-RATE              PIC 9(01)V9(03).
           03  WRK-RATE2             PIC 9(03).
      *
           03  WRK-JIHI2            PIC 9(08).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *     03  WRK-TJIHI-G       OCCURS  5.
           03  WRK-TJIHI-G       OCCURS  10.
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
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
           03  WRK-SRYYMD           PIC X(08).
           03  WRK-SEL-SRYKACD      PIC X(02).
           03  WRK-SEL-SRYKANAME    PIC X(20).
           03  WRK-SEL-DRCD         PIC X(04).
           03  WRK-SEL-DRNAME       PIC X(20).
           03  DEBUG-CNT            PIC 9(03).
           03  WRK-SYS-BEDSU        PIC 9(04).
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
      ****2005/12/30  @@@@-BRAND START
           03  WRK-SMEI-NYUHEN-MONEY  PIC S9(07).
           03  WRK-SUM-SYOHI          PIC S9(07).
      ****2005/12/30  @@@@-BRAND END
      *
           03  WRK-STARTYMD      PIC 9(08).
           03  WRK-LASTYMD       PIC 9(08).
           03  WRK-TABLECOL      PIC 9(02).
           03  WRK-NYURRK-G.
               05  WRK-NYURRK-SRYKA    PIC X(02).
               05  WRK-NYURRK-DRCD     PIC X(04).
               05  WRK-NYURRK-NYUYMD   PIC 9(08).
               05  WRK-NYURRK-STUYMD   PIC 9(08).
           03  WRK-ACCT-SDAY     PIC 9(02).
           03  WRK-ACCT-EDAY     PIC 9(02).
           03  WRK-ACCT-IN       PIC 9(04).
           03  WRK-JIHIMONEY     PIC 9(10).
           03  WRK-AZUKARI       PIC 9(10).
           03  WRK-NYU-PTID      PIC 9(10).
      *
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      * 20071011 追加 T.Tanaka
      *--- 2009/10/28 ADD + DELETE START -------------------------------
      *     03  WRK-GENMEN        PIC 9(09).
           03  WRK-HKNNAI            PIC 9(11).
           03  WRK-JOSEI             PIC 9(11).
      *--- 2009/10/28 ADD + DELETE END ---------------------------------
      * 20071011 追加 
      *
       01  SUM-AREA.
           03         SUM-SRYKAMAX      PIC  9(03).
           03         SUM-DRMAX         PIC  9(03).
           03         SUM-DAYMAX        PIC  9(03).
           03         SUM-SRYKA-G.    
             05       SUM-SRYKACD       PIC  X(02).
             05       SUM-SRYKANAME     PIC  X(20).
           03         SUM-DR-G.    
             05       SUM-DRCD          PIC  X(04).
             05       SUM-DRNAME        PIC  X(20).
           03         SUM-BEDSUIPN      PIC  9(04).
           03         SUM-PRFLG         PIC  9(01).
           03         SUM-BF-MISYU      PIC S9(09).
           03         SUM-TENKI-G       OCCURS   3.
             05       SUM-TENKI-MSG     PIC  X(22).
           03         SUM-DAY-TBL       OCCURS   32.
             05       SUM-DAY           PIC  X(08).
             05       SUM-SYUNOU-CNT    PIC  9(09).
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *       05       SUM-YOSO-TBL      OCCURS   40.
      * 20080328 H20年4月改正対応START tanaka
      *       05       SUM-YOSO-TBL      OCCURS   49.
             05       SUM-YOSO-TBL      OCCURS   50.
      * 20080328 H20年4月改正対応END
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
               07     SUM-YOSO          PIC S9(09).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      * 20080328 H20年4月改正対応START tanaka
      *    公害保険番号 
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE   "975". 
      * 20080328 H20年4月改正対応END
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
      *    患者入院履歴情報(入院履歴全枝番号用)
       01  PTNYUINRRK-M-REC.
           COPY    "CPPTNYUINRRK.INC"
           REPLACING  //PTNYUINRRK-// BY //PTNYUINRRK-M-//.
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           MOVE    "A00000D122"    TO  JOB-PGID
      *
           MOVE    "基本台帳(入院日報)"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      * 20070913 4.0対応START t.tanaka
                                   SPA-AREA
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
           MOVE  D100LNK-YMD1          TO  WRK-STARTYMD
           MOVE  D100LNK-YMD2          TO  WRK-LASTYMD
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
                MOVE    11      TO  INP-STS 
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
               MOVE    "1005"              TO  SYS-1005-KANRICD
      * 20080226 4.0対応漏れ修正START t.tanaka
               MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      * 20080226 4.0対応漏れ修正END
               MOVE    D100LNK-SRYKACD     TO  SYS-1005-KBNCD
               MOVE    D100LNK-YMD2        TO  WRK-SYMD
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
               MOVE  SPA-HOSPNUM           TO  SYS-1010-HOSPNUM
      * 20070913 4.0対応END
               MOVE    "1010"              TO  SYS-1010-KANRICD
               MOVE    D100LNK-DRCD        TO  SYS-1010-KBNCD
      ***  2004/12/06  MOTO
      ***         MOVE    D100LNK-YMD2        TO  WRK-SYMD
               MOVE    D100LNK-YMD2        TO  WRK-SYSYMD
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
      *    入院自費情報取得
      *    区分11
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "11"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
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
      *    区分12
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "12"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
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
      *    区分13
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "13"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
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
      *    区分14
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "14"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
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
      *    区分15
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "15"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
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
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *
      *    区分16
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "16"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    D100LNK-YMD1        TO  SYS-1013-EDYUKYMD
           MOVE    D100LNK-YMD2        TO  SYS-1013-STYUKYMD
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
      *    区分17
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "17"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    D100LNK-YMD1        TO  SYS-1013-EDYUKYMD
           MOVE    D100LNK-YMD2        TO  SYS-1013-STYUKYMD
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
      *    区分18
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "18"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    D100LNK-YMD1        TO  SYS-1013-EDYUKYMD
           MOVE    D100LNK-YMD2        TO  SYS-1013-STYUKYMD
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
      *    区分19
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "19"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    D100LNK-YMD1        TO  SYS-1013-EDYUKYMD
           MOVE    D100LNK-YMD2        TO  SYS-1013-STYUKYMD
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
      *    区分10
           MOVE    SPACE               TO  SYS-1013-REC
           INITIALIZE                      SYS-1013-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM           TO  SYS-1013-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    "10"                TO  SYS-1013-KBNCD
           MOVE    D100LNK-YMD2        TO  WRK-SYMD
           MOVE    D100LNK-YMD1        TO  SYS-1013-EDYUKYMD
           MOVE    D100LNK-YMD2        TO  SYS-1013-STYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           PERFORM 810-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI2        =   ZERO
               MOVE  MCPDATA-REC       TO  SYS-1013-REC
               MOVE  SYS-1013-JIHINAME TO  WRK-TJIHI(10)
               MOVE  1                 TO  WRK-TJIHI-FLG(10)
           ELSE
               MOVE  SPACE             TO  WRK-TJIHI(10)
           END-IF                                     
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      *
      *    労災医療機関情報
           MOVE    SPACE                TO  SYS-4001-REC
           INITIALIZE                       SYS-4001-REC
      * 20070913 4.0対応START t.tanaka
           MOVE  SPA-HOSPNUM            TO  SYS-4001-HOSPNUM
      * 20070913 4.0対応END
           MOVE    "4001"               TO  SYS-4001-KANRICD
           MOVE    "*"                  TO  SYS-4001-KBNCD
           MOVE    D100LNK-YMD2         TO  WRK-SYMD
      ***  20050916  MOTO
           MOVE  D100LNK-YMD1           TO  SYS-4001-EDYUKYMD
           MOVE  D100LNK-YMD2           TO  SYS-4001-STYUKYMD
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
      *
           IF    SUM-DRCD     NOT =  "9999"
                AND  SUM-SRYKACD      =  "99"
                    MOVE  "(医師変更を含む)"  
                                       TO  SUM-TENKI-MSG(1)
           END-IF
           IF    SUM-DRCD         =  "9999"
                AND  SUM-SRYKACD  NOT =  "99"
                    MOVE  "(転科を含む)"  
                                       TO  SUM-TENKI-MSG(1)
           END-IF
           IF    SUM-DRCD     NOT =  "9999"
                AND  SUM-SRYKACD  NOT =  "99"
                    MOVE  "(転科、医師変更を含む)"  
                                       TO  SUM-TENKI-MSG(1)
           END-IF
           MOVE  SUM-TENKI-MSG(1)  TO  SUM-TENKI-MSG(2)
           MOVE  SUM-TENKI-MSG(1)  TO  SUM-TENKI-MSG(3)
      *
      *    病床数の取得
           ADD  WRK-SYS-BEDSU      TO  SUM-BEDSUIPN
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
           MOVE    SPA-HOSPNUM      TO     TNS-HOSPNUM
      * 20080226 4.0対応漏れ修正END
           PERFORM   910-TENSU-READ-SEC
      ***     MOVE   TNS-GAITENTTLKBN  TO     WRK-GAITENTTLKBN  
           MOVE   TNS-NYUTENTTLKBN  TO     WRK-NYUTENTTLKBN
      *
      *    基本台帳(入院日報)用対象患者ファイル初期化
           OPEN  OUTPUT D122PT-FILE
           CLOSE        D122PT-FILE  
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
           PERFORM  110-SRYMD-PARA-HENSYU-SEC
      *
           .
       110-DR-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日付編集処理
      *****************************************************************
       110-SRYMD-PARA-HENSYU-SEC                   SECTION.
      *
      *    印刷年月日取得
           MOVE     1              TO   I3 
           MOVE     D100LNK-YMD1   TO   WRK-TMP-YMD
           INITIALIZE  WRK-SYMD
           MOVE    WRK-TMP-YMD     TO   WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD        TO   WRK-TMP-LYMD
      *
           PERFORM  UNTIL  WRK-TMP-YMD  >   D100LNK-YMD2
      ***0700725 削除START　T.Tanaka
      *           OR  IDX3         >   40
      ***削除END
              IF    WRK-TMP-YMD    >    WRK-TMP-LYMD    
                    IF WRK-TMP-MM  >=   12
                       ADD   1  TO WRK-TMP-YY
                       MOVE  1  TO WRK-TMP-MM
                       MOVE  1  TO WRK-TMP-DD
                    ELSE
                       ADD   1  TO WRK-TMP-MM
                       MOVE  1  TO WRK-TMP-DD
                    END-IF
                    INITIALIZE  WRK-SYMD
                    MOVE    WRK-TMP-YMD TO  WRK-SYMD(1:6)
                    PERFORM 31012-LASTDAY-GET-SEC
                    MOVE    WRK-SYMD    TO  WRK-TMP-LYMD
              END-IF 
              MOVE  WRK-TMP-YMD TO   SUM-DAY(I3) 
              ADD   1           TO   WRK-TMP-DD
              ADD   1           TO   I3
           END-PERFORM
           MOVE     "計"        TO   SUM-DAY(I3)  
           MOVE     I3          TO   SUM-DAYMAX   
      *
           .
       110-SRYMD-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    基本台帳(入院日報)用対象患者ファイルOPEN
           OPEN  I-O D122PT-FILE
      *
      *    入院履歴集計処理
           PERFORM 310-NYURRK-CNT-SEC
      *
      *    収納情報集計処理
           PERFORM 320-SYUNOU-CNT-SEC
      *
      *    基本台帳(入院日報)用対象患者ファイルCLOSE
           CLOSE     D122PT-FILE
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
           INITIALIZE                   D122
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
           INITIALIZE                   D1221
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      *
           MOVE   "(入院日報)"    TO    D122-SUBTITLE
           MOVE    WRK-SYSYMDWH   TO    D122-SYSYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
           MOVE   "(入院日報)"    TO    D1221-SUBTITLE
           MOVE    WRK-SYSYMDWH   TO    D1221-SYSYMD
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      *
           EVALUATE INP-STS 
           WHEN 10
             MOVE
             "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO    D122-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
             MOVE
             "入力された集計対象期間の範囲指定に誤りがあります"
                                  TO    D1221-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
           WHEN 11
             MOVE
             "入力する集計対象期間の範囲は同月内で指定をして下さい"
                                  TO    D122-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
             MOVE
             "入力する集計対象期間の範囲は同月内で指定をして下さい"
                                  TO    D1221-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
           WHEN 20
             MOVE
             "入力された入外区分の値に誤りがあります"
                                  TO    D122-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
             MOVE
             "入力された入外区分の値に誤りがあります"
                                  TO    D1221-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
           WHEN 30
             MOVE
             "入力された診療科コードの値に誤りがあります"
                                  TO    D122-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
             MOVE
             "入力された診療科コードの値に誤りがあります"
                                  TO    D1221-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
           WHEN 40
             MOVE
             "入力された医師コードの値に誤りがあります"
                                  TO    D122-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
             MOVE
             "入力された医師コードの値に誤りがあります"
                                  TO    D1221-JOBMSG
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
           END-EVALUATE
      *
           PERFORM 340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴集計処理
      *****************************************************************
       310-NYURRK-CNT-SEC      SECTION.
      *
          MOVE     1         TO   I3
          MOVE WRK-STARTYMD  TO WRK-TMP-YMD
          MOVE WRK-STARTYMD  TO WRK-TMP-LYMD
      * 
          PERFORM  UNTIL  WRK-TMP-YMD  >   WRK-LASTYMD
                     OR   WRK-TMP-DD   >   31
             INITIALIZE                  PTNYUINRRK-REC
      * 20070913 4.0対応START t.tanaka
      *       MOVE  D100LNK-HOSPID   TO   PTNYUINRRK-HOSPID
             MOVE  D100LNK-HOSPNUM   TO   PTNYUINRRK-HOSPNUM
      * 20070913 4.0対応END
             MOVE  WRK-TMP-LYMD      TO   PTNYUINRRK-NYUINYMD
             MOVE  WRK-TMP-YMD       TO   PTNYUINRRK-TAIINYMD
      *       MOVE  WRK-LASTYMD      TO   PTNYUINRRK-NYUINYMD
      *       MOVE  WRK-STARTYMD     TO   PTNYUINRRK-TAIINYMD
      **        入院履歴により入院数、退院数、在院数を取得する。
             PERFORM  900-NYURRK-SELECT-SEC
             PERFORM  UNTIL   FLG-PTNYUINRRK  =   1
      *         テスト患者処理
                INITIALIZE                   PTINF-REC
      * 20070913 4.0対応START t.tanaka
      *          MOVE     PTNYUINRRK-HOSPID   TO  PTINF-HOSPID
                MOVE     PTNYUINRRK-HOSPNUM   TO  PTINF-HOSPNUM
      * 20070913 4.0対応END
                MOVE     PTNYUINRRK-PTID     TO  PTINF-PTID
                PERFORM  900-PTINF-READ-SEC
                IF  FLG-PTINF = ZERO
                 AND  PTINF-TSTPTNUMKBN  NOT  =  "1"
      *
      *            履歴レコード初期化
                   INITIALIZE               WRK-NYURRK-G
                   INITIALIZE               PTNYUINRRK-M-REC
                   MOVE    PTNYUINRRK-PTID  
                                        TO  PTNYUINRRK-M-PTID
      * 20070913 4.0対応START t.tanaka
      *            MOVE    D100LNK-HOSPID
      *                                  TO  PTNYUINRRK-M-HOSPID
                   MOVE    D100LNK-HOSPNUM
                                        TO  PTNYUINRRK-M-HOSPNUM
      * 20070913 4.0対応END
                   MOVE    PTNYUINRRK-RRKNUM
                                        TO  PTNYUINRRK-M-RRKNUM
      *
                   PERFORM  900-NYURRK-M-SELECT-SEC
                   PERFORM  UNTIL   FLG-PTNYUINRRK-M  =   1
      *               IF 連続レコード判定 科指定>全科が同じか？
      *                                 医師指定>医師が同じか？
      *                                   全指定>判定なし
                      IF  ( ( SUM-DRCD    = "9999"
                         OR   SUM-DRCD    = PTNYUINRRK-M-DRCD1(2:4) )
                        AND ( SUM-SRYKACD = "99"
                         OR   SUM-SRYKACD = PTNYUINRRK-M-NYUINKA ) )
      *		
                          IF 0            = WRK-NYURRK-NYUYMD
                             MOVE PTNYUINRRK-M-TENNYUYMD 
                                            TO WRK-NYURRK-NYUYMD
                          END-IF
                          MOVE PTNYUINRRK-M-TENSTUYMD
                                            TO WRK-NYURRK-STUYMD
      *                    MOVE SUM-DRCD    TO WRK-NYURRK-DRCD
      *                    MOVE SUM-SRYKACD TO WRK-NYURRK-SRYKA 
                      ELSE
      *                  カウント処理
                          PERFORM  310-NYURRK-CNT-SUB-SEC
      *
      *                  履歴レコード初期化
                         INITIALIZE       WRK-NYURRK-G
                      END-IF
                      PERFORM  900-NYURRK-M-FETCH-SEC
                   END-PERFORM
      *=== sarge対応 ADD By Takashima(2006/09/16) start
                   PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/16) end
      *
      *           カウント処理
                  PERFORM  310-NYURRK-CNT-SUB-SEC
      * 20040401 追加 Y.Takahashi
                  IF     PTNYUINRRK-PTID  NOT =  WRK-NYU-PTID
      *               患者別に当日預り金を診療行為情報より取得する。
                      PERFORM 320-AZUKARI-GET-SEC 
                  END-IF
                  MOVE   PTNYUINRRK-PTID  TO  WRK-NYU-PTID
      */20040401 追加
                END-IF
                PERFORM  900-NYURRK-FETCH-SEC
             END-PERFORM
             PERFORM   900-NYURRK-CLOSE-SEC
      * 集計日カウントアップ
             ADD   1            TO WRK-TMP-DD
             MOVE WRK-TMP-YMD   TO WRK-TMP-LYMD
      *
          END-PERFORM
      *
          PERFORM  VARYING  I3     FROM 1  BY  1
                   UNTIL    I3     >=   SUM-DAYMAX
      * 入院、退院、在院 合計
               ADD  SUM-YOSO(I3 2)
                    TO  SUM-YOSO (SUM-DAYMAX 2)
               ADD  SUM-YOSO(I3 3)
                    TO  SUM-YOSO (SUM-DAYMAX 3)
               COMPUTE SUM-YOSO(I3 4)  =
                       SUM-YOSO(I3 4) 
                    -  SUM-YOSO(I3 3)
      *         ADD  SUM-YOSO(I3 4)
      *              TO  SUM-YOSO (SUM-DAYMAX 4)
      *
      *
          END-PERFORM
      *
          .
       310-NYURRK-CNT-EXT.
          EXIT.
      *
      *****************************************************************
      *    入院履歴集計サブ処理
      *****************************************************************
       310-NYURRK-CNT-SUB-SEC      SECTION.
      *
            IF      0     <   WRK-NYURRK-NYUYMD
      *
                MOVE  WRK-TMP-YMD   TO  WRK-SRYYMD
                PERFORM  320-SRYYMD-GROUPING-SEC
                IF I3  <     SUM-DAYMAX
                    IF  FLG-GET = ZERO
                        MOVE   1   TO   FLG-GET
                    END-IF
                END-IF
      *
      * 入院
                IF    WRK-TMP-LYMD  >= WRK-NYURRK-NYUYMD   
                 AND  WRK-TMP-YMD   <= WRK-NYURRK-NYUYMD
                    ADD   1      TO   SUM-YOSO(I3 2)
                END-IF
      *
      * 退院
                IF    WRK-TMP-LYMD  >= WRK-NYURRK-STUYMD   
                 AND  WRK-TMP-YMD   <= WRK-NYURRK-STUYMD
                    ADD   1      TO   SUM-YOSO(I3 3)
                END-IF
      *
      * 在院
                IF    WRK-TMP-LYMD  >= WRK-NYURRK-NYUYMD   
                 AND  WRK-TMP-YMD   <= WRK-NYURRK-STUYMD
                    ADD   1      TO   SUM-YOSO(I3 4)
                END-IF
      *
            END-IF
      *
           .
       310-NYURRK-CNT-SUB-EXT.
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
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更による削除
      ***      INITIALIZE         PTINF-REC 
      ***      MOVE SYU-HOSPID TO PTINF-HOSPID
      ***      MOVE SYU-PTID   TO PTINF-PTID
      ***      PERFORM 900-PTINF-READ-SEC
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更による削除
      *
      *   集計対象年月のデータであるかをチェックする。 
            IF      SYU-DENPPRTYMD         >=  D100LNK-YMD1
              AND   SYU-DENPPRTYMD         <=  D100LNK-YMD2
      * 20070913 4.0対応START t.tanaka
      *        AND   SYU-HOSPID              =  D100LNK-HOSPID
              AND   SYU-HOSPNUM              =  D100LNK-HOSPNUM
      * 20070913 4.0対応END
              AND   SYU-CREATEKBN       NOT =  "3"
              AND   SYU-DENPJTIKBN      NOT =  "3"
              AND   SYU-NYUGAIKBN           =  "1"
              AND ((WRK-SEL-SRYKACD         =  SPACE)
               OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                    SYU-SRYKA               =  WRK-SEL-SRYKACD))
              AND ((WRK-SEL-DRCD            =  SPACE)
               OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                    SYU-DRCD                =  WRK-SEL-DRCD))
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更による削除
      ***        AND   FLG-PTINF               =  ZERO
      ***        AND   PTINF-TSTPTNUMKBN   NOT =  "1"   
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更による削除
      * 伝票作成区分 = 3(入院レセプト用ダミー)は対象外データとして取り扱う
      * 伝票状態区分 = 3(請求取消)は対象外データとして取り扱う
      * 患者情報にレコードが存在しないデータは抽出しない。
      * テスト患者番号区分 = 1(テスト患者)は対象外データとして取り扱う
      ***  20030704 収納情報抽出条件変更
      *
               MOVE  SYU-DENPPRTYMD   TO  WRK-SRYYMD
               PERFORM  320-SRYYMD-GROUPING-SEC
               IF I3  <     SUM-DAYMAX
                  IF  FLG-GET = ZERO
                       MOVE   1   TO   FLG-GET
                  END-IF
                  PERFORM 320-SYUNOU-CNT-SUB-SEC
               END-IF
            ELSE
               IF     SYU-DENPPRTYMD          <  D100LNK-YMD1
      * 20070913 4.0対応START t.tanaka
      *          AND   SYU-HOSPID              =  D100LNK-HOSPID
                AND   SYU-HOSPNUM              =  D100LNK-HOSPNUM
      * 20070913 4.0対応END
                AND   SYU-CREATEKBN       NOT =  "3"
                AND   SYU-DENPJTIKBN      NOT =  "3"
                AND   SYU-NYUGAIKBN           =  "1"
                AND ((WRK-SEL-SRYKACD         =  SPACE)
                 OR  (WRK-SEL-SRYKACD     NOT =  SPACE  AND
                      SYU-SRYKA               =  WRK-SEL-SRYKACD))
                AND ((WRK-SEL-DRCD            =  SPACE)
                 OR  (WRK-SEL-DRCD        NOT =  SPACE  AND
                      SYU-DRCD                =  WRK-SEL-DRCD))
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更による削除
      ***          AND   FLG-PTINF               =  ZERO
      ***          AND   PTINF-TSTPTNUMKBN   NOT =  "1"   
      ***  2005/04/19  MOTO  日レセ Ver2.5.0 KEY変更による削除
      ***        範囲指定期間前未収金の集計
                 COMPUTE  WRK-MONEY    =
                          SYU-SKYMONEY -  SYU-NYUKIN-TOTAL
                 ADD  WRK-MONEY 
                        TO  SUM-BF-MISYU
      ****2005/12/30  @@@@-BRAND  START
                 PERFORM 320-SYUMEI-CNT-SUB-SEC
      ****2005/12/30  @@@@-BRAND  END
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
                    UNTIL    I3     >=   SUM-DAYMAX
      *
      * 新患来院率
      ***              INITIALIZE                WRK-RATE
      ***                                        WRK-RATE2
      ***              COMPUTE  WRK-RATE =
      ***                       SUM-YOSO(I3 4) / SUM-YOSO(I3 5)
      ***              COMPUTE  WRK-RATE2 ROUNDED     =
      ***                       WRK-RATE  *  100
      ***              MOVE     WRK-RATE2    TO  WRK-RATE-Z9
      ***              MOVE     WRK-RATE-Z9  TO  SUM-YOSO(I3 6)
      *
      * 病床数
                    MOVE    SUM-BEDSUIPN  TO  SUM-YOSO (I3 5)
                    COMPUTE SUM-YOSO(SUM-DAYMAX 5) =
                            SUM-YOSO(SUM-DAYMAX 5) +
                            SUM-YOSO(I3 5)
      * 稼働率
                    INITIALIZE                WRK-RATE
                                              WRK-RATE2
                    COMPUTE  WRK-RATE =
                             SUM-YOSO (I3 4) /
                             SUM-YOSO (I3 5)
                    COMPUTE  WRK-RATE2 ROUNDED     =
                             WRK-RATE  *  100
                    MOVE     WRK-RATE2    TO  WRK-RATE-Z9
                    MOVE     WRK-RATE-Z9  TO  SUM-YOSO(I3 6)

      * 請求数
                    MOVE SUM-SYUNOU-CNT(I3) 
                         TO  SUM-YOSO (I3 8)
                    ADD  SUM-SYUNOU-CNT(I3)
                         TO  SUM-YOSO (SUM-DAYMAX 8)
      *
      * 平均点
                    INITIALIZE                WRK-TEN
                    COMPUTE  WRK-TEN  ROUNDED      =
                             SUM-YOSO (I3 7) / 
                             SUM-YOSO (I3 8)
                    MOVE     WRK-TEN  TO  SUM-YOSO (I3 9)
      *
      * 過日未収金残額
                   COMPUTE  I4  =  I3   -  1
                   IF    I3     =  1
                         MOVE      SUM-BF-MISYU
                                   TO  WRK-MONEY   
                   ELSE
                         COMPUTE   WRK-MONEY   = 
                                       SUM-YOSO (I4 20)
                                   +   SUM-YOSO (I4 22)
                   END-IF
                   MOVE  WRK-MONEY 
                         TO  SUM-YOSO (I3 20)
      * 入金額  
      * 入金額から当日預り金を引き去る              
      ***                      COMPUTE SUM-YOSO(I3 21) =
      ***                      SUM-YOSO(I3 21) -
      ***                      SUM-YOSO(I3 23)
      ***                      COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
      ***                      SUM-YOSO(SUM-DAYMAX 21) -
      ***                      SUM-YOSO(I3 23)
      *
      * 自費金額から当日預り金を引き去る  
                   EVALUATE WRK-NYUTENTTLKBN
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *                WHEN  1                
      ****                      COMPUTE SUM-YOSO(I3 36) =
      ****                      SUM-YOSO(I3 36) -
      ****                      SUM-YOSO(I3 23)
      ****                      COMPUTE SUM-YOSO(SUM-DAYMAX 36) =
      ****                      SUM-YOSO(SUM-DAYMAX 36) -
      ****                      SUM-YOSO(I3 23)
      *                         MOVE   0    TO    SUM-YOSO(I3 36)
      *                         MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 36)
      *                WHEN  2                
      ****                      COMPUTE SUM-YOSO(I3 37) =
      ****                      SUM-YOSO(I3 37) -
      ****                      SUM-YOSO(I3 23)
      ****                      COMPUTE SUM-YOSO(SUM-DAYMAX 37) =
      ****                      SUM-YOSO(SUM-DAYMAX 37) -
      ****                      SUM-YOSO(I3 23)
      *                         MOVE   0    TO    SUM-YOSO(I3 37)
      *                         MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 37)
      *                WHEN  3                
      ****                      COMPUTE SUM-YOSO(I3 38) =
      ****                      SUM-YOSO(I3 38) -
      ****                      SUM-YOSO(I3 23)
      ****                      COMPUTE SUM-YOSO(SUM-DAYMAX 38) =
      ****                      SUM-YOSO(SUM-DAYMAX 38) -
      ****                      SUM-YOSO(I3 23)
      *                         MOVE   0    TO    SUM-YOSO(I3 38)
      *                         MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 38)
      *                WHEN  4                
      ****                      COMPUTE SUM-YOSO(I3 39) =
      ****                      SUM-YOSO(I3 39) -
      ****                      SUM-YOSO(I3 23)
      ****                      COMPUTE SUM-YOSO(SUM-DAYMAX 39) =
      ****                      SUM-YOSO(SUM-DAYMAX 39) -
      ****                      SUM-YOSO(I3 23)
      *                         MOVE   0    TO    SUM-YOSO(I3 39)
      *                         MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 39)
      *                WHEN  5                
      ****                      COMPUTE SUM-YOSO(I3 40) =
      ****                      SUM-YOSO(I3 40) -
      ****                      SUM-YOSO(I3 23)
      ****                      COMPUTE SUM-YOSO(SUM-DAYMAX 40) =
      ****                      SUM-YOSO(SUM-DAYMAX 40) -
      ****                      SUM-YOSO(I3 23)
      *                         MOVE   0    TO    SUM-YOSO(I3 40)
      *                         MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 40)
      *-------------------------------------------------------------------
                      WHEN  1                
                               MOVE   0    TO    SUM-YOSO(I3 40)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 40)
                      WHEN  2                
                               MOVE   0    TO    SUM-YOSO(I3 41)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 41)
                      WHEN  3                
                               MOVE   0    TO    SUM-YOSO(I3 42)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 42)
                      WHEN  4                
                               MOVE   0    TO    SUM-YOSO(I3 43)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 43)
                      WHEN  5                
                               MOVE   0    TO    SUM-YOSO(I3 44)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 44)
                      WHEN  6                
                               MOVE   0    TO    SUM-YOSO(I3 45)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 45)
                      WHEN  7                
                               MOVE   0    TO    SUM-YOSO(I3 46)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 46)
                      WHEN  8                
                               MOVE   0    TO    SUM-YOSO(I3 47)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 47)
                      WHEN  9                
                               MOVE   0    TO    SUM-YOSO(I3 48)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 48)
                      WHEN  10                
                               MOVE   0    TO    SUM-YOSO(I3 49)
                               MOVE   0    TO    SUM-YOSO(SUM-DAYMAX 49)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
                   END-EVALUATE
      *--- 2009/10/28 DELETE START -------------------------------------
      * 助成金額の算出
      *             COMPUTE WRK-MONEY =
      *                     SUM-YOSO(I3 10) +
      *                     SUM-YOSO(I3 11) +
      *                     SUM-YOSO(I3 13) +
      *                     SUM-YOSO(I3 14) +
      ** 20070913 消費税削除START t.tanaka
      **                     SUM-YOSO(I3 17) +
      ** 20070913 消費税削除END
      *                     SUM-YOSO(I3 18)
      ** 20071011 減免金額修正START t.tanaka
      *             MOVE    SUM-YOSO(I3 15) TO WRK-GENMEN
      *             COMPUTE WRK-GENMEN = WRK-GENMEN / 10
      *             COMPUTE WRK-GENMEN = WRK-GENMEN * 10
      ** 20071011 減免金額修正END
      *             COMPUTE SUM-YOSO(I3 16) =
      *                     WRK-MONEY -
      ** 20071011 減免金額修正START t.tanaka
      **                     SUM-YOSO(I3 15) -
      *                    WRK-GENMEN -
      ** 20071011 減免金額修正END
      *                    SUM-YOSO(I3 19) 
      *             COMPUTE SUM-YOSO(SUM-DAYMAX 16) =
      *                     SUM-YOSO(SUM-DAYMAX 16) +
      *                     SUM-YOSO(I3 16) 
      *--- 2009/10/28 DELETE END ---------------------------------------
           END-PERFORM
      *
      * 稼動率
      *
      ***     INITIALIZE                WRK-RATE
      ***                               WRK-RATE2
      ***     COMPUTE  WRK-RATE =
      ***              SUM-YOSO(SUM-DAYMAX 4) / 
      ***              SUM-YOSO(SUM-DAYMAX 5)
      ***     COMPUTE  WRK-RATE2 ROUNDED     =
      ***              WRK-RATE     *   100
      ***     MOVE     WRK-RATE2    TO  WRK-RATE-Z9
      ***     MOVE     WRK-RATE-Z9  TO  SUM-YOSO(SUM-DAYMAX 6)
      *
      * 平均点(計)
      *
           INITIALIZE        WRK-TEN
           COMPUTE  WRK-TEN  ROUNDED      =
                    SUM-YOSO (SUM-DAYMAX 7) / 
                    SUM-YOSO (SUM-DAYMAX 8)
           MOVE     WRK-TEN
                    TO  SUM-YOSO (SUM-DAYMAX 9)
      *
      * 過日未収金残額(計)
           MOVE  SUM-YOSO (SUM-DAYMAX - 1 20) 
                    TO  SUM-YOSO (SUM-DAYMAX 20)
      *
           .
       320-ETC-CNT-EXT.
           EXIT.
      *
      **** 20050302 open-cobol 0.23への対応 SUZUKI
      ******************************************************************
      **    入金額集計(収納明細情報より入返金日で集計する) 現在未使用
      ******************************************************************
      * 320-NYUKIN-CNT-SEC      SECTION.
      **
      *     INITIALIZE                 SYUMEI-REC
      *                                FLG-SYUMEI
      *     MOVE    D100LNK-HOSPID TO  SMEI-HOSPID
      *     MOVE    "2"            TO  SMEI-NYUGAIKBN
      *     MOVE    SUM-DAY (I3)   TO  SMEI-NYUHEN-YMD
      *     PERFORM 920-SMEI-DBSELECT-SEC
      *     PERFORM UNTIL  FLG-SYUMEI NOT =  ZERO
      *             INITIALIZE               PTINF-REC 
      *             MOVE SMEI-HOSPID      TO PTINF-HOSPID
      *             MOVE SMEI-PTID        TO PTINF-PTID
      *             PERFORM   900-PTINF-READ-SEC
      *             PERFORM   900-SYUNOU-READ-SEC
      *             IF   SMEI-HOSPID          =  D100LNK-HOSPID
      *              AND SMEI-NYUGAIKBN       =  "2"
      *              AND (WRK-SEL-SRYKACD     =  SPACE)
      *               OR (WRK-SEL-SRYKACD NOT =  SPACE  AND
      *                   SYU-SRYKA           =  WRK-SEL-SRYKACD)
      *              AND (WRK-SEL-DRCD        =  SPACE)
      *               OR (WRK-SEL-DRCD    NOT =  SPACE  AND
      *                    SYU-DRCD           =  WRK-SEL-DRCD)
      *              AND  FLG-PTINF           =  ZERO
      *              AND  PTINF-TSTPTNUMKBN   NOT =   "1"   
      ** 入金額(収納明細情報より入返金額を集計する。)              
      *                   ADD   SMEI-NYUHEN-MONEY
      *                        TO  SUM-YOSO(I3 18)
      *                   ADD   SMEI-NYUHEN-MONEY
      *                        TO  SUM-YOSO(SUM-DAYMAX 18)
      *             END-IF      
      *             PERFORM  920-SMEI-DBFETCH-SEC
      *     END-PERFORM
      *     PERFORM 920-SMEI-DBCLOSE-SEC
      **
      *     .
      * 320-NYUKIN-CNT-EXT.
      *     EXIT.
      **** 20050302 ここまで SUZUKI
      ******************************************************************
      *    収納情報集計サブ処理
      *****************************************************************
       320-SYUNOU-CNT-SUB-SEC      SECTION.
      *
            INITIALIZE            FLG-INVALID     
            MOVE SYU-DENPPRTYMD   TO  D122P-SRYYMD
            MOVE SYU-PTID         TO  D122P-PTID
            READ D122PT-FILE
                 KEY      IS      D122P-KEY  
                 INVALID  KEY     MOVE  1  TO FLG-INVALID
            END-READ
      *
            IF   FLG-INVALID      =     1 
      ***        診療日別対象患者ファイルに未処理のデータは患者数を集計処理を行う。
                 INITIALIZE       FLG-INVALID     
                 WRITE D122P
                        INVALID  KEY  
                        MOVE  1  TO FLG-INVALID
                 END-WRITE
      ***      受診履歴情報により初診、再診、その他件数を取得する。
      ***          PERFORM 900-JYURRK-READ-SEC
      ***          PERFORM 320-JYURRK-CNT-SUB-SEC
      *        当日預り金
      ***      患者別に当日預り金を診療行為情報より取得する。
      ***             PERFORM 911-SRY-SELECT-SEC  
      ***             PERFORM 320-AZUKARI-GET-SUB-SEC
            END-IF
      *
      *請求数のカウント
          ADD     1       TO   SUM-SYUNOU-CNT(I3)
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
      * 20080328 H20年4月改正対応START tanaka
           OR  SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM
      * 20080328 H20年4月改正対応END
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
                          TO  SUM-YOSO(I3 24)
                   ADD    SYU-SSU-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 24)
      ** 合計点数に加算
                   ADD    SYU-SSU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  指導料保険点数            
                   ADD    SYU-SDO-HKNTEN
                          TO  SUM-YOSO (I3 25)
                   ADD    SYU-SDO-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 25)
      ** 合計点数に加算
                   ADD    SYU-SDO-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  在宅料保険点数            
                   ADD    SYU-ZTK-HKNTEN
                          TO  SUM-YOSO (I3 26)
                   ADD    SYU-ZTK-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 26)
      ** 合計点数に加算
                   ADD    SYU-ZTK-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  投薬料保険点数            
                   ADD    SYU-TYK-HKNTEN
                          TO  SUM-YOSO (I3 27)
                   ADD    SYU-TYK-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 27)
      ** 合計点数に加算
                   ADD    SYU-TYK-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  注射料保険点数            
                   ADD    SYU-CSY-HKNTEN
                          TO  SUM-YOSO (I3 28)
                   ADD    SYU-CSY-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 28)
      ** 合計点数に加算FLG-SRYACT
                   ADD    SYU-CSY-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  処置料保険点数            
                   ADD    SYU-SYC-HKNTEN
                          TO  SUM-YOSO (I3 29)
                   ADD    SYU-SYC-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 29)
      ** 合計点数に加算
                   ADD    SYU-SYC-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  手術料保険点数            
                   ADD    SYU-SJT-HKNTEN
                          TO  SUM-YOSO (I3 30)
                   ADD    SYU-SJT-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 30)
      ** 合計点数に加算
                   ADD    SYU-SJT-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      **
      **  検査料保険点数            
      *             ADD    SYU-KNS-HKNTEN
      *                    TO  SUM-YOSO (I3 31)
      *             ADD    SYU-KNS-HKNTEN   
      *                    TO  SUM-YOSO(SUM-DAYMAX 31)
      *** 合計点数に加算
      *             ADD    SYU-KNS-HKNTEN
      *                    TO  SYU-TOTAL-HKNTEN
      **
      **  X線料保険点数            
      *             ADD    SYU-GZU-HKNTEN
      *                    TO  SUM-YOSO (I3 32)
      *             ADD    SYU-GZU-HKNTEN   
      *                    TO  SUM-YOSO(SUM-DAYMAX 32)
      *** 合計点数に加算
      *             ADD    SYU-GZU-HKNTEN
      *                    TO  SYU-TOTAL-HKNTEN
      **
      **  その他保険点数            
      *             ADD    SYU-ETC-HKNTEN
      *                    TO  SUM-YOSO (I3 33)
      *             ADD    SYU-ETC-HKNTEN   
      *                    TO  SUM-YOSO(SUM-DAYMAX 33)
      *** 合計点数に加算
      *             ADD    SYU-ETC-HKNTEN
      *                    TO  SYU-TOTAL-HKNTEN
      **  入院料保険点数            
      *             ADD    SYU-NYN-HKNTEN
      *                    TO  SUM-YOSO (I3 34)
      *             ADD    SYU-NYN-HKNTEN   
      *                    TO  SUM-YOSO(SUM-DAYMAX 34)
      *** 合計点数に加算
      *             ADD    SYU-NYN-HKNTEN
      *                    TO  SYU-TOTAL-HKNTEN      
      **  保険適用外金額            
      *             ADD    SYU-TOTAL-TGMONEY       
      *                    TO  SUM-YOSO (I3 35)
      *             ADD    SYU-TOTAL-TGMONEY       
      *                    TO  SUM-YOSO (SUM-DAYMAX 35)
      *             ADD    SYU-TOTAL-TGMONEY-TAX
      *                    TO  SUM-YOSO (I3 35)
      *             ADD    SYU-TOTAL-TGMONEY-TAX
      *                    TO  SUM-YOSO (SUM-DAYMAX 35)
      **
      **  自費１            
      *             ADD    SYU-JIHI-1       
      *                    TO  SUM-YOSO (I3 36)
      *             ADD    SYU-JIHI-1
      *                    TO  SUM-YOSO (SUM-DAYMAX 36)
      *             ADD    SYU-JIHI-1-TAX
      *                    TO  SUM-YOSO (I3 36)
      *             ADD    SYU-JIHI-1-TAX
      *                    TO  SUM-YOSO (SUM-DAYMAX 36)
      **
      **  自費２            
      *             ADD    SYU-JIHI-2       
      *                    TO  SUM-YOSO (I3 37)
      *             ADD    SYU-JIHI-2
      *                    TO  SUM-YOSO (SUM-DAYMAX 37)
      *             ADD    SYU-JIHI-2-TAX
      *                    TO  SUM-YOSO (I3 37)
      *             ADD    SYU-JIHI-2-TAX
      *                    TO  SUM-YOSO (SUM-DAYMAX 37)
      **
      **  自費３            
      *             ADD    SYU-JIHI-3       
      *                    TO  SUM-YOSO (I3 38)
      *             ADD    SYU-JIHI-3
      *                    TO  SUM-YOSO (SUM-DAYMAX 38)
      *             ADD    SYU-JIHI-3-TAX
      *                    TO  SUM-YOSO (I3 38)
      *             ADD    SYU-JIHI-3-TAX
      *                    TO  SUM-YOSO (SUM-DAYMAX 38)
      **
      **  自費４            
      *             ADD    SYU-JIHI-4       
      *                    TO  SUM-YOSO (I3 39)
      *             ADD    SYU-JIHI-4
      *                    TO  SUM-YOSO (SUM-DAYMAX 39)
      *             ADD    SYU-JIHI-4-TAX
      *                    TO  SUM-YOSO (I3 39)
      *             ADD    SYU-JIHI-4-TAX
      *                    TO  SUM-YOSO (SUM-DAYMAX 39)
      **
      **  自費５            
      *             ADD    SYU-JIHI-5       
      *                    TO  SUM-YOSO (I3 40)
      *             ADD    SYU-JIHI-5
      *                    TO  SUM-YOSO (SUM-DAYMAX 40)
      *             ADD    SYU-JIHI-5-TAX
      *                    TO  SUM-YOSO (I3 40)
      *             ADD    SYU-JIHI-5-TAX
      *                    TO  SUM-YOSO (SUM-DAYMAX 40)
      *----------------------------------------------------------------
      *
      *  麻酔点数            
                   ADD    SYU-MSI-HKNTEN
                          TO  SUM-YOSO (I3 31)
                   ADD    SYU-MSI-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 31)
      ** 合計点数に加算
                   ADD    SYU-MSI-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  検査料保険点数            
                   ADD    SYU-KNS-HKNTEN
                          TO  SUM-YOSO (I3 32)
                   ADD    SYU-KNS-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 32)
      ** 合計点数に加算
                   ADD    SYU-KNS-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  X線料保険点数            
                   ADD    SYU-GZU-HKNTEN
                          TO  SUM-YOSO (I3 33)
                   ADD    SYU-GZU-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 33)
      ** 合計点数に加算
                   ADD    SYU-GZU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  リハ点数            
                   ADD    SYU-ETC-HKNTEN
                          TO  SUM-YOSO (I3 34)
                   ADD    SYU-ETC-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 34)
      ** 合計点数に加算
                   ADD    SYU-ETC-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  精神科専門点数            
                   ADD    SYU-SSN-HKNTEN
                          TO  SUM-YOSO (I3 35)
                   ADD    SYU-SSN-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 35)
      ** 合計点数に加算
                   ADD    SYU-SSN-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      *
      *  放射線治療点数            
                   ADD    SYU-HOU-HKNTEN
                          TO  SUM-YOSO (I3 36)
                   ADD    SYU-HOU-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 36)
      ** 合計点数に加算
                   ADD    SYU-HOU-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      * 20080328 H20年4月改正対応START tanaka
      *  病理診断点数            
                   ADD    SYU-BYR-HKNTEN
                          TO  SUM-YOSO (I3 50)
                   ADD    SYU-BYR-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 50)
      ** 合計点数に加算
                   ADD    SYU-BYR-HKNTEN
                          TO  SYU-TOTAL-HKNTEN
      * 20080328 H20年4月改正対応END
      *
      *  入院料保険点数            
                   ADD    SYU-NYN-HKNTEN
                          TO  SUM-YOSO (I3 37)
                   ADD    SYU-NYN-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 37)
      ** 合計点数に加算
                   ADD    SYU-NYN-HKNTEN
                          TO  SYU-TOTAL-HKNTEN      
      *
      *  療養担当手当点数            
                   ADD    SYU-RYO-HKNTEN
                          TO  SUM-YOSO (I3 38)
                   ADD    SYU-RYO-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 38)
      ** 合計点数に加算
                   ADD    SYU-RYO-HKNTEN
                          TO  SYU-TOTAL-HKNTEN      
      *  保険適用外金額            
                   ADD    SYU-TOTAL-TGMONEY       
                          TO  SUM-YOSO (I3 39)
                   ADD    SYU-TOTAL-TGMONEY       
                          TO  SUM-YOSO (SUM-DAYMAX 39)
                   ADD    SYU-TOTAL-TGMONEY-TAX
                          TO  SUM-YOSO (I3 39)
                   ADD    SYU-TOTAL-TGMONEY-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 39)
      *
      *  自費１            
                   ADD    SYU-JIHI-1       
                          TO  SUM-YOSO (I3 40)
                   ADD    SYU-JIHI-1
                          TO  SUM-YOSO (SUM-DAYMAX 40)
                   ADD    SYU-JIHI-1-TAX
                          TO  SUM-YOSO (I3 40)
                   ADD    SYU-JIHI-1-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 40)
      *
      *  自費２            
                   ADD    SYU-JIHI-2       
                          TO  SUM-YOSO (I3 41)
                   ADD    SYU-JIHI-2
                          TO  SUM-YOSO (SUM-DAYMAX 41)
                   ADD    SYU-JIHI-2-TAX
                          TO  SUM-YOSO (I3 41)
                   ADD    SYU-JIHI-2-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 41)
      *
      *  自費３            
                   ADD    SYU-JIHI-3       
                          TO  SUM-YOSO (I3 42)
                   ADD    SYU-JIHI-3
                          TO  SUM-YOSO (SUM-DAYMAX 42)
                   ADD    SYU-JIHI-3-TAX
                          TO  SUM-YOSO (I3 42)
                   ADD    SYU-JIHI-3-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 42)
      *
      *  自費４            
                   ADD    SYU-JIHI-4       
                          TO  SUM-YOSO (I3 43)
                   ADD    SYU-JIHI-4
                          TO  SUM-YOSO (SUM-DAYMAX 43)
                   ADD    SYU-JIHI-4-TAX
                          TO  SUM-YOSO (I3 43)
                   ADD    SYU-JIHI-4-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 43)
      *
      *  自費５            
                   ADD    SYU-JIHI-5       
                          TO  SUM-YOSO (I3 44)
                   ADD    SYU-JIHI-5
                          TO  SUM-YOSO (SUM-DAYMAX 44)
                   ADD    SYU-JIHI-5-TAX
                          TO  SUM-YOSO (I3 44)
                   ADD    SYU-JIHI-5-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 44)
      *
      *  自費６            
                   ADD    SYU-JIHI-6       
                          TO  SUM-YOSO (I3 45)
                   ADD    SYU-JIHI-6
                          TO  SUM-YOSO (SUM-DAYMAX 45)
                   ADD    SYU-JIHI-6-TAX
                          TO  SUM-YOSO (I3 45)
                   ADD    SYU-JIHI-6-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 45)
      *
      *  自費７            
                   ADD    SYU-JIHI-7       
                          TO  SUM-YOSO (I3 46)
                   ADD    SYU-JIHI-7
                          TO  SUM-YOSO (SUM-DAYMAX 46)
                   ADD    SYU-JIHI-7-TAX
                          TO  SUM-YOSO (I3 46)
                   ADD    SYU-JIHI-7-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 46)
      *
      *  自費８            
                   ADD    SYU-JIHI-8       
                          TO  SUM-YOSO (I3 47)
                   ADD    SYU-JIHI-8
                          TO  SUM-YOSO (SUM-DAYMAX 47)
                   ADD    SYU-JIHI-8-TAX
                          TO  SUM-YOSO (I3 47)
                   ADD    SYU-JIHI-8-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 47)
      *
      *  自費９            
                   ADD    SYU-JIHI-9       
                          TO  SUM-YOSO (I3 48)
                   ADD    SYU-JIHI-9
                          TO  SUM-YOSO (SUM-DAYMAX 48)
                   ADD    SYU-JIHI-9-TAX
                          TO  SUM-YOSO (I3 48)
                   ADD    SYU-JIHI-9-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 48)
      *
      *  自費１０            
                   ADD    SYU-JIHI-10       
                          TO  SUM-YOSO (I3 49)
                   ADD    SYU-JIHI-10
                          TO  SUM-YOSO (SUM-DAYMAX 49)
                   ADD    SYU-JIHI-10-TAX
                          TO  SUM-YOSO (I3 49)
                   ADD    SYU-JIHI-10-TAX
                          TO  SUM-YOSO (SUM-DAYMAX 49)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      *
      * 保険内点数(合計保険点数)
                   ADD    SYU-TOTAL-HKNTEN
                          TO  SUM-YOSO (I3 7)
                   ADD    SYU-TOTAL-HKNTEN   
                          TO  SUM-YOSO(SUM-DAYMAX 7)
      *
      *--- 2009/10/28 ADD START ----------------------------------------
                   INITIALIZE     WRK-HKNNAI
      *--- 2009/10/28 ADD END ------------------------------------------
      * 保険内負担金              
      *
      *--- 2010/02/22 ADD START ----------------------------------------
      *--- 2010/08/25 CHANGE START -------------------------------------
      *             IF ( SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
      *               OR SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM 
      *               OR SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM )
      *               AND  PTRSI-JIBAISEIKBN  NOT  =  2
                   IF   ( (SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
                       AND  SYU-PTFTNRATE  NOT =  ZERO )
                       OR SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM 
                       OR SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM )
                       AND  PTRSI-JIBAISEIKBN  NOT  =  2
      *--- 2010/08/25 CHANGE END ---------------------------------------
      *            労災初診金額金額
                        ADD  SYU-RSISHOSHIN-MONEY  
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISHOSHIN-MONEY  
                               TO  SUM-YOSO(SUM-DAYMAX 10)
                        ADD    SYU-RSISHOSHIN-MONEY   TO   WRK-HKNNAI
      *
      *            労災再診金額金額
                        ADD  SYU-RSISAISHIN-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISAISHIN-MONEY
                               TO  SUM-YOSO(SUM-DAYMAX 10)
                        ADD   SYU-RSISAISHIN-MONEY    TO   WRK-HKNNAI
      *
      *            労災指導金額金額
                        ADD  SYU-RSISDO-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSISDO-MONEY
                               TO  SUM-YOSO(SUM-DAYMAX 10)
                        ADD   SYU-RSISDO-MONEY    TO   WRK-HKNNAI
      *
      *            労災その他金額金額
                        ADD  SYU-RSIETC-MONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-RSIETC-MONEY
                               TO  SUM-YOSO(SUM-DAYMAX 10)
                        ADD    SYU-RSIETC-MONEY   TO   WRK-HKNNAI
      *
                        ADD  SYU-HKNTEKMONEY
                               TO  SUM-YOSO(I3 10)
                        ADD  SYU-HKNTEKMONEY
                               TO  SUM-YOSO(SUM-DAYMAX 10)
                        ADD    SYU-HKNTEKMONEY   TO   WRK-HKNNAI
                   ELSE
      *--- 2010/02/22 ADD END   ----------------------------------------
      *            主保険分負担金額
      *
                   IF      SYU-SYUCOMPTOTAL  NOT = ZERO 
                           ADD SYU-SYUCOMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD SYU-SYUCOMPTOTAL
                               TO  SUM-YOSO(SUM-DAYMAX 10)
      *--- 2009/10/28 ADD START ----------------------------------------
                           MOVE    SYU-SYUCOMPTOTAL    TO   WRK-HKNNAI
      *--- 2009/10/28 ADD END ------------------------------------------
                   ELSE IF SYU-KOH1COMPTOTAL NOT = ZERO
                           ADD SYU-KOH1COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD SYU-KOH1COMPTOTAL
                               TO  SUM-YOSO(SUM-DAYMAX 10)
      *--- 2009/10/28 ADD START ----------------------------------------
                           MOVE    SYU-KOH1COMPTOTAL   TO   WRK-HKNNAI
      *--- 2009/10/28 ADD END ------------------------------------------
                   ELSE IF SYU-KOH2COMPTOTAL NOT = ZERO
                           ADD SYU-KOH2COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH2COMPTOTAL
                               TO  SUM-YOSO(SUM-DAYMAX 10)
      *--- 2009/10/28 ADD START ----------------------------------------
                           MOVE    SYU-KOH2COMPTOTAL   TO   WRK-HKNNAI
      *--- 2009/10/28 ADD END ------------------------------------------
                   ELSE IF SYU-KOH3COMPTOTAL NOT = ZERO
                           ADD     SYU-KOH3COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH3COMPTOTAL
                               TO  SUM-YOSO(SUM-DAYMAX 10)
      *--- 2009/10/28 ADD START ----------------------------------------
                           MOVE    SYU-KOH3COMPTOTAL   TO   WRK-HKNNAI
      *--- 2009/10/28 ADD END ------------------------------------------
                   ELSE IF SYU-KOH3COMPTOTAL NOT = ZERO
                           ADD     SYU-KOH4COMPTOTAL
                               TO  SUM-YOSO(I3 10)
                           ADD     SYU-KOH4COMPTOTAL
                               TO  SUM-YOSO(SUM-DAYMAX 10)
      *--- 2009/10/28 ADD START ----------------------------------------
                           MOVE    SYU-KOH4COMPTOTAL   TO   WRK-HKNNAI
      *--- 2009/10/28 ADD END ------------------------------------------
                   END-IF 
                   END-IF 
                   END-IF 
                   END-IF 
                   END-IF
      *
      *--- 2010/02/22 DELETE START -------------------------------------
      *             IF ( SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
      ** 20080328 H20年4月改正対応START tanaka
      **               OR SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM )
      *               OR SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM 
      *               OR SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM )
      ** 20080328 H20年4月改正対応END
      *               AND PTRSI-JIBAISEIKBN    NOT  =  2
      *            労災初診金額金額
      *                  ADD  SYU-RSISHOSHIN-MONEY  
      *                         TO  SUM-YOSO(I3 10)
      *                  ADD  SYU-RSISHOSHIN-MONEY  
      *                         TO  SUM-YOSO(SUM-DAYMAX 10)
      **--- 2009/10/28 ADD START ----------------------------------------
      *                  ADD    SYU-RSISHOSHIN-MONEY   TO   WRK-HKNNAI
      **--- 2009/10/28 ADD END ------------------------------------------
      **
      **            労災再診金額金額
      *                  ADD  SYU-RSISAISHIN-MONEY
      *                         TO  SUM-YOSO(I3 10)
      *                  ADD  SYU-RSISAISHIN-MONEY
      *                         TO  SUM-YOSO(SUM-DAYMAX 10)
      **--- 2009/10/28 ADD START ----------------------------------------
      *                  ADD   SYU-RSISAISHIN-MONEY    TO   WRK-HKNNAI
      **--- 2009/10/28 ADD END ------------------------------------------
      **
      **            労災指導金額金額
      *                  ADD  SYU-RSISDO-MONEY
      *                         TO  SUM-YOSO(I3 10)
      *                  ADD  SYU-RSISDO-MONEY
      *                         TO  SUM-YOSO(SUM-DAYMAX 10)
      **--- 2009/10/28 ADD START ----------------------------------------
      *                  ADD   SYU-RSISDO-MONEY    TO   WRK-HKNNAI
      **--- 2009/10/28 ADD END ------------------------------------------
      **
      **            労災その他金額金額
      *                  ADD  SYU-RSIETC-MONEY
      *                         TO  SUM-YOSO(I3 10)
      *                  ADD  SYU-RSIETC-MONEY
      *                         TO  SUM-YOSO(SUM-DAYMAX 10)
      **--- 2009/10/28 ADD START ----------------------------------------
      *                  ADD    SYU-RSIETC-MONEY   TO   WRK-HKNNAI
      **--- 2009/10/28 ADD END ------------------------------------------
      *--- 2010/02/22 DELETE END   -------------------------------------
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
                          TO  SUM-YOSO(SUM-DAYMAX 11)
      * 食事療養費(保険+自費)
                   COMPUTE WRK-SYOKUJIRYO    =
                           SYU-RYOYOHI-SKJ
                       +   SYU-RYOYOHI-SKJ-JIHI
      *** 070725追加START　T.Tanaka
                       +   SYU-RYOYOHI-LIFE
      *** 追加END

                   ADD    WRK-SYOKUJIRYO
                          TO  SUM-YOSO(I3 12)
                   ADD    WRK-SYOKUJIRYO
                          TO  SUM-YOSO(SUM-DAYMAX 12)
      * 標準負担額(保険+自費)
                   COMPUTE WRK-SYOKUJIFTN    =
                           SYU-SKYMONEY-SKJ-KEI
                       +   SYU-SKYMONEY-SKJ-JIHI-KEI
      *** 070725追加START　T.Tanaka
                       +   SYU-SKYMONEY-LIFE-KEI
      *** 追加END

                   ADD    WRK-SYOKUJIFTN
                          TO  SUM-YOSO(I3 13)
                   ADD    WRK-SYOKUJIFTN
                          TO  SUM-YOSO(SUM-DAYMAX 13)
      * 室料差額
                   ADD    SYU-RMSAGAKU
                          TO  SUM-YOSO(I3 14)
                   ADD    SYU-RMSAGAKU
                          TO  SUM-YOSO(SUM-DAYMAX 14)
      * 20070913 減免金額追加START t.tanaka
      * 減免金額
                   ADD SYU-DISCOUNT-MONEY
                          TO  SUM-YOSO(I3 15)
                   ADD SYU-DISCOUNT-MONEY
                          TO  SUM-YOSO(SUM-DAYMAX 15)
      * 20070913 減免金額追加END
      *
      *--- 2009/10/28 ADD START ----------------------------------------
      *    助成金額の算出
                   COMPUTE  WRK-MONEY  =  WRK-HKNNAI  +  WRK-JIHI
                                       +  SYU-CHOSEI  +  SYU-RMSAGAKU
                                       +  WRK-SYOKUJIFTN
                   COMPUTE  WRK-JOSEI  =  WRK-MONEY
                                       -  SYU-DISCOUNT-MONEY
      *--- 2010/06/29 ADD START ----------------------------------------
                   IF ( SYU-SYUHKNNUM    =   WRK-CONS-ROUSAI-HKNNUM
                     OR SYU-SYUHKNNUM    =   WRK-CONS-JIBAI-HKNNUM 
                     OR SYU-SYUHKNNUM    =   WRK-CONS-KOGAI-HKNNUM )
                     AND PTRSI-JIBAISEIKBN    NOT  =  2
                     CONTINUE
                   ELSE
      *--- 2010/06/29 ADD END   ----------------------------------------
                     IF   SYU-DISCOUNT-MONEY  =  ZERO
                       EVALUATE  SYS-1001-DISCOUNT-HASU-KBN2
                       WHEN  1
                         COMPUTE  WRK-JOSEI  ROUNDED  =  WRK-JOSEI / 10
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI  *  10
                       WHEN  2
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI
                                                   /  10
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI
                                                   *  10 
                       WHEN  3
                         IF   WRK-JOSEI(11:1)  NOT =  0
                           COMPUTE  WRK-JOSEI  =  WRK-JOSEI  /  10
                           COMPUTE  WRK-JOSEI  =  WRK-JOSEI  *  10 + 10
                         END-IF
                       END-EVALUATE
                     ELSE
                       EVALUATE  SYS-1001-DISCOUNT-HASU-KBN
                       WHEN  1
                         COMPUTE  WRK-JOSEI  ROUNDED  =  WRK-JOSEI / 10
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI  *  10
                       WHEN  2
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI  /  10
                         COMPUTE  WRK-JOSEI  =  WRK-JOSEI  *  10 
                       WHEN  3
                         IF   WRK-JOSEI(11:1)  NOT =  0
                           COMPUTE  WRK-JOSEI  =  WRK-JOSEI  /  10
                           COMPUTE  WRK-JOSEI  =  WRK-JOSEI  *  10 + 10
                         END-IF
                       END-EVALUATE
                     END-IF
      *--- 2010/06/29 ADD START ----------------------------------------
                   END-IF
      *--- 2010/06/29 ADD END   ----------------------------------------
      *
                   COMPUTE  WRK-JOSEI  =  WRK-JOSEI  -  SYU-SKYMONEY
                   ADD    WRK-JOSEI    TO   SUM-YOSO(I3 16) 
                   ADD    WRK-JOSEI    TO   SUM-YOSO(SUM-DAYMAX 16)
      *--- 2009/10/28 ADD END ------------------------------------------
      *
      ****2005/12/30  @@@@-BRAND START
      * 消費税              
      *             ADD    SYU-JIHI-TAX
      *                    TO  SUM-YOSO(I3 17)
      *             ADD    SYU-JIHI-TAX
      *                    TO  SUM-YOSO(SUM-DAYMAX 17)
      * 消費税              
                   ADD    SYU-SKYMONEY-TAX-SAI
                          TO  SUM-YOSO(I3 17)
                   ADD    SYU-SKYMONEY-TAX-SAI
                          TO  SUM-YOSO(SUM-DAYMAX 17)
      ****2005/12/30  @@@@-BRAND END

      * 調整金              
                   ADD    SYU-CHOSEI
                          TO  SUM-YOSO(I3 18)
                   ADD    SYU-CHOSEI
                          TO  SUM-YOSO(SUM-DAYMAX 18)
      * 窓口請求金額              
                   ADD    SYU-SKYMONEY
                          TO  SUM-YOSO(I3 19)
                   ADD    SYU-SKYMONEY
                          TO  SUM-YOSO(SUM-DAYMAX 19)
      * 入金額(当日の診療に対する入金額)   
      * 20071024 入金額の重複を修正START T.Tanaka
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正START
                   MOVE   ZERO  TO  FLG-SYUMEI
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正END
                   PERFORM  320-SYUMEI-SELECT-SEC
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正START
                   PERFORM  UNTIL  FLG-SYUMEI  = 1
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正END
                      IF     SMEI-NYUHEN-YMD <= D100LNK-YMD2
                       AND  SMEI-NYUHEN-YMD >= D100LNK-YMD1
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正START
                          MOVE   SMEI-NYUHEN-YMD(7:2)     TO    I3
                          MOVE   D100LNK-YMD1(7:2)        TO    I8
                          COMPUTE  I3  =  I3  -  I8  +  1
      *                    ADD    SYU-NYUKIN-TOTAL
                          ADD    SMEI-NYUHEN-MONEY
                                 TO  SUM-YOSO(I3 21)
      *                    ADD    SYU-NYUKIN-TOTAL
                          ADD    SMEI-NYUHEN-MONEY
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正END
                                 TO  SUM-YOSO(SUM-DAYMAX 21)
                     END-IF
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正START
                   PERFORM  320-SYUMEI-FETCH-SEC
                   END-PERFORM 
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正END
                   PERFORM  320-SYUMEI-CLOSE-SEC
      * 20071024 入金額の重複を修正END

      * 未収金残額              
                   COMPUTE     WRK-MONEY        =
                               SYU-SKYMONEY     -    SYU-NYUKIN-TOTAL
                   ADD    WRK-MONEY
                          TO  SUM-YOSO(I3 22)
                   ADD    WRK-MONEY
                          TO  SUM-YOSO(SUM-DAYMAX 22)
      * 入金額 
      * 入金額から当日預り金を引き去る              
                   EVALUATE WRK-NYUTENTTLKBN
                      WHEN  1                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-1      -
                            SYU-JIHI-1-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-1              -
                            SYU-JIHI-1-TAX
                      WHEN  2                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-2      -
                            SYU-JIHI-2-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-2              -
                            SYU-JIHI-2-TAX
                      WHEN  3                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-3      -
                            SYU-JIHI-3-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-3              -
                            SYU-JIHI-3-TAX
                      WHEN  4                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-4      -
                            SYU-JIHI-4-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-4              -
                            SYU-JIHI-4-TAX
                      WHEN  5                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-5      -
                            SYU-JIHI-5-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-5              -
                            SYU-JIHI-5-TAX
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
                      WHEN  6                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-6      -
                            SYU-JIHI-6-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-6              -
                            SYU-JIHI-6-TAX
                      WHEN  7                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-7      -
                            SYU-JIHI-7-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-7              -
                            SYU-JIHI-7-TAX
                      WHEN  8                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-8      -
                            SYU-JIHI-8-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-8              -
                            SYU-JIHI-8-TAX
                      WHEN  9                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-9      -
                            SYU-JIHI-9-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-9              -
                            SYU-JIHI-9-TAX
                      WHEN  10                
                            COMPUTE SUM-YOSO(I3 21) =
                            SUM-YOSO(I3 21) -
                            SYU-JIHI-10      -
                            SYU-JIHI-10-TAX
                            COMPUTE SUM-YOSO(SUM-DAYMAX 21) =
                            SUM-YOSO(SUM-DAYMAX 21) -
                            SYU-JIHI-10              -
                            SYU-JIHI-10-TAX
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
                   END-EVALUATE
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
           MOVE  ZERO              TO   WRK-SMEI-NYUHEN-MONEY
           MOVE  ZERO              TO   I3
           PERFORM  320-SYUMEI-SELECT-SEC
           PERFORM   UNTIL  FLG-SYUMEI = 1
             IF     SMEI-NYUHEN-YMD <= D100LNK-YMD2
               AND  SMEI-NYUHEN-YMD >= D100LNK-YMD1
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正START
      *          COMPUTE  WRK-SMEI-NYUHEN-MONEY = 
      *                   WRK-SMEI-NYUHEN-MONEY + SMEI-NYUHEN-MONEY
                MOVE     SMEI-NYUHEN-YMD(7:2)     TO    I3
                MOVE     D100LNK-YMD1(7:2)        TO    I8
                COMPUTE  I3  =  I3  -  I8  +  1
                ADD   SMEI-NYUHEN-MONEY  TO  SUM-YOSO(I3 21)
                ADD   SMEI-NYUHEN-MONEY  TO  SUM-YOSO(SUM-DAYMAX 21)
      *       ELSE
      *         COMPUTE  WRK-SMEI-NYUHEN-MONEY =
      *                  WRK-SMEI-NYUHEN-MONEY + 0
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正END
             END-IF
             PERFORM  320-SYUMEI-FETCH-SEC
           END-PERFORM.
      *=== sarge対応 ADD By Takashima(2006/09/16) start
           PERFORM  320-SYUMEI-CLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/16) end
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正START
      *     ADD   WRK-SMEI-NYUHEN-MONEY  TO  SUM-YOSO(I3 21)
      *     ADD   WRK-SMEI-NYUHEN-MONEY  TO  SUM-YOSO(SUM-DAYMAX 21)
      * 20080716 収納明細を更新した場合に、入金額が表示されない不具合の修正END
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
      *
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
                 PERFORM 900-SRYACCT-READ-SEC
                 IF      FLG-SRYACCT      =   ZERO
                   AND   ACCT-DAY(1 IDX7) >   ZERO
      *  初診の件数としてカウントする。
                         ADD  1  TO  SUM-YOSO(I3 2)
                         ADD  1  TO  SUM-YOSO(SUM-DAYMAX 2)
                         ADD  1  TO  SUM-YOSO(I3 5)
                         ADD  1  TO  SUM-YOSO(SUM-DAYMAX 5)
                 ELSE
      * 20070913 4.0対応START t.tanaka
      *                MOVE JYURRK-HOSPID TO  ACCT-HOSPID  
                      MOVE JYURRK-HOSPNUM TO  ACCT-HOSPNUM  
      * 20070913 4.0対応END
                      MOVE JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
                      MOVE JYURRK-PTID         TO  ACCT-PTID
                      MOVE JYURRK-SRYKA        TO  ACCT-SRYKA
                      MOVE JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
                      MOVE "12"                TO  ACCT-SRYKBN
                      PERFORM 900-SRYACCT-READ-SEC
                      IF  FLG-SRYACCT          =   ZERO
                      AND ACCT-DAY(1 IDX7)     >   ZERO
      *  再診の件数としてカウントする。
                         ADD  1  TO  SUM-YOSO(I3 3)
                         ADD  1  TO  SUM-YOSO(SUM-DAYMAX 3)
                         ADD  1  TO  SUM-YOSO(I3 5)
                         ADD  1  TO  SUM-YOSO(SUM-DAYMAX 5)
                      ELSE
      *  その他件数としてカウントする。
                         ADD  1  TO  SUM-YOSO(I3 4)
                         ADD  1  TO  SUM-YOSO(SUM-DAYMAX 4)
                         ADD  1  TO  SUM-YOSO(I3 5)
                         ADD  1  TO  SUM-YOSO(SUM-DAYMAX 5)
                      END-IF
                 END-IF
           END-IF
      *
      * 新患件数
           PERFORM  900-JYURRK-SELECT-SEC 
           IF       FLG-JYURRK       =   ZERO
             AND    JYURRK-SRYYMD    =   SYU-DENPPRTYMD 
      * 新患件数としてカウントする。
                    ADD  1  TO  SUM-YOSO(I3 5)
                    ADD  1  TO  SUM-YOSO(SUM-DAYMAX 5)
           END-IF 
           PERFORM  900-JYURRK-CLOSE-SEC 
      *
           .           
       320-JYURRK-CNT-SUB-EXT.
           EXIT.
      *****************************************************************
      *    預り金対象者抽出者処理
      *****************************************************************
       320-AZUKARI-GET-SEC      SECTION.
      *
      *    集計初期値
           INITIALIZE                      SRYACT-REC
      * 20070913 4.0対応START t.tanaka
      *     MOVE   PTNYUINRRK-HOSPID    TO  SRY-HOSPID  
           MOVE   PTNYUINRRK-HOSPNUM    TO  SRY-HOSPNUM  
      * 20070913 4.0対応END
           MOVE   "1"                  TO  SRY-NYUGAIKBN 
           MOVE   PTNYUINRRK-PTID      TO  SRY-PTID
           MOVE   WRK-INI-AZUKARICD    TO  SRY-SRYCD(1)
                                           SRY-SRYCD(2)
                                           SRY-SRYCD(3)
                                           SRY-SRYCD(4)
                                           SRY-SRYCD(5)
           MOVE   I3                   TO  WRK-ACCT-IN
      *    集計日範囲
           MOVE   WRK-TMP-YMD(1:6)     TO  SRY-SRYYM
           MOVE   WRK-TMP-YMD(7:2)     TO  WRK-ACCT-SDAY
           MOVE   WRK-TMP-LYMD(7:2)    TO  WRK-ACCT-EDAY
      *    
      *    
           PERFORM  911-SRY-SELECT-SEC
      *
           PERFORM  UNTIL      FLG-SRYACT  = 1
               IF    FLG-SRYACT     =   ZERO
                AND  SRY-NYUGAIKBN  =   "1"
                AND (SUM-SRYKACD    =   SRY-SRYKA
                 OR  SUM-SRYKACD    =   "99")

                     PERFORM VARYING IDX3  FROM  1 BY 1
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *                       UNTIL   IDX3  >  5
      *--- 20100928 CHANGE START ---------------------------------------
      *                       UNTIL   IDX3  >  10
                             UNTIL   IDX3  >  5
      *--- 20100928 CHANGE START ---------------------------------------
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
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
      *=== sarge対応 ADD By Takashima(2006/09/16) start
                        END-IF
                    END-PERFORM
      *=== 不要なDBCLOSECURSOR削除 By Uchida(2007/02/09) start
      *              PERFORM 900-SRYACCT-CLOSE-SEC
      *=== 不要なDBCLOSECURSOR削除 By Uchida(2007/02/09) end
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
        PERFORM VARYING  IDX4   FROM  WRK-ACCT-SDAY  BY 1
                   UNTIL IDX4    >    WRK-ACCT-EDAY
            IF  ACCT-DAY(1 IDX4) >    ZERO
                PERFORM VARYING  IDX5  FROM  2  BY 4
                   UNTIL  IDX5   >    4
                     IF  ACCT-DAY(IDX5 IDX4) > 0
                         INITIALIZE          JYURRK-REC
      * 20070913 4.0対応START t.tanaka
      *                   MOVE ACCT-HOSPID     TO JYURRK-HOSPID
                         MOVE ACCT-HOSPNUM     TO JYURRK-HOSPNUM
      * 20070913 4.0対応END
                         MOVE ACCT-PTID       TO JYURRK-PTID
                         MOVE ACCT-NYUGAIKBN  TO JYURRK-NYUGAIKBN
                         MOVE ACCT-SRYKA      TO JYURRK-SRYKA
                         MOVE ACCT-SRYYM      TO JYURRK-SRYYMD(1:6)
                         MOVE IDX4(3:2)       TO JYURRK-SRYYMD(7:2)
                         MOVE IDX5            TO IDX6
                         ADD      -1          TO IDX6
                         MOVE IDX6            TO JYURRK-RENNUM
                         MOVE ACCT-HKNCOMBI   TO JYURRK-HKNCOMBINUM
                         PERFORM 900-JYURRK-DRCD-READ-SEC
                         IF  ( WRK-SEL-DRCD     = SPACE
                          OR ( WRK-SEL-DRCD NOT = SPACE
                           AND WRK-SEL-DRCD     = JYURRK-DRCD(2:4)))
                          AND  FLG-JYURRK       = ZERO
                               COMPUTE WRK-AZUKARI =
                                 ACCT-DAY(IDX5 IDX4) * WRK-JIHIMONEY
                               ADD  WRK-AZUKARI
                                TO  SUM-YOSO(WRK-ACCT-IN 23)
                               ADD  WRK-AZUKARI
                                TO  SUM-YOSO(SUM-DAYMAX 23)
                         END-IF
                     END-IF
                END-PERFORM
            END-IF
        END-PERFORM
      *
      * 20040401 削除 Y.Takahashi
      *     PERFORM  UNTIL      FLG-SRYACT  = 1
      **
      **   集計対象のデータであるかをチェックする。 
      ** 20040318  変更
      ****      IF    FLG-SRYACT     =   ZERO
      ****       AND  SRY-NYUGAIKBN  =   "2"
      ****       AND  SRY-SRYKA      =   SUM-SRYKACD
      *      IF    FLG-SRYACT     =   ZERO
      *       AND  SRY-NYUGAIKBN  =   "1"
      *       AND (SUM-SRYKACD    =   SRY-SRYKA
      *        OR  SUM-SRYKACD    =   "99")
      ** 20040318  変更
      *         MOVE SRY-HOSPID       TO  ACCT-HOSPID  
      *         MOVE SRY-NYUGAIKBN    TO  ACCT-NYUGAIKBN
      *         MOVE SRY-PTID         TO  ACCT-PTID
      *         MOVE SRY-SRYKA        TO  ACCT-SRYKA
      *         MOVE SRY-SRYYM        TO  ACCT-SRYYM
      *         MOVE SRY-SRYKBN       TO  ACCT-SRYKBN
      *         MOVE SRY-ZAINUM       TO  ACCT-ZAINUM
      *         PERFORM  900-SRYACCT-READ2-SEC
      *         MOVE SYU-SRYYMD(7:2)  TO  IDX7G
      **
      *         IF   FLG-SRYACCT          =   ZERO
      *          AND ACCT-DAY(1 IDX7)     >   ZERO
      *           PERFORM VARYING   IDX3        FROM 1  BY  1  
      *                 UNTIL     IDX3        >    5
      *            IF   SRY-SRYCD(IDX3)        =    WRK-INI-AZUKARICD
      *             AND SRY-JIHIMONEY(IDX3)    >    ZERO
      **   当日預り金
      *                ADD  SRY-JIHIMONEY(IDX3)
      *                     TO  SUM-YOSO(I3 23)
      *                ADD  SRY-JIHIMONEY(IDX3)
      *                     TO  SUM-YOSO(SUM-MONTHMAX 23)
      *            END-IF
      *           END-PERFORM 
      *         END-IF
      *      END-IF
      *      PERFORM 911-SRY-FETCH-SEC
      *     END-PERFORM           
      *     PERFORM 911-SRY-CLOSE-SEC
      */20040401 削除 Y.Takahashi
      *
           .
       320-AZUKARI-GET-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日インデックス判別処理
      *****************************************************************
       320-SRYYMD-GROUPING-SEC      SECTION.
      *
             MOVE  1             TO  I3
             MOVE  SUM-DAYMAX    TO  I4
             PERFORM  UNTIL  I3  >   I4
                      IF   SUM-DAY(I3)  =  WRK-SRYYMD
                           MOVE  ZERO   TO I4
                      ELSE
                           ADD   1      TO I3
                      END-IF
             END-PERFORM
      *
           .
       320-SRYYMD-GROUPING-EXT.
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
            INITIALIZE                   D122
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
            INITIALIZE                   D1221
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      * 帳票サブタイトル
            MOVE   "(入院日報)"      TO  D122-SUBTITLE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
            MOVE   "(入院日報)"      TO  D1221-SUBTITLE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      * 印字対象診療年月
            MOVE   WRK-SRYYMD1(1:16) TO  D122-SRYYM
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
            MOVE   WRK-SRYYMD1(1:16) TO  D1221-SRYYM
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      * 診療科名称
            MOVE   SUM-SRYKANAME     TO  D122-SRYKA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
            MOVE   SUM-SRYKANAME     TO  D1221-SRYKA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      * 医師名称
            MOVE   SUM-DRNAME        TO  D122-DRNAME
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
            MOVE   SUM-DRNAME        TO  D1221-DRNAME
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      * 帳票出力年月日
            MOVE    WRK-SYSYMDWH     TO  D122-SYSYMD  
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
            MOVE    WRK-SYSYMDWH     TO  D1221-SYSYMD  
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      * 転帰テキスト 
            MOVE   SUM-TENKI-MSG(1)  TO  D122-TENKI(1)
            MOVE   SUM-TENKI-MSG(2)  TO  D122-TENKI(2)
            MOVE   SUM-TENKI-MSG(3)  TO  D122-TENKI(3)
      * 
      * 入院の自費タイトル設定
            PERFORM  VARYING  IDX2   FROM 1  BY  1
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *               UNTIL    IDX2   >    5 
                     UNTIL    IDX2   >    10 
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *         MOVE  WRK-TJIHI(IDX2) TO  D122-TJIHI(IDX2)
               MOVE  WRK-TJIHI(IDX2) TO  D1221-TJIHI(IDX2)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
            END-PERFORM
      *
            MOVE     1        TO           I3
            IF       SUM-DAYMAX      >     17
                     MOVE     15           TO   I5
            ELSE
                     MOVE     SUM-DAYMAX   TO   I5
            END-IF         
      *
            PERFORM    UNTIL    I3     >     SUM-DAYMAX
              PERFORM  VARYING  I6     FROM  1  BY  1
                       UNTIL    I6     >     I5
                          OR    I3     >     SUM-DAYMAX
               IF    I3       =        SUM-DAYMAX
                     MOVE     "計"
                              TO  D122-TDAY(17)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
                     MOVE     "計"
                              TO  D1221-TDAY(17)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
               ELSE 
                     MOVE     SUM-DAY(I3)(7:2)
                              TO  D122-TDAY(I6)(1:2)
                     MOVE     "日"
                              TO  D122-TDAY(I6)(3:2)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
                     MOVE     SUM-DAY(I3)(7:2)
                              TO  D1221-TDAY(I6)(1:2)
                     MOVE     "日"
                              TO  D1221-TDAY(I6)(3:2)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
               END-IF
      ***  20040922 MOTOHASI MONPEに変更
             MOVE    D122-TDAY(I6)      TO      WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT      TO      D122-TDAY(I6)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
             MOVE    WRK-OUT-INPUT      TO      D1221-TDAY(I6)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      ***  20040922 MOTOHASI MONPEに変更
      * 20040407 修正 Y.Takahashi
      *         PERFORM VARYING  I4     FROM  1  BY  1
      *                 UNTIL    I4     >     40
               PERFORM VARYING  I4     FROM  2  BY  1
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *                 UNTIL    I4     >     40
      * 20080328 H20年4月改正対応START tanaka
      *                 UNTIL    I4     >     49
                       UNTIL    I4     >     50
      * 20080328 H20年4月改正対応END
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
                       COMPUTE I7  =   I4   -  1
      */20040407 修正
                       IF     I3       =     SUM-DAYMAX
                         IF   I4       =   1
                          OR  I4       =  4
                          OR  I4       =  5
                          OR  I4       =  6
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      ** 20040626 追加 Y.Takahashi 保険外自費が無い場合は数値は空白
      *                    OR ( I4               = 36 
      *                     AND WRK-TJIHI-FLG(1) = ZERO )
      *                    OR ( I4               = 37 
      *                     AND WRK-TJIHI-FLG(2) = ZERO )
      *                    OR ( I4               = 38 
      *                     AND WRK-TJIHI-FLG(3) = ZERO )
      *                    OR ( I4               = 39 
      *                     AND WRK-TJIHI-FLG(4) = ZERO )
      *                    OR ( I4               = 40 
      *                     AND WRK-TJIHI-FLG(5) = ZERO )
      **/20040626 追加
      *-------------------------------------------------------
                          OR ( I4               = 40 
                           AND WRK-TJIHI-FLG(1) = ZERO )
                          OR ( I4               = 41 
                           AND WRK-TJIHI-FLG(2) = ZERO )
                          OR ( I4               = 42 
                           AND WRK-TJIHI-FLG(3) = ZERO )
                          OR ( I4               = 43 
                           AND WRK-TJIHI-FLG(4) = ZERO )
                          OR ( I4               = 44 
                           AND WRK-TJIHI-FLG(5) = ZERO )
                          OR ( I4               = 45 
                           AND WRK-TJIHI-FLG(6) = ZERO )
                          OR ( I4               = 46 
                           AND WRK-TJIHI-FLG(7) = ZERO )
                          OR ( I4               = 47 
                           AND WRK-TJIHI-FLG(8) = ZERO )
                          OR ( I4               = 48 
                           AND WRK-TJIHI-FLG(9) = ZERO )
                          OR ( I4               = 49 
                           AND WRK-TJIHI-FLG(10) = ZERO )
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      ***                     未使用行は空白を印字する。
      * 20040407 修正 Y.Takahashi
      *                        MOVE     SPACE
      *                                 TO  D122-YOSO(17 I4)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *                        MOVE     SPACE
      *                                 TO  D122-YOSO(17 I7)
      **/20040407 修正
                              IF  I7   >   37
      * 20080328 H20年4月改正対応START tanaka
      *                            COMPUTE  IDX9  =   I7   -  37 
      *                            MOVE     SPACE
      *                                     TO  D1221-YOSO(17 IDX9)
                                  IF I7  =  49
                                      COMPUTE  IDX9  =   I7   -  11
                                      MOVE     SPACE
                                               TO  D122-YOSO(17 IDX9)
                                  ELSE
                                      COMPUTE  IDX9  =   I7   -  37 
                                      MOVE     SPACE
                                               TO  D1221-YOSO(17 IDX9)
                                  END-IF
      * 20080328 H20年4月改正対応END
                              ELSE
                                  MOVE     SPACE
                                           TO  D122-YOSO(17 I7)
                              END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
                         ELSE
                              MOVE     SUM-YOSO(I3 I4)
                                       TO  WRK-S9
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      ** 20040407 修正 Y.Takahashi
      **                        MOVE     WRK-S9
      **                                 TO  D122-YOSO(17 I4)
      *                        MOVE     WRK-S9
      *                                 TO  D122-YOSO(17 I7)
      **/20040407 修正
                              IF  I7   >   37
      * 20080328 H20年4月改正対応START tanaka
      *                            COMPUTE  IDX9  =   I7   -  37 
      *                            MOVE     WRK-S9
      *                                   TO  D1221-YOSO(17 IDX9)
                                  IF I7  =  49
                                     COMPUTE  IDX9  =   I7   -  11
                                      MOVE     WRK-S9
                                           TO  D122-YOSO(17 IDX9)
                                  ELSE
                                     COMPUTE  IDX9  =   I7   -  37 
                                      MOVE     WRK-S9
                                           TO  D1221-YOSO(17 IDX9)
                                   END-IF
      * 20080328 H20年4月改正対応END
                              ELSE
                                  MOVE     WRK-S9
                                           TO  D122-YOSO(17 I7)
                              END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
                         END-IF
                       ELSE
                         IF   I4       =   1
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      ** 20040626 追加 Y.Takahashi 保険外自費が無い場合は数値は空白
      *                    OR ( I4               = 36 
      *                     AND WRK-TJIHI-FLG(1) = ZERO )
      *                    OR ( I4               = 37 
      *                     AND WRK-TJIHI-FLG(2) = ZERO )
      *                    OR ( I4               = 38 
      *                     AND WRK-TJIHI-FLG(3) = ZERO )
      *                    OR ( I4               = 39 
      *                     AND WRK-TJIHI-FLG(4) = ZERO )
      *                    OR ( I4               = 40 
      *                     AND WRK-TJIHI-FLG(5) = ZERO )
      **/20040626 追加
      *----------------------------------------------------------
                          OR ( I4               = 40 
                           AND WRK-TJIHI-FLG(1) = ZERO )
                          OR ( I4               = 41 
                           AND WRK-TJIHI-FLG(2) = ZERO )
                          OR ( I4               = 42 
                           AND WRK-TJIHI-FLG(3) = ZERO )
                          OR ( I4               = 43 
                           AND WRK-TJIHI-FLG(4) = ZERO )
                          OR ( I4               = 44 
                           AND WRK-TJIHI-FLG(5) = ZERO )
                          OR ( I4               = 45 
                           AND WRK-TJIHI-FLG(6) = ZERO )
                          OR ( I4               = 46 
                           AND WRK-TJIHI-FLG(7) = ZERO )
                          OR ( I4               = 47 
                           AND WRK-TJIHI-FLG(8) = ZERO )
                          OR ( I4               = 48 
                           AND WRK-TJIHI-FLG(9) = ZERO )
                          OR ( I4               = 49 
                           AND WRK-TJIHI-FLG(10) = ZERO )
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
      ***                     未使用行は空白を印字する。
      * 20040407 修正 Y.Takahashi
      *                        MOVE     SPACE
      *                                 TO  D122-YOSO(I6 I4)
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *                        MOVE     SPACE
      *                                 TO  D122-YOSO(I6 I7)
      **/20040407 修正
                              IF  I7   >   37
      * 20080328 H20年4月改正対応START tanaka   
      *                            COMPUTE  IDX9  =   I7   -  37
      *                            MOVE     SPACE
      *                                     TO  D1221-YOSO(I6 IDX9)
                                  IF I7  =  49
                                      COMPUTE  IDX9  =   I7   -  11
                                      MOVE     SPACE
                                              TO  D122-YOSO(I6 IDX9)
                                  ELSE
                                      COMPUTE  IDX9  =   I7   -  37
                                      MOVE     SPACE
                                              TO  D1221-YOSO(I6 IDX9)
                                  END-IF
      * 20080328 H20年4月改正対応END
                              ELSE
                                  MOVE     SPACE
                                           TO  D122-YOSO(I6 I7)
                              END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
                         ELSE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
      *                        MOVE     SUM-YOSO(I3 I4)
      *                                 TO  WRK-S9
      ** 20040407 修正 Y.Takahashi
      **                        MOVE     WRK-S9
      **                                 TO  D122-YOSO(I6 I4)
      *                        MOVE     WRK-S9
      *                                 TO  D122-YOSO(I6 I7)
      **/20040407 修正
                              MOVE     SUM-YOSO(I3 I4)
                                       TO  WRK-S9
                              IF  I7   >   37
      * 20080328 H20年4月改正対応START tanaka
      *                           COMPUTE  IDX9  =   I7   -  37
      *                           MOVE     WRK-S9
      *                                    TO  D1221-YOSO(I6 IDX9)
                                  IF I7  =  49
                                      COMPUTE  IDX9  =   I7   -  11
                                      MOVE     WRK-S9
                                              TO  D122-YOSO(I6 IDX9)
                                  ELSE
                                      COMPUTE  IDX9  =   I7   -  37
                                      MOVE     WRK-S9
                                              TO  D1221-YOSO(I6 IDX9)
                                  END-IF
      * 20080328 H20年4月改正対応END
                              ELSE
                                  MOVE     WRK-S9
                                           TO  D122-YOSO(I6 I7)
                              END-IF
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
                         END-IF
                       END-IF
               END-PERFORM 
               ADD   1        TO       I3
              END-PERFORM 
      *
              PERFORM 340-PRINT-OUT-SEC
              MOVE    17        TO     I5
              INITIALIZE               D122-DATA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
              INITIALIZE               D1221-DATA
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
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
               MOVE    WRK-PAGE            TO  D122-PAGE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
               ADD     1                   TO  CNT-PAGE
               MOVE    CNT-PAGE            TO  WRK-PAGE
               MOVE    WRK-PAGE            TO  D1221-PAGE
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
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
      ***         MOVE   "A00000D122"             TO  SPRT-PRTID
      * 20080328 H20年4月改正対応START tanaka
               IF  D100LNK-YMD1  <  "20080401"
                   MOVE   "A00000D122O.red"             TO  SPRT-PRTID
               ELSE
                   MOVE   "A00000D122N.red"             TO  SPRT-PRTID
               END-IF
      * 20080328 H20年4月改正対応END
      ***  20040922 MOTOHASI MONPEに変更
               MOVE    "基本台帳(入院日報)"    TO  SPRT-TITLE
               MOVE    D122                    TO  SPRT-PRTDATA
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
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) start
                   MOVE   "A00000D1221.red"        TO  SPRT-PRTID
                   MOVE    "基本台帳(入院日報)"    TO  SPRT-TITLE
                   MOVE    D1221                   TO  SPRT-PRTDATA
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
      *=== 自費、基本診療項目追加対応 By Takashima(2006/11/09) end
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
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
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
           MOVE    "1"              TO   SYU-NYUGAIKBN
      *=== 終了時０ページ対応 By Takashima(2007/01/23) start
      * 20071024 入金額の重複を修正START T.Tanaka
          MOVE    "00000000"       TO   SYU-SRYYMD
      *     MOVE    D100LNK-YMD1     TO   SYU-SRYYMD
      * 20071024 入金額の重複を修正END
      *=== 終了時０ページ対応 By Takashima(2007/01/23) END
           MOVE    "99999999"       TO   SYU-UPSRYYMD
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
               MOVE    "DBFETCH"            TO  MCP-FUNC
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
      *     MOVE    "DBCLOSE"            TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
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
      *****************************************************************
      *    受診履歴情報読込処理
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           INITIALIZE                 JYURRK-REC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    SYU-HOSPID     TO  JYURRK-HOSPID  
           MOVE    SYU-HOSPNUM     TO  JYURRK-HOSPNUM  
      * 20070913 4.0対応END
           MOVE    SYU-PTID       TO  JYURRK-PTID  
           MOVE    SYU-NYUGAIKBN  TO  JYURRK-NYUGAIKBN  
           MOVE    SYU-DENPNUM    TO  JYURRK-DENPNUM  
      *
           MOVE    JYURRK-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key6"                  TO  MCP-PATHNAME
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
      *     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
               MOVE    "tbl_jyurrk"             TO  MCP-TABLE
               MOVE    "key6"                  TO  MCP-PATHNAME
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
      *     MOVE    PATH-TBL-JYURRK-KEY6    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_jyurrk"             TO  MCP-TABLE
           MOVE    "key6"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
      *
           .
       900-JYURRK-READ-EXT.
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
           MOVE    SYU-DENPPRTYMD  TO  JYURRK-UPSRYYMD  
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
      *    入院履歴情報検索
      *****************************************************************
       900-NYURRK-SELECT-SEC                 SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *    MOVE    PATH-TBL-PTNYUINRRK-KEY26    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptnyuinrrk"             TO  MCP-TABLE
           MOVE    "key26"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC           =   ZERO
	               PERFORM 900-NYURRK-FETCH-SEC
           ELSE
               MOVE    1           TO    FLG-PTNYUINRRK
               MOVE    1           TO    FLG-END
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 900-NYURRK-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       900-NYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索
      *****************************************************************
       900-NYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *    MOVE    PATH-TBL-PTNYUINRRK-KEY26    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptnyuinrrk"             TO  MCP-TABLE
           MOVE    "key26"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK
               MOVE    MCPDATA-REC  TO  PTNYUINRRK-REC
           ELSE
               MOVE    1            TO  FLG-PTNYUINRRK
               MOVE    1            TO  FLG-END
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 900-NYURRK-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       900-NYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-NYURRK-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *     MOVE  "DBCLOSE"          TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *    MOVE    PATH-TBL-PTNYUINRRK-KEY26    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptnyuinrrk"             TO  MCP-TABLE
           MOVE    "key26"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           .
       900-NYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報検索(入院履歴枝番号のMIN値取得:KEY5)
      *****************************************************************
       900-NYURRK-M-SELECT-SEC                 SECTION.
      *
           MOVE    PTNYUINRRK-M-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTNYUINRRK-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptnyuinrrk"             TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC           =   ZERO
               PERFORM 900-NYURRK-M-FETCH-SEC
           ELSE
               MOVE    1           TO    FLG-PTNYUINRRK-M
               MOVE    1           TO    FLG-END
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       900-NYURRK-M-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院履歴情報検索(入院履歴枝番号のMIN値取得:KEY5)
      *****************************************************************
       900-NYURRK-M-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"          TO  MCP-FUNC
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTNYUINRRK-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptnyuinrrk"             TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK-M
               MOVE    MCPDATA-REC  TO  PTNYUINRRK-M-REC
           ELSE
               MOVE    1            TO  FLG-PTNYUINRRK-M
               MOVE    1            TO  FLG-END
      *=== sarge対応 DEL By Takashima(2006/09/16) start
      *         PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/16) end
           END-IF
           .
       900-NYURRK-M-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理(入院履歴枝番号のMIN値取得:KEY5)
      *****************************************************************
        900-M-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/17) start
      *      MOVE  "DBCLOSE"          TO  MCP-FUNC
            MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/17) end
      * 20070913 4.0対応START t.tanaka
      *     MOVE    PATH-TBL-PTNYUINRRK-KEY5    TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
           MOVE    "tbl_ptnyuinrrk"             TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                          MCPAREA
                                          MCPDATA-REC
      * 20070913 4.0対応START t.tanaka
                                        SPA-AREA
      * 20070913 4.0対応END
            .
        900-M-DBCLOSE-EXT.
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
      *=== sarge対応 DEL By Takashima(2006/09/16) end
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
           MOVE    D100LNK-YMD2   TO  HKN-TEKSTYMD
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
      * 20070913 4.0対応END
                                        MCPAREA
      * 20070913 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      * 20070913 4.0対応END
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
      * 20070913 4.0対応START t.tanaka
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
                                        MCPAREA
      * 20070913 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      * 20070913 4.0対応END
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
      * 20070913 4.0対応END
                                        MCPAREA
      * 20070913 4.0対応START t.tanaka
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      * 20070913 4.0対応END
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      *
      * 20070913 4.0対応START t.tanaka
      *     CALL    "MCPSUB"             USING
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      * 20070913 4.0対応END
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
