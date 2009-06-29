      ********************************************************************
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
      *********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000S137.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 保険別総括表
      *  管理者            :
      *  作成日付    作業者        記述
      *  04/03/22    @@@@-高橋     新規作成
      *  04/05/18    @@@@-有村     前期高齢者の給付割合判定修正
      *  04/07/12    @@@@-本橋     公費単独患者を併用公費一覧に集計する
      *  04/08/19    @@@@-本橋     公費単独の集計欄を追加する
      *  04/08/20    @@@@-本橋     食事療養(日数、請求額、標準負担額)を追加する
      *                            ※別フォームでの印字
      *  04/09/17    @@@@-本橋     MONPE化
      *  05/03/08    @@@@-鈴木     open-cobol 0.23への対応
      *  05/04/12    @@@@-本橋     窓口領収額の集計における不具合修正
      *  05/04/19    @@@@-本橋     船員保険（職務上）が正しく集計されない不具合修正
      *  05/09/07    @@@@-本橋     公費助成額の算出を修正
      *  06/08/09    @@@@-高嶋     sarge対応
      *  07/05/08    @@@@-内田     テスト患者の判定修正
      *  07/08/24    @@@@-大竹     高齢者・老人の給付割合の項目を７割に変更
      *                            食事療養欄の日数を回数に変更
      *  07/08/31    @@@@-長屋     船員保険の集計判定を修正
      *  07/09/20    @@@@-長屋     4.0対応
      *  07/11/14    @@@@-田中     公費単独時に医療収益・保険請求額・公費請求額が
      *                            正しく集計されない不具合を修正
      *  08/04/23    @@@@-田中     H20年4月改正対応(後期高齢者、義務教育就学前)
      *                            旧総合病院の時に診療科が表示されない不具合を修正
      *  08/05/12    @@@@-田中     返戻が正しく印字されない不具合を修正
      *  08/05/26    @@@@-田中     古い公費名を印字してしまう不具合を修正
      *  08/09/09    @@@@-田中     公費単独欄の食事療養の不具合を修正
      *  08/09/29    @@@@-田中     H20年10月改正対応(協会けんぽ)
      *  08/10/14    @@@@-田中     主科設定時に診療科を印字する
      *  08/10/30    @@@@-田中     窓口請求額の不具合を修正
      *  08/12/18    @@@@-田中     自費(095XXXXX、096XXXXXで入力)の集計欄を追加
      *                            食事療養の収益を医療収益に加算
      *  08/12/23    @@@@-田中     医療収益の計算式を変更
      *  09/01/08    @@@@-田中     公費請求額の計算式を変更
      *  09/06/26    @@@@-門間     自費分が正しく集計されない不具合を修正
      ******************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    社保用ファイル
           SELECT  S137TMP-FILE    ASSIGN    S137TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  S137TMP-KEY
                                   FILE    STATUS  IS  STS-S137TMP.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    社保一時ファイル
       FD  S137TMP-FILE.
           COPY    "A00000S137TMP.INC".
      *
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *    印刷用データ 名称領域
           COPY    "CPCOMMONPRT.INC".
      *
      *    エラーファイル 名称領域
      ***07/09/20 nagaya 4.0対応 start
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      ***07/09/20 nagaya 4.0対応 end
      *
      *    社保一時ファイル 名称領域
      ***07/09/20 nagaya 4.0対応 start
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //S137TP//.
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //S137TP//.
      ***07/09/20 nagaya 4.0対応 end
      *
           COPY    "A00000S137.INC".
           COPY    "A00000S137N1.INC".
           COPY    "A00000S137N2.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA                 PIC X(02).
           03  STS-RECE                 PIC X(02).
           03  STS-RECEERR              PIC X(02).
           03  STS-RECEUPD              PIC X(02).
           03  STS-S137TMP              PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI             PIC 9(01).
           03  FLG-SYSKANRI3            PIC 9(01).
           03  FLG-JYURRK               PIC 9(01).
           03  FLG-PASS                 PIC 9(01).
           03  FLG-TOKUTEI              PIC 9(01).
           03  FLG-TYPE                 PIC 9(01).
           03  FLG-PTINF                PIC 9(01).
           03  FLG-HKNJAINF             PIC 9(01).
           03  FLG-SYUNOU               PIC 9(01).
           03  FLG-TENSU                PIC 9(01).
           03  FLG-S137TMP              PIC 9(01).
           03  FLG-PRINT                PIC 9(01).
           03  INP-STS                  PIC 9(04).
           03  FLG-KUMIAI               PIC 9(01).
           03  FLG-HKNNUM               PIC 9(01).
           03  FLG-PTHKNINF             PIC 9(01).
           03  FLG-HEIYOU               PIC 9(01).
           03  FLG-RJNHKN               PIC 9(01).
           03  FLG-END                  PIC 9(01).
           03  FLG-HKNCOMBI             PIC 9(01).
      *--- 2009/06/26 ADD START ----------------------------------------
           03  FLG-FIRST                PIC 9(01).
      *--- 2009/06/26 ADD END   ----------------------------------------
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                 PIC 9(02).
           03  CNT-PAGE                 PIC 9(06).
           03  CNT-KOHI                 PIC 9(01).
           03  CNT-NINZU                PIC 9(06).
           03  CNT-JYURRK               PIC 9(06).
           03  CNT-DATA                 PIC 9(06).
           03  CNT-READ                 PIC 9(06).
           03  CNT-KOH                  PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY               PIC 9(02).
               05  SYS-MM               PIC 9(02).
               05  SYS-DD               PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                      PIC 9(01).
           03  IDX1                     PIC 9(04).
           03  IDX2                     PIC 9(04).
           03  IDX3                     PIC 9(04).
           03  IDX4                     PIC 9(04).
           03  IDX5                     PIC 9(04).
           03  IDX6                     PIC 9(03).

           03  IDA-G.
               05  IDA                  PIC 9(02).
           03  IDB-G.
               05  IDB                  PIC 9(02).
           03  IDC-G.
               05  IDC                  PIC 9(04).
           03  IDD-G.
               05  IDD                  PIC 9(04).
           03  IDY                      PIC 9(04).
           03  IDZ                      PIC 9(04).
           03  IDM                      PIC 9(04).
           03  IDX-MAX                  PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID           PIC 9(07).
           03  WRK-PARA-SHELLID         PIC X(08).
      ***07/09/20 nagaya 4.0対応 start
      *     03  WRK-PARA-HOSPID       PIC X(24).
           03  WRK-PARA-HOSPNUM         PIC 9(02).
      ***07/09/20 nagaya 4.0対応 end
      *    対象診療年月
           03  WRK-PARA-SRYYM           PIC X(06).
      *    対象年月最終日・初日
           03  WRK-PARA-PRTLYMD         PIC X(08).
           03  WRK-PARA-PRTSYMD         PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY            PIC 9(04).
               05  WRK-SYSMM            PIC 9(02).
               05  WRK-SYSDD            PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY              PIC 9(04).
               05  WRK-SMM              PIC 9(02).
               05  WRK-SDD              PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYY            PIC 9(04).
               05  WRK-SRYMM            PIC 9(02).
               05  WRK-SRYDD            PIC 9(02).
           03  WRK-HENYMDG              PIC X(22).
           03  WRK-SRYYM                PIC X(22).
           03  WRK-SEIYMD               PIC X(22).
           03  WRK-RECEERR              PIC X(200).
           03  WRK-SRYKA                PIC X(02).
           03  WRK-NYUGAIKBN            PIC X(01).
           03  WRK-PARA-NYUGAIKBN       PIC X(01).
           03  WRK-PTID                 PIC 9(10).
           03  WRK-HKNNUM               PIC X(03).
           03  WRK-KOHNUM               PIC X(03).
           03  WRK-TENTNK               PIC 9(04).
           03  WRK-JIBAITNK             PIC 99.9.
           03  WRK-HKNJANUM             PIC X(06).
           03  WRK-HKNJANAME            PIC X(40).
           03  WRK-KYURATE              PIC 9(02).
           03  WRK-KASAN-TBL.
               05  WRK-KENSU            PIC 9(04).
               05  WRK-NISSU            PIC 9(04).
               05  WRK-TENSU            PIC 9(09).
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *         05  WRK-MADOGK           PIC 9(09).
               05  WRK-MADOGK           PIC S9(09).
               05  WRK-JIHIGK           PIC 9(09).
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
               05  WRK-HKNGK            PIC 9(09).
               05  WRK-KOHGK            PIC 9(09).
      *--- 2008/12/18 CHANGE START -------------------------------------
      *         05  WRK-SYUEKI           PIC 9(09).
               05  WRK-SYUEKI           PIC S9(09).
      *--- 2008/12/18 CHANGE END ---------------------------------------
               05  WRK-SNISSU           PIC 9(04).
               05  WRK-SSEIKYU          PIC 9(09).
               05  WRK-SFTNGK           PIC 9(09).
      *--- 2008/12/18 ADD START ----------------------------------------
               05  WRK-SKJSKYZAN        PIC 9(09).
               05  WRK-SKJSKY           PIC 9(09).
               05  WRK-SKJFTN           PIC 9(09).
      *--- 2008/12/18 ADD END ------------------------------------------
               05  WRK-KKENSU           PIC 9(04).
               05  WRK-KNISSU           PIC 9(04).
               05  WRK-KTENSU           PIC 9(09).
           03  WRK-KOHITAN-GOKEI-TBL.
               05  WRK-GKTKENSU         PIC 9(04).
               05  WRK-GKTNISSU         PIC 9(04).
               05  WRK-GKTTENSU         PIC 9(09).
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *         05  WRK-GKTMADOGK        PIC 9(09).
               05  WRK-GKTMADOGK        PIC S9(09).
               05  WRK-GKTJIHIGK        PIC 9(09).
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
               05  WRK-GKTHKNGK         PIC 9(09).
               05  WRK-GKTKOHGK         PIC 9(09).
               05  WRK-GKTSYUEKI        PIC 9(09).
               05  WRK-SGKTNISSU        PIC 9(04).
               05  WRK-SGKTSEIKYU       PIC 9(09).
               05  WRK-SGKTFTNGK        PIC 9(09).
           03  WRK-FUKUKOHNUM           PIC X(03).
           03  WRK-KOHI-GOKEI-TBL.
               05  WRK-GKKENSU          PIC 9(04).
               05  WRK-GKNISSU          PIC 9(04).
               05  WRK-GKTENSU          PIC 9(09).
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *     03  WRK-MADOSEIKYUGK-G       PIC 9(09).
           03  WRK-MADOSEIKYUGK-G       PIC S9(09).
           03  WRK-JIHISEIKYUGK-G       PIC 9(09).
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
           03  WRK-KOHSEIKYUGK          PIC 9(09).
           03  WRK-KOHSEIKYUGK-G        PIC 9(09).
           03  WRK-ZENCOMPFTN           PIC 9(09).
           03  WRK-KOHSKJSEIKYUGK       PIC 9(09).
           03  WRK-PATH                 PIC X(64).
           03  WRK-KOHTMP-KEY.
               05  WRK-KOHTMP-RJNKBN        PIC  X(03).
               05  WRK-KOHTMP-KOHNUM        PIC  X(03).
           03  WRK-KOHTANTMP-KEY.
               05  WRK-KOHTANTMP-KOHNUM     PIC  X(03).
               05  WRK-KOHTANTMP-KOHNUM2    PIC  X(03).
           03  WRK-CHK-TBL.
               05  WRK-CHK-PTID2            PIC 9(10).
               05  WRK-CHK-SRYKA-TBL    OCCURS 20.
                   07  WRK-CHK-PTID         PIC 9(10).
                   07  WRK-CHK-DAY-TBL.
                       09  WRK-CHK-DAY      PIC 9(01)  OCCURS 31.
           03  WRK-CNT1                 PIC 9(02).
           03  WRK-CNT2                 PIC 9(02).
           03  WRK-CNT3                 PIC 9(02).
           03  WRK-CNT4                 PIC 9(02).
           03  WRK-KOHNAME-G            PIC X(24).
           03  WRK-KOHNAME-G2           PIC X(24).
           03  WRK-KOHTNAME-G           PIC X(24).
           03  WRK-KOHTNAME-G2          PIC X(24).
      *--- 2009/06/26 ADD START ----------------------------------------
           03  WRK-JIHI                 PIC 9(07).
           03  WRK-TGMONEY              PIC 9(07).
           03  WRK-JIHI-HKNJANUM        PIC X(08).
           03  WRK-YY1                  PIC 9(04).
           03  WRK-YY2                  PIC 9(04).
           03  WRK-NENREI               PIC 9(03).
           03  WRK-JIHI-TBL        OCCURS 4.
               05  WRK-JIHI-HKNNUM      PIC X(03).
      *--- 2009/06/26 ADD END   ----------------------------------------
      *
           03  WRK-MAE-INPUT            PIC X(200).
           03  WRK-OUT-INPUT            PIC X(200).
      *
           03  WRK-PAGE                 PIC ZZ9.
           03  WRK-Z3                   PIC ZZ9.
           03  WRK-Z4                   PIC ZZZZ.
           03  WRK-Z5                   PIC Z,ZZZ.
           03  WRK-Z6                   PIC ZZ,ZZ9.
           03  WRK-Z7                   PIC ZZZ,ZZZ.
           03  WRK-Z9                   PIC Z,ZZZ,ZZZ.
           03  WRK-Z92                  PIC ZZZZZZZZZ.
           03  WRK-Z10                  PIC ZZ,ZZZ,ZZ9.
           03  WRK-Z11                  PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13                  PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-Z14                  PIC ZZ,ZZZ,ZZZ,ZZZ.
      *--- 2008/12/23  ADD START ---------------------------------------
           03  WRK-Z93                  PIC --------Z.
      *--- 2008/12/23  ADD END -----------------------------------------
      *
      * 2004/03/18  追加
           03  WRK-SYS-BEDSU            PIC 9(04).
           03  WRK-SYS-BEDSUIPN         PIC 9(04).
      * 2004/03/18  追加
      * 20071114 公費単独の不具合を修正START t.tanaka
           03  WRK-FUKUKOHTEN           PIC 9(08).
           03  WRK-FUKUKOHNISSU         PIC 9(08).
      * 20071114 公費単独の不具合を修正END
      * 20080909 公費単独欄の食事療養の不具合を修正START
           03  WRK-FUKU-S-KAISU         PIC 9(08).
           03  WRK-FUKU-S-SKYMONEY      PIC 9(08).
           03  WRK-FUKU-S-FTNMONEY      PIC 9(08).
      * 20080909 公費単独欄の食事療養の不具合を修正END
      *--- 2008/12/23 ADD START ----------------------------------------
           03  WRK-SKYMONEY             PIC S9(08).
      *--- 2008/12/23 ADD END ------------------------------------------
      *--- 2009/01/08 ADD START ----------------------------------------
           03  WRK-FTNFLG               PIC 9(01).
           03  WRK-HKNFTN               PIC 9(09).
           03  WRK-KOHFTN               PIC 9(09).
      *--- 2009/01/08 ADD END ------------------------------------------
      *
      *    診療科名退避エリア
       01  SUM-AREA.
           03  SUM-SRYKAMAX             PIC 9(03).
           03  SUM-SRYDR-TBL        OCCURS 100.
               05  SUM-SRYKA            PIC X(02).
               05  SUM-SRYKANAME        PIC X(24).
      *
      *    入院履歴情報退避エリア
       01  WRK-NYUIN-AREA.
           03  WRK-NYUIN-TBL.
               05  WRK-NYUIN-DAY        PIC  9(01)  OCCURS 31.
      *
      *    室料差額退避エリア
       01  WRK-ROOM-SAGAKU-TBL.
           03  WRK-ROOM-MAX             PIC 9(02).
           03  WRK-ROOM-DATA        OCCURS 50.
               05  WRK-ROOM-CD          PIC 9(03).
               05  WRK-ROOM-SGK         PIC 9(05).
      *
           COPY    "CPSHELLTBL.INC".
      *
      ***07/09/20 nagaya 4.0対応 start
      *     COPY    "ORCA-DBPATH".
      ***07/09/20 nagaya 4.0対応 end
      *
      ******************************************************************
      *    ファイルレイアウト
      ******************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    職員情報情報
           COPY    "CPSK1010.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *--- 2009/06/26 ADD START ----------------------------------------
      *    主科設定情報 
           COPY    "CPSK2009.INC".
      *--- 2009/06/26 ADD END   ----------------------------------------
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    室料差額情報
           COPY    "CPSK5113.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    請求管理マスタ
           COPY    "CPRCF010.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      ******************************************************************
      *    サブプロ用　領域
      ******************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *--- 2009/06/26 ADD START ----------------------------------------
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *--- 2009/06/26 ADD END   ----------------------------------------
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
	       COPY    "A00000S100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
           USING   A00000S100-LNK.
      *
      ******************************************************************
      *    主　　処理
      ******************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC
      *
           PERFORM  300-END-SEC
      *
      *** 20050308 open-cobol 0.23への対応 SUZUKI
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *** 20050308 ここまで
      ******************************************************************
      *    初期　処理
      ******************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                   FLG-AREA
           INITIALIZE                   STS-AREA
           INITIALIZE                   WRK-AREA
           INITIALIZE                   CNT-AREA
           INITIALIZE                   SYS-AREA
           INITIALIZE                   WRK-PARA
           INITIALIZE                   IDX-AREA
           INITIALIZE                   SUM-AREA
           INITIALIZE                   WRK-ROOM-SAGAKU-TBL
           INITIALIZE                   WRK-NYUIN-AREA
      ***07/09/20 nagaya 4.0対応 start
           INITIALIZE                   SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *     INITIALIZE                  S137TP
      *     INITIALIZE                  S137
      *
      ***07/09/20 nagaya 4.0対応 start
           MOVE    S100LNK-HOSPNUM      TO   WRK-PARA-HOSPNUM
           MOVE    S100LNK-HOSPNUM      TO   SPA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
      *    ステップ管理開始処理
           MOVE    "STS"                TO   SJOBKANRI-MODE
           INITIALIZE                        JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
           MOVE    S100LNK-HOSPNUM      TO   JOB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    S100LNK-JOBID        TO   JOB-JOBID
           MOVE    S100LNK-SHELLID      TO   JOB-SHELLID
           MOVE    "A00000S100"         TO   JOB-PGID
           MOVE    "保険別請求チェック表(社保)"   TO   JOB-SHELLMSG
           CALL    "ORCSJOB"            USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    S100LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      ***07/09/20 nagaya 4.0対応 end
      *
      *     PERFORM 100-DBOPEN-SEC
      *
      *    パラメタ編集処理
           PERFORM  110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      ******************************************************************
      *    パラメタ編集処理
      ******************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    システム日付
           MOVE   S100LNK-PRTKANRI-SKYYMD   TO   WRK-SYMD
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE   WRK-HENYMDG               TO   WRK-SEIYMD
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正START tanaka
           MOVE   S100LNK-PRTKANRI-SKYYMD   TO   SPA-SYSYMD
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正END
      *
      ***  20040820  @@@@-MOTOHASI  追加
           MOVE   S100LNK-NYUGAIKBN   TO   WRK-PARA-NYUGAIKBN
      ***  20040820
      *
      *    対象日付処理
           MOVE   S100LNK-YM          TO   WRK-SRYYMD(1:6)
           MOVE   "01"                TO   WRK-SRYYMD(7:2)
           MOVE   S100LNK-YM          TO   WRK-SYMD(1:6)
           MOVE   "01"                TO   WRK-SYMD(7:2)
           PERFORM   31012-SEIWA-HEN-SEC
           MOVE   WRK-HENYMDG(1:16)   TO   WRK-SRYYM
           MOVE   S100LNK-YM          TO   WRK-PARA-SRYYM
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE   S100LNK-YM          TO   WRK-SYMD(1:6)
           PERFORM   31012-LASTDAY-GET-SEC
           MOVE   WRK-SYMD            TO   WRK-PARA-PRTLYMD
      *
      *    対象年月初日取得
           INITIALIZE   WRK-SYMD
           MOVE   S100LNK-YM          TO   WRK-PARA-PRTSYMD(1:6)
           MOVE   "01"                TO   WRK-PARA-PRTSYMD(7:2)
      *
      *    医療機関ＩＤ編集
           MOVE   SPACE               TO   SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE   "1001"              TO   SYS-1001-KANRICD
           MOVE   "*"                 TO   SYS-1001-KBNCD
      ***07/09/20 nagaya 4.0対応 start
           MOVE   SPA-SYSYMD          TO   SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE   SPA-HOSPNUM         TO   SYS-1001-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE   SYS-1001-REC        TO   MCPDATA-REC
           PERFORM   800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE   MCPDATA-REC         TO   SYS-1001-REC
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
             MOVE   SYS-1001-HOSPNUM    TO   WRK-PARA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
             MOVE   SYS-1001-BEDSU      TO   WRK-SYS-BEDSU
             MOVE   SYS-1001-BEDSUIPN   TO   WRK-SYS-BEDSUIPN
           ELSE
             MOVE   "医療機関ＩＤが取得できませんでした。"
                                        TO   WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
      *    自賠責点数単価取得
           MOVE   SPACE               TO   SYS-4001-REC
           INITIALIZE                      SYS-4001-REC
           MOVE   "4001"              TO   SYS-4001-KANRICD
           MOVE   "*"                 TO   SYS-4001-KBNCD
      ***07/09/20 nagaya 4.0対応 start
           MOVE   SPA-SYSYMD          TO   SYS-4001-STYUKYMD
                                           SYS-4001-EDYUKYMD
           MOVE   SPA-HOSPNUM         TO   SYS-4001-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE   SYS-4001-REC        TO   MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF   FLG-SYSKANRI   =   ZERO
             MOVE    MCPDATA-REC      TO   SYS-4001-REC
           ELSE
             INITIALIZE              SYS-4001-REC
           END-IF
           IF   SYS-4001-TENKBN   =   "1"
             MOVE    11.5             TO   WRK-JIBAITNK
           ELSE
             MOVE    12               TO   WRK-JIBAITNK
           END-IF
      *
      *    診療科情報取得
      ***07/09/20 nagaya 4.0対応 start
           MOVE   SPACE               TO   SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
      ***07/09/20 nagaya 4.0対応 end
           MOVE   "1005"              TO   SYS-1005-KANRICD
           MOVE   WRK-PARA-PRTLYMD    TO   WRK-SYMD
      ***07/09/20 nagaya 4.0対応 start
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正START tanaka
      *       MOVE    SPA-SRYYMD             TO  SYS-1005-STYUKYMD
      *                                          SYS-1005-EDYUKYMD
           MOVE    SPA-SYSYMD         TO   SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD
      * 20080423 旧総合病院の時に診療科が表示されない不具合を修正END
           MOVE    SPA-HOSPNUM        TO   SYS-1005-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           PERFORM   820-SYSKANRI-DBSELECT-SEC
           MOVE   1        TO     IDX1
           PERFORM  UNTIL  FLG-SYSKANRI3  =  1
             IF   FLG-SYSKANRI3   =   ZERO
               MOVE SYS-1005-KBNCD         TO   SUM-SRYKA(IDX1)
               MOVE SYS-1005-SRYKANAME1    TO   SUM-SRYKANAME(IDX1)
               ADD    1             TO   IDX1
             END-IF
             PERFORM   820-SYSKANRI-DBFETCH-SEC
           END-PERFORM
           SUBTRACT   1       FROM   IDX1
           MOVE    IDX1             TO   SUM-SRYKAMAX
      *=== sarge対応 ADD By Takashima(2006/09/17) start
           PERFORM  820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/09/17) end
      *
           MOVE    "S137TP"                  TO   S137TP-FILE-ID
           MOVE    S100LNK-PRTKANRI-TERMID   TO   S137TP-TERMID
      ***07/09/20 nagaya 4.0対応 start
           MOVE    WRK-PARA-HOSPNUM          TO   S137TP-HOSPNUM
      **
      *     MOVE    "RECEERR"           TO  RECEERR-FILE-ID
      *     MOVE    S100LNK-PRTKANRI-TERMID     TO  RECEERR-TERMID
      ***07/09/20 nagaya 4.0対応 end
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    主　　処理
      ******************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    データ集計
           PERFORM  2001-SYUKEI-SEC
      *
      *    印刷処理
           IF   CNT-DATA    >    ZERO
             PERFORM  2002-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      ******************************************************************
      *    データ抽出処理
      ******************************************************************
       2001-SYUKEI-SEC                SECTION.
      *
           OPEN     OUTPUT   S137TMP-FILE
           CLOSE             S137TMP-FILE
           OPEN     I-O      S137TMP-FILE
      *--- 2009/06/26 ADD START ----------------------------------------
      *    自費データ集計
           IF  WRK-PARA-NYUGAIKBN  =  2
             OR  WRK-PARA-NYUGAIKBN  =  SPACE
             PERFORM  2001-JIHI-SYUKEI-SEC
           END-IF
      *--- 2009/06/26 ADD END   ----------------------------------------
      *    社保データ集計
           PERFORM  2001-SYAHO-SYUKEI-SEC
      *
           CLOSE   S137TMP-FILE
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD START ----------------------------------------
      *****************************************************************
      *    自費データ抽出処理１
      *****************************************************************
       2001-JIHI-SYUKEI-SEC                SECTION.
      *
           INITIALIZE            FLG-FIRST
                                 SYUNOU-REC
                                 WRK-JIHI-HKNJANUM
           MOVE    2        TO   WRK-NYUGAIKBN
           PERFORM  920-DBSELECT3-SEC
             PERFORM  UNTIL   FLG-JYURRK   =   1
      *    データ集計
             PERFORM  2001-JIHI-G-SEC
             PERFORM  920-DBFETCH3-SEC
           END-PERFORM
      *
           PERFORM  920-DBCLOSE3-SEC
      *
           IF       WRK-JIHI       NOT =  0
             OR     WRK-TGMONEY    NOT =  0
             PERFORM  2001-KASAN-JIHI-G-SEC
           END-IF
      *
           .
       2001-JIHI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費データ抽出処理２
      *****************************************************************
        2001-JIHI-G-SEC                SECTION.
      *
           PERFORM  910-PTHKNINF-READ-SEC
      *    自費集計処理
           IF     FLG-FIRST   =   1
             IF    SYU-SYUHKNNUM  NOT =  "039" AND "040" AND "060" AND
                                         "067" AND "068" AND "069" 
               IF  SYU-SYUHKNNUM(1:2)  NOT =  "97"  AND  "98"
               PERFORM  2001-KASAN-JIHI-G-SEC
               MOVE   0   TO   WRK-JIHI
                               WRK-TGMONEY
               END-IF
             END-IF
           END-IF
      *
      *    収納情報よりデータ抽出
           PERFORM  940-SYUNOU-READ-SEC
           PERFORM  910-PTHKNINF-READ-SEC
           IF    SYU-SYUHKNNUM  NOT =  "039" AND "040" AND "060" AND
                                       "067" AND "068" AND "069" 
             IF  SYU-SYUHKNNUM(1:2)  NOT =  "97"  AND  "98"
               IF       SYU-TOTAL-HKNTEN            =  0
                 AND    SYU-TOTAL-MONEY             =  0
                 AND  ( SYU-JIHI-TOTAL         NOT  =  0
                 OR     SYU-JIHI-TOTAL-TAX     NOT  =  0 
                 OR     SYU-TOTAL-TGMONEY      NOT  =  0
                 OR     SYU-TOTAL-TGMONEY-TAX  NOT  =  0 )
                 COMPUTE  WRK-JIHI     =  WRK-JIHI
                                       +  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
                 COMPUTE  WRK-TGMONEY  =  WRK-TGMONEY
                                       +  SYU-TOTAL-TGMONEY
                                       +  SYU-TOTAL-TGMONEY-TAX
               END-IF
             END-IF
           END-IF
      *
           MOVE  1               TO  FLG-FIRST
           MOVE  PTHKN-HKNJANUM  TO  WRK-JIHI-HKNJANUM
      *
           .
        2001-JIHI-G-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費集計処理１
      *****************************************************************
        2001-KASAN-JIHI-G-SEC               SECTION.
      *
           MOVE     SYU-PTID                TO   WRK-PTID
           PERFORM  900-PTINF-READ-SEC
           PERFORM  930-HKNCOMBI-SET-SEC
      *
      *    保険算定年齢チェック等
           INITIALIZE                       ORCSAGECHKAREA
           MOVE     SYU-HOSPNUM             TO  AGECHK-HOSPNUM
           MOVE     SYU-PTID                TO  AGECHK-PTID
           MOVE     S100LNK-YM              TO  AGECHK-SRYYMD(1:6)
           MOVE     "01"                    TO  AGECHK-SRYYMD(7:2)
           MOVE     COMB-HKNCOMBINUM        TO  AGECHK-HKNCOMBINUM
           CALL     "ORCSAGECHK"            USING
                                            ORCSAGECHKAREA
                                            SPA-AREA
      *
           MOVE     PTINF-BIRTHDAY(1:4)     TO    WRK-YY1
           MOVE     S100LNK-YM(1:4)         TO    WRK-YY2
           COMPUTE  WRK-NENREI   =   WRK-YY2   -   WRK-YY1
           IF  PTINF-BIRTHDAY(5:2)  >  S100LNK-YM(5:2)
               SUBTRACT   1   FROM   WRK-NENREI
           END-IF
      *
      *    キー設定
           INITIALIZE    S137TMP-REC
      *
           MOVE     SYU-NYUGAIKBN       TO    S137TMP-NYUGAIKBN
      *
           IF    SYS-1001-HOSPSBT1  =  1 
             OR  SYS-2009-SYUKA-KBN = 1
             MOVE    SYU-SRYKA          TO    S137TMP-SRYKA
           ELSE
             MOVE    "00"               TO    S137TMP-SRYKA
           END-IF
      *
           MOVE    "9"       TO   S137TMP-SRYYMKBN
      *    TMPファイル読み込み
           MOVE    1         TO   FLG-TYPE
           PERFORM  800-SKTTMP-READ-SEC
      *
      *    公費設定
           MOVE   SYU-KOH1HKNNUM   TO   WRK-JIHI-HKNNUM(1)
           MOVE   SYU-KOH2HKNNUM   TO   WRK-JIHI-HKNNUM(2)
           MOVE   SYU-KOH3HKNNUM   TO   WRK-JIHI-HKNNUM(3)
           MOVE   SYU-KOH4HKNNUM   TO   WRK-JIHI-HKNNUM(4)
      *
      *    明細集計場所設定
           PERFORM  2001-JIHI-IDX-SET-SEC
      *    社保集計
           PERFORM  2001-JIHI-DATA-SET-SEC
      *    公費集計
           IF    SYU-KOH1HKNNUM  NOT =  SPACE
               AND  SYU-SYUHKNNUM  NOT =  SPACE
             PERFORM  2001-JIHI-K-DATA-SET-SEC
           END-IF
      *    公費単独集計
           IF    SYU-KOH1HKNNUM  NOT =  SPACE
             AND SYU-SYUHKNNUM   =   SPACE
             PERFORM  2001-JIHI-KT-DATA-SET-SEC
           END-IF
      *
      *    書き出し
           IF  FLG-S137TMP   =  1
             ADD      1             TO   CNT-DATA
             WRITE    S137TMP-REC
           ELSE
             REWRITE  S137TMP-REC
           END-IF
      *
           .
        2001-KASAN-JIHI-G-EXT.
           EXIT.
      *****************************************************************
      *    自費集計場所設定
      *****************************************************************
       2001-JIHI-IDX-SET-SEC                SECTION.
      *
      *    小計場所設定
           IF   WRK-NENREI   >=   70
             IF  SYU-PTFTNRATE   =   10
               MOVE   9            TO   IDX2
             ELSE
               EVALUATE PTHKN-HOJOKBN
                 WHEN "A"
                 WHEN "B"
                 WHEN "C"
                 WHEN "9"
                   MOVE    9        TO   IDX2
                 WHEN OTHER
                   MOVE    18       TO   IDX2
               END-EVALUATE
             END-IF
           ELSE
             IF  AGECHK-SYUGAKUKBN  NOT =  1
               IF   COMB-HONKZKKBN   =   "1"
                 MOVE    27      TO    IDX2
               ELSE
                 MOVE    36      TO    IDX2
               END-IF
             ELSE
               MOVE      45      TO    IDX2
             END-IF
           END-IF
      *    保険集計場所設定
           MOVE    46              TO    IDX3
      *
      *    明細集計場所設定
           IF    SYU-KOH1HKNNUM  NOT =  SPACE
             EVALUATE  IDX2
               WHEN 9
                 MOVE     1          TO    IDX1
               WHEN 18
                 MOVE     10         TO    IDX1
               WHEN 27
                 MOVE     19         TO    IDX1
               WHEN 36
                 MOVE     28         TO    IDX1
               WHEN 45
                 MOVE     37         TO    IDX1
               WHEN 49
                 MOVE     47         TO    IDX1
               WHEN 52
                 MOVE     50         TO    IDX1
             END-EVALUATE
           ELSE
             EVALUATE  SYU-SYUHKNNUM
               WHEN "001"
               WHEN "009"
                 COMPUTE   IDX1  =  IDX2  -  7
               WHEN "002"
                 COMPUTE   IDX1  =  IDX2  -  6
               WHEN "003"
                 COMPUTE   IDX1  =  IDX2  -  5
               WHEN "004"
                 COMPUTE   IDX1  =  IDX2  -  4
               WHEN "031" THRU "034"
                 COMPUTE   IDX1  =  IDX2  -  3
               WHEN "006"
                 COMPUTE   IDX1  =  IDX2  -  2
               WHEN OTHER
                 COMPUTE   IDX1  =  IDX2  -  1
             END-EVALUATE
           END-IF
      *
           .
       2001-JIHI-IDX-SET-EXT.
           EXIT.
      *****************************************************************
      *    自費集計処理２
      *****************************************************************
       2001-JIHI-DATA-SET-SEC                SECTION.
      *
      *    公費単独時は集計しない
           IF  SYU-SYUHKNNUM   NOT =   SPACE
      *
      *    自費・保険外のみの場合、件数・日数は集計しない
      *    窓口請求額加算
             ADD     WRK-TGMONEY      TO     S137TMP-MADOGK(IDX1)
                                             S137TMP-MADOGK(IDX2)
                                             S137TMP-MADOGK(IDX3)
                                             S137TMP-GMADOGK
      *
      *    自費金額加算
             ADD     WRK-JIHI         TO     S137TMP-JIHIGK(IDX1)
                                             S137TMP-JIHIGK(IDX2)
                                             S137TMP-JIHIGK(IDX3)
                                             S137TMP-GJIHIGK
      *
      *    医療収益
             ADD     WRK-JIHI         TO     S137TMP-SYUEKI(IDX1)
                                             S137TMP-SYUEKI(IDX2)
                                             S137TMP-SYUEKI(IDX3)
                                             S137TMP-GSYUEKI
             ADD     WRK-TGMONEY      TO     S137TMP-SYUEKI(IDX1)
                                             S137TMP-SYUEKI(IDX2)
                                             S137TMP-SYUEKI(IDX3)
                                             S137TMP-GSYUEKI
           END-IF
      *
           .
       2001-JIHI-DATA-SET-EXT.
           EXIT.
      ******************************************************************
      *    公費集計処理
      ******************************************************************
       2001-JIHI-K-DATA-SET-SEC                SECTION.
      *
           PERFORM VARYING IDX5   FROM 1   BY 1
                   UNTIL   IDX5  >  4
                   OR  WRK-JIHI-TBL(IDX5)  =  SPACE
      *
      *    公費判別
             PERFORM  VARYING IDX4   FROM  1   BY  1
                      UNTIL   IDX4   >  50
                      OR   S137TMP-KOH-KEY(IDX4)  =  SPACE
               IF   WRK-JIHI-TBL(IDX5)   =   S137TMP-KOHNUM(IDX4)
                 MOVE    IDX4       TO   CNT-KOH
                 MOVE    50         TO   IDX4
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
             IF   IDX4   <   50
               MOVE    IDX4         TO   CNT-KOH
             END-IF
      *
      *    公費名
             MOVE  WRK-JIHI-TBL(IDX5)  TO   S137TMP-KOHNUM(CNT-KOH)
      *    自費・保険外のみの場合、件数・日数は集計しない
      *
           END-PERFORM
           .
       2001-JIHI-K-DATA-SET-EXT.
           EXIT.
      ******************************************************************
      *    公費単独集計処理
      ******************************************************************
       2001-JIHI-KT-DATA-SET-SEC           SECTION.
      *
      *    公費判別
           PERFORM  VARYING IDX4   FROM 1   BY 1
                    UNTIL   IDX4  >  10
                    OR   S137TMP-KOHTAN-KEY(IDX4)  =  SPACE
             IF  SYU-KOH1HKNNUM  =  S137TMP-KOHTNUM(IDX4)
                 AND   SYU-KOH2HKNNUM  =  S137TMP-KOHTNUM2(IDX4)
               MOVE   IDX4      TO   CNT-KOH
               MOVE   11        TO   IDX4
             END-IF
           END-PERFORM
      *
      *    該当公費がなかった場合
           IF   IDX4   <  11
             MOVE   IDX4      TO   CNT-KOH
           END-IF
      *
      *    公費名
           MOVE  SYU-KOH1HKNNUM           TO   S137TMP-KOHTNUM(CNT-KOH)
           MOVE  SYU-KOH1HKNNUM           TO   WRK-KOHTANTMP-KOHNUM
      *    併用公費名
           MOVE  SYU-KOH2HKNNUM           TO   S137TMP-KOHTNUM2(CNT-KOH)
           MOVE  SYU-KOH2HKNNUM           TO   WRK-KOHTANTMP-KOHNUM2
      *    自費・保険外のみの場合、件数・日数は集計しない
      *
      *    窓口請求額
           ADD   WRK-TGMONEY              TO   S137TMP-KTMADOGK(CNT-KOH)
           ADD   WRK-TGMONEY              TO   S137TMP-GMADOGK
      *    自費金額
           ADD   WRK-JIHI                 TO   S137TMP-KTJIHIGK(CNT-KOH)
           ADD   WRK-JIHI                 TO   S137TMP-GJIHIGK
      *
      *    医療収益
           ADD   WRK-JIHI                 TO   S137TMP-KTSYUEKI(CNT-KOH)
           ADD   WRK-JIHI                 TO   S137TMP-GSYUEKI
           ADD   WRK-TGMONEY              TO   S137TMP-KTSYUEKI(CNT-KOH)
           ADD   WRK-TGMONEY              TO   S137TMP-GSYUEKI
           .
       2001-JIHI-KT-DATA-SET-EXT.
           EXIT.
      *--- 2009/06/26 ADD END   ----------------------------------------
      ******************************************************************
      *    社保データ抽出処理
      ******************************************************************
       2001-SYAHO-SYUKEI-SEC                SECTION.
      *
      ***07/09/20 nagaya 4.0対応 start
      *     INITIALIZE        WRK-PATH
      *     MOVE    "SEIKYU-KEY3"  TO    WRK-PATH
      ***07/09/20 nagaya 4.0対応 start
           PERFORM  900-SEIKYU-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-END   =   1
      *
      *    集計場所設定
             PERFORM   2001-IDX-SET1-SEC
      *
      *    データ集計
             PERFORM   2001-DATA-SET1-SEC
      *
             PERFORM   900-SEIKYU-READ-SEC
           END-PERFORM
      *
      *
           PERFORM   900-SEIKYU-CLOSE-SEC
      *
           .
       2001-SYAHO-SYUKEI-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保集計場所設定
      ******************************************************************
       2001-IDX-SET1-SEC                SECTION.
      *
      *    小計場所設定
           IF   RECE10-RJNHKNNUM  NOT =  SPACE
             IF   RECE10-KYURATE   =   9
               MOVE    49            TO    IDX2
             ELSE
               MOVE    52            TO    IDX2
             END-IF
      *    保険集計場所設定
             MOVE    53              TO    IDX3
           ELSE
             IF   RECE10-AGE   >=   70
      ***** 04/05/18 UPD
      *         IF  RECE10-AGE    =    9
               IF  RECE10-KYURATE   =   9
      *****
                 MOVE   9            TO   IDX2
               ELSE
      **20070831 nagaya 船員保険判定修正 start
      *           MOVE    18  TO    IDX2
                  EVALUATE RECE10-HOJOKBN-KEY
                    WHEN "A"
                    WHEN "B"
                    WHEN "C"
                      MOVE    9        TO   IDX2
                    WHEN OTHER
                      MOVE    18       TO   IDX2
                  END-EVALUATE
      **20070831 nagaya 船員保険判定修正 end
               END-IF
             ELSE
      * 20080423 H20年4月改正対応START tanaka
               IF S100LNK-YM   <   "200804"
      * 20080423 H20年4月改正対応END
                 IF   RECE10-AGE   >=   3
                   IF   RECE10-HONKZKKBN   =  "1"
                     MOVE    27      TO    IDX2
                   ELSE
                     MOVE    36      TO    IDX2
                   END-IF
                 ELSE
                   MOVE      45      TO    IDX2
                 END-IF
      * 20080423 H20年4月改正対応START tanaka
               ELSE
                 IF  RECE10-FLG(1)  NOT =  1
                   IF   RECE10-HONKZKKBN   =   "1"
                     MOVE    27      TO    IDX2
                   ELSE
                     MOVE    36      TO    IDX2
                   END-IF
                 ELSE
                   MOVE      45      TO    IDX2
                 END-IF
               END-IF
      * 20080423 H20年4月改正対応END
             END-IF
      *    保険集計場所設定
             MOVE    46              TO    IDX3
           END-IF
      *
      *    明細集計場所設定
           IF    RECE10-KOHNUM(1)  NOT =  SPACE
             EVALUATE  IDX2
               WHEN 9
                 MOVE     1          TO    IDX1
               WHEN 18
                 MOVE     10         TO    IDX1
               WHEN 27
                 MOVE     19         TO    IDX1
               WHEN 36
                 MOVE     28         TO    IDX1
               WHEN 45
                 MOVE     37         TO    IDX1
               WHEN 49
                 MOVE     47         TO    IDX1
               WHEN 52
                 MOVE     50         TO    IDX1
             END-EVALUATE
           ELSE
             IF   RECE10-RJNHKNNUM   NOT =   SPACE
               IF   RECE10-KYURATE   =   9
                 MOVE    48          TO    IDX1
               ELSE
                 MOVE    51          TO    IDX1
               END-IF
             ELSE
               EVALUATE  RECE10-HKNNUM
                 WHEN "001"
      * 20080929 H20年10月改正対応START
                 WHEN "009"
      * 20080929 H20年10月改正対応END
                   COMPUTE   IDX1  =  IDX2  -  7
                 WHEN "002"
                   COMPUTE   IDX1  =  IDX2  -  6
                 WHEN "003"
                   COMPUTE   IDX1  =  IDX2  -  5
                 WHEN "004"
                   COMPUTE   IDX1  =  IDX2  -  4
                 WHEN "031"  THRU "034"
                   COMPUTE   IDX1  =  IDX2  -  3
                 WHEN "006"
                   COMPUTE   IDX1  =  IDX2  -  2
                 WHEN OTHER
                   COMPUTE   IDX1  =  IDX2  -  1
               END-EVALUATE
             END-IF
           END-IF
           .
       2001-IDX-SET1-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保データ集計処理
      ******************************************************************
       2001-DATA-SET1-SEC                SECTION.
      *
      *    キー設定
           INITIALIZE    S137TMP-REC
      *
           MOVE    RECE10-NYUGAIKBN    TO   S137TMP-NYUGAIKBN
           MOVE    RECE10-RECEKA       TO   S137TMP-SRYKA
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     MOVE    RECE10-SRYYM       TO    S137TMP-SRYYM
           IF   RECE10-SRYYM  <  "200804"
             MOVE    "0"       TO   S137TMP-SRYYMKBN
           ELSE
             MOVE    "9"       TO   S137TMP-SRYYMKBN
           END-IF
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *    TMPファイル読み込み
           MOVE    1           TO    FLG-TYPE
           PERFORM 800-SKTTMP-READ-SEC
      *
      ***  20040712  @@@@-MOTOHASI  公費単独時は集計しない
           IF  RECE10-HKNNUM   NOT =   SPACE
      ***  20040712  @@@@-MOTOHASI  ここまで
      *
      *    加算データ設定
             PERFORM  2001-KASAN-SEC
      *
      *    件数加算
             ADD    1                TO    S137TMP-KENSU(IDX1)
                                           S137TMP-KENSU(IDX2)
                                           S137TMP-KENSU(IDX3)
                                           S137TMP-GKENSU
      *
      *    日数加算
             ADD    WRK-NISSU        TO    S137TMP-NISSU(IDX1)
                                           S137TMP-NISSU(IDX2)
                                           S137TMP-NISSU(IDX3)
                                           S137TMP-GNISSU
      *
      *    点数加算
             ADD    WRK-TENSU        TO    S137TMP-TENSU(IDX1)
                                           S137TMP-TENSU(IDX2)
                                           S137TMP-TENSU(IDX3)
                                           S137TMP-GTENSU
      *
      *    窓口請求額加算
             ADD    WRK-MADOGK       TO    S137TMP-MADOGK(IDX1)
                                           S137TMP-MADOGK(IDX2)
                                           S137TMP-MADOGK(IDX3)
                                           S137TMP-GMADOGK
      *
      *--- 2008/12/18 ADD START ----------------------------------------
      *    自費金額加算
             ADD    WRK-JIHIGK       TO    S137TMP-JIHIGK(IDX1)
                                           S137TMP-JIHIGK(IDX2)
                                           S137TMP-JIHIGK(IDX3)
                                           S137TMP-GJIHIGK
      *--- 2008/12/18 ADD END ------------------------------------------
      *
      *    保険請求額加算
             ADD    WRK-HKNGK        TO    S137TMP-HKNGK(IDX1)
                                           S137TMP-HKNGK(IDX2)
                                           S137TMP-HKNGK(IDX3)
                                           S137TMP-GHKNGK
      *
      *    公費請求額
             ADD    WRK-KOHGK        TO    S137TMP-KOHGK(IDX1)
                                           S137TMP-KOHGK(IDX2)
                                           S137TMP-KOHGK(IDX3)
                                           S137TMP-GKOHGK
      *
      *    医療収益
             ADD    WRK-SYUEKI       TO    S137TMP-SYUEKI(IDX1)
                                           S137TMP-SYUEKI(IDX2)
                                           S137TMP-SYUEKI(IDX3)
                                           S137TMP-GSYUEKI
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *
      *    日数加算
             ADD    WRK-SNISSU       TO    S137TMP-SNISSU(IDX1)
                                           S137TMP-SNISSU(IDX2)
                                           S137TMP-SNISSU(IDX3)
                                           S137TMP-SGNISSU
      *    請求額加算
             ADD    WRK-SSEIKYU      TO    S137TMP-SSEIKYU(IDX1)
                                           S137TMP-SSEIKYU(IDX2)
                                           S137TMP-SSEIKYU(IDX3)
                                           S137TMP-SGSEIKYU
      *    標準負担額加算
             ADD    WRK-SFTNGK       TO    S137TMP-SFTNGK(IDX1)
                                           S137TMP-SFTNGK(IDX2)
                                           S137TMP-SFTNGK(IDX3)
                                           S137TMP-SGFTNGK
      ***  20040820  @@@@-MOTOHASI
      *
      *
      ***  20040712  @@@@-MOTOHASI  公費単独時は集計しない
           END-IF
      ***  20040712  @@@@-MOTOHASI  ここまで
      *
      *    公費集計処理(公費単独時は集計しない)
           IF    RECE10-KOHNUM(1)  NOT =  SPACE
      *
      ***  20040712  @@@@-MOTOHASI  公費単独時も集計するようにする
      ***  20040819  @@@@-MOTOHASI  公費単独時は集計しない
               AND  RECE10-HKNNUM  NOT =  SPACE
             PERFORM   2001-DATA-SET11-SEC
           END-IF
      *
      *
      ***  20040819  @@@@-MOTOHASI  公費単独時の集計
           IF    RECE10-KOHNUM(1)  NOT =  SPACE
               AND RECE10-HKNNUM   =   SPACE
             PERFORM  2001-DATA-SET21-SEC
           END-IF
      ***  20040819  @@@@-MOTOHASI  ここまで
      *
      *    書き出し
      *    公費単独の場合は社保のデータは更新しない
      ***  20040712  @@@@-MOTOHASI  公費単独時も更新するようにする
      ***     IF     RECE10-HKNNUM NOT = SPACE
           IF   FLG-S137TMP   =   1
             ADD    1           TO   CNT-DATA
             WRITE     S137TMP-REC
           ELSE
             REWRITE   S137TMP-REC
           END-IF
      ***  20040712  @@@@-MOTOHASI  公費単独時も更新するようにする
      ***     END-IF
      *
      *    保険合計用TMPファイルに加算
      *     PERFORM  2001-DATA-SET3-SEC
      *
           .
       2001-DATA-SET1-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保公費集計処理
      ******************************************************************
       2001-DATA-SET11-SEC                SECTION.
      *
           PERFORM VARYING IDX5   FROM 1   BY 1
                   UNTIL   IDX5  >  4
                   OR  RECE10-KOHNUM(IDX5)  =  SPACE
      *
      *    公費判別
             PERFORM  VARYING IDX4   FROM  1   BY  1
                      UNTIL   IDX4   >  50
                      OR   S137TMP-KOH-KEY(IDX4)  =  SPACE
               IF   RECE10-KOHNUM(IDX5)   =   S137TMP-KOHNUM(IDX4)
                   AND   RECE10-RJNHKNNUM   =   S137TMP-RJNKBN(IDX4)
                 MOVE    IDX4       TO   CNT-KOH
                 MOVE    50         TO   IDX4
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
             IF   IDX4   <   50
               MOVE    IDX4         TO   CNT-KOH
             END-IF
      *
      *    老人併用区分設定
             MOVE  RECE10-RJNHKNNUM     TO   S137TMP-RJNKBN(CNT-KOH)
      *    公費名
             MOVE  RECE10-KOHNUM(IDX5)  TO   S137TMP-KOHNUM(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  S137TMP-KKENSU(CNT-KOH)
      *    療養の給付:診療実日数
             ADD   RECE10-JNISSU(IDX5 + 1)   TO  S137TMP-KNISSU(CNT-KOH)
      *    療養の給付：点数
             ADD   RECE10-TOTALTEN(IDX5 + 1) TO  S137TMP-KTENSU(CNT-KOH)
      *
             ADD   1                         TO   WRK-KKENSU
             ADD   RECE10-JNISSU(IDX5 + 1)   TO   WRK-KNISSU
             ADD   RECE10-TOTALTEN(IDX5 + 1) TO   WRK-KTENSU
      *
           END-PERFORM
           .
       2001-DATA-SET11-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保公費単独集計処理
      ******************************************************************
       2001-DATA-SET21-SEC                SECTION.
      *
           MOVE   SPACE    TO   WRK-FUKUKOHNUM
      * 20071114 公費単独の不具合を修正START t.tanaka
           MOVE   ZERO     TO   WRK-FUKUKOHTEN
           MOVE   ZERO     TO   WRK-FUKUKOHNISSU
      * 20071114 公費単独の不具合を修正END
      * 20080909 公費単独欄の食事療養の不具合を修正START
           MOVE   ZERO     TO   WRK-FUKU-S-KAISU
           MOVE   ZERO     TO   WRK-FUKU-S-SKYMONEY
           MOVE   ZERO     TO   WRK-FUKU-S-FTNMONEY
      * 20080909 公費単独欄の食事療養の不具合を修正END
      *    他の公費を併用しているか判定
           IF  RECE10-KOHNUM(2)  NOT =  SPACE
               OR  RECE10-KOHNUM(3)  NOT =  SPACE
               OR  RECE10-KOHNUM(4)  NOT =  SPACE
      *    他の公費を併用している場合
             PERFORM   VARYING IDX4   FROM 2   BY 1
                       UNTIL   IDX4   >  4
               IF  RECE10-KOHNUM(IDX4)  NOT =  SPACE
      *    診療点数の比較
                 IF  RECE10-TOTALTEN(2)  >  RECE10-TOTALTEN(IDX4 + 1)
                   MOVE   1       TO   IDX5
                   MOVE   RECE10-KOHNUM(IDX4)      TO   WRK-FUKUKOHNUM
      * 20071114 公費単独の不具合を修正START t.tanaka
                   MOVE   RECE10-TOTALTEN(IDX4 + 1)  TO   WRK-FUKUKOHTEN
                   MOVE   RECE10-JNISSU(IDX4 + 1)  TO   WRK-FUKUKOHNISSU
      * 20071114 公費単独の不具合を修正END
      * 20080909 公費単独欄の食事療養の不具合を修正START
                   MOVE   RECE10-SHOKUJINISSU(IDX4 + 1)
                                              TO   WRK-FUKU-S-KAISU
                   MOVE   RECE10-SHOKUJIRYOYOHI(IDX4 + 1)
                                              TO   WRK-FUKU-S-SKYMONEY
                   MOVE   RECE10-SHOKUJIFTN(IDX4 + 1)
                                              TO   WRK-FUKU-S-FTNMONEY
      * 20080909 公費単独欄の食事療養の不具合を修正END
                 ELSE
                   IF  RECE10-TOTALTEN(2)  <  RECE10-TOTALTEN(IDX4 + 1)
                     MOVE   IDX4              TO   IDX5
                     MOVE   RECE10-KOHNUM(1)  TO   WRK-FUKUKOHNUM
      * 20071114 公費単独の不具合を修正START t.tanaka
                     MOVE   RECE10-TOTALTEN(2)  TO   WRK-FUKUKOHTEN
                     MOVE   RECE10-JNISSU(2)  TO   WRK-FUKUKOHNISSU
      * 20071114 公費単独の不具合を修正END
      * 20080909 公費単独欄の食事療養の不具合を修正START
                     MOVE   RECE10-SHOKUJINISSU(2)
                                            TO   WRK-FUKU-S-KAISU
                     MOVE   RECE10-SHOKUJIRYOYOHI(2)
                                            TO   WRK-FUKU-S-SKYMONEY
                     MOVE   RECE10-SHOKUJIFTN(2)
                                            TO   WRK-FUKU-S-FTNMONEY
      * 20080909 公費単独欄の食事療養の不具合を修正END
                   ELSE
      *    診療点数が等しい場合、012、013、014、018、053、を優先とする
                     IF  RECE10-KOHNUM(2) = "012" OR "013" OR "014"
                                            OR "018" OR "053"
                       MOVE   2       TO   IDX5
                       MOVE   RECE10-KOHNUM(1)  TO   WRK-FUKUKOHNUM
      * 20071114 公費単独の不具合を修正START t.tanaka
                       MOVE   RECE10-TOTALTEN(IDX4 + 1)
                                            TO   WRK-FUKUKOHTEN
                       MOVE   RECE10-JNISSU(IDX4 + 1)
                                            TO   WRK-FUKUKOHNISSU
      * 20080909 公費単独欄の食事療養の不具合を修正START
                       MOVE   RECE10-SHOKUJINISSU(IDX4 + 1)
                                            TO   WRK-FUKU-S-KAISU
                       MOVE   RECE10-SHOKUJIRYOYOHI(IDX4 + 1)
                                            TO   WRK-FUKU-S-SKYMONEY
                       MOVE   RECE10-SHOKUJIFTN(IDX4 + 1)
                                            TO   WRK-FUKU-S-FTNMONEY
      * 20080909 公費単独欄の食事療養の不具合を修正END
      * 20071114 公費単独の不具合を修正END
                     ELSE
                       MOVE    1        TO   IDX5
                       MOVE   RECE10-KOHNUM(2)    TO   WRK-FUKUKOHNUM
      * 20071114 公費単独の不具合を修正START t.tanaka
                       MOVE   RECE10-TOTALTEN(2)  TO   WRK-FUKUKOHTEN
                       MOVE   RECE10-JNISSU(2)    TO   WRK-FUKUKOHNISSU
      * 20080909 公費単独欄の食事療養の不具合を修正START
                       MOVE   RECE10-SHOKUJINISSU(2)
                                            TO   WRK-FUKU-S-KAISU
                       MOVE   RECE10-SHOKUJIRYOYOHI(2)
                                            TO   WRK-FUKU-S-SKYMONEY
                       MOVE   RECE10-SHOKUJIFTN(2)
                                            TO   WRK-FUKU-S-FTNMONEY
      * 20080909 公費単独欄の食事療養の不具合を修正END
      * 20071114 公費単独の不具合を修正END
                     END-IF
                   END-IF
                 END-IF
                 MOVE   4      TO   IDX4
               END-IF
             END-PERFORM
           ELSE
             MOVE    1       TO   IDX5
           END-IF
      *
      *     PERFORM VARYING IDX5     FROM    1   BY  1
      *             UNTIL   IDX5     >       4
      *             OR  RECE10-KOHNUM (IDX5) =   SPACE
      *
      *    公費判別
           PERFORM  VARYING IDX4   FROM 1   BY 1
                    UNTIL   IDX4  >  10
                    OR   S137TMP-KOHTAN-KEY(IDX4)  =  SPACE
             IF  RECE10-KOHNUM(IDX5)  =  S137TMP-KOHTNUM(IDX4)
                 AND   WRK-FUKUKOHNUM  =  S137TMP-KOHTNUM2(IDX4)
               MOVE   IDX4      TO   CNT-KOH
      *--- 2009/01/08 CHANGE START -------------------------------------
      *         MOVE   10        TO   IDX4
               MOVE   11        TO   IDX4
      *--- 2009/01/08 ADD END ------------------------------------------
             END-IF
           END-PERFORM
      *
      *    該当公費がなかった場合
      *--- 2009/01/08 CHANGE START -------------------------------------
      *     IF   IDX4   <  10
           IF   IDX4   <  11
      *--- 2009/01/08 ADD END ------------------------------------------
             MOVE   IDX4      TO   CNT-KOH
           END-IF
      *    公費名
           MOVE  RECE10-KOHNUM(IDX5)      TO   S137TMP-KOHTNUM(CNT-KOH)
      *    併用公費名
           MOVE  WRK-FUKUKOHNUM           TO   S137TMP-KOHTNUM2(CNT-KOH)
      *    療養の給付：件数
           ADD   1                        TO   S137TMP-KTKENSU(CNT-KOH)
           ADD   1                        TO   S137TMP-GKENSU
      *    療養の給付:診療実日数
           ADD  RECE10-JNISSU(IDX5 + 1)   TO   S137TMP-KTNISSU(CNT-KOH)
      * 20071114 公費単独の不具合を修正START t.tanaka
           ADD  WRK-FUKUKOHNISSU          TO   S137TMP-KTNISSU(CNT-KOH)
      * 20071114 公費単独の不具合を修正END
           ADD  RECE10-JNISSU(IDX5 + 1)   TO   S137TMP-GNISSU
      * 20071114 公費単独の不具合を修正START t.tanaka
           ADD  WRK-FUKUKOHNISSU          TO   S137TMP-GNISSU
      * 20071114 公費単独の不具合を修正END
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(IDX5 + 1) TO   S137TMP-KTTENSU(CNT-KOH)
      * 20071114 公費単独の不具合を修正START t.tanaka
           ADD  WRK-FUKUKOHTEN            TO   S137TMP-KTTENSU(CNT-KOH)
      * 20071114 公費単独の不具合を修正END
           ADD  RECE10-TOTALTEN(IDX5 + 1) TO   S137TMP-GTENSU
      * 20071114 公費単独の不具合を修正START t.tanaka
           ADD  WRK-FUKUKOHTEN            TO   S137TMP-GTENSU
      * 20071114 公費単独の不具合を修正END
      *
      *    収納情報から窓口請求額、公費請求額を集計
           MOVE    ZERO                  TO   WRK-MADOSEIKYUGK-G
                                              WRK-KOHSEIKYUGK-G
      *--- 2008/12/23 ADD START ----------------------------------------
                                              WRK-JIHISEIKYUGK-G
      *--- 2008/12/23 ADD END ------------------------------------------
           PERFORM  920-SYUNOU-SET-SEC
           MOVE   WRK-MADOSEIKYUGK-G     TO   WRK-MADOGK
      *--- 2008/12/23 ADD START ----------------------------------------
           MOVE   WRK-JIHISEIKYUGK-G     TO   WRK-JIHIGK
      *--- 2008/12/23 ADD END ------------------------------------------
           MOVE   WRK-KOHSEIKYUGK-G      TO   WRK-KOHGK
      *    窓口請求額
           ADD    WRK-MADOGK             TO   S137TMP-KTMADOGK(CNT-KOH)
           ADD    WRK-MADOGK             TO   S137TMP-GMADOGK
      *--- 2008/12/23 ADD START ----------------------------------------
      *    自費金額
           ADD    WRK-JIHIGK             TO   S137TMP-KTJIHIGK(CNT-KOH)
           ADD    WRK-JIHIGK             TO   S137TMP-GJIHIGK
      *--- 2008/12/23 ADD END ------------------------------------------
      *    公費請求額
      * 20071114 公費単独の不具合を修正START t.tanaka
      *       COMPUTE  WRK-KOHGK  =  WRK-KOHGK  +
      *                             (RECE10-TOTALTEN(IDX5 + 1) * 10)
      *--- 2009/01/08 ADD START ----------------------------------------
           IF  RECE10-SHOKUJIFTN(IDX5 + 1)  NOT =  ZERO
      *    患者食事負担有り
             COMPUTE   WRK-KOHGK  =  ((WRK-FUKUKOHTEN
                                  +  RECE10-TOTALTEN(IDX5 + 1))  *  10)
                                  -  (WRK-MADOGK
                                  -  RECE10-SHOKUJIFTN(IDX5 + 1))
           ELSE
      *    患者食事負担無し
      *--- 2009/01/08 ADD END ------------------------------------------
              COMPUTE  WRK-KOHGK  =  ((WRK-FUKUKOHTEN
                               +  RECE10-TOTALTEN(IDX5 + 1)) * 10)
                               +  RECE10-SHOKUJIRYOYOHI(IDX5 + 1)
                               -  RECE10-FTNMONEY(IDX5 + 1)
      *--- 2009/01/08 ADD START ----------------------------------------
           END-IF
      *--- 2009/01/08 ADD END ------------------------------------------
      *
      * 20071114 公費単独の不具合を修正END
           ADD   WRK-KOHGK             TO   S137TMP-KTKOHGK(CNT-KOH)
           ADD   WRK-KOHGK             TO   S137TMP-GKOHGK
      *    保険請求額
      *      限度額が有る場合は(点数×１０) − 患者負担 で算出する
      * 20071113 正しく集計されない不具合を修正START t.tanaka
      *       IF    RECE10-FTNMONEY(IDX5 + 1)  =  ZERO
      *        COMPUTE  WRK-HKNGK  =  RECE10-TOTALTEN(IDX5 + 1)
      *                            * RECE10-KYURATE
      *       ELSE
      *         COMPUTE  WRK-HKNGK  =  (RECE10-TOTALTEN(IDX5 + 1) * 10)
      *                             -  RECE10-FTNMONEY(IDX5 + 1)
      *       END-IF
      *       ADD  WRK-HKNGK  TO  S137TMP-KTHKNGK(CNT-KOH)
      *       ADD  WRK-HKNGK  TO  S137TMP-GHKNGK
           MOVE   ZERO                 TO   WRK-HKNGK
      * 20071113 正しく集計されない不具合を修正END
      *--- 2008/12/23 CHANGE + ADD START -------------------------------
      *    食事療養費の患者負担分以外を集計
           MOVE  RECE10-SHOKUJIRYOYOHI(IDX5 + 1)  TO   WRK-SKJSKY
           ADD   WRK-FUKU-S-SKYMONEY              TO   WRK-SKJSKY
           MOVE  RECE10-SHOKUJIFTN(IDX5 + 1)      TO   WRK-SKJFTN
           ADD   WRK-FUKU-S-FTNMONEY              TO   WRK-SKJFTN
           COMPUTE   WRK-SKJSKYZAN  =  WRK-SKJSKY  -  WRK-SKJFTN
      *
      *    医療収益
      *     COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
      *                          +  WRK-KOHGK
      *--- 2009/01/08 ADD START ----------------------------------------
       DISPLAY "IDX5 + 1 =" IDX5
           IF  RECE10-SHOKUJIFTN(IDX5 + 1)  NOT =  ZERO
      *    患者食事負担無し
      *--- 2009/01/08 ADD END ------------------------------------------
             COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
                                  +  WRK-KOHGK  +  WRK-JIHIGK
                                  +  WRK-SKJSKYZAN
           ELSE
      *    患者食事負担有り
             COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
                                  +  WRK-KOHGK  +  WRK-JIHIGK
           END-IF
      *--- 2008/12/23 CHANGE + ADD END ---------------------------------
           ADD   WRK-SYUEKI            TO   S137TMP-KTSYUEKI(CNT-KOH)
           ADD   WRK-SYUEKI            TO   S137TMP-GSYUEKI
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *
      *    日数加算
           ADD  RECE10-SHOKUJINISSU(IDX5 + 1)
                                       TO   S137TMP-SKTNISSU(CNT-KOH)
           ADD   WRK-FUKU-S-KAISU      TO   S137TMP-SKTNISSU(CNT-KOH)
           ADD   RECE10-SHOKUJINISSU(IDX5 + 1)  TO   S137TMP-SGNISSU
           ADD   WRK-FUKU-S-KAISU      TO   S137TMP-SGNISSU
      *    請求額加算
           ADD   RECE10-SHOKUJIRYOYOHI(IDX5 + 1)
                                       TO   S137TMP-SKTSEIKYU(CNT-KOH)
           ADD   WRK-FUKU-S-SKYMONEY   TO   S137TMP-SKTSEIKYU(CNT-KOH)
           ADD   RECE10-SHOKUJIRYOYOHI(IDX5 + 1)  TO   S137TMP-SGSEIKYU
           ADD   WRK-FUKU-S-SKYMONEY   TO   S137TMP-SGSEIKYU
      *    標準負担額加算
           ADD   RECE10-SHOKUJIFTN(IDX5 + 1)
                                       TO   S137TMP-SKTFTNGK(CNT-KOH)
           ADD   WRK-FUKU-S-FTNMONEY   TO   S137TMP-SKTFTNGK(CNT-KOH)
           ADD   RECE10-SHOKUJIFTN(IDX5 + 1)  TO  S137TMP-SGFTNGK
           ADD   WRK-FUKU-S-FTNMONEY   TO   S137TMP-SGFTNGK
           .
       2001-DATA-SET21-EXT.
           EXIT.
      *
      ******************************************************************
      *    加算データ設定処理
      ******************************************************************
       2001-KASAN-SEC                SECTION.
      *
           INITIALIZE    WRK-KASAN-TBL
      *
           MOVE    RECE10-JNISSU(1)    TO   WRK-NISSU
           MOVE    RECE10-TOTALTEN(1)  TO   WRK-TENSU
      *
      *    限度額が有る場合は(点数×１０) − 患者負担 で算出する
           IF    RECE10-FTNMONEY(1)  =  ZERO
      ***  20050419  @@@@-MOTOHASI
               AND  RECE10-KYURATE  NOT =  ZERO
      ***  20050419  @@@@-MOTOHASI
             COMPUTE   WRK-HKNGK  =  RECE10-TOTALTEN(1)
                                  *  RECE10-KYURATE
           ELSE
             COMPUTE   WRK-HKNGK  =  (RECE10-TOTALTEN(1) * 10)
                                  -  RECE10-FTNMONEY(1)
           END-IF
      *
      *    収納情報から窓口請求額、公費請求額を集計
           MOVE    ZERO                  TO   WRK-MADOSEIKYUGK-G
                                              WRK-KOHSEIKYUGK-G
      *--- 2008/12/23 ADD START ----------------------------------------
           MOVE    ZERO                  TO   WRK-JIHISEIKYUGK-G
      *--- 2008/12/23 ADD END ------------------------------------------
           PERFORM  920-SYUNOU-SET-SEC
           MOVE    WRK-MADOSEIKYUGK-G    TO   WRK-MADOGK
      *--- 2008/12/23 ADD START ----------------------------------------
           MOVE    WRK-JIHISEIKYUGK-G    TO   WRK-JIHIGK
      *--- 2008/12/23 ADD END ------------------------------------------
           MOVE    WRK-KOHSEIKYUGK-G     TO   WRK-KOHGK
      *
      *    公費単独の場合の編集
           IF    RECE10-HKNNUM  =  SPACE
             MOVE    RECE10-JNISSU(2)    TO   WRK-NISSU
             MOVE    RECE10-TOTALTEN(2)  TO   WRK-TENSU
             COMPUTE  WRK-HKNGK  =  (RECE10-TOTALTEN(2) * 10)
                                 +  RECE10-SHOKUJIRYOYOHI(2)
             ADD     WRK-HKNGK           TO   WRK-KOHGK
             MOVE    ZERO                TO   WRK-HKNGK
           END-IF
      *
      *--- 2008/12/23 DELETE START -------------------------------------
      *    医療収益
      *     COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
      *                          +  WRK-KOHGK
      *--- 2008/12/23 DELETE END ---------------------------------------
      *
      ***  20040820  @@@@-MOTOHASI  食事療養追加
      *    日数
           IF   RECE10-HKNNUM  NOT =  SPACE
             MOVE  RECE10-SHOKUJINISSU(1)     TO   WRK-SNISSU
           END-IF
      *    請求額
           IF   RECE10-HKNNUM  NOT =  SPACE
             MOVE  RECE10-SHOKUJIRYOYOHI(1)   TO   WRK-SSEIKYU
           END-IF
      *    標準負担額
           IF   RECE10-HKNNUM  NOT =  SPACE
             MOVE  RECE10-SHOKUJIFTN(1)       TO   WRK-SFTNGK
           END-IF
      ***  20040820  @@@@-MOTOHASI
      *
      *--- 2008/12/23 ADD START ----------------------------------------
      *    食事療養費の患者負担分以外を集計
           COMPUTE  WRK-SKJSKYZAN   =   WRK-SSEIKYU   -   WRK-SFTNGK
      *
      *    医療収益
           COMPUTE  WRK-SYUEKI  =  WRK-HKNGK  +  WRK-MADOGK
                                +  WRK-KOHGK  +  WRK-JIHIGK
                                +  WRK-SKJSKYZAN
      *--- 2008/12/23 ADD END ------------------------------------------
           .
       2001-KASAN-EXT.
           EXIT.
      *
      ******************************************************************
      *    印刷編集処理
      ******************************************************************
       2002-PRINT-SEC                SECTION.
      *
      *    社保データ印刷処理
           PERFORM 2002-PRINT-TYPE1-SEC
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保データ印刷処理
      ******************************************************************
       2002-PRINT-TYPE1-SEC                SECTION.
      *
           OPEN    INPUT        S137TMP-FILE
           MOVE    ZERO          TO   FLG-PRINT
           INITIALIZE            S137TMP-REC
      *
           MOVE    1             TO   FLG-TYPE
           PERFORM   900-TMP-READ-SEC
      *
           PERFORM   UNTIL FLG-PRINT  =  1
      *
      *
      ***  20040820  @@@@-MOTOHASI  追加
             IF    WRK-PARA-NYUGAIKBN   =  "1"
                 OR   SPACE
               IF   S137TMP-NYUGAIKBN   =  "1"
      ***  20040820  @@@@-MOTOHASI
      *    入院フォーム
      *    １枚目
                 MOVE    1       TO   WRK-CNT1
                 MOVE    1       TO   WRK-CNT2
      *
      *    見出し編集
                 PERFORM  3102-HEAD-HEN-SEC
      *
      *    明細編集
                 PERFORM  3202-BODY-HEN-SEC
      *
      *    印刷処理
                 PERFORM  340-PRINT-OUT-SEC
      *
      *    ２枚目
                 MOVE     1         TO   WRK-CNT1
                 MOVE     1         TO   WRK-CNT2
      *
                 MOVE     ZERO      TO   FLG-PASS
                 PERFORM  UNTIL FLG-PASS   =   1
                          AND    S137TMP-KOH-KEY(WRK-CNT1)  =  SPACE
                   PERFORM  3103-HEAD-HEN-SEC
      *
      *    明細編集
                   PERFORM  3203-BODY-HEN-SEC
      *
      *    印刷処理
                   PERFORM  350-PRINT-OUT-SEC
                   MOVE     1       TO   FLG-PASS
                 END-PERFORM
               END-IF
             END-IF
      *
      *
      *    20040820  @@@@-MOTOHASI  追加
             IF   WRK-PARA-NYUGAIKBN   =  "2"
                 OR   SPACE
               IF    S137TMP-NYUGAIKBN   =  "2"
      *    20040820  @@@@-MOTOHASI
      *    外来フォーム
      *    見出し編集
                 MOVE     1             TO   WRK-CNT1
                 MOVE     1             TO   WRK-CNT2
                 MOVE     ZERO          TO   FLG-PASS
      *
                 PERFORM  UNTIL  FLG-PASS  =  1
                   AND   S137TMP-KOH-KEY(WRK-CNT1)   =   SPACE
                   PERFORM  3101-HEAD-HEN-SEC
      *
      *    明細編集
                   PERFORM  3201-BODY-HEN-SEC
      *
      *    印刷処理
                   PERFORM  330-PRINT-OUT-SEC
                   MOVE     1     TO     FLG-PASS
                 END-PERFORM
               END-IF
             END-IF
      *
      *    20040820  @@@@-MOTOHASI  食事療養フォームの印字
      *           PERFORM  3102-HEAD-HEN-SEC
      *
      *           MOVE     1       TO   WRK-CNT1
      *           MOVE     1       TO   WRK-CNT2
      *           PERFORM  3202-BODY-HEN-SEC
      *
      *           PERFORM  340-PRINT-OUT-SEC
      *    20040820  @@@@-MOTOHASI
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    S137TMP-FILE
      *
           .
       2002-PRINT-TYPE1-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保帳票編集<ヘッダー部>処理
      ******************************************************************
       3101-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S137
      *
      *    ページ数
      * 20040322 編集 Y.Takahashi
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S137-PAGE
      *
      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
           MOVE     1               TO    IDX4
           MOVE     SUM-SRYKAMAX    TO    IDX-MAX
           PERFORM   UNTIL IDX4   >   IDX-MAX
             IF   S137TMP-SRYKA   =   SUM-SRYKA(IDX4)
               MOVE    ZERO         TO    IDX-MAX
               MOVE    SUM-SRYKANAME(IDX4)   TO   S137-SRYKANAME
             ELSE
               ADD     1            TO    IDX4
             END-IF
           END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF   S137TMP-NYUGAIKBN   =  "1"
             MOVE    "入院"         TO   S137-NYUGAI
           ELSE
             MOVE    "外来"         TO   S137-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S137-SRYYM
           MOVE    WRK-SEIYMD       TO   S137-SYSYMD
      *
           .
       3101-HEAD-HEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保帳票編集<ヘッダー部>処理
      ******************************************************************
       3102-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S137N1
      *
      *    ページ数
      * 20040322 編集 Y.Takahashi
           ADD     1                TO    CNT-PAGE
           MOVE    CNT-PAGE         TO    WRK-PAGE
           MOVE    WRK-PAGE         TO    S137N1-PAGE
      *
      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
             MOVE     1               TO   IDX4
             MOVE     SUM-SRYKAMAX    TO   IDX-MAX
             PERFORM  UNTIL    IDX4   >   IDX-MAX
               IF  S137TMP-SRYKA   =   SUM-SRYKA(IDX4)
                 MOVE   ZERO                   TO   IDX-MAX
                 MOVE   SUM-SRYKANAME(IDX4)    TO   S137N1-SRYKANAME
               ELSE
                 ADD    1                      TO   IDX4
               END-IF
             END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF    S137TMP-NYUGAIKBN      =    "1"
             MOVE     "入院"        TO   S137N1-NYUGAI
           ELSE
             MOVE     "外来"        TO   S137N1-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S137N1-SRYYM
           MOVE    WRK-SEIYMD       TO   S137N1-SYSYMD
      *
           .
       3102-HEAD-HEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保帳票編集<ヘッダー部>処理
      ******************************************************************
       3103-HEAD-HEN-SEC                 SECTION.
      **
           INITIALIZE                      S137N2
      *
      *    ページ数
      * 20040322 編集 Y.Takahashi
           ADD     1                TO   CNT-PAGE
           MOVE    CNT-PAGE         TO   WRK-PAGE
           MOVE    WRK-PAGE         TO   S137N2-PAGE
      *
      *    診療科名(旧総合病院時のみ)
      * 20081014 主科設定時に診療科を印字するSTART
      *     IF    SYS-1001-HOSPSBT1  =  1
      * 20081014 主科設定時に診療科を印字するEND
           MOVE     1               TO   IDX4
           MOVE     SUM-SRYKAMAX    TO   IDX-MAX
           PERFORM  UNTIL  IDX4   >  IDX-MAX
             IF   S137TMP-SRYKA   =   SUM-SRYKA(IDX4)
               MOVE    ZERO         TO   IDX-MAX
               MOVE    SUM-SRYKANAME(IDX4)   TO   S137N2-SRYKANAME
             ELSE
               ADD     1            TO   IDX4
             END-IF
           END-PERFORM
      * 20081014 主科設定時に診療科を印字するSTART
      *     END-IF
      * 20081014 主科設定時に診療科を印字するEND
      *
           IF   S137TMP-NYUGAIKBN   =  "1"
             MOVE     "入院"        TO   S137N2-NYUGAI
           ELSE
             MOVE     "外来"        TO   S137N2-NYUGAI
           END-IF
      *
           MOVE    WRK-SRYYM        TO   S137N2-SRYYM
           MOVE    WRK-SEIYMD       TO   S137N2-SYSYMD
      *
           .
       3103-HEAD-HEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保帳票編集<明細>処理
      ******************************************************************
       3201-BODY-HEN-SEC               SECTION.
      *
      *
           PERFORM    VARYING IDX1   FROM 1   BY 1
                      UNTIL   IDX1   >   53
                      OR   FLG-PASS  =  1
      *
      *    件数
             MOVE     S137TMP-KENSU(IDX1)   TO   WRK-Z4
             MOVE     WRK-Z4                TO   S137-KENSU(IDX1)
      *
      *    日数
             MOVE     S137TMP-NISSU(IDX1)   TO   WRK-Z4
             MOVE     WRK-Z4                TO   S137-NISSU(IDX1)
      *
      *    点数
             MOVE     S137TMP-TENSU(IDX1)   TO   WRK-Z92
             MOVE     WRK-Z92               TO   S137-TENSU(IDX1)
      *
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *    窓口請求額
      *        MOVE     S137TMP-MADOGK(IDX1)  TO   WRK-Z92
      *        MOVE     WRK-Z92               TO   S137-MADOGK(IDX1)
             MOVE     S137TMP-MADOGK(IDX1)  TO   WRK-Z93
             MOVE     WRK-Z93               TO   S137-MADOGK(IDX1)
      *
      *    自費金額
             MOVE     S137TMP-JIHIGK(IDX1)  TO   WRK-Z92
             MOVE     WRK-Z92               TO   S137-JIHIGK(IDX1)
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
      *
      *    保険請求額
             MOVE     S137TMP-HKNGK(IDX1)   TO   WRK-Z92
             MOVE     WRK-Z92               TO   S137-HKNGK(IDX1)
      *
      *    公費請求額
             MOVE     S137TMP-KOHGK(IDX1)   TO   WRK-Z92
             MOVE     WRK-Z92               TO   S137-KOHGK(IDX1)
      *
      *    医療収益
             MOVE     S137TMP-SYUEKI(IDX1)  TO   WRK-Z92
             MOVE     WRK-Z92               TO   S137-SYUEKI(IDX1)
           END-PERFORM
      *
      *    合計
           IF   FLG-PASS   =   ZERO
      *
      *    件数
             MOVE     S137TMP-GKENSU    TO   WRK-Z4
             MOVE     WRK-Z4            TO   S137-GKENSU
      *
      *    日数
             MOVE     S137TMP-GNISSU    TO   WRK-Z4
             MOVE     WRK-Z4            TO   S137-GNISSU
      *
      *    点数
             MOVE     S137TMP-GTENSU    TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137-GTENSU
      *
      *    窓口請求額
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *       MOVE     S137TMP-GMADOGK   TO   WRK-Z92
      *       MOVE     WRK-Z92           TO   S137-GMADOGK
             MOVE     S137TMP-GMADOGK   TO   WRK-Z93
             MOVE     WRK-Z93           TO   S137-GMADOGK
      *
      *    自費金額
             MOVE     S137TMP-GJIHIGK   TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137-GJIHIGK
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
      *    保険請求額
             MOVE     S137TMP-GHKNGK    TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137-GHKNGK
      *
      *    公費請求額
             MOVE     S137TMP-GKOHGK    TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137-GKOHGK
      *
      *    医療収益
             MOVE     S137TMP-GSYUEKI   TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137-GSYUEKI
           END-IF
      *
      *
           INITIALIZE    WRK-KOHI-GOKEI-TBL
           PERFORM  VARYING IDX3   FROM 1   BY 1
                    UNTIL   IDX3   >   30
                    OR      S137TMP-KOH-KEY(WRK-CNT1)  =  SPACE
      *
      *    公費名
             INITIALIZE                 WRK-KOHTMP-KEY
             MOVE   S137TMP-KOH-KEY(WRK-CNT1)  TO   WRK-KOHTMP-KEY
             PERFORM  333-KOHNAME-HEN-SEC
             MOVE   WRK-KOHNAME-G              TO   S137-KOHNAME(IDX3)
      *    件数
             MOVE   S137TMP-KKENSU(WRK-CNT1)   TO   WRK-Z4
             MOVE   WRK-Z4                     TO   S137-KKENSU(IDX3)
      *
      *    日数
             MOVE   S137TMP-KNISSU(WRK-CNT1)   TO   WRK-Z4
             MOVE   WRK-Z4                     TO   S137-KNISSU(IDX3)
      *
      *    点数
             MOVE   S137TMP-KTENSU(WRK-CNT1)   TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-KTENSU(IDX3)
      *
      *    公費小計加算
             ADD    S137TMP-KKENSU(WRK-CNT1)   TO   WRK-GKKENSU
             ADD    S137TMP-KNISSU(WRK-CNT1)   TO   WRK-GKNISSU
             ADD    S137TMP-KTENSU(WRK-CNT1)   TO   WRK-GKTENSU
             ADD    1                          TO   WRK-CNT1
           END-PERFORM
      *
      *    公費小計印字
           MOVE    WRK-GKKENSU                 TO   WRK-Z4
           MOVE    WRK-Z4                      TO   S137-GKKENSU
      *
           MOVE    WRK-GKNISSU                 TO   WRK-Z4
           MOVE    WRK-Z4                      TO   S137-GKNISSU
      *
           MOVE    WRK-GKTENSU                 TO   WRK-Z92
           MOVE    WRK-Z92                     TO   S137-GKTENSU
      *
      ***  20040819  @@@@-MOTOHASI  公費単独時の集計
      *
           INITIALIZE       WRK-KOHITAN-GOKEI-TBL
           PERFORM   VARYING IDX3  FROM 1   BY 1
                     UNTIL   IDX3   >  10
                     OR    FLG-PASS  =  1
                     OR    S137TMP-KOHTAN-KEY(WRK-CNT2)  =   SPACE
      *
      *    公費名
             DISPLAY "   IDX3 = " IDX3
             INITIALIZE            WRK-KOHTANTMP-KEY
             MOVE   S137TMP-KOHTAN-KEY(WRK-CNT2)  TO  WRK-KOHTANTMP-KEY
             PERFORM  444-KOHNAME-HEN-SEC
             MOVE   WRK-KOHTNAME-G             TO   S137-KOHTNAME(IDX3)
      *    件数
             MOVE   S137TMP-KTKENSU(WRK-CNT2)  TO   WRK-Z4
             MOVE   WRK-Z4                     TO   S137-KTKENSU(IDX3)
      *
      *    日数
             MOVE   S137TMP-KTNISSU(WRK-CNT2)  TO   WRK-Z4
             MOVE   WRK-Z4                     TO   S137-KTNISSU(IDX3)
      *
      *    点数
             MOVE   S137TMP-KTTENSU(WRK-CNT2)  TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-KTTENSU(IDX3)
      *
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *    窓口請求額
      *       MOVE   S137TMP-KTMADOGK(WRK-CNT2) TO   WRK-Z92
      *       MOVE   WRK-Z92                    TO   S137-KTMADOGK(IDX3)
             MOVE   S137TMP-KTMADOGK(WRK-CNT2) TO   WRK-Z93
             MOVE   WRK-Z93                    TO   S137-KTMADOGK(IDX3)
      *
      *    自費金額
             MOVE   S137TMP-KTJIHIGK(WRK-CNT2) TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-KTJIHIGK(IDX3)
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
      *
      *    保険請求額
             MOVE   S137TMP-KTHKNGK(WRK-CNT2)  TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-KTHKNGK(IDX3)
      *
      *    公費請求額
             MOVE   S137TMP-KTKOHGK(WRK-CNT2)  TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-KTKOHGK(IDX3)
      *
      *    医療収益
             MOVE   S137TMP-KTSYUEKI(WRK-CNT2) TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-KTSYUEKI(IDX3)
      *
      *    公費単独小計加算
             ADD   S137TMP-KTKENSU(WRK-CNT2)   TO   WRK-GKTKENSU
             ADD   S137TMP-KTNISSU(WRK-CNT2)   TO   WRK-GKTNISSU
             ADD   S137TMP-KTTENSU(WRK-CNT2)   TO   WRK-GKTTENSU
             ADD   S137TMP-KTMADOGK(WRK-CNT2)  TO   WRK-GKTMADOGK
      *--- 2008/12/18 ADD START ----------------------------------------
             ADD   S137TMP-KTJIHIGK(WRK-CNT2)  TO   WRK-GKTJIHIGK
      *--- 2008/12/18 ADD END ------------------------------------------
             ADD   S137TMP-KTHKNGK(WRK-CNT2)   TO   WRK-GKTHKNGK
             ADD   S137TMP-KTKOHGK(WRK-CNT2)   TO   WRK-GKTKOHGK
             ADD   S137TMP-KTSYUEKI(WRK-CNT2)  TO   WRK-GKTSYUEKI
      *
             ADD    1                          TO   WRK-CNT2
           END-PERFORM
      *
      *    公費小計印字
           IF   FLG-PASS   =   ZERO
             MOVE   WRK-GKTKENSU               TO   WRK-Z4
             MOVE   WRK-Z4                     TO   S137-GKTKENSU
      *
             MOVE   WRK-GKTNISSU               TO   WRK-Z4
             MOVE   WRK-Z4                     TO   S137-GKTNISSU
      *
             MOVE   WRK-GKTTENSU               TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-GKTTENSU
      *
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *    窓口請求額小計
      *       MOVE   WRK-GKTMADOGK              TO   WRK-Z92
      *       MOVE   WRK-Z92                    TO   S137-GKTMADOGK
             MOVE   WRK-GKTMADOGK              TO   WRK-Z93
             MOVE   WRK-Z93                    TO   S137-GKTMADOGK
      *
      *    自費金額小計
             MOVE   WRK-GKTJIHIGK              TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-GKTJIHIGK
      *
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
             MOVE   WRK-GKTHKNGK               TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-GKTHKNGK
      *
             MOVE   WRK-GKTKOHGK               TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-GKTKOHGK
      *
             MOVE   WRK-GKTSYUEKI              TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137-GKTSYUEKI
           END-IF
      ***  20040819  @@@@-MOTOHASI  ここまで
      *
           .
       3201-BODY-HEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保帳票編集<明細>処理(入院：１枚目)
      ******************************************************************
       3202-BODY-HEN-SEC               SECTION.
      *
           PERFORM    VARYING IDX1   FROM 1   BY 1
                      UNTIL   IDX1  >  46
      *
      *    件数
             MOVE     S137TMP-KENSU(IDX1)    TO   WRK-Z4
             MOVE     WRK-Z4                 TO   S137N1-KENSU(IDX1)
      *
      *    日数
             MOVE     S137TMP-NISSU(IDX1)    TO   WRK-Z4
             MOVE     WRK-Z4                 TO   S137N1-NISSU(IDX1)
      *
      *    点数
             MOVE     S137TMP-TENSU(IDX1)    TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N1-TENSU(IDX1)
      *
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *    窓口請求額
      *       MOVE     S137TMP-MADOGK(IDX1)   TO   WRK-Z92
      *       MOVE     WRK-Z92                TO   S137N1-MADOGK(IDX1)
             MOVE     S137TMP-MADOGK(IDX1)   TO   WRK-Z93
             MOVE     WRK-Z93                TO   S137N1-MADOGK(IDX1)
      *
      *    自費金額
             MOVE     S137TMP-JIHIGK(IDX1)   TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N1-JIHIGK(IDX1)
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
      *
      *    保険請求額
             MOVE     S137TMP-HKNGK(IDX1)    TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N1-HKNGK(IDX1)
      *
      *    公費請求額
             MOVE     S137TMP-KOHGK(IDX1)    TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N1-KOHGK(IDX1)
      *
      *    医療収益
             MOVE     S137TMP-SYUEKI(IDX1)   TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N1-SYUEKI(IDX1)
      *    食事療養
      *    日数
             MOVE     S137TMP-SNISSU(IDX1)   TO   WRK-Z4
             MOVE     WRK-Z4                 TO   S137N1-SNISSU(IDX1)
      *
      *    請求額
             MOVE     S137TMP-SSEIKYU(IDX1)  TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N1-SSEIKYU(IDX1)
      *
      *    標準負担額
             MOVE     S137TMP-SFTNGK(IDX1)   TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N1-SFTNGK(IDX1)
           END-PERFORM
      *
           .
       3202-BODY-HEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    社保帳票編集<明細>処理(入院：２枚目)
      ******************************************************************
       3203-BODY-HEN-SEC               SECTION.
      *
      *
           PERFORM    VARYING IDX1   FROM 47   BY 1
                      UNTIL   IDX1  >  53
                      OR       FLG-PASS  = 1
             ADD      1         TO    IDX6
      *
      *    件数
             MOVE     S137TMP-KENSU(IDX1)    TO   WRK-Z4
             MOVE     WRK-Z4                 TO   S137N2-KENSU(IDX6)
      *
      *    日数
             MOVE     S137TMP-NISSU(IDX1)    TO   WRK-Z4
             MOVE     WRK-Z4                 TO   S137N2-NISSU(IDX6)
      *
      *    点数
             MOVE     S137TMP-TENSU(IDX1)    TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N2-TENSU(IDX6)
      *
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *    窓口請求額
      *       MOVE     S137TMP-MADOGK(IDX1)   TO   WRK-Z92
      *       MOVE     WRK-Z92                TO   S137N2-MADOGK(IDX6)
             MOVE     S137TMP-MADOGK(IDX1)   TO   WRK-Z93
             MOVE     WRK-Z93                TO   S137N2-MADOGK(IDX6)
      *
      *    自費金額
             MOVE     S137TMP-JIHIGK(IDX1)   TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N2-JIHIGK(IDX6)
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
      *    保険請求額
             MOVE     S137TMP-HKNGK(IDX1)    TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N2-HKNGK(IDX6)
      *
      *    公費請求額
             MOVE     S137TMP-KOHGK(IDX1)    TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N2-KOHGK(IDX6)
      *
      *    医療収益
             MOVE     S137TMP-SYUEKI(IDX1)   TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N2-SYUEKI(IDX6)
      *    食事療養
      *    日数
             MOVE     S137TMP-SNISSU(IDX1)   TO   WRK-Z4
             MOVE     WRK-Z4                 TO   S137N2-SNISSU(IDX6)
      *
      *    請求額
             MOVE     S137TMP-SSEIKYU(IDX1)  TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N2-SSEIKYU(IDX6)
      *
      *    標準負担額
             MOVE     S137TMP-SFTNGK(IDX1)   TO   WRK-Z92
             MOVE     WRK-Z92                TO   S137N2-SFTNGK(IDX6)
           END-PERFORM
      *
      *    合計
           IF   FLG-PASS   =   ZERO
      *
      *    件数
             MOVE     S137TMP-GKENSU    TO   WRK-Z4
             MOVE     WRK-Z4            TO   S137N2-GKENSU
      *
      *    日数
             MOVE     S137TMP-GNISSU    TO   WRK-Z4
             MOVE     WRK-Z4            TO   S137N2-GNISSU
      *
      *    点数
             MOVE     S137TMP-GTENSU    TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137N2-GTENSU
      *
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *    窓口請求額
      *       MOVE     S137TMP-GMADOGK   TO   WRK-Z92
      *       MOVE     WRK-Z92           TO   S137N2-GMADOGK
             MOVE     S137TMP-GMADOGK   TO   WRK-Z93
             MOVE     WRK-Z93           TO   S137N2-GMADOGK
      *
      *    自費金額
             MOVE     S137TMP-GJIHIGK   TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137N2-GJIHIGK
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
      *    保険請求額
             MOVE     S137TMP-GHKNGK    TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137N2-GHKNGK
      *
      *    公費請求額
             MOVE     S137TMP-GKOHGK    TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137N2-GKOHGK
      *
      *    医療収益
             MOVE     S137TMP-GSYUEKI   TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137N2-GSYUEKI
           END-IF
      *    食事療養
      *    日数
             MOVE     S137TMP-SGNISSU   TO   WRK-Z4
             MOVE     WRK-Z4            TO   S137N2-SGNISSU
      *
      *    請求額
             MOVE     S137TMP-SGSEIKYU  TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137N2-SGSEIKYU
      *
      *    標準負担額
             MOVE     S137TMP-SGFTNGK   TO   WRK-Z92
             MOVE     WRK-Z92           TO   S137N2-SGFTNGK
      *
      *
           INITIALIZE    WRK-KOHI-GOKEI-TBL
           PERFORM   VARYING IDX3   FROM 1   BY 1
                     UNTIL   IDX3  >  30
                     OR    S137TMP-KOH-KEY(WRK-CNT1)  =  SPACE
      *
      *    公費名
             INITIALIZE         WRK-KOHTMP-KEY
             MOVE   S137TMP-KOH-KEY(WRK-CNT1)  TO   WRK-KOHTMP-KEY
             PERFORM    333-KOHNAME-HEN-SEC
             MOVE   WRK-KOHNAME-G              TO   S137N2-KOHNAME(IDX3)
      *
      *    件数
             MOVE   S137TMP-KKENSU(WRK-CNT1)   TO   WRK-Z4
             MOVE   WRK-Z4                     TO   S137N2-KKENSU(IDX3)
      *
      *    日数
             MOVE   S137TMP-KNISSU(WRK-CNT1)   TO   WRK-Z4
             MOVE   WRK-Z4                     TO   S137N2-KNISSU(IDX3)
      *
      *    点数
             MOVE   S137TMP-KTENSU(WRK-CNT1)   TO   WRK-Z92
             MOVE   WRK-Z92                    TO   S137N2-KTENSU(IDX3)
      *
      *    公費小計加算
             ADD    S137TMP-KKENSU(WRK-CNT1)   TO   WRK-GKKENSU
             ADD    S137TMP-KNISSU(WRK-CNT1)   TO   WRK-GKNISSU
             ADD    S137TMP-KTENSU(WRK-CNT1)   TO   WRK-GKTENSU
             ADD    1                          TO   WRK-CNT1
           END-PERFORM
      *
      *    公費小計印字
           MOVE    WRK-GKKENSU                 TO   WRK-Z4
           MOVE    WRK-Z4                      TO   S137N2-GKKENSU
      *
           MOVE    WRK-GKNISSU                 TO   WRK-Z4
           MOVE    WRK-Z4                      TO   S137N2-GKNISSU
      *
           MOVE    WRK-GKTENSU                 TO   WRK-Z92
           MOVE    WRK-Z92                     TO   S137N2-GKTENSU
      *
      ***  20040820  @@@@-MOTOHASI  公費単独時の集計
      *
           INITIALIZE    WRK-KOHITAN-GOKEI-TBL
           PERFORM  VARYING IDX3  FROM 1  BY 1
                    UNTIL   IDX3  >  10
                    OR   FLG-PASS  =  1
                    OR   S137TMP-KOHTAN-KEY(WRK-CNT2)  =  SPACE
      *
      *    公費名
             INITIALIZE            WRK-KOHTANTMP-KEY
             MOVE   S137TMP-KOHTAN-KEY(WRK-CNT2)  TO   WRK-KOHTANTMP-KEY
             PERFORM    444-KOHNAME-HEN-SEC
             MOVE   WRK-KOHTNAME-G            TO   S137N2-KOHTNAME(IDX3)
      *    件数
             MOVE   S137TMP-KTKENSU(WRK-CNT2)    TO   WRK-Z4
             MOVE   WRK-Z4                    TO   S137N2-KTKENSU(IDX3)
      *
      *    日数
             MOVE   S137TMP-KTNISSU(WRK-CNT2)    TO   WRK-Z4
             MOVE   WRK-Z4                    TO   S137N2-KTNISSU(IDX3)
      *
      *    点数
             MOVE   S137TMP-KTTENSU(WRK-CNT2)    TO   WRK-Z92
             MOVE   WRK-Z92                   TO   S137N2-KTTENSU(IDX3)
      *
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *    窓口請求額
      *       MOVE   S137TMP-KTMADOGK(WRK-CNT2)   TO    WRK-Z92
      *       MOVE   WRK-Z92                   TO   S137N2-KTMADOGK(IDX3)
             MOVE   S137TMP-KTMADOGK(WRK-CNT2)   TO    WRK-Z93
             MOVE   WRK-Z93                   TO   S137N2-KTMADOGK(IDX3)
      *
      *    自費金額
             MOVE   S137TMP-KTJIHIGK(WRK-CNT2)   TO    WRK-Z92
             MOVE   WRK-Z92                   TO   S137N2-KTJIHIGK(IDX3)
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
      *    保険請求額
             MOVE   S137TMP-KTHKNGK(WRK-CNT2)    TO    WRK-Z92
             MOVE   WRK-Z92                   TO   S137N2-KTHKNGK(IDX3)
      *
      *    公費請求額
             MOVE   S137TMP-KTKOHGK(WRK-CNT2)    TO    WRK-Z92
             MOVE   WRK-Z92                   TO   S137N2-KTKOHGK(IDX3)
      *
      *    医療収益
             MOVE   S137TMP-KTSYUEKI(WRK-CNT2)   TO    WRK-Z92
             MOVE   WRK-Z92                   TO   S137N2-KTSYUEKI(IDX3)
      *    食事療養
      *    日数
             MOVE   S137TMP-SKTNISSU(WRK-CNT2)   TO    WRK-Z4
             MOVE   WRK-Z4                    TO   S137N2-SKTNISSU(IDX3)
      *
      *    請求額
             MOVE   S137TMP-SKTSEIKYU(WRK-CNT2)  TO    WRK-Z92
             MOVE   WRK-Z92                  TO   S137N2-SKTSEIKYU(IDX3)
      *
      *    標準負担額
             MOVE   S137TMP-SKTFTNGK(WRK-CNT2)   TO    WRK-Z92
             MOVE   WRK-Z92                  TO   S137N2-SKTFTNGK(IDX3)
      *
      *    公費単独小計加算
             ADD    S137TMP-KTKENSU(WRK-CNT2)      TO   WRK-GKTKENSU
             ADD    S137TMP-KTNISSU(WRK-CNT2)      TO   WRK-GKTNISSU
             ADD    S137TMP-KTTENSU(WRK-CNT2)      TO   WRK-GKTTENSU
             ADD    S137TMP-KTMADOGK(WRK-CNT2)     TO   WRK-GKTMADOGK
      *--- 2008/12/18 ADD START ----------------------------------------
             ADD    S137TMP-KTJIHIGK(WRK-CNT2)     TO   WRK-GKTJIHIGK
      *--- 2008/12/18 ADD END ------------------------------------------
             ADD    S137TMP-KTHKNGK(WRK-CNT2)      TO   WRK-GKTHKNGK
             ADD    S137TMP-KTKOHGK(WRK-CNT2)      TO   WRK-GKTKOHGK
             ADD    S137TMP-KTSYUEKI(WRK-CNT2)     TO   WRK-GKTSYUEKI
             ADD    S137TMP-SKTNISSU(WRK-CNT2)     TO   WRK-SGKTNISSU
             ADD    S137TMP-SKTSEIKYU(WRK-CNT2)    TO   WRK-SGKTSEIKYU
             ADD    S137TMP-SKTFTNGK(WRK-CNT2)     TO   WRK-SGKTFTNGK
      *
             ADD    1          TO    WRK-CNT2
           END-PERFORM
      *
      *    公費小計印字
           IF   FLG-PASS   =   ZERO
             MOVE    WRK-GKTKENSU            TO   WRK-Z4
             MOVE    WRK-Z4                  TO   S137N2-GKTKENSU
      *
             MOVE    WRK-GKTNISSU            TO   WRK-Z4
             MOVE    WRK-Z4                  TO   S137N2-GKTNISSU
      *
             MOVE    WRK-GKTTENSU            TO   WRK-Z92
             MOVE    WRK-Z92                 TO   S137N2-GKTTENSU
      *
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *    窓口請求額小計
      *       MOVE    WRK-GKTMADOGK           TO   WRK-Z92
      *       MOVE    WRK-Z92                 TO   S137N2-GKTMADOGK
             MOVE    WRK-GKTMADOGK           TO   WRK-Z93
             MOVE    WRK-Z93                 TO   S137N2-GKTMADOGK
      *
      *    自費金額小計
             MOVE    WRK-GKTJIHIGK           TO   WRK-Z92
             MOVE    WRK-Z92                 TO   S137N2-GKTJIHIGK
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
      *
             MOVE    WRK-GKTHKNGK            TO   WRK-Z92
             MOVE    WRK-Z92                 TO   S137N2-GKTHKNGK
      *
             MOVE    WRK-GKTKOHGK            TO   WRK-Z92
             MOVE    WRK-Z92                 TO   S137N2-GKTKOHGK
      *
             MOVE    WRK-GKTSYUEKI           TO   WRK-Z92
             MOVE    WRK-Z92                 TO   S137N2-GKTSYUEKI
      *
             MOVE    WRK-SGKTNISSU           TO   WRK-Z4
             MOVE    WRK-Z4                  TO   S137N2-SGKTNISSU
      *
             MOVE    WRK-SGKTSEIKYU          TO   WRK-Z92
             MOVE    WRK-Z92                 TO   S137N2-SGKTSEIKYU
      *
             MOVE    WRK-SGKTFTNGK           TO   WRK-Z92
             MOVE    WRK-Z92                 TO   S137N2-SGKTFTNGK
      *
           END-IF
      ***  20040819  @@@@-MOTOHASI  ここまで
      *
           .
       3203-BODY-HEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    帳票編集<印刷>処理
      ******************************************************************
       330-PRINT-OUT-SEC         SECTION.
      *
      *
           INITIALIZE                         ORCSPRTAREA
           MOVE   "INS"                       TO   SPRT-MODE
           MOVE   S100LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE   S100LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE   S100LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE   S100LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE   S100LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE   S100LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE   S100LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE   S100LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***     MOVE    "A00000S137"        TO  SPRT-PRTID
      * 20080423 H20年4月改正対応START tanaka
      *     MOVE    "A00000S137.red"        TO  SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     IF S100LNK-YM < "200804"
      *       MOVE    "A00000S137O.red"        TO  SPRT-PRTID
      *     ELSE
           IF   S137TMP-SRYYMKBN  =  "0"
      * 20080512 返戻が正しく印字されない不具合を修正END
             MOVE    "A00000S137O.red"        TO   SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
           ELSE
             MOVE    "A00000S137N.red"        TO   SPRT-PRTID
           END-IF
      *     END-IF
      * 20080512 返戻が正しく印字されない不具合を修正END
      * 20080423 H20年4月改正対応END
      ***  20040917 MOTOHASI MONPEに変更
           MOVE    S100LNK-SPRT-TITLE         TO   SPRT-TITLE
           MOVE    S137                       TO   SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID    TO   SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID      TO   SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM     TO   SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"                  USING
                                              ORCSPRTAREA
      ***07/09/20 nagaya 4.0対応 start
                                              SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   SPRT-RETURN   =   ZERO
             CONTINUE               ELSE
             MOVE   "印刷ＤＢに更新できませんでした"  TO   WRK-RECEERR
	         PERFORM 500-ERR-HENSYU-SEC
	       END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      ******************************************************************
      *    帳票編集<印刷>処理
      ******************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *
      *
           INITIALIZE                          ORCSPRTAREA
           MOVE    "INS"                       TO   SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***     MOVE    "A00000S137N1"        TO  SPRT-PRTID
      * 20080423 H20年4月改正対応START tanaka
      *     MOVE    "A00000S137N1.red"        TO  SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *    IF S100LNK-YM < "200804"
      *         MOVE    "A00000S137N1O.red"        TO  SPRT-PRTID
      *     ELSE
           IF S137TMP-SRYYMKBN   =   "0"
      * 20080512 返戻が正しく印字されない不具合を修正END
             MOVE    "A00000S137N1O.red"       TO   SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
           ELSE
             MOVE    "A00000S137N1N.red"       TO   SPRT-PRTID
           END-IF
      *     END-IF
      * 20080512 返戻が正しく印字されない不具合を修正END
      * 20080423 H20年4月改正対応END
      ***  20040917 MOTOHASI MONPEに変更
           MOVE    S100LNK-SPRT-TITLE          TO   SPRT-TITLE
           MOVE    S137N1                      TO   SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID     TO   SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID       TO   SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM      TO   SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"                   USING
                                               ORCSPRTAREA
      ***07/09/20 nagaya 4.0対応 start
                                               SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE    "印刷ＤＢに更新できませんでした"   TO   WRK-RECEERR
	         PERFORM   500-ERR-HENSYU-SEC
	       END-IF
           .
       340-PRINT-OUT-EXT.
           EXIT.
      *
      ******************************************************************
      *    帳票編集<印刷>処理
      ******************************************************************
       350-PRINT-OUT-SEC         SECTION.
      *
      *
           INITIALIZE                          ORCSPRTAREA
           MOVE    "INS"                       TO   SPRT-MODE
           MOVE    S100LNK-PRTKANRI-RENNUM     TO   SPRT-RENNUM
           MOVE    S100LNK-PRTKANRI-TBL-KEY    TO   SPRT-TBL-KEY
           MOVE    S100LNK-PRTKANRI-TBL-GROUP  TO   SPRT-TBL-GROUP
           MOVE    S100LNK-PRTKANRI-SRYYM      TO   SPRT-SRYYM
           MOVE    S100LNK-PRTKANRI-SKYYMD     TO   SPRT-SKYYMD
           MOVE    S100LNK-PRTKANRI-SHELLID    TO   SPRT-SHELLID
           MOVE    S100LNK-PRTKANRI-SHORI-RENNUM  TO   SPRT-SHORI-RENNUM
           MOVE    S100LNK-PRTKANRI-PRIORITY   TO   SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***     MOVE    "A00000S137N2"        TO  SPRT-PRTID
      * 20080423 H20年4月改正対応START tanaka
      *     MOVE    "A00000S137N2.red"        TO  SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
      *     IF S100LNK-YM < "200804"
      *         MOVE    "A00000S137N2O.red"        TO  SPRT-PRTID
      *     ELSE
      * 20080909 公費単独欄の食事療養の不具合を修正
           IF   S137TMP-SRYYMKBN   =  "0"
      * 20080512 返戻が正しく印字されない不具合を修正END
             MOVE    "A00000S137N2O.red"       TO   SPRT-PRTID
      * 20080512 返戻が正しく印字されない不具合を修正START tanaka
           ELSE
             MOVE    "A00000S137N2N.red"       TO   SPRT-PRTID
           END-IF
      * 20080909 公費単独欄の食事療養の不具合を修正
      *     END-IF
      * 20080512 返戻が正しく印字されない不具合を修正END
      * 20080423 H20年4月改正対応END
      ***  20040917 MOTOHASI MONPEに変更
           MOVE    S100LNK-SPRT-TITLE          TO   SPRT-TITLE
           MOVE    S137N2                      TO   SPRT-PRTDATA
           MOVE    S100LNK-PRTKANRI-TERMID     TO   SPRT-TERMID
           MOVE    S100LNK-PRTKANRI-OPID       TO   SPRT-OPID
           MOVE    S100LNK-PRTKANRI-PRTNM      TO   SPRT-PRTNM
      ***** site-libで動かすためにコメントアウト
      ***     MOVE    "1"          TO  SPRT-SITEKBN
      *****
           CALL    "ORCSPRT"                   USING
                                               ORCSPRTAREA
      ***07/09/20 nagaya 4.0対応 start
                                               SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF  SPRT-RETURN   =   ZERO
             CONTINUE
           ELSE
             MOVE   "印刷ＤＢに更新できませんでした"  TO   WRK-RECEERR
	         PERFORM    500-ERR-HENSYU-SEC
	       END-IF
           .
       350-PRINT-OUT-EXT.
           EXIT.
      ******************************************************************
      *    公費名編集処理
      ******************************************************************
       333-KOHNAME-HEN-SEC               SECTION.
      *
           MOVE    SPACE       TO    WRK-KOHNAME-G
                                     WRK-KOHNAME-G2
      *
           IF   WRK-KOHTMP-RJNKBN  NOT =  SPACE
             MOVE  "２７＋"    TO    WRK-KOHNAME-G
           END-IF
      *
           MOVE    WRK-KOHTMP-KOHNUM    TO   WRK-KOHNUM
           PERFORM   950-HKNNUM-SET-SEC
           IF   WRK-KOHNUM    =   "051"
             MOVE    "特定疾患"         TO   HKN-TANSEIDONAME
           END-IF
           STRING  WRK-KOHNAME-G      DELIMITED BY SPACE
                   HKN-HBTNUM         DELIMITED BY SPACE
                   "("                DELIMITED BY SIZE
                   HKN-TANSEIDONAME   DELIMITED BY SPACE
                   ")"                DELIMITED BY SIZE
                   INTO               WRK-KOHNAME-G2
           END-STRING
           DISPLAY "WRK-KOHNAME-G2 "WRK-KOHNAME-G2
      *
           MOVE    WRK-KOHNAME-G2       TO   WRK-KOHNAME-G
      *
      ***  20040917 MOTOHASI
           MOVE   WRK-KOHNAME-G         TO   WRK-MAE-INPUT
           PERFORM   2009-HENKAN-SEC
           MOVE   WRK-OUT-INPUT         TO   WRK-KOHNAME-G
      ***  20040917 MOTOHASI
           .
       333-KOHNAME-HEN-EXT.
           EXIT.
      ******************************************************************
      *    公費名編集処理(公費単独)  20040819  @@@@-MOTOHASI
      ******************************************************************
       444-KOHNAME-HEN-SEC               SECTION.
      *
           MOVE    SPACE          TO   WRK-KOHTNAME-G
                                       WRK-KOHTNAME-G2
      *
           IF   WRK-KOHTANTMP-KOHNUM2  NOT =  SPACE
             STRING   "＋"                         DELIMITED BY SIZE
                      WRK-KOHTANTMP-KOHNUM2(2:2)   DELIMITED BY SPACE
                      INTO                         WRK-KOHTNAME-G
             END-STRING
           END-IF
      *
           MOVE   WRK-KOHTANTMP-KOHNUM    TO   WRK-KOHNUM
           PERFORM   950-HKNNUM-SET-SEC
           IF   WRK-KOHNUM   =   "051"
             MOVE    "特定疾患"    TO    HKN-TANSEIDONAME
           END-IF
           STRING  HKN-HBTNUM         DELIMITED BY SPACE
                   "("                DELIMITED BY SIZE
                   HKN-TANSEIDONAME   DELIMITED BY SPACE
                   ")"                DELIMITED BY SIZE
                   WRK-KOHTNAME-G     DELIMITED BY SPACE
                   INTO               WRK-KOHTNAME-G2
           END-STRING
      *
           MOVE   WRK-KOHTNAME-G2         TO   WRK-KOHTNAME-G
      *
      ***  20040917 MOTOHASI
           MOVE   WRK-KOHTNAME-G          TO   WRK-MAE-INPUT
           PERFORM   2009-HENKAN-SEC
           MOVE   WRK-OUT-INPUT           TO   WRK-KOHTNAME-G
      ***  20040917 MOTOHASI
           .
       444-KOHNAME-HEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    システム管理マスタ読込
      ******************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    S100LNK-PRTKANRI-SKYYMD     TO  ORC-DBYMD
      ***07/09/20 nagaya 4.0対応 end
           MOVE    "DBSELECT"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY"     TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE    "SYSKANRI-KEY"     TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"        USING
             MOVE    "tbl_syskanri"    TO   MCP-TABLE
             MOVE    "key"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
             IF   MCP-RC   =   ZERO
                 MOVE    ZERO          TO   FLG-SYSKANRI
             ELSE
               MOVE    1               TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO   FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      ******************************************************************
      *    管理マスタ検索(診療科目情報)
      ******************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-SYMD              TO  ORC-DBYMD
      ***07/09/20 nagaya 4.0対応 start
           MOVE    SYS-1005-REC        TO   MCPDATA-REC
           MOVE    "DBSELECT"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key2"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             PERFORM   820-SYSKANRI-DBFETCH-SEC
           ELSE
             MOVE    1                 TO   FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/09/17) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/17) end
           END-IF
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      ******************************************************************
      *     管理マスタ検索(診療科目情報)
      ******************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"       TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    ZERO               TO   FLG-SYSKANRI3
             MOVE    MCPDATA-REC        TO   SYS-1005-REC
           ELSE
             MOVE    1                  TO   FLG-SYSKANRI3
      *=== sarge対応 DEL By Takashima(2006/09/17) start
      *         PERFORM 820-SYSKANRI-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/09/17) end
           END-IF
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      ******************************************************************
      *    ＤＢ　クローズ処理
      ******************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"                TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"        TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"         TO   MCP-TABLE
           MOVE    "key2"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
      ***07/09/20 nagaya 4.0対応 end
                                          MCPAREA
                                          MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                          SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      ******************************************************************
      *    西暦日本語変換処理
      ******************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO   LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO   LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO   WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "  "
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    月末日算出処理
      ******************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD            TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI        TO   WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    全角変換処理
      ******************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO   KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO   KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF   KANACHK-RC   =   ZERO
             MOVE   KANACHK-OUT-INPUT  TO   WRK-OUT-INPUT
           ELSE
             MOVE   WRK-MAE-INPUT      TO   WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      *
      ******************************************************************
      *    エラー出力処理
      ******************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN   INPUT   RECEERR-FILE
           IF   STS-RECEERR   =   ZERO
             CLOSE   RECEERR-FILE
           ELSE
             OPEN    OUTPUT            RECEERR-FILE
      *
             MOVE    WRK-RECEERR       TO   RECEERR-REC
             WRITE   RECEERR-REC
             CLOSE   RECEERR-FILE
      *
      *    ジョブ終了処理
             MOVE    "JBE"             TO   SJOBKANRI-MODE
             INITIALIZE                     JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
             MOVE    S100LNK-HOSPNUM   TO   JOB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
             MOVE    S100LNK-JOBID     TO   JOB-JOBID
             MOVE    S100LNK-SHELLID   TO   JOB-SHELLID
             MOVE    WRK-RECEERR       TO   JOB-YOBI
             MOVE    "9999"            TO   JOB-ERRCD
             CALL    "ORCSJOB"         USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    集計ファイル読込
      ******************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
             READ    S137TMP-FILE
               INVALID    KEY
                 MOVE    1             TO   FLG-S137TMP
               NOT INVALID  KEY
                 MOVE    ZERO          TO   FLG-S137TMP
             END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    管理マスタ読み込み
      ******************************************************************^M
       800-SYSKANRI-READ-SEC           SECTION.
      *
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-SYSYMD          TO  ORC-DBYMD
      ***07/09/20 nagaya 4.0対応 end
           MOVE    "DBSELECT"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key10"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC    =   ZERO
             MOVE    "DBFETCH"         TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE    "SYSKANRI-KEY10"     TO  ORC-DBPATH
      *       CALL    "ORCMCPSUB"         USING
             MOVE    "tbl_syskanri"    TO   MCP-TABLE
             MOVE    "key10"           TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
             IF   MCP-RC   =   ZERO
               MOVE    ZERO            TO   FLG-SYSKANRI
             ELSE
               MOVE    1               TO   FLG-SYSKANRI
             END-IF
           ELSE
             MOVE    1                 TO   FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 Takashima(2006/08/09)
      *      MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *      MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
      *      CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_syskanri"      TO   MCP-TABLE
           MOVE    "key10"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
            .
       800-SYSKANRI-READ-EXT.
            EXIT.
      *
      ******************************************************************
      *    一時ファイル読込
      ******************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  S137TMP-FILE   NEXT
             AT  END
               MOVE    1     TO   FLG-PRINT
             NOT AT END
               ADD     1     TO   CNT-READ
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    請求管理読込
      ******************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE   WRK-PARA-HOSPID       TO    RECE10-HOSPID.
           MOVE    WRK-PARA-HOSPNUM      TO   RECE10-HOSPNUM.
      ***07/09/20 nagaya 4.0対応 end
           MOVE    WRK-PARA-SRYYM        TO   RECE10-SKYYM.
      *
           MOVE    RECE10-REC            TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
      *     MOVE    "SEIKYU-KEY4"       TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PATH            TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_seikyu"          TO   MCP-TABLE
           MOVE    "key3"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      ***07/09/20 nagaya 4.0対応 end
                                         MCPAREA
                                         MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                         SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC    =   ZERO
             PERFORM 900-SEIKYU-READ-SEC
           ELSE
             MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      ******************************************************************
      *    請求マスター読込
      ******************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
      *     MOVE    "SEIKYU-KEY4"       TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PATH            TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
           MOVE    "DBFETCH"             TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
           MOVE    "tbl_seikyu"          TO   MCP-TABLE
           MOVE    "key3"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
      *     CALL    "ORCMCPSUB"          USING
      ***07/09/20 nagaya 4.0対応 end
                                         MCPAREA
                                         MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                         SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC    =   ZERO
             MOVE    ZERO                TO   FLG-END
             MOVE    MCPDATA-REC         TO   RECE10-REC
      *
      *      テスト患者のときは読み飛ばす
             MOVE    SPACE               TO   PTINF-REC
             INITIALIZE                       PTINF-REC
      *=== テスト患者の判定修正 Uchida(2007/05/08)
      *         MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
      *         MOVE    RECE10-PTID         TO  PTINF-PTID
      *         MOVE    PTINF-REC           TO  MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE    RECE10-HOSPID       TO  WRK-PARA-HOSPID
             MOVE    RECE10-HOSPNUM      TO   WRK-PARA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
               MOVE    RECE10-PTID       TO   WRK-PTID
      *=== テスト患者の判定修正 Uchida(2007/05/08)
             PERFORM 900-PTINF-READ-SEC
             IF   FLG-PTINF   =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =  "1"
               GO  TO    900-SEIKYU-READ-SEC
             END-IF
      **    ０９１と０５１を同じ公費として扱う
             PERFORM  VARYING IDX5   FROM 1   BY 1
                      UNTIL   IDX5   >  4
                      OR   RECE10-KOHNUM(IDX5)   =   SPACE
               IF   RECE10-KOHNUM(IDX5)  =  "091"
                 MOVE    "051"           TO   RECE10-KOHNUM(IDX5)
               END-IF
             END-PERFORM
           ELSE
               MOVE    1                 TO   FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      ******************************************************************
      *    請求管理情報クローズ処理
      ******************************************************************
       900-SEIKYU-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "SEIKYU-KEY4"       TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PATH            TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_seikyu"        TO   MCP-TABLE
           MOVE    "key3"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者マスター読込処理
      ******************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           INITIALIZE                   PTINF-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PARA-HOSPID     TO  PTINF-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO   PTINF-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    WRK-PTID             TO   PTINF-PTID
           MOVE    PTINF-REC            TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_ptinf"          TO   MCP-TABLE
           MOVE    "key"                TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
             MOVE    "tbl_ptinf"        TO   MCP-TABLE
             MOVE    "key"              TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC      TO   PTINF-REC
               MOVE    ZERO             TO   FLG-PTINF
             ELSE
               MOVE    1                TO   FLG-PTINF
               INITIALIZE               PTINF-REC
             END-IF
           ELSE
               MOVE    1                TO   FLG-PTINF
               INITIALIZE               PTINF-REC
           END-IF
      *
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE     "DBCLOSECURSOR"     TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
            MOVE    "tbl_ptinf"         TO   MCP-TABLE
            MOVE    "key"               TO   MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD START ----------------------------------------
      *****************************************************************
      *    患者保険情報マスター読込処理
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           INITIALIZE                      PTHKNINF-REC
           MOVE    SYU-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SYU-PTID            TO  PTHKN-PTID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      MCP-RC              =   ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
             MOVE    "key6"            TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"           USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF      MCP-RC            =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
             ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTHKNINF
             INITIALIZE                PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"         TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *   保険組合せ情報セット
      *****************************************************************
       930-HKNCOMBI-SET-SEC  SECTION.
      *
           INITIALIZE  HKNCOMBI-REC
           MOVE  SYU-HOSPNUM            TO  COMB-HOSPNUM
           MOVE  SYU-PTID               TO  COMB-PTID
           MOVE  SYU-HKNCOMBINUM        TO  COMB-HKNCOMBINUM
           MOVE  HKNCOMBI-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"             TO  MCP-FUNC
           MOVE  "tbl_hkncombi"         TO  MCP-TABLE
           MOVE  "key"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"            TO  MCP-FUNC
             MOVE  "tbl_hkncombi"       TO  MCP-TABLE
             MOVE  "key"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC        TO  HKNCOMBI-REC
               MOVE  ZERO               TO  FLG-HKNCOMBI
             ELSE
               INITIALIZE               HKNCOMBI-REC
               MOVE  1                  TO  FLG-HKNCOMBI
             END-IF
      *
           ELSE
             MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE  "tbl_hkncombi"         TO  MCP-TABLE
           MOVE  "key"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       930-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD END   ----------------------------------------
      ******************************************************************
      *    受診履歴情報検索
      ******************************************************************
       920-DBSELECT3-SEC                 SECTION.
      *
           INITIALIZE                 JYURRK-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    WRK-PARA-HOSPID   TO  JYURRK-HOSPID
           MOVE    WRK-PARA-HOSPNUM   TO   JYURRK-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    WRK-NYUGAIKBN      TO   JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-PRTSYMD   TO   JYURRK-SRYYMD
           MOVE    WRK-PARA-PRTLYMD   TO   JYURRK-UPSRYYMD
           MOVE    JYURRK-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "JYURRK-KEY21"    TO   ORC-DBPATH
      *     CALL     "ORCMCPSUB"        USING
           MOVE    "tbl_jyurrk"       TO   MCP-TABLE
           MOVE    "key21"            TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC    =   ZERO
             PERFORM 920-DBFETCH3-SEC
           ELSE
             MOVE    1                TO   FLG-JYURRK
           END-IF
           .
       920-DBSELECT3-EXT.
           EXIT.
      *
      ******************************************************************
      *     受診履歴情報フェッチ
      ******************************************************************
       920-DBFETCH3-SEC                SECTION.
      *
           MOVE    "DBFETCH"        TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "JYURRK-KEY21"    TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"          USING
           MOVE    "tbl_jyurrk"     TO   MCP-TABLE
           MOVE    "key21"          TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"      USING
      ***07/09/20 nagaya 4.0対応 end
                                    MCPAREA
                                    MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                    SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    ZERO           TO   FLG-JYURRK
             MOVE    MCPDATA-REC    TO   JYURRK-REC
      *
      *    テスト患者は対象外
             MOVE    JYURRK-PTID    TO   WRK-PTID
             PERFORM  900-PTINF-READ-SEC
             IF   FLG-PTINF   =   ZERO
                 AND    PTINF-TSTPTNUMKBN  =  "1"
               GO  TO  920-DBFETCH3-SEC
             END-IF
      *
           ELSE
             MOVE    1            TO  FLG-JYURRK
           END-IF
           .
       920-DBFETCH3-EXT.
           EXIT.
      *
      ******************************************************************
      *    受診履歴情報クローズ処理
      ******************************************************************
       920-DBCLOSE3-SEC                 SECTION.
      *
      *=== sarge対応 Takashima(2006/08/09)
      *      MOVE  "DBCLOSE"                TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"         TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE    "JYURRK-KEY21"  TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
           MOVE    "tbl_jyurrk"            TO   MCP-TABLE
           MOVE    "key21"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
      ***07/09/20 nagaya 4.0対応 end
                                           MCPAREA
                                           MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                           SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           .
       920-DBCLOSE3-EXT.
           EXIT.
      *
      ******************************************************************
      *   保険者情報セット
      ******************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  WRK-PARA-HOSPID      TO  HKNJA-HOSPID
           MOVE   WRK-PARA-HOSPNUM     TO   HKNJA-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE   WRK-HKNJANUM         TO   HKNJA-HKNJANUM
           MOVE   HKNJAINF-REC         TO   MCPDATA-REC
           MOVE   "DBSELECT"           TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNJAINF-KEY"       TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE   "tbl_hknjainf"       TO   MCP-TABLE
           MOVE   "key"                TO   MCP-PATHNAME
           CALL   "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "HKNJAINF-KEY"  TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 start
             PERFORM  925-HKNJAINF-READ-SEC
      *     ELSE
           END-IF
      *
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNJAINF-KEY"   TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE    "tbl_hknjainf"     TO   MCP-TABLE
           MOVE    "key"              TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
           .
       920-HKNJAINF-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険者情報読込
      ******************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"           TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL  "ORCMCPSUB"  USING
           MOVE    "tbl_hknjainf"      TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      ***07/09/20 nagaya 4.0対応 end
                                       MCPAREA
                                       MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                       SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE   MCPDATA-REC        TO   HKNJAINF-REC
             MOVE   ZERO               TO   FLG-HKNJAINF
             MOVE   HKNJA-HKNJANAME    TO   WRK-HKNJANAME
           ELSE
             INITIALIZE   HKNJAINF-REC
             MOVE   1                  TO   FLG-HKNJAINF
             MOVE   WRK-HKNJANUM       TO   WRK-HKNJANAME
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    請求点数集計処理
      ******************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO    TO    WRK-KOHSEIKYUGK
                                 WRK-KOHSKJSEIKYUGK
                                 WRK-ZENCOMPFTN
      *--- 2009/01/08 ADD START ----------------------------------------
                                 WRK-FTNFLG
      *--- 2009/01/08 ADD END ------------------------------------------
      *
      ***  20050907 MOTOHASI 公費助成額の算出を修正
           IF   RECE10-RJNHKNNUM  NOT =  SPACE
             MOVE   SYU-KOH-COMPFTN(1)  TO   WRK-ZENCOMPFTN
           ELSE
             MOVE   SYU-SYUCOMPFTN      TO   WRK-ZENCOMPFTN
           END-IF
      ***  20050907 MOTOHASI 公費助成額の算出を修正
      *
           PERFORM  VARYING IDX   FROM 1   BY 1
                    UNTIL   IDX   >  4
                    OR   SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF   SYU-KOH-HKNNUM(IDX)  NOT =  "027"
      *    差額から助成額(公費請求額)を算出
               IF   RECE10-RJNHKNNUM  NOT =  SPACE
      *
      ***  20050907 MOTOHASI 公費助成額の算出を修正
                 IF   SYU-KOH-COMPFTN(IDX) < WRK-ZENCOMPFTN
      ***  20050907 MOTOHASI 公費助成額の算出を修正
      *
                   COMPUTE  WRK-KOHSEIKYUGK  =  SYU-KOH-COMPFTN(1)
                                             -  SYU-KOH-COMPFTN(IDX)
                   COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SKJ-FTN(1)
                                                -  SYU-SKJ-FTN(IDX)
      ***  20050907 MOTOHASI 公費助成額の算出を修正
                   MOVE   SYU-KOH-COMPFTN(IDX)  TO   WRK-ZENCOMPFTN
                 END-IF
      ***  20050907 MOTOHASI 公費助成額の算出を修正
               ELSE
      *
      ***  20050907 MOTOHASI 公費助成額の算出を修正
                 IF   SYU-KOH-COMPFTN(IDX)  <  WRK-ZENCOMPFTN
      ***  20050907 MOTOHASI 公費助成額の算出を修正
      *
      *--- 2009/01/08 ADD+DELETE START ---------------------------------
      *             COMPUTE  WRK-KOHSEIKYUGK  =  SYU-SYUCOMPFTN
      *                                       -  SYU-KOH-COMPFTN(IDX)
      *             COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
      *                                          -  SYU-SKJ-FTN(IDX)
           DISPLAY  "  RECE10-FTNMONEY(1)  =  "RECE10-FTNMONEY(1)
           DISPLAY  "  RECE10-KYURATE  =  "RECE10-KYURATE
                   IF    RECE10-FTNMONEY(1)  =  ZERO
                       AND  RECE10-KYURATE  NOT = ZERO
           DISPLAY "限度なし" RECE10-PTNUM
                     MOVE  SYU-KOH-HKNNUM(IDX)   TO  WRK-KOHNUM
                     PERFORM   950-HKNNUM-SET-SEC
                     IF   HKN-RECEFTNDSPKBN  =  1
           DISPLAY "四捨五入する" 
                       COMPUTE  WRK-KOHSEIKYUGK  =  SYU-SYUCOMPFTN
                                                 -  SYU-KOH-COMPFTN(IDX)
                       COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
                                                    -  SYU-SKJ-FTN(IDX)
                     ELSE
           DISPLAY "四捨五入しない" 
                       COMPUTE  WRK-KOHSEIKYUGK = SYU-SYUCOMPFTN-ENTANI
                                          -  SYU-KOH-COMPFTN-ENTANI(IDX)
                       COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
                                                    -  SYU-SKJ-FTN(IDX)
                     END-IF
                   ELSE
           DISPLAY "限度あり" RECE10-PTNUM    
                     ADD    SYU-KOH-COMPFTN(IDX)  TO  WRK-KOHFTN
                     MOVE   1   TO    WRK-FTNFLG
                     COMPUTE  WRK-KOHSKJSEIKYUGK  =  SYU-SYUSKJ-FTN
                                                 -  SYU-SKJ-FTN(IDX)
                   END-IF
      *--- 2009/01/08 ADD+DELETE END -----------------------------------
      ***  20050907 MOTOHASI 公費助成額の算出を修正
                   MOVE   SYU-KOH-COMPFTN(IDX)  TO   WRK-ZENCOMPFTN
                 END-IF
      ***  20050907 MOTOHASI 公費助成額の算出を修正
               END-IF
      *
             END-IF
           END-PERFORM
      *
      *--- 2009/01/08 ADD START ----------------------------------------
           IF  WRK-FTNFLG  =  1
             COMPUTE  WRK-HKNFTN       =   WRK-HKNFTN
                                       -   WRK-KOHFTN
             INITIALIZE    WRK-KOHFTN
           ELSE
             ADD    WRK-KOHSEIKYUGK     TO   WRK-KOHSEIKYUGK-G
           END-IF
      *--- 2009/01/08 ADD END ------------------------------------------
      *--- 2009/01/08 DELETE START -------------------------------------
      *     ADD    WRK-KOHSEIKYUGK       TO   WRK-KOHSEIKYUGK-G
      *--- 2009/01/08 DELETE END ---------------------------------------
           ADD    WRK-KOHSKJSEIKYUGK    TO   WRK-KOHSEIKYUGK-G
      *--- 2008/12/23 CHANGE + ADD START -------------------------------
      *     ADD    SYU-SKYMONEY        TO   WRK-MADOSEIKYUGK-G
           COMPUTE   WRK-SKYMONEY    =   SYU-SKYMONEY
                                     -   SYU-JIHI-TOTAL
                                     -   SYU-JIHI-TOTAL-TAX
           ADD    WRK-SKYMONEY          TO   WRK-MADOSEIKYUGK-G
           ADD    SYU-JIHI-TOTAL        TO   WRK-JIHISEIKYUGK-G
           ADD    SYU-JIHI-TOTAL-TAX    TO   WRK-JIHISEIKYUGK-G
      *--- 2008/12/23 CHANGE + ADD END ---------------------------------
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      ******************************************************************
      *    請求点数集計処理(公費単独)  20040819  @@@@-MOTOHASI
      ******************************************************************
       900-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO    TO    WRK-KOHSKJSEIKYUGK
      *
           PERFORM  VARYING IDX   FROM 1   BY 1
                    UNTIL    IDX  >   4
      *    公費請求額(食事標準負担額)を算出
             IF  SYU-KOH-HKNNUM(IDX)  NOT =  SPACE
               MOVE   SYU-SKJ-FTN-RECE(IDX)  TO   WRK-KOHSKJSEIKYUGK
               MOVE   4                 TO   IDX
             END-IF
           END-PERFORM
      *
           ADD    WRK-KOHSKJSEIKYUGK    TO   WRK-KOHSEIKYUGK-G
      *--- 2008/12/18 CHANGE + ADD START -------------------------------
      *     ADD    SYU-SKYMONEY        TO   WRK-MADOSEIKYUGK-G
      *    窓口請求額集計
           COMPUTE   WRK-SKYMONEY   =   SYU-SKYMONEY
                                    -   SYU-JIHI-TOTAL
                                    -   SYU-JIHI-TOTAL-TAX
           ADD    WRK-SKYMONEY          TO   WRK-MADOSEIKYUGK-G
      *    自費集計
           ADD    SYU-JIHI-TOTAL        TO   WRK-JIHISEIKYUGK-G
           ADD    SYU-JIHI-TOTAL-TAX    TO   WRK-JIHISEIKYUGK-G
      *--- 2008/12/18 CHANGE + ADD END ---------------------------------
      *
           .
       900-SEIKYU-SEC-EXT.
           EXIT.
      *
      ******************************************************************
      *  収納情報セット
      ******************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
           INITIALIZE                 SYUNOU-REC
      *--- 2009/01/08 ADD START ----------------------------------------
                                      WRK-HKNFTN
           MOVE   RECE10-FTNMONEY(1)  TO   WRK-HKNFTN
      *--- 2009/01/08 ADD END ------------------------------------------
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  RECE10-HOSPID      TO  SYU-HOSPID
           MOVE    RECE10-HOSPNUM     TO   SYU-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    RECE10-NYUGAIKBN   TO   SYU-NYUGAIKBN
           MOVE    RECE10-PTID        TO   SYU-PTID
           MOVE    SYUNOU-REC         TO   MCPDATA-REC
           MOVE    "DBSELECT"         TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "SYUNOU-KEY2"      TO  ORC-DBPATH
      *       CALL  "ORCMCPSUB"        USING
           MOVE    "tbl_syunou"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
      ***07/09/20 nagaya 4.0対応 start
      *         MOVE  "SYUNOU-KEY2"  TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
             PERFORM   925-SYUNOU-READ-SEC
      *
             PERFORM   UNTIL  FLG-SYUNOU   =   1
      *--- 2009/06/26 ADD START ----------------------------------------
               IF       SYU-TOTAL-HKNTEN =  0
                 AND    SYU-TOTAL-MONEY             =  0
                 AND  ( SYU-JIHI-TOTAL         NOT  =  0
                 OR     SYU-JIHI-TOTAL-TAX     NOT  =  0 
                 OR     SYU-TOTAL-TGMONEY      NOT  =  0
                 OR     SYU-TOTAL-TGMONEY-TAX  NOT  =  0 )
                 CONTINUE
               ELSE
      *--- 2009/06/26 ADD START ----------------------------------------
               IF  RECE10-SRYYM   =   SYU-SRYYMD(1:6)
                   AND   SYU-DENPJTIKBN   NOT =   "3"
                   AND   RECE10-TEKSTYMD  <=  SYU-SRYYMD
      *
      ***  20050412  @@@@-MOTOHASI
                   AND   SYU-SYUHKNNUM  =  RECE10-HKNNUM
      ***  20050412  @@@@-MOTOHASI
      *
      *    旧総合病院の時は該当する診療科のみ対象にする
      *--- 2008/10/30  CHANGE  ADD  START ------------------------------
      * 20081014 主科設定時に診療科を印字するSTART
                 IF   SYS-1001-HOSPSBT1  NOT =  1
                     OR   RECE10-NYUGAIKBN  =  "1"
      *              IF    RECE10-NYUGAIKBN      = "1" OR
      * 20081014 主科設定時に診療科を印字するEND
      *--- 2008/10/30  CHANGE  ADD  END --------------------------------
                     OR   RECE10-RECEKA  =  SYU-SRYKA
      *
      ***  20040819  @@@@-MOTOHASI  公費単独時の集計追加
                   IF    RECE10-KOHNUM(1)  NOT =   SPACE
                       AND RECE10-HKNNUM   =   SPACE
                     PERFORM  900-SEIKYU-SEC
                   ELSE
                     PERFORM  800-SEIKYU-SEC
                   END-IF
      ***  20040819  @@@@-MOTOHASI  ここまで
      *
                 END-IF
               END-IF
      *
      ***07/09/20 nagaya 4.0対応 start
      *           MOVE  "SYUNOU-KEY2"  TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
      *--- 2009/06/26 ADD START ----------------------------------------
               END-IF
      *--- 2009/06/26 ADD START ----------------------------------------
               PERFORM   925-SYUNOU-READ-SEC
             END-PERFORM
      *       ELSE
           END-IF
      *
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *       MOVE  "DBCLOSE"      TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"    TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "SYUNOU-KEY2"  TO  ORC-DBPATH
      *       CALL  "ORCMCPSUB"  USING
           MOVE    "tbl_syunou"       TO   MCP-TABLE
           MOVE    "key2"             TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
                                      MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *--- 2009/01/08 ADD START ----------------------------------------
           IF  WRK-FTNFLG  =  1
             ADD    WRK-HKNFTN     TO   WRK-KOHSEIKYUGK-G
           END-IF
      *--- 2009/01/08 ADD END ------------------------------------------
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    収納読込
      ******************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
             MOVE    "DBFETCH"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL  "ORCMCPSUB"  USING
             MOVE    "tbl_syunou"       TO   MCP-TABLE
             MOVE    "key2"             TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC        TO   SYUNOU-REC
             MOVE    ZERO               TO   FLG-SYUNOU
           ELSE
             INITIALIZE     SYUNOU-REC
             MOVE    1                  TO   FLG-SYUNOU
           END-IF
      *
           .
       925-SYUNOU-READ-EXT.
           EXIT.
      *
      *--- 2009/06/26 ADD START ----------------------------------------
      *****************************************************************
      *    収納情報READ自費・自賠・労災用(外来)
      *****************************************************************
       940-SYUNOU-READ-SEC              SECTION.
      *
           INITIALIZE                   SYUNOU-REC
           MOVE  JYURRK-HOSPNUM         TO  SYU-HOSPNUM
           MOVE  JYURRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE  JYURRK-PTID            TO  SYU-PTID
           MOVE  JYURRK-DENPNUM         TO  SYU-DENPNUM
           MOVE  SYUNOU-REC             TO  MCPDATA-REC
           MOVE  "DBSELECT"             TO  MCP-FUNC
           MOVE  "tbl_syunou"           TO  MCP-TABLE
           MOVE  "key"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"            TO  MCP-FUNC
             MOVE  "tbl_syunou"         TO  MCP-TABLE
             MOVE  "key"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  MCPDATA-REC        TO  SYUNOU-REC
               MOVE  ZERO               TO  FLG-SYUNOU
             ELSE
               INITIALIZE  SYUNOU-REC
               MOVE  1                  TO  FLG-SYUNOU
             END-IF

           ELSE
             MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE  "tbl_syunou"           TO  MCP-TABLE
           MOVE  "key"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       940-SYUNOU-READ-EXT.
           EXIT.
      *--- 2009/06/26 ADD END   ----------------------------------------
      ******************************************************************
      *    保険番号情報セット
      ******************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE                   HKNNUM-REC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  WRK-PARA-HOSPID      TO  HKN-HOSPID
           MOVE    WRK-PARA-HOSPNUM     TO   HKN-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    WRK-KOHNUM           TO   HKN-HKNNUM
           MOVE    HKNNUM-REC           TO   MCPDATA-REC
           MOVE    "DBSELECT"           TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNNUM-KEY2"        TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
           MOVE    "tbl_hknnum"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
      ***07/09/20 nagaya 4.0対応 start
      *       MOVE  "HKNNUM-KEY2"      TO  ORC-DBPATH
      ***07/09/20 nagaya 4.0対応 end
             PERFORM   955-HKNNUM-READ-SEC
      * 20080526 古い公費名を印字してしまう不具合を修正START tanaka
           ELSE
             MOVE    1       TO     FLG-HKNNUM
      * 20080526 古い公費名を印字してしまう不具合を修正END
           END-IF
      *
      * 20080526 古い公費名を印字してしまう不具合を修正START tanaka
           PERFORM   UNTIL  FLG-HKNNUM   =   1
             IF   HKN-TEKSTYMD   <=   WRK-SRYYMD
                 AND   HKN-TEKEDYMD   >=   WRK-SRYYMD
               MOVE  1        TO  FLG-HKNNUM
             END-IF
      *
             IF   FLG-HKNNUM   =   ZERO
               MOVE    "tbl_hknnum"     TO   MCP-TABLE
               MOVE    "key2"           TO   MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
             END-IF
           END-PERFORM
      * 20080526 古い公費名を印字してしまう不具合を修正END
      *
           INITIALIZE  MCPAREA
      *=== sarge対応 Takashima(2006/08/09)
      *     MOVE  "DBCLOSE"        TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"      TO   MCP-FUNC
      *=== sarge対応 Takashima(2006/08/09)
      ***07/09/20 nagaya 4.0対応 start
      *     MOVE  "HKNNUM-KEY2"    TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"      USING
           MOVE    "tbl_hknnum"         TO   MCP-TABLE
           MOVE    "key2"               TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
                                        MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                        SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険番号情報読込
      ******************************************************************
       955-HKNNUM-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"          TO   MCP-FUNC
      ***07/09/20 nagaya 4.0対応 start
      *     CALL  "ORCMCPSUB"  USING
           MOVE  "tbl_hknnum"       TO   MCP-TABLE
           MOVE  "key2"             TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                    MCPAREA
                                    MCPDATA-REC
      ***07/09/20 nagaya 4.0対応 start
                                    SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
           IF   MCP-RC   =   ZERO
             MOVE    MCPDATA-REC    TO   HKNNUM-REC
             MOVE    ZERO           TO   FLG-HKNNUM
           ELSE
             INITIALIZE    HKNNUM-REC
             MOVE    1              TO   FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    終了　　処理
      ******************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"              TO   SJOBKANRI-MODE
           INITIALIZE                 JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
           MOVE    S100LNK-HOSPNUM    TO   JOB-HOSPNUM
      ***07/09/20 nagaya 4.0対応 end
           MOVE    S100LNK-JOBID      TO   JOB-JOBID
           MOVE    S100LNK-SHELLID    TO   JOB-SHELLID
           MOVE    CNT-PAGE           TO   JOB-UPDCNT
           CALL    "ORCSJOB"          USING
                                      ORCSJOBKANRIAREA
                                      JOBKANRI-REC
      ***07/09/20 nagaya 4.0対応 start
                                      SPA-AREA
      ***07/09/20 nagaya 4.0対応 end
      *     PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      ******************************************************************
      *    ＤＢ　オープン処理
      ******************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"           TO   MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"        USING
           MOVE    LOW-VALUE          TO   MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                MCPDATA-REC.
                                      MCPDATA-REC
                                      SPA-AREA
                                        .
      ***07/09/20 nagaya 4.0対応 end
      *
           MOVE    "DBSTART"          TO   MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"        USING
           MOVE    LOW-VALUE          TO   MCP-TABLE
                                           MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
      ***07/09/20 nagaya 4.0対応 end
                                      MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                MCPDATA-REC.
                                      MCPDATA-REC
                                      SPA-AREA
                                      .
      ***07/09/20 nagaya 4.0対応 end
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      ******************************************************************
      *    ＤＢ　クローズ処理
      ******************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO   MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/20 nagaya 4.0対応 end
      *
           MOVE    "DBDISCONNECT"       TO   MCP-FUNC.
      ***07/09/20 nagaya 4.0対応 start
      *     CALL    "ORCMCPSUB"          USING
           MOVE    LOW-VALUE            TO   MCP-TABLE
                                             MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      ***07/09/20 nagaya 4.0対応 end
                                        MCPAREA
      ***07/09/20 nagaya 4.0対応 start
      *                                  MCPDATA-REC.
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      ***07/09/20 nagaya 4.0対応 end
           .
       900-DBCLOSE-EXT.
           EXIT.
