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
       PROGRAM-ID.             A00000L109.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者一覧
      *  コンポーネント名  : (診療行為)患者一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/02/20    @@@@-高橋     新規作成
      *  04/09/17    @@@@-本橋     MONPE化
      *  04/10/06    @@@@-本橋     日医標準レセプトソフトver2.4.0対応による
      *                            受診履歴テーブル−最終区分(lastkbn)の削除
      *  04/12/06    @@@@-本橋     有効期限が設定された医師コードにおいて
      *                            シス管理より医師名が取得できない不具合を修正
      *  05/01/18    @@@@-本橋     記号もしくは番号のどちらかが未入力であっても
      *                            印字するように修正
      *  05/03/10    @@@@-鈴木     open-cobol 0.23への対応による変更
      *                            monpe文字化け修正
      *  06/08/09    @@@@-高嶋     sarge対応
      *  07/09/20    @@@@-吉川     Ver4.0対応
      *  08/04/03    @@@@-吉川     シス管の値が取得できない不具合を修正
      *                            後期高齢者対応(ver4.2.0)
      *                            記号・番号１６文字以上対応(ver4.2.0)
      *  08/06/02    @@@@-田中     診療行為の欄が空白になる不具合を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *    初診患者抽出一時ファイル
           SELECT  L109T-FILE      ASSIGN  L109TPARA
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     L109T-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *    初診患者抽出一時ファイル
       FD  L109T-FILE.
           COPY    "A00000L109TMP.INC".
      *
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域 
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           COPY    "A00000L109.INC".
      *
       01  L109TPARA.
           03  FILLER              PIC  X(05)    VALUE "/tmp/".
           03  L109TPARA-FILENAME  PIC  X(10) VALUE "A00000L109".
           03  FILLER              PIC  X(04) VALUE ".TMP".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SRYKA-ALL       PIC 9(01).
           03  FLG-DRCD-ALL        PIC 9(01).
           03  FLG-SRYCD-ALL       PIC 9(01).
           03  FLG-PRSORT          PIC X(01).
           03  FLG-GET             PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-ATEND           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI2       PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTBYO           PIC 9(01).
           03  FLG-BYO             PIC 9(01).
           03  FLG-F-JYURRK        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  INP-STS             PIC 9(04).
           03  FLG-11DUMMY         PIC 9(01).
           03  FLG-12DUMMY         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  WRK-PRDTLCNT        PIC 9(05).
           03  CNT-BYO             PIC 9(05).
           03  CNT-DRCD            PIC 9(05).
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
           03  SELKA-IDX         PIC 9(04).
           03  IDX1              PIC 9(04).
           03  IDX2              PIC 9(04).
           03  IDX3              PIC 9(04).
           03  IDX4              PIC 9(04).
           03  IDX5              PIC 9(04).
           03  IDX6              PIC 9(04).
           03  IDX7G.    
              05  IDX7           PIC 9(02).
           03  IDX8G.    
              05  IDX8           PIC 9(02).
           03  DIDXG.    
              05  DIDX           PIC 9(02).
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
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID    PIC 9(07).
           03  WRK-PARA-SHELLID  PIC X(08).
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     03  WRK-PARA-HOSPID   PIC X(24).
           03  WRK-PARA-HOSPNUM  PIC 9(02).
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           03  WRK-PARA-PRTYM    PIC X(06).
           03  WRK-PARA-PRTLYMD  PIC X(08).
           03  WRK-PARA-PRTSYMD  PIC X(08).
           03  WRK-PARA-SRYKA    PIC X(02).
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
      *
      *****03  WRK-IN                PIC X(01).
      *
           03  WRK-MONEY             PIC 9(08).
           03  WRK-JIHI              PIC 9(08).
           03  WRK-SYUEKI            PIC 9(08).
      *
           03  WRK-JIHI2            PIC 9(08).
           03  WRK-TJIHI-G       OCCURS  5.
               05  WRK-TJIHI        PIC X(08).
               05  WRK-TJIHI-FLG    PIC 9(01).
           03  WRK-TJIHIN-G       OCCURS  5.
               05  WRK-TJIHIN       PIC X(08).
               05  WRK-TJIHIN-FLG   PIC 9(01).
      *
           03  WRK-4001-TENTANKA PIC 9(02)V9(01).
      *
           03  WRK-RENNUM-X         PIC ZZ9.
           03  WRK-PAGE             PIC ZZ9.
           03  WRK-Z3               PIC ZZ9.
           03  WRK-Z5               PIC ZZZZ9.
           03  WRK-S9-3             PIC --9.
           03  WRK-Z9-5             PIC ZZ,ZZ9.
           03  WRK-Z9               PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ               PIC ZZZZZZZZZZZZ.
           03  WRK-S9               PIC -----------9.
           03  WRK-SZ               PIC -----------Z.
           03  WRK-NYUGAIKBN        PIC X(01).
           03  WRK-SRYKA            PIC X(02).
           03  WRK-SRYKANM          PIC X(30).
           03  WRK-PRDRCD           PIC X(05).
           03  WRK-PRSRYCD          PIC X(18).
           03  WRK-L100LNK-YMD1     PIC X(22).
           03  WRK-L100LNK-YMD2     PIC X(22).
           03  WRK-KOH1FTNNUM       PIC X(30).
           03  WRK-KOH2FTNNUM       PIC X(30).
           03  WRK-KOH3FTNNUM       PIC X(30).
           03  WRK-KIGONUM          PIC X(80).
           03  WRK-NUM              PIC X(80).
           03  WRK-HEN-KIGONUM      PIC X(80).
           03  WRK-BIRTHDAY.
            05 WRK-BIRTHY           PIC 9(04).
            05 WRK-BIRTHM           PIC 9(02).
            05 WRK-BIRTHD           PIC 9(02).
           03  WRK-F-SRYYMD         PIC X(08).
           03  WRK-FG-SRYYMD        PIC X(08).
           03  WRK-FN-SRYYMD        PIC X(08).
           03  WRK-NENREI           PIC 9(03).
           03  WRK-SRYYMDDMY        PIC 9(10).
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU      PIC X(02)   OCCURS   10.
           03  WRK-MOJI             PIC X(10).
           03  WRK-MOJISU           PIC 9(02).
           03  WRK-MAE-INPUT        PIC X(200).
           03  WRK-OUT-INPUT        PIC X(200).
           03  WRK-PTID             PIC 9(10).
           03  DEBUG-CNT            PIC 9(03).
      *    SQL用
           03  WRK-SQL              PIC X(20000).
           03  WRK-SQL-PNT          PIC 9(05).
           03  WRK-SQL-COUNT        PIC X(20000).
           03  WRK-SQLC             PIC 9(05).
      *
      * 2003/06/24  追加
           03  WRK-SYS-BEDSU     PIC 9(04).
           03  WRK-SYS-BEDSUIPN  PIC 9(04).
      * 2003/06/24  追加
      *
       01  WRK-SRYKA-AREA.
           03  WRK-SRYKAMAX        PIC    9(03).
           03  WRK-SRYKA-TBL       OCCURS 100.    
               05  WRK-SRYKA-CD    PIC    X(02).
               05  WRK-SRYKANAME   PIC    X(30).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973". 
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     COPY    "ORCA-DBPATH".
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY   "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY   "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY   "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
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
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
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
      *    診療行為情報
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴情報
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    受診履歴情報(初来院受診履歴用)
       01  JYURRK-F-REC.
           COPY    "CPJYURRK.INC"
           REPLACING  //JYURRK-// BY //JYURRK-F-//.
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者入院履歴情報
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者病名情報
       01  PTBYO-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者最終来院日
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    点数マスタ
      * 01  TNS-TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    病名マスタ
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    患者マスタ（照会用コピー句）
       01  QUERY-PTINF-REC.
           COPY    "CPQPTINF.INC".
      *
      **    患者マスタ件数取得用（照会用コピー句）
      * 01  QUERY-PTINF-CNT-REC.
      *     COPY    "CPQPTINF-CNT.INC".
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "A00000L100LNK.INC".
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING       A00000L100-LNK.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF      INP-STS                 =   ZERO
             PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *     ELSE
      *       PERFORM 210-ERR-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
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
           INITIALIZE                  WRK-SRYKA-AREA
           INITIALIZE                  L109T
           INITIALIZE                  L109
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
           INITIALIZE                  SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
           MOVE    L100LNK-HOSPNUM     TO  JOB-HOSPNUM
                                           SPA-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    L100LNK-JOBID       TO  JOB-JOBID
           MOVE    L100LNK-SHELLID     TO  JOB-SHELLID
           MOVE    "A00000L109"        TO  JOB-PGID
           MOVE    "(診療行為)患者一覧"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
                                   SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    L100LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           PERFORM 100-DBOPEN-SEC
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
           MOVE    L100LNK-PRTKANRI-SKYYMD TO WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG             TO WRK-SYSYMDWH 
           IF      L100LNK-YMD1       =     SPACE
                MOVE   L100LNK-PRTKANRI-SKYYMD   TO  L100LNK-YMD1
           END-IF
           IF      L100LNK-YMD2       =     SPACE
                MOVE   L100LNK-PRTKANRI-SKYYMD   TO  L100LNK-YMD2
           END-IF
      *
      *    対象年月日１編集
           INITIALIZE  WRK-SYMD
           MOVE    L100LNK-YMD1        TO    WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO    WRK-L100LNK-YMD1 
      *
      *    対象年月日２編集
           INITIALIZE  WRK-SYMD
           MOVE    L100LNK-YMD2        TO    WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO    WRK-L100LNK-YMD2 
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *     MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                                 TO  SYS-1001-STYUKYMD(1:6)
      *                                     SYS-1001-EDYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
      *                                     SYS-1001-EDYUKYMD(7:2)
           MOVE    L100LNK-YMD1        TO  SYS-1001-STYUKYMD
           MOVE    L100LNK-YMD2        TO  SYS-1001-EDYUKYMD
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
               MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               MOVE    SYS-1001-BEDSU      TO  WRK-SYS-BEDSU
               MOVE    SYS-1001-BEDSUIPN   TO  WRK-SYS-BEDSUIPN
           ELSE    
               MOVE    "医療機関ＩＤが取得できませんでした。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
      *
      *    診療科情報
           IF L100LNK-SRYKA   =    SPACE
      ***  全診療印刷フラグON
              MOVE   1        TO   FLG-SRYKA-ALL
           ELSE
              INITIALIZE                      SYS-1005-REC
              MOVE    "1005"              TO  SYS-1005-KANRICD
              MOVE    L100LNK-SRYKA       TO  SYS-1005-KBNCD
              MOVE    L100LNK-YMD2        TO  WRK-SYMD
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *        MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                                    TO  SYS-1005-STYUKYMD(1:6)
      *                                        SYS-1005-EDYUKYMD(1:6)
      *        MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
      *                                        SYS-1005-EDYUKYMD(7:2)
              MOVE    L100LNK-YMD1        TO  SYS-1005-STYUKYMD
              MOVE    L100LNK-YMD2        TO  SYS-1005-EDYUKYMD
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
              MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
              MOVE    SYS-1005-REC        TO  MCPDATA-REC
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                MOVE  MCPDATA-REC         TO  SYS-1005-REC
              ELSE
      ***       診療科コード入力値エラーの場合
                MOVE  1                   TO  INP-STS
                MOVE    "診療科コードの値が不正です。"
                                           TO  WRK-RECEERR
                PERFORM 500-ERR-HENSYU-SEC
              END-IF                                     
           END-IF
      *
      *    医師情報
           IF L100LNK-DRCD    =    SPACE
      ***  全診療印刷フラグON
              MOVE   1        TO   FLG-DRCD-ALL
           ELSE
              INITIALIZE                      SYS-1010-REC
              MOVE    "1010"              TO  SYS-1010-KANRICD
              MOVE    L100LNK-DRCD        TO  SYS-1010-KBNCD
      ***  2004/12/06  MOTO
      ***        MOVE    L100LNK-YMD2        TO  WRK-SYMD
              MOVE    L100LNK-YMD2        TO  WRK-SYSYMD
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *        MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                                    TO  SYS-1010-STYUKYMD(1:6)
      *                                        SYS-1010-EDYUKYMD(1:6)
      *        MOVE    "01"                TO  SYS-1010-STYUKYMD(7:2)
      *                                        SYS-1010-EDYUKYMD(7:2)
              MOVE    L100LNK-YMD1        TO  SYS-1010-STYUKYMD
              MOVE    L100LNK-YMD2        TO  SYS-1010-EDYUKYMD
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
              MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
              MOVE    SYS-1010-REC        TO  MCPDATA-REC
              PERFORM 800-SYSKANRI-READ-SEC
              IF      FLG-SYSKANRI        =   ZERO
                MOVE  MCPDATA-REC       TO  SYS-1010-REC
              ELSE
      ***      医師コード入力値エラーの場合
                MOVE  2                  TO  INP-STS
                MOVE    "医師コードの値が不正です。"
                                           TO  WRK-RECEERR
                PERFORM 500-ERR-HENSYU-SEC
              END-IF                                     
           END-IF
      *
      *    診療行為情報
           IF L100LNK-SELCD    =    SPACE
      ***  全病名印刷フラグON
      *        MOVE   1        TO   FLG-SRYCD-ALL
               MOVE    "診療行為コードが入力されていません。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    明細出力順序
           IF      L100LNK-PRSORT    =   SPACE
             OR    L100LNK-PRSORT    =   "0"
             OR    L100LNK-PRSORT    =   "1"
             OR    L100LNK-PRSORT    =   "2"
               MOVE L100LNK-PRSORT    TO  FLG-PRSORT
           ELSE
      ***      明細出力順序コード入力値エラーの場合
              MOVE 3                 TO  INP-STS
              MOVE    "印字順序の値が不正です。"
                                         TO  WRK-RECEERR
              PERFORM 500-ERR-HENSYU-SEC
           END-IF                                     
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    診療行為患者抽出
           PERFORM 320-JYURRK-11GET-SEC
      *    診療行為患者印刷
           PERFORM 330-PRT-MAIN-SEC
      *
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
           INITIALIZE                     L109
      *
           ADD     1                 TO   CNT-PAGE
           MOVE    CNT-PAGE          TO   WRK-PAGE
           MOVE    WRK-PAGE          TO   L109-PAGE
           MOVE    "(診療行為)"          TO   L109-MIDASI
           MOVE    WRK-SYSYMDWH      TO   L109-SYSYMD
           MOVE    WRK-L100LNK-YMD1  TO   L109-YMD1
           MOVE    WRK-L100LNK-YMD2  TO   L109-YMD2
           EVALUATE  INP-STS
              WHEN 1
                MOVE "入力された診療科コードの値が不正です。"
                                     TO   L109-ERRMSG
              WHEN 2
                MOVE "入力された医師コードの値が不正です。"
                                     TO   L109-ERRMSG
              WHEN 3
                MOVE "入力された明細出力順序コードの値が不正です。"
                                     TO   L109-ERRMSG
           END-EVALUATE
           PERFORM 340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴 患者情報抽出処理
      *****************************************************************
       320-JYURRK-11GET-SEC      SECTION.
      *
           OPEN     OUTPUT         L109T-FILE
           CLOSE                   L109T-FILE
           OPEN     I-O            L109T-FILE
      *
      *    診療行為検索該当患者読み込み  [key21使用] 
      *    外来
            INITIALIZE              JYURRK-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *      MOVE L100LNK-HOSPID TO  JYURRK-HOSPID
            MOVE L100LNK-HOSPNUM TO  JYURRK-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
            MOVE "2"            TO  JYURRK-NYUGAIKBN
            MOVE L100LNK-YMD1   TO  JYURRK-SRYYMD 
            MOVE L100LNK-YMD2   TO  JYURRK-UPSRYYMD 
            PERFORM  900-JYURRK-SELECT-SEC
            PERFORM  900-JYURRK-FETCH-SEC
            PERFORM  UNTIL   FLG-JYURRK  =   1
                  PERFORM  320-PTBYO-11GET-SEC
                  PERFORM  900-JYURRK-FETCH-SEC
            END-PERFORM
            PERFORM   900-JYURRK-CLOSE-SEC
      *    入院
            INITIALIZE              JYURRK-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *      MOVE L100LNK-HOSPID TO  JYURRK-HOSPID
            MOVE L100LNK-HOSPNUM TO  JYURRK-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
            MOVE "1"            TO  JYURRK-NYUGAIKBN
            MOVE L100LNK-YMD1   TO  JYURRK-SRYYMD 
            MOVE L100LNK-YMD2   TO  JYURRK-UPSRYYMD 
            PERFORM  900-JYURRK-SELECT-SEC
            PERFORM  900-JYURRK-FETCH-SEC
            PERFORM  UNTIL   FLG-JYURRK  =   1
                  PERFORM  320-PTBYO-11GET-SEC
                  PERFORM  900-JYURRK-FETCH-SEC
            END-PERFORM
            PERFORM   900-JYURRK-CLOSE-SEC
      *
           CLOSE                   L109T-FILE
      *
           .
       320-JYURRK-11GET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為 患者情報抽出処理
      *****************************************************************
       320-PTBYO-11GET-SEC      SECTION.
      *
      ***  20041006  Ver 2.4.0対応による削除  MOTO
      ***     IF       JYURRK-LASTKBN    =      "1"
            IF     JYURRK-EDANUM     =      "1"
            AND     JYURRK-RENNUM     =      "1"
            AND (( FLG-SRYKA-ALL      NOT =  "1"
              AND  JYURRK-SRYKA       =      L100LNK-SRYKA )
             OR    FLG-SRYKA-ALL      =      "1" )
            AND (( FLG-DRCD-ALL       NOT =  "1"
              AND  JYURRK-DRCD        =      L100LNK-DRCD )
             OR    FLG-DRCD-ALL       =      "1" )
      *         where SQL 作成
                INITIALIZE              WRK-SQL
      *
                MOVE    1               TO  WRK-SQL-PNT 
                STRING  ",view_q002 b"         DELIMITED   BY  SIZE
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *               " where a.hospid='"       DELIMITED   BY  SIZE
      *               L100LNK-HOSPID            DELIMITED   BY  SIZE
                     " where a.hospnum='"      DELIMITED   BY  SIZE
                     L100LNK-HOSPNUM           DELIMITED   BY  SIZE
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
                     "' "                      DELIMITED   BY  SIZE
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *               " and  b.hospid='"       DELIMITED   BY  SIZE
      *               L100LNK-HOSPID            DELIMITED   BY  SIZE
                     " and  b.hospnum='"       DELIMITED   BY  SIZE
                     L100LNK-HOSPNUM           DELIMITED   BY  SIZE
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
                     "' "                      DELIMITED   BY  SIZE
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *               " and  h.hospid='"       DELIMITED   BY  SIZE
      *               L100LNK-HOSPID            DELIMITED   BY  SIZE
                     " and  h.hospnum='"       DELIMITED   BY  SIZE
                     L100LNK-HOSPNUM           DELIMITED   BY  SIZE
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
                     "' "                      DELIMITED   BY  SIZE
                     "and (a.ptid = "          DELIMITED   BY  SIZE
                     JYURRK-PTID               DELIMITED   BY  SIZE
                     ") "                      DELIMITED   BY  SIZE
                     "and (b.ptid = "          DELIMITED   BY  SIZE
                     JYURRK-PTID               DELIMITED   BY  SIZE
                     ") "                      DELIMITED   BY  SIZE
                     "and (h.ptid = "          DELIMITED   BY  SIZE
                     JYURRK-PTID               DELIMITED   BY  SIZE
                     ") "                      DELIMITED   BY  SIZE
      *               "and (h.hospid = a.hospid) "  DELIMITED   BY  SIZE
      *               "and (b.hospid = a.hospid) "  DELIMITED   BY  SIZE
      *               "and (h.ptid = a.ptid) "  DELIMITED   BY  SIZE
      *               "and (b.ptid = a.ptid) "  DELIMITED   BY  SIZE
                     " and (b.nyugaikbn ="     DELIMITED   BY  SIZE
                     JYURRK-NYUGAIKBN          DELIMITED   BY  SIZE
                     ") "                      DELIMITED   BY  SIZE
                     " and (b.sryymd ="        DELIMITED   BY  SIZE
                     JYURRK-SRYYMD             DELIMITED   BY  SIZE
                     ") "                      DELIMITED   BY  SIZE
                     " and (b.sryka ='"        DELIMITED   BY  SIZE
                     JYURRK-SRYKA              DELIMITED   BY  SIZE
                     "') "                      DELIMITED   BY  SIZE
                     " and ("                  DELIMITED   BY  SIZE
                     "(b.srycd1 = '"           DELIMITED   BY  SIZE
                     L100LNK-SELCD             DELIMITED   BY  SIZE
                     "') or "                  DELIMITED   BY  SIZE
                     "(b.srycd2 = '"           DELIMITED   BY  SIZE
                     L100LNK-SELCD             DELIMITED   BY  SIZE
                     "') or "                  DELIMITED   BY  SIZE
                     "(b.srycd3 = '"           DELIMITED   BY  SIZE
                     L100LNK-SELCD             DELIMITED   BY  SIZE
                     "') or "                  DELIMITED   BY  SIZE
                     "(b.srycd4 = '"           DELIMITED   BY  SIZE
                     L100LNK-SELCD             DELIMITED   BY  SIZE
                     "') or "                  DELIMITED   BY  SIZE
                     "(b.srycd5 = '"           DELIMITED   BY  SIZE
                     L100LNK-SELCD             DELIMITED   BY  SIZE
                     "')) and "                DELIMITED   BY  SIZE
                     "(a.tstptnumkbn <> '1')"  DELIMITED   BY  SIZE
                INTO    WRK-SQL
                WITH    POINTER WRK-SQL-PNT
                END-STRING
      *
                PERFORM 910-PTINF-SELECT-SEC 
                PERFORM 910-PTINF-READ-SEC     
                PERFORM  UNTIL   FLG-PTINF  =   1
                     PERFORM 320-WRITE-TEMP-SEC     
                     PERFORM 910-PTINF-READ-SEC     
                END-PERFORM
                PERFORM 910-PTINF-CLOSE-SEC
      **                       PERFORM  320-PTBYO-11GET-SEC
           END-IF
                .
       320-PTBYO-11GET-EXT.
           EXIT.
      *****************************************************************
      *    件数判別処理
      *****************************************************************
       320-WRITE-TEMP-SEC      SECTION.
      *
      * 患者情報抽出
           INITIALIZE                      PTINF-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE     L100LNK-HOSPID     TO  PTINF-HOSPID
           MOVE     L100LNK-HOSPNUM    TO  PTINF-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE     QUERY-PTINF-PTID   TO  PTINF-PTID
           PERFORM  900-PTINF-READ-SEC
      * 患者番号情報抽出
           INITIALIZE                      PTNUM-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE     L100LNK-HOSPID     TO  PTNUM-HOSPID
           MOVE     L100LNK-HOSPNUM    TO  PTNUM-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE     QUERY-PTINF-PTID   TO  PTNUM-PTID
           PERFORM  940-PTNUM-READ-SEC
      * 保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    L100LNK-HOSPID      TO  COMB-HOSPID
           MOVE    L100LNK-HOSPNUM     TO  COMB-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    QUERY-PTINF-PTID    TO  COMB-PTID
           MOVE    JYURRK-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
           PERFORM 930-HKNCOMBI-READ-SEC
      * 患者保険情報情報抽出
           INITIALIZE                      PTHKNINF-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    L100LNK-HOSPID      TO  PTHKN-HOSPID
           MOVE    L100LNK-HOSPNUM     TO  PTHKN-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    QUERY-PTINF-PTID    TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           PERFORM 950-PTHKNINF-READ-SEC
      * 最終来院日
           INITIALIZE                      SRYKARRK-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    L100LNK-HOSPID      TO  KARRK-HOSPID
           MOVE    L100LNK-HOSPNUM     TO  KARRK-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    QUERY-PTINF-PTID    TO  KARRK-PTID
           MOVE    JYURRK-SRYKA        TO  KARRK-SRYKA
           PERFORM 970-SRYKARRK-READ-SEC
      * 公費情報１
           INITIALIZE                  PTKOHINF-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    L100LNK-HOSPID      TO  PTKOH-HOSPID
           MOVE    L100LNK-HOSPNUM     TO  PTKOH-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    QUERY-PTINF-PTID    TO  PTKOH-PTID
           MOVE    COMB-KOH1ID         TO  PTKOH-KOHID
           PERFORM 960-PTKOHINF-READ-SEC
           MOVE    PTKOH-FTNJANUM      TO  WRK-KOH1FTNNUM
      ** TMP書き込み
           IF    FLG-DRCD-ALL   =      "1"
                MOVE  SPACE                 TO  L109T-DRCD
           ELSE
                MOVE  JYURRK-DRCD           TO  L109T-DRCD
           END-IF
           MOVE    JYURRK-SRYKA        TO  L109T-SRYKA
           MOVE    L100LNK-SELCD       TO  L109T-SRYCD
           MOVE    PTNUM-PTNUM         TO  L109T-PTNUM
           MOVE    PTINF-KANANAME      TO  L109T-KANANAME
           MOVE    PTINF-NAME          TO  L109T-NAME
           MOVE    PTINF-SEX           TO  L109T-SEX
           MOVE    PTINF-BIRTHDAY      TO  L109T-BIRTHDAY
           MOVE    PTHKN-HKNJANUM      TO  L109T-HKNJANUM
           MOVE    PTHKN-HONKZKKBN     TO  L109T-HONKAZKKBN
      *
      *    記号・番号
      *    全角変換
           MOVE    PTHKN-KIGO          TO  WRK-MAE-INPUT
           PERFORM 31012-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-KIGONUM
           MOVE    PTHKN-NUM           TO  WRK-MAE-INPUT
           PERFORM 31012-HENKAN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-NUM
      *
           INITIALIZE              WRK-HEN-KIGONUM 
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) start
                                   L109T-KIGO
                                   L109T-BANGO
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) end
      ***  20050118  記号・番号どちらかが未入力でも印字  MOTO
      ***     IF      WRK-KIGONUM     NOT = SPACE 
      ***      AND    WRK-NUM         NOT = SPACE 
           IF  WRK-KIGONUM  = SPACE  AND  WRK-NUM = SPACE 
               MOVE      SPACE         TO  WRK-HEN-KIGONUM
           ELSE
      ***  20050118  記号・番号どちらかが未入力でも印字  MOTO
                STRING  WRK-KIGONUM     DELIMITED   BY  SPACE
                        "・"            DELIMITED   BY  SIZE
                        WRK-NUM         DELIMITED   BY  SPACE
                        INTO            WRK-HEN-KIGONUM
                END-STRING
           END-IF
           MOVE    WRK-HEN-KIGONUM     TO  L109T-KIGONUM
      *=== 後期高齢者対応 by Yoshikawa(2008/04/03) start
      *    後期高齢者は番号のみ
           IF  COMB-HKNNUM    =   "039"
               MOVE   WRK-NUM          TO  L109T-KIGONUM
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) start
           ELSE
      *    記号・番号で１６文字以上は別行に編集
               IF  WRK-HEN-KIGONUM(33:)   NOT =  SPACE
                   MOVE    SPACE       TO  L109T-KIGONUM
                   MOVE    WRK-KIGONUM TO  L109T-KIGO
                   MOVE    WRK-NUM     TO  L109T-BANGO
               END-IF
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) end
           END-IF
      *=== 後期高齢者対応 by Yoshikawa(2008/04/03) end
           MOVE    WRK-KOH1FTNNUM      TO  L109T-KOH1FTNNUM
           MOVE    JYURRK-NYUGAIKBN    TO  L109T-NYUGAIKBN
           MOVE    JYURRK-SRYYMD       TO  L109T-SRYYMD
           MOVE    KARRK-LASTYMD       TO  L109T-LASTYMD
      *
      *
      *    サブキー値の設定
      *
           EVALUATE  FLG-PRSORT
               WHEN  ""
                 MOVE  SPACE           TO  L109T-SUBKEY1
               WHEN  "0"
                 MOVE  PTINF-KANANAME  TO  L109T-SUBKEY1
               WHEN  "1"
                 MOVE  PTHKN-HKNJANUM  TO  L109T-SUBKEY1
               WHEN  "2"
                 MOVE  L109T-LASTYMD   TO  WRK-SRYYMDDMY
                 COMPUTE  WRK-SRYYMDDMY  =  9999999999  -  WRK-SRYYMDDMY
                 MOVE  WRK-SRYYMDDMY   TO  L109T-SUBKEY1
           END-EVALUATE
      *
           WRITE L109T
      *
      *     ADD      1     TO  CNT-DRCD
           .
       320-WRITE-TEMP-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索患者印刷メイン処理
      *****************************************************************
       330-PRT-MAIN-SEC                SECTION.
      *
           INITIALIZE      L109T
           OPEN    INPUT   L109T-FILE
      *
      *    患者情報読込
           PERFORM 900-L109T-READ-SEC
           MOVE L109T-SRYCD TO   WRK-PRSRYCD
           MOVE L109T-DRCD  TO   WRK-PRDRCD
      *
           PERFORM VARYING  IDX2 FROM 1 BY 1
                   UNTIL    FLG-ATEND =   1
      *    明細処理
               PERFORM 330-PRT-DETL-SEC
      *    患者情報読込
               PERFORM 900-L109T-READ-SEC
           END-PERFORM
      *    最終頁の印刷
           IF  IDX2 >   15
               PERFORM  330-PRT-HEAD-SEC
               PERFORM  340-PRINT-OUT-SEC
               INITIALIZE               L109
           END-IF
           IF    FLG-DRCD-ALL      =     "2"
                MOVE  "＜医師別計＞"    TO   L109-CNTTIT2
                MOVE  CNT-DRCD          TO   WRK-Z5
                MOVE  WRK-Z5            TO   L109-CNT2(1:5)
                MOVE  "件"              TO   L109-CNT2(8:2)
           END-IF
      *     MOVE  "＜診療行為別計＞"    TO   L109-CNTTIT2
      *     MOVE  CNT-BYO           TO   WRK-Z5
      *     MOVE  WRK-Z5            TO   L109-CNT2(1:5)
      *     MOVE  "件"              TO   L109-CNT2(8:2)
           MOVE  "＜＜総合計＞＞"  TO   L109-CNTTIT3
           MOVE  WRK-PRDTLCNT      TO   WRK-Z5
           MOVE  WRK-Z5            TO   L109-CNT3(1:5)
           MOVE  "件"              TO   L109-CNT3(8:2)
      *
           PERFORM  330-PRT-HEAD-SEC
           PERFORM  340-PRINT-OUT-SEC
           CLOSE    L109T-FILE
           .
       330-PRT-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    患者診療行為データ明細転送
      *****************************************************************
       330-PRT-DETL-SEC                SECTION.
      *
            IF   WRK-PRSRYCD  NOT  =   L109T-SRYCD
                 IF  IDX2 >   15
                     PERFORM  330-PRT-HEAD-SEC
                     PERFORM  340-PRINT-OUT-SEC
                     INITIALIZE        L109
                 END-IF
                 IF    FLG-DRCD-ALL      =     "2"
                      MOVE  "＜医師別計＞"   TO L109-CNTTIT1
                      MOVE  CNT-DRCD         TO WRK-Z5
                      MOVE  WRK-Z5           TO L109-CNT1(1:5)
                      MOVE  "件"             TO L109-CNT1(8:2)
                 END-IF
      *           MOVE  "＜診療行為別計＞" TO L109-CNTTIT3
      *           MOVE  CNT-BYO        TO WRK-Z5
      *           MOVE  WRK-Z5           TO L109-CNT3(1:5)
      *           MOVE  "件"             TO L109-CNT3(8:2)
                 PERFORM  330-PRT-HEAD-SEC
                 PERFORM  340-PRINT-OUT-SEC
                 MOVE 1             TO   IDX2
                 MOVE L109T-SRYCD   TO   WRK-PRSRYCD
                 MOVE L109T-DRCD    TO   WRK-PRDRCD
                 INITIALIZE              L109
                 INITIALIZE              CNT-DRCD
                 INITIALIZE              CNT-BYO
            ELSE IF   WRK-PRDRCD  NOT =  L109T-DRCD
                   IF IDX2 >   15
                      PERFORM  330-PRT-HEAD-SEC
                      PERFORM  340-PRINT-OUT-SEC
                      INITIALIZE            L109
                   END-IF
                   MOVE  "＜医師別計＞" TO  L109-CNTTIT3
                   MOVE  CNT-DRCD       TO  WRK-Z5
                   MOVE  WRK-Z5         TO  L109-CNT3(1:5)
                   MOVE  "件"           TO  L109-CNT3(8:2)
                   PERFORM  330-PRT-HEAD-SEC
                   PERFORM  340-PRINT-OUT-SEC
                   MOVE 1           TO   IDX2
                   MOVE L109T-DRCD  TO   WRK-PRDRCD
                   INITIALIZE            L109
                   INITIALIZE            CNT-DRCD
                ELSE  IF  IDX2      >    15
                         PERFORM  330-PRT-HEAD-SEC
                         PERFORM  340-PRINT-OUT-SEC
                         MOVE 1      TO  IDX2
                         INITIALIZE      L109
                END-IF
               END-IF
           END-IF
      *
      * 件数集計
           ADD   1                   TO  WRK-PRDTLCNT
           ADD   1                   TO  CNT-BYO
           ADD   1                   TO  CNT-DRCD
      * 通番
           MOVE  WRK-PRDTLCNT        TO  WRK-Z5
           MOVE  WRK-Z5              TO  L109-NUM(IDX2)
      *
      * 患者番号
           MOVE  L109T-PTNUM         TO  L109-PTNUM(IDX2)
      * カナ氏名
           MOVE  L109T-KANANAME      TO  L109-KANANAME(IDX2)
      * 氏名
           MOVE  L109T-NAME          TO  L109-NAME(IDX2)
      * 性別
           EVALUATE  L109T-SEX
               WHEN  "1"
                     MOVE  "男"      TO  L109-SEX(IDX2)
               WHEN  "2"
                     MOVE  "女"      TO  L109-SEX(IDX2)
           END-EVALUATE 
      * 生年月日
           MOVE  L109T-BIRTHDAY(1:4) TO  L109-BIRTHDAY(IDX2)(1:4) 
           MOVE  "/"                 TO  L109-BIRTHDAY(IDX2)(5:1) 
           MOVE  L109T-BIRTHDAY(5:2) TO  L109-BIRTHDAY(IDX2)(6:2) 
           MOVE  "/"                 TO  L109-BIRTHDAY(IDX2)(8:1) 
           MOVE  L109T-BIRTHDAY(7:2) TO  L109-BIRTHDAY(IDX2)(9:2) 
      * 年齢
           MOVE  L100LNK-PRTKANRI-SKYYMD   TO  WRK-SYSYMD
           MOVE  L109T-BIRTHDAY            TO  WRK-BIRTHDAY
           COMPUTE WRK-NENREI              =   WRK-SYSYY
                                           -   WRK-BIRTHY
           IF    WRK-SYSMM           <         WRK-BIRTHM
                 COMPUTE WRK-NENREI  =         WRK-NENREI   -   1
           END-IF
           MOVE    WRK-NENREI              TO  WRK-Z3
           MOVE    WRK-Z3                  TO  L109-AGE(IDX2)
      * 保険者番号
           MOVE  L109T-HKNJANUM      TO  L109-HKNJANUM(IDX2)
      * 本人家族区分
           EVALUATE L109T-HONKAZKKBN
               WHEN 1
                    MOVE  "本"     TO  L109-HONKAZKKBN(IDX2)
               WHEN 2
                    MOVE  "家"     TO  L109-HONKAZKKBN(IDX2)
           END-EVALUATE
      * 記号・番号
           MOVE  L109T-KIGONUM       TO  L109-KIGONUM(IDX2)
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) start
           MOVE  L109T-KIGO          TO  L109-KIGO(IDX2)
           MOVE  L109T-BANGO         TO  L109-BANGO(IDX2)
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) end
      * 公費番号
           MOVE  L109T-KOH1FTNNUM    TO  L109-KOH1FTNNUM(IDX2)
      * 入院外来区分
           EVALUATE L109T-NYUGAIKBN
               WHEN 1
                    MOVE  "入"     TO  L109-NYUGAIKBN(IDX2)
               WHEN 2
                    MOVE  "外"     TO  L109-NYUGAIKBN(IDX2)
           END-EVALUATE
      *
      * 診療科情報
           INITIALIZE                     SYS-1005-REC
           MOVE    "1005"            TO   SYS-1005-KANRICD
           MOVE    L109T-SRYKA       TO   SYS-1005-KBNCD
           MOVE    L100LNK-YMD2      TO   WRK-SYMD
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *     MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                               TO  SYS-1005-STYUKYMD(1:6)
      *                                   SYS-1005-EDYUKYMD(1:6)
      *     MOVE    "01"              TO  SYS-1005-STYUKYMD(7:2)
      *                                   SYS-1005-EDYUKYMD(7:2)
           MOVE    L100LNK-YMD1      TO  SYS-1005-STYUKYMD
           MOVE    L100LNK-YMD2      TO  SYS-1005-EDYUKYMD
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
           MOVE    SPA-HOSPNUM       TO  SYS-1005-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    SYS-1005-REC      TO   MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI      =    ZERO
               MOVE  MCPDATA-REC     TO   SYS-1005-REC
               MOVE  SYS-1005-SRYKANAME
                                     TO   L109-SRYKA(IDX2)
           END-IF                                     
      * 診察開始日
           MOVE  L109T-SRYYMD(1:4)   TO  L109-SRYYMD(IDX2)(1:4) 
           MOVE  "/"                 TO  L109-SRYYMD(IDX2)(5:1) 
           MOVE  L109T-SRYYMD(5:2)   TO  L109-SRYYMD(IDX2)(6:2) 
           MOVE  "/"                 TO  L109-SRYYMD(IDX2)(8:1) 
           MOVE  L109T-SRYYMD(7:2)   TO  L109-SRYYMD(IDX2)(9:2) 
      * 最終来院日
           MOVE  L109T-LASTYMD(1:4)  TO  L109-LASTYMD(IDX2)(1:4) 
           MOVE  "/"                 TO  L109-LASTYMD(IDX2)(5:1) 
           MOVE  L109T-LASTYMD(5:2)  TO  L109-LASTYMD(IDX2)(6:2) 
           MOVE  "/"                 TO  L109-LASTYMD(IDX2)(8:1) 
           MOVE  L109T-LASTYMD(7:2)  TO  L109-LASTYMD(IDX2)(9:2) 
      * 転帰区分
      *     EVALUATE L109T-TENKIKBN
      *         WHEN 1
      *              MOVE  "治癒"     TO  L109-TENKI(IDX2)
      *         WHEN 2
      *              MOVE  "死亡"     TO  L109-TENKI(IDX2)
      *         WHEN 3
      *              MOVE  "中止"     TO  L109-TENKI(IDX2)
      *         WHEN 8
      *              MOVE  "移転"     TO  L109-TENKI(IDX2)
      *     END-EVALUATE
      * 転帰日
      *     MOVE  L109T-TENKIYMD(1:4) TO  L109-TENKIYMD(IDX2)(1:4) 
      *     MOVE  "/"                 TO  L109-TENKIYMD(IDX2)(5:1) 
      *     MOVE  L109T-TENKIYMD(5:2) TO  L109-TENKIYMD(IDX2)(6:2) 
      *     MOVE  "/"                 TO  L109-TENKIYMD(IDX2)(8:1) 
      *     MOVE  L109T-TENKIYMD(7:2) TO  L109-TENKIYMD(IDX2)(9:2) 
      *
           .
       330-PRT-DETL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者診療行為データ明細転送
      *****************************************************************
       330-PRT-HEAD-SEC                SECTION.
      *
           ADD     1                 TO   CNT-PAGE
           MOVE    CNT-PAGE          TO   WRK-PAGE
           MOVE    WRK-PAGE          TO   L109-PAGE
           MOVE    "(診療行為)"      TO   L109-MIDASI
           MOVE    WRK-SYSYMDWH      TO   L109-SYSYMD
           MOVE    WRK-L100LNK-YMD1  TO   L109-YMD1
           MOVE    WRK-L100LNK-YMD2  TO   L109-YMD2
      *
      *    診療行為情報
           MOVE    L100LNK-SELCD     TO   L109-SRYCD
           INITIALIZE                     TNS-TENSU-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
           MOVE    SPA-HOSPNUM       TO   TNS-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    L100LNK-SELCD     TO   TNS-SRYCD
      * 20080602 診療行為の欄が空白になる不具合を修正START tanaka
      *     MOVE    "00000000"        TO   TNS-YUKOSTYMD
      *     MOVE    "00000000"        TO   TNS-YUKOEDYMD
           MOVE    L100LNK-YMD1        TO   TNS-YUKOSTYMD
           MOVE    L100LNK-YMD2        TO   TNS-YUKOEDYMD
      * 20080602 診療行為の欄が空白になる不具合を修正END
           PERFORM 980-TENSU-READ-SEC
           IF      FLG-BYO           =    ZERO
               MOVE  TNS-NAME        TO   L109-SRYMEI
           END-IF                                     
      *     MOVE  WRK-SQL             TO   L109-SRYMEI
      *
      *    職員情報
           IF    FLG-DRCD-ALL  NOT =     "1"                                     
                INITIALIZE                     SYS-1010-REC
                MOVE    "1010"            TO   SYS-1010-KANRICD
                IF      WRK-PRDRCD    NOT = SPACE
                     MOVE    WRK-PRDRCD        TO   SYS-1010-KBNCD
                ELSE
                     MOVE    L100LNK-DRCD      TO   SYS-1010-KBNCD
                END-IF
      ***  2004/12/06  MOTO
      ***        MOVE    L100LNK-YMD2        TO  WRK-SYMD
                MOVE    L100LNK-YMD2        TO  WRK-SYSYMD
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *          MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                                    TO  SYS-1010-STYUKYMD(1:6)
      *                                        SYS-1010-EDYUKYMD(1:6)
      *          MOVE    "01"              TO  SYS-1010-STYUKYMD(7:2)
      *                                        SYS-1010-EDYUKYMD(7:2)
                MOVE    L100LNK-YMD1      TO  SYS-1010-STYUKYMD
                MOVE    L100LNK-YMD2      TO  SYS-1010-EDYUKYMD
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
                MOVE    SPA-HOSPNUM       TO  SYS-1010-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
                MOVE    SYS-1010-REC      TO   MCPDATA-REC
                PERFORM 800-SYSKANRI-READ-SEC
                IF      FLG-SYSKANRI      =    ZERO
                    MOVE  MCPDATA-REC     TO   SYS-1010-REC
                    MOVE  SYS-1010-NAME   TO   L109-DRNAME
                ELSE                                     
                    MOVE  "医師未登録"    TO   L109-DRNAME
                END-IF                                     
           ELSE
                MOVE  "全医師"            TO   L109-DRNAME
           END-IF                                     
           .
       330-PRT-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       340-PRINT-OUT-SEC         SECTION.
      *    
               INITIALIZE                  ORCSPRTAREA
               MOVE    "INS"               TO  SPRT-MODE
               MOVE    L100LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
               MOVE    L100LNK-PRTKANRI-TBL-KEY
                                           TO  SPRT-TBL-KEY
               MOVE    L100LNK-PRTKANRI-TBL-GROUP
                                           TO  SPRT-TBL-GROUP
               MOVE    L100LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
               MOVE    L100LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
               MOVE    L100LNK-PRTKANRI-SHELLID 
                                           TO  SPRT-SHELLID
               MOVE    L100LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SPRT-SHORI-RENNUM
               MOVE    L100LNK-PRTKANRI-PRIORITY
                                           TO  SPRT-PRIORITY
      ***  20040917 MOTOHASI MONPEに変更
      ***         MOVE   "A00000L109"         TO  SPRT-PRTID
               MOVE   "A00000L109.red"         TO  SPRT-PRTID
      ***  20040917 MOTOHASI MONPEに変更
               MOVE   "(診療行為)患者一覧"     TO  SPRT-TITLE
               MOVE    L109                TO  SPRT-PRTDATA
               MOVE    L100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
               MOVE    L100LNK-PRTKANRI-OPID   TO  SPRT-OPID
               MOVE    L100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***  site-libを実行環境にする為コメントアウト
      ***         MOVE    "1"          TO  SPRT-SITEKBN
               CALL    "ORCSPRT"         USING
                                         ORCSPRTAREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
                                         SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF  SPRT-RETURN         =   ZERO
                   CONTINUE               ELSE
                   MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC                           
               END-IF
      *
           .
       340-PRINT-OUT-EXT.
           EXIT.
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
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               MOVE    L100LNK-JOBID       TO  JOB-JOBID
               MOVE    L100LNK-SHELLID     TO  JOB-SHELLID
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    L100LNK-JOBID   TO  JOB-JOBID
           MOVE    L100LNK-SHELLID TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
                                   SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
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
      *    全角変換処理
      *****************************************************************
       31012-HENKAN-SEC                 SECTION.
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
       31012-HENKAN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    受診履歴情報検索
      *****************************************************************
       900-JYURRK-SELECT-SEC               SECTION.
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "JYURRK-KEY21"      TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
          IF      MCP-RC           =   ZERO
               MOVE    ZERO        TO  FLG-JYURRK
           ELSE
               MOVE    1           TO  FLG-JYURRK
           END-IF        
           .
       900-JYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報FETCH
      *****************************************************************
       900-JYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "JYURRK-KEY21"       TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-JYURRK
               MOVE    MCPDATA-REC  TO  JYURRK-REC
           ELSE
               MOVE    1            TO  FLG-JYURRK
               INITIALIZE               JYURRK-REC
           END-IF  
           .
       900-JYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報 ＤＢ　クローズ処理
      *****************************************************************
       900-JYURRK-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE  "DBCLOSE"            TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "JYURRK-KEY21"       TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       900-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報検索 OPEN
      *****************************************************************
       900-PTBYO-SELECT-SEC               SECTION.
      *
           MOVE    PTBYO-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *** データチェック
          IF      MCP-RC           =   ZERO
               MOVE    ZERO        TO  FLG-PTBYO
           ELSE
               MOVE    1           TO  FLG-PTBYO
           END-IF        
           .
       900-PTBYO-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報FETCH
      *****************************************************************
       900-PTBYO-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *    ** データチェック
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTBYO
               MOVE    MCPDATA-REC  TO  PTBYO-REC
           ELSE
               MOVE    1            TO  FLG-PTBYO
               INITIALIZE               PTBYO-REC
           END-IF  
           .
       900-PTBYO-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴情報 ＤＢ　クローズ処理
      *****************************************************************
       900-PTBYO-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE  "DBCLOSE"            TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTBYOMEI-KEY2"     TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       900-PTBYO-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ検索処理
      *****************************************************************
       910-PTINF-SELECT-SEC            SECTION.
      *
      *    INITIALIZE                  MCPAREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    PATH-QUERY-PTINF-KEY
      *                                 TO  MCP-PATH
           MOVE    "query_ptinf"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    SPACE               TO  QUERY-PTINF-REC
           MOVE    WRK-SQL             TO  QUERY-PTINF-SQL
           MOVE    QUERY-PTINF-REC     TO  MCPDATA-REC
           PERFORM 910-DB-SELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    患者マスタ読込処理
      *****************************************************************
       910-PTINF-READ-SEC              SECTION.
      *
      *    INITIALIZE              MCPAREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    PATH-QUERY-PTINF-KEY
      *                                 TO  MCP-PATH
           MOVE    "query_ptinf"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    SPACE               TO  MCPDATA-REC
           PERFORM 910-DB-READ-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  QUERY-PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスタクローズ処理
      *****************************************************************
       910-PTINF-CLOSE-SEC             SECTION.
      *
      *    INITIALIZE              MCPAREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    PATH-QUERY-PTINF-KEY
      *                                 TO  MCP-PATH
           MOVE    "query_ptinf"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    SPACE               TO  MCPDATA-REC
           PERFORM 910-DB-CLOSE-SEC
      *
           .
       910-PTINF-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DB-SELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     CALL    "MCPSUB"    USING   MCPAREA
      *                                 MCPDATA-REC
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DB-READ-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     CALL    "MCPSUB"    USING   MCPAREA
      *                                 MCPDATA-REC
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
      *
       910-DB-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"       TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     CALL    "MCPSUB"    USING   MCPAREA
      *                                 MCPDATA-REC
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    初来院受診履歴情報検索
      *****************************************************************
       910-JYURRK-SELECT-SEC               SECTION.
      *
           MOVE    JYURRK-F-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "JYURRK-KEY13"      TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
          IF      MCP-RC           =   ZERO
               MOVE    ZERO        TO  FLG-F-JYURRK
           ELSE
               MOVE    1           TO  FLG-F-JYURRK
           END-IF        
           .
       910-JYURRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初来院受診履歴情報FETCH
      *****************************************************************
       910-JYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "JYURRK-KEY13"       TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_jyurrk"         TO  MCP-TABLE
           MOVE    "key13"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-F-JYURRK
               MOVE    MCPDATA-REC  TO  JYURRK-F-REC
           ELSE
               MOVE    1            TO  FLG-F-JYURRK
               INITIALIZE               JYURRK-F-REC
           END-IF  
           .
       910-JYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    初来院受診履歴情報 ＤＢ　クローズ処理
      *****************************************************************
       910-JYURRK-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE  "DBCLOSE"            TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE  "JYURRK-KEY13"       TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
      *                                MCPAREA
      *                                ORCMCPAREA
      *                                MCPDATA-REC
           MOVE  "tbl_jyurrk"         TO  MCP-TABLE
           MOVE  "key13"              TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       910-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計情報読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    SRYACCT-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"            TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SRYACCT-KEY3"   TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_sryacct"     TO  MCP-TABLE
           MOVE    "key3"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "SRYACCT-KEY3"   TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                         SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SRYACCT-REC
                   MOVE    ZERO                    TO  FLG-SRYACCT
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SRYACCT-KEY3"   TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SRYACT-KEY8"       TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SRYACT-KEY8"        TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
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
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE  "DBCLOSE"            TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE  "SRYACT-KEY8"        TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"          USING
      *                                MCPAREA
      *                                ORCMCPAREA
      *                                MCPDATA-REC
           MOVE  "tbl_sryact"         TO  MCP-TABLE
           MOVE  "key8"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       900-SRYACT-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診患者データ一時ファイルREAD
      *****************************************************************
       900-L109T-READ-SEC                SECTION.
      *
           READ L109T-FILE  
                NEXT AT  END  MOVE  1  TO  FLG-ATEND
           END-READ
           .
       900-L109T-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SYSKANRI-KEY10"    TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "SYSKANRI-KEY10"    TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SYSKANRI-KEY10"    TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"         USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
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
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI2
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI2
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI2
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       810-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBSELECT-SEC                 SECTION.
      *
           MOVE    WRK-SYSYMD            TO  ORC-DBYMD
           MOVE    SYS-1005-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"                TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
          IF      MCP-RC           =   ZERO
                PERFORM 820-SYSKANRI-DBFETCH-SEC
           ELSE
               MOVE    1           TO    FLG-SYSKANRI3
               PERFORM 820-SYSKANRI-DBCLOSE-SEC
           END-IF        
           .
       820-SYSKANRI-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マスタ検索(診療科目情報)
      *****************************************************************
       820-SYSKANRI-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"                  TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"         USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC           =   ZERO
               MOVE    ZERO                   TO  FLG-SYSKANRI3
               MOVE    MCPDATA-REC  TO  SYS-1005-REC
           ELSE
               MOVE    1                           TO  FLG-SYSKANRI3
               PERFORM 820-SYSKANRI-DBCLOSE-SEC
           END-IF  
           .
       820-SYSKANRI-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       820-SYSKANRI-DBCLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE  "DBCLOSE"                TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE  "tbl_syskanri"           TO  MCP-TABLE
           MOVE  "key2"                   TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
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
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       930-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       930-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報読み込み
      *****************************************************************
       940-PTNUM-READ-SEC         SECTION.
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
                   MOVE    ZERO            TO  FLG-PTNUM
               ELSE
                   MOVE    1               TO  FLG-PTNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTNUM-KEY"         TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       940-PTNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       950-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
     *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTHKNINF-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       950-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       960-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
     *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "PTKOHINF-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       960-PTKOHINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    最終来院日情報読み込み
      *****************************************************************
       970-SRYKARRK-READ-SEC         SECTION.
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SRYKARRK-REC
                   MOVE    ZERO            TO  FLG-SRYKARRK
               ELSE
                   MOVE    1               TO  FLG-SRYKARRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-SRYKARRK
           END-IF
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "SRYKARRK-KEY"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       970-SRYKARRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為読み込み
      *****************************************************************
       980-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"         TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "TENSU-KEY"        TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_tensu"        TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *         MOVE    "TENSU-KEY"        TO  ORC-DBPATH
      *         CALL    "ORCMCPSUB"          USING
      *                                      MCPAREA
      *                                      ORCMCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_tensu"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   MOVE    ZERO            TO  FLG-BYO
               ELSE
                   MOVE    1               TO  FLG-BYO
               END-IF
           ELSE
               MOVE    1              TO  FLG-BYO
           END-IF
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE    "DBCLOSE"          TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     MOVE    "TENSU-KEY"        TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_tensu"        TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       980-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    "DBOPEN"            TO  MCP-FUNC.
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    "DBSTART"           TO  MCP-FUNC.
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) start
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/20) end
      *
           .
       900-DBCLOSE-EXT.
           EXIT.
