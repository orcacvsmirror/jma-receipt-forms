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
       PROGRAM-ID.             A00000L103.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者一覧
      *  コンポーネント名  : (在院)患者一覧
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
      *  05/03/01    @@@@-鈴木     open-cobol 0.23への対応による変更
      *  06/08/09    @@@@-高嶋     sarge対応
      *  07/09/11    @@@@-吉川     Ver4.0対応
      *  08/04/03    @@@@-吉川     シス管の値が取得できない不具合を修正
      *                            後期高齢者対応(ver4.2.0)
      *                            記号・番号１６文字以上対応(ver4.2.0)
      *  09/09/02    @@@@-田中     古い保健情報が印字される不具合を修正
      *  10/02/08    @@@@-田中     シス管の値が取得できない不具合を修正
      *                            DBOPEN・DBCOMMITの処理が2重になっていた不具合を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *    受診患者抽出一時ファイル
           SELECT  L103T-FILE      ASSIGN  L103TPARA
                                   ORGANIZATION    INDEXED
                                   ACCESS  MODE    DYNAMIC
                                   RECORD  KEY     L103T-KEY
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *    受診患者抽出一時ファイル
       FD  L103T-FILE.
           COPY    "A00000L103TMP.INC".
      *
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    エラーファイル 名称領域 
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
           COPY    "A00000L103.INC".
      *
       01  L103TPARA.
           03  FILLER              PIC  X(05)    VALUE "/tmp/".
           03  L103TPARA-FILENAME  PIC  X(10) VALUE "A00000L103".
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
           03  FLG-F-JYURRK        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTNYUINRRK-M    PIC 9(01).
           03  INP-STS             PIC 9(04).
           03  FLG-11DUMMY         PIC 9(01).
           03  FLG-12DUMMY         PIC 9(01).
           03  FLG-DATAREAD        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  WRK-PRDTLCNT        PIC 9(05).
           03  CNT-SRYKA           PIC 9(05).
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
      **** 20050301
           03  JIHIIDX           PIC 9(04).
           03  NG                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID    PIC 9(07).
           03  WRK-PARA-SHELLID  PIC X(08).
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     03  WRK-PARA-HOSPID   PIC X(24).
           03  WRK-PARA-HOSPNUM  PIC 9(02).
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
           03  WRK-PRSRYKA          PIC X(02).
           03  WRK-PRDRCD           PIC X(05).
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
           03  WRK-JPTID            PIC X(10).
           03  WRK-JDRCD            PIC X(08).
           03  WRK-JSRYKA           PIC X(02).
           03  WRK-NYUINKA          PIC X(02).
           03  WRK-NYUINYMDDMY      PIC 9(08).
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU      PIC X(02)   OCCURS   10.
           03  WRK-MOJI             PIC X(10).
           03  WRK-MOJISU           PIC 9(02).
           03  WRK-MAE-INPUT        PIC X(200).
           03  WRK-OUT-INPUT        PIC X(200).
           03  WRK-PTID             PIC 9(10).
           03  DEBUG-CNT            PIC 9(03).
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     COPY    "ORCA-DBPATH".
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *    患者入院履歴情報(入院履歴全枝番号用)
       01  PTNYUINRRK-M-REC.
           COPY    "CPPTNYUINRRK.INC"
           REPLACING  //PTNYUINRRK-// BY //PTNYUINRRK-M-//.
      *    一時保存用
      * 01  L103T.
      *     COPY    "A00000L103TMP.INC"
      *     REPLACING  //L103T-// BY //L103T-T-//.
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
           INITIALIZE                  L103T
           INITIALIZE                  L103
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
           INITIALIZE                  SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
           MOVE    L100LNK-HOSPNUM     TO  JOB-HOSPNUM
                                           SPA-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    L100LNK-JOBID       TO  JOB-JOBID
           MOVE    L100LNK-SHELLID     TO  JOB-SHELLID
           MOVE    "A00000L103"        TO  JOB-PGID
      ***  20040917 MOTOHASI
      ***     MOVE    "(受診)患者一覧"    TO  JOB-SHELLMSG
           MOVE    "(在院)患者一覧"    TO  JOB-SHELLMSG
      ***  20040917 MOTOHASI
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
                                   SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *     MOVE    L100LNK-PRTKANRI-TERMID
      *                             TO  RECEERR-TERMID
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
      *--- 2010/02/08 DELETE START -------------------------------------
      *     PERFORM 100-DBOPEN-SEC
      *--- 2010/02/08 DELETE END ---------------------------------------
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *     MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                                 TO  SYS-1001-STYUKYMD(1:6)
      *                                     SYS-1001-EDYUKYMD(1:6)
      *     MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
      *                                     SYS-1001-EDYUKYMD(7:2)
      *--- 2010/02/08 CHANGE START -------------------------------------
      *     MOVE    L100LNK-YMD1        TO  SYS-1001-STYUKYMD
      *     MOVE    L100LNK-YMD2        TO  SYS-1001-EDYUKYMD
           MOVE     L100LNK-PRTKANRI-SKYYMD   TO   SYS-1001-STYUKYMD
                                                   SYS-1001-EDYUKYMD
      *--- 2010/02/08 CHANGE END ---------------------------------------
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *         MOVE    SYS-1001-HOSPID     TO  WRK-PARA-HOSPID
               MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *        MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                                    TO  SYS-1005-STYUKYMD(1:6)
      *                                        SYS-1005-EDYUKYMD(1:6)
      *        MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
      *                                        SYS-1005-EDYUKYMD(7:2)
              MOVE    L100LNK-YMD1        TO  SYS-1005-STYUKYMD
              MOVE    L100LNK-YMD2        TO  SYS-1005-EDYUKYMD
      *--- 2010/02/08 CHANGE START -------------------------------------
      *        MOVE    L100LNK-YMD1        TO  SYS-1005-STYUKYMD
      *        MOVE    L100LNK-YMD2        TO  SYS-1005-EDYUKYMD
              MOVE    L100LNK-YMD2        TO   SYS-1005-STYUKYMD
              MOVE    L100LNK-YMD1        TO   SYS-1005-EDYUKYMD
      *--- 2010/02/08 CHANGE END ---------------------------------------
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
              MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *        MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                                    TO  SYS-1010-STYUKYMD(1:6)
      *                                        SYS-1010-EDYUKYMD(1:6)
      *        MOVE    "01"                TO  SYS-1010-STYUKYMD(7:2)
      *                                        SYS-1010-EDYUKYMD(7:2)
      *--- 2010/02/08 CHANGE START -------------------------------------
      *        MOVE    L100LNK-YMD1        TO  SYS-1010-STYUKYMD
      *        MOVE    L100LNK-YMD2        TO  SYS-1010-EDYUKYMD
              MOVE    L100LNK-YMD2        TO   SYS-1010-STYUKYMD
              MOVE    L100LNK-YMD1        TO   SYS-1010-EDYUKYMD
      *--- 2010/02/08 CHANGE END ---------------------------------------
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
              MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *    明細出力順序
      *     IF L100LNK-PRSORT  =    SPACE
      ***  全診療印刷フラグON
      *        MOVE    "1"               TO  FLG-PRSORT
      *     ELSE
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
      *     END-IF
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    受診患者抽出
           PERFORM 320-JYURRK-11GET-SEC
      *    受診患者印刷
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
           INITIALIZE                     L103
      *
           ADD     1                 TO   CNT-PAGE
           MOVE    CNT-PAGE          TO   WRK-PAGE
           MOVE    WRK-PAGE          TO   L103-PAGE
           MOVE    "(在院)"          TO   L103-MIDASI
           MOVE    WRK-SYSYMDWH      TO   L103-SYSYMD
           MOVE    WRK-L100LNK-YMD1  TO   L103-YMD1
           MOVE    WRK-L100LNK-YMD2  TO   L103-YMD2
           EVALUATE  INP-STS
              WHEN 1
                MOVE "入力された診療科コードの値が不正です。"
                                     TO   L103-ERRMSG
              WHEN 2
                MOVE "入力された医師コードの値が不正です。"
                                     TO   L103-ERRMSG
              WHEN 3
                MOVE "入力された明細出力順序コードの値が不正です。"
                                     TO   L103-ERRMSG
           END-EVALUATE
           PERFORM 340-PRINT-OUT-SEC
      *
           .
       210-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴 受診患者情報抽出処理
      *****************************************************************
       320-JYURRK-11GET-SEC      SECTION.
      *
           OPEN     OUTPUT         L103T-FILE
           CLOSE                   L103T-FILE
           OPEN     I-O            L103T-FILE
      *
      *     MOVE WRK-SRYKAMAX   TO  IDX3
      *     MOVE '0000000000'   TO  WRK-JPTID
      *     MOVE '00000000'     TO  WRK-JDRCD
      *     MOVE '00'           TO  WRK-JSRYKA
      *
      *    外来受診患者読み込み
            INITIALIZE                      PTNYUINRRK-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *      MOVE    L100LNK-HOSPID      TO  PTNYUINRRK-HOSPID 
            MOVE    L100LNK-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
            MOVE    L100LNK-YMD2        TO  PTNYUINRRK-NYUINYMD
            MOVE    L100LNK-YMD1        TO  PTNYUINRRK-TAIINYMD
      *								 
            PERFORM  900-NYURRK-SELECT-SEC
            PERFORM  900-NYURRK-FETCH-SEC
            PERFORM  UNTIL   FLG-PTNYUINRRK  =   1
      ***      患者情報の取得
                INITIALIZE                   PTINF-REC 
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *          MOVE     PTNYUINRRK-HOSPID   TO  PTINF-HOSPID
                MOVE     PTNYUINRRK-HOSPNUM  TO  PTINF-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
                MOVE     PTNYUINRRK-PTID     TO  PTINF-PTID
                PERFORM  900-PTINF-READ-SEC
                IF  FLG-PTINF = ZERO
                 AND  PTINF-TSTPTNUMKBN  NOT  =  "1"
                     INITIALIZE                PTNYUINRRK-M-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *               MOVE PTNYUINRRK-HOSPID TO PTNYUINRRK-M-HOSPID 
                     MOVE PTNYUINRRK-HOSPNUM TO PTNYUINRRK-M-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
                     MOVE PTNYUINRRK-PTID   TO PTNYUINRRK-M-PTID 
                     MOVE PTNYUINRRK-RRKNUM TO PTNYUINRRK-M-RRKNUM 
                     MOVE "00"              TO WRK-NYUINKA
      **** 20050301 open-cobol 0.23への対応 SUZUKI
      *               MOVE "0"               TO FLG-DATAREAD
                     MOVE  0                TO FLG-DATAREAD
      **** 20050301 SUZUKI
                     PERFORM  900-NYURRK-M-SELECT-SEC
                     PERFORM  900-NYURRK-M-FETCH-SEC
                     PERFORM  UNTIL   FLG-PTNYUINRRK-M  =   1
      *                 データを書き込む
                        IF   WRK-NYUINKA    NOT =  PTNYUINRRK-M-NYUINKA
                         AND FLG-DATAREAD       =  "1"
                         AND FLG-SRYKA-ALL  NOT  =  "1"
                             WRITE L103T
      **** 20050301 open-cobol 0.23への対応 SUZUKI
      *                       MOVE "0"          TO  FLG-DATAREAD
                             MOVE  0           TO  FLG-DATAREAD
      **** 20050301 SUZUKI
                        END-IF
                        MOVE  PTNYUINRRK-M-NYUINKA    TO  WRK-NYUINKA
                        IF    PTNYUINRRK-M-TENSTUYMD   >=   L100LNK-YMD1
                          AND PTNYUINRRK-M-TENNYUYMD   <=   L100LNK-YMD2
                          AND  (( FLG-SRYKA-ALL  NOT  =  "1"
                            AND  PTNYUINRRK-M-NYUINKA =  L100LNK-SRYKA )
                           OR    FLG-SRYKA-ALL        =  "1" )
                          AND (( FLG-DRCD-ALL     NOT =  "1"
                            AND  PTNYUINRRK-M-DRCD1   =  L100LNK-DRCD )
                           OR    FLG-DRCD-ALL   =  "1" )
                            IF   FLG-DATAREAD         =  "0"
                               PERFORM  320-WRITE-TEMP-SEC
      **** 20050301 open-cobol 0.23への対応 SUZUKI
      *                         MOVE  "1"              TO FLG-DATAREAD
                               MOVE   1               TO FLG-DATAREAD
      **** 20050301 SUZUKI 
                            ELSE
                               MOVE  PTNYUINRRK-M-TENSTUYMD
                                                      TO  L103T-TAIINYMD
                            END-IF
                        END-IF
                        PERFORM  900-NYURRK-M-FETCH-SEC
                     END-PERFORM
      *=== sarge対応 ADD By Takashima(2006/08/09) start
                     PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 ADD By Takashima(2006/08/09) end
      *              データを書き込む
      **** 20050301 open-cobol 0.23への対応 SUZUKI
      *               IF  FLG-DATAREAD       =  "1"
               	     IF  FLG-DATAREAD       =   1
      **** 20050301 SUZUKI
                         WRITE L103T
                     END-IF
                  END-IF
      *         END-IF
               PERFORM  900-NYURRK-FETCH-SEC
            END-PERFORM
            PERFORM  900-NYURRK-CLOSE-SEC
      *
           CLOSE                   L103T-FILE
      *
           .
       320-JYURRK-11GET-EXT.
           EXIT.
      *****************************************************************
      *    受診件数判別処理
      *****************************************************************
       320-WRITE-TEMP-SEC      SECTION.
      *
      * 患者情報抽出
           INITIALIZE                      PTINF-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE     L100LNK-HOSPID         TO  PTINF-HOSPID
           MOVE     L100LNK-HOSPNUM        TO  PTINF-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE     PTNYUINRRK-M-PTID      TO  PTINF-PTID
           PERFORM  900-PTINF-READ-SEC
      * 患者番号情報抽出
           INITIALIZE                      PTNUM-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE     L100LNK-HOSPID     TO  PTNUM-HOSPID
           MOVE     L100LNK-HOSPNUM    TO  PTNUM-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE     PTINF-PTID         TO  PTNUM-PTID
           PERFORM  940-PTNUM-READ-SEC
      * 保険組合せマスター検索
           INITIALIZE                        HKNCOMBI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    L100LNK-HOSPID        TO  COMB-HOSPID
           MOVE    L100LNK-HOSPNUM       TO  COMB-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    PTINF-PTID            TO  COMB-PTID
           MOVE    PTNYUINRRK-M-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
           PERFORM 930-HKNCOMBI-READ-SEC
      * 患者保険情報情報抽出
           INITIALIZE                      PTHKNINF-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    L100LNK-HOSPID      TO  PTHKN-HOSPID
           MOVE    L100LNK-HOSPNUM     TO  PTHKN-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    PTINF-PTID          TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           PERFORM 950-PTHKNINF-READ-SEC
      * 公費情報１
           INITIALIZE                  PTKOHINF-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    L100LNK-HOSPID      TO  PTKOH-HOSPID
           MOVE    L100LNK-HOSPNUM     TO  PTKOH-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    PTINF-PTID          TO  PTKOH-PTID
           MOVE    COMB-KOH1ID         TO  PTKOH-KOHID
           PERFORM 960-PTKOHINF-READ-SEC
           MOVE    PTKOH-FTNJANUM      TO  WRK-KOH1FTNNUM
      * 公費情報２
           INITIALIZE                  PTKOHINF-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    L100LNK-HOSPID      TO  PTKOH-HOSPID
           MOVE    L100LNK-HOSPNUM     TO  PTKOH-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    PTINF-PTID          TO  PTKOH-PTID
           MOVE    COMB-KOH2ID         TO  PTKOH-KOHID
           PERFORM 960-PTKOHINF-READ-SEC
           MOVE    PTKOH-FTNJANUM      TO  WRK-KOH2FTNNUM
      * 公費情報３
           INITIALIZE                  PTKOHINF-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    L100LNK-HOSPID      TO  PTKOH-HOSPID
           MOVE    L100LNK-HOSPNUM     TO  PTKOH-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    PTINF-PTID          TO  PTKOH-PTID
           MOVE    COMB-KOH3ID         TO  PTKOH-KOHID
           PERFORM 960-PTKOHINF-READ-SEC
           MOVE    PTKOH-FTNJANUM      TO  WRK-KOH3FTNNUM
      *
           IF    FLG-SRYKA-ALL  =      "1"
                MOVE  SPACE                 TO  L103T-SRYKA
           ELSE
                MOVE  PTNYUINRRK-M-NYUINKA  TO  L103T-SRYKA
		   END-IF
		   IF    FLG-DRCD-ALL   =      "1"
                MOVE  SPACE                 TO  L103T-DRCD
           ELSE
                MOVE  PTNYUINRRK-M-DRCD1    TO  L103T-DRCD
		   END-IF
           MOVE  PTNUM-PTNUM           TO  L103T-PTNUM
           MOVE  PTINF-PTID            TO  L103T-PTID
           MOVE  PTINF-KANANAME        TO  L103T-KANANAME
           MOVE  PTINF-NAME            TO  L103T-NAME
           MOVE  PTINF-SEX             TO  L103T-SEX
           MOVE  PTINF-BIRTHDAY        TO  L103T-BIRTHDAY
           MOVE  PTHKN-HKNJANUM        TO  L103T-HKNJANUM
           MOVE  PTHKN-HONKZKKBN       TO  L103T-HONKAZKKBN
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
                                   L103T-KIGO
                                   L103T-BANGO
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
           MOVE  WRK-HEN-KIGONUM      TO  L103T-KIGONUM
      *=== 後期高齢者対応 by Yoshikawa(2008/04/03) start
      *    後期高齢者は番号のみ
           IF  COMB-HKNNUM    =   "039"
               MOVE   WRK-NUM          TO     L103T-KIGONUM
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) start
           ELSE
      *    記号・番号で１６文字以上は別行に編集
               IF  WRK-HEN-KIGONUM(33:)   NOT =  SPACE
                   MOVE    SPACE       TO     L103T-KIGONUM
                   MOVE    WRK-KIGONUM TO     L103T-KIGO
                   MOVE    WRK-NUM     TO     L103T-BANGO
               END-IF
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) end
           END-IF
      *=== 後期高齢者対応 by Yoshikawa(2008/04/03) end
           MOVE  WRK-KOH1FTNNUM       TO  L103T-KOH1FTNNUM
           MOVE  WRK-KOH2FTNNUM       TO  L103T-KOH2FTNNUM
           MOVE  WRK-KOH3FTNNUM       TO  L103T-KOH3FTNNUM
      *    転科日
           MOVE  PTNYUINRRK-M-TENNYUYMD  TO  L103T-NYUINYMD
           MOVE  PTNYUINRRK-M-TENSTUYMD  TO  L103T-TAIINYMD
      *
           PERFORM 320-F-JYURRK-CHK-SEC
           MOVE    WRK-F-SRYYMD    TO  L103T-ATAMAGAKI
      *
      *    サブキー値の設定
      *
           EVALUATE  FLG-PRSORT
               WHEN  ""
                 MOVE  SPACE           TO  L103T-SUBKEY1
               WHEN  "0"
                 MOVE  PTINF-KANANAME  TO  L103T-SUBKEY1
               WHEN  "1"
                 MOVE  PTHKN-HKNJANUM  TO  L103T-SUBKEY1
               WHEN  "2"
                 MOVE  L103T-NYUINYMD   TO  WRK-NYUINYMDDMY
                 COMPUTE  WRK-NYUINYMDDMY  
				         =  99999999  -  WRK-NYUINYMDDMY
                 MOVE  WRK-NYUINYMDDMY   TO  L103T-SUBKEY1
           END-EVALUATE
      *
      *     WRITE L103T
      *
           .
       320-WRITE-TEMP-EXT.
           EXIT.
      *
      *****************************************************************
      *    初来院受診履歴チェック処理
      *****************************************************************
       320-F-JYURRK-CHK-SEC      SECTION.
      *
           INITIALIZE                 WRK-FN-SRYYMD 
      *
      *    入院受診患者読み込み
           INITIALIZE                 JYURRK-F-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE L100LNK-HOSPID    TO  JYURRK-F-HOSPID
           MOVE L100LNK-HOSPNUM   TO  JYURRK-F-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE PTINF-PTID        TO  JYURRK-F-PTID 
           MOVE "1"               TO  JYURRK-F-NYUGAIKBN
           MOVE "00000000"        TO  JYURRK-F-SRYYMD 
           MOVE "99999999"        TO  JYURRK-F-UPSRYYMD 
           PERFORM  910-JYURRK-SELECT-SEC
           PERFORM  910-JYURRK-FETCH-SEC
      *
           PERFORM  UNTIL   FLG-F-JYURRK  =   1
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *         IF   JYURRK-F-HOSPID       =   L100LNK-HOSPID
               IF   JYURRK-F-HOSPNUM      =   L100LNK-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
                AND JYURRK-F-NYUGAIKBN    =   "1"
      ***  20041006  Ver 2.4.0対応による削除  MOTO
      ***          AND JYURRK-F-LASTKBN      =   "1"
                    MOVE JYURRK-F-SRYYMD  TO  WRK-FN-SRYYMD
                    MOVE   1              TO  FLG-F-JYURRK
               END-IF
               PERFORM  910-JYURRK-FETCH-SEC
           END-PERFORM
           PERFORM   910-JYURRK-CLOSE-SEC
      *
           INITIALIZE                 WRK-FG-SRYYMD 
      *
      *    外来受診患者読み込み
           INITIALIZE                 JYURRK-F-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE L100LNK-HOSPID    TO  JYURRK-F-HOSPID
           MOVE L100LNK-HOSPNUM   TO  JYURRK-F-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE PTINF-PTID        TO  JYURRK-F-PTID 
           MOVE "2"               TO  JYURRK-F-NYUGAIKBN
           MOVE "00000000"        TO  JYURRK-F-SRYYMD 
           MOVE "99999999"        TO  JYURRK-F-UPSRYYMD 
           PERFORM  910-JYURRK-SELECT-SEC
           PERFORM  910-JYURRK-FETCH-SEC
      *
           PERFORM  UNTIL   FLG-F-JYURRK  =   1
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *         IF   JYURRK-F-HOSPID       =   L100LNK-HOSPID
               IF   JYURRK-F-HOSPNUM      =   L100LNK-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
                AND JYURRK-F-NYUGAIKBN    =   "2"
      ***  20041006  Ver 2.4.0対応による削除  MOTO
      ***          AND JYURRK-F-LASTKBN      =   "1"
                    MOVE JYURRK-F-SRYYMD  TO  WRK-FG-SRYYMD
                    MOVE   1              TO  FLG-F-JYURRK
               END-IF
               PERFORM  910-JYURRK-FETCH-SEC
           END-PERFORM
           PERFORM   910-JYURRK-CLOSE-SEC
      *
           IF    WRK-FN-SRYYMD      =    SPACE
                 MOVE WRK-FG-SRYYMD TO   WRK-F-SRYYMD
           ELSE
            IF   WRK-FG-SRYYMD      =    SPACE
                 MOVE WRK-FN-SRYYMD TO   WRK-F-SRYYMD
            ELSE
             IF  WRK-FN-SRYYMD      <    WRK-FG-SRYYMD
                 MOVE WRK-FN-SRYYMD TO   WRK-F-SRYYMD
             ELSE
                 MOVE WRK-FG-SRYYMD TO   WRK-F-SRYYMD
             END-IF
            END-IF
           END-IF       
      *
           .
       320-F-JYURRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受診患者印刷メイン処理
      *****************************************************************
       330-PRT-MAIN-SEC                SECTION.
      *
           INITIALIZE      L103T
           OPEN    INPUT   L103T-FILE
      *
      *    患者情報読込
           PERFORM 900-L103T-READ-SEC
           MOVE L103T-SRYKA TO   WRK-PRSRYKA
           MOVE L103T-DRCD  TO   WRK-PRDRCD
      *
           PERFORM VARYING  IDX2 FROM 1 BY 1
                   UNTIL    FLG-ATEND =   1
      *    明細処理
               PERFORM 330-PRT-DETL-SEC
      *    患者情報読込
               PERFORM 900-L103T-READ-SEC
           END-PERFORM
      *    最終頁の印刷
           IF  IDX2 >   15
               PERFORM  330-PRT-HEAD-SEC
               PERFORM  340-PRINT-OUT-SEC
               INITIALIZE               L103
           END-IF
		   IF    FLG-SRYKA-ALL          =     "2"
                IF    FLG-DRCD-ALL      =     "2"
                     MOVE  "＜医師別計＞"    TO   L103-CNTTIT1
                     MOVE  CNT-DRCD          TO   WRK-Z5
                     MOVE  WRK-Z5            TO   L103-CNT1(1:5)
                     MOVE  "件"              TO   L103-CNT1(8:2)
                END-IF
                MOVE  "＜診療科別計＞"  TO   L103-CNTTIT2
                MOVE  CNT-SRYKA         TO   WRK-Z5
                MOVE  WRK-Z5            TO   L103-CNT2(1:5)
                MOVE  "件"              TO   L103-CNT2(8:2)
           ELSE
                IF    FLG-DRCD-ALL       =    "2"
                     MOVE  "＜医師別計＞"    TO   L103-CNTTIT2
                     MOVE  CNT-DRCD          TO   WRK-Z5
                     MOVE  WRK-Z5            TO   L103-CNT2(1:5)
                     MOVE  "件"              TO   L103-CNT2(8:2)
                END-IF
           END-IF
           MOVE  "＜＜総合計＞＞"  TO   L103-CNTTIT3
           MOVE  WRK-PRDTLCNT      TO   WRK-Z5
           MOVE  WRK-Z5            TO   L103-CNT3(1:5)
           MOVE  "件"              TO   L103-CNT3(8:2)
           PERFORM  330-PRT-HEAD-SEC
           PERFORM  340-PRINT-OUT-SEC
           CLOSE    L103T-FILE
           .
       330-PRT-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    患者病名データ明細転送
      *****************************************************************
       330-PRT-DETL-SEC                SECTION.
      *
           IF   WRK-PRSRYKA  NOT  =   L103T-SRYKA
                IF  IDX2 >   15
                    PERFORM  330-PRT-HEAD-SEC
                    PERFORM  340-PRINT-OUT-SEC
                    INITIALIZE        L103
                END-IF
                IF    FLG-DRCD-ALL     =     "2"
                     MOVE  "＜医師別計＞"   TO L103-CNTTIT2
                     MOVE  CNT-DRCD         TO WRK-Z5
                     MOVE  WRK-Z5           TO L103-CNT2(1:5)
                     MOVE  "件"             TO L103-CNT2(8:2)
                END-IF
                MOVE  "＜診療科別計＞" TO L103-CNTTIT3
                MOVE  CNT-SRYKA        TO WRK-Z5
                MOVE  WRK-Z5           TO L103-CNT3(1:5)
                MOVE  "件"             TO L103-CNT3(8:2)
                PERFORM  330-PRT-HEAD-SEC
                PERFORM  340-PRINT-OUT-SEC
                MOVE 1             TO   IDX2
                MOVE L103T-SRYKA   TO   WRK-PRSRYKA
                MOVE L103T-DRCD    TO   WRK-PRDRCD
                INITIALIZE              L103
                INITIALIZE              CNT-DRCD
                INITIALIZE              CNT-SRYKA
           ELSE IF   WRK-PRDRCD  NOT =  L103T-DRCD
                   IF IDX2 >   15
                      PERFORM  330-PRT-HEAD-SEC
                      PERFORM  340-PRINT-OUT-SEC
                      INITIALIZE            L103
                   END-IF
                   MOVE  "＜医師別計＞" TO  L103-CNTTIT3
                   MOVE  CNT-DRCD       TO  WRK-Z5
                   MOVE  WRK-Z5         TO  L103-CNT3(1:5)
                   MOVE  "件"           TO  L103-CNT3(8:2)
                   PERFORM  330-PRT-HEAD-SEC
                   PERFORM  340-PRINT-OUT-SEC
                   MOVE 1           TO   IDX2
                   MOVE L103T-DRCD  TO   WRK-PRDRCD
                   INITIALIZE            L103
                   INITIALIZE            CNT-DRCD
                ELSE  IF  IDX2      >    15
                         PERFORM  330-PRT-HEAD-SEC
                         PERFORM  340-PRINT-OUT-SEC
                         MOVE 1      TO  IDX2
                         INITIALIZE      L103
                      END-IF
                END-IF
           END-IF
      *
      * 件数集計
           ADD   1                   TO  WRK-PRDTLCNT
           ADD   1                   TO  CNT-SRYKA
           ADD   1                   TO  CNT-DRCD
      * 通番
           MOVE  WRK-PRDTLCNT        TO  WRK-Z5
           MOVE  WRK-Z5              TO  L103-NUM(IDX2)
      *
      * 患者番号
           MOVE  L103T-PTNUM         TO  L103-PTNUM(IDX2)
      * カナ氏名
           MOVE  L103T-KANANAME      TO  L103-KANANAME(IDX2)
      * 氏名
           MOVE  L103T-NAME          TO  L103-NAME(IDX2)
      * 性別
           EVALUATE  L103T-SEX
               WHEN  "1"
                     MOVE  "男"      TO  L103-SEX(IDX2)
               WHEN  "2"
                     MOVE  "女"      TO  L103-SEX(IDX2)
           END-EVALUATE 
      * 生年月日
           MOVE  L103T-BIRTHDAY(1:4) TO  L103-BIRTHDAY(IDX2)(1:4) 
           MOVE  "/"                 TO  L103-BIRTHDAY(IDX2)(5:1) 
           MOVE  L103T-BIRTHDAY(5:2) TO  L103-BIRTHDAY(IDX2)(6:2) 
           MOVE  "/"                 TO  L103-BIRTHDAY(IDX2)(8:1) 
           MOVE  L103T-BIRTHDAY(7:2) TO  L103-BIRTHDAY(IDX2)(9:2) 
      * 年齢
           MOVE  L100LNK-PRTKANRI-SKYYMD   TO  WRK-SYSYMD
           MOVE  L103T-BIRTHDAY            TO  WRK-BIRTHDAY
           COMPUTE WRK-NENREI              =   WRK-SYSYY
                                           -   WRK-BIRTHY
           IF    WRK-SYSMM           <         WRK-BIRTHM
                 COMPUTE WRK-NENREI  =         WRK-NENREI   -   1
           END-IF
           MOVE    WRK-NENREI              TO  WRK-Z3
           MOVE    WRK-Z3                  TO  L103-AGE(IDX2)
      * 保険者番号
           MOVE  L103T-HKNJANUM      TO  L103-HKNJANUM(IDX2)
      * 本人家族区分
           EVALUATE L103T-HONKAZKKBN
               WHEN 1
                    MOVE  "本"     TO  L103-HONKAZKKBN(IDX2)
               WHEN 2
                    MOVE  "家"     TO  L103-HONKAZKKBN(IDX2)
           END-EVALUATE
      * 記号・番号
           MOVE  L103T-KIGONUM       TO  L103-KIGONUM(IDX2)
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) start
           MOVE  L103T-KIGO          TO  L103-KIGO(IDX2)
           MOVE  L103T-BANGO         TO  L103-BANGO(IDX2)
      *=== 記号・番号１６文字以上対応 by Yoshikawa(2008/04/03) end
      * 頭書年月日
           IF    L103T-ATAMAGAKI  NOT =  SPACE
                 MOVE  L103T-ATAMAGAKI(1:4)
                          TO  L103-ATAMAGAKI(IDX2)(1:4) 
                 MOVE  "/"
                          TO  L103-ATAMAGAKI(IDX2)(5:1) 
                 MOVE  L103T-ATAMAGAKI(5:2)
                          TO  L103-ATAMAGAKI(IDX2)(6:2) 
                 MOVE  "/"      
                          TO  L103-ATAMAGAKI(IDX2)(8:1) 
                 MOVE  L103T-ATAMAGAKI(7:2)
                          TO  L103-ATAMAGAKI(IDX2)(9:2)
           END-IF
      * 公費番号
           MOVE  L103T-KOH1FTNNUM    TO  L103-KOH1FTNNUM(IDX2)
           MOVE  L103T-KOH2FTNNUM    TO  L103-KOH2FTNNUM(IDX2)
           MOVE  L103T-KOH3FTNNUM    TO  L103-KOH3FTNNUM(IDX2)
      * 入院日
           IF    L103T-NYUINYMD  NOT =  SPACE
                 MOVE  L103T-NYUINYMD(1:4)
                          TO  L103-NYUINYMD(IDX2)(1:4) 
                 MOVE  "/"
                          TO  L103-NYUINYMD(IDX2)(5:1) 
                 MOVE  L103T-NYUINYMD(5:2)
                          TO  L103-NYUINYMD(IDX2)(6:2) 
                 MOVE  "/"
                          TO  L103-NYUINYMD(IDX2)(8:1) 
                 MOVE  L103T-NYUINYMD(7:2)
                          TO  L103-NYUINYMD(IDX2)(9:2)
           END-IF
      * 退院日
           IF    L103T-TAIINYMD  NOT =  "99999999"
                 MOVE  L103T-TAIINYMD(1:4)
                          TO  L103-TAIINYMD(IDX2)(1:4) 
                 MOVE  "/"
                          TO  L103-TAIINYMD(IDX2)(5:1) 
                 MOVE  L103T-TAIINYMD(5:2)
                          TO  L103-TAIINYMD(IDX2)(6:2) 
                 MOVE  "/"
                          TO  L103-TAIINYMD(IDX2)(8:1) 
                 MOVE  L103T-TAIINYMD(7:2)
                          TO  L103-TAIINYMD(IDX2)(9:2)
           END-IF
      * 
           .
       330-PRT-DETL-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名データ明細転送
      *****************************************************************
       330-PRT-HEAD-SEC                SECTION.
      *
           ADD     1                 TO   CNT-PAGE
           MOVE    CNT-PAGE          TO   WRK-PAGE
           MOVE    WRK-PAGE          TO   L103-PAGE
           MOVE    "(在院)"          TO   L103-MIDASI
           MOVE    WRK-SYSYMDWH      TO   L103-SYSYMD
           MOVE    WRK-L100LNK-YMD1  TO   L103-YMD1
           MOVE    WRK-L100LNK-YMD2  TO   L103-YMD2
      *
      *    診療科情報
           IF    FLG-SRYKA-ALL  NOT =     "1"
                INITIALIZE                     SYS-1005-REC
                MOVE    "1005"            TO   SYS-1005-KANRICD
                IF      WRK-PRSRYKA    NOT =   SPACE
                     MOVE    WRK-PRSRYKA       TO   SYS-1005-KBNCD
                ELSE                                    
                     MOVE    L100LNK-SRYKA     TO   SYS-1005-KBNCD
                END-IF                                     
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *          MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                                    TO  SYS-1005-STYUKYMD(1:6)
      *                                        SYS-1005-EDYUKYMD(1:6)
      *          MOVE    "01"              TO  SYS-1005-STYUKYMD(7:2)
      *                                        SYS-1005-EDYUKYMD(7:2)
      *--- 2010/02/08 CHANGE START -------------------------------------
      *          MOVE    L100LNK-YMD1      TO   SYS-1005-STYUKYMD
      *          MOVE    L100LNK-YMD2      TO   SYS-1005-EDYUKYMD
                MOVE    L100LNK-YMD2      TO   SYS-1005-STYUKYMD
                MOVE    L100LNK-YMD1      TO   SYS-1005-EDYUKYMD
      *--- 2010/02/08 CHANGE END ---------------------------------------
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
                MOVE    SPA-HOSPNUM       TO  SYS-1005-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
                MOVE    L100LNK-YMD2      TO   WRK-SYMD
                MOVE    SYS-1005-REC      TO   MCPDATA-REC
                PERFORM 800-SYSKANRI-READ-SEC
                IF      FLG-SYSKANRI      =    ZERO
                    MOVE  MCPDATA-REC     TO   SYS-1005-REC
                    MOVE  SYS-1005-SRYKANAME
                                          TO   L103-SRYKA
                END-IF                                     
           ELSE                                     
                MOVE  "全科"              TO   L103-SRYKA
           END-IF                                     
      *
      *    職員情報
           IF    FLG-DRCD-ALL  NOT =     "1"
                INITIALIZE                     SYS-1010-REC
                MOVE    "1010"            TO   SYS-1010-KANRICD
                IF      WRK-PRSRYKA    NOT =   SPACE
                     MOVE    WRK-PRDRCD        TO   SYS-1010-KBNCD
                ELSE
                     MOVE    L100LNK-DRCD      TO   SYS-1010-KBNCD
                END-IF                                     
      ***  2004/12/06  MOTO
      ***        MOVE    L100LNK-YMD2        TO  WRK-SYMD
                MOVE    L100LNK-YMD2        TO  WRK-SYSYMD
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) start
      *          MOVE    L100LNK-PRTKANRI-SRYYM(1:6)
      *                                    TO  SYS-1010-STYUKYMD(1:6)
      *                                        SYS-1010-EDYUKYMD(1:6)
      *          MOVE    "01"              TO  SYS-1010-STYUKYMD(7:2)
      *                                        SYS-1010-EDYUKYMD(7:2)
      *--- 2010/02/08 CHANGE START -------------------------------------
      *          MOVE    L100LNK-YMD1      TO   SYS-1010-STYUKYMD
      *          MOVE    L100LNK-YMD2      TO   SYS-1010-EDYUKYMD
                MOVE    L100LNK-YMD2      TO   SYS-1010-STYUKYMD
                MOVE    L100LNK-YMD1      TO   SYS-1010-EDYUKYMD
      *--- 2010/02/08 CHANGE END ---------------------------------------
      *=== シス管取得不具合修正 by Yoshikawa(2008/04/03) end
                MOVE    SPA-HOSPNUM       TO  SYS-1010-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
                MOVE    SYS-1010-REC      TO   MCPDATA-REC
                PERFORM 800-SYSKANRI-READ-SEC
                IF      FLG-SYSKANRI      =    ZERO
                    MOVE  MCPDATA-REC     TO   SYS-1010-REC
                    MOVE  SYS-1010-NAME   TO   L103-DRNAME
                ELSE                                     
                    MOVE  "医師未登録"    TO   L103-DRNAME
                END-IF                                     
           ELSE                                     
                MOVE  "全医師"            TO   L103-DRNAME
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
      ***         MOVE   "A00000L103"         TO  SPRT-PRTID
               MOVE   "A00000L103.red"         TO  SPRT-PRTID
      ***  20040917 MOTOHASI MONPEに変更
               MOVE   "(在院)患者一覧"     TO  SPRT-TITLE
               MOVE    L103                TO  SPRT-PRTDATA
               MOVE    L100LNK-PRTKANRI-TERMID TO  SPRT-TERMID
               MOVE    L100LNK-PRTKANRI-OPID   TO  SPRT-OPID
               MOVE    L100LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
      ***  site-libを実行環境にする為コメントアウト
      ***         MOVE    "1"          TO  SPRT-SITEKBN
               CALL    "ORCSPRT"         USING
                                         ORCSPRTAREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
                                         SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
               MOVE    L100LNK-JOBID       TO  JOB-JOBID
               MOVE    L100LNK-SHELLID     TO  JOB-SHELLID
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    L100LNK-JOBID   TO  JOB-JOBID
           MOVE    L100LNK-SHELLID TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
                                   SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
      *--- 2010/02/08 DELETE START -------------------------------------
      *     PERFORM 900-DBDISCONNECT-SEC
      *--- 2010/02/08 DELETE END ---------------------------------------
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
      *    診患者データ一時ファイルREAD
      *****************************************************************
       900-L103T-READ-SEC                SECTION.
      *
           READ L103T-FILE  
                NEXT AT  END  MOVE  1  TO  FLG-ATEND
           END-READ
           .
       900-L103T-READ-EXT.
           EXIT.
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    WRK-SYSYMD          TO  ORC-DBYMD
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *         MOVE    "SYSKANRI-KEY10"      TO  ORC-DBPATH
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    "SYSKANRI-KEY10"    TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
                                        MCPDATA-REC
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    "SYSKANRI-KEY2"  TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
           .
       820-SYSKANRI-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    来院受診履歴情報検索
      *****************************************************************
       910-JYURRK-SELECT-SEC               SECTION.
      *
           MOVE    JYURRK-F-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *    来院受診履歴情報FETCH
      *****************************************************************
       910-JYURRK-FETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"            TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *    来院受診履歴情報 ＤＢ　クローズ処理
      *****************************************************************
       910-JYURRK-CLOSE-SEC                 SECTION.
      *
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE  "DBCLOSE"            TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
           .
       910-JYURRK-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴情報検索
      *****************************************************************
       900-NYURRK-SELECT-SEC                 SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    "PTNYUINRRK-KEY26"  TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"            USING
      *                                     MCPAREA
      *                                     ORCMCPAREA
      *                                     MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC           =   ZERO
                MOVE    ZERO         TO  FLG-PTNYUINRRK
                MOVE    MCPDATA-REC  TO  PTNYUINRRK-REC
           ELSE
                MOVE    1           TO    FLG-PTNYUINRRK
                MOVE    1           TO    FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/09) start
      *          PERFORM 900-NYURRK-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/09) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    "PTNYUINRRK-KEY26" TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"            USING
      *                                    MCPAREA
      *                                    ORCMCPAREA
      *                                    MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC           =   ZERO
                MOVE    ZERO         TO  FLG-PTNYUINRRK
                MOVE    MCPDATA-REC  TO  PTNYUINRRK-REC
           ELSE
                MOVE    1            TO  FLG-PTNYUINRRK
                MOVE    1            TO  FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/09) start
      *          PERFORM 900-NYURRK-CLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/09) end
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
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE  "DBCLOSE"          TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE  "PTNYUINRRK-KEY26" TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"            USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    "PTNYUINRRK-KEY5"  TO  ORC-DBPATH
      *     CALL     "ORCMCPSUB"            USING
      *                                     MCPAREA
      *                                     ORCMCPAREA
      *                                     MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
      *--- 2009/09/02 CHANGE START -------------------------------------
      *     MOVE    "key5"              TO  MCP-PATHNAME
           MOVE    "key4"              TO  MCP-PATHNAME
      *--- 2009/09/02 CHANGE END ---------------------------------------
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK-M
               MOVE    MCPDATA-REC  TO  PTNYUINRRK-M-REC
           ELSE
               MOVE    1           TO    FLG-PTNYUINRRK-M
               MOVE    1           TO    FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/09) start
      *         PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/09) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE    "PTNYUINRRK-KEY5" TO  ORC-DBPATH
      *     CALL    "ORCMCPSUB"            USING
      *                                    MCPAREA
      *                                    ORCMCPAREA
      *                                    MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
      *--- 2009/09/02 CHANGE START -------------------------------------
      *     MOVE    "key5"              TO  MCP-PATHNAME
           MOVE    "key4"              TO  MCP-PATHNAME
      *--- 2009/09/02 CHANGE END ---------------------------------------
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK-M
               MOVE    MCPDATA-REC  TO  PTNYUINRRK-M-REC
           ELSE
               MOVE    1            TO  FLG-PTNYUINRRK-M
               MOVE    1            TO  FLG-END
      *=== sarge対応 DEL By Takashima(2006/08/09) start
      *         PERFORM 900-M-DBCLOSE-SEC
      *=== sarge対応 DEL By Takashima(2006/08/09) end
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
      *=== sarge対応 UP By Takashima(2006/08/09) start
      *     MOVE  "DBCLOSE"          TO  MCP-FUNC
           MOVE  "DBCLOSECURSOR"    TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     MOVE  "PTNYUINRRK-KEY5"  TO  ORC-DBPATH
      *     CALL  "ORCMCPSUB"            USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC
           MOVE  "tbl_ptnyuinrrk"   TO  MCP-TABLE
      *--- 2009/09/02 CHANGE START -------------------------------------
      *     MOVE    "key5"              TO  MCP-PATHNAME
           MOVE    "key4"              TO  MCP-PATHNAME
      *--- 2009/09/02 CHANGE END ---------------------------------------
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
           .
       900-M-DBCLOSE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       930-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
           MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
      *=== sarge対応 UP By Takashima(2006/08/09) end
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
           .
       960-PTKOHINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    "DBOPEN"            TO  MCP-FUNC.
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
      *
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    "DBSTART"           TO  MCP-FUNC.
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
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
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) start
      *     CALL    "ORCMCPSUB"          USING
      *                                  MCPAREA
      *                                  ORCMCPAREA
      *                                  MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *=== Ver4.0対応 By Yoshikawa(2007/09/11) end
      *
           .
       900-DBCLOSE-EXT.
           EXIT.
