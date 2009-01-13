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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000H03A5.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 外来請求書兼領収書　印刷（ＨＣＭ０３Ｖ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/02/23    @@@@−半田   新規作成(jma-receipt-hosp 2.1.0 カスタマイズ)
      *  04/03/03    @@@@−高橋   A5用に修正
      *  04/09/27    @@@@−本橋   ORCHC03V02(jma-receipt 2.4.0)の複数診療科処理を移植 
      *  05/02/28    @@@@−鈴木   患者カナ氏名を印字
      *  05/10/17    @@@@-呉秀凡  DBアクセスMCPSUBへの変更と
      *                            Open-cobol0.23への対応
      *  05/11/14    @@@@-劉洋    その他自費表示項目が５項目から
      *                           １０項目印字に変更する
      *  06/01/21    @@@@-劉洋    今回入金額にマイナス印字されること
      *                           に変更する
      *  06/03/30    @@@@-T.T     システム管理マスタからの情報の取得を修正
      *                           (期限が設定されている場合)
      *  06/04/11    @@@@-T.T     患者カナ氏名の印字不具合を修正
      *  06/07/26    @@@@-内田    Ver3.1.0対応
      *  06/07/29    @@@@-内田    sarge対応
      *  06/09/25    @@@@-内田    Ver3.2.0対応
      *  06/10/21    @@@@-内田    Ver3.3.0対応
      *  07/03/13    @@@@-内田    Ver3.4.0対応（消費税内税）
      *  07/04/06    @@@@-内田    入金額等の０円記載
      *                           （再）の文字の編集を削除
      *  07/06/27    @@@@-大竹    自賠責時の印字変更
      *  07/08/13    @@@@-吉川   「医学管理」見出し修正
      *  07/09/21    @@@@-佐藤    4.0対応
      *  08/02/08    @@@@-田中    再印刷処理の追加
      *                           訂正時の合計印刷処理
      *  08/03/05    @@@@-高嶋    公務災害対応(ver.4.1)
      *  08/03/13    @@@@-吉川    病理診断対応(ver.4.2)
      *  08/04/03    @@@@-吉川    外来月別請求書印刷不具合の修正
      *                           労災テーブル読み込み不具合の修正
      *  08/09/29    @@@@-田中    H20年10月改正対応(協会けんぽ)
      *  08/10/22    @@@@-田中    生年月日,性別,本人家族区分,内労災診療等を追加
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-小豆沢  04/01/29  保険適用外の記載と消費税対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  HC01PARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
      *****COPY    "HC03.INC".
      * 200400303 変更
      *****     COPY    "HCM03V02.INC"  REPLACING   //HCM03V02//
      *****                             BY          //HC03//.
           COPY    "A00000H03A5.INC"  REPLACING   //HCM03V02//
                                   BY             //HC03//.
      * 20040303 変更
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *    シェル用領域
      **** COPY    "CPCOMMONSHELL.INC".
      *
      *    シェル用領域
      *    COPY    "CPSHELLTBL.INC".
      *****COPY    "ORCA-DBPATH".
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *=== 公務災害対応 by k.takashima (2008/03/05) start
           03  FLG-PTHKNINF        PIC 9(01).
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
           03  IDX-JIHI            PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
           03  IDH3                PIC 9(04).
      *
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDXM                PIC 9(02).
           03  IDK                 PIC 9(04).
           03  IDX2-1              PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
      *****03  WRK-HKNCOMBI        PIC X(40).
           03  WRK-HKNCOMBIMEI     PIC X(40).
      *
           03  WRK-SKYMONEY        PIC S9(11).
           03  WRK-MISYUMONEY      PIC S9(11).
           03  WRK-MISYU           PIC S9(11).
           03  WRK-GOKTEN          PIC S9(11).
           03  WRK-SKYMONEY-TAX-SAI    PIC S9(11).
      *
           03  WRK-DENPNUM         PIC 9(07).
           03  WRK-SRYKA           PIC X(02).
           03  WRK-HKNCOMBI        PIC X(04).
      *
           03  WRK-WARI            PIC 9(02)V9.
      *****03  WRK-Z6              PIC ZZZZZZ.
      *****03  WRK-Z62             PIC ZZZ9.9.
           03  WRK-Z6              PIC ZZZ.
           03  WRK-Z62             PIC 9.9.
      *
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-NO              PIC Z(7).
           03  WRK-Z7-G.
               05  WRK-FIL             PIC X(09).
               05  WRK-Z7              PIC Z,ZZZ,ZZZ.
           03  WRK-Z72-G.
      *****    05  WRK-FIL2            PIC X(09).
      ****     05  WRK-Z72             PIC -,---,---.
               05  WRK-FIL2            PIC X(18).
           03  WRK-Z72                 PIC S9(07).
           03  WRK-Z72-H               PIC Z,ZZZ,ZZZ.
           03  WRK-Z72-HS              REDEFINES   WRK-Z72-H
                                       PIC -,---,---.
      *---(03.04.00) LINE ADD START -----------------------------------
           03  WRK-Z72-H9              PIC Z,ZZZ,ZZ9.
      *---(03.04.00) LINE ADD END   -----------------------------------
      *    自費集計
           03  WRK-JIHI-REC.
      ****2005/11/14 @@@@-BRAND  START
      *        05  WRK-JIHI            PIC S9(07)   OCCURS  5.
               05  WRK-JIHI            PIC S9(07)   OCCURS  10.
      ****2005/11/14 @@@@-BRAND  END
           03  WRK-JIHI-TOTAL          PIC S9(08).
           03  WRK-JIHIMEI-REC.
      ****2005/11/14 @@@@-BRAND  START
      *        05  WRK-JIHIMEI         PIC X(20)   OCCURS  5.
               05  WRK-JIHIMEI         PIC X(20)   OCCURS  10.
      ****2005/11/14 @@@@-BRAND  END
      *
      *    複数科まとめ集計
           03  WRK-HKNTEN-KIN              PIC 9(07).
           03  WRK-HKNTEN-KEI              PIC 9(07)V99.
      ****2006/01/21 @@@@-BRAND  START
      *    03  WRK-NYUKIN                  PIC 9(07).
           03  WRK-NYUKIN                  PIC S9(07).
           03  WRK-NYUKIN-TOTAL            PIC S9(07).
           03  WRK-ZENMISYU                PIC S9(07).
      ****2006/01/21 @@@@-BRAND  END
      *
      * 20040223 変更
      ***     03  WRK-SRYKA-MEI               PIC X(30).
           03  WRK-SRYKA-MEI               PIC X(40).
      * 20040223 変更
           03  WRK-PAGE                    PIC 9(03).
      *
      *    文字変換
           03  WRK-ZENKAKU-G.
               05  WRK-ZENKAKU         PIC X(02)   OCCURS   10.
           03  WRK-MOJI                PIC X(10).
           03  WRK-MOJISU              PIC 9(02).
      *---(03.02.00) LINE ADD START -----------------------------------
           03  WRK-LBLSRYYMD           PIC X(10).
           03  WRK-SRYYMDG             PIC X(50).
           03  WRK-STSRYYMDG           PIC X(22).
           03  WRK-EDSRYYMDG           PIC X(22).
      *---(03.02.00) LINE ADD END   -----------------------------------
      *
      *----(03.04.01)--ADD-START---
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-HOSPTEL             PIC X(15).
      *****07/09/21  4.0対応  Satoh  START*****
           03  WRK-MSG                 PIC X(1000).
      *****07/09/21  4.0対応  Satoh  END*******
      *----(03.04.01)--ADD-END-----
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
           03  WRK-FORMID              PIC X(30).
      *--- 2008/10/22  ADD  START --------------------------------------
           03  WRK-HONKZKKBN           PIC X(04).
           03  WRK-Z73-G.
               05  WRK-FIL3            PIC X(14).
           03  WRK-Z72-H2               PIC ZZZ,ZZZ.
           03  WRK-Z72-HS2              REDEFINES  WRK-Z72-H2.
      *--- 2008/10/22  ADD  END ----------------------------------------
      *
       01  CONST-AREA.
           03  CONST-H200401           PIC X(08)   VALUE "20080401".
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      *     COPY     "ORCA-DBPATH".
      ****2005/10/17 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  END*******
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報 
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1013.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
      *----(03.04.01)--ADD-START---
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *----(03.04.01)--ADD-END-----
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *---(03.02.00) LINE ADD START -----------------------------------
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
           COPY    "CPSYUPARA03.INC".
      *---(03.02.00) LINE ADD END   -----------------------------------
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *    請求書収納パラメタ
           COPY    "CPORCHC03SUB.INC".
      *---(03.02.00) LINE ADD START -----------------------------------
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *---(03.02.00) LINE ADD END   -----------------------------------
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****07/09/21  4.0対応  Satoh  START*****
      *     COPY    "CPORCMCP.INC".
      *****07/09/21  4.0対応  Satoh  END*******
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    処方箋共通パラメタ
           COPY    "CPORCHC03.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC03AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
      *
      ***  20040927  Ver 2.4.0移植  MOTO
      ****    帳票印刷開始処理
      ***     PERFORM 400-SYOKI-PRINT-SEC
      ****
      ****    帳票編集処理
      ***     PERFORM 300-HC03-HEN-SEC
      ****
      ****    帳票印刷処理
      ***     PERFORM 400-PRINT-SEC
      *
      *---(03.02.00) LINE ADD START -----------------------------------
      *     IF      ORCHC03-HAKHOUFLG   =   ZERO
      *        対象の伝票番号分のみ
      *         PERFORM 201-ONLY-SYUNOU-SEC
           IF  (ORCHC03-PARAFLG  =  1)
               PERFORM  203-PARA-SYUNOU-SEC
      *---(03.02.00) LINE ADD END   -----------------------------------
           ELSE
      *---(03.02.00) LINE ADD START -----------------------------------
      *        発行区分による
      *         PERFORM 202-KAKHOU-SYUNOU-SEC
             IF  ORCHC03-HAKHOUFLG  =  ZERO
      *        対象の伝票番号分のみ
               PERFORM 201-ONLY-SYUNOU-SEC
             ELSE
      *        発行区分による
               PERFORM 202-KAKHOU-SYUNOU-SEC
             END-IF
      *---(03.02.00) LINE ADD END   -----------------------------------
           END-IF
      ***  20040927  Ver 2.4.0移植  MOTO
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    "HC03"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *    変更★★★★★★
      ***  20040927  Ver 2.4.0移植  MOTO
      ***     MOVE    "001"               TO  HC01PARA-CNT
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      ***  20040927  Ver 2.4.0移植  MOTO
           OPEN    OUTPUT               PRT-FILE
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
      * 20040223 追加(控分データの転送)
      *      MOVE   HC03-TYPE-A         TO  HC03-TYPE-B
      **      MOVE   HC03-NO             TO  HC03-B-NO    
      **      MOVE   HC03-SRYYMD         TO  HC03-B-SRYYMD   
      **      MOVE   "1234567"           TO  HC03-B-NO    
      **      MOVE   "平成１０年１０月"  TO  HC03-B-SRYYMD   
      * 20040223 追加
      *
      *　　印刷用データ出力
              WRITE  HC03
      *
      ******MOVE   "HC01.sh"           TO  SHELL-ID
      *     MOVE   "HC03.dia"          TO  SHELL-DIA
      *     MOVE   "HC03.def"          TO  SHELL-DEF
      *     MOVE   HC01PARA            TO  SHELL-DAT
      ***** MOVE   SYS-1031-PRTNM(3)   TO  SHELL-PRTNM 
            CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
      ***** MOVE   "HC03.dia"          TO  MKPRT-DIA
      ***** MOVE   "HC03.def"          TO  MKPRT-DEF
      * 200400303 変更
      ***** MOVE   "HCM03V02.red"      TO  MKPRT-DIA
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
      *      MOVE   "A00000H03A5.red"   TO  MKPRT-DIA
            MOVE   WRK-FORMID          TO  MKPRT-DIA
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
      * 200400303 変更
            MOVE   SPACE               TO  MKPRT-DEF
            MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   SYS-1031-PRTNM(3)   TO  MKPRT-PRTNM
      * 20040223 変更
      ***** CALL   "ORCSMKPRT"         USING
      *****                            ORCSMKPRTAREA
      * 20080208 再印刷処理の追加START t.tanaka
      *      CALL   "ORCSMKPRTSITE"     USING
            MOVE  "請求書兼領収書"     TO MKPRT-INFO
            CALL   "ORCSMKPRTSITE1"     USING
      * 20080208 再印刷処理の追加END
                                       ORCSMKPRTAREA
      *****07/09/21  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      * 20040223 変更
      *
      *   印刷用スクリプト処理
      *★   ETC-EXEC SHELL   SHELL-AREA 10
      *    MOVE    SHELL-NAME          TO  SHELLTBL-NAME
      *    MOVE    SHELL-ARG1          TO  SHELLTBL-ARG1
      *    MOVE    SHELL-ARG2          TO  SHELLTBL-ARG2
      *    MOVE    SHELL-ARG3          TO  SHELLTBL-ARG3
      *    MOVE    SHELL-ARG4          TO  SHELLTBL-ARG4
      *    MOVE    SHELL-ARG5          TO  SHELLTBL-ARG5
      *    MOVE    SHELLTBL            TO  MCPDATA-REC
      *    MOVE    "SHELL"             TO  MCP-FUNC
      *    MOVE    PATH-SHELL-SHELL    TO  MCP-PATH
      *    CALL    "MCPSUB"            USING
      *            MCPAREA
      **********   MCPDATA-REC
      *
      *
           .
       400-PRINT-EXT.
           EXIT.
      *---(03.02.00) LINE ADD START -----------------------------------
      *****************************************************************
      *   帳票印刷処理（印刷ＤＢに出力）
      *****************************************************************
       401-PRINT-SEC                SECTION.
      *
      *     MOVE    HC03-TYPE-A         TO  HC03-TYPE-B
           MOVE    ORCHC03-PRTAREA     TO  ORCSPRTAREA
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
      *     MOVE   "A00000H03A5.red"    TO  MKPRT-DIA
      *=== 外来月別請求書印刷不具合修正 by Yoshikawa(2008/04/03) start
      *     MOVE    WRK-FORMID          TO  MKPRT-DIA
           MOVE    WRK-FORMID          TO  SPRT-PRTID
      *=== 外来月別請求書印刷不具合修正 by Yoshikawa(2008/04/03) end
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
           MOVE    HC03                TO  SPRT-PRTDATA
           MOVE    "2"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *****07/09/21  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
           IF  (SPRT-RETURN  =  ZERO)
               CONTINUE
           ELSE
               DISPLAY  "ORCSPRT ERR: PTID =  " SPRT-PTID
           END-IF
      *
           .
       401-PRINT-EXT.
           EXIT.
      *---(03.02.00) LINE ADD END   -----------------------------------
      *****************************************************************
      *   対象の伝票番号分のみ処理  20040927  Ver 2.4.0移植  MOTO
      *****************************************************************
       201-ONLY-SYUNOU-SEC                SECTION.
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
      *****07/09/21  4.0対応  Satoh  START*****
      *     MOVE    SPA-HOSPID          TO  SYU-HOSPID
           MOVE    SPA-HOSPNUM          TO  SYU-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    ORCHC03-DENPNUM     TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      *     MOVE    PATH-TBL-SYUNOU-KEY     TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      *         MOVE    PATH-TBL-SYUNOU-KEY    TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
      *****07/09/21  4.0対応  Satoh  END*******
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
      ****2006/07/29 @@@@-BRAND  START
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      ****2006/07/29 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      *     MOVE    PATH-TBL-SYUNOU-KEY     TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
           CALL    "ORCDBMAIN"          USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
      *    診療科名編集
           MOVE    SPACE                TO  WRK-SRYKA-MEI
           MOVE    SYU-SRYKA            TO  WRK-SRYKA
           PERFORM 1000-SRYKAMEI-HEN-SEC
      *    保険組合せ
           PERFORM 1001-HKNCOMBI-HEN-SEC
      *
           MOVE    ORCHC03-DENPNUM     TO  WRK-DENPNUM
           MOVE    ORCHC03-SEIKYU      TO  WRK-SKYMONEY
           MOVE    ORCHC03-NYUKIN      TO  WRK-NYUKIN
           MOVE    ORCHC03-SEIKYU-TAX-SAI  TO  WRK-SKYMONEY-TAX-SAI
      *---(03.03.00) LINE ADD START -----------------------------------
           MOVE    ORCHC03-ZENMISYU-2  TO  WRK-MISYUMONEY
      *    前回請求額
           IF     (ORCHC03-ZEN-HENKIN  >   ZERO ) OR
                  (ORCHC03-KON-HENKIN (1)  >   ZERO )
      *        今回返金額
               COMPUTE WRK-MISYUMONEY      =   WRK-MISYUMONEY
                                           -   ORCHC03-ZEN-HENKIN
                                           -   ORCHC03-KON-HENKIN
                                                             (1)
           ELSE
      *        前回までの超過額
               COMPUTE WRK-MISYUMONEY      =   WRK-MISYUMONEY
                                           -   ORCHC03-KANYUKIN
               IF      WRK-MISYUMONEY      <   ZERO
                   MOVE    ZERO            TO  WRK-MISYUMONEY
               END-IF
           END-IF
      *    返金対応
           IF      (ORCHC03-HAKKOFLG   =   ZERO )  AND
                   (ORCHC03-NYUKIN     =   ZERO )  AND
                   (ORCHC03-ZEN-HENKIN >   ZERO )  AND
                   (WRK-MISYUMONEY     <   ZERO )  AND
                   ((WRK-MISYUMONEY * -1 )
                                       >   WRK-SKYMONEY)
      *        返金額が今回請求額より多い時、マイナス入金とする
               COMPUTE WRK-NYUKIN          =   WRK-SKYMONEY
                                           +   WRK-MISYUMONEY
           END-IF
      *---(03.03.00) LINE ADD END   -----------------------------------
      *
      * 20080208 訂正時の合計印刷処理の追加START t.tanaka
           IF     (ORCHC03-TEIKIN-KBN  =   1   )  AND
                  (ORCHC03-HAKKOFLG    =   2   )
               MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY
               MOVE    SYU-NYUKIN-TOTAL    TO  WRK-NYUKIN
           END-IF
      * 20080208 訂正時の合計印刷処理の追加END

      *    帳票印刷開始処理
           PERFORM 400-SYOKI-PRINT-SEC
      *
      *    帳票編集処理
           PERFORM 300-HC03-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRINT-SEC
      *
           .
       201-ONLY-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1001-REC
      *****07/09/21  4.0対応  Satoh  START*****
           INITIALIZE                  SYS-1001-REC
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*****
      ****2006/03/30 T.T  START
      *    MOVE    ZERO                TO  SYS-1001-STYUKYMD
      *    MOVE    ALL "9"             TO  SYS-1001-EDYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
      ****2006/03/30 T.T  END
      *
      *    システム管理検索
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      **    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
      *****07/09/21  4.0対応  Satoh  END*******
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      ****2006/07/29 @@@@-BRAND  START
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      ****2006/07/29 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_syskanri"       TO  MCP-TABLE
            MOVE    "key10"                TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"          USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                                   SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*****
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1002-REC
      *****07/09/21  4.0対応  Satoh  START*****
           INITIALIZE                  SYS-1002-REC
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
      ****2006/03/30 T.T  START
      *    MOVE    ZERO                TO  SYS-1002-STYUKYMD
      *    MOVE    ALL "9"             TO  SYS-1002-EDYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1002-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1002-EDYUKYMD
      ****2006/03/30 T.T  END
      *
      *    システム管理検索
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      **    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_syskanri"     TO  MCP-TABLE
            MOVE    "key10"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"        USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_syskanri"         TO  MCP-TABLE
            MOVE    "key10"                TO  MCP-PATHNAME
      *****07/09/21  4.0対応  Satoh  END*******
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1002-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1002-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      ****2006/07/29 @@@@-BRAND  START
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      ****2006/07/29 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_syskanri"         TO  MCP-TABLE
            MOVE    "key10"                TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"           USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *
      **** 出力先プリンタ名割り当て情報編集
      *    MOVE    SPACE               TO  SYS-1031-REC
      *    INITIALIZE                  SYS-1031-REC
      *    MOVE    "1031"              TO  SYS-1031-KANRICD
      *    MOVE    "*"                 TO  SYS-1031-KBNCD
      *    MOVE    SYS-1031-REC        TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *    MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      *    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      *    CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
      *                                 MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      *
      ****2005/10/17 @@@@-BRAND  END
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      ****2005/10/17 @@@@-BRAND  START
      *        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *        MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      *        PERFORM 900-KANRIMST-READ-SEC
      *        IF      MCP-RC              =   ZERO
      *            MOVE    MCPDATA-REC     TO  SYS-1031-REC
      *        END-IF
      *    ELSE
      *        INITIALIZE                  SYS-1031-REC
      *        MOVE    1               TO  FLG-SYSKANRI
      *    END-IF
      *    MOVE    "DBCLOSE"           TO  MCP-FUNC
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *    MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    CALL    "ORCMCPSUB"          USING
      *    CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
      *                                 MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      *
      ****2005/10/17 @@@@-BRAND  END
      *****                             MCPDATA-REC
      *
      *    労災医療機関情報編集
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
      ****2005/10/17 @@@@-BRAND  START
      ****2006/03/30 T.T  START
      *    MOVE    ZERO                TO  SYS-4001-STYUKYMD
      *    MOVE    ALL "9"             TO  SYS-4001-EDYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-4001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-4001-EDYUKYMD
      ****2006/03/30 T.T  END
      ****2005/10/17 @@@@-BRAND  END
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      **    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
      *****07/09/21  4.0対応  Satoh  END*******
               PERFORM 900-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-4001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      ****2006/07/29 @@@@-BRAND  START
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      ****2006/07/29 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
      ****    収納マスタ検索
      ***     INITIALIZE                  SYUNOU-REC
      ***     MOVE    SPA-HOSPID          TO  SYU-HOSPID
      ***     MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
      ***     MOVE    SPA-PTID            TO  SYU-PTID
      ***     MOVE    ORCHC03-DENPNUM     TO  SYU-DENPNUM
      ****
      ***     MOVE    SYUNOU-REC          TO  MCPDATA-REC
      ***     MOVE    "DBSELECT"          TO  MCP-FUNC
      ***     MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      ***     IF      MCP-RC              =   ZERO
      ***         MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      ***         PERFORM 980-SYUNOU-READ-SEC
      ***     ELSE
      ***         INITIALIZE                  SYUNOU-REC
      ***         MOVE    1               TO  FLG-SYUNOU
      ***     END-IF
      ****
      ***     MOVE    "DBCLOSE"           TO  MCP-FUNC
      ***     MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      ****
      ***     MOVE    SPACE                TO  WRK-SRYKA-MEI
      ****    診療科名編集
      ***     PERFORM 1000-SRYKAMEI-HEN-SEC
      ****
      ****    複数科まとめがある時、収納を集計する
      ***     IF      ORCHC03-FUKU-DENPNUM    NOT =   ZERO
      ***         PERFORM 1001-FUKU-SYUNOU-SEC
      ***     END-IF
      ****
      ****    保険組合せマスター検索
      ***     INITIALIZE                      HKNCOMBI-REC
      ***     MOVE    SPA-HOSPID          TO  COMB-HOSPID
      ***     MOVE    SPA-PTID            TO  COMB-PTID
      ***     MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
      ***     PERFORM 910-HKNCOMBI-READ-SEC
      ***     IF      FLG-HKNCOMBI        =   1
      ***          DISPLAY "*** HKNCOMBI  ERR:"   COMB-KEY "."
      ***     END-IF
      ***     PERFORM 1001-HKNCOMBI-HEN-SEC
      ****
      ***     IF    ( COMB-HKNNUM         =   "971" OR "973" )
      ****
      ****        患者労災保険情報検索
      ***         INITIALIZE                      PTRSIINF-REC
      ***         MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
      ***         MOVE    SPA-PTID            TO  PTRSI-PTID
      ***         MOVE    COMB-HKNID          TO  PTRSI-HKNID
      ***         PERFORM 910-PTRSIINF-READ-SEC
      ***     ELSE
      ***         INITIALIZE                      PTRSIINF-REC
      ***     END-IF
      *
      ***  20040927  Ver 2.4.0移植  MOTO
      *    自費名称編集
           PERFORM 1002-JIHIMSG-HEN-SEC
      ***  20040927  Ver 2.4.0移植  MOTO
      *
      *----(03.04.01)--ADD-START---
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *----(03.04.01)--ADD-END-----
      *
           .
       100-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費名称編集処理  20040927  Ver 2.4.0移植  MOTO
      *****************************************************************
       1002-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE     SPA-HOSPNUM        TO SYS-1013-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    "1013"              TO  SYS-1013-KANRICD
      ****2006/03/30 T.T  START
      *    MOVE    ZERO                TO  SYS-1013-STYUKYMD
      *    MOVE    ALL "9"             TO  SYS-1013-EDYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1013-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1013-EDYUKYMD
      ****2006/03/30 T.T  END
      *
      *    システム管理検索
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
      *     MOVE    PATH-TBL-SYSKANRI-KEY2     TO  MCP-PATH
      **    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *         MOVE    PATH-TBL-SYSKANRI-KEY2    TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
      *****07/09/21  4.0対応  Satoh  END*******
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           INITIALIZE                  WRK-JIHIMEI-REC
           PERFORM
               UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1013-REC
               IF      SPA-NYUGAIKBN       =   SYS-1013-KBNCD(1:1)
                   EVALUATE    SYS-1013-KBNCD(2:1)
      ****2005/11/14 @@@@-BRAND  START
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (5)
                       WHEN    "6"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (6)
                       WHEN    "7"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (7)
                       WHEN    "8"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (8)
                       WHEN    "9"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                    (9)
                       WHEN    "0"
                           MOVE    SYS-1013-JIHINAME   TO  WRK-JIHIMEI
                                                                   (10)
      ****2005/11/14 @@@@-BRAND  END
                   END-EVALUATE
               END-IF
      *
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "SYSKANRI-KEY2"      TO  ORC-DBPATH
      *         MOVE    PATH-TBL-SYSKANRI-KEY2      TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
      *****07/09/21  4.0対応  Satoh  END*******
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
      *
      ****2006/07/29 @@@@-BRAND  START
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      ****2006/07/29 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYSKANRI-KEY2"     TO  ORC-DBPATH
      *     MOVE    PATH-TBL-SYSKANRI-KEY2    TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_syskanri"      TO  MCP-TABLE
            MOVE    "key2"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           .
       1002-JIHIMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理  20040927  Ver 2.4.0移植  MOTO
      *****************************************************************
       1001-HKNCOMBI-HEN-SEC             SECTION.
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
      *****07/09/21  4.0対応  Satoh  START*****
      *     MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-HOSPNUM          TO  COMB-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*****
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
                DISPLAY "*** HKNCOMBI  ERR:"   COMB-KEY "."
           END-IF
      *=== 公務災害対応 by k.takashima (2008/03/05) start
      *     PERFORM 10011-HKNCOMBIMEI-HEN-SEC
           PERFORM 910-PTHKNINF-READ-SEC
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      *
           IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *
      *        患者労災保険情報検索
               INITIALIZE                      PTRSIINF-REC
      *****07/09/21  4.0対応  Satoh  START*****
      *         MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
               MOVE    SPA-HOSPNUM          TO  PTRSI-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               PERFORM 910-PTRSIINF-READ-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *=== 公務災害対応 by k.takashima (2008/03/05) start
           PERFORM 10011-HKNCOMBIMEI-HEN-SEC
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      *
           .
       1001-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理  20040927  Ver 2.4.0移植  MOTO
      *****************************************************************
       10011-HKNCOMBIMEI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
      *=== 公務災害対応 by k.takashima (2008/03/05) start
      *     STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
      *             "　"                    DELIMITED  BY  SIZE
      *             COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
      *             "　"                    DELIMITED  BY  SIZE
      *             COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
      *             "　"                    DELIMITED  BY  SIZE
      *             COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
      *             "　"                    DELIMITED  BY  SIZE
      *             COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
      *                   INTO              WRK-HKNCOMBIMEI
      *     END-STRING
           IF  ( COMB-HKNNUM   =  "971" )
           AND ( PTRSI-HKNKBN  =  "5"   )
               MOVE    PTHKN-KIGO     TO   WRK-HKNCOMBIMEI
           ELSE
      * 20080929 H20年10月改正対応START
               IF    ( SYU-SRYYMD      >=     "20081001" )
                 AND  ( COMB-HKNNUM      =      "001" )
                    INSPECT COMB-SYU-TANSEIDONAME
                           REPLACING    ALL    "政管"   BY  "協会"
               END-IF
      * 20080929 H20年10月改正対応END
               STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       "　"                    DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                             INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      *
           .
       10011-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       1000-SRYKAMEI-HEN-SEC                SECTION.
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE     SPA-HOSPNUM        TO  SYS-1005-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
      ***  20040927  Ver 2.4.0移植  MOTO
      ***     MOVE    SYU-SRYKA           TO  SYS-1005-KBNCD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
      ***  20040927  Ver 2.4.0移植  MOTO
      ****2006/03/30 T.T  START
      *    MOVE    ZERO                TO  SYS-1005-STYUKYMD
      *    MOVE    ALL "9"             TO  SYS-1005-EDYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      ****2006/03/30 T.T  END
      *
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      **    MOVE    SPA-SRYYMD          TO  ORC-DBYMD
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
               MOVE    "tbl_syskanri"   TO  MCP-TABLE
               MOVE    "key10"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *         MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
      *****07/09/21  4.0対応  Satoh  END*******
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               IF      WRK-SRYKA-MEI       =   SPACE
                   MOVE    SYS-1005-SRYKANAME2
                                               TO  WRK-SRYKA-MEI
               ELSE
      * 20040223 変更
      ***             IF      WRK-SRYKA-MEI(19:2) NOT =   SPACE
                   IF      WRK-SRYKA-MEI(39:2) NOT =   SPACE
      * 20040223 変更
                       CONTINUE
                   ELSE
                       STRING  WRK-SRYKA-MEI   DELIMITED  BY  SPACE
                               "、"            DELIMITED  BY  SIZE
                               SYS-1005-SRYKANAME2
                                               DELIMITED  BY  SPACE
                               INTO                WRK-SRYKA-MEI
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
      ****2006/07/29 @@@@-BRAND  START
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      ****2006/07/29 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      *    MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_syskanri"       TO  MCP-TABLE
            MOVE    "key10"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"          USING
      *****07/09/21  4.0対応  Satoh  END*****
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           .
       1000-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科まとめ編集処理
      *****************************************************************
      *** 1001-FUKU-SYUNOU-SEC                SECTION.
      ****
      ***     MOVE    SPACE                TO  WRK-SRYKA-MEI
      ***     INITIALIZE                  WRK-SYUNOU-REC
      ****
      ****    収納マスタ検索
      ***     INITIALIZE                  SYUNOU-REC
      ***     MOVE    SPA-HOSPID          TO  SYU-HOSPID
      ***     MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
      ***     MOVE    SPA-PTID            TO  SYU-PTID
      ***     MOVE    ORCHC03-FUKU-DENPNUM
      ***                                 TO  SYU-FUKU-DENPNUM
      ****
      ***     MOVE    SYUNOU-REC          TO  MCPDATA-REC
      ***     MOVE    "DBSELECT"          TO  MCP-FUNC
      ***     MOVE    "SYUNOU-KEY12"      TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      ***     IF      MCP-RC              =   ZERO
      ***         MOVE    "SYUNOU-KEY12"      TO  ORC-DBPATH
      ***         PERFORM 980-SYUNOU-READ-SEC
      ***     ELSE
      ***         INITIALIZE                  SYUNOU-REC
      ***         MOVE    1               TO  FLG-SYUNOU
      ***     END-IF
      ***     MOVE    ZERO                TO  FLG-CHK
      ***     PERFORM UNTIL    FLG-SYUNOU     =   1
      ****
      ***         IF      FLG-CHK             =   ZERO
      ***             MOVE    SYUNOU-REC          TO  WRK-SYUNOU-REC
      ****            入金額はクリアしておく
      ***             MOVE    ZERO                TO  WRK-SYU-NYUKIN-TOTAL
      ***             MOVE    ZERO                TO  WRK-SYU-CHOSEI
      ***             MOVE    1                   TO  FLG-CHK
      ***         ELSE
      ***             PERFORM 10011-SYUNOU-ADD-SEC
      ***         END-IF
      ****        今回入力分以外の入金額集計
      ***         IF      SYU-DENPNUM     NOT =   ORCHC03-DENPNUM
      ****            入金額
      ***             ADD     SYU-NYUKIN-TOTAL    TO  WRK-SYU-NYUKIN-TOTAL
      ****            調整金
      ***             ADD     SYU-CHOSEI          TO  WRK-SYU-CHOSEI
      ***         END-IF
      ****
      ****        診療科名編集
      ***         PERFORM 1000-SRYKAMEI-HEN-SEC
      ****
      ***         MOVE    "SYUNOU-KEY12"      TO  ORC-DBPATH
      ***         PERFORM 980-SYUNOU-READ-SEC
      ***     END-PERFORM
      ****
      ***     MOVE    "DBCLOSE"           TO  MCP-FUNC
      ***     MOVE    "SYUNOU-KEY"        TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      ****
      ****    調整金
      ***     COMPUTE WRK-SYU-CHOSEI       =   WRK-SYU-CHOSEI
      ***                                  +   ORCHC03-KON-CHOSEI
      ****    保険適用金額再計算
      ****
      ****    集計の点数から負担額計算
      ***     COMPUTE WRK-HKNTEN-KEI      =  (WRK-SYU-TOTAL-HKNTEN  *  10 )
      ***                                 *  (SPA-FTNRATE   /  100)
      ***     COMPUTE WRK-HKNTEN-KIN      =  (WRK-HKNTEN-KEI
      ***                                 +   5  )
      ***                                 /   10
      ***     COMPUTE WRK-HKNTEN-KIN      =   WRK-HKNTEN-KIN
      ***                                 *   10
      ***     MOVE    WRK-HKNTEN-KIN      TO  WRK-SYU-HKNTEKMONEY
      ****
      ****    合計した収納を編集
      ***     MOVE    WRK-SYUNOU-REC       TO  SYUNOU-REC
      ****
      ***     .
      *** 1001-FUKU-SYUNOU-EXT.
      ***     EXIT.
      ****
      *****************************************************************
      *    合計請求額計算処理
      *****************************************************************
      *** 10011-SYUNOU-ADD-SEC           SECTION.
      ****
      ****    請求額
      ***     ADD     SYU-SKYMONEY        TO  WRK-SYU-SKYMONEY
      ****    点数
      ***     PERFORM VARYING     IDX     FROM    1   BY  1
      ***             UNTIL       IDX     >   12
      ***         ADD     SYU-HKNTEN (IDX)    TO  WRK-SYU-HKNTEN (IDX)
      ***         ADD     SYU-MONEY  (IDX)    TO  WRK-SYU-MONEY  (IDX)
      ***         ADD     SYU-TGMONEY (IDX)   TO  WRK-SYU-TGMONEY(IDX)
      ***         ADD     SYU-TGMONEY-TAX (IDX)
      ***                                     TO  WRK-SYU-TGMONEY-TAX(IDX)
      ***     END-PERFORM
      ****    保険適用外消費税
      ***     ADD     SYU-TGMONEY-TAX-SAI     TO  WRK-SYU-TGMONEY-TAX-SAI
      ****    自費
      ***     PERFORM VARYING     IDX     FROM    1   BY  1
      ***             UNTIL       IDX     >   5
      ***         ADD     SYU-JIHI-TAXNASI (IDX)
      ***                                     TO  WRK-SYU-JIHI-TAXNASI(IDX)
      ***         ADD     SYU-JIHI-TAXARI (IDX)
      ***                                     TO  WRK-SYU-JIHI-TAXARI (IDX)
      ***     END-PERFORM
      ***     ADD     SYU-JIHI-TOTAL      TO  WRK-SYU-JIHI-TOTAL
      ***     ADD     SYU-JIHI-TOTAL-TAX  TO  WRK-SYU-JIHI-TOTAL-TAX
      ***     ADD     SYU-JIHI-TAX        TO  WRK-SYU-JIHI-TAX
      ****    請求金額−消費税（再掲）
      ***     ADD     SYU-SKYMONEY-TAX-SAI
      ***                                 TO  WRK-SYU-SKYMONEY-TAX-SAI
      ****
      ****    各金額
      ***     ADD     SYU-RJN-FTN         TO  WRK-SYU-RJN-FTN
      ***     ADD     SYU-KOH-FTN         TO  WRK-SYU-KOH-FTN
      ***     ADD     SYU-YKZ-FTN         TO  WRK-SYU-YKZ-FTN
      ***     .
      *** 10011-SYUNOU-ADD-EXT.
      ***     EXIT.
      ****
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HC03-HEN-SEC                SECTION.
      *
           INITIALIZE                  HC03
      *
      *    伝票番号
      ***  20040927  Ver 2.4.0移植  MOTO
      ***     MOVE    ORCHC03-DENPNUM     TO  WRK-NO  
           MOVE    WRK-DENPNUM         TO  WRK-NO  
      ***  20040927  Ver 2.4.0移植  MOTO
           MOVE    WRK-NO              TO  HC03-NO
      *    診療科
      *****MOVE    SYS-1005-SRYKANAME2 TO  HC03-MIDASI
      * 20040223 変更
      ***     MOVE    WRK-SRYKA-MEI(1:20) TO  HC03-MIDASI
           MOVE    WRK-SRYKA-MEI(1:40) TO  HC03-MIDASI
      * 20040223 変更
      *    患者番号
           MOVE    SPA-PTNUM           TO  HC03-KNJNO
      **** 20050228 追加 SUZUKI
      **** 20060411 変更 T.T
      *    患者カナ名
      ***     MOVE    SPA-KANANAME       TO  HC03-KNJKANANAME
           INITIALIZE   PTINF-REC
      *****07/09/21  4.0対応  Satoh  START*****
      *     MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF   FLG-PTINF   =  ZERO
             MOVE    PTINF-KANANAME      TO  HC03-KNJKANANAME
           END-IF
      **** 20060411 変更 T.T
      **** 20050228 ここまで
      *    患者名称
           MOVE    SPA-NAME            TO  HC03-KNJNAMW
      *    保険種別
      ***  20040922  Ver 2.4.0移植  MOTO
      ***     MOVE    WRK-HKNCOMBI        TO  HC03-HOKEN
           MOVE    WRK-HKNCOMBIMEI     TO  HC03-HOKEN
      ***  20040922  Ver 2.4.0移植  MOTO
      *    負担割合
           IF      SYU-PTFTNRATE       >   ZERO
               COMPUTE WRK-WARI        =   SYU-PTFTNRATE   /   10
               IF      SYU-PTFTNRATE(3:1)  =   ZERO
                   MOVE    WRK-WARI            TO  WRK-Z6
                   MOVE    WRK-Z6              TO  HC03-FUTAN
               ELSE
                   MOVE    WRK-WARI            TO  WRK-Z62
                   MOVE    WRK-Z62             TO  HC03-FUTAN
               END-IF
           END-IF
      *--- 2008/10/22  ADD  START --------------------------------------
      *    生年月日
           MOVE    PTINF-BIRTHDAY         TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:22)      TO  HC03-TANJYOYMD(1:22)
           MOVE   "生"   TO     HC03-TANJYOYMD(23:2)
      *    性別
           EVALUATE    PTINF-SEX
             WHEN    "1"
               MOVE    "男"           TO  HC03-SEX
             WHEN    "2"
               MOVE    "女"           TO  HC03-SEX
           END-EVALUATE
      *--- 2008/10/22  ADD  END ----------------------------------------
      *    診療日
      *---(03.02.00) LINE ADD START -----------------------------------
      *     MOVE    SYU-SRYYMD          TO  WRK-SYMD
      *     PERFORM 31012-SEIWA-HEN-SEC
      *     MOVE    WRK-HENYMDG         TO  HC03-SRYYMD
           PERFORM 3101-SRYYMD-HEN-SEC
           MOVE    WRK-LBLSRYYMD       TO  HC03-LBLSRYYMD
           MOVE    WRK-SRYYMDG         TO  HC03-SRYYMD
      *---(03.02.00) LINE ADD END   -----------------------------------
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HC03-HAKYMD
      *
      *----(03.04.01)--UPD-START---
      *    医療機関住所
      *    MOVE    SYS-1002-ADRS       TO  HC03-IRYOADRS
      *    医療機関名
      *    MOVE    SYS-1001-HOSPNAME   TO  HC03-IRYOMEI
      *    医療機関ＴＥＬ
      **** MOVE    SYS-1002-TEL        TO  HC03-IRYOTEL
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HC03-IRYOADRS
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HC03-IRYOMEI
      *    医療機関ＴＥＬ
           MOVE    WRK-HOSPTEL         TO  HC03-IRYOTEL
      *----(03.04.01)--UPD-END-----
      *
      *    自費名称編集
           PERFORM 3102-JIHIMSG-HEN-SEC
      *
           MOVE    SPACE               TO  WRK-Z7-G
           MOVE    SPACE               TO  WRK-Z72-G
      *    診療科
           MOVE    SYU-HKNTEN (01)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO1
      *    指導科
           MOVE    SYU-HKNTEN (02)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO2
      *    在宅科
           MOVE    SYU-HKNTEN (03)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO3
      *    投薬科
           MOVE    SYU-HKNTEN (04)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO4
      *    注射科
           MOVE    SYU-HKNTEN (05)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO5
      *    処置科
           MOVE    SYU-HKNTEN (06)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO6
      *    手術科
           MOVE    SYU-HKNTEN (07)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO7
      *    麻酔
           MOVE    SYU-HKNTEN (08)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO8
      *    検査科
           MOVE    SYU-HKNTEN (09)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G          TO  HC03-HKNRYO9
      *    Ｘ線科
           MOVE    SYU-HKNTEN (10)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO10
      *    リハビリ
           MOVE    SYU-HKNTEN (11)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO11
      *    精神科専門
           MOVE    SYU-HKNTEN (12)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO12
      *    放射線治療
           MOVE    SYU-HKNTEN (13)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO13
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
      **    入院料
      *    病理診断
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
           MOVE    SYU-HKNTEN (14)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO14
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
      **    療養担当手当て
      *    入院料
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
           MOVE    SYU-HKNTEN (15)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO15
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
      *    療養担当手当て
           MOVE    SYU-HKNTEN (16)     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HKNRYO16
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
      *    見出し
           PERFORM  3001-MEISAI-HEN-SEC
      *    合計点数
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
      *     COMPUTE WRK-GOKTEN          =   SYU-HKNTEN (01)
      *                                 +   SYU-HKNTEN (02)
      *                                 +   SYU-HKNTEN (03)
      *                                 +   SYU-HKNTEN (04)
      *                                 +   SYU-HKNTEN (05)
      *                                 +   SYU-HKNTEN (06)
      *                                 +   SYU-HKNTEN (07)
      *                                 +   SYU-HKNTEN (08)
      *                                 +   SYU-HKNTEN (09)
      *                                 +   SYU-HKNTEN (10)
      *                                 +   SYU-HKNTEN (11)
      *                                 +   SYU-HKNTEN (12)
      *                                 +   SYU-HKNTEN (13)
      *                                 +   SYU-HKNTEN (14)
      *                                 +   SYU-HKNTEN (15)
           COMPUTE WRK-GOKTEN          =   SYU-TOTAL-HKNTEN
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-GOKTEN
      *
      *=== 自賠責時の印字変更 DEL & ADD BY y.otake (2007/06/27) start
      *    労災・自賠の時、点数表示なし
      *     IF    ( COMB-HKNNUM             =    "971"  OR  "973" )
      *    労災・自賠で患者請求が無い場合、点数表示なし
      *=== 公務災害対応 by k.takashima (2008/03/05) start
      *     IF    ( COMB-HKNNUM             =    "971"  AND
      *             SYS-4001-RSIIRYOKBN     =    "1" )  OR
      *           ( COMB-HKNNUM             =    "973"  AND
      *             SYU-PTFTNRATE           =    ZERO )
           IF    ( COMB-HKNNUM             =    "971"
           AND     PTRSI-HKNKBN        NOT =    "5"
           AND     SYS-4001-RSIIRYOKBN     =    "1" )
           OR    ( COMB-HKNNUM             =    "971"
           AND     PTRSI-HKNKBN            =    "5"
           AND     SYU-PTFTNRATE           =    ZERO )
           OR    ( COMB-HKNNUM             =    "973"
           AND     SYU-PTFTNRATE           =    ZERO )
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      *=== 自賠責時の印字変更 DEL & ADD BY y.otake (2007/06/27) end
               MOVE    SPACE               TO  HC03-HKNRYO1
               MOVE    SPACE               TO  HC03-HKNRYO2
               MOVE    SPACE               TO  HC03-HKNRYO3
               MOVE    SPACE               TO  HC03-HKNRYO4
               MOVE    SPACE               TO  HC03-HKNRYO5
               MOVE    SPACE               TO  HC03-HKNRYO6
               MOVE    SPACE               TO  HC03-HKNRYO7
               MOVE    SPACE               TO  HC03-HKNRYO8
               MOVE    SPACE               TO  HC03-HKNRYO9
               MOVE    SPACE               TO  HC03-HKNRYO10
               MOVE    SPACE               TO  HC03-HKNRYO11
               MOVE    SPACE               TO  HC03-HKNRYO12
               MOVE    SPACE               TO  HC03-HKNRYO13
               MOVE    SPACE               TO  HC03-HKNRYO14
               MOVE    SPACE               TO  HC03-HKNRYO15
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
               MOVE    SPACE               TO  HC03-HKNRYO16
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
               MOVE    ALL "*"             TO  HC03-GOKTEN
           END-IF
      *
      *--- 2008/10/22  ADD  START  --------------------------------------
      *    本人家族区分
           EVALUATE  COMB-HONKZKKBN
             WHEN  1
               MOVE    "本人"   TO    HC03-HONKZKKBN
             WHEN  2
               MOVE    "家族"   TO    HC03-HONKZKKBN
             WHEN  OTHER
               MOVE    SPACE    TO    HC03-HONKZKKBN
           END-EVALUATE 
      *    労災・自賠の診察料等の記載
           IF   (SYU-RSI-TOTAL     NOT =   ZERO) AND
                (SYS-4001-RSIPRTKBN    =   "2")
             EVALUATE    COMB-HKNNUM
               WHEN    "971"
                 MOVE  "内労災診察等"     TO      HC03-RSIMIDASI
                 MOVE  "（"               TO      HC03-RSIMIDASI2
                 MOVE  "）"               TO      HC03-RSIMIDASI3
               WHEN    "973"
                 MOVE  "内自賠診察等"     TO      HC03-RSIMIDASI
                 MOVE  "（"               TO      HC03-RSIMIDASI2
                 MOVE  "）"               TO      HC03-RSIMIDASI3
             END-EVALUATE
             MOVE    SYU-RSI-TOTAL        TO      WRK-Z72
             PERFORM 820-HENKAN-SEC
             MOVE    WRK-Z72-G            TO      HC03-RSIBUN
             MOVE    "円"                 TO      HC03-RSIEN
           ELSE
             MOVE    SPACE              TO      HC03-RSIMIDASI
             MOVE    SPACE              TO      HC03-RSIMIDASI2
             MOVE    SPACE              TO      HC03-RSIMIDASI3
             MOVE    SPACE              TO      HC03-RSIBUN
             MOVE    SPACE              TO      HC03-RSIEN
           END-IF
      *--- 2008/10/22  ADD  END  ----------------------------------------
      *    保険適用分
           MOVE    SYU-HKNTEKMONEY     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HOKENBUN
      *
      *    保険適用外編集
           PERFORM     VARYING   IDX-JIHI FROM  1  BY  1
      *=== 公務災害対応 by k.takashima (2008/03/05) start
      *                 UNTIL     IDX-JIHI  >  15
                       UNTIL     IDX-JIHI  >  16
      *=== 公務災害対応 by k.takashima (2008/03/05) end
               MOVE   ZERO      TO    WRK-Z72
               COMPUTE   WRK-Z72   =   SYU-TGMONEY(IDX-JIHI)  +
                                       SYU-TGMONEY-TAX(IDX-JIHI)
               PERFORM   800-HENKAN-SEC
               MOVE    WRK-Z72-G           TO  HC03-JIHIBUN(IDX-JIHI)
           END-PERFORM
      *
      *    保険適用外消費税（再掲）
           MOVE    SYU-TGMONEY-TAX-SAI     TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G               TO  HC03-HOKENZEI
      *
           IF    ( COMB-HKNNUM             =    "971"
                                           OR   "973" )
      *        労災・自賠の時、点数表示なし
      *=== 公務災害対応 by k.takashima (2008/03/05) start
      *         EVALUATE    TRUE
      *         WHEN      ( COMB-HKNNUM             =    "971" )
      *             AND   ( SYS-4001-RSIIRYOKBN NOT =   "1" )
      **
      *         WHEN      ( COMB-HKNNUM             =    "973" )
      *             AND   ( PTRSI-HKNKBN        =    "4" ) 
      *             AND   ( SYU-PTFTNRATE   NOT =   ZERO  )
               IF    ( COMB-HKNNUM             =    "971"
               AND     PTRSI-HKNKBN        NOT =    "5"
               AND     SYS-4001-RSIIRYOKBN     =    "1" )
               OR    ( COMB-HKNNUM             =    "971"
               AND     PTRSI-HKNKBN            =    "5"
               AND     SYU-PTFTNRATE           =    ZERO )
               OR    ( COMB-HKNNUM             =    "973"
               AND     SYU-PTFTNRATE           =    ZERO )
                   MOVE    SPACE           TO  HC03-HOKENBUN
           ELSE
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      *
                   COMPUTE WRK-GOKTEN      =    SYU-HKNTEKMONEY
                                           +    SYU-RSISHOSHIN-MONEY
                                           +    SYU-RSISAISHIN-MONEY
                                           +    SYU-RSISDO-MONEY
                                           +    SYU-RSIETC-MONEY
                   MOVE    WRK-GOKTEN      TO  WRK-Z72
                   PERFORM 800-HENKAN-SEC
                   MOVE    WRK-Z72-G           TO  HC03-HOKENBUN
      *=== 公務災害対応 by k.takashima (2008/03/05) start
      *         WHEN    OTHER
      *             MOVE    SPACE               TO  HC03-HOKENBUN
      *         END-EVALUATE
               END-IF
      *=== 公務災害対応 by k.takashima (2008/03/05) end
           END-IF
      *
      *    保険適用外分
           MOVE    ZERO                TO  WRK-GOKTEN
      **** PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   10
      *        COMPUTE WRK-GOKTEN      =   WRK-GOKTEN
      *                                +  (SYU-TGMONEY (IDX)
      *                                +   SYU-TGMONEY-TAX  (IDX) )
      *****END-PERFORM
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
      *     COMPUTE WRK-GOKTEN          =   SYU-TGMONEY (16)
      *                                 +   SYU-TGMONEY-TAX  (16)
           COMPUTE WRK-GOKTEN          =   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
      *****MOVE    SYU-TOTAL-TGMONEY   TO  WRK-Z72
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-HOKENGAI
      *    自費１から１０
           MOVE    ZERO                TO  WRK-JIHI-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
      ****2005/11/14 @@@@-BRAND  START
      *            UNTIL       IDX     >  5
                   UNTIL       IDX     >  10 
      ****2005/11/14 @@@@-BRAND  END
               COMPUTE WRK-JIHI (IDX)      =   SYU-JIHI-TAXNASI (IDX)
                                           +   SYU-JIHI-TAXARI  (IDX)
               MOVE    WRK-JIHI (IDX)  TO  WRK-Z72
               PERFORM 800-HENKAN-SEC
               MOVE    WRK-Z72-G       TO  HC03-JIHI(IDX)
           END-PERFORM
      *
      *    自費計
           COMPUTE WRK-JIHI-TOTAL      =   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
      *    自費消費税
           MOVE    SYU-JIHI-TAX        TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
      ****2005/11/14 @@@@-BRAND  START
      *    MOVE    WRK-Z72-G           TO  HC03-JIHI(7)
           MOVE    WRK-Z72-G           TO  HC03-JIHI(12)
      ****2005/11/14 @@@@-BRAND  END
      *    自費合計
      *---(03.04.00) LINE ADD START -----------------------------------
      *     COMPUTE WRK-GOKTEN          =   WRK-JIHI-TOTAL
      *                                 +   SYU-JIHI-TAX
           IF      SYU-SRYYMD          <=  "20070331"
             COMPUTE WRK-GOKTEN        =   WRK-JIHI-TOTAL
                                       +   SYU-JIHI-TAX
           ELSE
             MOVE    WRK-JIHI-TOTAL    TO  WRK-GOKTEN
           END-IF
      *---(03.04.00) LINE ADD END   -----------------------------------
           MOVE    WRK-GOKTEN          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
      ****2005/11/14 @@@@-BRAND  START
      *    MOVE    WRK-Z72-G           TO  HC03-JIHI(6)
           MOVE    WRK-Z72-G           TO  HC03-JIHI(11)
      ****2005/11/14 @@@@-BRAND  END
      *    薬剤一部負担
           MOVE    SYU-YKZ-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-YAKUZAI
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-ROUJIN
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-KOUHI
      ****    複数科まとめの時、＊とする
      ***     IF      ORCHC03-FUKU-DENPNUM    >   ZERO
      ***         IF      SYU-RJN-FTN         >   ZERO
      ***             MOVE    ALL "*"             TO  HC03-ROUJIN
      ***         END-IF
      ***         IF      SYU-KOH-FTN         >   ZERO
      ***             MOVE    ALL "*"             TO  HC03-KOUHI
      ***         END-IF
      ***     END-IF
      ****
      *    調整金
           MOVE    SYU-CHOSEI          TO  WRK-Z72
           PERFORM 800-HENKAN-SEC
           MOVE    WRK-Z72-G           TO  HC03-CHOSEI
      *    今回請求額
      ***  20040927  Ver 2.4.0移植  MOTO
      ***     MOVE    ORCHC03-SEIKYU      TO  WRK-Z72
      ***     PERFORM 800-HENKAN-SEC
      ***     MOVE    WRK-Z72-G           TO  HC03-KONSEIKYU
      ***     MOVE    ORCHC03-SEIKYU      TO  WRK-SKYMONEY
           MOVE    WRK-SKYMONEY        TO  WRK-Z72
      *---(03.04.00) LINE ADD START -----------------------------------
      *     PERFORM 800-HENKAN-SEC
           PERFORM 810-HENKAN-SEC
      *---(03.04.00) LINE ADD END   -----------------------------------
           MOVE    WRK-Z72-G           TO  HC03-KONSEIKYU
      ***  20040927  Ver 2.4.0移植  MOTO
      ****    複数科まとめの時、まとめ額
      ***     IF      ORCHC03-FUKU-DENPNUM    >   ZERO
      ***         MOVE    ORCHC03-KON-SEIKYU  TO  WRK-Z72
      ***         PERFORM 800-HENKAN-SEC
      ***         MOVE    WRK-Z72-G           TO  HC03-KONSEIKYU
      ***         MOVE    ORCHC03-KON-SEIKYU  TO  WRK-SKYMONEY
      ***     END-IF
      *    今回請求額消費税（再掲）
      ***  20040922  Ver 2.4.0移植  MOTO
      ***     MOVE    ORCHC03-SEIKYU-TAX-SAI TO  WRK-Z72
      ***     PERFORM 800-HENKAN-SEC
      ***     MOVE    WRK-Z72-G           TO  HC03-SEIKYUZEI
           MOVE    WRK-SKYMONEY-TAX-SAI    TO  WRK-Z72
      *---(03.04.00) LINE ADD START -----------------------------------
      *     PERFORM 800-HENKAN-SEC
           PERFORM 810-HENKAN-SEC
      *---(03.04.00) LINE ADD END   -----------------------------------
           MOVE    WRK-Z72-G           TO  HC03-SEIKYUZEI
      ***  20040922  Ver 2.4.0移植  MOTO
      *    複数科まとめの時、まとめ額
      ***     IF      ORCHC03-FUKU-DENPNUM    >   ZERO
      ***         MOVE    WRK-SYU-SKYMONEY-TAX-SAI
      ***                                     TO  WRK-Z72
      ***         PERFORM 800-HENKAN-SEC
      ***         MOVE    WRK-Z72-G       TO  HC03-SEIKYUZEI
      ***     END-IF
      *
      *    今回入金額
      ***  20040922  Ver 2.4.0移植  MOTO
      ********MOVE    SYU-NYUKIN-TOTAL    TO  WRK-Z7
      ***     MOVE    ORCHC03-NYUKIN      TO  WRK-Z72
      ***     PERFORM 800-HENKAN-SEC
      ***     MOVE    WRK-Z72-G           TO  HC03-KONNYUKIN
           MOVE    WRK-NYUKIN          TO  WRK-Z72
      *---(03.04.00) LINE ADD START -----------------------------------
      *     PERFORM 800-HENKAN-SEC
           PERFORM 810-HENKAN-SEC
      *---(03.04.00) LINE ADD END   -----------------------------------
           MOVE    WRK-Z72-G           TO  HC03-KONNYUKIN
      ***  20040922  Ver 2.4.0移植  MOTO
      ****    複数科まとめの時、まとめ額
      ***     IF      ORCHC03-FUKU-DENPNUM    >   ZERO
      ***         MOVE    SPACE               TO  HC03-KONNYUKIN
      ***         MOVE    SPACE               TO  WRK-Z7-G
      ***         COMPUTE WRK-NYUKIN          =   SYU-NYUKIN-TOTAL
      ***                                     +   ORCHC03-NYUKIN
      ***         MOVE    WRK-NYUKIN          TO  WRK-Z72
      ***         PERFORM 800-HENKAN-SEC
      ***         MOVE    WRK-Z72-G          TO  HC03-KONNYUKIN
      ***     END-IF
      *    前回未収額計算
      *!!! PERFORM 3101-MISYU-HEN-SEC
      *****MOVE    ORCHC03-ZENMISYU    TO  WRK-MISYUMONEY
           MOVE    WRK-MISYUMONEY      TO  WRK-Z72
      *---(03.04.00) LINE ADD START -----------------------------------
      *     PERFORM 800-HENKAN-SEC
           PERFORM 810-HENKAN-SEC
      *---(03.04.00) LINE ADD END   -----------------------------------
           MOVE    WRK-Z72-G           TO  HC03-ZENMISYU
      *    合計請求額
           COMPUTE WRK-GOKTEN          =   WRK-SKYMONEY
                                       +   WRK-MISYUMONEY
           MOVE    WRK-GOKTEN          TO  WRK-Z72
      *---(03.04.00) LINE ADD START -----------------------------------
      *     PERFORM 800-HENKAN-SEC
           PERFORM 810-HENKAN-SEC
      *---(03.04.00) LINE ADD END   -----------------------------------
           MOVE    WRK-Z72-G           TO  HC03-GOUKEIGAK
      *
      *    発行区分
           EVALUATE    ORCHC03-HAKKOFLG
      *---(03.04.00) LINE ADD START -----------------------------------
      *         WHEN    1
      *             MOVE    "（再）"        TO  HC03-MIDASHI
      *---(03.04.00) LINE ADD END   -----------------------------------
               WHEN    2
                   MOVE    "（訂正）"      TO  HC03-MIDASHI
           END-EVALUATE
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
           IF    ( SYU-SRYYMD              >=  CONST-H200401 )
               MOVE    "A00000H03A5N.red"       TO  WRK-FORMID
           ELSE
               MOVE    "A00000H03A5O.red"       TO  WRK-FORMID
           END-IF
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
      *
           .
       300-HC03-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   明細見出編集処理
      *****************************************************************
       3001-MEISAI-HEN-SEC                SECTION.
      *
           MOVE  "初・再診料"      TO  HC03-MEISAI (1)
      *=== 「医学管理」見出し修正 By Yoshikawa(2007/08/13) start
      *     MOVE  "医 学 管 理 料"      TO  HC03-MEISAI (2)
           MOVE  "医学管理等"      TO  HC03-MEISAI (2)
      *=== 「医学管理」見出し修正 By Yoshikawa(2007/08/13) end
           MOVE  "在 宅 医 療"      TO  HC03-MEISAI (3)
           MOVE  "投　 　　薬"      TO  HC03-MEISAI (4)
           MOVE  "注　 　　射"      TO  HC03-MEISAI (5)
           MOVE  "処 　　　置"      TO  HC03-MEISAI (6)
           MOVE  "手 　　　術"      TO  HC03-MEISAI (7)
           MOVE  "麻 　　　酔"      TO  HC03-MEISAI (8)
           MOVE  "検 　　　査"      TO  HC03-MEISAI (9)
           MOVE  "画 像 診 断"      TO  HC03-MEISAI (10)
           MOVE  "リハビリテーション"  TO  HC03-MEISAI (11)
           MOVE  "精神科専門療法"      TO  HC03-MEISAI (12)
           MOVE  "放射線治療"      TO  HC03-MEISAI (13)
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
      *     MOVE  "入　院　料　等"      TO  HC03-MEISAI (14)
      *     MOVE  "療養担当手当"        TO  HC03-MEISAI (15)
           MOVE  "病 理 診 断"      TO  HC03-MEISAI (14)
           MOVE  "入 院 料 等"      TO  HC03-MEISAI (15)
           MOVE  "療養担当手当"        TO  HC03-MEISAI (16)
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
      *    明細削除
      *    療養担当手当て
      *=== 病理診断対応 by Yoshikawa(2008/03/13) start
      *     IF    (SYU-HKNTEN(15)       =  ZERO)  AND
      *           (SYU-TGMONEY(15)      =  ZERO)  AND
      *           (SYU-TGMONEY-TAX(15)  =  ZERO)
      *         MOVE  SPACE             TO  HC03-MEISAI (15)
      *     END-IF
           IF    (SYU-HKNTEN (16)      =  ZERO)  AND
                 (SYU-TGMONEY (16)     =  ZERO)  AND
                 (SYU-TGMONEY-TAX(16)  =  ZERO)
             MOVE  SPACE                 TO  HC03-MEISAI (16)
           END-IF
      *=== 病理診断対応 by Yoshikawa(2008/03/13) end
      *
           .
       3001-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   複数科・保険入力の印刷処理  20040922  Ver 2.4.0移植  MOTO
      *****************************************************************
       202-KAKHOU-SYUNOU-SEC                SECTION.
      *
           INITIALIZE                  ORCHC03SUBAREA
      *    収納マスタまとめ計算
           CALL    "ORCHC03SUB"        USING
                                       SPA-AREA
                                       ORCHC03AREA
                                       ORCHC03SUBAREA
      ****2006/01/21 @@@@-BRAND  START
      *    入金合計
            MOVE    ZERO                TO  WRK-NYUKIN-TOTAL
                  PERFORM VARYING IDX-SYU     FROM    1   BY  1
                     UNTIL   IDX-SYU     >   ORCHC03SUB-MAX
                    ADD     ORCHC03SUB-NYUKIN-TOTAL(IDX-SYU)
                                             TO  WRK-NYUKIN-TOTAL
                  END-PERFORM
      *    入金合計から前回未収額の入金額を求める
           IF      ORCHC03-NYUKIN      >   WRK-NYUKIN-TOTAL
               COMPUTE WRK-ZENMISYU    =   ORCHC03-NYUKIN
                                       -   WRK-NYUKIN-TOTAL
           ELSE
               MOVE    ZERO            TO  WRK-ZENMISYU
           END-IF
      ****2006/01/21 @@@@-BRAND  END
      *
           PERFORM VARYING IDX-SYU     FROM    1   BY  1
                   UNTIL   IDX-SYU     >   ORCHC03SUB-MAX
               MOVE    ORCHC03SUB-SYUNOU-REC(IDX-SYU)
                                           TO  SYUNOU-REC
      *        診療科名編集
               MOVE    SPACE               TO  WRK-SRYKA-MEI
               IF      ORCHC03SUB-DOUJI-SRYKA-G (IDX-SYU)
                                           =   SPACE
                   MOVE    SYU-SRYKA           TO  WRK-SRYKA
                   PERFORM 1000-SRYKAMEI-HEN-SEC
               ELSE
                   PERFORM 2021-GRP-SRYKAMEI-HEN-SEC
               END-IF
      *        保険組合せ
               IF      SYU-HKNCOMBINUM     =   ZERO  OR  SPACE
                   MOVE    "複数保険使用"      TO  WRK-HKNCOMBIMEI
                   MOVE    ZERO                TO  SYU-PTFTNRATE
               ELSE
                   PERFORM 1001-HKNCOMBI-HEN-SEC
               END-IF
      *
               MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
               MOVE    ORCHC03SUB-SKYMONEY (IDX-SYU)
                                           TO  WRK-SKYMONEY
               MOVE    ORCHC03SUB-NYUKIN-TOTAL (IDX-SYU)
                                           TO  WRK-NYUKIN
               MOVE    ORCHC03SUB-SKYMONEY-TAX-SAI (IDX-SYU)
                                           TO  WRK-SKYMONEY-TAX-SAI
      ****2006/01/21 @@@@-BRAND  START
      *        １枚目に前回未収額を表示する
               IF      IDX-SYU             =   1
      *---(03.03.00) LINE ADD START -----------------------------------
                  AND  (ORCHC03-HAKKOFLG   =   ZERO )
      ***********  MOVE    ORCHC03-ZENMISYU    TO  WRK-MISYUMONEY
                   MOVE    ORCHC03-ZENMISYU-2  TO  WRK-MISYUMONEY
      *---(03.03.00) LINE ADD END   -----------------------------------
                  COMPUTE WRK-NYUKIN          =   WRK-NYUKIN
                                              +   WRK-ZENMISYU
                ELSE
                  MOVE    ZERO                TO  WRK-MISYUMONEY
                END-IF
      *---(03.03.00) LINE ADD START -----------------------------------
               IF      IDX-SYU             =   1
                   IF     (ORCHC03-ZEN-HENKIN  >   ZERO )  OR
                          (ORCHC03SUB-HENKIN (IDX-SYU) >  ZERO)
                       COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                               -   ORCHC03-ZEN-HENKIN
                                               -   ORCHC03SUB-HENKIN
                                                         (IDX-SYU)
                   ELSE
                       COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                               -   ORCHC03-KANYUKIN
                       IF      WRK-MISYUMONEY      <   ZERO
                           MOVE    ZERO            TO  WRK-MISYUMONEY
                       END-IF
                   END-IF
      *            返金対応
                   IF      (ORCHC03-HAKKOFLG   =   ZERO )  AND
                           (WRK-NYUKIN         =   ZERO )  AND
                           (ORCHC03-ZEN-HENKIN >   ZERO )  AND
                           (WRK-MISYUMONEY     <   ZERO )  AND
                           ((WRK-MISYUMONEY * -1)
                                               >   WRK-SKYMONEY)
      *            返金額が今回請求額より多い時、マイナス入金とする
                       COMPUTE WRK-NYUKIN      =   WRK-SKYMONEY
                                               +   WRK-MISYUMONEY
                   END-IF
               ELSE
                   COMPUTE WRK-MISYUMONEY  =   WRK-MISYUMONEY
                                           -   ORCHC03SUB-HENKIN
                                                         (IDX-SYU)
               END-IF
      *---(03.03.00) LINE ADD END   -----------------------------------
      ****2006/01/21 @@@@-BRAND  END
      *
      *        帳票印刷開始処理
               PERFORM 400-SYOKI-PRINT-SEC
      *
      *        帳票編集処理
               PERFORM 300-HC03-HEN-SEC
      *
      *        帳票印刷処理
               PERFORM 400-PRINT-SEC
           END-PERFORM
      *
           .
       202-KAKHOU-SYUNOU-EXT.
           EXIT.
      *
      *---(03.02.00) LINE ADD START -----------------------------------
      *****************************************************************
      *    パラメタテーブルからの印刷処理
      *****************************************************************
       203-PARA-SYUNOU-SEC                   SECTION.
      *
           PERFORM  900-PARA-KEY5-SEL-SEC
      *
           PERFORM  UNTIL  ( FLG-PARA  NOT =  ZERO )
      *
               MOVE    PARA-DATA-REC         TO  SYUPARA03-AREA
               MOVE    SYUPARA03-SYU-DATA    TO  SYUNOU-REC
      *
               MOVE    SYUPARA03-DENPNUM     TO  WRK-DENPNUM
               MOVE    SYUPARA03-SEIKYU      TO  WRK-SKYMONEY
               MOVE    SYUPARA03-SEIKYU-TAX-SAI
                                             TO  WRK-SKYMONEY-TAX-SAI
               MOVE    SYUPARA03-NYUKIN      TO  WRK-NYUKIN
               MOVE    SYUPARA03-MISYU       TO  WRK-MISYUMONEY
               MOVE    SYUPARA03-SRYKAMEI    TO  WRK-SRYKA-MEI
      *
               INITIALIZE                        HKNCOMBI-REC
                                                 PTRSIINF-REC
               IF  ( SYUPARA03-HKNCOMBI-MAX  =  1 )
                   PERFORM  1001-HKNCOMBI-HEN-SEC
               END-IF
      *
               MOVE    SYUPARA03-HKNCOMBIMEI TO  WRK-HKNCOMBIMEI
      *
               IF  ( ORCHC03-PRTKBN  =  "1" )
      *
      *            帳票編集処理
                   PERFORM  300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM  401-PRINT-SEC
               ELSE
      *
      *            帳票印刷開始処理
                   PERFORM  400-SYOKI-PRINT-SEC
      *
      *            帳票編集処理
                   PERFORM  300-HC03-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM  400-PRINT-SEC
               END-IF
      *
               PERFORM  900-PARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_para"                TO  MCP-TABLE
           MOVE    "key5"                    TO  MCP-PATHNAME
           PERFORM  990-DBCLOSE-SEC
      *
           .
       202-KAKHOU-SYUNOU-EXT.
           EXIT.
      *
      *---(03.02.00) LINE ADD END   -----------------------------------
      *****************************************************************
      *    複数診療科編集処理  20040922  Ver 2.4.0移植  MOTO
      *****************************************************************
       2021-GRP-SRYKAMEI-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDH3    FROM    1   BY  1
                   UNTIL       IDH3    >   10
               IF      ORCHC03SUB-DOUJI-SRYKA (IDX-SYU IDH3)
                                       NOT =   SPACE
                   MOVE    ORCHC03SUB-DOUJI-SRYKA(IDX-SYU IDH3)
                                           TO  WRK-SRYKA
                   PERFORM 1000-SRYKAMEI-HEN-SEC
               END-IF
           END-PERFORM
           .
       2021-GRP-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理
      *****************************************************************
       800-HENKAN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
      *****MOVE    WRK-Z72             TO  KANACHK-MAE-INPUT
           IF      WRK-Z72             <   ZERO
               MOVE    WRK-Z72             TO  WRK-Z72-HS
               MOVE    WRK-Z72-HS          TO  KANACHK-MAE-INPUT
           ELSE
               MOVE    WRK-Z72             TO  WRK-Z72-H
               MOVE    WRK-Z72-H           TO  KANACHK-MAE-INPUT
           END-IF
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-Z72-G
           END-IF
           .
       800-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角全角変換処理（ゼロ表示用）
      *****************************************************************
       810-HENKAN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           IF      WRK-Z72             <   ZERO
               MOVE    WRK-Z72             TO  WRK-Z72-HS
               MOVE    WRK-Z72-HS          TO  KANACHK-MAE-INPUT
           ELSE
               MOVE    WRK-Z72             TO  WRK-Z72-H9
               MOVE    WRK-Z72-H9           TO  KANACHK-MAE-INPUT
           END-IF
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:18)    TO  WRK-Z72-G
           END-IF
           .
       810-HENKAN-EXT.
           EXIT.
      *
      *--- 2008/10/22  ADD  START --------------------------------------
      *****************************************************************
      *    半角全角変換処理
      *****************************************************************
       820-HENKAN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           IF      WRK-Z72             <   ZERO
             MOVE    WRK-Z72             TO  WRK-Z72-HS2
             MOVE    WRK-Z72-HS2          TO  KANACHK-MAE-INPUT
           ELSE
             MOVE    WRK-Z72             TO  WRK-Z72-H2
             MOVE    WRK-Z72-H2           TO  KANACHK-MAE-INPUT
           END-IF
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
             MOVE    KANACHK-OUT-INPUT(1:14)    TO  WRK-Z72-G
           END-IF
           .
       820-HENKAN-EXT.
           EXIT.
      *
      *--- 2008/10/22  ADD  END --------------------------------------
      *****************************************************************
      *    未収金額編集処理
      *****************************************************************
      *** 3101-MISYU-HEN-SEC             SECTION.
      ****
      ***     MOVE    ZERO                TO  WRK-MISYUMONEY
      ***     MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
      ****    収納テーブル読み込み
      ***     INITIALIZE                  SYUNOU-REC
      ***     MOVE    SPA-HOSPID          TO  SYU-HOSPID
      ***     MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
      ***     MOVE    SPA-PTID            TO  SYU-PTID
      ****
      ***     MOVE    SYUNOU-REC          TO  MCPDATA-REC
      ***     MOVE    "DBSELECT"          TO  MCP-FUNC
      ***     MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      ***     IF      MCP-RC              =   ZERO
      ***         MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      ***         PERFORM 980-SYUNOU-READ-SEC
      ***     ELSE
      ***         INITIALIZE                  SYUNOU-REC
      ***         MOVE    1               TO  FLG-SYUNOU
      ***     END-IF
      ***     MOVE    ZERO                TO  WRK-MISYUMONEY
      ***     PERFORM
      ***         UNTIL     FLG-SYUNOU    =   1
      ***         IF      ORCHC03-FUKU-DENPNUM    =   ZERO
      ***             IF      SYU-DENPNUM     NOT =   WRK-DENPNUM
      ***                 COMPUTE WRK-MISYU       =   SYU-SKYMONEY
      ***                                         -   SYU-NYUKIN-TOTAL
      ***                 ADD     WRK-MISYU           TO  WRK-MISYUMONEY
      ***             END-IF
      ***         ELSE
      ***             IF      SYU-FUKU-DENPNUM    =   ORCHC03-FUKU-DENPNUM
      ***                 CONTINUE
      ***             ELSE
      ***                 COMPUTE WRK-MISYU       =   SYU-SKYMONEY
      ***                                         -   SYU-NYUKIN-TOTAL
      ***                 ADD     WRK-MISYU           TO  WRK-MISYUMONEY
      ***             END-IF
      ***         END-IF
      ****
      ***         MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      ***         PERFORM 980-SYUNOU-READ-SEC
      ***     END-PERFORM
      ****
      ***     MOVE    "DBCLOSE"           TO  MCP-FUNC
      ***     MOVE    "SYUNOU-KEY2"       TO  ORC-DBPATH
      ***     CALL    "ORCMCPSUB"          USING
      ***                                  MCPAREA
      ***                                  ORCMCPAREA
      ***                                  MCPDATA-REC
      ***     .
      *** 3101-MISYU-HEN-EXT.
      ***     EXIT.
      ****
      *****************************************************************
      *    自費名称編集処理  20040922  Ver 2.4.0移植  MOTO
      *****************************************************************
       3102-JIHIMSG-HEN-SEC             SECTION.
      *
           MOVE    WRK-JIHIMEI(1)      TO  HC03-JIHIMSG1
           MOVE    WRK-JIHIMEI(2)      TO  HC03-JIHIMSG2
           MOVE    WRK-JIHIMEI(3)      TO  HC03-JIHIMSG3
           MOVE    WRK-JIHIMEI(4)      TO  HC03-JIHIMSG4
           MOVE    WRK-JIHIMEI(5)      TO  HC03-JIHIMSG5
      ****2005/11/14 @@@@-BRAND  START
           MOVE    WRK-JIHIMEI(6)      TO  HC03-JIHIMSG6
           MOVE    WRK-JIHIMEI(7)      TO  HC03-JIHIMSG7
           MOVE    WRK-JIHIMEI(8)      TO  HC03-JIHIMSG8
           MOVE    WRK-JIHIMEI(9)      TO  HC03-JIHIMSG9
           MOVE    WRK-JIHIMEI(10)     TO  HC03-JIHIMSG10
      ****2005/11/14 @@@@-BRAND  END
      *
           .
       3102-JIHIMSG-HEN-EXT.
           EXIT.
      *---(03.02.00) LINE ADD START -----------------------------------
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       3101-SRYYMD-HEN-SEC        SECTION.
      *
           MOVE    SPACE                   TO  WRK-SRYYMDG
                                               WRK-LBLSRYYMD
      *
           IF  ( ORCHC03-HAKKOFLG  =  4 )
               MOVE    "診療年月"          TO  WRK-LBLSRYYMD
               MOVE    SYU-SRYYMD          TO  WRK-SYMD
               PERFORM  31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG (1:16)  TO  WRK-SRYYMDG
           ELSE
               IF  ( ORCHC03-PARAFLG  =  1 )
               AND ( SYUPARA03-STSRYYMD  NOT =  SYUPARA03-EDSRYYMD )
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   PERFORM  31012-SRYYMD-HEN-SEC
               ELSE
                   MOVE    "診療日"        TO  WRK-LBLSRYYMD
                   MOVE    SYU-SRYYMD      TO  WRK-SYMD
                   PERFORM  31012-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG     TO  WRK-SRYYMDG
               END-IF
           END-IF
      *
           .
       3101-SRYYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       31012-SRYYMD-HEN-SEC        SECTION.
      *
           MOVE    SPACE                   TO  WRK-SRYYMDG
      *
           MOVE    SYUPARA03-STSRYYMD      TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG             TO  WRK-STSRYYMDG
      *
           MOVE    SYUPARA03-EDSRYYMD      TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG             TO  WRK-EDSRYYMDG
      *
           STRING  WRK-STSRYYMDG           DELIMITED  BY  SIZE
                   " 〜 "                  DELIMITED  BY  SIZE
                   WRK-EDSRYYMDG           DELIMITED  BY  SIZE
                   INTO                    WRK-SRYYMDG
           END-STRING
      *
           .
       31012-SRYYMD-HEN-EXT.
           EXIT.
      *---(03.02.00) LINE ADD END   -----------------------------------
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
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-START---
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    SPA-SRYYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1900-EDYUKYMD
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(10) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    SYS-1900-PRTKBN(10)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           MOVE    ALL "　"            TO  WRK-ZENKAKU-G
      *
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL       IDK     >   WRK-MOJISU
               IF      WRK-MOJI (IDK:1)    NUMERIC
                   MOVE    WRK-MOJI(IDK:1)     TO  IDX2-1
                   IF      IDX2-1              =   ZERO
                       MOVE    10                  TO  IDX2
                   ELSE
                       MOVE    IDX2-1              TO  IDX2
                   END-IF
                   MOVE    TBL-SUJI (IDX2)     TO  WRK-ZENKAKU(IDK)
               ELSE
                   IF      WRK-MOJI (IDK:1)    =   "."
                       MOVE    "．"            TO  WRK-ZENKAKU(IDK)
                   END-IF
               END-IF
           END-PERFORM
           .
       2009-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
            CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
          IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     MOVE    PATH-TBL-PTINF-KEY     TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
            MOVE    "tbl_ptinf"        TO  MCP-TABLE
            MOVE    "key"              TO  MCP-PATHNAME
            CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                         MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                            SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *         MOVE    PATH-TBL-PTINF-KEY     TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **        CALL    "ORCMCPSUB"          USING
      *         CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
               MOVE    "tbl_ptinf"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                            MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                     ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                            MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                            SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      ****2006/07/29 @@@@-BRAND  START
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      ****2006/07/29 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "PTINF-KEY"         TO  ORC-DBPATH
      *     MOVE    PATH-TBL-PTINF-KEY     TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "tbl_ptinf"        TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                            SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *---(03.02.00) LINE ADD START -----------------------------------
      *****************************************************************
      *    パラメタテーブル検索
      *****************************************************************
       900-PARA-KEY5-SEL-SEC         SECTION.
      *
           MOVE    ZERO                   TO  FLG-PARA
      *
           INITIALIZE                         PARA-REC
      *****07/09/21  4.0対応  Satoh  START*****
           MOVE    SPA-HOSPNUM            TO  PARA-HOSPNUM
      *****07/09/21  4.0対応  Satoh  END*******
           MOVE    ORCHC03-PARA-GYOUMUID  TO  PARA-GYOUMUID
           MOVE    ORCHC03-PARA-TERMID    TO  PARA-TERMID
           MOVE    ORCHC03-PARA-FILEMEI   TO  PARA-FILEMEI
           MOVE    PARA-REC               TO  MCPDATA-REC
           MOVE    "tbl_para"             TO  MCP-TABLE
           MOVE    "key5"                 TO  MCP-PATHNAME
           PERFORM  910-DBSELECT-SEC
           IF  ( MCP-RC  =  ZERO )
               PERFORM  900-PARA-KEY5-FET-SEC
           ELSE
               MOVE  1                    TO  FLG-PARA
               INITIALIZE                     PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタテーブルFETCH
      *****************************************************************
       900-PARA-KEY5-FET-SEC         SECTION.
      *
           MOVE    ZERO                   TO  FLG-PARA
      *
           MOVE    "tbl_para"             TO  MCP-TABLE
           MOVE    "key5"                 TO  MCP-PATHNAME
           PERFORM  920-DBFETCH-SEC
           IF  ( MCP-RC  =  ZERO )
               MOVE  MCPDATA-REC          TO  PARA-REC
           ELSE
               MOVE  1                    TO  FLG-PARA
               INITIALIZE                     PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *
      *---(03.02.00) LINE ADD END   -----------------------------------
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
           CALL    "ORCDBMAIN"          USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                        SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *=== 公務災害対応 by k.takashima (2008/03/05) start
      *****************************************************************
      *    患者保険情報読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC    TO    PTHKNINF-REC
                   MOVE    ZERO           TO    FLG-PTHKNINF
               ELSE
                   MOVE    1              TO    FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1              TO    FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *=== 公務災害対応 by k.takashima (2008/03/05) end
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                            SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *         MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **        CALL    "ORCMCPSUB"          USING
      *         CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*****
                                            MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                     ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                            MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                            SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
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
      ****2006/07/29 @@@@-BRAND  START
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      ****2006/07/29 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "HKNCOMBI-KEY"      TO  ORC-DBPATH
      *     MOVE    PATH-TBL-HKNCOMBI-KEY    TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                            SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       910-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
      *     MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
           MOVE    "tbl_ptrsiinf"        TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                            SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **        MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
      *         MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **        CALL    "ORCMCPSUB"          USING
      *         CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
               MOVE    "tbl_ptrsiinf"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*******
                                            MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                     ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                            MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                            SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   MOVE    ZERO            TO  FLG-PTRSIINF
               ELSE
                   MOVE    1               TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
      ****2006/07/29 @@@@-BRAND  START
      *     MOVE    "DBCLOSE"           TO  MCP-FUNC
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      ****2006/07/29 @@@@-BRAND  END
      *****07/09/21  4.0対応  Satoh  START*****
      ****2005/10/17 @@@@-BRAND  START
      **    MOVE    "PTRSIINF-KEY"      TO  ORC-DBPATH
      *     MOVE    PATH-TBL-PTRSIINF-KEY    TO  MCP-PATH
      ****2005/10/17 @@@@-BRAND  END
      ****2005/10/17 @@@@-BRAND  START
      **    CALL    "ORCMCPSUB"          USING
      *     CALL     "MCPSUB"          USING
      ****2005/10/17 @@@@-BRAND  END
      *=== 労災情報読込み不具合修正 by Yoshikawa(2008/04/03) start
      *     MOVE    "tbl_presiinf"        TO  MCP-TABLE
           MOVE    "tbl_ptrsiinf"        TO  MCP-TABLE
      *=== 労災情報読込み不具合修正 by Yoshikawa(2008/04/03) end
           MOVE    "key"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
      *****07/09/21  4.0対応  Satoh  END*****
                                        MCPAREA
      ****2005/10/17 @@@@-BRAND  START
      *                                 ORCMCPAREA
      ****2005/10/17 @@@@-BRAND  END
                                        MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                            SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           .
       910-PTRSIINF-READ-EXT.
           EXIT.
      *---(03.02.00) LINE ADD START -----------------------------------
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      *     CALL    "MONFUNC"           USING
           CALL    "ORCDBMAIN"          USING
      *****07/09/21  4.0対応  Satoh  END*******
                                       MCPAREA
                                       MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      *     CALL    "MONFUNC"           USING
           CALL    "ORCDBMAIN"          USING
      *****07/09/21  4.0対応  Satoh  END*******
                                       MCPAREA
                                       MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *****07/09/21  4.0対応  Satoh  START*****
      *     CALL    "MONFUNC"           USING
           CALL    "ORCDBMAIN"          USING
      *****07/09/21  4.0対応  Satoh  END*******
                                       MCPAREA
                                       MCPDATA-REC
      *****07/09/21  4.0対応  Satoh  START*****
                                       SPA-AREA
      *****07/09/21  4.0対応  Satoh  END*******
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *---(03.02.00) LINE ADD END   -----------------------------------
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.

      *

