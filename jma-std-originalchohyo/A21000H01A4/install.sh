#!/bin/sh
#########################################################################
#
# 帳票インストールツール
# (古い帳票の再インストール・コピーするの面倒だなあツール)
#
# update
# 	2003.09.27
# 		新規作成 (K.Togazono)
# 	2003.10.07
#		CBLファイルをコピーしないバグを修正。
# 		/usr/lib/jma-receipt/site-libからの
# 		ファイルコピーをコメントアウト。(T.Onuki)
#
#########################################################################


#----------------
# 定数 
#----------------

DIRTYOHYO=$1

DIRCBL=cobol
DIRCPY=${DIRCBL}/copy
DIRFORM=form
DIRREC=record

SRCBASE=/usr/local/site-jma-receipt
INSBASE=/usr/lib/jma-receipt/site-lib
INSCOPY=/usr/lib/jma-receipt/

PRGNAME=

FILESO=
FILECBL=
FILEINC=
#(↓.redも含む)
FILERED=
#(↓.redの場合は空)
FILEDEF=


#----------------
# 処理
#----------------

# Etchで使用するコードUTF-8への変換
# echo message (default character-code EUC)
function echomsg() {
  if [ "${LANG}" = "ja_JP.UTF-8" ] || [ "${LANG}" = "ja_JP.UTF8" ]; then
    if [ -z $2 ]; then
      echo `echo $1 | nkf -wE`
    else
      echo -n `echo $1 | nkf -wE`
    fi 
  else
    if [ -z $2 ]; then
      echo $1
    else
      echo -n $1
    fi
  fi
  return 0
}

# rootユーザでないとだめ
if test `whoami` != "root"
then
	echomsg "rootユーザで実行してください"
	exit
fi

# 帳票プログラムはどこ？
if test "$DIRTYOHYO" = ""
then
#	echo "第１引数に帳票プログラムの絶対パスを指定してください"
#	exit
        DIRTYOHYO=./
fi

if ! test -d $DIRTYOHYO
then
	echomsg "帳票プログラムが見付かりません。パスが間違ってませんか？"
	exit
fi
# (パスの最後に"/"をくっつけてたら削除する)
DIRTYOHYO=`echo "$DIRTYOHYO" | sed -e "s/\/$//"`

# "*.CBL"はあるか？ 
FILECBL=`find $DIRTYOHYO -name "*.CBL" -print 2>/dev/null`
if test "$FILECBL" = ""
then
	echomsg "\"*.CBL\"ファイルが見付かりません"
	exit
fi
FILECBL=`echo $FILECBL | sed -e "s/.*\///"`

# プログラム名を拾ってくる
PRGNAME=`echo $FILECBL | sed -e "s/\.CBL//"`

# "*.INC"はあるか？ 
FILEINC=`find $DIRTYOHYO -name "*.INC" -print 2>/dev/null`
if test "$FILEINC" = ""
then
	echo "\"*.INC\"ファイルが見付かりません"
	exit
fi
FILEINC=`echo $FILEINC | sed -e "s/.*\///"`

# "*.dia"または"*.red"はあるか？ 
FILERED=`find $DIRTYOHYO -name "*.red" -print 2>/dev/null`
if test "$FILERED" = ""
then
	FILERED=`find $DIRTYOHYO -name "*.dia" -print 2>/dev/null`
	if test "$FILERED" = ""
	then
		echo "\"*.dia\"または\"*.red\"ファイルが見付かりません"
		exit
	fi
	# "*.dia"は、"*.def"ファイルも持っている
	FILEDEF=`find $DIRTYOHYO -name "*.def" -print 2>/dev/null`
	FILEDEF=`echo $FILEDEF | sed -e "s/.*\///"`
fi
FILERED=`echo $FILERED | sed -e "s/.*\///"`


# ファイルの更新時刻を確認する
# (あれ？コピーしてる？まあいいか。)
if ! find ${SRCBASE}/${DIRCBL}/${FILECBL} 2>/dev/null -o \
	find ${DIRTYOHYO}/${FILECBL} -newer ${SRCBASE}/${DIRCBL}/${FILECBL} 2>/dev/null
then
	echo "cp ${DIRTYOHYO}/${FILECBL} ${SRCBASE}/${DIRCBL}"
	if ! cp ${DIRTYOHYO}/${FILECBL} ${SRCBASE}/${DIRCBL}
        then
             exit
        fi
fi

if ! find ${SRCBASE}/${DIRCPY}/${FILEINC} 2>/dev/null -o \
	find ${DIRTYOHYO}/${FILEINC} -newer ${SRCBASE}/${DIRCPY}/${FILEINC} 2>/dev/null
then
	echo "cp ${DIRTYOHYO}/${FILEINC} ${SRCBASE}/${DIRCPY}"
	if ! cp ${DIRTYOHYO}/*.INC ${SRCBASE}/${DIRCPY}
        then
             exit
        fi
fi

if ! find ${SRCBASE}/${DIRFORM}/${FILERED} 2>/dev/null -o \
	find ${DIRTYOHYO}/${FILERED} -newer ${SRCBASE}/${DIRFORM}/${FILERED} 2>/dev/null
then
	echo "cp ${DIRTYOHYO}/${FILERED} ${SRCBASE}/${DIRFORM}"
	if ! cp ${DIRTYOHYO}/*.red ${SRCBASE}/${DIRFORM}
        then
             exit
        fi
fi

if test "$FILEDEF" != ""
then
	if ! find ${SRCBASE}/${DIRREC}/${FILEDEF} 2>/dev/null -o \
		find ${DIRTYOHYO}/${FILEDEF} -newer ${SRCBASE}/${DIRREC}/${FILEDEF} 2>/dev/null
	then
		echo "cp ${DIRTYOHYO}/${FILEDEF} ${SRCBASE}/${DIRREC}"
		if ! cp ${DIRTYOHYO}/*.def ${SRCBASE}/${DIRREC}
                then
                     exit
                fi
	fi
fi


# 古いファイルを削除する
rm -f ${INSCOPY}/${PRGNAME}.so
# (大文字ってことはないけど、ゼロとは限らない、、、)
rm -f ${INSCOPY}/${PRGNAME}.SO
# (古いバージョンは、*.redじゃなくて、*.diaが入っている可能性がある？)
# (↓ *.red/*.diaの拡張子を取り払ってるだけ)
OLDREDNAME=`echo $FILERED | sed -e "s/\..*" 2>/dev/null`
rm -f ${INSCOPY}/${DIRFORM}/${OLDREDNAME}.dia
rm -f ${INSCOPY}/${DIRREC}/${OLDREDNAME}.def


# コンパイルを実行する
sh /usr/lib/jma-receipt/scripts/allways/site-upgrade.sh
if test $? -ne 0
then
	echomsg "コンパイルに失敗しました。シェルを再実行するか、手動で設定してください。"
	exit
fi


# コピーしなくても良いものもあるので。
#echo "最近のプログラムは、${INSCOPY}に *.so等をコピーしなくても良いものがあります。"
#echo "詳しくは、各帳票プログラムの READMEを参照して下さい。"
#echo -n "コンパイルしたファイルを ${INSCOPY}にコピーしますか？[Y/n]: "
#read ANS
#if test "$ANS" = "n" -o "$ANS" = "N"
#then
#	echo "*.so ファイル等をコピーせずに終了します。"	
#	exit
#fi


# ファイルをコピーする
#cp ${INSBASE}/${PRGNAME}.so ${INSCOPY}
#cp ${INSBASE}/${DIRFORM}/${FILERED} ${INSCOPY}/${DIRFORM}

#if test "$FILEDEF" != ""
#then
#	cp ${INSBASE}/${DIRREC}/${FILEDEF} ${INSCOPY}/${DIRREC}
#fi


echomsg "帳票プログラムのインストールを終了します"
exit
