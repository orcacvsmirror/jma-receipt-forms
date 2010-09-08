#!/bin/dash

. /etc/jma-receipt/jma-receipt.env

usrname=`whoami`

# Etchで使用するコードUTF-8への変換
# echo message (default character-code EUC)
function echomsg() {
  if [ "${LANG}" = "ja_JP.UTF-8" ] || [ "${LANG}" = "ja_JP.UTF8" ] ||
     [ "${LANG}" = "ja_JP.utf8" ]; then
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

# check user
if [ $usrname != "root" ] ; then
  echomsg "rootユーザで実行してください"
  exit 1
fi

# start message
echomsg "公開帳票 プログラムコピー中..."

# copy
cp -af *.CBL ${SITESRCDIR}/cobol 2> /dev/null
cp -af *.INC ${SITESRCDIR}/cobol/copy 2> /dev/null
cp -af *.red ${SITESRCDIR}/form 2> /dev/null
cp -af version_* ${SITESRCDIR}/doc

# run site-upgrade.sh
if [ -f ${SCRIPTSDIR}/allways/site-upgrade.sh ] ; then
  sh ${SCRIPTSDIR}/allways/site-upgrade.sh
else
  echomsg "公開帳票 プログラムコピー異常終了!!"
  exit 1
fi

# end message
sync
echomsg "公開帳票 プログラムコピー終了!!"
exit 0
