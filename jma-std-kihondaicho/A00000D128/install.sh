#!/bin/dash

. /etc/jma-receipt/jma-receipt.env

usrname=`whoami`

# Etch�ǻ��Ѥ��륳����UTF-8�ؤ��Ѵ�
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
  echomsg "root�桼���Ǽ¹Ԥ��Ƥ�������"
  exit 1
fi

# start message
echomsg "����Ģɼ ������Ģ��������� �ץ���ॳ�ԡ���..."

# copy
cp -af *.CBL ${SITESRCDIR}/cobol 2> /dev/null
cp -af *.INC ${SITESRCDIR}/cobol/copy 2> /dev/null
cp -af *.red ${SITESRCDIR}/form 2> /dev/null
cp -af version_* ${SITESRCDIR}/doc

# run site-upgrade.sh
if [ -f ${SCRIPTSDIR}/allways/site-upgrade.sh ] ; then
  bash ${SCRIPTSDIR}/allways/site-upgrade.sh
else
  echomsg "����Ģɼ ������Ģ��������� �ץ���ॳ�ԡ��۾ｪλ!!"
  exit 1
fi

# end message
sync
echomsg "����Ģɼ ������Ģ��������� �ץ���ॳ�ԡ���λ!!"
exit 0
