#!/usr/bin/bash
src_file=$1
tgt_file=$2
tmpfile="/tmp/tempfile_$$.tmp"
if [ ! -f ${src_file} ]; then
	echo "Source file ${src_file} absent."
	exit 1
fi
src_basename=$(basename $1) 
tgt_basename=$(basename $2) 
src_product=$(echo $src_basename | cut -d '-' -f 2 | cut -d '.' -f 1)
tgt_product=$(echo $tgt_basename | cut -d '-' -f 2 | cut -d '.' -f 1)

cat $src_file | sed -e "s/$src_product\-/$tgt_product\-/g" | \
	        sed -e "s/${src_product}\_/${tgt_product}\_/g" > ${tmpfile}

cp -f ${tmpfile} ${tgt_file}
