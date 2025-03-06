#!/bin/bash
# extract ICV's of all subj
# Daria J Aug 2019


# ------------SET----------------
ana=~scripts/;


unset POSISLY_CORRECT



echo "subj_id,IntraCranialVol" > FS6_all_subj_icv.csv

for subj_id in `cat ${ana}all_subj.txt` ; do

	echo -n "${subj_id}," >> FS6_all_subj_icv.csv;

	echo -n "`cat ${SUBJECTS_DIR}/${subj_id}/stats/aseg.stats | grep IntraCranialVol | awk -F, '{ print $4 }'| sed 's/\ //g'`," >> FS6_all_subj_icv.csv;

done

# I had to reformat the csv afterwards.