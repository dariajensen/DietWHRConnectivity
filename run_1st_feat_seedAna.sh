#!/bin/bash
#
# This script copies and adapts a specified feat design-fsf file (paths) from a refsubject for the other subjects. Then it runs 1st level feat analysis for each subject.
#
# Daria Jensen 27 Feb 2019
# -------------------------------------------- #

# ------------SET DIRECTORIES ---------------- #
dir=~/data/; #project dir
ana=~/scripts/; #project dir
wh_dir=~/whitehall/; # org data stored here

# Reference subject where fsf file was created:
refdesign=${dir}feat_runs/feat_1stLevel_ref_L_Hippo.fsf;
refsub=WH_017; # path in ref design: /home/fs0/djensen/scratch/project_WH/data/WH_017


# ------------ START SCRIPT ------------------ #
cd ${ana}

# set files for copying
data=filtered_func_data_clean.nii.gz;
data_sm=filtered_func_data_clean_smooth23.nii.gz;

mkdir log #create log file


# clean up feats before running
rm ${dir}WH*/feat.*

n=512; # change for WHII sample to: 664


for s in `cat n${n}_list.txt` # participants list
do # do this step with groups of subjects and not all at ones!

	no=${s:3:4};#uses only the number
	echo "subject: " $no
	
	cd ${dir}${s};
	FILE=1st_level_L_Hippo_FS6_smooth.feat/report.html
	if test -f "$FILE"; then
        echo "$FILE exists.";
    else
    
	sub_wh_dir=${wh_dir}RESTING/${s}/Resting_MB6_Feat_Nbrain.ica/;
	ln -s ${sub_wh_dir}${s}_${data_sm} ${dir}${s}/${data_sm}

	#rm -r 1st_level_*_Hippo_FS6_smooth.feat; # rm old one if necessary
	
	for hemi in R L; do

		# copy fsf-file from reference and replace subject ID:
		# use smooth version atm
		# added 'FS6' to the .fsf file
		sed -e "s/1st_level_L_Hippo/1st_level_L_Hippo_smooth/g" -e "s/${refsub}/${s}/g" -e "s/L_Hippo/${hemi}_Hippo_FS6/g"  -e "s/_clean/_clean_smooth23/g"  -e "s/.txt/_smooth.txt/g"  ${refdesign}>${dir}${s}/${s}_1st_level_${hemi}_Hippo_FS6_smooth.fsf;
	
		# then run feat
		fsl_sub -q bigmem.q feat ${s}_1st_level_${hemi}_Hippo_FS6_smooth.fsf;

	done
	fi
done
echo "done run_1st_feat_seedAna.sh "

#go back to ./hippoDietWHR_seedana.sh to run group analysis

