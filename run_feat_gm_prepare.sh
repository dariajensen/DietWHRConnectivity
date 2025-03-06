#!/bin/bash
# run feat_gm_prepare on all WHII participants for the WHR cohort and the diet cohort seperately
#
# Daria Jensen Aug 2021
# ----------------------------

path=/home/fs0/djensen/; #server path
dir=${path}scratch/project_WH/data/; #project dir

cd $dir;

fOutL=1st_level_L_Hippo_FS6_smooth.feat
fOutR=1st_level_R_Hippo_FS6_smooth.feat

subjno=664; # change here to 512 sampel for diet data.

fsl_sub -q bigmem.q feat_gm_prepare 4D_GM_FS6_Hippo_smooth_R_withoutIF_n${subjno}_test.nii.gz WH_0XX/${fOutR} WH_0XY/${fOutR} .. #bild for data security ;

fsl_sub -q bigmem.q feat_gm_prepare 4D_GM_FS6_Hippo_smooth_L_withoutIF_n${subjno}.nii.gz WH_0XX/${fOutL} WH_0XY/${fOutL} .. #bild for data security ;