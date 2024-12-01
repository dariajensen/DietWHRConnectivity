#!/bin/bash
#
# DTI for diet and metabolism datasets
#
# Daria Jensen Sep 2022
#
# ----------------------------

# ------------SET----------------
path=/home/fs0/djensen/; #server path
#path=/Users/dariajensen/; #local parth
dir=${path}scratch/project_WH/data/; #project dir
ana=${path}scratch/project_WH/scripts/; #project dir
wh_dir=/vols/Data/whitehall/;
FS6dir=${wh_dir}STRUCTURAL/FREESURFER6/;
behana=/Users/dariajensen/Dropbox/DPhil_backup/2_WHII/beh/scripts;
DTIdir=/vols/Data/whitehall/DTI/DTI_CORRECTED_LATEST/;
sana_dir='/vols/Data/whitehall/PAPERS/SANA_PAPERS/SVD_Paper';


# --------------------------------------------
# RUN TBSS
# --------------------------------------------

cd ~/data;
mkdir tbss
mkdir tbss_diet

cd tbss #tbss_diet

#copy all FA images into that folder.
subjno=664;#512;
# check if all 1st level report.html exist:
for s in `cat ${ana}n${subjno}_list_seedAnaSub_ica.txt`
do
	cp ${DTIdir}${s}/dti_1RUN_FA.nii.gz ${s}_FA.nii.gz 
done


#dti_1RUN_MD.nii.gz  
#dti_1RUN_L1.nii.gz 
#dti_1RUN_RD.nii.gz 

tbss_1_preproc *.nii.gz

tbss_2_reg -T

fsl_sub -q bigmem.q  tbss_3_postreg -S

#cd stats
#fslview all_FA -b 0,0.8 mean_FA_skeleton -b 0.2,0.8 -l Green

fsl_sub -q bigmem.q tbss_4_prestats 0.2

mkdir L1
mkdir RD
mkdir MD

subjno=657;#506;

cd ~/scratch/project_WH/data/tbss
cd ~/scratch/project_WH/data/tbss_diet
for s in `cat ${ana}n${subjno}_dti_subjects.txt`
do
	cp ${DTIdir}${s}/dti_1RUN_L1.nii.gz L1/${s}_FA.nii.gz
	cp ${DTIdir}${s}/dti_1RUN_MD.nii.gz MD/${s}_FA.nii.gz 
	cp ${DTIdir}${s}/dti_1RUN_RD.nii.gz RD/${s}_FA.nii.gz 
done
fsl_sub -q bigmem.q tbss_non_FA L1
fsl_sub -q bigmem.q tbss_non_FA RD
fsl_sub -q bigmem.q tbss_non_FA MD


### extract global parameter estimates:
for p in  FA MD RD L1 ; do
    fslmeants -i all_${p}_skeletonised.nii.gz -m mean_FA_skeleton_mask >> global_${p}.txt;
done


# --------------------------------------------
### RANDOMISE
# --------------------------------------------

### DIET
DIR=XX/data/tbss_diet; #_diet
MASK=${DIR}/stats/mean_FA_skeleton_mask.nii.gz;

### DIET slope and WHR inter with all confounders
add='ahei512slope';
MAT=${DIR}/${add}_covs_R1.mat
CON=${DIR}/${add}_covs_R1.con
for i in FA MD L1 RD
do
echo ${i}
INP=${DIR}/stats/all_${i}_skeletonised.nii.gz
OUT=${DIR}/stats/${i}/${i}_${add}_covs_R1
fsl_sub -q bigmem.q randomise -i ${INP} -o ${OUT} -m ${MASK} -d ${MAT} -t ${CON} -n 5000 --T2
done

add='ahei512inter';
MAT=${DIR}/${add}_covs_R1.mat
CON=${DIR}/${add}_covs_R1.con
for i in FA MD L1 RD
do
echo ${i}
INP=${DIR}/stats/all_${i}_skeletonised.nii.gz
OUT=${DIR}/stats/${i}/${i}_${add}_covs_R1
fsl_sub -q bigmem.q randomise -i ${INP} -o ${OUT} -m ${MASK} -d ${MAT} -t ${CON} -n 5000 --T2
done

add='ahei512avg';
MAT=${DIR}/${add}_covs_R1.mat
CON=${DIR}/${add}_covs_R1.con
for i in FA MD L1 RD
do
echo ${i}
INP=${DIR}/stats/all_${i}_skeletonised.nii.gz
OUT=${DIR}/stats/${i}/${i}_${add}_covs_R1
fsl_sub -q bigmem.q randomise -i ${INP} -o ${OUT} -m ${MASK} -d ${MAT} -t ${CON} -n 5000 --T2
done


add='ahei512cum';
MAT=${DIR}/${add}_covs_R1.mat
CON=${DIR}/${add}_covs_R1.con
for i in FA MD L1 RD
do
echo ${i}
INP=${DIR}/stats/all_${i}_skeletonised.nii.gz
OUT=${DIR}/stats/${i}/${i}_${add}_covs_R1
fsl_sub -q bigmem.q randomise -i ${INP} -o ${OUT} -m ${MASK} -d ${MAT} -t ${CON} -n 5000 --T2
done


# WHR R1 is --> cubic spline LME model
DIR=/home/fs0/djensen/scratch/project_WH/data/tbss;
MASK=${DIR}/stats/mean_FA_skeleton_mask.nii.gz;

add='WHR657inter'; #
MAT=${DIR}/${add}_covs_R1.mat;
CON=${DIR}/${add}_covs_R1.con;
for i in FA L1 RD MD
do
echo ${i}
INP=${DIR}/stats/all_${i}_skeletonised.nii.gz;
OUT=${DIR}/stats/${i}/${i}_${add}_covs_R1;
fsl_sub -q bigmem.q randomise -i ${INP} -o ${OUT} -m ${MASK} -d ${MAT} -t ${CON} -n 5000 --T2
done

add='WHR657slope'; #
MAT=${DIR}/${add}_covs_R1.mat;
CON=${DIR}/${add}_covs_R1.con;
for i in FA L1 RD MD
do
echo ${i}
INP=${DIR}/stats/all_${i}_skeletonised.nii.gz;
OUT=${DIR}/stats/${i}/${i}_${add}_covs_R1;
fsl_sub -q bigmem.q randomise -i ${INP} -o ${OUT} -m ${MASK} -d ${MAT} -t ${CON} -n 5000 --T2
done




# --------------------------------------------
### check results:
# --------------------------------------------
# check skeleton:
#fsleyes all_FA -b 0,0.8 mean_FA_skeleton -b 0.2,0.8 -l Green

cd ~/scratch/project_WH/data/tbss_diet/stats
t=ahei512slope_covs_R1
t=ahei512inter_covs_R1
t=ahei512avg_covs_R1
t=ahei512cum_covs_R1

cd ~/scratch/project_WH/data/tbss/stats
t=WHR657inter_covs_R1
t=WHR657slope_covs_R1


fsleyes $FSLDIR/data/standard/MNI152_T1_1mm_brain mean_FA_skeleton -cm Green -dr 0.2 0.7 FA/FA_${t}_tfce_corrp_tstat1.nii.gz -cm  Red-Yellow -dr 0.95 1 FA/FA_${t}_tfce_corrp_tstat2.nii.gz -cm  blue-lightblue -dr 0.95 1 MD/MD_${t}_tfce_corrp_tstat1.nii.gz -cm  Red-Yellow -dr 0.95 1 MD/MD_${t}_tfce_corrp_tstat2.nii.gz -cm  blue-lightblue -dr 0.95 1 RD/RD_${t}_tfce_corrp_tstat1.nii.gz -cm  Red-Yellow -dr 0.95 1 RD/RD_${t}_tfce_corrp_tstat2.nii.gz -cm  blue-lightblue -dr 0.95 1 L1/L1_${t}_tfce_corrp_tstat1.nii.gz -cm  Red-Yellow -dr 0.95 1 L1/L1_${t}_tfce_corrp_tstat2.nii.gz -cm  blue-lightblue -dr 0.95 1 &



# --------------------------------------------
### tbss_fill for those with results 
# --------------------------------------------
# ahei slope
cd XX/data/tbss_diet/stats
t=ahei512slope_covs_R1;


tbss_fill FA/FA_${t}_tfce_corrp_tstat1.nii.gz 0.95 mean_FA tbss_fill_FA_${t}_tfce_corrp_tstat1;
tbss_fill L1/L1_${t}_tfce_corrp_tstat2.nii.gz 0.95 mean_FA tbss_fill_L1_${t}_tfce_corrp_tstat2;
tbss_fill MD/MD_${t}_tfce_corrp_tstat2.nii.gz 0.95 mean_FA tbss_fill_MD_${t}_tfce_corrp_tstat2;

# view tbss_filled results
fsleyes $FSLDIR/data/standard/MNI152_T1_1mm_brain mean_FA -dr 0 0.6 mean_FA_skeleton -cm Green -dr 0.2 0.7 tbss_fill_FA_${t}_tfce_corrp_tstat1 -cm Red-Yellow tbss_fill_L1_${t}_tfce_corrp_tstat2 -cm blue-lightblue tbss_fill_MD_${t}_tfce_corrp_tstat2 -cm blue-lightblue &


# WHR intercept
cd XX/data/tbss/stats:
t=WHR657inter_covs_R1;


tbss_fill FA/FA_${t}_tfce_corrp_tstat2.nii.gz 0.95 mean_FA tbss_fill_FA_${t}_tfce_corrp_tstat2;
tbss_fill RD/RD_${t}_tfce_corrp_tstat1.nii.gz 0.95 mean_FA tbss_fill_RD_${t}_tfce_corrp_tstat1;
tbss_fill MD/MD_${t}_tfce_corrp_tstat1.nii.gz 0.95 mean_FA tbss_fill_MD_${t}_tfce_corrp_tstat1;
tbss_fill MD/MD_${t}_tfce_corrp_tstat2.nii.gz 0.95 mean_FA tbss_fill_MD_${t}_tfce_corrp_tstat2;

# view tbss_filled results
fsleyes $FSLDIR/data/standard/MNI152_T1_1mm_brain mean_FA -dr 0 0.6 mean_FA_skeleton -cm Green -dr 0.2 0.7 tbss_fill_FA_${t}_tfce_corrp_tstat2 -cm blue-lightblue tbss_fill_RD_${t}_tfce_corrp_tstat1 -cm Red-Yellow tbss_fill_MD_${t}_tfce_corrp_tstat1 -cm Red-Yellow  tbss_fill_MD_${t}_tfce_corrp_tstat2 -cm blue-lightblue &



 # --------------------------------------------
### extract significant clusters:
# --------------------------------------------
# for ahei
cd ~/scratch/project_WH/data/tbss_diet/stats
t=ahei512slope_covs_R1;

cd FA # MD L1 RD
input=FA_${t}_tfce_corrp_tstat1;

cd ../L1
input=L1_${t}_tfce_corrp_tstat2;

cd ../MD
input=MD_${t}_tfce_corrp_tstat2;


# for whr
cd XX/data/tbss/stats
t=WHR657inter_covs_R1; #t=WHR657inter2; 
cd FA # MD L1 RD
input=FA_${t}_tfce_corrp_tstat2;

cd ../MD
input=MD_${t}_tfce_corrp_tstat2;

input=MD_${t}_tfce_corrp_tstat1;

cd ../RD
input=RD_${t}_tfce_corrp_tstat1;


# masking this with the significant voxels from cope
fslmaths ${input} -thr 0.95 -bin ${input}_thrbin;
fslmaths ${input}_thrbin -bin ${input}_thrbin;
# run cluster to extract the clusters and local maxima in several different outputs:
cluster --in=${input} --thresh=0.95 --oindex=cluster_${input}_thrbin --olmax=lmax_${input}_thrbin.txt --mm   > cluster_${input}_thrbin.txt;
cat cluster_${input}_thrbin.txt;#output



fslstats cluster_${input}_thrbin -V 



# --------------------------------------------
# Identify MAX Coordinates: ---- 
# --------------------------------------------

# check atlas to use
#atlasquery --dumpatlases

# Read each line from the coordinates file
while read -r x y z; do
    # Query the atlas for the given coordinates
    #result=$(atlasq ohi -a "JHU White-Matter Tractography Atlas" -c "$x","$y","$z")
    result=$(atlasq ohi -a "XTRACT HCP Probabilistic Tract Atlases" -c "$x","$y","$z")
    
    # Print the result
    echo "Coordinates ($x, $y, $z): $result"
done < coordinates.txt


# example:
#atlasq ohi -a "JHU White-Matter Tractography Atlas" -c 23,-88,0 -V



# Note: potential use not whole cluster, but individual ones.

# --------------------------------------------
# extract cluster parameter estimates: ---- 
# --------------------------------------------


cd /home/fs0/djensen/scratch/project_WH/data/tbss_diet/stats/
t=ahei512slope_covs_R1;


input=MD_${t}_tfce_corrp_tstat2;
fslmaths MD/cluster_${input}_thrbin -bin MD/cluster_${input}_thrbin_bin;
fslmeants -i all_MD_skeletonised.nii.gz -m MD/cluster_${input}_thrbin_bin >> all_cluster_${input}_thrbin_bin.txt;
input=L1_${t}_tfce_corrp_tstat2;
fslmaths L1/cluster_${input}_thrbin -bin L1/cluster_${input}_thrbin_bin;
fslmeants -i all_L1_skeletonised.nii.gz -m L1/cluster_${input}_thrbin_bin >> all_cluster_${input}_thrbin_bin.txt;
input=FA_${t}_tfce_corrp_tstat1;
fslmaths FA/cluster_${input}_thrbin -bin FA/cluster_${input}_thrbin_bin;
fslmeants -i all_FA_skeletonised.nii.gz -m FA/cluster_${input}_thrbin_bin >> all_cluster_${input}_thrbin_bin.txt;

cd /home/fs0/djensen/scratch/project_WH/data/tbss/stats
rm */fslmeants.*

# also for whr for each FA, MD and RD
cd /home/fs0/djensen/scratch/project_WH/data/tbss/stats/
t=WHR657inter_covs_R1;
#t=WHR657inter2; #covs2

input=FA_${t}_tfce_corrp_tstat2;
fslmaths FA/cluster_${input}_thrbin -bin FA/cluster_${input}_thrbin_bin;
fslmeants -i all_FA_skeletonised.nii.gz -m FA/cluster_${input}_thrbin_bin >> all_cluster_${input}_thrbin_bin.txt;
input=MD_${t}_tfce_corrp_tstat1;
fslmaths MD/cluster_${input}_thrbin -bin MD/cluster_${input}_thrbin_bin;
fslmeants -i all_MD_skeletonised.nii.gz -m MD/cluster_${input}_thrbin_bin >> all_cluster_${input}_thrbin_bin.txt;
input=MD_${t}_tfce_corrp_tstat2;
fslmaths MD/cluster_${input}_thrbin -bin MD/cluster_${input}_thrbin_bin;
fslmeants -i all_MD_skeletonised.nii.gz -m MD/cluster_${input}_thrbin_bin >> all_cluster_${input}_thrbin_bin.txt;
input=RD_${t}_tfce_corrp_tstat1;
fslmaths RD/cluster_${input}_thrbin -bin RD/cluster_${input}_thrbin_bin;
fslmeants -i all_RD_skeletonised.nii.gz -m RD/cluster_${input}_thrbin_bin >> all_cluster_${input}_thrbin_bin.txt;



cd /home/fs0/djensen/scratch/project_WH/data/tbss/stats
rm */fslmeants.*


# --------------------------------------------
# we create scatter plots and further analysis with this in R. so the cluster text file values were copied into the csv file for visualisation in R. --> see WM_dti_tracts_linearRegr.R 
# --------------------------------------------


