#!/bin/sh
#PBS -N monthly_report
#PBS -l nodes=1:ppn=8,mem=46gb
#PBS -l walltime=20:00:00
#PBS -j oe
#PBS -m abe
#PBS -V
#PBS -M akhagan@umich.edu
#PBS -l qos=flux
#PBS -q fluxod
#PBS -A pschloss_fluxod

pwd

cd $PBS_O_WORKDIR

#assumes that "sftp -b sftp_batch ejpress" has already been completed

dir="ejp_transfer_$(date +'%Y_%m_%d')" #folder containing the transfer completed on today's date

tempdir="ejp_transfer_temp" #temporary file placement from sftp

compdir="ejp_transfer_comp" #compiled files from all transfers

######################################################
#unzip files from today's transfer

mkdir reports/$(date +'%Y_%m_%d') #make destination folder for report

mkdir data/$dir #create folder to contain most recent transfer

mkdir data/${compdir}_decry #make folder to contain compiled data files

ls data/

cp data/$tempdir/*.zip data/$dir #copy most recent transfer zipped files to new folder

cd data/$dir

echo cd data/$dir

ls #confirm transfer

unzip -o '*.zip' #unzip new files in new directory

cd ../ #puts you back into the data folder


pwd

######################################################
#unencrypt & decompress previous files and merge with files from today's transfer

gpg --batch --passphrase-file ../pass --output ${compdir}_decry.tar.gz --decrypt ${compdir}.tar.gz.gpg #unencrypt directory with all prior xml files compiled

tar xvzf ${compdir}_decry.tar.gz -C ${compdir}_decry/ #decompress compiled directory

ls

cp -f $dir/*.xml ${compdir}_decry/ #move unzipped xml files into compiled directory (updated xml files will replace old versions)

cd ../ #move back up to project root

pwd

##################################################

Rscript code/run_monthly_reports.R #generate monthly reports

#################################################
#encrypt compliled and standalone versions of today's transfer & delete unencrypted data

cd data/

ls

rm $dir/*.xml

ls $dir

tar czf ${dir}.tar.gz $dir #compress today's transfer

gpg --batch --passphrase-file ../pass -c ${dir}.tar.gz #save today's transfer as encrypted file

rm $tempdir/*.zip $dir/*.zip #delete unencrypted files from today's transfer

rmdir $dir #delete today's directory

tar czf ${compdir}.tar.gz ${compdir}_decry #compress new compiled files

gpg --batch --yes --passphrase-file ../pass -c ${compdir}.tar.gz #encrypt new compiled files

rm ${compdir}/*.xml ${dir}.tar.gz ${compdir}_decry.tar.gz ${compdir}.tar.gz #delete remaining unencrypted data

rmdir $compdir/

ls

stat -f $PBS_JOBID
~                                                                               
~    
