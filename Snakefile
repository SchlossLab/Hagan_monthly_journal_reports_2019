import datetime

date = datetime.datetime.today().strftime('%Y_%m_%d')
workdir = f"data/ejp_transfer_{date}" #folder containing the transfer completed on today's date
reports_dir = f"reports/{date}" #destination folder for report
tempdir = "data/ejp_transfer_temp" #temporary file placement from sftp
compdir = "data/ejp_transfer_comp" #compiled files from all transfers
passphrase_file = "pass"

checkpoint download:
    output: dir(tempdir)
    params:
        tempdir = tempdir
    shell:
        """
        sftp -b sftp_batch ejpress
        get *.zip {params.tempdir}
        bye
        """

def get_zip_filenames(wildcards):
    dir = checkpoints.download.get(**wildcards).output.dir
    filename_bases, = glob_wildcards(f"{dir}/{{filename}}.zip")
    return expand("{dir}/{filename}.zip", dir=dir, filename=filenames)

######################################################
#unzip files from today's transfer

rule copy_zip_files:
    input:
        f"{tempdir}/{{filename}}.zip"
    output:
        f"{workdir}/{{filename}}"
    params:
        workdir = workdir
    shell:
        """
        cp {input} {params.workdir}
        unzip {params.workdir}/{wildcards.filename}.zip
        """

######################################################
#unencrypt & decompress previous files and merge with files from today's transfer

rule unencrypt: #unencrypt directory with all prior xml files compiled
    input:
        f"{compdir}.tar.gz.gpg"
    output:
        tar = f"{compdir}_unencrypted.tar.gz"
    shell:
        """
        gpg --batch --passphrase-file ../pass --output {output.tar} --decrypt {input}
        """

checkpoint untar:  #decompress compiled directory
    input:
        tar = rules.ununcrypt.output.tar
    output:
        dir = dir(f"{compdir}_unencrypted")
    shell:
        """
        tar -xzf {input.tar}
        """

rule copy_xml_files:  #move unzipped xml files into compiled directory (updated xml files will replace old versions)
    input:
        f"{workdir}/{{filename}}.xml"
    output:
        f"{compdir}_unencrypted/{{filename}}.xml"
    shell:
        """
        cp {input} {output}
        """

##################################################
def get_xml_filenames:
    dir = checkpoints.checkpoint.get(**wildcards).output.dir
    filename_bases, = glob_wildcards(f"{dir}/{{filename}}.xml")
    return expand("{dir}/{filename}.xml", dir=dir, filename=filenames)

rule run_monthly_reports:  #generate monthly reports
    input:
        R = "code/run_monthly_reports.R"
        xml = get_xml_filenames
    script:
        "input.R"

#################################################
#encrypt compliled and standalone versions of today's transfer & delete unencrypted data

rule compress_today:
    input:
        f"{workdir}"
    output:
        tar=f"{workdir}.tar.gz"
    shell:
        """
        tar -czf {output} {input}
        """

rule encrypt_today:  #save today's transfer as encrypted file
    input:
        rules.compress_today.output.tar
    output:
        f"{rules.compress_today.output.tar}.gpg"
    params:
        pass=passphrase_file
    shell:
        """
        gpg --batch --passphrase-file {params.pass} -c {input}
        """

rule compress_compiled: #compress new compiled files
    input:
        f'{compdir}'
    output:
        tar=f'{compdir}.tar.gz'
    shell:
        """
        tar -czf {output} {input}
        """

rule encrypt_compiled:  #encrypt new compiled files
    input:
        rules.compress_compiled.output.tar
    output:
        f"{rules.compress_compiled.output.tar}.gpg"
    params:
        pass=passphrase_file
    shell:
        """
        gpg --batch --passphrase-file {params.pass} -c {input}
        """

#delete unencrypted files from today's transfer
#delete today's directory
#delete remaining unencrypted data
rule cleanup:
    params:
        tempdir = tempdir
        compdir = compdir
        workdir = workdir
    shell:
        "rm -rf {params.workdir} {params.tempdir}/*.zip {params.compdir}/*.xml  {params.compdir}.tar.gz {params.workdir}.tar.gz "
