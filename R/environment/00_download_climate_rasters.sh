#!/bin/bash
set -e
set -u
set -o pipefail

N=8

for a in `seq 1994 2016`
do
                
        
        (for b in `seq 1 12`
        
        do 
                        d="0"
 
                if [ $b -lt 10 ]
                then
                        c="$d$b"
                else
                        c=$b
                fi

                echo "year = $a month = $c"

                file_name=https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/pr/CHELSA_pr_${c}_${a}_V.2.1.tif 
                
                file_name_2=https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tasmax/CHELSA_tasmax_${c}_${a}_V.2.1.tif

                file_name_4=https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tasmin/CHELSA_tasmin_${c}_${a}_V.2.1.tif

                echo $file_name

                curl --output rasters/CHELSA_prec_${a}_${c}_V1.2.1.tif $file_name

                curl --output rasters/CHELSA_tmax_${a}_${c}_V1.2.1.tif $file_name_2        

                curl --output rasters/CHELSA_tmin_${a}_${c}_V1.2.1.tif $file_name_4

        done) &

        
done

