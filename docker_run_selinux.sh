# Converts all the tsv files in a directory to rds file
# Rajiv Gangadharan 11/Aug/2021
#
user_name=`id -un`
DATA_DIR_HOST=/home/rajivg/Documents/Work/Code/Datasets
WEB_DIR_HOST=/var/www/html

docker rm portfolio-insights
docker run -it --name portfolio-insights -v/var/www/html:/usr/src/www/html:z -v/home/rajivg/Documents/Work/Code/Datasets:/usr/src/inst/extdata:z  rajivgangadharan/mtrxbook

