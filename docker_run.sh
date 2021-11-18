# Converts all the tsv files in a directory to rds file
# Rajiv Gangadharan 11/Aug/2021
#
user_name=`id -un`
data_dir=data
docker rm portfolio-insights
docker run --name portfolio-insights -v/usr/src/inst/extdata:/usr/src/inst/extdata rajivgangadharan/mtrxbook-minimal
