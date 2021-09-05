#!/usr/bin/bash
# Rajiv Gangadharan 06/Sep/2021
container_name="portfolio-insights"
RUNNING=$(docker container inspect $container_name -f {{.State.Running}})
echo "Is the docker image running? "$RUNNING
if [ "$RUNNING" ==  "false" ]; then
	echo "Starting docker image "$container_name
	docker start $container_name
	[ $? -eq 0 ] || (echo "Error staring container." && exit 100)
	docker exec $container_name \
		Rscript -e 'bookdown::render_book("index.Rmd",output_dir="book")' \
		&& cp -R /usr/src/book/* /usr/src/www/html/
else
	echo "Container is running, wait for exiting."
fi
