#!/bin/bash
docker-compose build --force-rm

docker run -dti --name rrricanes -p 8787:8787 rrricanes_rrricanes

$SHELL
