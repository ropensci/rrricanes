## Run

```
docker run \
  -dti \
  -e DISABLE_AUTH=true \
  -p 8787:8787 \
  --name rrricanes \
  -v /home/timtrice/Projects/ropensci/rrricanes:/home/rstudio/rrricanes \
  timtrice/rrricanes:release
```

## Shell

```
docker exec -ti rrricanes /bin/bash
```
