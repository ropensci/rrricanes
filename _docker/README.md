# README

**rrricanes** uses Docker for package development. Docker is a container service  that enables developers to maintain images to easily reproduce specific environments.

**rrricanes** currently can be used with any R version greater than or equal to 3.3.3:

  * 3.3.3 (Another Canoe)

  * 3.4.0 (You Stupid Darkness)

  * 3.4.1 (Single Candle)

  * 3.4.2 (Short Summer)

  * 3.4.3 (Kite-eating Tree)

  * 3.4.4. (Someone to Lean On)

  * devel

All images are automatically built via [Docker Hub](https://hub.docker.com/r/timtrice/rrricanes/) and can be accessed through the Tags link. The builds are based on the `develop` branch via GitHub. 

For example, to test a **rrricanes** deployment under R 3.4.1, 

```
docker pull timtrice/rrricanes:3.4.1
```

All images are built based on the [rocker/tidyversion](https://github.com/rocker-org/rocker-versioned/tree/master/tidyverse) image. Package dependencies are based on the base image's `BUILD_DATE` variable which varies on the version of R. For instance, the `BUILD_DATE` for R 3.3.3 is April 21, 2017. 

Each image uses the MRAN repository to ensure only packages available on the `BUILD_DATE` are used. Use of GitHub packages are possible but must specificy a release prior to the `BUILD_DATE`.

If there are packages used that do not have release dates prior to the `BUILD_DATE`, well, I haven't figured that out yet... Hopefully, rrricanes will still run without it. 

My working preference is to use a local `rrricanes` repository and attach it as a volume to the Docker container. With that being said, in the docker-compose files, the variable `LOCAL_RRRICANES` should be set to your local working directory prior to running docker-compose. 

For example, run `rrricanes:3.4.3`:
```{bash}
export LOCAL_RRRICANES=/home/timtrice/Projects/rrricanes
docker-compose -f _docker/3.4.3/docker-compose.yml up -d
```

The other option is to simply run the `docker run` command.

```{bash}
docker run -dti -p 8787:8787 --name rrricanes timtrice/rrricanes:3.4.3
```

Then, you can `docker exec` into the running container and clone the GitHub repository.
