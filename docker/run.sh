account="stitam"
container="webseq"
version="0.1.0.dev"

# build docker image
sudo docker build -f Dockerfile -t ${account}/${container}:${version} .

# OPTION 1 convert docker to singularity in one step
# sudo singularity build ${account}-${container}-${version}.img docker-daemon://${account}/${container}:${version}

# OPTION 2 export docker as tar and then convert to singularity
sudo docker image save ${account}/${container}:${version} -o ${account}-${container}-${version}.tar
sudo singularity build ${account}-${container}-${version}.img docker-archive:${account}-${container}-${version}.tar

# push to Docker Hub
# sudo docker push ${account}/${container}:${version}