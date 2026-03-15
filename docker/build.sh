docker build --pull --progress=plain --tag inbobmk/flandersqmd:devel ../flandersqmd
docker run -it --rm -e INPUT_FOLDER=source -e GITHUB_TOKEN=$GITHUB_PAT -e GITHUB_SHA=$(git rev-parse HEAD) -e GITHUB_REPOSITORY=inbo/flandersqmd-book -e GITHUB_REF_NAME=$(git rev-parse --abbrev-ref HEAD) -e GITHUB_EVENT_NAME=test --entrypoint=/bin/bash inbobmk/flandersqmd:devel
docker run --rm -e INPUT_FOLDER=source -e GITHUB_TOKEN=$GITHUB_PAT -e GITHUB_SHA=$(git rev-parse HEAD) -e GITHUB_REPOSITORY=inbo/flandersqmd-book -e GITHUB_REF_NAME=$(git rev-parse --abbrev-ref HEAD) -e GITHUB_EVENT_NAME=test inbobmk/flandersqmd:devel
docker push inbobmk/flandersqmd:devel
