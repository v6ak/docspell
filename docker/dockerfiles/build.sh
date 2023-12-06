#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Please specify a version"
    exit 1
fi

version="$1"
if [[ $version == v* ]]; then
    version="${version:1}"
fi

push=""
if [ -z "$2" ] || [ "$2" == "--push" ]; then
    push="$2"
    if [ ! -z "$push" ]; then
        echo "Running with $push !"
    fi
elif [ "$2" == "--load" ]; then
    push="$2"
else
    echo "Don't understand second argument: $2"
    exit 1
fi

if ! docker buildx version > /dev/null; then
    echo "The docker buildx command is required."
    echo "See: https://github.com/docker/buildx#binary-release"
    exit 1
fi

tag_prefix="ghcr.io/$GITHUB_ACTOR/"

set -e
cd "$(dirname "$0")"

trap "{ docker buildx rm docspell-builder; }" EXIT

platforms=${PLATFORMS:-"linux/amd64,linux/arm64"}
docker buildx create --name docspell-builder --use

case $version in
    *SNAPSHOT)
        echo ">>>> Building nightly images for $version <<<<<"
        url_base="https://github.com/$GITHUB_REPOSITORY/releases/download/nightly"

        echo "============ Building Restserver ============"
        docker buildx build \
               --platform="$platforms" $push \
               --build-arg restserver_url="$url_base/docspell-restserver-$version.zip" \
               --tag "${tag_prefix}docspell/restserver:nightly" \
               -f restserver.dockerfile .

        echo "============ Building Joex ============"
        docker buildx build \
               --platform="$platforms" $push \
               --build-arg joex_url="$url_base/docspell-joex-$version.zip" \
               --tag "${tag_prefix}docspell/joex:nightly" \
               -f joex.dockerfile .
        ;;
    *)
        echo ">>>> Building release images for $version <<<<<"
        echo "============ Building Restserver ============"
        docker buildx build \
               --platform="$platforms" $push \
               --build-arg version=$version \
               --tag "${tag_prefix}docspell/restserver:v$version" \
               --tag "${tag_prefix}docspell/restserver:latest" \
               -f restserver.dockerfile .

        echo "============ Building Joex ============"
        docker buildx build \
               --platform="$platforms" $push \
               --build-arg version=$version \
               --tag "${tag_prefix}docspell/joex:v$version" \
               --tag "${tag_prefix}docspell/joex:latest" \
               -f joex.dockerfile .
esac
