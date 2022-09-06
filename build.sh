#!/nix/store/can473ld4dc8izcjlm4i5daxsh1yl5d8-bash-4.4-p23/bin/bash

RATE=$2

TAG_DATE=$(date '+%Y%m%d%H%M%S')

# Git Tag
git commit -am "Build ${TAG_DATE}.$1 - $3"
git tag -a "${TAG_DATE}.$1" -m "Build ${TAG_DATE}"
git push origin --tags

cabal run DropspotMarketCompile "\"${1}\"" 400 builds/dsMarket.$1.$TAG_DATE

echo "-----------------------------"
echo "dsMarket.$1.$TAG_DATE"
echo "-----------------------------"