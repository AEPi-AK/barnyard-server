#!/bin/bash
cd "$(dirname "$0")"

pushd ..

stack exec -- yesod keter
sudo cp game-server.keter /opt/keter/incoming
popd


