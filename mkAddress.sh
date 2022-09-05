#!/bin/bash
echo "Upload - $1"
scp -i ~/.ssh/cardano_id_rsa $1 magic@testnet.ds.node:~/mkAddress/.

ssh -i ~/.ssh/cardano_id_rsa magic@testnet.ds.node "source ~/.bash_profile; cd mkAddress; ./mkAddress.preprod.sh $1"