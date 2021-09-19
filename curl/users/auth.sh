#!/bin/bash
curl -G -d "login=$1&pass_hash=$2" localhost:5555/auth
echo ""
