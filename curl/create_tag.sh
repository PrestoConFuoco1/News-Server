#!/bin/bash
curl -G -d "token=$1&name=$2" localhost:5555/tags/create
echo ""
