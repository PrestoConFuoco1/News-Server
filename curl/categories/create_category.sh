#!/bin/bash
curl -G -d "token=$1&name=$2&parent_id=$3" localhost:5555/categories/create
echo ""
