#!/bin/bash
curl -v -G -d "token=admi&name=$1&parent_id=$2" localhost:5555/categories/create
echo ""
