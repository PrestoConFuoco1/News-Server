#!/bin/bash
curl -G -d "token=$1&category_id=$2&name=$3&parent_id=$4" localhost:5555/categories/edit
echo ""
