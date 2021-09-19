#!/bin/bash
curl -G -d "token=$1&category_id=$2&name=$3" localhost:5555/categories/edit
echo ""
