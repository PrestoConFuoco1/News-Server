#!/bin/bash
curl -G -d "token=$1&category_id=$2" localhost:5555/categories/edit
echo ""
