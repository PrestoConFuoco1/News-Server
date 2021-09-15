#!/bin/bash
curl -G -d "name=$1&parent_id=10" localhost:5555/categories/create
echo ""
