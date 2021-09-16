#!/bin/bash
curl -v -G -d "token=$1&name=Ocaml&parent_id=5" localhost:5555/categories/create
echo ""
