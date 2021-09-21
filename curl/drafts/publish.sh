#!/bin/bash
curl -G -d "token=$1&draft_id=$2" localhost:5555/publish
echo ""
