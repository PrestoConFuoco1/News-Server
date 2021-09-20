#!/bin/bash
curl -G -d "token=$1" localhost:5555/drafts/get
echo ""
