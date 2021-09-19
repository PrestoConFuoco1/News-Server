#!/bin/bash
curl -G -d "login=$1&pass_hash=123&firstname=jojo&lastname=bean" localhost:5555/users/create
echo ""
