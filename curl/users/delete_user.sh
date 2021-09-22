#!/bin/bash
curl -G -d "token=$1&user_id=$2" localhost:5555/users/delete
echo ""
