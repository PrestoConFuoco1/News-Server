#!/bin/bash
curl -G -d "tags__in=[4,7]" localhost:5555/posts/get
echo ""
