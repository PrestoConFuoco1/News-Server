#!/bin/bash
curl -G -d "tags__all=[4,5]" localhost:5555/posts
echo ""
