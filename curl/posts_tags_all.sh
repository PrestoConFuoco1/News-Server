#!/bin/bash
curl -G -d "tags__all=[123,124,125]" localhost:5555/posts
