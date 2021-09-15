#!/bin/python3

import json
import subprocess

process = subprocess.run(['./posts_all.sh'], capture_output=True, text=True)
to_python = json.loads(process.stdout)
for i in range(len(to_python)):
    print(to_python[i]['title'])

