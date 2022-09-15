#!/bin/bash

URL=${BITBUCKET_SERVER_URL:-http://localhost:7990}

set -e

if curl -s "$URL/status" | grep -q RUNNING ; then
    echo "Bitbucket server at $URL is up"
else
    if docker-compose version >/dev/null 2>&1; then
        echo "Bitbucket server at $URL does not seem to be up, starting local instance..."
        SCRIPTS_DIR=$(cd "$(dirname "$0")" || exit 1; pwd)
        (cd "$SCRIPTS_DIR" && docker-compose up --build -d --wait)
        echo "You can leave this Bitbucket server running until you are done testing."
        echo "To stop this instance, run 'docker-compose down' in the scripts directory."
    else
        echo "No Bitbucket server found at $URL, and docker-compose is not installed."
        echo "Refer to README.md for instructions."
        exit 1
    fi
fi
