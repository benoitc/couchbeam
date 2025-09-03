#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "[docker-test] Starting CouchDB and running eunit..."
docker compose up -d --build couchdb
docker compose run --rm test
rc=$?
echo "[docker-test] Bringing down stack..."
docker compose down -v
exit $rc
