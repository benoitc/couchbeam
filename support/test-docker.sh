#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "[docker-test] Building and running eunit in containers..."
docker compose up --build --abort-on-container-exit test
rc=$?
echo "[docker-test] Bringing down stack..."
docker compose down -v
exit $rc

