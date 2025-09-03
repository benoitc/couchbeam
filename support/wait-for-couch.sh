#!/usr/bin/env bash
set -euo pipefail

URL=${COUCHDB_URL:-http://localhost:5984}
USER=${COUCHDB_ADMIN:-admin}
PASS=${COUCHDB_PASSWORD:-change_me}

echo "Waiting for CouchDB at $URL ..."
for i in {1..60}; do
  if curl -fsS "$URL/" >/dev/null; then
    echo "CouchDB up."
    break
  fi
  sleep 1
done

# verify auth works (if admin party disabled)
curl -fsS -u "$USER:$PASS" "$URL/_all_dbs" >/dev/null || true

# ensure system databases exist (idempotent)
curl -fs -u "$USER:$PASS" -X PUT "$URL/_users" >/dev/null 2>/dev/null || true
curl -fs -u "$USER:$PASS" -X PUT "$URL/_replicator" >/dev/null 2>/dev/null || true
curl -fs -u "$USER:$PASS" -X PUT "$URL/_global_changes" >/dev/null 2>/dev/null || true
echo "Ready."
