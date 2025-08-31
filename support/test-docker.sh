#!/bin/bash

# Script to run the tests with Docker

echo "Starting the CouchDB and the test environment..."
docker-compose up -d

echo "Waiting for the CouchDB to be ready..."
sleep 5  # Give time for the container to start
until curl -s http://localhost:5984/ > /dev/null 2>&1; do
    echo "CouchDB is not ready yet, waiting..."
    sleep 2
done

echo "CouchDB is ready!"

echo "Running the tests in the Erlang container..."
docker-compose exec erlang-dev sh -c "rebar3 get-deps && rebar3 eunit"

echo "Tests are finished. To stop the containers, run: docker-compose down"