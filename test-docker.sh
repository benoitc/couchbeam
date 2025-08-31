#!/bin/bash

# Script to run the tests with Docker

echo "Starting the CouchDB and the test environment..."
docker-compose up -d

echo "Waiting for the CouchDB to be ready..."
until curl -s http://admin:admin@localhost:5984/ > /dev/null; do
    echo "CouchDB is not ready yet, waiting..."
    sleep 2
done

echo "CouchDB is ready!"

echo "Running the tests in the Erlang container..."
docker-compose exec erlang-dev sh -c "rebar3 get-deps && rebar3 eunit"

echo "Tests are finished. To stop the containers, run: docker-compose down"