.PHONY: test test-docker clean docker-up docker-down docker-shell

# To run the tests locally (requires CouchDB)
test:
	rebar3 eunit

# To run the tests with Docker
test-docker: docker-up
	cd support && docker-compose exec erlang-dev sh -c "apk add --no-cache rebar3 git >/dev/null 2>&1 || true; rebar3 get-deps && rebar3 eunit"

# To start the Docker environment
docker-up:
	cd support && docker-compose up -d
	@echo "Waiting for CouchDB health status..."
	@until [ "$$(docker inspect -f '{{.State.Health.Status}}' couchbeam-couchdb 2>/dev/null)" = "healthy" ]; do \
		sleep 1; \
	done
	@echo "CouchDB is healthy!"

# To stop the Docker environment
docker-down:
	cd support && docker-compose down

# To get a shell in the Erlang container
docker-shell: docker-up
	cd support && docker-compose exec erlang-dev sh

# Show recent logs for debugging
docker-logs:
	cd support && docker-compose logs --no-color --tail=200

# To clean everything
clean:
	rebar3 clean
	cd support && docker-compose down -v
