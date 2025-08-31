.PHONY: test test-docker clean docker-up docker-down docker-shell

# To run the tests locally (requires CouchDB)
test:
	rebar3 eunit

# To run the tests with Docker
test-docker: docker-up
	cd support && docker-compose exec erlang-dev sh -c "rebar3 get-deps && rebar3 eunit"

# To start the Docker environment
docker-up:
	cd support && docker-compose up -d
	@echo "Waiting for the CouchDB..."
	@until curl -s http://admin:admin@localhost:5984/ > /dev/null 2>&1; do \
		sleep 1; \
	done
	@echo "CouchDB is ready!"

# To stop the Docker environment
docker-down:
	cd support && docker-compose down

# To get a shell in the Erlang container
docker-shell: docker-up
	cd support && docker-compose exec erlang-dev sh

# To clean everything
clean:
	rebar3 clean
	cd support && docker-compose down -v