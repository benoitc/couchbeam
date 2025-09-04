DOCKER?=docker
COMPOSE?=docker compose

.PHONY: docker-build docker-up docker-down docker-logs docker-test docker-shell
docs:
	rebar3 ex_doc

docker-build:
	$(COMPOSE) build

docker-up:
	$(COMPOSE) up -d couchdb

docker-down:
	$(COMPOSE) down -v

docker-logs:
	$(COMPOSE) logs -f --tail=200

docker-test:
	# Start CouchDB in the background
	$(COMPOSE) up -d --build couchdb ; \
	# Run tests as a one-off container, attach only to test output
	$(COMPOSE) run --rm test ; \
	status=$$? ; \
	$(COMPOSE) down -v ; \
	exit $$status

docker-shell:
	$(COMPOSE) run --rm test bash
