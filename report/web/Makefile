ROOT_DIR := $(abspath ../..)
WEB_DIR := $(ROOT_DIR)/report/web
GENERATED_DIR := $(WEB_DIR)/generated
LOCAL_URL := http://localhost:8000/

.PHONY: build serve run deploy deploy-dry-run

build:
	cd "$(ROOT_DIR)" && Rscript report/R/10-build-web.R

serve:
	python3 -m http.server 8000 --directory "$(GENERATED_DIR)"

run: build
	@python3 -m http.server 8000 --directory "$(GENERATED_DIR)" & server_pid=$$!; \
	trap 'kill $$server_pid' EXIT INT TERM; \
	open "$(LOCAL_URL)"; \
	wait $$server_pid

deploy:
	./deploy.sh

deploy-dry-run:
	./deploy.sh --dry-run
