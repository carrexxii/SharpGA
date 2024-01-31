TESTS_DIR  := ./tests
TESTS_PROJ := $(TESTS_DIR)/tests.fsproj

all: test

.PHONY: build
build:
	@dotnet build

.PHONY: test
test: build
	@dotnet run --project $(TESTS_PROJ)

.PHONY: restore
restore:
	@dotnet restore
	@dotnet restore $(TESTS_PROJ)

.PHONY: clean
clean:
	@dotnet clean
	@dotnet clean $(TESTS_PROJ)

.PHONY: remove
remove: clean
	@rm -rf ./bin ./obj
	@rm -rf $(TESTS_DIR)/bin $(TESTS_DIR)/obj
