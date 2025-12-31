RUNTIME_DIR = runtime
BIN_DIR = bin
BINARY_NAME = go--
CARGO = cargo

.PHONY: all runtime compiler clean install

all: runtime compiler

runtime:
	@echo "[*] Building runtime"
	$(MAKE) -C $(RUNTIME_DIR) install
	@echo "[+] Built runtime"

compiler:
	@echo "[*] Building compiler"
	$(CARGO) build --release
	@echo "[+] Built compiler"
	@mkdir -p $(BIN_DIR)
	cp target/release/$(BINARY_NAME) $(BIN_DIR)/
	@echo "[+] Built release moved to $(BIN_DIR)/$(BINARY_NAME)"

install: all

clean:
	@echo "[*] Cleaning runtime"
	$(MAKE) -C $(RUNTIME_DIR) clean
	@echo "[*] Cleaning compiler"
	$(CARGO) clean
	@echo "[*] Removing bin directory"
	rm -rf $(BIN_DIR)
