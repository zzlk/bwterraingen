all: debug

debug: target/x86_64-unknown-linux-gnu/debug/bwmapserver

release: target/x86_64-unknown-linux-gnu/release/bwmapserver

local: debug
	RUST_LOG=info DEV_MODE=true ROOT_DIR=./bwmapserver DB_HOST=10.69.69.100 DB_PORT=5432 DB_CONNECTIONS=16 DB_DATABASE=bounding.net USE_SSL=false DB_USER=bounding.net DB_PASSWORD=averylongandcomplicatedpasswordtoguessihope RUST_BACKTRACE=full cargo run --target=x86_64-unknown-linux-gnu --bin bwmapserver

clean:
	cargo clean

check:
	cargo check --target=x86_64-unknown-linux-gnu

run:
	cargo run --target=x86_64-unknown-linux-gnu

.PHONY: all image clean debug release push target/x86_64-unknown-linux-gnu/debug/bwmapserver target/x86_64-unknown-linux-gnu/release/bwmapserver

target/x86_64-unknown-linux-gnu/debug/bwmapserver:
	cargo build --target=x86_64-unknown-linux-gnu

target/x86_64-unknown-linux-gnu/release/bwmapserver:
	cargo build --release --target=x86_64-unknown-linux-gnu
