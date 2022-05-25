all: debug

debug: target/x86_64-unknown-linux-gnu/debug/bwterraingen

release: target/x86_64-unknown-linux-gnu/release/bwterraingen

clean:
	cargo clean

check:
	cargo check --target=x86_64-unknown-linux-gnu

run:
	cargo run --target=x86_64-unknown-linux-gnu -- ~/Downloads/RJ_GiantMountain.scx 18 18

flamegraph: debug
	RUST_LOG=none CARGO_BUILD_TARGET=x86_64-unknown-linux-gnu cargo flamegraph -c "record --call-graph fp -g" --dev -- ~/Downloads/RJ_GiantMountain.scx 18 18

profile: debug
	valgrind --tool=callgrind --dump-instr=yes --trace-jump=yes --simulate-cache=yes --collect-jumps=yes target/x86_64-unknown-linux-gnu/debug/bwterraingen ~/Downloads/RJ_GiantMountain.scx 18 18

test:
	cargo test --target=x86_64-unknown-linux-gnu

wasm:
	cargo build --release --target=wasm32-unknown-unknown
	wasm-bindgen target/wasm32-unknown-unknown/release/bwterraingen.wasm --out-dir temp

.PHONY: all image clean debug release push wasm target/x86_64-unknown-linux-gnu/debug/bwterraingen target/x86_64-unknown-linux-gnu/release/bwterraingen

target/x86_64-unknown-linux-gnu/debug/bwterraingen:
	cargo build --target=x86_64-unknown-linux-gnu

target/x86_64-unknown-linux-gnu/release/bwterraingen:
	cargo build --release --target=x86_64-unknown-linux-gnu
