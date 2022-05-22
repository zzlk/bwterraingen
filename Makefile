all: debug

debug: target/x86_64-unknown-linux-gnu/debug/bwterraingen

release: target/x86_64-unknown-linux-gnu/release/bwterraingen

clean:
	cargo clean

check:
	cargo check --target=x86_64-unknown-linux-gnu

run:
	cargo run --target=x86_64-unknown-linux-gnu

flamegraph:
	CARGO_BUILD_TARGET=x86_64-unknown-linux-gnu cargo flamegraph -c "record --call-graph dwarf -g" --dev -- ~/Downloads/RJ_GiantMountain.scx 18 18

profile:
	valgrind --tool=callgrind --dump-instr=yes --trace-jump=yes --simulate-cache=yes --collect-jumps=yes target/x86_64-unknown-linux-gnu/debug/bwterraingen ~/Downloads/RJ_GiantMountain.scx 18 18

test:
	cargo test --target=x86_64-unknown-linux-gnu

.PHONY: all image clean debug release push target/x86_64-unknown-linux-gnu/debug/bwterraingen target/x86_64-unknown-linux-gnu/release/bwterraingen

target/x86_64-unknown-linux-gnu/debug/bwterraingen:
	cargo build --target=x86_64-unknown-linux-gnu

target/x86_64-unknown-linux-gnu/release/bwterraingen:
	cargo build --release --target=x86_64-unknown-linux-gnu
