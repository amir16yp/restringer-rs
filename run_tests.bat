@echo off
set RUSTFLAGS=-A warnings
cargo test --quiet -- --nocapture --test-threads=1