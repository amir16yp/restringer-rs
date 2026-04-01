@echo off
set RUSTFLAGS=-A warnings
cargo test --quiet -- --nocapture