# aoc-2020

This is my repository for Advent of Code 2020. This year I am using
[Rune](https://github.com/rune-rs/rune/), a scripting language based on Rust.

To make this ergonomic, I'm using hot-reloading with file monitoring, which will
recompute the results based on which files are edited. This also uses Rust to
host the script runtime instead of an external binary.
