# `kollect` changelog

<!-- markdownlint-disable MD024 -->

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- next-header -->
## Unreleased

- `LinearMap::new` and `LinearSet::new` are now `const`
- Uses `foldhash` instead of `ahash` as default hasher.

## `0.4.1`

- Fixed `linear_map::serde_as_seq` to be named `serde_as_map` as it should.

## `0.4.0`

### Changed 🔧
- Changed `serde` implementations for maps to be as sequences of `(k,v)` pairs
by default rather than as serde maps.
- Swap `serde_as_seq` adapters to `serde_as_map` to mirror the above change

## `0.3.2`

### Added ⭐

- Implement `Ord` and `PartialOrd` for `LinearSet`

## `0.3.1`

### Added ⭐

- Implement `Hash` for `LinearSet`

## `0.3.0`

### Changed 🔧

- Upgrade `indexmap` v1.9 -> v2.0
- Changed `OrderedMap::get_index_mut` to return the key as `&K` instead of `&mut K`
- Added `LinearMap::truncate` and `LinearSet::truncate`
- Added `LinearMap::as_slice`
