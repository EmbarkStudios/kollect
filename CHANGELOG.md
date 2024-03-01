# `kollect` changelog

<!-- markdownlint-disable MD024 -->

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## `0.3.0`

### Changed 🔧

- Upgrade `indexmap` v1.9 -> v2.0
- Changed `OrderedMap::get_index_mut` to return the key as `&K` instead of `&mut K`
- Added `LinearMap::truncate` and `LinearSet::truncate`
- Added `LinearMap::as_slice`
