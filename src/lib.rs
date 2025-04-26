//! # 🗂️ `kollect`
//!
//! A set of collections optimized for game development usecases.
//!
//! # Provided Collections
//!
//! - [`LinearMap`], useful when you want a collection that behaves as a key-value map but you will only ever have
//!   up to a few tens of elements. For small numbers of elements and small size of contained key/value types, this
//!   will likely be faster and take up less memory than other map types, but its random access operations have O(len)
//!   complexity rather than O(1) as with hash-based maps.
//! - [`UnorderedMap`], useful when you want a general-purpose key-value map, you plan to do random access
//!   lookup by key more frequently than iteration of the contained elements, and you don't care about order of
//!   those elements.
//! - [`OrderedMap`], useful when you want a key-value map including a set order of element pairs, or when
//!   you plan to iterate over the contained elements more frequently than you do random access lookup by key.
//!
//! - [`LinearSet`], useful in the same situation as [`LinearMap`] but when you're operating on a set of values rather
//!   than a map.
//! - [`UnorderedSet`], useful when you want set-like operations, will do more random access than iteration,
//!   and will fill with a medium to high number of elements.
//! - [`OrderedSet`], useful when you want set-like operations and need a defined order of elements, or you
//!   plan to iterate over the contained elements more frequently than do random access lookup on them.
//!
//! # Usage table
//!
//! Number of elements | Access pattern | Need defined order | Choose
//! ---|---|---|---
//! Less than ~128 | Any | Any | [`LinearMap`]/[`LinearSet`]
//! More than ~128 | More Random Access | No | [`UnorderedMap`]/[`UnorderedSet`]
//! More than ~128 | More Iteration | No | [`OrderedMap`]/[`OrderedSet`]
//! More than ~128 | Any | Yes | [`OrderedMap`]/[`OrderedSet`]
//!
//! # Specialized hashers
//!
//! The [`specialized_hashers`] module contains some helpful specialized hasher variants that can speed up your
//! hash-based collections when you have knowledge about the keys/elements being used.
//!
//! For example, if your keys in a map or set are small and primitive-integer-only, then you may want to use a
//! [`BuildPrimitiveHasher`]-based alias of one of the hash-based collections ([`UnorderedPrimitiveMap`], etc.).
//!
//! Alternatively, if you are keying a map or creating a collection of elements which are themselves already a
//! hash value (or which otherwise qualify as [`NoHashable`]), then you may want to use one of the [`BuildNoHashHasher`]-based
//! aliases of one of the hash-based collections ([`UnorderedNoHashMap`], etc.).
//!
//! # `serde_as_seq` for maps
//!
//! When the `serde` feature is enabled, there are adapters usable with `#[serde(with = "path::to::module")]` at `kollect::some_map::serde_as_seq`
//! which will serialize and deserialize those maps as sequences of `(k, v)` pair elements rather than as native serde Maps.
//! This is useful with JSON because JSON maps can only have string keys, and if you try to serialize a map to JSON which has a key type
//! that can't be serialized as a string, the serde JSON impl will simply panic at runtime. Using `serde_as_seq` will circumvent this issue
//! and allow you to serialize a wider range of key types.
//!
//! # Feature flags
//!
//! `kollect` uses a set of [feature flags] to optionally reduce the number of dependencies.
//!
//! The following optional features are available:
//!
//! Name | Description | Default?
//! ---|---|---
//! `speedy` | Enables [`speedy`] support for most types | No
//! `serde` | Enables [`serde`] support for most types | No
//!
//! [`speedy`]: https://crates.io/crates/speedy
//! [`serde`]: https://crates.io/crates/serde
//! [feature flags]: https://doc.rust-lang.org/cargo/reference/features.html

#![cfg_attr(docsrs, feature(doc_auto_cfg, doc_cfg))]
#![cfg_attr(test, allow(clippy::float_cmp))]

use core::hash::Hash;

use specialized_hashers::BuildNoHashHasher;
use specialized_hashers::BuildPrimitiveHasher;

/// The default hasher used by hash-based collections in this crate.
pub type BuildHasher = foldhash::fast::RandomState;

/// Provides a fast, general purpose, key-value map with **no** defined order of elements
pub mod unordered_map;

#[doc(inline)]
pub use unordered_map::UnorderedMap;

/// Type-alias of [`UnorderedMap`] that is useful when you are using small keys consisting of only a few primitive types.
pub type UnorderedPrimitiveMap<K, V> = UnorderedMap<K, V, BuildPrimitiveHasher>;

/// Provides a fast, general purpose, deduplicated set type with **no** defined order of elements
pub mod unordered_set;
#[doc(inline)]
pub use unordered_set::UnorderedSet;

/// Type-alias of [`UnorderedSet`] that is useful when you are using small elements consisting of only a few primitive types.
pub type UnorderedPrimitiveSet<T> = UnorderedSet<T, BuildPrimitiveHasher>;

/// Provides a fast, general purpose, key-value map **with** a defined order of elements.
pub mod ordered_map;
#[doc(inline)]
pub use ordered_map::OrderedMap;

/// Type-alias of [`OrderedMap`] that is useful when you are using small keys consisting of only a few primitive types.
pub type OrderedPrimitiveMap<K, V> = OrderedMap<K, V, BuildPrimitiveHasher>;

/// Provides a fast, general purpose, deduplicated set type **with* a defined order of elements.
pub mod ordered_set;
#[doc(inline)]
pub use ordered_set::OrderedSet;

/// Type-alias of [`OrderedSet`] that is useful when you are using small elements consisting of only a few primitive types.
pub type OrderedPrimitiveSet<T> = OrderedSet<T, BuildPrimitiveHasher>;

/// A key-value map specialized for small numbers of elements, implemented by searching linearly in a vector.
pub mod linear_map;
#[doc(inline)]
pub use linear_map::LinearMap;

/// A set specialized for small numbers of elements, implemented by searching linearly in a vector.
pub mod linear_set;
#[doc(inline)]
pub use linear_set::LinearSet;

/// Type-alias of [`UnorderedMap`] that is useful when you are using keys that are [`NoHashable`]
pub type UnorderedNoHashMap<K, V> = UnorderedMap<K, V, BuildNoHashHasher<K>>;

/// Type-alias of [`UnorderedSet`] that is useful when you are using elements that are [`NoHashable`]
pub type UnorderedNoHashSet<T> = UnorderedSet<T, BuildNoHashHasher<T>>;

/// Type-alias of [`OrderedMap`] that is useful when you are using keys that are [`NoHashable`]
pub type OrderedNoHashMap<K, V> = OrderedMap<K, V, BuildNoHashHasher<K>>;

/// Type-alias of [`OrderedSet`] that is useful when you are using elements that are [`NoHashable`]
pub type OrderedNoHashSet<T> = OrderedSet<T, BuildNoHashHasher<T>>;

/// Contains [`BuildHasher`][core::hash::BuildHasher]s (which can be used with other hash-based collections provided by this crate)
/// specialized for specific types of elements or situations.
pub mod specialized_hashers;

#[doc(inline)]
pub use specialized_hashers::NoHashable;

#[cfg(test)]
mod tests;

#[macro_use]
mod internal_macros;

const STATIC_RANDOM_SEED: u64 = 0x86c11a44c63f4f2f;

pub static FIXED_BUILD_HASHER: foldhash::fast::FixedState =
    foldhash::fast::FixedState::with_seed(STATIC_RANDOM_SEED);

#[inline(always)]
pub fn get_fixed_hasher() -> foldhash::fast::FoldHasher {
    use core::hash::BuildHasher;
    FIXED_BUILD_HASHER.build_hasher()
}

#[inline(always)]
pub fn hash_one_fixed<H: Hash>(one: H) -> u64 {
    use core::hash::Hasher;
    let mut hasher = foldhash::fast::FoldHasher::with_seed(
        STATIC_RANDOM_SEED,
        foldhash::SharedSeed::global_fixed(),
    );
    one.hash(&mut hasher);
    hasher.finish()
}

#[cold]
#[inline(never)]
fn panic_key_already_existed() {
    panic!("Failed to insert to map as the key already existed");
}
