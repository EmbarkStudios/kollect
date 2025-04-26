use core::hash::BuildHasher;
use core::hash::Hasher;
use core::marker::PhantomData;

pub type BuildFixedHasher = foldhash::fast::FixedState;

/// A [`BuildHasher`] that builds a hasher specialized in hashing small keys containing
/// only primitive types. The exact hasher implementation provided by this [`BuildHasher`]
/// may change over time if better algorithms are discovered or implemented.
#[allow(missing_debug_implementations)]
#[derive(Default, Clone)]
pub struct BuildPrimitiveHasher;

impl BuildHasher for BuildPrimitiveHasher {
    type Hasher = rustc_hash::FxHasher;

    #[inline]
    fn build_hasher(&self) -> Self::Hasher {
        rustc_hash::FxHasher::default()
    }
}

/// Used to mark a type as able to be used with a [`NoHashHasher`]-based hash map or hash set.
///
/// In order to implement this trait for a type, that type should call [`Hasher::write_u64`] exactly *once*
/// in its [`Hash`] implementation. In writing that `u64`, it should *distribute the entropy of the key space*\[1\] across
/// the *whole 64 bits* (but in particular the very lowest and very highest bits are most important)!
/// Actually distributing entropy well is very important if you are going to use a [`NoHashHasher`] with the
/// Rust `std::HashMap` implementation, which is used internally by all the hash-based maps and sets provided by this crate.
/// In addition, it's important to note that this *should not* be used in cases where hash DOS attacks are a concern, and that it is
/// possible to get [unintentionally quadratic behavior] when reading from one map and writing to another when they both use a
/// [`NoHashHasher`].
///
/// You likely **should not** use a raw u8, u16 or u32 cast to a u64, or even an incrementing u64 counter and write it to the `NoHashHasher`,
/// because while you may get good entropy in the lowest bits (good!) you will get no entropy in the highest 7 bits (bad!), which are used
/// by the SwissTable-type implementation to disambiguate between items within the same "group" in parallel.
/// In particular, it is important to have good entropy both in the *lowest* and *highest* bits
/// of the u64, since the lowest bits will be used to select the index of the bucket (after truncating via modulo arithmetic to the capacity
/// of the collection) and the highest 7 bits will be used to quickly disambiguate collisions that end up at the same index after that
/// modulus.
///
/// That being said, sometimes in a particular use case can still be faster using a [`NoHashHasher`], so if you suspect it might then try benchmarking
/// and see :)
///
/// The best way to guarantee those properties is if you've already used a good hash function and the type you want to make [`NoHashable`] is therefore
/// already *in itself* a "cached" u64 hash. This is possible if you are generating and handing out handles yourself or are already precomputing hashes
/// for another reason. Otherwise, it may be better to use an `IntegralHasher` (TODO: actually expose this once `ahash` PR lands)
/// which *does* do hashing, but in a way specialized to be fast and high-quality for integer types.
///
/// \[1\] this means that for all the possible values of the set of keys you may want to insert into the table,
/// the bits changed by those values get distributed to change a uniform distribution of the full 64 bits.
///
/// # Example
///
/// ```
/// use core::hash::{Hash, Hasher};
/// use kollect::UnorderedNoHashMap;
/// use kollect::NoHashable;
///
/// /// Stores the pre-hashed hash of a string
/// #[derive(PartialEq, Eq, Clone, Copy)]
/// struct MyNoHashKey(u64);
///
/// // to create a key, we hash a string
/// impl<'a> From<&'a str> for MyNoHashKey {
///     fn from(string: &'a str) -> Self {
///         // the std hashmap DefaultHasher is guaranteed to be the same/deterministic hash when created through `new`
///         let mut hasher = std::collections::hash_map::DefaultHasher::new();
///         string.hash(&mut hasher);
///         Self(hasher.finish())
///     }
/// }
///
/// // just pass through the already-hashed u64. it's best to implement manually so we ensure it actually just calls `write_u64` once.
/// impl Hash for MyNoHashKey {
///     fn hash<H: Hasher>(&self, hasher: &mut H) {
///         hasher.write_u64(self.0)
///     }
/// }
///
/// // since our type is indeed writing a well-distributed u64, this is okay to implement!
/// impl NoHashable for MyNoHashKey {}
///
/// let mut m = UnorderedNoHashMap::<MyNoHashKey, String>::default();
///
/// let string1 = "some string";
/// let string1_key = MyNoHashKey::from(string1);
///
/// let string2 = "some other string";
/// let string2_key = MyNoHashKey::from(string2);
///
/// // this doesn't actually do any hashing!
/// m.insert(string1_key, string1.to_owned());
/// m.insert(string2_key, string2.to_owned());
///
/// // and neither does accessing!
/// assert_eq!(Some("some string"), m.get(&string1_key).map(String::as_str));
/// assert_eq!(Some("some other string"), m.get(&string2_key).map(String::as_str));
/// ```
///
/// [unintentionally quadratic behavior]: https://accidentallyquadratic.tumblr.com/post/153545455987/rust-hash-iteration-reinsertion
pub trait NoHashable {}

/// A hasher that does nothing. Use one as the hasher in a hash-table-based collection by using a [`BuildNoHashHasher`] as the third generic type
/// of the collection. See [`NoHashable`] for more information on which types can be hashed by this type of hasher.
#[derive(Clone, Copy)]
#[allow(missing_debug_implementations)]
pub struct NoHashHasher<T>(u64, PhantomData<T>);

impl<T> Default for NoHashHasher<T> {
    #[inline]
    fn default() -> Self {
        Self(0, PhantomData)
    }
}

impl<T: NoHashable> Hasher for NoHashHasher<T> {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, _: &[u8]) {
        unreachable!("invalid use of NoHashHasher!")
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.0 = i;
    }
}

/// A [`BuildHasher`] that creates [`NoHashHasher`]s. See docs of [`NoHashable`] for more information and an example.
#[allow(missing_debug_implementations)]
#[derive(Clone, Copy)]
pub struct BuildNoHashHasher<T>(PhantomData<T>);

impl<T> Default for BuildNoHashHasher<T> {
    #[inline]
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<T: NoHashable> BuildHasher for BuildNoHashHasher<T> {
    type Hasher = NoHashHasher<T>;

    #[inline]
    fn build_hasher(&self) -> Self::Hasher {
        NoHashHasher::default()
    }
}
