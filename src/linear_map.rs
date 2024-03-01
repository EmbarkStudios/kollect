// This code originally from the `linear_map` crate, but elected to be copy/reimplemented with modifications here as
// that crate is no longer actively maintained.

#![deny(missing_docs)]

use core::cmp::Ordering;
use core::hash::Hash;
use core::hash::Hasher;
use std::borrow::Borrow;
use std::fmt;
use std::fmt::Debug;
use std::iter;
use std::mem;
use std::ops;
use std::slice;
use std::vec;

use crate::STATIC_RANDOM_STATE;

use self::Entry::Occupied;
use self::Entry::Vacant;

/// A map implemented by searching linearly in a vector.
///
/// `LinearMap`'s keys are compared using the [`Eq`][eq] trait. All search operations
/// (`contains_key`, `get`, `get_mut`, `insert`, and `remove`) run in `O(n)` time, making this
/// implementation suitable only for small numbers of keys, but simpler and likely faster than
/// a hashtable-based map for small numbers of keys. The ordering of the keys in the
/// underlying vector is arbitrary, but well defined.
///
/// Implements `PartialEq`, `Eq`, and `Hash` such that two maps are equal and hash to the same value if they have
/// the same `(k, v)` element pairs regardless of order. However, the `Hash` implementation is not fully cryptographically secure.
///
/// Implements `Ord`, *but* the implementation requires that for maps
/// of the same length, we copy the `Vec`s containing all the `(k, v)` element pairs for both maps,
/// sort them by `k`, and then do [lexographical] ordering between them,
/// which is very slow and it's not recommended to use this functionality if at all possible.
///
/// It is a logic error for a key to be modified in such a way that the key's equality, as
/// determined by the [`Eq`][eq] trait, changes while it is in the map. This is normally only
/// possible through [`Cell`][cell], [`RefCell`][ref_cell], global state, I/O, or unsafe code.
///
/// [cell]: https://doc.rust-lang.org/nightly/std/cell/struct.Cell.html
/// [eq]: https://doc.rust-lang.org/nightly/std/cmp/trait.Eq.html
/// [ref_cell]: https://doc.rust-lang.org/nightly/std/cell/struct.RefCell.html
///
/// # Ordering
///
/// Ordering of elements is well-defined but may be unexpected due to the use of
/// [`swap_remove`][Vec::swap_remove] in the implementation of [`LinearMap::remove`].
/// If you want to maintain order on removal, see [`LinearMap::shift_remove`], but note
/// its additional time complexity. You can also use one of the sorting functions to
/// change the order after doing arbitrary insertion and removals.
///
/// # Example
///
/// ```
/// use kollect::linear_map::LinearMap;
///
/// // type inference lets us omit an explicit type signature (which
/// // would be `LinearMap<&str, &str>` in this example).
/// let mut book_reviews = LinearMap::new();
///
/// // review some books.
/// book_reviews.insert("Adventures of Huckleberry Finn",    "My favorite book.");
/// book_reviews.insert("Grimms' Fairy Tales",               "Masterpiece.");
/// book_reviews.insert("Pride and Prejudice",               "Very enjoyable.");
/// book_reviews.insert("The Adventures of Sherlock Holmes", "Eye lyked it alot.");
///
/// // check for a specific one.
/// if !book_reviews.contains_key("Les Misérables") {
///     println!("We've got {} reviews, but Les Misérables ain't one.",
///              book_reviews.len());
/// }
///
/// // oops, this review has a lot of spelling mistakes. let's delete it.
/// book_reviews.remove("The Adventures of Sherlock Holmes");
///
/// // look up the values associated with some keys.
/// let to_find = ["Pride and Prejudice", "Alice's Adventure in Wonderland"];
/// for book in &to_find {
///     match book_reviews.get(book) {
///         Some(review) => println!("{}: {}", book, review),
///         None => println!("{} is unreviewed.", book)
///     }
/// }
///
/// // iterate over everything.
/// for (book, review) in &book_reviews {
///     println!("{}: \"{}\"", book, review);
/// }
/// ```
///
/// [lexographical]: core::cmp::Ord#lexographical-comparison
pub struct LinearMap<K, V> {
    pub(crate) storage: Vec<(K, V)>,
}

impl<K, V> LinearMap<K, V> {
    /// Creates a new [`LinearMap`] from the `Vec` without checking for duplicate entries or other logic errors.
    ///
    /// This is faster than `<LinearMap as From<Vec<(K, V)>>::from(vec)` but could result in logic errors.
    #[inline]
    pub fn from_vec_unchecked(storage: Vec<(K, V)>) -> Self {
        Self { storage }
    }
}

impl<K: Eq, V> LinearMap<K, V> {
    /// Creates an empty map. This method does not allocate.
    #[inline]
    pub fn new() -> Self {
        Self { storage: vec![] }
    }

    /// Creates an empty map with the given initial capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            storage: Vec::with_capacity(capacity),
        }
    }

    /// Returns the number of elements the map can hold without reallocating.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.storage.capacity()
    }

    /// Reserves capacity for at least `additional` more to be inserted in the
    /// map. The collection may reserve more space to avoid frequent
    /// reallocations.
    ///
    /// # Panics
    ///
    /// Panics if the new allocation size overflows `usize`.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.storage.reserve(additional);
    }

    /// Reserves the minimum capacity for exactly `additional` more elemnnts to
    /// be inserted in the map.
    ///
    /// Note that the allocator may give the collection more space than it
    /// requests. Therefore capacity cannot be relied upon to be precisely
    /// minimal. Prefer `reserve` if future insertions are expected.
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `usize`.
    #[inline]
    pub fn reserve_exact(&mut self, additional: usize) {
        self.storage.reserve_exact(additional);
    }

    /// Shrinks the capacity of the map as much as possible.
    ///
    /// It will drop down as close as possible to the current length but the
    /// allocator may still inform the map that there is more space than
    /// necessary. Therefore capacity cannot be relid upon to be minimal.
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.storage.shrink_to_fit();
    }

    /// Shrinks the capacity of the map as much as possible to a given minimum capacity.
    ///
    /// It will drop down as close as possible to either the given lower bound or the current length but the
    /// allocator may still inform the map that there is more space than
    /// necessary. Therefore capacity cannot be relid upon to be minimal.
    #[inline]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.storage.shrink_to(min_capacity);
    }

    /// Shortens the map, keeping the first len elements and dropping the rest.
    ///
    /// If len is greater than the map's current length, this has no effect.
    #[inline]
    pub fn truncate(&mut self, len: usize) {
        self.storage.truncate(len);
    }

    /// Returns the number of elements in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.storage.len()
    }

    /// Returns true if the map contains no elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.storage.is_empty()
    }

    /// Clears the map, removing all elements.
    ///
    /// Unlike the hashtable-based maps provided by this crate, keeps the
    /// whole allocated memory for reuse and does not shrink by default since
    /// this map is not designed to ever have a high number of elements and clearing
    /// the underlying vector storage is O(1).
    #[inline]
    pub fn clear(&mut self) {
        self.storage.clear();
    }

    /// Scan through the map and keep those key-value pairs where the
    /// closure returns `true`.
    ///
    /// The order the elements are visited is not specified.
    pub fn retain<F>(&mut self, mut keep_fn: F)
    where
        F: FnMut(&K, &mut V) -> bool,
    {
        let mut del = 0;
        {
            let v = &mut *self.storage;
            for i in 0..v.len() {
                if !keep_fn(&v[i].0, &mut v[i].1) {
                    del += 1;
                } else if del > 0 {
                    v.swap(i - del, i);
                }
            }
        }
        if del > 0 {
            let len = self.storage.len();
            self.storage.truncate(len - del);
        }
    }

    /// Removes all key-value pairs from the map and returns an iterator.
    ///
    /// All key-value pairs are removed even if the iterator is not exhausted. However, the
    /// behavior of this method is unspecified if the iterator is leaked.
    ///
    /// The iterator's item type is `(K, V)`.
    #[inline]
    pub fn drain(&mut self) -> Drain<'_, K, V> {
        Drain {
            iter: self.storage.drain(..),
        }
    }

    /// Returns an iterator yielding references to the map's keys and their corresponding values.
    ///
    /// The iterator's item type is `(&K, &V)`.
    #[inline]
    pub fn iter(&self) -> Iter<'_, K, V> {
        Iter {
            iter: self.storage.iter(),
        }
    }

    /// Returns an iterator yielding references to the map's keys and mutable references to their
    /// corresponding values.
    ///
    /// The iterator's item type is `(&K, &mut V)`.
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        IterMut {
            iter: self.storage.iter_mut(),
        }
    }

    /// Returns an iterator yielding mutable references to the map's keys and values. **Careful: it is possible to create
    /// logic errors by modifying the keys of the map using this type**.
    ///
    /// In order to not create logic errors, all keys should remain unique after they have been modified, i.e. no two keys
    /// should be `Eq` equal to each other.
    ///
    /// The iterator's item type is `(&mut K, &mut V)`.
    #[inline]
    pub fn iter_full_unchecked_mut(&mut self) -> IterFullUncheckedMut<'_, K, V> {
        IterFullUncheckedMut {
            iter: self.storage.iter_mut(),
        }
    }

    /// Returns an iterator yielding references to the map's keys.
    ///
    /// The iterator's item type is `&K`.
    #[inline]
    pub fn keys(&self) -> Keys<'_, K, V> {
        Keys { iter: self.iter() }
    }

    /// Returns an iterator yielding references to the map's values.
    ///
    /// The iterator's item type is `&V`.
    #[inline]
    pub fn values(&self) -> Values<'_, K, V> {
        Values { iter: self.iter() }
    }

    /// Returns an iterator yielding references to the map's values.
    ///
    /// The iterator's item type is `&V`.
    #[inline]
    pub fn values_mut(&mut self) -> ValuesMut<'_, K, V> {
        ValuesMut {
            iter: self.storage.iter_mut(),
        }
    }

    /// Returns a reference to the value in the map whose key is equal to the given key.
    ///
    /// Returns `None` if the map contains no such key.
    ///
    /// The given key may be any borrowed form of the map's key type, but `Eq` on the borrowed form
    /// *must* match that of the key type.
    pub fn get<Q: ?Sized + Eq>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        for (k, v) in self {
            if key == k.borrow() {
                return Some(v);
            }
        }
        None
    }

    /// Returns a mutable reference to the value in the map whose key is equal to the given key.
    ///
    /// Returns `None` if the map contains no such key.
    ///
    /// The given key may be any borrowed form of the map's key type, but `Eq` on the borrowed form
    /// *must* match that of the key type.
    pub fn get_mut<Q: ?Sized + Eq>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
    {
        for (k, v) in self {
            if key == k.borrow() {
                return Some(v);
            }
        }
        None
    }

    /// Returns a reference to the element in the map at the given index, if it exists.
    #[inline]
    pub fn get_index(&self, index: usize) -> Option<&(K, V)> {
        self.storage.get(index)
    }

    /// Returns a mutable reference to the element in the map at the given index, if it exists.
    ///
    /// Note the key is still an immutable reference as it is a logic error to change the key's value
    /// while it is in the map.
    #[inline]
    pub fn get_index_mut(&mut self, index: usize) -> Option<(&K, &mut V)> {
        let (k, v) = self.storage.get_mut(index)?;
        Some((&*k, v))
    }

    /// Checks if the map contains a key that is equal to the given key.
    ///
    /// The given key may be any borrowed form of the map's key type, but `Eq` on the borrowed form
    /// *must* match that of the key type.
    #[inline]
    pub fn contains_key<Q: ?Sized + Eq>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
    {
        self.get(key).is_some()
    }

    /// Inserts a key-value pair into the map.
    ///
    /// Returns `None` if the map did not contain a key that is equal to the given key.
    ///
    /// If the map did contain such a key, its corresponding value is replaced with the given
    /// value, and the old value is returned. The key is not updated, though. This matters for
    /// values that can be `==` without being identical. See the [standard library's documentation]
    /// [std] for more details.
    ///
    /// [std]: https://doc.rust-lang.org/nightly/std/collections/index.html#insert-and-complex-keys
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        match self.entry(key) {
            Occupied(mut e) => Some(e.insert(value)),
            Vacant(e) => {
                e.insert(value);
                None
            }
        }
    }

    /// Removes (by swap) the key in the map that is equal to the given key and returns its corresponding
    /// value.
    ///
    /// Returns `None` if the map contained no such key.
    ///
    /// If you need to retain the order of elements in the map, use `shift_remove` instead.
    ///
    /// The given key may be any borrowed form of the map's key type, but `Eq` on the borrowed form
    /// *must* match that of the key type.
    pub fn remove<Q: ?Sized + Eq>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
    {
        for i in 0..self.storage.len() {
            if self.storage[i].0.borrow() == key {
                return Some(self.storage.swap_remove(i).1);
            }
        }
        None
    }

    /// Removes the key in the map that is equal to the given key and returns its corresponding
    /// value by shifting all elements after it down (computes in O(n**2) time worst case).
    ///
    /// Returns `None` if the map contained no such key.
    ///
    /// If you do not need to retain the order of elements in the map, use `remove` instead, which does not
    /// need to shift all subsequent elements.
    ///
    /// The given key may be any borrowed form of the map's key type, but `Eq` on the borrowed form
    /// *must* match that of the key type.
    pub fn shift_remove<Q: ?Sized + Eq>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
    {
        for i in 0..self.storage.len() {
            if self.storage[i].0.borrow() == key {
                return Some(self.storage.remove(i).1);
            }
        }
        None
    }

    /// Returns the given key's corresponding entry in the map for in-place manipulation.
    pub fn entry(&mut self, key: K) -> Entry<'_, K, V> {
        match self.storage.iter().position(|(k, _)| key == *k) {
            None => Vacant(VacantEntry { map: self, key }),
            Some(index) => Occupied(OccupiedEntry { map: self, index }),
        }
    }

    /// Sort the map’s key-value pairs by the default ordering of the keys.
    ///
    /// Since we are guaranteed to have no equal elements, we can use unstable sort by default.
    #[inline]
    pub fn sort_keys(&mut self)
    where
        K: Ord,
    {
        self.storage.sort_unstable_by(|(k1, _), (k2, _)| k1.cmp(k2));
    }

    /// Sort the map’s key-value pairs by the given comparison function.
    #[inline]
    pub fn sort_by<F>(&mut self, mut cmp: F)
    where
        F: FnMut(&K, &V, &K, &V) -> Ordering,
    {
        self.storage
            .sort_by(|(k1, v1), (k2, v2)| cmp(k1, v1, k2, v2));
    }

    /// Sort the map’s key-value pairs by the given comparison function with an
    /// unstable sorting function.
    #[inline]
    pub fn sort_unstable_by<F>(&mut self, mut cmp: F)
    where
        F: FnMut(&K, &V, &K, &V) -> Ordering,
    {
        self.storage
            .sort_unstable_by(|(k1, v1), (k2, v2)| cmp(k1, v1, k2, v2));
    }

    /// Reverses the order of the contained elements in the vector storage
    #[inline]
    pub fn reverse(&mut self) {
        self.storage.reverse();
    }

    /// View `self` as a slice of `(K, V)`
    #[inline]
    pub fn as_slice(&self) -> &[(K, V)] {
        &self.storage
    }
}

impl<K: Clone, V: Clone> Clone for LinearMap<K, V> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
        }
    }

    #[inline]
    fn clone_from(&mut self, other: &Self) {
        self.storage.clone_from(&other.storage);
    }
}

impl<K: Eq + Debug, V: Debug> Debug for LinearMap<K, V> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self).finish()
    }
}

impl<K: Eq, V> Default for LinearMap<K, V> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq, V> Extend<(K, V)> for LinearMap<K, V> {
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, key_values: I) {
        for (key, value) in key_values {
            self.insert(key, value);
        }
    }
}

impl<K: Eq, V> iter::FromIterator<(K, V)> for LinearMap<K, V> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = (K, V)>>(key_values: I) -> Self {
        let mut map = Self::new();
        map.extend(key_values);
        map
    }
}

impl<'a, K: Eq + Borrow<Q>, V, Q: ?Sized + Eq> ops::Index<&'a Q> for LinearMap<K, V> {
    type Output = V;

    #[inline]
    fn index(&self, key: &'a Q) -> &V {
        self.get(key).expect("key not found")
    }
}

impl<K: Eq, V: PartialEq> PartialEq for LinearMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for (key, value) in self {
            if other.get(key) != Some(value) {
                return false;
            }
        }

        true
    }
}

impl<K: Eq, V: Eq> Eq for LinearMap<K, V> {}

impl<K, V> PartialOrd for LinearMap<K, V>
where
    K: Eq + Ord,
    V: Ord,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<K, V> Ord for LinearMap<K, V>
where
    K: Eq + Ord,
    V: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        // first compare lengths, if equal, we have to sort and do lexographical ordering...
        match self.len().cmp(&other.len()) {
            Ordering::Less => return Ordering::Less,
            Ordering::Greater => return Ordering::Greater,
            Ordering::Equal => (),
        }
        let mut self_seq = self.storage.iter().collect::<Vec<_>>();
        self_seq.sort_unstable_by_key(|(k, _v)| k); // unstable ok because no keys should be equal
        let mut other_seq = other.storage.iter().collect::<Vec<_>>();
        other_seq.sort_unstable_by_key(|(k, _v)| k);
        self_seq.into_iter().cmp(other_seq)
    }
}

impl<K, V> Hash for LinearMap<K, V>
where
    K: Hash,
    V: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        // although this is in theory not fully cryptographically secure, we don't care about that here.
        // also, since we are using a deterministic hasher, we don't have to worry about differences between maps' hashers.
        // Thus we can use xor as an order-independent hash-combination function. Thus, this
        // will create the same hash as long as two graphs have the same element (k, v) pairs, regardless of order.
        // this also satisfies the requirement that Eq and Hash have the same semantics, as HashMap's PartialEq and Eq
        // also have this semantics that as long as the elements are equal the maps are equal.
        let mut hash = 0u64;
        for elt in self.storage.iter() {
            let elt_hash = STATIC_RANDOM_STATE.hash_one(elt);
            hash ^= elt_hash;
        }
        state.write_u64(hash);
    }
}

impl<K: Eq, V> From<LinearMap<K, V>> for Vec<(K, V)> {
    #[inline]
    fn from(other: LinearMap<K, V>) -> Self {
        other.storage
    }
}

impl<K: Eq, V> From<Vec<(K, V)>> for LinearMap<K, V> {
    #[inline]
    fn from(other: Vec<(K, V)>) -> Self {
        other.into_iter().collect()
    }
}

impl<K: Eq + Clone, V: Clone, const N: usize> From<[(K, V); N]> for LinearMap<K, V> {
    #[inline]
    fn from(arr: [(K, V); N]) -> Self {
        Self::from(arr.to_vec())
    }
}

/// A view into a single occupied location in a `LinearMap`.
///
/// See [`LinearMap::entry`] for details.
#[allow(missing_debug_implementations)]
pub struct OccupiedEntry<'a, K, V> {
    map: &'a mut LinearMap<K, V>,
    index: usize,
}

/// A view into a single vacant location in a `LinearMap`.
///
/// See [`LinearMap::entry`] for details.
#[allow(missing_debug_implementations)]
pub struct VacantEntry<'a, K, V> {
    map: &'a mut LinearMap<K, V>,
    key: K,
}

/// A view into a single entry in a `LinearMap`.
///
/// See [`LinearMap::entry`] for details.
#[allow(missing_debug_implementations)]
pub enum Entry<'a, K, V> {
    /// An occupied entry.
    Occupied(OccupiedEntry<'a, K, V>),

    /// A vacant entry.
    Vacant(VacantEntry<'a, K, V>),
}

impl<'a, K, V> Entry<'a, K, V> {
    /// Ensures that the entry is occupied by inserting the given value if it is vacant.
    ///
    /// Returns a mutable reference to the entry's value.
    #[inline]
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(default),
        }
    }

    /// Ensures that the entry is occupied by inserting the the result of the given function if it
    /// is vacant.
    ///
    /// Returns a mutable reference to the entry's value.
    #[inline]
    pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V {
        match self {
            Occupied(entry) => entry.into_mut(),
            Vacant(entry) => entry.insert(default()),
        }
    }

    /// Provides in-place mutable access to an occupied entry before any
    /// potential inserts into the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_map::LinearMap;
    ///
    /// let mut map: LinearMap<&str, u32> = LinearMap::new();
    ///
    /// map.entry("poneyland")
    ///    .and_modify(|e| { *e += 1 })
    ///    .or_insert(42);
    /// assert_eq!(map["poneyland"], 42);
    ///
    /// map.entry("poneyland")
    ///    .and_modify(|e| { *e += 1 })
    ///    .or_insert(42);
    /// assert_eq!(map["poneyland"], 43);
    /// ```
    #[inline]
    pub fn and_modify<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut V),
    {
        match self {
            Occupied(mut entry) => {
                f(entry.get_mut());
                Occupied(entry)
            }
            Vacant(entry) => Vacant(entry),
        }
    }
}

impl<'a, K, V> OccupiedEntry<'a, K, V> {
    /// Returns a reference to the entry's value.
    #[inline]
    pub fn get(&self) -> &V {
        &self.map.storage[self.index].1
    }

    /// Returns a mutable reference to the entry's value.
    #[inline]
    pub fn get_mut(&mut self) -> &mut V {
        &mut self.map.storage[self.index].1
    }

    /// Returns a mutable reference to the entry's value with the same lifetime as the map.
    #[inline]
    pub fn into_mut(self) -> &'a mut V {
        &mut self.map.storage[self.index].1
    }

    /// Replaces the entry's value with the given one and returns the previous value.
    #[inline]
    pub fn insert(&mut self, value: V) -> V {
        mem::replace(self.get_mut(), value)
    }

    /// Removes (by swap) the entry from the map and returns its value.
    ///
    /// If you need to retain the order of elements in the map, use `shift_remove` instead.
    #[inline]
    pub fn remove(self) -> V {
        self.map.storage.swap_remove(self.index).1
    }

    /// Removes the entry from the map by shifting all subsequent elements down and returns its value.
    ///
    /// If you don't care about retaining the order of elements in the map, use `remove` instead.
    #[inline]
    pub fn shift_remove(self) -> V {
        self.map.storage.remove(self.index).1
    }
}

impl<'a, K, V: Default> Entry<'a, K, V> {
    /// If `self` is occupied, returns a mutable reference to the contained value. If it is vacant,
    /// inserts the default value and returns a mutable reference to that new contained value.
    #[inline]
    pub fn or_default(self) -> &'a mut V {
        match self {
            Self::Occupied(e) => e.into_mut(),
            Self::Vacant(e) => e.insert(V::default()),
        }
    }
}

impl<'a, K, V> VacantEntry<'a, K, V> {
    /// Inserts the entry into the map with the given value.
    ///
    /// Returns a mutable reference to the entry's value with the same lifetime as the map.
    #[inline]
    pub fn insert(self, value: V) -> &'a mut V {
        self.map.storage.push((self.key, value));
        &mut self.map.storage.last_mut().unwrap().1
    }
}

/// A consuming iterator over a `LinearMap`.
///
/// Acquire through [`IntoIterator`](struct.LinearMap.html#method.into_iter).
#[allow(missing_debug_implementations)]
pub struct IntoIter<K, V> {
    iter: vec::IntoIter<(K, V)>,
}

impl<K, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);

    #[inline]
    fn next(&mut self) -> Option<(K, V)> {
        self.iter.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<K, V> DoubleEndedIterator for IntoIter<K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<(K, V)> {
        self.iter.next_back()
    }
}

impl<K, V> ExactSizeIterator for IntoIter<K, V> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

/// A draining iterator over a `LinearMap`.
///
/// See [`LinearMap::drain`] for details.
#[allow(missing_debug_implementations)]
pub struct Drain<'a, K, V> {
    iter: vec::Drain<'a, (K, V)>,
}

/// An iterator yielding references to a `LinearMap`'s keys and their corresponding values.
///
/// See [`LinearMap::iter`] for details.
#[allow(missing_debug_implementations)]
pub struct Iter<'a, K, V> {
    iter: slice::Iter<'a, (K, V)>,
}

/// An iterator yielding references to a `LinearMap`'s keys and mutable references to their
/// corresponding values.
///
/// See [`LinearMap::iter_mut`] for details.
#[allow(missing_debug_implementations)]
pub struct IterMut<'a, K, V> {
    iter: slice::IterMut<'a, (K, V)>,
}

/// An iterator yielding mutable references to a `LinearMap`'s keys and values. Modifying keys
/// could potentially create logic errors, so be careful when using it!
///
/// See [`LinearMap::iter_full_unchecked_mut`] for details.
#[allow(missing_debug_implementations)]
pub struct IterFullUncheckedMut<'a, K, V> {
    iter: slice::IterMut<'a, (K, V)>,
}

/// An iterator yielding references to a `LinearMap`'s keys.
///
/// See [`LinearMap::keys`] for details.
#[allow(missing_debug_implementations)]
pub struct Keys<'a, K, V> {
    iter: Iter<'a, K, V>,
}

/// An iterator yielding references to a `LinearMap`'s values.
///
/// See [`LinearMap::values`] for details.
#[allow(missing_debug_implementations)]
pub struct Values<'a, K, V> {
    iter: Iter<'a, K, V>,
}

/// An iterator yielding mutable references to a `LinearMap`'s values.
///
/// See [`LinearMap::values_mut`] for details.
#[allow(missing_debug_implementations)]
pub struct ValuesMut<'a, K, V> {
    iter: slice::IterMut<'a, (K, V)>,
}

macro_rules! impl_iter {
    ($typ:ty, $item:ty, $map:expr) => {
        impl<'a, K, V> Iterator for $typ {
            type Item = $item;

            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                self.iter.next().map($map)
            }

            #[inline]
            fn size_hint(&self) -> (usize, Option<usize>) {
                self.iter.size_hint()
            }
        }

        impl<'a, K, V> DoubleEndedIterator for $typ {
            #[inline]
            fn next_back(&mut self) -> Option<Self::Item> {
                self.iter.next_back().map($map)
            }
        }

        impl<'a, K, V> ExactSizeIterator for $typ {
            #[inline]
            fn len(&self) -> usize {
                self.iter.len()
            }
        }
    };
}
impl_iter! {Drain<'a,K,V>,  (K,V),  |e| e }
impl_iter! {Iter<'a,K,V>,  (&'a K, &'a V),  |e| (&e.0, &e.1) }
impl_iter! {IterMut<'a,K,V>,  (&'a K, &'a mut V),  |e| (&e.0, &mut e.1) }
impl_iter! {IterFullUncheckedMut<'a,K,V>,  (&'a mut K, &'a mut V),  |e| (&mut e.0, &mut e.1) }
impl_iter! {Keys<'a,K,V>,  &'a K,  |e| e.0 }
impl_iter! {Values<'a,K,V>,  &'a V,  |e| e.1 }
impl_iter! {ValuesMut<'a,K,V>,  &'a mut V,  |e| &mut e.1 }

impl<'a, K, V> Clone for Iter<'a, K, V> {
    fn clone(&self) -> Self {
        Iter {
            iter: self.iter.clone(),
        }
    }
}

impl<'a, K, V> Clone for Keys<'a, K, V> {
    #[inline]
    fn clone(&self) -> Self {
        Keys {
            iter: self.iter.clone(),
        }
    }
}

impl<'a, K, V> Clone for Values<'a, K, V> {
    #[inline]
    fn clone(&self) -> Self {
        Values {
            iter: self.iter.clone(),
        }
    }
}

impl<K: Eq, V> IntoIterator for LinearMap<K, V> {
    type Item = (K, V);
    type IntoIter = IntoIter<K, V>;

    #[inline]
    fn into_iter(self) -> IntoIter<K, V> {
        IntoIter {
            iter: self.storage.into_iter(),
        }
    }
}

impl<'a, K: Eq, V> IntoIterator for &'a LinearMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    #[inline]
    fn into_iter(self) -> Iter<'a, K, V> {
        self.iter()
    }
}

impl<'a, K: Eq, V> IntoIterator for &'a mut LinearMap<K, V> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;

    #[inline]
    fn into_iter(self) -> IterMut<'a, K, V> {
        self.iter_mut()
    }
}

#[allow(dead_code)]
fn assert_covariance() {
    fn a<'a, K, V>(x: LinearMap<&'static K, &'static V>) -> LinearMap<&'a K, &'a V> {
        x
    }

    fn b<'a, K, V>(x: IntoIter<&'static K, &'static V>) -> IntoIter<&'a K, &'a V> {
        x
    }

    fn c<'i, 'a, K, V>(x: Iter<'i, &'static K, &'static V>) -> Iter<'i, &'a K, &'a V> {
        x
    }

    fn d<'i, 'a, K, V>(x: Keys<'i, &'static K, &'static V>) -> Keys<'i, &'a K, &'a V> {
        x
    }

    fn e<'i, 'a, K, V>(x: Values<'i, &'static K, &'static V>) -> Values<'i, &'a K, &'a V> {
        x
    }
}

/// You can use this with the `#[serde(with = "module")]` [field attribute] to make an [`LinearMap`]
/// serialize/deserialize as a sequence of `(k, v)` elements, rather than as a map in the serde data model.
/// This can be helpful when serializing to JSON because true JSON maps are only allowed to be keyed by strings,
/// and thus the key types of Rust maps serialized to JSON maps are quite limited. Worse, this limitation only shows up
/// as a runtime panic! With this adapter to serialize as sequence, you should be able to use any key type that impls `Serialize`.
///
/// [field attribute]: https://serde.rs/field-attrs.html#with
#[cfg(feature = "serde")]
pub mod serde_as_seq {
    use serde::Deserialize;
    use serde::Serialize;

    use super::*;

    /// You can use this with the `#[serde(serialize_with = "function")]` [field attribute] to make an [`LinearMap`]
    /// serialize as a sequence of `(k, v)` pairs, rather than a map in the serde data model. See the module-level docs for more.
    ///
    /// [field attribute]: https://serde.rs/field-attrs.html#serialize_with
    #[inline]
    pub fn serialize<K, V, S>(map: &LinearMap<K, V>, serializer: S) -> Result<S::Ok, S::Error>
    where
        K: serde::Serialize + Hash + Eq,
        V: serde::Serialize,
        S: serde::Serializer,
    {
        map.storage.serialize(serializer)
    }

    /// You can use this with the `#[serde(deserialize_with = "function")]` [field attribute] to deserialize a [`LinearMap`]
    /// from a sequence of `(k, v)` pairs, rather than a map in the serde data model. See the module-level docs for more.
    ///
    /// [field attribute]: https://serde.rs/field-attrs.html#serialize_with
    #[inline]
    pub fn deserialize<'de, K, V, D>(deserializer: D) -> Result<LinearMap<K, V>, D::Error>
    where
        K: serde::Deserialize<'de> + Eq,
        V: serde::Deserialize<'de>,
        D: serde::Deserializer<'de>,
    {
        let storage = Vec::<(K, V)>::deserialize(deserializer)?;
        Ok(LinearMap { storage })
    }
}

#[cfg(feature = "serde")]
impl<K, V> serde::Serialize for LinearMap<K, V>
where
    K: serde::Serialize + Eq,
    V: serde::Serialize,
{
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        let mut ser_map = serializer.serialize_map(Some(self.len()))?;
        for (k, v) in self {
            ser_map.serialize_entry(k, v)?;
        }
        ser_map.end()
    }
}

#[cfg(feature = "serde")]
impl<'de, K, V> serde::Deserialize<'de> for LinearMap<K, V>
where
    K: serde::Deserialize<'de> + Eq,
    V: serde::Deserialize<'de>,
{
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use core::marker::PhantomData;
        use serde::de::MapAccess;
        use serde::de::Visitor;

        struct LinearMapVisitor<K, V>(PhantomData<(K, V)>);

        impl<'de, K, V> Visitor<'de> for LinearMapVisitor<K, V>
        where
            K: serde::Deserialize<'de> + Eq,
            V: serde::Deserialize<'de>,
        {
            type Value = LinearMap<K, V>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("a map of (k, v) pairs")
            }

            fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut map = LinearMap::with_capacity(access.size_hint().unwrap_or(0));

                while let Some((k, v)) = access.next_entry::<K, V>()? {
                    map.insert(k, v);
                }

                Ok(map)
            }
        }

        deserializer.deserialize_map(LinearMapVisitor::<K, V>(PhantomData))
    }
}

#[cfg(feature = "speedy")]
impl<'a, C, K, V> speedy::Readable<'a, C> for LinearMap<K, V>
where
    C: speedy::Context,
    K: speedy::Readable<'a, C>,
    V: speedy::Readable<'a, C>,
{
    #[inline]
    fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let storage = Vec::<(K, V)>::read_from(reader)?;
        Ok(Self { storage })
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        Vec::<(K, V)>::minimum_bytes_needed()
    }
}

#[cfg(feature = "speedy")]
impl<C, K, V> speedy::Writable<C> for LinearMap<K, V>
where
    C: speedy::Context,
    K: speedy::Writable<C>,
    V: speedy::Writable<C>,
{
    #[inline]
    fn write_to<T: ?Sized + speedy::Writer<C>>(
        &self,
        writer: &mut T,
    ) -> Result<(), <C as speedy::Context>::Error> {
        self.storage.write_to(writer)
    }

    #[inline]
    fn bytes_needed(&self) -> Result<usize, <C as speedy::Context>::Error> {
        self.storage.bytes_needed()
    }
}
