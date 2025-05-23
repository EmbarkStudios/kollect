use indexmap::Equivalent;

use core::cmp::Ordering;
use core::fmt;
use core::hash::BuildHasher;
use core::hash::Hash;
use core::hash::Hasher;
use core::ops::RangeBounds;

use indexmap::IndexMap;

pub use indexmap::map::Drain;
pub use indexmap::map::Entry;
pub use indexmap::map::IntoIter;
pub use indexmap::map::IntoKeys;
pub use indexmap::map::IntoValues;
pub use indexmap::map::Iter;
pub use indexmap::map::IterMut;
pub use indexmap::map::Keys;
pub use indexmap::map::Values;
pub use indexmap::map::ValuesMut;

/// A key-to-value map that has a specified order of contained elements.
///
/// It is a good choice to use this map if you need to lookup elements by index, maintain a stable order,
/// or iterate over the contained elements more frequently than looking them up by key even if you don't care about order.
/// If you plan to lookup elements by key much more frequently than iterating the contained elements, and you do not care about
/// order, then think about using [`UnorderedMap`] instead.
///
/// The order is *not* automatically maintained, thus you can move element order as you please, or sort
/// with the various sorting functions.
///
/// This is a wrapper around [`indexmap::IndexMap`] which implements various traits in ways that fit
/// our use cases better than the choices `indexmap` made. If you really need to access the wrapped map directly,
/// you can do so with the `inner`, `inner_mut` or `into_inner` methods, but be careful as the semantics of the traits
/// mentioned below may be different.
///
/// Implements `PartialEq`, `Eq`, and `Hash` such that two maps are equal and hash to the same value if they have
/// the same `(k, v)` element pairs ***and*** the same order of those elements.
///
/// Implements `Ord` with [lexographical] ordering between element pairs.
///
/// [`UnorderedMap`]: crate::UnorderedMap
/// [lexographical]: core::cmp::Ord#lexographical-comparison
pub struct OrderedMap<K, V, S = crate::BuildHasher> {
    pub(crate) inner: IndexMap<K, V, S>,
}

impl<K, V> OrderedMap<K, V, crate::BuildHasher> {
    /// [`IndexMap::new`] but using an [`ahash`] hasher.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// [`IndexMap::with_capacity`] but using an [`ahash`] hasher.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, crate::BuildHasher::default())
    }
}

impl<K, V, S> OrderedMap<K, V, S> {
    /// See [`IndexMap::with_capacity_and_hasher`]
    #[inline]
    pub fn with_capacity_and_hasher(capacity: usize, hasher: S) -> Self {
        Self {
            inner: IndexMap::<K, V, S>::with_capacity_and_hasher(capacity, hasher),
        }
    }

    /// See [`IndexMap::len`]
    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// See [`IndexMap::capacity`]
    #[inline]
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// See [`IndexMap::keys`]
    #[inline]
    pub fn keys(&self) -> Keys<'_, K, V> {
        self.inner.keys()
    }

    /// See [`IndexMap::into_keys`]
    #[inline]
    pub fn into_keys(self) -> IntoKeys<K, V> {
        self.inner.into_keys()
    }

    /// See [`IndexMap::values`]
    #[inline]
    pub fn values(&self) -> Values<'_, K, V> {
        self.inner.values()
    }

    /// See [`IndexMap::values_mut`]
    #[inline]
    pub fn values_mut(&mut self) -> ValuesMut<'_, K, V> {
        self.inner.values_mut()
    }

    /// See [`IndexMap::into_values`]
    #[inline]
    pub fn into_values(self) -> IntoValues<K, V> {
        self.inner.into_values()
    }

    /// See [`IndexMap::iter`]
    #[inline]
    pub fn iter(&self) -> Iter<'_, K, V> {
        self.inner.iter()
    }

    /// See [`IndexMap::iter_mut`]
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        self.inner.iter_mut()
    }

    /// See [`IndexMap::is_empty`]
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// See [`IndexMap::drain`]
    #[inline]
    pub fn drain<R>(&mut self, range: R) -> Drain<'_, K, V>
    where
        R: RangeBounds<usize>,
    {
        self.inner.drain(range)
    }

    /// See [`IndexMap::clear`]
    ///
    /// Note that this method does not shrink the underlying allocation (keeps capacity the same) and is `O(capacity)`.
    /// Thus repeated calls to `clear_no_shrink` on a map that is far under-occupied may be unexpectedly expensive. Consider using
    /// [`clear_and_shrink`] or [`clear_and_shrink_to`] to shrink the underlying allocation when appropriate when clearing.
    ///
    /// [`clear_and_shrink`]: OrderedMap::clear_and_shrink
    /// [`clear_and_shrink_to`]: OrderedMap::clear_and_shrink_to
    #[inline]
    pub fn clear_no_shrink(&mut self) {
        self.inner.clear();
    }

    /// See [`IndexMap::hasher`]
    #[inline]
    pub fn hasher(&self) -> &S {
        self.inner.hasher()
    }

    /// Access the wrapped [`IndexMap`].
    #[inline]
    pub fn inner(&self) -> &IndexMap<K, V, S> {
        &self.inner
    }

    /// Access the wrapped [`IndexMap`] mutably.
    #[inline]
    pub fn inner_mut(&mut self) -> &mut IndexMap<K, V, S> {
        &mut self.inner
    }

    /// Extract the wrapped [`IndexMap`].
    #[inline]
    pub fn into_inner(self) -> IndexMap<K, V, S> {
        self.inner
    }
}

impl<K, V, const N: usize> From<[(K, V); N]> for OrderedMap<K, V, crate::BuildHasher>
where
    K: Hash + Eq,
{
    fn from(arr: [(K, V); N]) -> Self {
        Self {
            inner: IndexMap::<K, V, crate::BuildHasher>::from_iter(arr),
        }
    }
}

impl<K, V, S> OrderedMap<K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    /// See [`IndexMap::retain`]
    #[inline]
    pub fn retain<F>(&mut self, keep: F)
    where
        F: FnMut(&K, &mut V) -> bool,
    {
        self.inner.retain(keep);
    }

    /// See [`IndexMap::reserve`]
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    /// See [`IndexMap::shrink_to_fit`]
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.inner.shrink_to_fit();
    }

    /// See [`IndexMap::shrink_to`]
    #[inline]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.inner.shrink_to(min_capacity);
    }

    /// Clears the map, removing all key-value pairs.
    ///
    /// Note that this shrinks the capacity of the map based on a basic heuristic. See [`clear_and_shrink`] for more details, which this
    /// method redirects to internally.
    ///
    /// [`clear_and_shrink`]: OrderedMap::clear_and_shrink
    #[inline]
    pub fn clear(&mut self) {
        self.clear_and_shrink();
    }

    /// Clears and shrinks the capacity of the map on a basic heuristic. If you have a more specific heuristic, see [`clear_and_shrink_to`].
    ///
    /// If the map previously had > 128 element capacity, shrinks to whichever is larger between 128 and 110% of the previous length of the map
    /// in an effort to reduce reallocation for repeated use-and-clear on similar numbers of items. If the map had <= 128 element capacity, no shrink happens.
    ///
    /// [`clear_and_shrink_to`]: OrderedMap::clear_and_shrink_to
    #[inline]
    pub fn clear_and_shrink(&mut self) {
        if self.capacity() > 128 {
            let new_cap = 128usize.max((self.len() as f64 * 1.1) as usize);
            self.clear_and_shrink_to(new_cap);
        } else {
            self.clear_no_shrink();
        }
    }

    /// Clears and shrinks the capacity of the map to the given capacity.
    #[inline]
    pub fn clear_and_shrink_to(&mut self, capacity: usize) {
        self.clear_no_shrink();
        self.shrink_to(capacity);
    }

    /// See [`IndexMap::entry`]
    #[inline]
    pub fn entry(&mut self, key: K) -> Entry<'_, K, V> {
        self.inner.entry(key)
    }

    /// See [`IndexMap::get`]
    #[inline]
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.get(k)
    }

    /// See [`IndexMap::get_key_value`]
    #[inline]
    pub fn get_key_value<Q>(&self, k: &Q) -> Option<(&K, &V)>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.get_key_value(k)
    }

    /// See [`IndexMap::get_full`]
    pub fn get_full<Q>(&self, key: &Q) -> Option<(usize, &K, &V)>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.get_full(key)
    }

    /// See [`IndexMap::get_mut`]
    #[inline]
    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.get_mut(k)
    }

    /// See [`IndexMap::get_full`]
    pub fn get_full_mut<Q>(&mut self, key: &Q) -> Option<(usize, &K, &mut V)>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.get_full_mut(key)
    }
    /// See [`IndexMap::contains_key`]
    #[inline]
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.contains_key(key)
    }

    /// See [`IndexMap::get_index_of`]
    #[inline]
    pub fn get_index_of<Q>(&self, key: &Q) -> Option<usize>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.get_index_of(key)
    }

    /// See [`IndexMap::insert`]
    #[inline]
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.inner.insert(k, v)
    }

    /// Same as [`OrderedMap::insert`] but will panic if the key inserted
    /// is not unique
    #[inline]
    pub fn insert_unique(&mut self, k: K, v: V) {
        if self.inner.insert(k, v).is_some() {
            crate::panic_key_already_existed();
        }
    }

    /// See [`IndexMap::insert_full`]
    #[inline]
    pub fn insert_full(&mut self, key: K, value: V) -> (usize, Option<V>) {
        self.inner.insert_full(key, value)
    }

    /// See [`IndexMap::remove`]
    ///
    /// **NOTE:** This is equivalent to `.swap_remove(key)`, if you need to
    /// preserve the order of the keys in the map, use `.shift_remove_entry(key)`
    /// instead.
    #[inline]
    pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.swap_remove(k)
    }

    /// See [`IndexMap::remove_entry`]
    ///
    /// **NOTE:** This is equivalent to `.swap_remove_entry(key)`, if you need to
    /// preserve the order of the keys in the map, use `.shift_remove_entry(key)`
    /// instead.
    #[inline]
    pub fn remove_entry<Q>(&mut self, key: &Q) -> Option<(K, V)>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.swap_remove_entry(key)
    }

    /// See [`IndexMap::swap_remove`]
    #[inline]
    pub fn swap_remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.swap_remove(key)
    }

    /// See [`IndexMap::swap_remove_entry`]
    #[inline]
    pub fn swap_remove_entry<Q>(&mut self, key: &Q) -> Option<(K, V)>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.swap_remove_entry(key)
    }

    /// See [`IndexMap::swap_remove_full`]
    #[inline]
    pub fn swap_remove_full<Q>(&mut self, key: &Q) -> Option<(usize, K, V)>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.swap_remove_full(key)
    }

    /// See [`IndexMap::shift_remove`]
    #[inline]
    pub fn shift_remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.shift_remove(key)
    }

    /// See [`IndexMap::shift_remove_entry`]
    #[inline]
    pub fn shift_remove_entry<Q>(&mut self, key: &Q) -> Option<(K, V)>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.shift_remove_entry(key)
    }

    /// See [`IndexMap::shift_remove_full`]
    #[inline]
    pub fn shift_remove_full<Q>(&mut self, key: &Q) -> Option<(usize, K, V)>
    where
        Q: ?Sized + Hash + Equivalent<K>,
    {
        self.inner.shift_remove_full(key)
    }

    /// See [`IndexMap::pop`]
    #[inline]
    pub fn pop(&mut self) -> Option<(K, V)> {
        self.inner.pop()
    }

    /// See [`IndexMap::sort_keys`]
    #[inline]
    pub fn sort_keys(&mut self)
    where
        K: Ord,
    {
        self.inner.sort_keys();
    }

    /// See [`IndexMap::sort_by`]
    #[inline]
    pub fn sort_by<F>(&mut self, cmp: F)
    where
        F: FnMut(&K, &V, &K, &V) -> Ordering,
    {
        self.inner.sort_by(cmp);
    }

    /// See [`IndexMap::sorted_by`]
    #[inline]
    pub fn sorted_by<F>(self, cmp: F) -> IntoIter<K, V>
    where
        F: FnMut(&K, &V, &K, &V) -> Ordering,
    {
        self.inner.sorted_by(cmp)
    }

    /// See [`IndexMap::sort_unstable_keys`]
    #[inline]
    pub fn sort_unstable_keys(&mut self)
    where
        K: Ord,
    {
        self.inner.sort_unstable_keys();
    }

    /// See [`IndexMap::sort_unstable_by`]
    #[inline]
    pub fn sort_unstable_by<F>(&mut self, cmp: F)
    where
        F: FnMut(&K, &V, &K, &V) -> Ordering,
    {
        self.inner.sort_unstable_by(cmp);
    }

    /// See [`IndexMap::sorted_unstable_by`]
    #[inline]
    pub fn sorted_unstable_by<F>(self, cmp: F) -> IntoIter<K, V>
    where
        F: FnMut(&K, &V, &K, &V) -> Ordering,
    {
        self.inner.sorted_unstable_by(cmp)
    }

    /// See [`IndexMap::reverse`]
    #[inline]
    pub fn reverse(&mut self) {
        self.inner.reverse();
    }
}

impl<K, V, S> OrderedMap<K, V, S> {
    /// See [`IndexMap::get_index`]
    #[inline]
    pub fn get_index(&self, index: usize) -> Option<(&K, &V)> {
        self.inner.get_index(index)
    }

    /// See [`IndexMap::get_index_mut`]
    #[inline]
    pub fn get_index_mut(&mut self, index: usize) -> Option<(&K, &mut V)> {
        self.inner.get_index_mut(index)
    }

    /// See [`IndexMap::first`]
    #[inline]
    pub fn first(&self) -> Option<(&K, &V)> {
        self.inner.first()
    }

    /// See [`IndexMap::first_mut`]
    #[inline]
    pub fn first_mut(&mut self) -> Option<(&K, &mut V)> {
        self.inner.first_mut()
    }

    /// See [`IndexMap::last`]
    #[inline]
    pub fn last(&self) -> Option<(&K, &V)> {
        self.inner.last()
    }

    /// See [`IndexMap::last_mut`]
    #[inline]
    pub fn last_mut(&mut self) -> Option<(&K, &mut V)> {
        self.inner.last_mut()
    }

    /// See [`IndexMap::swap_remove_index`]
    #[inline]
    pub fn swap_remove_index(&mut self, index: usize) -> Option<(K, V)> {
        self.inner.swap_remove_index(index)
    }

    /// See [`IndexMap::shift_remove_index`]
    #[inline]
    pub fn shift_remove_index(&mut self, index: usize) -> Option<(K, V)> {
        self.inner.shift_remove_index(index)
    }

    /// See [`IndexMap::move_index`]
    #[inline]
    pub fn move_index(&mut self, from: usize, to: usize) {
        self.inner.move_index(from, to);
    }

    /// See [`IndexMap::swap_indices`]
    #[inline]
    pub fn swap_indices(&mut self, a: usize, b: usize) {
        self.inner.swap_indices(a, b);
    }
}

impl<K, V, S> Clone for OrderedMap<K, V, S>
where
    K: Clone,
    V: Clone,
    S: Clone,
{
    #[inline]
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }

    #[inline]
    fn clone_from(&mut self, other: &Self) {
        self.inner.clone_from(&other.inner);
    }
}

impl<K, V, S> Default for OrderedMap<K, V, S>
where
    S: BuildHasher + Default,
{
    #[inline]
    fn default() -> Self {
        Self {
            inner: IndexMap::with_hasher(S::default()),
        }
    }
}

impl<K, V, S> fmt::Debug for OrderedMap<K, V, S>
where
    K: fmt::Debug,
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OrderedMap")
            .field("inner", &self.inner)
            .finish()
    }
}

impl<K, V, S> PartialEq for OrderedMap<K, V, S>
where
    K: Eq + Hash,
    V: PartialEq,
    S: BuildHasher,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        // lexographical equality, meaning all elements must be in the same order and be equal.
        // short circuit as soon as there's disagreement.
        self.inner
            .iter()
            .zip(other.inner.iter())
            .all(|(self_elt, other_elt)| self_elt == other_elt)
    }
}

impl<K, V, S> Eq for OrderedMap<K, V, S>
where
    K: Eq + Hash,
    V: Eq,
    S: BuildHasher,
{
}

impl<K, V, S> PartialOrd for OrderedMap<K, V, S>
where
    K: Eq + Hash + Ord,
    V: Ord,
    S: BuildHasher,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<K, V, S> Ord for OrderedMap<K, V, S>
where
    K: Eq + Hash + Ord,
    V: Ord,
    S: BuildHasher,
{
    fn cmp(&self, other: &Self) -> Ordering {
        // first compare lengths, if equal, we do lexographical ordering...
        match self.len().cmp(&other.len()) {
            Ordering::Less => return Ordering::Less,
            Ordering::Greater => return Ordering::Greater,
            Ordering::Equal => (),
        }
        self.iter().cmp(other.iter())
    }
}

impl<K, V, S> Hash for OrderedMap<K, V, S>
where
    K: Hash,
    V: Hash,
    S: BuildHasher,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        for elt in self.inner.iter() {
            elt.hash(state);
        }
    }
}

impl<K, V, S> FromIterator<(K, V)> for OrderedMap<K, V, S>
where
    K: Eq + Hash,
    S: BuildHasher + Default,
{
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self {
            inner: IndexMap::<K, V, S>::from_iter(iter),
        }
    }
}

impl<K, V, S> IntoIterator for OrderedMap<K, V, S> {
    type Item = (K, V);
    type IntoIter = indexmap::map::IntoIter<K, V>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a, K, V, S> IntoIterator for &'a OrderedMap<K, V, S> {
    type Item = (&'a K, &'a V);
    type IntoIter = indexmap::map::Iter<'a, K, V>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<'a, K, V, S> IntoIterator for &'a mut OrderedMap<K, V, S> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = indexmap::map::IterMut<'a, K, V>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter_mut()
    }
}

impl<K, V, S> Extend<(K, V)> for OrderedMap<K, V, S>
where
    K: Eq + Hash,
    S: BuildHasher,
{
    #[inline]
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        self.inner.extend(iter);
    }
}

impl<'a, K, V, S> Extend<(&'a K, &'a V)> for OrderedMap<K, V, S>
where
    K: Eq + Hash + Copy,
    V: Copy,
    S: BuildHasher,
{
    #[inline]
    fn extend<T: IntoIterator<Item = (&'a K, &'a V)>>(&mut self, iter: T) {
        self.inner.extend(iter);
    }
}

/// You can use this with the `#[serde(with = "module")]` [field attribute] to make an [`OrderedMap`]
/// serialize/deserialize as a native map in the serde data model rather than a sequence of `(k, v)` pairs.
/// We choose to serialize as sequences by default because true JSON maps are only allowed to be keyed by strings,
/// and thus the key types of Rust maps serialized to JSON maps are quite limited. Worse, this limitation only shows up
/// as a runtime panic! If you want to defeat this behavior and actually serialize as a native map, use this adapter.
///
/// [field attribute]: https://serde.rs/field-attrs.html#with
#[cfg(feature = "serde")]
pub mod serde_as_map {

    use super::*;

    /// You can use this with the `#[serde(serialize_with = "function")]` [field attribute] to make an [`OrderedMap`]
    /// serialize as a map in the serde data model rather than a sequence of `(k, v)` pairs. See the module-level docs for more.
    ///
    /// [field attribute]: https://serde.rs/field-attrs.html#serialize_with
    pub fn serialize<K, V, RS, S>(
        map: &OrderedMap<K, V, RS>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        K: serde::Serialize + Hash + Eq,
        V: serde::Serialize,
        RS: BuildHasher,
        S: serde::Serializer,
    {
        use serde::ser::Serialize;
        map.inner.serialize(serializer)
    }

    /// You can use this with the `#[serde(deserialize_with = "function")]` [field attribute] to deserialize a [`OrderedMap`]
    /// from the native serde data model map type rather than a sequence of `(k, v)` pairs. See the module-level docs for more.
    ///
    /// [field attribute]: https://serde.rs/field-attrs.html#serialize_with
    pub fn deserialize<'de, K, V, S, D>(deserializer: D) -> Result<OrderedMap<K, V, S>, D::Error>
    where
        K: serde::Deserialize<'de> + Eq + Hash,
        V: serde::Deserialize<'de>,
        S: BuildHasher + Default,
        D: serde::Deserializer<'de>,
    {
        use serde::Deserialize;
        Ok(OrderedMap {
            inner: IndexMap::<K, V, S>::deserialize(deserializer)?,
        })
    }
}

#[cfg(feature = "serde")]
impl<K, V, RS> serde::Serialize for OrderedMap<K, V, RS>
where
    K: serde::Serialize + Eq + Hash,
    V: serde::Serialize,
    RS: BuildHasher + Default,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(Some(self.inner.len()))?;
        for elt in self.inner.iter() {
            seq.serialize_element(&elt)?;
        }
        seq.end()
    }
}

#[cfg(feature = "serde")]
impl<'de, K, V, S> serde::Deserialize<'de> for OrderedMap<K, V, S>
where
    K: serde::Deserialize<'de> + Eq + Hash,
    V: serde::Deserialize<'de>,
    S: BuildHasher + Default,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use core::marker::PhantomData;
        use serde::de::SeqAccess;
        use serde::de::Visitor;

        struct IndexMapSeqVisitor<K, V, S>(PhantomData<(K, V, S)>);

        impl<'de, K, V, S> Visitor<'de> for IndexMapSeqVisitor<K, V, S>
        where
            K: serde::Deserialize<'de> + Eq + Hash,
            V: serde::Deserialize<'de>,
            S: BuildHasher + Default,
        {
            type Value = IndexMap<K, V, S>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("a sequence of (k, v) pairs")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut map =
                    IndexMap::with_capacity_and_hasher(seq.size_hint().unwrap_or(0), S::default());

                while let Some((k, v)) = seq.next_element::<(K, V)>()? {
                    map.insert(k, v);
                }

                Ok(map)
            }
        }

        let map = deserializer.deserialize_seq(IndexMapSeqVisitor::<K, V, S>(PhantomData))?;
        Ok(Self { inner: map })
    }
}

#[cfg(feature = "speedy")]
impl<'a, C, K, V, S> speedy::Readable<'a, C> for OrderedMap<K, V, S>
where
    C: speedy::Context,
    K: speedy::Readable<'a, C> + Eq + Hash,
    V: speedy::Readable<'a, C>,
    S: BuildHasher + Default,
{
    fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let length = reader.read_u32()? as usize;
        let map = (0..length)
            .map(|_| -> Result<_, <C as speedy::Context>::Error> {
                let key = K::read_from(reader)?;
                let value = V::read_from(reader)?;
                Ok((key, value))
            })
            .collect::<Result<_, _>>()?;
        Ok(Self { inner: map })
    }
}

#[cfg(feature = "speedy")]
impl<C, K, V, S> speedy::Writable<C> for OrderedMap<K, V, S>
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
        writer.write_u32(self.inner.len() as u32)?;
        writer.write_collection(self.iter())
    }

    #[inline]
    fn bytes_needed(&self) -> Result<usize, C::Error> {
        crate::internal_macros::unsafe_is_length!(self.len());

        let mut count = core::mem::size_of::<u32>(); // for len
        for elt in self {
            count += elt.bytes_needed()?;
        }

        Ok(count)
    }
}
