use core::cmp::Ordering;
use core::fmt;
use core::hash::BuildHasher;
use core::hash::Hash;
use core::hash::Hasher;
use core::ops::RangeBounds;

use indexmap::Equivalent;
use indexmap::IndexSet;

pub use indexmap::set::Difference;
pub use indexmap::set::Drain;
pub use indexmap::set::Intersection;
pub use indexmap::set::IntoIter;
pub use indexmap::set::Iter;
pub use indexmap::set::SymmetricDifference;
pub use indexmap::set::Union;

/// A set that has a specified order of contained elements.
///
/// It is a good choice to use this set if you need to lookup elements by index, maintain a stable order,
/// or iterate over the contained elements more frequently than looking them up using random access (insertion or removal),
/// even if you don't care about order. If you plan to lookup elements much more frequently than iterating the contained
/// elements, and you do not care about order, then think about using [`UnorderedSet`] instead. If you plan to have a small
/// number of elements (up to a few dozen) and those elements are fast to compare and/or small, consider [`LinearSet`] instead.
///
/// The order is *not* automatically maintained, thus you can move element order as you please, or sort
/// with the various sorting functions.
///
/// This is a wrapper around [`indexmap::IndexSet`] which implements various traits in ways that fit
/// our use cases better than the choices `indexmap` made. If you really need to access the wrapped map directly,
/// you can do so with the `inner`, `inner_mut` or `into_inner` methods, but be careful as the semantics of the traits
/// mentioned below may be different.
///
/// Implements `PartialEq`, `Eq`, and `Hash` such that two maps are equal and hash to the same value if they have
/// the same elements ***and*** the same order of those elements.
///
/// Implements `Ord` with [lexographical] ordering between element pairs.
///
/// [`UnorderedSet`]: crate::UnorderedSet
/// [`LinearSet`]: crate::LinearSet
/// [lexographical]: core::cmp::Ord#lexographical-comparison
pub struct OrderedSet<T, S = ahash::RandomState> {
    pub(crate) inner: IndexSet<T, S>,
}

impl<T> OrderedSet<T, ahash::RandomState> {
    /// [`IndexSet::new`] but using an [`ahash`] hasher.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// [`IndexSet::with_capacity`] but using an [`ahash`] hasher.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, ahash::RandomState::default())
    }
}

impl<T, S> OrderedSet<T, S> {
    /// See [`IndexSet::with_capacity_and_hasher`]
    #[inline]
    pub fn with_capacity_and_hasher(capacity: usize, hasher: S) -> Self {
        Self {
            inner: IndexSet::<T, S>::with_capacity_and_hasher(capacity, hasher),
        }
    }

    /// See [`IndexSet::len`]
    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// See [`IndexSet::capacity`]
    #[inline]
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// See [`IndexSet::iter`]
    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        self.inner.iter()
    }

    /// See [`IndexSet::is_empty`]
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// See [`IndexSet::drain`]
    #[inline]
    pub fn drain<R>(&mut self, range: R) -> Drain<'_, T>
    where
        R: RangeBounds<usize>,
    {
        self.inner.drain(range)
    }

    /// See [`IndexSet::clear`]
    ///
    /// Note that this method does not shrink the underlying allocation (keeps capacity the same) and is `O(capacity)`.
    /// Thus repeated calls to `clear_no_shrink` on a map that is far under-occupied may be unexpectedly expensive. Consider using
    /// [`clear_and_shrink`] or [`clear_and_shrink_to`] to shrink the underlying allocation when appropriate when clearing.
    ///
    /// [`clear_and_shrink`]: OrderedSet::clear_and_shrink
    /// [`clear_and_shrink_to`]: OrderedSet::clear_and_shrink_to
    #[inline]
    pub fn clear_no_shrink(&mut self) {
        self.inner.clear();
    }

    /// See [`IndexSet::hasher`]
    #[inline]
    pub fn hasher(&self) -> &S {
        self.inner.hasher()
    }

    /// Access the wrapped [`IndexSet`].
    #[inline]
    pub fn inner(&self) -> &IndexSet<T, S> {
        &self.inner
    }

    /// Access the wrapped [`IndexSet`] mutably.
    #[inline]
    pub fn inner_mut(&mut self) -> &mut IndexSet<T, S> {
        &mut self.inner
    }

    /// Extract the wrapped [`IndexSet`].
    #[inline]
    pub fn into_inner(self) -> IndexSet<T, S> {
        self.inner
    }
}

impl<T, const N: usize> From<[T; N]> for OrderedSet<T, ahash::RandomState>
where
    T: Hash + Eq,
{
    fn from(arr: [T; N]) -> Self {
        Self {
            inner: IndexSet::<T, ahash::RandomState>::from_iter(arr),
        }
    }
}

impl<T, S> OrderedSet<T, S>
where
    T: Hash + Eq,
    S: BuildHasher,
{
    /// See [`IndexSet::retain`]
    #[inline]
    pub fn retain<F>(&mut self, keep: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.inner.retain(keep);
    }

    /// See [`IndexSet::reserve`]
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    /// See [`IndexSet::shrink_to_fit`]
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.inner.shrink_to_fit();
    }

    /// See [`IndexSet::shrink_to`]
    #[inline]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.inner.shrink_to(min_capacity);
    }

    /// Clears the map, removing all key-value pairs.
    ///
    /// Note that this shrinks the capacity of the map based on a basic heuristic. See [`clear_and_shrink`] for more details, which this
    /// method redirects to internally.
    ///
    /// [`clear_and_shrink`]: OrderedSet::clear_and_shrink
    #[inline]
    pub fn clear(&mut self) {
        self.clear_and_shrink();
    }

    /// Clears and shrinks the capacity of the map on a basic heuristic. If you have a more specific heuristic, see [`clear_and_shrink_to`].
    ///
    /// If the map previously had > 128 element capacity, shrinks to whichever is larger between 128 and 110% of the previous length of the map
    /// in an effort to reduce reallocation for repeated use-and-clear on similar numbers of items. If the map had <= 128 element capacity, no shrink happens.
    ///
    /// [`clear_and_shrink_to`]: OrderedSet::clear_and_shrink_to
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

    /// See [`IndexSet::insert`]
    #[inline]
    pub fn insert(&mut self, value: T) -> bool {
        self.inner.insert(value)
    }

    /// See [`IndexSet::insert_full`]
    #[inline]
    pub fn insert_full(&mut self, value: T) -> (usize, bool) {
        self.inner.insert_full(value)
    }

    /// See [`IndexSet::difference`]
    #[inline]
    pub fn difference<'a, S2>(&'a self, other: &'a IndexSet<T, S2>) -> Difference<'a, T, S2>
    where
        S2: BuildHasher,
    {
        self.inner.difference(other)
    }

    /// See [`IndexSet::symmetric_difference`]
    #[inline]
    pub fn symmetric_difference<'a, S2>(
        &'a self,
        other: &'a IndexSet<T, S2>,
    ) -> SymmetricDifference<'a, T, S, S2>
    where
        S2: BuildHasher,
    {
        self.inner.symmetric_difference(other)
    }

    /// See [`IndexSet::intersection`]
    #[inline]
    pub fn intersection<'a, S2>(&'a self, other: &'a IndexSet<T, S2>) -> Intersection<'a, T, S2>
    where
        S2: BuildHasher,
    {
        self.inner.intersection(other)
    }

    /// See [`IndexSet::union`]
    #[inline]
    pub fn union<'a, S2>(&'a self, other: &'a IndexSet<T, S2>) -> Union<'a, T, S>
    where
        S2: BuildHasher,
    {
        self.inner.union(other)
    }

    /// See [`IndexSet::contains`]
    #[inline]
    pub fn contains<Q: ?Sized>(&self, value: &Q) -> bool
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.contains(value)
    }

    /// See [`IndexSet::get`]
    #[inline]
    pub fn get<Q: ?Sized>(&self, value: &Q) -> Option<&T>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.get(value)
    }

    /// See [`IndexSet::get_full`]
    #[inline]
    pub fn get_full<Q: ?Sized>(&self, value: &Q) -> Option<(usize, &T)>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.get_full(value)
    }

    /// See [`IndexSet::get_index_of`]
    #[inline]
    pub fn get_index_of<Q: ?Sized>(&self, value: &Q) -> Option<usize>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.get_index_of(value)
    }

    /// See [`IndexSet::replace`]
    #[inline]
    pub fn replace(&mut self, value: T) -> Option<T> {
        self.inner.replace(value)
    }

    /// See [`IndexSet::replace_full`]
    #[inline]
    pub fn replace_full(&mut self, value: T) -> (usize, Option<T>) {
        self.inner.replace_full(value)
    }

    /// See [`IndexSet::remove`]
    #[inline]
    pub fn remove<Q: ?Sized>(&mut self, value: &Q) -> bool
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.remove(value)
    }

    /// See [`IndexSet::swap_remove`]
    #[inline]
    pub fn swap_remove<Q: ?Sized>(&mut self, value: &Q) -> bool
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.swap_remove(value)
    }

    /// See [`IndexSet::shift_remove`]
    #[inline]
    pub fn shift_remove<Q: ?Sized>(&mut self, value: &Q) -> bool
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.shift_remove(value)
    }

    /// See [`IndexSet::take`]
    #[inline]
    pub fn take<Q: ?Sized>(&mut self, value: &Q) -> Option<T>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.take(value)
    }

    /// See [`IndexSet::swap_take`]
    #[inline]
    pub fn swap_take<Q: ?Sized>(&mut self, value: &Q) -> Option<T>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.swap_take(value)
    }

    /// See [`IndexSet::shift_take`]
    #[inline]
    pub fn shift_take<Q: ?Sized>(&mut self, value: &Q) -> Option<T>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.shift_take(value)
    }

    /// See [`IndexSet::swap_remove_full`]
    #[inline]
    pub fn swap_remove_full<Q: ?Sized>(&mut self, value: &Q) -> Option<(usize, T)>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.swap_remove_full(value)
    }

    /// See [`IndexSet::shift_remove_full`]
    #[inline]
    pub fn shift_remove_full<Q: ?Sized>(&mut self, value: &Q) -> Option<(usize, T)>
    where
        Q: Hash + Equivalent<T>,
    {
        self.inner.shift_remove_full(value)
    }

    /// See [`IndexSet::pop`]
    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    /// See [`IndexSet::sort`]
    #[inline]
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.inner.sort_unstable();
    }

    /// See [`IndexSet::sort_by`]
    #[inline]
    pub fn sort_by<F>(&mut self, cmp: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        self.inner.sort_by(cmp);
    }

    /// See [`IndexSet::sorted_by`]
    #[inline]
    pub fn sorted_by<F>(self, cmp: F) -> IntoIter<T>
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        self.inner.sorted_by(cmp)
    }

    /// See [`IndexSet::sort_unstable_by`]
    #[inline]
    pub fn sort_unstable_by<F>(&mut self, cmp: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        self.inner.sort_unstable_by(cmp);
    }

    /// See [`IndexSet::sorted_unstable_by`]
    #[inline]
    pub fn sorted_unstable_by<F>(self, cmp: F) -> IntoIter<T>
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        self.inner.sorted_unstable_by(cmp)
    }

    /// See [`IndexSet::reverse`]
    #[inline]
    pub fn reverse(&mut self) {
        self.inner.reverse();
    }
}

impl<T, S> Clone for OrderedSet<T, S>
where
    T: Clone,
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

impl<T, S> Default for OrderedSet<T, S>
where
    S: BuildHasher + Default,
{
    #[inline]
    fn default() -> Self {
        Self {
            inner: IndexSet::with_hasher(S::default()),
        }
    }
}

impl<T, S> fmt::Debug for OrderedSet<T, S>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OrderedSet")
            .field("inner", &self.inner)
            .finish()
    }
}

impl<T, S> PartialEq for OrderedSet<T, S>
where
    T: Eq + Hash,
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

impl<T, S> Eq for OrderedSet<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
}

impl<T, S> PartialOrd for OrderedSet<T, S>
where
    T: Eq + Hash + Ord,
    S: BuildHasher,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T, S> Ord for OrderedSet<T, S>
where
    T: Eq + Hash + Ord,
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

impl<T, S> Hash for OrderedSet<T, S>
where
    T: Hash,
    S: BuildHasher,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        for elt in self.inner.iter() {
            elt.hash(state);
        }
    }
}

impl<T, S> FromIterator<T> for OrderedSet<T, S>
where
    T: Eq + Hash,
    S: BuildHasher + Default,
{
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self {
            inner: IndexSet::<T, S>::from_iter(iter),
        }
    }
}

impl<T, S> IntoIterator for OrderedSet<T, S> {
    type Item = T;
    type IntoIter = indexmap::set::IntoIter<T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a, T, S> IntoIterator for &'a OrderedSet<T, S> {
    type Item = &'a T;
    type IntoIter = indexmap::set::Iter<'a, T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<T, S> Extend<T> for OrderedSet<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
    #[inline]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.inner.extend(iter);
    }
}

impl<'a, T, S> Extend<&'a T> for OrderedSet<T, S>
where
    T: Eq + Hash + Copy + 'a,
    S: BuildHasher,
{
    #[inline]
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        self.inner.extend(iter);
    }
}

#[cfg(feature = "serde")]
impl<T, RS> serde::Serialize for OrderedSet<T, RS>
where
    T: serde::Serialize,
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
impl<'de, T, S> serde::Deserialize<'de> for OrderedSet<T, S>
where
    T: serde::Deserialize<'de> + Eq + Hash,
    S: BuildHasher + Default,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use core::marker::PhantomData;
        use serde::de::SeqAccess;
        use serde::de::Visitor;

        struct IndexSetSeqVisitor<T, S>(PhantomData<(T, S)>);

        impl<'de, T, S> Visitor<'de> for IndexSetSeqVisitor<T, S>
        where
            T: serde::Deserialize<'de> + Eq + Hash,
            S: BuildHasher + Default,
        {
            type Value = IndexSet<T, S>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("a sequence of elements")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut set =
                    IndexSet::with_capacity_and_hasher(seq.size_hint().unwrap_or(0), S::default());

                while let Some(elt) = seq.next_element::<T>()? {
                    set.insert(elt);
                }

                Ok(set)
            }
        }

        let set = deserializer.deserialize_seq(IndexSetSeqVisitor::<T, S>(PhantomData))?;
        Ok(Self { inner: set })
    }
}

#[cfg(feature = "speedy")]
impl<'a, C, T, S> speedy::Readable<'a, C> for OrderedSet<T, S>
where
    C: speedy::Context,
    T: speedy::Readable<'a, C> + Eq + Hash,
    S: BuildHasher + Default,
{
    #[inline]
    fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let length = reader.read_u32()? as usize;
        reader.read_collection(length)
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

#[cfg(feature = "speedy")]
impl<C, T, S> speedy::Writable<C> for OrderedSet<T, S>
where
    C: speedy::Context,
    T: speedy::Writable<C>,
{
    #[inline]
    fn write_to<W: ?Sized + speedy::Writer<C>>(
        &self,
        writer: &mut W,
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
