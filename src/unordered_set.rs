use core::borrow::Borrow;
use core::cmp::Ordering;
use core::fmt;
use core::hash::BuildHasher;
use core::hash::Hash;
use core::hash::Hasher;

use std::collections::HashSet;

pub use std::collections::hash_set::Difference;
pub use std::collections::hash_set::Drain;
pub use std::collections::hash_set::Intersection;
pub use std::collections::hash_set::IntoIter;
pub use std::collections::hash_set::Iter;
pub use std::collections::hash_set::SymmetricDifference;
pub use std::collections::hash_set::Union;

use crate::hash_one_fixed;

/// A deduplicated set that does not have a specified order of contained elements.
///
/// It is a good choice to use this set if you plan to do insertion, removal, and lookup significantly
/// more often than iteration of the contained elements. If you will iterate the elements often (even if you don't
/// specifically care about their order), think about using an [`OrderedSet`] instead. If you plan to have a small
/// number of elements (up to a few dozen) and those elements are fast to compare and/or small, consider [`LinearSet`] instead.
///
/// This is a wrapper around [`std::collections::HashSet`] which implements various traits in ways that fit
/// our use cases better than the choices `std` made. If you really need to access the wrapped [`HashSet`],
/// you can do so with the `inner`, `inner_mut`, and `into_inner` methods. However, be careful as using these
/// has different trait implementation semantics as mentioned below.
///
/// Implements `PartialEq`, `Eq`, and `Hash` such that two sets are equal and hash to the same value if they have
/// the same elements, regardless of order. However, the `Hash` implementation is not fully cryptographically secure.
///
/// Implements `Ord`, *but* the implementation requires that for sets
/// of the same length, we allocate a `Vec` containing all the elements for both sets,
/// sort them, and then do [lexographical] ordering between them,
/// which is very slow and it's not recommended to use this functionality if at all possible.
///
/// [`OrderedSet`]: crate::OrderedSet
/// [`LinearSet`]: crate::LinearSet
/// [lexographical]: core::cmp::Ord#lexographical-comparison
pub struct UnorderedSet<T, S = crate::BuildHasher> {
    pub(crate) inner: HashSet<T, S>,
}

impl<T> UnorderedSet<T, crate::BuildHasher> {
    /// [`HashSet::new`] but using an [`foldhash`] hasher.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// [`HashSet::with_capacity`] but using an [`foldhash`] hasher.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, crate::BuildHasher::default())
    }
}

impl<T, S> UnorderedSet<T, S> {
    /// See [`HashSet::with_capacity_and_hasher`]
    #[inline]
    pub fn with_capacity_and_hasher(capacity: usize, hasher: S) -> Self {
        Self {
            inner: HashSet::<T, S>::with_capacity_and_hasher(capacity, hasher),
        }
    }

    /// See [`HashSet::len`]
    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// See [`HashSet::capacity`]
    #[inline]
    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    /// See [`HashSet::iter`]
    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        self.inner.iter()
    }

    /// See [`HashSet::is_empty`]
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// See [`HashSet::drain`]
    #[inline]
    pub fn drain(&mut self) -> Drain<'_, T> {
        self.inner.drain()
    }

    /// See [`HashSet::retain`]
    #[inline]
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.inner.retain(f);
    }

    /// See [`HashSet::clear`]
    ///
    /// Note that this method does not shrink the underlying allocation (keeps capacity the same) and is `O(capacity)`.
    /// Thus repeated calls to `clear_no_shrink` on a set that is far under-occupied may be unexpectedly expensive. Consider using
    /// [`clear_and_shrink`] or [`clear_and_shrink_to`] to shrink the underlying allocation when appropriate when clearing.
    ///
    /// [`clear_and_shrink`]: UnorderedSet::clear_and_shrink
    /// [`clear_and_shrink_to`]: UnorderedSet::clear_and_shrink_to
    #[inline]
    pub fn clear_no_shrink(&mut self) {
        self.inner.clear();
    }

    /// See [`HashSet::hasher`]
    #[inline]
    pub fn hasher(&self) -> &S {
        self.inner.hasher()
    }

    /// Access the wrapped [`HashSet`].
    #[inline]
    pub fn inner(&self) -> &HashSet<T, S> {
        &self.inner
    }

    /// Access the wrapped [`HashSet`] mutably.
    #[inline]
    pub fn inner_mut(&mut self) -> &mut HashSet<T, S> {
        &mut self.inner
    }

    /// Extract the wrapped [`HashSet`].
    #[inline]
    pub fn into_inner(self) -> HashSet<T, S> {
        self.inner
    }
}

impl<T, const N: usize> From<[T; N]> for UnorderedSet<T, crate::BuildHasher>
where
    T: Hash + Eq,
{
    fn from(arr: [T; N]) -> Self {
        Self {
            inner: HashSet::<T, crate::BuildHasher>::from_iter(arr),
        }
    }
}

impl<T, S> UnorderedSet<T, S>
where
    T: Hash + Eq,
    S: BuildHasher,
{
    /// See [`HashSet::reserve`]
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional);
    }

    /// See [`HashSet::try_reserve`]
    #[inline]
    pub fn try_reserve(
        &mut self,
        additional: usize,
    ) -> Result<(), std::collections::TryReserveError> {
        self.inner.try_reserve(additional)
    }

    /// See [`HashSet::shrink_to_fit`]
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.inner.shrink_to_fit();
    }

    /// See [`HashSet::shrink_to`]
    #[inline]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.inner.shrink_to(min_capacity);
    }

    /// Clears the set, removing all elements.
    ///
    /// Note that this shrinks the capacity of the set based on a basic heuristic. See [`clear_and_shrink`] for more details, which this
    /// method redirects to internally.
    ///
    /// [`clear_and_shrink`]: UnorderedSet::clear_and_shrink
    #[inline]
    pub fn clear(&mut self) {
        self.clear_and_shrink();
    }

    /// Clears and shrinks the capacity of the set on a basic heuristic. If you have a more specific heuristic, see [`clear_and_shrink_to`].
    ///
    /// If the set previously had > 128 element capacity, shrinks to whichever is larger between 128 and 110% of the previous length of the set
    /// in an effort to reduce reallocation for repeated use-and-clear on similar numbers of items. If the set had <= 128 element capacity, no shrink happens.
    ///
    /// [`clear_and_shrink_to`]: UnorderedSet::clear_and_shrink_to
    #[inline]
    pub fn clear_and_shrink(&mut self) {
        if self.capacity() > 128 {
            let new_cap = 128usize.max((self.len() as f64 * 1.1) as usize);
            self.clear_and_shrink_to(new_cap);
        } else {
            self.clear_no_shrink();
        }
    }

    /// Clears and shrinks the capacity of the set to the given capacity.
    #[inline]
    pub fn clear_and_shrink_to(&mut self, capacity: usize) {
        self.clear_no_shrink();
        self.shrink_to(capacity);
    }

    /// See [`HashSet::difference`]
    #[inline]
    pub fn difference<'a>(&'a self, other: &'a Self) -> Difference<'a, T, S> {
        self.inner.difference(&other.inner)
    }

    /// See [`HashSet::symmetric_difference`]
    #[inline]
    pub fn symmetric_difference<'a>(&'a self, other: &'a Self) -> SymmetricDifference<'a, T, S> {
        self.inner.symmetric_difference(&other.inner)
    }

    /// See [`HashSet::intersection`]
    #[inline]
    pub fn intersection<'a>(&'a self, other: &'a Self) -> Intersection<'a, T, S> {
        self.inner.intersection(&other.inner)
    }

    /// See [`HashSet::union`]
    #[inline]
    pub fn union<'a>(&'a self, other: &'a Self) -> Union<'a, T, S> {
        self.inner.union(&other.inner)
    }

    /// See [`HashSet::contains`]
    #[inline]
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.inner.contains(value)
    }

    /// See [`HashSet::get`]
    #[inline]
    pub fn get<Q>(&self, value: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.inner.get(value)
    }

    /// See [`HashSet::is_disjoint`]
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.inner.is_disjoint(&other.inner)
    }

    /// See [`HashSet::is_subset`]
    pub fn is_subset(&self, other: &Self) -> bool {
        self.inner.is_subset(&other.inner)
    }

    /// See [`HashSet::is_superset`]
    #[inline]
    pub fn is_superset(&self, other: &Self) -> bool {
        self.inner.is_superset(&other.inner)
    }

    /// See [`HashSet::insert`]
    #[inline]
    pub fn insert(&mut self, value: T) -> bool {
        self.inner.insert(value)
    }

    /// See [`HashSet::replace`]
    #[inline]
    pub fn replace(&mut self, value: T) -> Option<T> {
        self.inner.replace(value)
    }

    /// See [`HashSet::remove`]
    #[inline]
    pub fn remove<Q>(&mut self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.inner.remove(value)
    }

    /// See [`HashSet::take`]
    #[inline]
    pub fn take<Q>(&mut self, value: &Q) -> Option<T>
    where
        T: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.inner.take(value)
    }
}

impl<T, S> Clone for UnorderedSet<T, S>
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

impl<T, S> Default for UnorderedSet<T, S>
where
    S: BuildHasher + Default,
{
    #[inline]
    fn default() -> Self {
        Self {
            inner: HashSet::with_hasher(S::default()),
        }
    }
}

impl<T, S> fmt::Debug for UnorderedSet<T, S>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnorderedSet")
            .field("inner", &self.inner)
            .finish()
    }
}

impl<T, S> PartialEq for UnorderedSet<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<T, S> Eq for UnorderedSet<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
}

impl<T, S> PartialOrd for UnorderedSet<T, S>
where
    T: Eq + Hash + Ord,
    S: BuildHasher,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T, S> Ord for UnorderedSet<T, S>
where
    T: Eq + Hash + Ord,
    S: BuildHasher,
{
    fn cmp(&self, other: &Self) -> Ordering {
        // first compare lengths, if equal, we have to sort and do lexographical ordering...
        match self.len().cmp(&other.len()) {
            Ordering::Less => return Ordering::Less,
            Ordering::Greater => return Ordering::Greater,
            Ordering::Equal => (),
        }
        let mut self_seq = self.inner.iter().collect::<Vec<_>>();
        self_seq.sort();
        let mut other_seq = other.inner.iter().collect::<Vec<_>>();
        other_seq.sort();
        self_seq.into_iter().cmp(other_seq)
    }
}

impl<T, S> Hash for UnorderedSet<T, S>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        // although this is in theory not fully cryptographically secure, we don't care about that here.
        // also, since we are using a deterministic hasher, we don't have to worry about differences between sets' hashers.
        // Thus we can use xor as an order-independent hash-combination function. Thus, this
        // will create the same hash as long as two graphs have the same elements, regardless of order.
        // this also satisfies the requirement that Eq and Hash have the same semantics, as HashSet's PartialEq and Eq
        // also have this semantics that as long as the elements are equal the sets are equal.
        let mut hash = 0u64;
        for elt in self.inner.iter() {
            let elt_hash = hash_one_fixed(elt);
            hash ^= elt_hash;
        }
        state.write_u64(hash);
    }
}

impl<T, S> FromIterator<T> for UnorderedSet<T, S>
where
    T: Eq + Hash,
    S: BuildHasher + Default,
{
    #[inline]
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self {
            inner: HashSet::<T, S>::from_iter(iter),
        }
    }
}

impl<T, S> IntoIterator for UnorderedSet<T, S> {
    type Item = T;
    type IntoIter = std::collections::hash_set::IntoIter<T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<'a, T, S> IntoIterator for &'a UnorderedSet<T, S> {
    type Item = &'a T;
    type IntoIter = std::collections::hash_set::Iter<'a, T>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl<T, S> Extend<T> for UnorderedSet<T, S>
where
    T: Eq + Hash,
    S: BuildHasher,
{
    #[inline]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.inner.extend(iter);
    }
}

impl<'a, T, S> Extend<&'a T> for UnorderedSet<T, S>
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
impl<T, RS> serde::Serialize for UnorderedSet<T, RS>
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
impl<'de, T, S> serde::Deserialize<'de> for UnorderedSet<T, S>
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

        struct HashSetSeqVisitor<T, S>(PhantomData<(T, S)>);

        impl<'de, T, S> Visitor<'de> for HashSetSeqVisitor<T, S>
        where
            T: serde::Deserialize<'de> + Eq + Hash,
            S: BuildHasher + Default,
        {
            type Value = HashSet<T, S>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("a sequence of T")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut set =
                    HashSet::with_capacity_and_hasher(seq.size_hint().unwrap_or(0), S::default());

                while let Some(elt) = seq.next_element::<T>()? {
                    set.insert(elt);
                }

                Ok(set)
            }
        }

        let set = deserializer.deserialize_seq(HashSetSeqVisitor::<T, S>(PhantomData))?;
        Ok(Self { inner: set })
    }
}

#[cfg(feature = "speedy")]
impl<'a, C, T, S> speedy::Readable<'a, C> for UnorderedSet<T, S>
where
    C: speedy::Context,
    T: speedy::Readable<'a, C> + Eq + Hash,
    S: BuildHasher + Default,
{
    #[inline]
    fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let set = HashSet::<T, S>::read_from(reader)?;
        Ok(Self { inner: set })
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        HashSet::<T, S>::minimum_bytes_needed()
    }
}

#[cfg(feature = "speedy")]
impl<C, T, S> speedy::Writable<C> for UnorderedSet<T, S>
where
    C: speedy::Context,
    T: speedy::Writable<C>,
{
    #[inline]
    fn write_to<W: ?Sized + speedy::Writer<C>>(
        &self,
        writer: &mut W,
    ) -> Result<(), <C as speedy::Context>::Error> {
        HashSet::<T, S>::write_to(&self.inner, writer)
    }

    #[inline]
    fn bytes_needed(&self) -> Result<usize, C::Error> {
        self.inner.bytes_needed()
    }
}
