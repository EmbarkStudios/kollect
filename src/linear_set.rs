// This code originally from the `linear_map` crate, but elected to be copy/reimplemented with modifications here as
// that crate is no longer actively maintained.

use core::cmp::Ordering;
use core::mem::ManuallyDrop;
use core::slice;
use std::borrow::Borrow;
use std::fmt;
use std::iter::Chain;
use std::iter::FromIterator;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::BitXor;
use std::ops::Sub;

use super::linear_map::Keys;
use super::linear_map::LinearMap;

/// An implementation of a set using the underlying representation of a
/// [`LinearMap`] where the value is (). This means the set is effectively
/// a [`Vec`] with operations optimized for working on items that function as
/// a set.
///
/// Implements `PartialEq`, `Eq`, and `Hash` such that two sets are equal and hash to the same value if they have
/// the same elements regardless of order. However, the `Hash` implementation is not fully cryptographically secure.
///
/// Implements `Ord`, *but* the implementation requires that for sets
/// of the same length, we copy the `Vec`s containing all the elements for both maps,
/// sort them, and then do [lexographical] ordering between them,
/// which is very slow and it's not recommended to use this functionality if at all possible.
///
/// It is a logic error for an element to be modified in such a way that the element's equality, as
/// determined by the [`Eq`][eq] trait, changes while it is in the set. This is normally only
/// possible through [`Cell`][cell], [`RefCell`][ref_cell], global state, I/O, or unsafe code.
///
/// [cell]: https://doc.rust-lang.org/nightly/std/cell/struct.Cell.html
/// [eq]: https://doc.rust-lang.org/nightly/std/cmp/trait.Eq.html
/// [ref_cell]: https://doc.rust-lang.org/nightly/std/cell/struct.RefCell.html
///
/// # Ordering
///
/// Ordering of elements is well-defined but may be unexpected due to the use of
/// [`swap_remove`][Vec::swap_remove] in the implementation of [`LinearSet::remove`].
/// If you want to maintain order on removal, see [`LinearSet::shift_remove`], but note
/// its additional time complexity. You can also use one of the sorting functions to
/// change the order after doing arbitrary insertion and removals.
///
/// # Examples
///
/// ```
/// use kollect::linear_set::LinearSet;;
/// // Type inference lets us omit an explicit type signature (which
/// // would be `LinearSet<&str>` in this example).
/// let mut books = LinearSet::new();
///
/// // Add some books.
/// books.insert("A Dance With Dragons");
/// books.insert("To Kill a Mockingbird");
/// books.insert("The Odyssey");
/// books.insert("The Great Gatsby");
///
/// // Check for a specific one.
/// if !books.contains("The Winds of Winter") {
///     println!("We have {} books, but The Winds of Winter ain't one.",
///              books.len());
/// }
///
/// // Remove a book.
/// books.remove("The Odyssey");
///
/// // Iterate over everything.
/// for book in &books {
///     println!("{}", book);
/// }
/// ```
///
/// The easiest way to use `LinearSet` with a custom type is to derive
/// `Eq`. We must also derive `PartialEq`, this will in the
/// future be implied by `Eq`.
///
/// ```
/// use kollect::linear_set::LinearSet;;
/// #[derive(Eq, PartialEq, Debug)]
/// struct Viking<'a> {
///     name: &'a str,
///     power: usize,
/// }
///
/// let mut vikings = LinearSet::new();
///
/// vikings.insert(Viking { name: "Einar", power: 9 });
/// vikings.insert(Viking { name: "Einar", power: 9 });
/// vikings.insert(Viking { name: "Olaf", power: 4 });
/// vikings.insert(Viking { name: "Harald", power: 8 });
///
/// // Use derived implementation to print the vikings.
/// for x in &vikings {
///     println!("{:?}", x);
/// }
/// ```
///
/// [lexographical]: core::cmp::Ord#lexographical-comparison
#[derive(Clone, Hash)]
#[allow(clippy::derived_hash_with_manual_eq)]
pub struct LinearSet<T> {
    map: LinearMap<T, ()>,
}

impl<T> LinearSet<T> {
    /// Creates a new [`LinearSet`] from the `Vec` without checking for duplicate entries or other logic errors.
    ///
    /// This is faster than `<LinearSet as From<Vec<T>>::from(vec)` but could result in logic errors.
    #[inline]
    pub fn from_vec_unchecked(storage: Vec<T>) -> Self {
        let mut manually_drop = ManuallyDrop::new(storage);
        // SAFETY:
        // we just made sure that the old vec will not get dropped/freed by putting it in a
        // manually drop.
        //
        // A tuple of (T, ()) is guaranteed to have the same size and alignment as T since `()`
        // is a ZST, so we can directly cast the pointer.
        let storage = unsafe {
            Vec::from_raw_parts(
                manually_drop.as_mut_ptr().cast::<T>().cast::<(T, ())>(),
                manually_drop.len(),
                manually_drop.capacity(),
            )
        };

        Self {
            map: LinearMap::from_vec_unchecked(storage),
        }
    }
}

impl<T: Eq> LinearSet<T> {
    /// Creates an empty [`LinearSet`].
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let mut set: LinearSet<i32> = LinearSet::new();
    /// ```
    #[inline]
    pub const fn new() -> Self {
        Self {
            map: LinearMap::new(),
        }
    }

    /// Creates an empty [`LinearSet`] with space for at least `n` elements in
    /// the set.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let mut set: LinearSet<i32> = LinearSet::with_capacity(10);
    /// ```
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            map: LinearMap::with_capacity(capacity),
        }
    }
}

impl<T> LinearSet<T>
where
    T: Eq,
{
    /// Returns the number of elements the set can hold without reallocating.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let set: LinearSet<i32> = LinearSet::with_capacity(100);
    /// assert!(set.capacity() >= 100);
    /// ```
    #[inline]
    pub fn capacity(&self) -> usize {
        self.map.capacity()
    }

    /// Reserves capacity for at least `additional` more elements to be inserted
    /// in the `LinearSet`. The collection may reserve more space to avoid
    /// frequent reallocations.
    ///
    /// # Panics
    ///
    /// Panics if the new allocation size overflows `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let mut set: LinearSet<i32> = LinearSet::new();
    /// set.reserve(10);
    /// ```
    pub fn reserve(&mut self, additional: usize) {
        self.map.reserve(additional);
    }

    /// Shrinks the capacity of the set as much as possible. It will drop
    /// down as much as possible while maintaining the internal rules
    /// and possibly leaving some space in accordance with the resize policy.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let mut set = LinearSet::with_capacity(100);
    /// set.insert(1);
    /// set.insert(2);
    /// assert!(set.capacity() >= 100);
    /// set.shrink_to_fit();
    /// assert!(set.capacity() >= 2);
    /// ```
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.map.shrink_to_fit();
    }

    /// Shrinks the capacity of the set as much as possible to a given minimum capacity.
    ///
    /// It will drop down as much as possible while maintaining the internal rules
    /// and possibly leaving some space in accordance with the resize policy.
    #[inline]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.map.shrink_to(min_capacity);
    }

    /// Shortens the set, keeping the first len elements and dropping the rest.
    ///
    /// If len is greater than the set's current length, this has no effect.
    #[inline]
    pub fn truncate(&mut self, len: usize) {
        self.map.truncate(len);
    }

    /// An iterator visiting all elements.
    ///
    /// Order is well-defined but may be different than expected.
    /// See notes on [ordering][LinearSet#Ordering].
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let mut set = LinearSet::new();
    /// set.insert("a");
    /// set.insert("b");
    ///
    /// for x in set.iter() {
    ///     println!("{}", x);
    /// }
    /// ```
    #[inline]
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            iter: self.map.keys(),
        }
    }

    /// A mutable iterator visiting all elements. **Careful: it is possible to introduce logic errors with this.**
    ///
    /// No two elements in the set should be modified to be `Eq` equal to each other, or else things may break
    /// in unexpected ways.
    ///
    /// Order is well-defined but may be different than expected.
    /// See notes on [ordering][LinearSet#Ordering].
    #[inline]
    pub fn iter_unchecked_mut(&mut self) -> IterUncheckedMut<'_, T> {
        let base_ptr = (self.map.storage.as_mut_slice() as *mut [(T, ())]).cast::<T>();
        // SAFETY: (T, ()) is guaranteed to have the same layout as T and the returned reference is
        // bound to the borrow on self
        let slice = unsafe { core::slice::from_raw_parts_mut(base_ptr, self.map.storage.len()) };
        IterUncheckedMut {
            iter: slice.iter_mut(),
        }
    }

    /// Visit the values representing the difference.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let a: LinearSet<_> = [1, 2, 3].iter().cloned().collect();
    /// let b: LinearSet<_> = [4, 2, 3, 4].iter().cloned().collect();
    ///
    /// // Can be seen as `a - b`.
    /// for x in a.difference(&b) {
    ///     println!("{}", x); // Print 1
    /// }
    ///
    /// let diff: LinearSet<_> = a.difference(&b).cloned().collect();
    /// assert_eq!(diff, [1].iter().cloned().collect());
    ///
    /// // Note that difference is not symmetric,
    /// // and `b - a` means something else:
    /// let diff: LinearSet<_> = b.difference(&a).cloned().collect();
    /// assert_eq!(diff, [4].iter().cloned().collect());
    /// ```
    pub fn difference<'a>(&'a self, other: &'a Self) -> Difference<'a, T> {
        Difference {
            iter: self.iter(),
            other,
        }
    }

    /// Visit the values representing the symmetric difference.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let a: LinearSet<_> = [1, 2, 3].iter().cloned().collect();
    /// let b: LinearSet<_> = [4, 2, 3, 4].iter().cloned().collect();
    ///
    /// for x in a.symmetric_difference(&b) {
    ///     println!("{}", x);
    /// }
    ///
    /// let diff1: LinearSet<_> = a.symmetric_difference(&b).cloned().collect();
    /// let diff2: LinearSet<_> = b.symmetric_difference(&a).cloned().collect();
    ///
    /// assert_eq!(diff1, diff2);
    /// assert_eq!(diff1, [1, 4].iter().cloned().collect());
    /// ```
    pub fn symmetric_difference<'a>(&'a self, other: &'a Self) -> SymmetricDifference<'a, T> {
        SymmetricDifference {
            iter: self.difference(other).chain(other.difference(self)),
        }
    }

    /// Visit the values representing the intersection.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let a: LinearSet<_> = [1, 2, 3].iter().cloned().collect();
    /// let b: LinearSet<_> = [4, 2, 3, 4].iter().cloned().collect();
    ///
    /// for x in a.intersection(&b) {
    ///     println!("{}", x);
    /// }
    ///
    /// let intersection: LinearSet<_> = a.intersection(&b).cloned().collect();
    /// assert_eq!(intersection, [2, 3].iter().cloned().collect());
    /// ```
    pub fn intersection<'a>(&'a self, other: &'a Self) -> Intersection<'a, T> {
        Intersection {
            iter: self.iter(),
            other,
        }
    }

    /// Visit the values representing the union.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let a: LinearSet<_> = [1, 2, 3].iter().cloned().collect();
    /// let b: LinearSet<_> = [4, 2, 3, 4].iter().cloned().collect();
    ///
    /// for x in a.union(&b) {
    ///     println!("{}", x);
    /// }
    ///
    /// let union: LinearSet<_> = a.union(&b).cloned().collect();
    /// assert_eq!(union, [1, 2, 3, 4].iter().cloned().collect());
    /// ```
    pub fn union<'a>(&'a self, other: &'a Self) -> Union<'a, T> {
        Union {
            iter: self.iter().chain(other.difference(self)),
        }
    }

    /// Returns the number of elements in the set.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let mut v = LinearSet::new();
    /// assert_eq!(v.len(), 0);
    /// v.insert(1);
    /// assert_eq!(v.len(), 1);
    /// ```
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Returns true if the set contains no elements.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let mut v = LinearSet::new();
    /// assert!(v.is_empty());
    /// v.insert(1);
    /// assert!(!v.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Clears the set, returning all elements in an iterator.
    #[inline]
    pub fn drain(&mut self) -> Drain<'_, T> {
        Drain {
            iter: self.map.drain(),
        }
    }

    /// Clears the set, removing all elements.
    ///
    /// Unlike the hashtable-based sets provided by this crate, keeps the
    /// whole allocated memory for reuse and does not shrink by default since
    /// this set is not designed to ever have a high number of elements and clearing
    /// the underlying vector storage is O(1).
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let mut v = LinearSet::new();
    /// v.insert(1);
    /// v.clear();
    /// assert!(v.is_empty());
    /// ```
    pub fn clear(&mut self) {
        self.map.clear();
    }

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all elements `e` such that `f(&e)` returns `false`.
    #[inline]
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.map.retain(|k, _| f(k));
    }

    /// Returns `true` if the set contains a value.
    ///
    /// The value may be any borrowed form of the set's value type, but
    /// `Eq` on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let set: LinearSet<_> = [1, 2, 3].iter().cloned().collect();
    /// assert_eq!(set.contains(&1), true);
    /// assert_eq!(set.contains(&4), false);
    /// ```
    #[inline]
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: ?Sized + Eq,
    {
        self.map.contains_key(value)
    }

    /// Returns `true` if the set has no elements in common with `other`.
    /// This is equivalent to checking for an empty intersection.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let a: LinearSet<_> = [1, 2, 3].iter().cloned().collect();
    /// let mut b = LinearSet::new();
    ///
    /// assert_eq!(a.is_disjoint(&b), true);
    /// b.insert(4);
    /// assert_eq!(a.is_disjoint(&b), true);
    /// b.insert(1);
    /// assert_eq!(a.is_disjoint(&b), false);
    /// ```
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.iter().all(|v| !other.contains(v))
    }

    /// Returns `true` if the set is a subset of another.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let sup: LinearSet<_> = [1, 2, 3].iter().cloned().collect();
    /// let mut set = LinearSet::new();
    ///
    /// assert_eq!(set.is_subset(&sup), true);
    /// set.insert(2);
    /// assert_eq!(set.is_subset(&sup), true);
    /// set.insert(4);
    /// assert_eq!(set.is_subset(&sup), false);
    /// ```
    pub fn is_subset(&self, other: &Self) -> bool {
        self.iter().all(|v| other.contains(v))
    }

    /// Returns `true` if the set is a superset of another.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let sub: LinearSet<_> = [1, 2].iter().cloned().collect();
    /// let mut set = LinearSet::new();
    ///
    /// assert_eq!(set.is_superset(&sub), false);
    ///
    /// set.insert(0);
    /// set.insert(1);
    /// assert_eq!(set.is_superset(&sub), false);
    ///
    /// set.insert(2);
    /// assert_eq!(set.is_superset(&sub), true);
    /// ```
    #[inline]
    pub fn is_superset(&self, other: &Self) -> bool {
        other.is_subset(self)
    }

    /// Returns a reference to the element in the set at the given index, if it exists.
    #[inline]
    pub fn get_index(&self, index: usize) -> Option<&T> {
        Some(&self.map.get_index(index)?.0)
    }

    /// Adds a value to the set.
    ///
    /// If the set did not have a value present, `true` is returned.
    ///
    /// If the set did have this key present, `false` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let mut set = LinearSet::new();
    ///
    /// assert_eq!(set.insert(2), true);
    /// assert_eq!(set.insert(2), false);
    /// assert_eq!(set.len(), 1);
    /// ```
    #[inline]
    pub fn insert(&mut self, value: T) -> bool {
        self.map.insert(value, ()).is_none()
    }

    /// Removes (by swap) a value from the set. Returns `true` if the value was
    /// present in the set.
    ///
    /// The value may be any borrowed form of the set's value type, but
    /// `Eq` on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let mut set = LinearSet::new();
    ///
    /// set.insert(2);
    /// assert_eq!(set.remove(&2), true);
    /// assert_eq!(set.remove(&2), false);
    /// ```
    #[inline]
    pub fn remove<Q>(&mut self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: ?Sized + Eq,
    {
        self.map.remove(value).is_some()
    }

    /// Removes a value from the set by shifting all subsequent elements. Returns `true` if the value was
    /// present in the set.
    ///
    /// The value may be any borrowed form of the set's value type, but
    /// `Eq` on the borrowed form *must* match those for
    /// the value type.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let mut set = LinearSet::new();
    ///
    /// set.insert(2);
    /// assert_eq!(set.remove(&2), true);
    /// assert_eq!(set.remove(&2), false);
    /// ```
    #[inline]
    pub fn shift_remove<Q>(&mut self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: ?Sized + Eq,
    {
        self.map.shift_remove(value).is_some()
    }

    /// Sort the set's elements by their [`Ord`] ordering.
    ///
    /// Since we are guaranteed to have no equal elements, we can use unstable sort by default.
    #[inline]
    pub fn sort(&mut self)
    where
        T: Ord,
    {
        self.map.sort_keys();
    }

    /// Sort the set's elements by the given comparison function
    #[inline]
    pub fn sort_by<F>(&mut self, mut cmp: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        self.map.sort_by(|t1, _, t2, _| cmp(t1, t2));
    }

    /// Sort the set's elements by the given comparison function with an unstable
    /// sorting function (the order of equal-evaluated elements is not guaranteed to be the same).
    #[inline]
    pub fn sort_unstable_by<F>(&mut self, mut cmp: F)
    where
        F: FnMut(&T, &T) -> Ordering,
    {
        self.map.sort_unstable_by(|t1, _, t2, _| cmp(t1, t2));
    }
}

impl<T> PartialEq for LinearSet<T>
where
    T: Eq,
{
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        self.iter().all(|key| other.contains(key))
    }
}

impl<T> Eq for LinearSet<T> where T: Eq {}

impl<T: Eq + Ord> PartialOrd for LinearSet<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.map.cmp(&other.map))
    }
}

impl<T: Eq + Ord> Ord for LinearSet<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.map.cmp(&other.map)
    }
}

impl<T> fmt::Debug for LinearSet<T>
where
    T: Eq + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<T> FromIterator<T> for LinearSet<T>
where
    T: Eq,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let iterator = iter.into_iter();
        let lower = iterator.size_hint().0;
        let mut set = Self::with_capacity(lower);
        set.extend(iterator);
        set
    }
}

impl<T> Extend<T> for LinearSet<T>
where
    T: Eq,
{
    #[inline]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for k in iter {
            self.insert(k);
        }
    }
}

impl<'a, T> Extend<&'a T> for LinearSet<T>
where
    T: 'a + Eq + Copy,
{
    #[inline]
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        self.extend(iter.into_iter().cloned());
    }
}

impl<T> Default for LinearSet<T>
where
    T: Eq,
{
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Eq> From<LinearSet<T>> for Vec<T> {
    #[inline]
    fn from(other: LinearSet<T>) -> Self {
        let storage = Vec::<_>::from(other.map);
        let mut manually_drop = ManuallyDrop::new(storage);
        // SAFETY:
        // we just made sure that the old vec will not get dropped/freed by putting it in a
        // manually drop.
        //
        // A tuple of (T, ()) is guaranteed to have the same size and alignment as T since `()`
        // is a ZST, so we can directly cast the pointer.
        unsafe {
            Self::from_raw_parts(
                manually_drop.as_mut_ptr().cast::<(T, ())>().cast::<T>(),
                manually_drop.len(),
                manually_drop.capacity(),
            )
        }
    }
}

impl<T: Eq> From<Vec<T>> for LinearSet<T> {
    #[inline]
    fn from(other: Vec<T>) -> Self {
        other.into_iter().collect()
    }
}

impl<T: Eq + Clone, const N: usize> From<[T; N]> for LinearSet<T> {
    #[inline]
    fn from(arr: [T; N]) -> Self {
        Self::from(arr.to_vec())
    }
}

impl<T> BitOr<&LinearSet<T>> for &LinearSet<T>
where
    T: Eq + Clone,
{
    type Output = LinearSet<T>;

    /// Returns the union of `self` and `rhs` as a new `LinearSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let a: LinearSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: LinearSet<_> = vec![3, 4, 5].into_iter().collect();
    ///
    /// let set = &a | &b;
    ///
    /// let mut i = 0;
    /// let expected = [1, 2, 3, 4, 5];
    /// for x in &set {
    ///     assert!(expected.contains(x));
    ///     i += 1;
    /// }
    /// assert_eq!(i, expected.len());
    /// ```
    fn bitor(self, rhs: &LinearSet<T>) -> LinearSet<T> {
        self.union(rhs).cloned().collect()
    }
}

impl<T> BitAnd<&LinearSet<T>> for &LinearSet<T>
where
    T: Eq + Clone,
{
    type Output = LinearSet<T>;

    /// Returns the intersection of `self` and `rhs` as a new `LinearSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let a: LinearSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: LinearSet<_> = vec![2, 3, 4].into_iter().collect();
    ///
    /// let set = &a & &b;
    ///
    /// let mut i = 0;
    /// let expected = [2, 3];
    /// for x in &set {
    ///     assert!(expected.contains(x));
    ///     i += 1;
    /// }
    /// assert_eq!(i, expected.len());
    /// ```
    fn bitand(self, rhs: &LinearSet<T>) -> LinearSet<T> {
        self.intersection(rhs).cloned().collect()
    }
}

impl<T> BitXor<&LinearSet<T>> for &LinearSet<T>
where
    T: Eq + Clone,
{
    type Output = LinearSet<T>;

    /// Returns the symmetric difference of `self` and `rhs` as a new `LinearSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let a: LinearSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: LinearSet<_> = vec![3, 4, 5].into_iter().collect();
    ///
    /// let set = &a ^ &b;
    ///
    /// let mut i = 0;
    /// let expected = [1, 2, 4, 5];
    /// for x in &set {
    ///     assert!(expected.contains(x));
    ///     i += 1;
    /// }
    /// assert_eq!(i, expected.len());
    /// ```
    fn bitxor(self, rhs: &LinearSet<T>) -> LinearSet<T> {
        self.symmetric_difference(rhs).cloned().collect()
    }
}

impl<T> Sub<&LinearSet<T>> for &LinearSet<T>
where
    T: Eq + Clone,
{
    type Output = LinearSet<T>;

    /// Returns the difference of `self` and `rhs` as a new `LinearSet<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    ///
    /// let a: LinearSet<_> = vec![1, 2, 3].into_iter().collect();
    /// let b: LinearSet<_> = vec![3, 4, 5].into_iter().collect();
    ///
    /// let set = &a - &b;
    ///
    /// let mut i = 0;
    /// let expected = [1, 2];
    /// for x in &set {
    ///     assert!(expected.contains(x));
    ///     i += 1;
    /// }
    /// assert_eq!(i, expected.len());
    /// ```
    fn sub(self, rhs: &LinearSet<T>) -> LinearSet<T> {
        self.difference(rhs).cloned().collect()
    }
}

/// [`LinearSet`] iterator
#[allow(missing_debug_implementations)]
pub struct Iter<'a, K> {
    iter: Keys<'a, K, ()>,
}

/// [`LinearSet`] iterator over mutable elements. **Careful: no two elements
/// should be made `Eq` equal or else unexpected behavior may occur.**
#[allow(missing_debug_implementations)]
pub struct IterUncheckedMut<'a, K> {
    iter: slice::IterMut<'a, K>,
}

/// [`LinearSet`] move iterator
#[allow(missing_debug_implementations)]
pub struct IntoIter<K> {
    iter: super::linear_map::IntoIter<K, ()>,
}

/// [`LinearSet`] drain iterator
#[allow(missing_debug_implementations)]
pub struct Drain<'a, K> {
    iter: super::linear_map::Drain<'a, K, ()>,
}

/// Intersection iterator
#[allow(missing_debug_implementations)]
pub struct Intersection<'a, T> {
    // iterator of the first set
    iter: Iter<'a, T>,
    // the second set
    other: &'a LinearSet<T>,
}

/// Difference iterator
#[allow(missing_debug_implementations)]
pub struct Difference<'a, T> {
    // iterator of the first set
    iter: Iter<'a, T>,
    // the second set
    other: &'a LinearSet<T>,
}

/// Symmetric difference iterator.
#[allow(missing_debug_implementations)]
pub struct SymmetricDifference<'a, T> {
    iter: Chain<Difference<'a, T>, Difference<'a, T>>,
}

/// Set union iterator.
#[allow(missing_debug_implementations)]
pub struct Union<'a, T> {
    iter: Chain<Iter<'a, T>, Difference<'a, T>>,
}

impl<'a, T> IntoIterator for &'a LinearSet<T>
where
    T: Eq,
{
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Iter<'a, T> {
        self.iter()
    }
}

impl<T> IntoIterator for LinearSet<T>
where
    T: Eq,
{
    type Item = T;
    type IntoIter = IntoIter<T>;

    /// Creates a consuming iterator, that is, one that moves each value out
    /// of the set. The set cannot be used after calling this.
    ///
    /// # Examples
    ///
    /// ```
    /// use kollect::linear_set::LinearSet;;
    /// let mut set = LinearSet::new();
    /// set.insert("a".to_string());
    /// set.insert("b".to_string());
    ///
    /// // Not possible to collect to a Vec<String> with a regular `.iter()`.
    /// let v: Vec<String> = set.into_iter().collect();
    ///
    /// for x in &v {
    ///     println!("{}", x);
    /// }
    /// ```
    #[inline]
    fn into_iter(self) -> IntoIter<T> {
        IntoIter {
            iter: self.map.into_iter(),
        }
    }
}

impl<K> Clone for Iter<'_, K> {
    #[inline]
    fn clone(&self) -> Self {
        Iter {
            iter: self.iter.clone(),
        }
    }
}
impl<'a, K> Iterator for Iter<'a, K> {
    type Item = &'a K;

    #[inline]
    fn next(&mut self) -> Option<&'a K> {
        self.iter.next()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
impl<K> ExactSizeIterator for Iter<'_, K> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<'a, K> Iterator for IterUncheckedMut<'a, K> {
    type Item = &'a mut K;

    #[inline]
    fn next(&mut self) -> Option<&'a mut K> {
        self.iter.next()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
impl<K> ExactSizeIterator for IterUncheckedMut<'_, K> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<K> Iterator for IntoIter<K> {
    type Item = K;

    #[inline]
    fn next(&mut self) -> Option<K> {
        self.iter.next().map(|(k, _)| k)
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
impl<K> ExactSizeIterator for IntoIter<K> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<K> Iterator for Drain<'_, K> {
    type Item = K;

    #[inline]
    fn next(&mut self) -> Option<K> {
        self.iter.next().map(|(k, _)| k)
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
impl<K> ExactSizeIterator for Drain<'_, K> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<T> Clone for Intersection<'_, T> {
    #[inline]
    fn clone(&self) -> Self {
        Intersection {
            iter: self.iter.clone(),
            ..*self
        }
    }
}

impl<'a, T> Iterator for Intersection<'a, T>
where
    T: Eq,
{
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        loop {
            match self.iter.next() {
                None => return None,
                Some(elt) => {
                    if self.other.contains(elt) {
                        return Some(elt);
                    }
                }
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (_, upper) = self.iter.size_hint();
        (0, upper)
    }
}

impl<T> Clone for Difference<'_, T> {
    #[inline]
    fn clone(&self) -> Self {
        Difference {
            iter: self.iter.clone(),
            ..*self
        }
    }
}

impl<'a, T> Iterator for Difference<'a, T>
where
    T: Eq,
{
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        loop {
            match self.iter.next() {
                None => return None,
                Some(elt) => {
                    if !self.other.contains(elt) {
                        return Some(elt);
                    }
                }
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (_, upper) = self.iter.size_hint();
        (0, upper)
    }
}

impl<T> Clone for SymmetricDifference<'_, T> {
    #[inline]
    fn clone(&self) -> Self {
        SymmetricDifference {
            iter: self.iter.clone(),
        }
    }
}

impl<'a, T> Iterator for SymmetricDifference<'a, T>
where
    T: Eq,
{
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        self.iter.next()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<T> Clone for Union<'_, T> {
    #[inline]
    fn clone(&self) -> Self {
        Union {
            iter: self.iter.clone(),
        }
    }
}

impl<'a, T> Iterator for Union<'a, T>
where
    T: Eq,
{
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        self.iter.next()
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

#[allow(dead_code)]
fn assert_covariance() {
    fn set<'new>(v: LinearSet<&'static str>) -> LinearSet<&'new str> {
        v
    }
    fn iter<'a, 'new>(v: Iter<'a, &'static str>) -> Iter<'a, &'new str> {
        v
    }
    fn into_iter<'new>(v: IntoIter<&'static str>) -> IntoIter<&'new str> {
        v
    }
    fn difference<'a, 'new>(v: Difference<'a, &'static str>) -> Difference<'a, &'new str> {
        v
    }
    fn symmetric_difference<'a, 'new>(
        v: SymmetricDifference<'a, &'static str>,
    ) -> SymmetricDifference<'a, &'new str> {
        v
    }
    fn intersection<'a, 'new>(v: Intersection<'a, &'static str>) -> Intersection<'a, &'new str> {
        v
    }
    fn union<'a, 'new>(v: Union<'a, &'static str>) -> Union<'a, &'new str> {
        v
    }
}

#[cfg(feature = "serde")]
impl<T> serde::Serialize for LinearSet<T>
where
    T: serde::Serialize + Eq,
{
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(self.iter())
    }
}

#[cfg(feature = "serde")]
impl<'de, T> serde::Deserialize<'de> for LinearSet<T>
where
    T: serde::Deserialize<'de> + Eq,
{
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use core::marker::PhantomData;
        use serde::de::SeqAccess;
        use serde::de::Visitor;

        struct LinearSetSeqVisitor<T>(PhantomData<T>);

        impl<'de, T> Visitor<'de> for LinearSetSeqVisitor<T>
        where
            T: serde::Deserialize<'de> + Eq,
        {
            type Value = LinearSet<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                formatter.write_str("a sequence of T elements")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut set = LinearSet::with_capacity(seq.size_hint().unwrap_or(0));

                while let Some(elt) = seq.next_element::<T>()? {
                    set.insert(elt);
                }

                Ok(set)
            }
        }

        let set = deserializer.deserialize_seq(LinearSetSeqVisitor::<T>(PhantomData))?;
        Ok(set)
    }
}

#[cfg(feature = "speedy")]
impl<'a, C, T> speedy::Readable<'a, C> for LinearSet<T>
where
    C: speedy::Context,
    T: speedy::Readable<'a, C> + Eq,
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
impl<C, T> speedy::Writable<C> for LinearSet<T>
where
    C: speedy::Context,
    T: speedy::Writable<C>,
{
    #[inline]
    fn write_to<W: ?Sized + speedy::Writer<C>>(
        &self,
        writer: &mut W,
    ) -> Result<(), <C as speedy::Context>::Error> {
        self.map.write_to(writer)
    }

    #[inline]
    fn bytes_needed(&self) -> Result<usize, <C as speedy::Context>::Error> {
        self.map.bytes_needed()
    }
}
