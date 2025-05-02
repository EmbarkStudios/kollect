use core::fmt::Debug;
use std::cell::UnsafeCell;
use std::num::NonZeroU32;
use std::num::NonZeroUsize;

use crate::UnorderedPrimitiveMap;
use crate::specialized_hashers::BuildPrimitiveHasher;

pub mod id;
pub mod networked_set;

pub use id::RawId;
pub use id::SetId;

/// Iterator over all `id`s and values in the set.
pub struct SparseSetIter<'a, K: SetId, V> {
    ids: std::iter::Chain<std::slice::Iter<'a, K>, std::slice::Iter<'a, K>>,
    values:
        std::iter::Chain<std::slice::Iter<'a, UnsafeCell<V>>, std::slice::Iter<'a, UnsafeCell<V>>>,
}

impl<'a, K: SetId, V> SparseSetIter<'a, K, V> {
    fn new(sparseset: &'a TypedSparseSet<K, V>) -> Self {
        // SAFETY: These accesses are guaranteed safe under normal borrowing rules (we are getting a shared ref through a shared ref).
        // The only way to break it would be misuse of unsafe methods to get mutable refs through a shared ref, for example `get_mut_dyn``,
        // in which case the caller of that unsafe method would have made the mistake, not the caller of this safe one.
        let (dense, data) = unsafe { (&*sparseset.dense.get(), &*sparseset.data.get()) };

        Self {
            ids: dense.iter().chain([].iter()),
            values: data.iter().chain([].iter()),
        }
    }

    fn new_chained(first: &'a TypedSparseSet<K, V>, second: &'a TypedSparseSet<K, V>) -> Self {
        // SAFETY: These accesses are guaranteed safe under normal borrowing rules (we are getting a shared ref through a shared ref).
        // The only way to break it would be misuse of unsafe methods to get mutable refs through a shared ref, for example `get_mut_dyn``,
        // in which case the caller of that unsafe method would have made the mistake, not the caller of this safe one.
        let (dense, data) = unsafe { (&*first.dense.get(), &*first.data.get()) };
        let (second_dense, second_data) = unsafe { (&*second.dense.get(), &*second.data.get()) };

        Self {
            ids: dense.iter().chain(second_dense.iter()),
            values: data.iter().chain(second_data.iter()),
        }
    }
}

impl<'a, K: SetId, V> Iterator for SparseSetIter<'a, K, V> {
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        // SAFETY: This access is guaranteed safe under normal borrowing rules (we are getting a shared ref through a shared ref).
        // The only way to break it would be misuse of unsafe methods to get mutable refs through a shared ref, for example `get_mut_dyn``,
        // in which case the caller of that unsafe method would have made the mistake, not the caller of this safe one.
        let value = self.values.next().map(|a| unsafe { &*a.get() });
        self.ids.next().copied().zip(value)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        min_size_hint(self.ids.size_hint(), self.values.size_hint())
    }
}

/// Iterator over all `id`s and values in the set.
pub struct SparseSetIterMut<'a, K: SetId, V> {
    ids: std::iter::Chain<std::slice::IterMut<'a, K>, std::slice::IterMut<'a, K>>,
    values: std::iter::Chain<
        std::slice::IterMut<'a, UnsafeCell<V>>,
        std::slice::IterMut<'a, UnsafeCell<V>>,
    >,
}

impl<'a, K: SetId, V> SparseSetIterMut<'a, K, V> {
    fn new(sparseset: &'a mut TypedSparseSet<K, V>) -> Self {
        Self {
            ids: sparseset.dense.get_mut().iter_mut().chain([].iter_mut()),
            values: sparseset.data.get_mut().iter_mut().chain([].iter_mut()),
        }
    }

    fn new_chained(
        first: &'a mut TypedSparseSet<K, V>,
        second: &'a mut TypedSparseSet<K, V>,
    ) -> Self {
        Self {
            ids: first
                .dense
                .get_mut()
                .iter_mut()
                .chain(second.dense.get_mut().iter_mut()),
            values: first
                .data
                .get_mut()
                .iter_mut()
                .chain(second.data.get_mut().iter_mut()),
        }
    }
}

impl<'a, K: SetId, V> Iterator for SparseSetIterMut<'a, K, V> {
    type Item = (K, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.values.next().map(|a| a.get_mut());
        self.ids.next().copied().zip(value)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        min_size_hint(self.ids.size_hint(), self.values.size_hint())
    }
}

pub struct TypedSparseSet<K: SetId, V> {
    sparse: UnsafeCell<SparseIndirectionTable>,
    dense: UnsafeCell<Vec<K>>,
    data: UnsafeCell<Vec<UnsafeCell<V>>>,
}

unsafe impl<K: SetId, V> Send for TypedSparseSet<K, V> {}
unsafe impl<K: SetId, V> Sync for TypedSparseSet<K, V> {}

#[cfg(feature = "serde")]
impl<K: SetId, V> serde::Serialize for TypedSparseSet<K, V>
where
    V: serde::Serialize,
    K: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let sparse_ref = unsafe { &*self.sparse.get() };
        let dense_ref = unsafe { &*self.dense.get() };
        let data_ref = unsafe { &*self.data.get() };
        // SAFETY: Vec does not use niche optimizations and UnsafeCell has the same memory layout
        // as T so it should be safe to convert inbetween these representations.
        let data_ref: &Vec<V> =
            unsafe { std::mem::transmute::<&Vec<UnsafeCell<V>>, &Vec<V>>(data_ref) };

        let mut state = serializer.serialize_struct("TypedSparseSet", 3)?;
        state.serialize_field("sparse", &sparse_ref)?;
        state.serialize_field("dense", &dense_ref)?;
        state.serialize_field("data", &data_ref)?;
        state.end()
    }
}

#[cfg(feature = "serde")]
impl<'de, K: SetId, V> serde::Deserialize<'de> for TypedSparseSet<K, V>
where
    V: serde::Deserialize<'de>,
    K: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(serde::Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Sparse,
            Dense,
            Data,
        }

        struct TypedSparseSetVisitor<K: SetId, V>(
            (std::marker::PhantomData<K>, std::marker::PhantomData<V>),
        );

        impl<'de, K: SetId + serde::Deserialize<'de>, V> serde::de::Visitor<'de>
            for TypedSparseSetVisitor<K, V>
        where
            V: serde::Deserialize<'de>,
        {
            type Value = TypedSparseSet<K, V>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct TypedSparseSet")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let sparse = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;
                let dense = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;
                let data: Vec<V> = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(2, &self))?;
                // SAFETY: Vec does not use niche optimizations and UnsafeCell has the same memory layout
                // as T so it should be safe to convert inbetween these representations.
                let data = unsafe { std::mem::transmute::<Vec<V>, Vec<UnsafeCell<V>>>(data) };

                Ok(TypedSparseSet {
                    sparse: UnsafeCell::new(sparse),
                    dense: UnsafeCell::new(dense),
                    data: UnsafeCell::new(data),
                })
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut sparse = None;
                let mut dense = None;
                let mut data = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Sparse => {
                            if sparse.is_some() {
                                return Err(serde::de::Error::duplicate_field("sparse"));
                            }
                            sparse = Some(map.next_value()?);
                        }
                        Field::Dense => {
                            if dense.is_some() {
                                return Err(serde::de::Error::duplicate_field("dense"));
                            }
                            dense = Some(map.next_value()?);
                        }
                        Field::Data => {
                            if data.is_some() {
                                return Err(serde::de::Error::duplicate_field("data"));
                            }
                            let data_tmp: Vec<V> = map.next_value()?;
                            // SAFETY: Vec does not use niche optimizations and UnsafeCell has the same memory layout
                            // as T so it should be safe to convert inbetween these representations.
                            data = Some(unsafe {
                                std::mem::transmute::<Vec<V>, Vec<UnsafeCell<V>>>(data_tmp)
                            });
                        }
                    }
                }
                let sparse = sparse.ok_or_else(|| serde::de::Error::missing_field("sparse"))?;
                let dense = dense.ok_or_else(|| serde::de::Error::missing_field("dense"))?;
                let data = data.ok_or_else(|| serde::de::Error::missing_field("data"))?;
                Ok(TypedSparseSet {
                    sparse: UnsafeCell::new(sparse),
                    dense: UnsafeCell::new(dense),
                    data: UnsafeCell::new(data),
                })
            }
        }

        const FIELDS: &[&str] = &["sparse", "dense", "data"];
        deserializer.deserialize_struct(
            "TypedSparseSet",
            FIELDS,
            TypedSparseSetVisitor((std::marker::PhantomData, std::marker::PhantomData)),
        )
    }
}

impl<K: SetId, T: Clone> Clone for TypedSparseSet<K, T> {
    fn clone(&self) -> Self {
        // SAFETY: Vec does not use niche optimizations and UnsafeCell has the same memory layout
        // as T so it should be safe to convert inbetween these representations.
        let data: &Vec<T> = unsafe {
            let data_ref: &Vec<_> = &*self.data.get();
            std::mem::transmute::<&Vec<UnsafeCell<T>>, &Vec<T>>(data_ref)
        };
        let data_clone = data.clone();
        // SAFETY: These accesses are guaranteed safe under normal borrowing rules.
        // The only way to break it would be misuse of unsafe methods to get mutable refs through a
        // shared ref.
        unsafe {
            Self {
                sparse: UnsafeCell::new((*self.sparse.get()).clone()),
                dense: UnsafeCell::new((*self.dense.get()).clone()),
                // SAFETY: Vec does not use niche optimizations and UnsafeCell has the same memory layout
                // as T so it should be safe to convert inbetween these representations.
                data: UnsafeCell::new(std::mem::transmute::<Vec<T>, Vec<UnsafeCell<T>>>(
                    data_clone,
                )),
            }
        }
    }
}

impl<K: SetId, V> Default for TypedSparseSet<K, V> {
    fn default() -> Self {
        Self {
            sparse: Default::default(),
            dense: Default::default(),
            data: Default::default(),
        }
    }
}

impl<K: SetId, V> TypedSparseSet<K, V> {
    pub fn reserve(&mut self, capacity: usize) {
        self.sparse.get_mut().values.reserve(capacity);
        self.dense.get_mut().reserve(capacity);
        self.data.get_mut().reserve(capacity);
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            sparse: UnsafeCell::new(SparseIndirectionTable::with_capacity(capacity)),
            dense: UnsafeCell::new(Vec::with_capacity(capacity)),
            data: UnsafeCell::new(Vec::with_capacity(capacity)),
        }
    }

    /// Returns `true` if the sparse set contains a value for the given id.
    #[inline]
    pub fn contains_key(&self, id: K) -> bool {
        // SAFETY: no API hands out borrows into `SparseIndirectionTable` meaning that no mutable aliases can exist.
        unsafe { &*self.sparse.get() }.contains(id)
    }

    #[inline]
    pub fn is_older_than_existing(&self, id: K) -> bool {
        // SAFETY: no API hands out borrows into `SparseIndirectionTable` meaning that no mutable aliases can exist.
        unsafe { &*self.sparse.get() }.is_older(id)
    }

    /// Clears the sparse set, removing all values.
    ///
    /// Note that this method has no effect on the allocated capacity of the sparse set.
    pub fn clear(&mut self) {
        self.sparse.get_mut().clear();
        self.dense.get_mut().clear();
        self.data.get_mut().clear();
    }

    /// Returns a reference to an element in the sparse set.
    pub fn get(&self, id: K) -> Option<&V> {
        // SAFETY: These accesses are guaranteed safe under normal borrowing rules (we are getting a shared ref through a shared ref).
        // The only way to break it would be misuse of unsafe methods to get mutable refs through a shared ref, for example `get_mut_dyn``,
        // in which case the caller of that unsafe method would have made the mistake, not the caller of this safe one.
        let (sparse, data) = unsafe { (&*self.sparse.get(), &*self.data.get()) };

        sparse.get_exact_version(id).map(|dense_index| {
            // SAFETY: the spareset data structure ensures that the index is valid
            let cell = unsafe { data.get_unchecked(dense_index) };

            // SAFETY: This access is guaranteed safe under normal borrowing rules (we are getting a shared ref through a shared ref).
            // The only way to break it would be misuse of unsafe methods to get mutable refs through a shared ref, for example `get_mut_dyn``,
            // in which case the caller of that unsafe method would have made the mistake, not the caller of this safe one.
            unsafe { &*cell.get() }
        })
    }

    /// Returns a mutable reference to an element in the sparse set.
    pub fn get_mut(&mut self, id: K) -> Option<&mut V> {
        let sparse = self.sparse.get_mut();

        sparse.get_exact_version(id).map(|dense_index| {
            // SAFETY: the spareset data structure ensures that the index is valid.
            let cell = unsafe { self.data.get_mut().get_unchecked_mut(dense_index) };

            cell.get_mut()
        })
    }

    /// Inserts the `id` key and `value` into this sparse set.
    ///
    /// Returns `true` when the *exact* key was already present in the sparse set
    /// otherwise `false` (i.e. if overwrote an older version that still existed, return `false`).
    ///
    /// Will panic if the key is older than the existing key in the set.
    /// Use `insert_ignore_old_key` if you want to silently skip insertion
    /// for older keys instead of panicking.
    #[inline(always)]
    pub fn insert(&mut self, id: K, value: V) -> bool {
        self.insert_impl(id, value, false)
    }

    /// Inserts the `id` key and `value` into this sparse set.
    ///
    /// Returns `true` when the *exact* key was already present in the sparse set
    /// otherwise `false` (i.e. if overwrote an older version that still existed, return `false`).
    ///
    /// Use `insert` if you want to assert that the key is newer or the same
    /// version as the exiting keys.
    #[inline(always)]
    pub fn insert_ignore_old_key(&mut self, id: K, value: V) -> bool {
        self.insert_impl(id, value, true)
    }

    fn insert_impl(&mut self, id: K, value: V, ignore_old_keys: bool) -> bool {
        assert!(!id.is_invalid());

        let sparse = self.sparse.get_mut();

        if let Some(dense_index) = sparse.get_older_or_exact_version(id) {
            // SAFETY: We know this item exists.
            unsafe {
                sparse.bump_version_unchecked(id);
            };

            // Important:
            // Version may have changed for this id index (i.e be a newer one),
            // thus we need to update it in the "dense" map.
            let old_key = self.dense.get_mut()[dense_index];
            self.dense.get_mut()[dense_index] = id;
            self.data.get_mut()[dense_index] = UnsafeCell::new(value);

            // If we overwrote an older version, we return `false`
            old_key.version() == id.version()
        } else {
            let success = sparse.set(id, self.dense.get_mut().len());

            if !ignore_old_keys {
                // If we tried to insert an older item we assert.
                debug_assert!(success, "Tried to insert an older key! {id}");
            }
            if success {
                self.dense.get_mut().push(id);
                self.data.get_mut().push(UnsafeCell::new(value));
            }

            false
        }
    }

    pub fn retain(&mut self, mut fun: impl FnMut(K, &mut V) -> bool) {
        // This is shit
        // TODO: Optimize :) (contributions welcome!)
        let mut to_remove = Vec::new();
        for (ent, val) in self.iter_mut() {
            if !fun(ent, val) {
                to_remove.push(ent);
            }
        }
        for ent in to_remove {
            self.remove(ent);
        }
    }

    /// Removes the id from this sparse set and returns the removed element.
    pub fn remove(&mut self, id: K) -> Option<V> {
        let sparse = self.sparse.get_mut();

        sparse.remove(id).map(|dense_index| {
            let dense = self.dense.get_mut();

            dense.swap_remove(dense_index);

            if dense_index < dense.len() {
                // If we don't end up in this block
                // it means we removed the final element.
                // there would be nothing in the sparse map to
                // update.
                //
                // Otherwise we just update the sparse map to
                // point to the right entry in the dense data.
                let swapped_entity = dense[dense_index];
                sparse.set(swapped_entity, dense_index);
            }

            self.data.get_mut().swap_remove(dense_index).into_inner()
        })
    }

    /// Unsafely returns a mutable reference to an element in the sparse set.
    ///
    /// # Safety
    ///
    /// The caller must ensure that there is no *mutable* access to the entire `data` storage
    /// as a whole (though there may be shared access), **and** that the access to the
    /// specific id's data is unique (no active references, mutable or not).
    #[inline]
    pub unsafe fn get_mut_unchecked(&self, id: K) -> Option<&mut V> {
        // SAFETY: no API hands out borrows into `SparseIndirectionTable` and there are no
        // reentrant calls to `get_mut_unchecked` which means that the access is unique.
        let sparse = unsafe { &*self.sparse.get() };

        sparse.get_exact_version(id).map(|dense_index| {
            // SAFETY: the caller must ensure that there is only shared access to the `data` array itself
            let data = unsafe { &*self.data.get() };

            // SAFETY: the spareset data structure ensures that the index is valid.
            let cell = unsafe { data.get_unchecked(dense_index) };

            // SAFETY: the caller must guarantee unique access to the value data, making this dereference safe.
            unsafe { &mut *cell.get() }
        })
    }

    /// Unsafely inserts the `id` key and `value` into this sparse set.
    ///
    /// # Safety
    ///
    /// The caller must ensure that access to the _entire_ `data` store as well as the `dense` id store are unique
    /// (no active references, mutable or not). This is required as the insert can cause reallocations, making existing
    /// references invalid.
    #[inline]
    pub unsafe fn insert_unchecked(&self, id: K, value: V) -> bool {
        assert!(!id.is_invalid());

        // SAFETY: no API hands out borrows into `SparseIndirectionTable` and there are no
        // reentrant calls to `insert_unchecked` which means that the access is unique.
        let sparse = unsafe { &mut *self.sparse.get() };

        // SAFETY: the caller must guarantee that access to the entire `TypedSparseSet` is unique. As a result, dereferencing
        // `self.dense` and `self.data` should be safe.
        let (dense, data) = unsafe { (&mut *self.dense.get(), &mut *self.data.get()) };

        if let Some(dense_index) = sparse.get_older_or_exact_version(id) {
            // SAFETY: We know this item exists.
            unsafe {
                sparse.bump_version_unchecked(id);
            };

            // Important:
            // Version may have changed for this id index (i.e be a newer one),
            // thus we need to update it in the "dense" map.
            let old_key = dense[dense_index];
            dense[dense_index] = id;
            data[dense_index] = UnsafeCell::new(value);

            // If we overwrote an older version, we return `false`
            old_key.version() == id.version()
        } else {
            let success = sparse.set(id, dense.len());

            // If we tried to insert an older item we assert.
            assert!(success, "Tried to insert an older id!");

            dense.push(id);
            data.push(UnsafeCell::new(value));

            false
        }
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.keys().is_empty()
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.keys().len()
    }

    #[inline]
    pub fn keys(&self) -> &[K] {
        // SAFETY: This access is guaranteed safe under normal borrowing rules (we are getting a shared ref through a shared ref).
        // The only way to break it would be misuse of unsafe methods to get mutable refs through a shared ref, for example `get_mut_dyn``,
        // in which case the caller of that unsafe method would have made the mistake, not the caller of this safe one.
        unsafe { &*self.dense.get() }
    }

    #[inline]
    pub fn iter(&self) -> SparseSetIter<'_, K, V> {
        SparseSetIter::new(self)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.iter_mut().map(|(_, v)| v)
    }

    #[inline]
    pub fn iter_mut(&mut self) -> SparseSetIterMut<'_, K, V> {
        SparseSetIterMut::new(self)
    }
}

/// We use `NonZeroUsize` so that the compiler optimizes the size of the
/// option flag away.
#[derive(Default, Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct VersionAndDenseIndex {
    // If this is `None` it means the element has never been used.
    version: Option<NonZeroU32>,

    // If this is `None` it means the element has either never
    // been used or the data has been cleared.
    dense_index: Option<NonZeroUsize>,
}

#[derive(Default, Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct SparseIndirectionTable {
    values: UnorderedPrimitiveMap<usize, VersionAndDenseIndex>,
}

impl SparseIndirectionTable {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            values: UnorderedPrimitiveMap::with_capacity_and_hasher(capacity, BuildPrimitiveHasher),
        }
    }
    /// Returns `true` if the sparse array contains the index.
    #[inline]
    fn contains(&self, id: impl SetId) -> bool {
        let index = id.index();

        if let Some(VersionAndDenseIndex {
            version: Some(version),
            dense_index: Some(_),
        }) = self.values.get(&index)
        {
            *version == id.version()
        } else {
            false
        }
    }

    #[inline]
    fn is_older(&self, id: impl SetId) -> bool {
        let index = id.index();

        if let Some(VersionAndDenseIndex {
            version: Some(version),
            dense_index: Some(_),
        }) = self.values.get(&index)
        {
            *version > id.version()
        } else {
            false
        }
    }

    /// Clears the sparse array, removing all values.
    ///
    /// Note that this method has no effect on the allocated capacity of the sparse set.
    #[inline]
    fn clear(&mut self) {
        for v in self.values.values_mut() {
            v.dense_index = None;
        }
    }

    /// Gets an item given a comparator for the id vs slot version.
    fn get<F: Fn(NonZeroU32, NonZeroU32) -> bool>(
        &self,
        id: impl SetId,
        version_cmp: F,
    ) -> Option<usize> {
        // Perform a manual map to give the optimizer the best chance.
        let index = id.index();

        #[allow(clippy::manual_map)]
        if let Some(VersionAndDenseIndex {
            version: Some(version),
            dense_index: Some(dense_index),
        }) = self.values.get(&index)
        {
            version_cmp(id.version(), *version).then_some(dense_index.get() - 1)
        } else {
            None
        }
    }

    /// Returns the dense index mapped to the sparse index where the slot has to have a
    /// version that is exactly equal to the id.
    #[inline]
    fn get_exact_version(&self, id: impl SetId) -> Option<usize> {
        self.get(id, |entity_version, slot_version| {
            entity_version == slot_version
        })
    }

    /// Returns the dense index mapped to the sparse index where the slot has to have a
    /// version that is exactly equal or older than the id.
    #[inline]
    fn get_older_or_exact_version(&self, id: impl SetId) -> Option<usize> {
        self.get(id, |entity_version, slot_version| {
            entity_version >= slot_version
        })
    }

    /// Bumps version of an id
    ///
    /// # Safety
    ///
    /// The entry must exist
    unsafe fn bump_version_unchecked(&mut self, id: impl SetId) {
        // SAFETY: same as function level safety
        unsafe {
            self.values.get_mut(&id.index()).unwrap_unchecked().version = Some(id.version());
        }
    }

    #[inline]
    fn set(&mut self, id: impl SetId, dense_index: usize) -> bool {
        let index = id.index();

        // Perform a manual map to give the optimizer the best chance.
        #[allow(clippy::manual_map)]
        let dense_index = match NonZeroUsize::new(dense_index + 1) {
            Some(i) => i,
            None => overflowed_index(),
        };

        let entry = self.values.entry(index).or_default();

        let version = if let Some(version) = entry.version {
            version
        } else {
            id.version()
        };

        if id.version() >= version {
            entry.version = Some(id.version());
            entry.dense_index = Some(dense_index);
            return true;
        }

        false
    }

    /// Removes and returns the dense index at position `index`.
    #[inline]
    fn remove(&mut self, id: impl SetId) -> Option<usize> {
        // Perform a manual map to give the optimizer the best chance.
        #[allow(clippy::manual_map)]
        if let Some(VersionAndDenseIndex {
            version: Some(version),
            dense_index,
        }) = self.values.get_mut(&id.index())
        {
            if *version == id.version() {
                let prev_dense_index = (*dense_index)?;
                *dense_index = None;
                Some(prev_dense_index.get() - 1)
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[cold]
fn overflowed_index() -> ! {
    panic!("dense_index + 1 overflowed")
}

#[inline]
fn min_size_hint(a: (usize, Option<usize>), b: (usize, Option<usize>)) -> (usize, Option<usize>) {
    // based on `std::iter::Zip::size_hint`
    // https://github.com/rust-lang/rust/blob/65519f5fc0bcd8a47e547226bbc7498a2b9a59fb/library/core/src/iter/adapters/zip.rs#L209

    let (a_lower, a_upper) = a;
    let (b_lower, b_upper) = b;

    let lower = std::cmp::min(a_lower, b_lower);

    let upper = match (a_upper, b_upper) {
        (Some(x), Some(y)) => Some(std::cmp::min(x, y)),
        (Some(x), None) => Some(x),
        (None, Some(y)) => Some(y),
        (None, None) => None,
    };

    (lower, upper)
}

#[cfg(test)]
mod test {
    use super::TypedSparseSet;
    use super::id::IdAllocator;
    use crate::define_id;

    define_id! {
        #[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
        pub struct MyEntityId;
    }

    #[derive(Copy, Clone, Debug, PartialEq)]
    #[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
    struct MyData {
        value: u32,
    }

    fn new_ent_id(idx: usize, version: u32) -> MyEntityId {
        MyEntityId::from_index_and_version(idx, version)
    }

    #[test]
    #[should_panic]
    pub fn test_sparse_set_inserting_older_item() {
        let mut sparse_set: TypedSparseSet<MyEntityId, MyData> = TypedSparseSet::default();

        let entity1_old = new_ent_id(0, 1);
        let entity1_new = new_ent_id(0, 2);

        let data1 = MyData { value: 0xfefefefe };

        let already_present = sparse_set.insert(entity1_new, data1);
        assert!(!already_present);

        // This should panic as we try to insert an older id
        let already_present = sparse_set.insert(entity1_old, data1);
        assert!(!already_present);
    }

    #[test]
    pub fn test_sparse_set_inserting_older_item_avoid_panic() {
        let mut sparse_set: TypedSparseSet<MyEntityId, MyData> = TypedSparseSet::default();

        let entity1_old = new_ent_id(0, 1);
        let entity1_new = new_ent_id(0, 2);

        let data1 = MyData { value: 0xfefefefe };

        let already_present = sparse_set.insert_ignore_old_key(entity1_new, data1);
        assert!(!already_present);

        let already_present = sparse_set.insert_ignore_old_key(entity1_old, data1);
        assert!(!already_present);
    }

    #[test]
    pub fn test_sparse_set() {
        let mut sparse_set: TypedSparseSet<MyEntityId, MyData> = TypedSparseSet::default();

        let entity1_old = new_ent_id(0, 1);
        let entity2_old = new_ent_id(1, 2);

        let entity1_new = new_ent_id(0, 4);
        let entity2_new = new_ent_id(1, 5);

        let data1 = MyData { value: 0xfefefefe };
        let data2 = MyData { value: 0xdeadbeef };

        assert!(!sparse_set.contains_key(entity1_old));
        assert!(!sparse_set.contains_key(entity1_new));
        assert!(!sparse_set.contains_key(entity2_old));
        assert!(!sparse_set.contains_key(entity2_new));

        let already_present = sparse_set.insert(entity1_old, data1);
        assert!(!already_present);

        let already_present = sparse_set.insert(entity2_old, data1);
        assert!(!already_present);

        assert!(sparse_set.get(entity1_old) == Some(&data1));
        assert!(sparse_set.get(entity1_new).is_none());
        assert!(sparse_set.get(entity2_old) == Some(&data1));
        assert!(sparse_set.get(entity2_new).is_none());

        assert!(sparse_set.contains_key(entity1_old));
        assert!(!sparse_set.contains_key(entity1_new));
        assert!(sparse_set.contains_key(entity2_old));
        assert!(!sparse_set.contains_key(entity2_new));

        let removed = sparse_set.remove(entity1_old);
        assert!(removed == Some(data1));

        assert!(sparse_set.get(entity1_old).is_none());
        assert!(sparse_set.get(entity1_new).is_none());
        assert!(sparse_set.get(entity2_old) == Some(&data1));
        assert!(sparse_set.get(entity2_new).is_none());

        assert!(!sparse_set.contains_key(entity1_old));
        assert!(!sparse_set.contains_key(entity1_new));
        assert!(sparse_set.contains_key(entity2_old));
        assert!(!sparse_set.contains_key(entity2_new));

        let already_present = sparse_set.insert(entity2_new, data2);
        assert!(!already_present);

        assert!(sparse_set.get(entity2_old).is_none());
        assert!(sparse_set.get(entity2_new) == Some(&data2));
        assert!(!sparse_set.contains_key(entity2_old));
        assert!(sparse_set.contains_key(entity2_new));

        let already_present = sparse_set.insert(entity1_new, data1);
        assert!(!already_present);

        let entity_ids: Vec<(MyEntityId, &MyData)> = sparse_set.iter().collect();

        assert!(entity_ids == vec![(entity2_new, &data2), (entity1_new, &data1)]);
    }

    #[test]
    fn test_sparse_set_and_id_allocator() {
        let mut set_a: TypedSparseSet<MyEntityId, MyData> = TypedSparseSet::default();
        let mut set_b: TypedSparseSet<MyEntityId, String> = TypedSparseSet::default();
        let mut alloc: IdAllocator<MyEntityId> = IdAllocator::new_client_or_standalone();

        let entity_1 = alloc.allocate();
        let entity_2 = alloc.allocate();

        let str1 = String::from("yippee");
        let data1 = MyData { value: 0xfefefefe };
        let data2 = MyData { value: 0xdeadbeef };

        set_a.insert(entity_1, data1);
        set_b.insert(entity_1, str1.clone());

        assert_eq!(set_a.get(entity_1), Some(&data1));
        assert_eq!(set_b.get(entity_1), Some(&str1));

        set_a.insert(entity_2, data2);

        assert_eq!(set_a.get(entity_2), Some(&data2));
        assert_eq!(set_b.get(entity_2), None);

        eprintln!("{entity_1:?}");
        eprintln!("{entity_2:?}");
        eprintln!("{alloc:#?}");
        assert!(alloc.remove(entity_1));

        let entity_3 = alloc.allocate();
        assert_eq!(entity_3.index(), 0);
        assert_eq!(entity_3.version().get(), 2);

        assert!(!set_a.contains_key(entity_3));
    }

    #[cfg(feature = "serde")]
    #[test]
    fn serialize_roundtrip() {
        let mut sparse_set: TypedSparseSet<MyEntityId, MyData> = TypedSparseSet::default();
        let entity1 = new_ent_id(0, 1);
        let entity2 = new_ent_id(1, 2);

        let entity3 = new_ent_id(2, 4);
        let entity4 = new_ent_id(3, 5);
        let entity4_new = new_ent_id(3, 6);

        let data1 = MyData { value: 0xfefefefe };
        let data2 = MyData { value: 0xdeadbeef };
        let data3 = MyData { value: 0xbeefefee };
        let data4 = MyData { value: 0xdeaddead };
        let data5 = MyData { value: 0xdeadfead };

        assert!(!sparse_set.contains_key(entity1));
        assert!(!sparse_set.contains_key(entity3));
        assert!(!sparse_set.contains_key(entity2));
        assert!(!sparse_set.contains_key(entity4));

        let already_present = sparse_set.insert(entity1, data1);
        assert!(!already_present);

        let already_present = sparse_set.insert(entity2, data2);
        assert!(!already_present);
        let already_present = sparse_set.insert(entity3, data3);
        assert!(!already_present);
        let already_present = sparse_set.insert(entity4, data4);
        assert!(!already_present);
        let already_present = sparse_set.insert(entity4_new, data5);
        assert!(!already_present);

        let serialized = bincode::serialize(&sparse_set).unwrap();
        let deserialized: TypedSparseSet<MyEntityId, MyData> =
            bincode::deserialize(&serialized).unwrap();

        assert_eq!(deserialized.get(entity1).unwrap(), &data1);
        assert_eq!(deserialized.get(entity2).unwrap(), &data2);
        assert_eq!(deserialized.get(entity3).unwrap(), &data3);
        assert!(!deserialized.contains_key(entity4));
        assert_eq!(deserialized.get(entity4_new).unwrap(), &data5);
        assert_eq!(deserialized.len(), 4);
    }
}
