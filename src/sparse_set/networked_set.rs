use std::ops::Index;
use std::ops::IndexMut;

use super::{SetId, SparseSetIter, SparseSetIterMut, TypedSparseSet};

#[derive(Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NetworkedSet<K: SetId, V> {
    server: TypedSparseSet<K, V>,
    client: TypedSparseSet<K, V>,
}

impl<K: SetId, T> Default for NetworkedSet<K, T> {
    fn default() -> Self {
        Self {
            server: Default::default(),
            client: Default::default(),
        }
    }
}

impl<K: SetId, V: Default> NetworkedSet<K, V> {
    /// Returns a reference to an existing element or inserts the default value
    /// and returns a reference to that.
    pub fn get_or_insert_default(&mut self, id: K) -> &V {
        if self.contains_key(id) {
            // SAFETY: We just checked existence
            return unsafe { self.get(id).unwrap_unchecked() };
        }
        self.insert(id, Default::default());
        // SAFETY: We just inserted the key
        unsafe { self.get(id).unwrap_unchecked() }
    }

    /// Returns a mutable reference to an existing element or inserts the default value
    /// and returns a mutable reference to that.
    pub fn get_mut_or_insert_default(&mut self, id: K) -> &mut V {
        if self.contains_key(id) {
            // SAFETY: We just checked existence
            return unsafe { self.get_mut(id).unwrap_unchecked() };
        }
        self.insert(id, Default::default());
        // SAFETY: We just inserted the key
        unsafe { self.get_mut(id).unwrap_unchecked() }
    }

    // Returns a mutable reference to an existing element or inserts the default value if the value didn't exist
    // or is newer than the existing one. Otherwise it returns `None`.
    pub fn try_get_mut_or_insert_default(&mut self, id: K) -> Option<&mut V> {
        if self.is_older_than_existing(id) {
            None
        } else {
            Some(self.get_mut_or_insert_default(id))
        }
    }
}

impl<K: SetId, V> NetworkedSet<K, V> {
    pub fn client(&self) -> &TypedSparseSet<K, V> {
        &self.client
    }

    pub fn client_mut(&mut self) -> &mut TypedSparseSet<K, V> {
        &mut self.client
    }

    pub fn server(&self) -> &TypedSparseSet<K, V> {
        &self.server
    }

    pub fn server_mut(&mut self) -> &mut TypedSparseSet<K, V> {
        &mut self.server
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            server: TypedSparseSet::with_capacity(capacity),
            client: TypedSparseSet::with_capacity(capacity),
        }
    }

    /// Returns `true` if the sparse set contains a value for the given id.
    #[inline]
    pub fn contains_key(&self, id: K) -> bool {
        if id.is_server_id() {
            self.server.contains_key(id)
        } else {
            self.client.contains_key(id)
        }
    }

    #[inline]
    pub fn is_older_than_existing(&self, id: K) -> bool {
        if id.is_server_id() {
            self.server.is_older_than_existing(id)
        } else {
            self.client.is_older_than_existing(id)
        }
    }

    /// Clears the sparse set storage, removing all values.
    ///
    /// Note that this method has no effect on the allocated capacity of the sparse set.
    pub fn clear(&mut self) {
        self.server.clear();
        self.client.clear();
    }

    /// Returns a reference to an element in the sparse set.
    pub fn get(&self, id: K) -> Option<&V> {
        if id.is_server_id() {
            self.server.get(id)
        } else {
            self.client.get(id)
        }
    }

    /// Returns a mutable reference to an element in the sparse set.
    pub fn get_mut(&mut self, id: K) -> Option<&mut V> {
        if id.is_server_id() {
            self.server.get_mut(id)
        } else {
            self.client.get_mut(id)
        }
    }

    /// Inserts the `id` key and `value` into this sparse set.
    ///
    /// Returns `true` when the key was already present in the sparse set
    /// otherwise `false`.
    ///
    /// Will panic if the key is older than the existing key in the set.
    /// Use `insert_ignore_old_key` if you want to silently skip insertion
    /// for older keys instead of panicking.
    pub fn insert(&mut self, id: K, value: V) -> bool {
        if id.is_server_id() {
            self.server.insert(id, value)
        } else {
            self.client.insert(id, value)
        }
    }

    /// Inserts the `id` key and `value` into this sparse set.
    ///
    /// Returns `true` when the key was already present in the sparse set
    /// otherwise `false`.
    ///
    /// Use `insert` if you want to assert that the key is newer or the same
    /// version as the exiting keys.
    pub fn insert_ignore_old_key(&mut self, id: K, value: V) -> bool {
        if id.is_server_id() {
            self.server.insert_ignore_old_key(id, value)
        } else {
            self.client.insert_ignore_old_key(id, value)
        }
    }

    /// Removes the id from this sparse set and returns the removed element.
    pub fn remove(&mut self, id: K) -> Option<V> {
        if id.is_server_id() {
            self.server.remove(id)
        } else {
            self.client.remove(id)
        }
    }

    /// Unsafely returns a mutable reference to an element in the sparse set.
    ///
    /// # Safety
    ///
    /// The caller must ensure that there is no *mutable* access to the entire `data` storage
    /// as a whole is only *shared*, **and** that the access to the specific id's data is
    /// unique (no active references, mutable or not).
    #[inline]
    pub unsafe fn get_mut_unchecked(&self, id: K) -> Option<&mut V> {
        if id.is_server_id() {
            // SAFETY: Same as function-level safety
            unsafe { self.server.get_mut_unchecked(id) }
        } else {
            // SAFETY: Same as function-level safety
            unsafe { self.client.get_mut_unchecked(id) }
        }
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
        if id.is_server_id() {
            // SAFETY: Same as function-level safety
            unsafe { self.server.insert_unchecked(id, value) }
        } else {
            // SAFETY: Same as function-level safety
            unsafe { self.client.insert_unchecked(id, value) }
        }
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.server.is_empty() && self.client.is_empty()
    }

    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.server.keys().iter().chain(self.client.keys().iter())
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.iter_mut().map(|(_, v)| v)
    }

    pub fn len(&self) -> usize {
        self.server.len() + self.client.len()
    }

    #[inline]
    pub fn iter(&self) -> SparseSetIter<'_, K, V> {
        SparseSetIter::new_chained(&self.server, &self.client)
    }

    #[inline]
    pub fn iter_mut(&mut self) -> SparseSetIterMut<'_, K, V> {
        SparseSetIterMut::new_chained(&mut self.server, &mut self.client)
    }

    pub fn retain(&mut self, mut fun: impl FnMut(K, &mut V) -> bool) {
        // This is shit
        // TODO (nummelin): Optimize :)
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
}

impl<'a, K: SetId, V: 'a + Copy> NetworkedSet<K, V> {
    /// Extends the set but ignores any older keys silently. Use `extend`
    /// if you want to assert that all data is inserted.
    pub fn extend_ignore_old_keys<I: IntoIterator<Item = (K, &'a V)>>(&mut self, iter: I) {
        let iter = iter.into_iter();
        for (k, v) in iter {
            self.insert_ignore_old_key(k, *v);
        }
    }
}

impl<K: SetId, V> Index<K> for NetworkedSet<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<K: SetId, V> IndexMut<K> for NetworkedSet<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

impl<'a, K: SetId, V: 'a + Copy> Extend<(K, &'a V)> for NetworkedSet<K, V> {
    fn extend<I: IntoIterator<Item = (K, &'a V)>>(&mut self, iter: I) {
        let iter = iter.into_iter();
        for (k, v) in iter {
            self.insert(k, *v);
        }
    }
}

impl<K: SetId, V> FromIterator<(K, V)> for NetworkedSet<K, V>
where
    K: SetId,
{
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let mut d = Self::default();
        for (k, v) in iter {
            d.insert(k, v);
        }
        d
    }
}
