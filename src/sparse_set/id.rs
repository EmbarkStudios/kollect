use core::marker::PhantomData;
use core::num::NonZeroU32;
use core::num::NonZeroU64;

/// Implemented for id types that can be used as keys in [`TypedSparseSet`][super::TypedSparseSet]s.
///
/// Must track both an index and a version for each index.
///
/// You can generate types that implement this trait with the [`define_id!`] macro.
pub trait SetId: std::fmt::Display + Copy {
    fn as_raw(&self) -> RawId;
    fn from_raw(raw: RawId) -> Self;
    fn is_invalid(&self) -> bool;
    fn is_server_id(&self) -> bool;
    fn index(&self) -> usize;
    fn version(&self) -> NonZeroU32;
}

pub type RawId = NonZeroU64;

/// Create your own [`SetId`] type.
///
/// The id will be represented internally by a [`RawId`], with the following layout:
///
/// Lower 32 bits contain the main index
/// Upper 31 bits contain nonzero version (i.e. beginning at 1)
/// Final 1 highest bit contains client/server flag, where 1 means client.
///
/// The server tag is 0 so that server ids can be encoded efficiently as two
/// varints, one for low 32 bits and one for high 32 bits. Add `#[with_speedy]` as seen in example below
/// to implement this through `speedy` (you'll need to add `speedy` as a dependency
/// in the crate you use this macro for it to work).
///
/// # Example
///
/// ```
/// kollect::define_id! {
///     /// Yay, my own Id!
///     pub struct MyEntityId;
/// }
///
/// kollect::define_id! {
///     #[with_speedy] // must come before other meta...
///     /// Wow, speedy impl!
///     pub struct MyEntityIdWithSpeedy;
/// }
/// ```
#[macro_export]
macro_rules! define_id {
    {
        #[with_speedy]
        $(#[$meta:meta])*
        $vis:vis struct $name:ident;
    } => {
$crate::define_id! {
    $(#[$meta])*
    $vis struct $name;
}

const _: () = {
    use $crate::sparse_set::RawId;

    impl<'a, C> speedy::Readable<'a, C> for $name
    where
        C: speedy::Context,
    {
        #[inline]
        fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
            let upper = reader.read_u64_varint()?;
            let lower = reader.read_u64_varint()?;

            Ok(Self {
                raw: RawId::new((upper << 32) | lower).expect("nonzero id"),
            })
        }

        #[inline]
        fn minimum_bytes_needed() -> usize {
            1
        }
    }

    impl<C> speedy::Writable<C> for $name
    where
        C: speedy::Context,
    {
        #[inline]
        fn write_to<T: ?Sized + speedy::Writer<C>>(&self, writer: &mut T) -> Result<(), C::Error> {
            let upper = self.raw.get() >> 32;
            let lower = self.raw.get() & 0xffff_ffff;
            writer.write_u64_varint(upper)?;
            writer.write_u64_varint(lower)?;
            Ok(())
        }

        #[inline]
        fn bytes_needed(&self) -> Result<usize, C::Error> {
            use speedy::private::VarInt64;
            use speedy::Writable;
            let upper: VarInt64 = (self.raw.get() >> 32).into();
            let lower: VarInt64 = (self.raw.get() & 0xffff_ffff).into();
            Ok(<VarInt64 as Writable<C>>::bytes_needed(&upper)?
                + <VarInt64 as Writable<C>>::bytes_needed(&lower)?)
        }
    }
};

    };

    {
        $(#[$meta:meta])*
        $vis:vis struct $name:ident;
    } => {
#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
$(#[$meta])*
pub struct $name {
    raw: $crate::sparse_set::RawId,
}

const _: () = {
    use std::num::NonZeroU32;
    use $crate::sparse_set::SetId;
    use $crate::sparse_set::RawId;

    impl SetId for $name {
        #[inline(always)]
        fn as_raw(&self) -> RawId {
            self.as_raw()
        }

        #[inline(always)]
        fn from_raw(raw: RawId) -> Self {
            Self::from_raw_checked(raw)
        }

        #[inline(always)]
        fn is_invalid(&self) -> bool {
            self == &Self::invalid()
        }

        #[inline(always)]
        fn index(&self) -> usize {
            self.index()
        }

        #[inline(always)]
        fn version(&self) -> NonZeroU32 {
            self.version()
        }

        #[inline(always)]
        fn is_server_id(&self) -> bool {
            self.is_server_id()
        }
    }

    impl std::fmt::Debug for $name {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if self.is_server_id() {
                write!(f, "server({}v{})", self.index(), self.version().get())
            } else {
                write!(f, "client({}v{})", self.index(), self.version().get())
            }
        }
    }

    impl std::fmt::Display for $name {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    #[allow(dead_code)]
    impl $name {
        #[inline]
        pub const fn invalid() -> Self {
            Self {
                // SAFETY: Invalid is non-zero.
                raw: unsafe { RawId::new_unchecked(0x0000_0001_FFFF_FFFF) },
            }
        }

        #[inline]
        pub const fn as_raw(&self) -> RawId {
            self.raw
        }

        #[inline]
        const fn try_get_version(repr: u64) -> Option<NonZeroU32> {
            NonZeroU32::new(((repr >> 32) as u32) & 0x7FFF_FFFF)
        }

        /// # Safety
        ///
        /// must ensure version in the given `RawId` is nonzero
        #[inline]
        unsafe fn get_version_unchecked(raw: RawId) -> NonZeroU32 {
            // SAFETY: version being nonzero is checked at construction time
            unsafe { NonZeroU32::new_unchecked(((raw.get() >> 32) as u32) & 0x7FFF_FFFF) }
        }

        #[inline]
        pub const fn from_u64(repr: u64) -> Self {
            // Layout of id is:
            // Upper 32-bits a non-zero version,
            // Lower 32-bits an "index".

            if Self::try_get_version(repr).is_none() {
                panic!("Expected non-zero version!");
            }
            Self {
                // SAFETY: Version is already non-zero
                raw: unsafe { RawId::new_unchecked(repr) },
            }
        }

        #[inline]
        pub fn from_raw_checked(repr: RawId) -> Self {
            let _validate = Self::try_get_version(repr.get()).expect("Expected non-zero version!");

            Self { raw: repr }
        }

        /// Make a new server id from index and version. Version must be nonzero.
        #[inline]
        pub fn from_index_and_version_server(index: usize, version: u32) -> Self {
            assert!(version != 0);
            Self {
                // SAFETY: Version is non-zero
                raw: unsafe {
                    RawId::new_unchecked(
                        (((version as u64) & 0x7FFF_FFFF) << 32) | (index as u64),
                    )
                },
            }
        }

        /// Make a new (client/standalone) id from index and version. Version must be nonzero.
        #[inline]
        pub fn from_index_and_version(index: usize, version: u32) -> Self {
            let mut id = Self::from_index_and_version_server(index, version);
            id.raw |= (1 << 63);
            id
        }

        #[inline]
        pub fn version(&self) -> NonZeroU32 {
            // SAFETY: This is already verified at construction time.
            unsafe { Self::get_version_unchecked(self.raw) }
        }

        #[inline]
        pub fn index(&self) -> usize {
            (self.raw.get() & 0xffff_ffff) as usize
        }

        #[inline]
        pub fn is_server_id(&self) -> bool {
            (self.raw.get() & (1 << 63)) == 0
        }

        #[inline]
        pub fn is_client_id(&self) -> bool {
            !self.is_server_id()
        }
    }
};

    };
}

/// Allocator for [`SetId`]s.
///
/// This is separated from the [`TypedSparseSet`][super::TypedSparseSet]
/// itself so that id allocation can be decoupled from concrete sets themselves.
///
/// It is aware of in what context it is running (client/server) in order to allocate client ids
/// that won't collide with the ids allocated on the server and vice-versa.
/// The top bit indicates if it's a server or client id, with 1 indicating client and 0 indicating
/// server. This is designed to work with id types generated via [`define_id!`].
#[derive(Debug, Clone)]
pub struct IdAllocator<T> {
    // Indices
    free: Vec<u32>,

    // Versions
    slots: Vec<NonZeroU32>,

    // Are we running as a server or in client/standalone mode.
    is_server: bool,

    _phantom: PhantomData<fn() -> T>,
}

impl<T: SetId> IdAllocator<T> {
    pub fn new_client_or_standalone() -> Self {
        Self {
            free: Vec::new(),
            slots: Vec::new(),
            is_server: false,
            _phantom: PhantomData,
        }
    }
    pub fn new_server() -> Self {
        Self {
            free: Vec::new(),
            slots: Vec::new(),
            is_server: false,
            _phantom: PhantomData,
        }
    }

    /// Allocate a new id
    ///
    /// Reuses previously freed slots, then allocates new ones if there are no free slots available
    pub fn allocate(&mut self) -> T {
        if let Some(free_idx) = self.free.pop() {
            let slot_version = &mut self.slots[free_idx as usize];
            let new_version = (*slot_version)
                .checked_add(1)
                .expect("version shouldnt overflow");
            assert!(new_version.get() != 0xffff_ffff);
            *slot_version = new_version;

            let new_version = if self.is_server {
                new_version.get()
            } else {
                new_version.get() | (1 << 31)
            };

            // SAFETY: version is nonzero
            let raw =
                unsafe { RawId::new_unchecked(((new_version as u64) << 32) | free_idx as u64) };

            T::from_raw(raw)
        } else {
            let idx = self.slots.len();

            // SAFETY: 1 is not 0
            self.slots.push(unsafe { NonZeroU32::new_unchecked(1) });

            let version = if self.is_server { 1 } else { 1 | (1 << 31) };

            // SAFETY: version is nonzero
            let raw = unsafe { RawId::new_unchecked(((version as u64) << 32) | idx as u64) };

            T::from_raw(raw)
        }
    }

    /// Remove an id. Returns true if successfully removed, false if the id didn't exist so didn't
    /// remove anything.
    pub fn remove(&mut self, id: T) -> bool {
        let version = id.version();
        let index = id.index();

        if let Some(slot) = self.slots.get_mut(index) {
            if slot.get() & 0x7FFF_FFFF == version.get() {
                self.free.push(index as u32);
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Returns true if the id currently exists
    pub fn contains(&self, id: T) -> bool {
        let version = id.version();
        let index = id.index();
        self.slots.get(index).copied() == Some(version)
    }

    /// Removes all currently allocated ids, leaving empty slots in their place.
    pub fn clear(&mut self) {
        for (idx, _) in self.slots[..].iter().enumerate() {
            self.free.push(idx as u32);
        }

        if !self.is_server {
            self.free.sort();
            self.free.reverse();
        }
    }
}

#[cfg(test)]
mod test {
    define_id! {
        struct BaseId;
    }

    #[cfg(feature = "speedy")]
    define_id! {
        #[with_speedy]
        struct SpeedyId;
    }
}
