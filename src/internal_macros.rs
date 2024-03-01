// taken from speedy internals impls...
//
// It's not physically possible to have a valid slice
// which is bigger than 8 exabytes. No machine exists
// which could have this much RAM, and you can't even
// have this much virtual address space on any modern CPU.
//
// So in practice this should be totally harmless while
// it will allow the LLVM optimizer to better do its job.
//
// It actually *does* affect optimization in practice
// allowing LLVM to assume the length won't overflow
// in certain cases.
#[cfg(feature = "speedy")]
macro_rules! unsafe_is_length {
    ($expr:expr) => {
        if $expr as u64 >= 0x7FFFFFFF_FFFFFFFF {
            // SAFETY: see comment above
            unsafe { std::hint::unreachable_unchecked() }
        }
    };
}

#[cfg(feature = "speedy")]
pub(crate) use unsafe_is_length;
