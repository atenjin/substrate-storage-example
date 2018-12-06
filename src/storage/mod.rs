extern crate substrate_primitives as primitives;

use super::*;
use std::borrow::Borrow;
use std::cmp;
use super::codec::Input;

use self::primitives::twox_128;

pub mod generator;
use self::generator::Storage as GenericStorage;

struct IncrementalInput<'a> {
    key: &'a [u8],
    pos: usize,
}

impl<'a> Input for IncrementalInput<'a> {
    fn read(&mut self, into: &mut [u8]) -> usize {
        let len = read_storage(self.key, into, self.pos).unwrap_or(0);
        let read = cmp::min(len, into.len());
        self.pos += read;
        read
    }
}

/// Return the value of the item in storage under `key`, or `None` if there is no explicit entry.
pub fn get<T: Codec + Sized>(key: &[u8]) -> Option<T> {
    let key = twox_128(key);
    read_storage(&key[..], &mut [0; 0][..], 0).map(|_| {
        let mut input = IncrementalInput {
            key: &key[..],
            pos: 0,
        };
        Decode::decode(&mut input).expect("storage is not null, therefore must be a valid type")
    })
}

/// Return the value of the item in storage under `key`, or the type's default if there is no
/// explicit entry.
pub fn get_or_default<T: Codec + Sized + Default>(key: &[u8]) -> T {
    get(key).unwrap_or_else(Default::default)
}

/// Return the value of the item in storage under `key`, or `default_value` if there is no
/// explicit entry.
pub fn get_or<T: Codec + Sized>(key: &[u8], default_value: T) -> T {
    get(key).unwrap_or(default_value)
}

/// Return the value of the item in storage under `key`, or `default_value()` if there is no
/// explicit entry.
pub fn get_or_else<T: Codec + Sized, F: FnOnce() -> T>(key: &[u8], default_value: F) -> T {
    get(key).unwrap_or_else(default_value)
}

/// Put `value` in storage under `key`.
pub fn put<T: Codec>(key: &[u8], value: &T) {
    value.using_encoded(|slice| set_storage(&twox_128(key)[..], slice));
}

/// Remove `key` from storage, returning its value if it had an explicit entry or `None` otherwise.
pub fn take<T: Codec + Sized>(key: &[u8]) -> Option<T> {
    let r = get(key);
    if r.is_some() {
        kill(key);
    }
    r
}

/// Remove `key` from storage, returning its value, or, if there was no explicit entry in storage,
/// the default for its type.
pub fn take_or_default<T: Codec + Sized + Default>(key: &[u8]) -> T {
    take(key).unwrap_or_else(Default::default)
}

/// Return the value of the item in storage under `key`, or `default_value` if there is no
/// explicit entry. Ensure there is no explicit entry on return.
pub fn take_or<T: Codec + Sized>(key: &[u8], default_value: T) -> T {
    take(key).unwrap_or(default_value)
}

/// Return the value of the item in storage under `key`, or `default_value()` if there is no
/// explicit entry. Ensure there is no explicit entry on return.
pub fn take_or_else<T: Codec + Sized, F: FnOnce() -> T>(key: &[u8], default_value: F) -> T {
    take(key).unwrap_or_else(default_value)
}

/// Check to see if `key` has an explicit entry in storage.
pub fn exists(key: &[u8]) -> bool {
    exists_storage(&twox_128(key)[..])
}

/// Ensure `key` has no explicit entry in storage.
pub fn kill(key: &[u8]) {
    clear_storage(&twox_128(key)[..]);
}

/// Get a Vec of bytes from storage.
pub fn get_raw(key: &[u8]) -> Option<Vec<u8>> {
    storage(&twox_128(key)[..])
}

/// Put a raw byte slice into storage.
pub fn put_raw(key: &[u8], value: &[u8]) {
    set_storage(&twox_128(key)[..], value)
}

/// The underlying runtime storage.
pub struct RuntimeStorage;

impl GenericStorage for RuntimeStorage {
    fn exists(&self, key: &[u8]) -> bool {
        super::storage::exists(key)
    }

    /// Load the bytes of a key from storage. Can panic if the type is incorrect.
    fn get<T: Codec>(&self, key: &[u8]) -> Option<T> {
        super::storage::get(key)
    }

    /// Put a value in under a key.
    fn put<T: Codec>(&self, key: &[u8], val: &T) {
        super::storage::put(key, val)
    }

    /// Remove the bytes of a key from storage.
    fn kill(&self, key: &[u8]) {
        super::storage::kill(key)
    }

    /// Take a value from storage, deleting it after reading.
    fn take<T: Codec>(&self, key: &[u8]) -> Option<T> {
        super::storage::take(key)
    }
}

/// A trait for working with macro-generated storage values under the substrate storage API.
pub trait StorageValue<T: Codec> {
    /// The type that get/take return.
    type Query;

    /// Get the storage key.
    fn key() -> &'static [u8];

    /// Does the value (explicitly) exist in storage?
    fn exists() -> bool;

    /// Load the value from the provided storage instance.
    fn get() -> Self::Query;

    /// Store a value under this key into the provided storage instance.
    fn put<Arg: Borrow<T>>(val: Arg);

    /// Mutate the value
    fn mutate<R, F: FnOnce(&mut Self::Query) -> R>(f: F) -> R;

    /// Clear the storage value.
    fn kill();

    /// Take a value from storage, removing it afterwards.
    fn take() -> Self::Query;
}

impl<T: Codec, U> StorageValue<T> for U where U: generator::StorageValue<T> {
    type Query = U::Query;

    fn key() -> &'static [u8] {
        <U as generator::StorageValue<T>>::key()
    }
    fn exists() -> bool {
        U::exists(&RuntimeStorage)
    }
    fn get() -> Self::Query {
        U::get(&RuntimeStorage)
    }
    fn put<Arg: Borrow<T>>(val: Arg) {
        U::put(val.borrow(), &RuntimeStorage)
    }
    fn mutate<R, F: FnOnce(&mut Self::Query) -> R>(f: F) -> R {
        U::mutate(f, &RuntimeStorage)
    }
    fn kill() {
        U::kill(&RuntimeStorage)
    }
    fn take() -> Self::Query {
        U::take(&RuntimeStorage)
    }
}

/// A strongly-typed map in storage.
pub trait StorageMap<K: Codec, V: Codec> {
    /// The type that get/take return.
    type Query;

    /// Get the prefix key in storage.
    fn prefix() -> &'static [u8];

    /// Get the storage key used to fetch a value corresponding to a specific key.
    fn key_for<KeyArg: Borrow<K>>(key: KeyArg) -> Vec<u8>;

    /// Does the value (explicitly) exist in storage?
    fn exists<KeyArg: Borrow<K>>(key: KeyArg) -> bool;

    /// Load the value associated with the given key from the map.
    fn get<KeyArg: Borrow<K>>(key: KeyArg) -> Self::Query;

    /// Store a value to be associated with the given key from the map.
    fn insert<KeyArg: Borrow<K>, ValArg: Borrow<V>>(key: KeyArg, val: ValArg);

    /// Remove the value under a key.
    fn remove<KeyArg: Borrow<K>>(key: KeyArg);

    /// Mutate the value under a key.
    fn mutate<KeyArg: Borrow<K>, R, F: FnOnce(&mut Self::Query) -> R>(key: KeyArg, f: F) -> R;

    /// Take the value under a key.
    fn take<KeyArg: Borrow<K>>(key: KeyArg) -> Self::Query;
}

impl<K: Codec, V: Codec, U> StorageMap<K, V> for U where U: generator::StorageMap<K, V> {
    type Query = U::Query;

    fn prefix() -> &'static [u8] {
        <U as generator::StorageMap<K, V>>::prefix()
    }

    fn key_for<KeyArg: Borrow<K>>(key: KeyArg) -> Vec<u8> {
        <U as generator::StorageMap<K, V>>::key_for(key.borrow())
    }

    fn exists<KeyArg: Borrow<K>>(key: KeyArg) -> bool {
        U::exists(key.borrow(), &RuntimeStorage)
    }

    fn get<KeyArg: Borrow<K>>(key: KeyArg) -> Self::Query {
        U::get(key.borrow(), &RuntimeStorage)
    }

    fn insert<KeyArg: Borrow<K>, ValArg: Borrow<V>>(key: KeyArg, val: ValArg) {
        U::insert(key.borrow(), val.borrow(), &RuntimeStorage)
    }

    fn remove<KeyArg: Borrow<K>>(key: KeyArg) {
        U::remove(key.borrow(), &RuntimeStorage)
    }

    fn mutate<KeyArg: Borrow<K>, R, F: FnOnce(&mut Self::Query) -> R>(key: KeyArg, f: F) -> R {
        U::mutate(key.borrow(), f, &RuntimeStorage)
    }

    fn take<KeyArg: Borrow<K>>(key: KeyArg) -> Self::Query {
        U::take(key.borrow(), &RuntimeStorage)
    }
}
