// Copyright 2017-2018 Parity Technologies (UK) Ltd.
// This file is part of Substrate.

// Substrate is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Substrate is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Substrate.  If not, see <http://www.gnu.org/licenses/>.

//! Strongly typed wrappers around values in storage.
//!
//! This crate exports a macro `storage_items!` and traits describing behavior of generated
//! structs.
//!
//! Three kinds of data types are currently supported:
//!   - values
//!   - maps
//!   - lists
//!
//! # Examples:
//!
//! ```rust
//! #[macro_use]
//! extern crate srml_support;
//!
//! type AuthorityId = [u8; 32];
//! type Balance = u64;
//! pub type SessionKey = [u8; 32];
//!
//! storage_items! {
//!     // public value
//!     pub Value: b"putd_key" => SessionKey;
//!     // private map.
//!     Balances: b"private_map:" => map [AuthorityId => Balance];
//!     // private list.
//!     Authorities: b"auth:" => list [AuthorityId];
//! }
//!
//!# fn main() { }
//! ```

use super::codec;
use std::vec::Vec;
pub use std::borrow::Borrow;
pub use std::marker::PhantomData;

/// Abstraction around storage.
pub trait Storage {
    /// true if the key exists in storage.
    fn exists(&self, key: &[u8]) -> bool;

    /// Load the bytes of a key from storage. Can panic if the type is incorrect.
    fn get<T: codec::Codec>(&self, key: &[u8]) -> Option<T>;

    /// Load the bytes of a key from storage. Can panic if the type is incorrect. Will panic if
    /// it's not there.
    fn require<T: codec::Codec>(&self, key: &[u8]) -> T { self.get(key).expect("Required values must be in storage") }

    /// Load the bytes of a key from storage. Can panic if the type is incorrect. The type's
    /// default is returned if it's not there.
    fn get_or_default<T: codec::Codec + Default>(&self, key: &[u8]) -> T { self.get(key).unwrap_or_default() }

    /// Put a value in under a key.
    fn put<T: codec::Codec>(&self, key: &[u8], val: &T);

    /// Remove the bytes of a key from storage.
    fn kill(&self, key: &[u8]);

    /// Take a value from storage, deleting it after reading.
    fn take<T: codec::Codec>(&self, key: &[u8]) -> Option<T> {
        let value = self.get(key);
        self.kill(key);
        value
    }

    /// Take a value from storage, deleting it after reading.
    fn take_or_panic<T: codec::Codec>(&self, key: &[u8]) -> T { self.take(key).expect("Required values must be in storage") }

    /// Take a value from storage, deleting it after reading.
    fn take_or_default<T: codec::Codec + Default>(&self, key: &[u8]) -> T { self.take(key).unwrap_or_default() }
}

/// A strongly-typed value kept in storage.
pub trait StorageValue<T: codec::Codec> {
    /// The type that get/take returns.
    type Query;

    /// Get the storage key.
    fn key() -> &'static [u8];

    /// true if the value is defined in storage.
    fn exists<S: Storage>(storage: &S) -> bool {
        storage.exists(Self::key())
    }

    /// Load the value from the provided storage instance.
    fn get<S: Storage>(storage: &S) -> Self::Query;

    /// Take a value from storage, removing it afterwards.
    fn take<S: Storage>(storage: &S) -> Self::Query;

    /// Store a value under this key into the provided storage instance.
    fn put<S: Storage>(val: &T, storage: &S) {
        storage.put(Self::key(), val)
    }

    /// Mutate this value
    fn mutate<R, F: FnOnce(&mut Self::Query) -> R, S: Storage>(f: F, storage: &S) -> R;

    /// Clear the storage value.
    fn kill<S: Storage>(storage: &S) {
        storage.kill(Self::key())
    }
}

/// A strongly-typed map in storage.
pub trait StorageMap<K: codec::Codec, V: codec::Codec> {
    /// The type that get/take returns.
    type Query;

    /// Get the prefix key in storage.
    fn prefix() -> &'static [u8];

    /// Get the storage key used to fetch a value corresponding to a specific key.
    fn key_for(x: &K) -> Vec<u8>;

    /// true if the value is defined in storage.
    fn exists<S: Storage>(key: &K, storage: &S) -> bool {
        storage.exists(&Self::key_for(key)[..])
    }

    /// Load the value associated with the given key from the map.
    fn get<S: Storage>(key: &K, storage: &S) -> Self::Query;

    /// Take the value under a key.
    fn take<S: Storage>(key: &K, storage: &S) -> Self::Query;

    /// Store a value to be associated with the given key from the map.
    fn insert<S: Storage>(key: &K, val: &V, storage: &S) {
        storage.put(&Self::key_for(key)[..], val);
    }

    /// Remove the value under a key.
    fn remove<S: Storage>(key: &K, storage: &S) {
        storage.kill(&Self::key_for(key)[..]);
    }

    /// Mutate the value under a key.
    fn mutate<R, F: FnOnce(&mut Self::Query) -> R, S: Storage>(key: &K, f: F, storage: &S) -> R;
}

// TODO: Remove this in favour of `decl_storage` macro.
/// Declares strongly-typed wrappers around codec-compatible types in storage.
#[macro_export]
macro_rules! storage_items {
	// simple values
	($name:ident : $key:expr => $ty:ty; $($t:tt)*) => {
		__storage_items_internal!(() () (OPTION_TYPE Option<$ty>) (get) (take) $name: $key => $ty);
		storage_items!($($t)*);
	};
	(pub $name:ident : $key:expr => $ty:ty; $($t:tt)*) => {
		__storage_items_internal!((pub) () (OPTION_TYPE Option<$ty>) (get) (take) $name: $key => $ty);
		storage_items!($($t)*);
	};
	($name:ident : $key:expr => default $ty:ty; $($t:tt)*) => {
		__storage_items_internal!(() () (RAW_TYPE $ty) (get_or_default) (take_or_default) $name: $key => $ty);
		storage_items!($($t)*);
	};
	(pub $name:ident : $key:expr => default $ty:ty; $($t:tt)*) => {
		__storage_items_internal!((pub) () (RAW_TYPE $ty) (get_or_default) (take_or_default) $name: $key => $ty);
		storage_items!($($t)*);
	};
	($name:ident : $key:expr => required $ty:ty; $($t:tt)*) => {
		__storage_items_internal!(() () (RAW_TYPE $ty) (require) (take_or_panic) $name: $key => $ty);
		storage_items!($($t)*);
	};
	(pub $name:ident : $key:expr => required $ty:ty; $($t:tt)*) => {
		__storage_items_internal!((pub) () (RAW_TYPE $ty) (require) (take_or_panic) $name: $key => $ty);
		storage_items!($($t)*);
	};

	($name:ident get($getfn:ident) : $key:expr => $ty:ty; $($t:tt)*) => {
		__storage_items_internal!(() ($getfn) (OPTION_TYPE Option<$ty>) (get) (take) $name: $key => $ty);
		storage_items!($($t)*);
	};
	(pub $name:ident get($getfn:ident) : $key:expr => $ty:ty; $($t:tt)*) => {
		__storage_items_internal!((pub) ($getfn) (OPTION_TYPE Option<$ty>) (get) (take) $name: $key => $ty);
		storage_items!($($t)*);
	};
	($name:ident get($getfn:ident) : $key:expr => default $ty:ty; $($t:tt)*) => {
		__storage_items_internal!(() ($getfn) (RAW_TYPE $ty) (get_or_default) (take_or_default) $name: $key => $ty);
		storage_items!($($t)*);
	};
	(pub $name:ident get($getfn:ident) : $key:expr => default $ty:ty; $($t:tt)*) => {
		__storage_items_internal!((pub) ($getfn) (RAW_TYPE $ty) (get_or_default) (take_or_default) $name: $key => $ty);
		storage_items!($($t)*);
	};
	($name:ident get($getfn:ident) : $key:expr => required $ty:ty; $($t:tt)*) => {
		__storage_items_internal!(() ($getfn) (RAW_TYPE $ty) (require) (take_or_panic) $name: $key => $ty);
		storage_items!($($t)*);
	};
	(pub $name:ident get($getfn:ident) : $key:expr => required $ty:ty; $($t:tt)*) => {
		__storage_items_internal!((pub) ($getfn) (RAW_TYPE $ty) (require) (take_or_panic) $name: $key => $ty);
		storage_items!($($t)*);
	};

	// maps
	($name:ident : $prefix:expr => map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!(() () (OPTION_TYPE Option<$ty>) (get) (take) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	(pub $name:ident : $prefix:expr => map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!((pub) () (OPTION_TYPE Option<$ty>) (get) (take) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	($name:ident : $prefix:expr => default map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!(() () (RAW_TYPE $ty) (get_or_default) (take_or_default) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	(pub $name:ident : $prefix:expr => default map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!((pub) () (RAW_TYPE $ty) (get_or_default) (take_or_default) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	($name:ident : $prefix:expr => required map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!(() () (RAW_TYPE $ty) (require) (take_or_panic) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	(pub $name:ident : $prefix:expr => required map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!((pub) () (RAW_TYPE $ty) (require) (take_or_panic) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};

	($name:ident get($getfn:ident) : $prefix:expr => map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!(() ($getfn) (OPTION_TYPE Option<$ty>) (get) (take) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	(pub $name:ident get($getfn:ident) : $prefix:expr => map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!((pub) ($getfn) (OPTION_TYPE Option<$ty>) (get) (take) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	($name:ident get($getfn:ident) : $prefix:expr => default map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!(() ($getfn) (RAW_TYPE $ty) (get_or_default) (take_or_default) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	(pub $name:ident get($getfn:ident) : $prefix:expr => default map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!((pub) ($getfn) (RAW_TYPE $ty) (get_or_default) (take_or_default) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	($name:ident get($getfn:ident) : $prefix:expr => required map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!(() ($getfn) (RAW_TYPE $ty) (require) (take_or_panic) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};
	(pub $name:ident get($getfn:ident) : $prefix:expr => required map [$kty:ty => $ty:ty]; $($t:tt)*) => {
		__storage_items_internal!((pub) ($getfn) (RAW_TYPE $ty) (require) (take_or_panic) $name: $prefix => map [$kty => $ty]);
		storage_items!($($t)*);
	};


	// lists
	($name:ident : $prefix:expr => list [$ty:ty]; $($t:tt)*) => {
		__storage_items_internal!(() $name: $prefix => list [$ty]);
		storage_items!($($t)*);
	};
	(pub $name:ident : $prefix:expr => list [$ty:ty]; $($t:tt)*) => {
		__storage_items_internal!((pub) $name: $prefix => list [$ty]);
		storage_items!($($t)*);
	};
	() => ()
}

#[macro_export]
#[doc(hidden)]
macro_rules! __storage_items_internal {
	// generator for values.
	(($($vis:tt)*) ($get_fn:ident) ($wraptype:ident $gettype:ty) ($getter:ident) ($taker:ident) $name:ident : $key:expr => $ty:ty) => {
		__storage_items_internal!{ ($($vis)*) () ($wraptype $gettype) ($getter) ($taker) $name : $key => $ty }
		pub fn $get_fn() -> $gettype { <$name as $crate::storage::generator::StorageValue<$ty>> :: get(&$crate::storage::RuntimeStorage) }
	};
	(($($vis:tt)*) () ($wraptype:ident $gettype:ty) ($getter:ident) ($taker:ident) $name:ident : $key:expr => $ty:ty) => {
		$($vis)* struct $name;

		impl $crate::storage::generator::StorageValue<$ty> for $name {
			type Query = $gettype;

			/// Get the storage key.
			fn key() -> &'static [u8] {
				$key
			}

			/// Load the value from the provided storage instance.
			fn get<S: $crate::GenericStorage>(storage: &S) -> Self::Query {
				storage.$getter($key)
			}

			/// Take a value from storage, removing it afterwards.
			fn take<S: $crate::GenericStorage>(storage: &S) -> Self::Query {
				storage.$taker($key)
			}

			/// Mutate this value.
			fn mutate<R, F: FnOnce(&mut Self::Query) -> R, S: $crate::GenericStorage>(f: F, storage: &S) -> R {
				let mut val = <Self as $crate::storage::generator::StorageValue<$ty>>::get(storage);

				let ret = f(&mut val);

				__handle_wrap_internal!($wraptype {
					// raw type case
					<Self as $crate::storage::generator::StorageValue<$ty>>::put(&val, storage)
				} {
					// Option<> type case
					match val {
						Some(ref val) => <Self as $crate::storage::generator::StorageValue<$ty>>::put(&val, storage),
						None => <Self as $crate::storage::generator::StorageValue<$ty>>::kill(storage),
					}
				});

				ret
			}
		}
	};
	// generator for maps.
	(($($vis:tt)*) ($get_fn:ident) ($wraptype:ident $gettype:ty) ($getter:ident) ($taker:ident) $name:ident : $prefix:expr => map [$kty:ty => $ty:ty]) => {
		__storage_items_internal!{ ($($vis)*) () ($wraptype $gettype) ($getter) ($taker) $name : $prefix => map [$kty => $ty] }
		pub fn $get_fn<K: $crate::storage::generator::Borrow<$kty>>(key: K) -> $gettype {
			<$name as $crate::storage::generator::StorageMap<$kty, $ty>> :: get(key.borrow(), &$crate::storage::RuntimeStorage)
		}
	};
	(($($vis:tt)*) () ($wraptype:ident $gettype:ty) ($getter:ident) ($taker:ident) $name:ident : $prefix:expr => map [$kty:ty => $ty:ty]) => {
		$($vis)* struct $name;

		impl $crate::storage::generator::StorageMap<$kty, $ty> for $name {
			type Query = $gettype;

			/// Get the prefix key in storage.
			fn prefix() -> &'static [u8] {
				$prefix
			}

			/// Get the storage key used to fetch a value corresponding to a specific key.
			fn key_for(x: &$kty) -> $crate::rstd::vec::Vec<u8> {
				let mut key = $prefix.to_vec();
				$crate::codec::Encode::encode_to(x, &mut key);
				key
			}

			/// Load the value associated with the given key from the map.
			fn get<S: $crate::GenericStorage>(key: &$kty, storage: &S) -> Self::Query {
				let key = <$name as $crate::storage::generator::StorageMap<$kty, $ty>>::key_for(key);
				storage.$getter(&key[..])
			}

			/// Take the value, reading and removing it.
			fn take<S: $crate::GenericStorage>(key: &$kty, storage: &S) -> Self::Query {
				let key = <$name as $crate::storage::generator::StorageMap<$kty, $ty>>::key_for(key);
				storage.$taker(&key[..])
			}

			/// Mutate the value under a key.
			fn mutate<R, F: FnOnce(&mut Self::Query) -> R, S: $crate::GenericStorage>(key: &$kty, f: F, storage: &S) -> R {
				let mut val = <Self as $crate::storage::generator::StorageMap<$kty, $ty>>::take(key, storage);

				let ret = f(&mut val);

				__handle_wrap_internal!($wraptype {
					// raw type case
					<Self as $crate::storage::generator::StorageMap<$kty, $ty>>::insert(key, &val, storage)
				} {
					// Option<> type case
					match val {
						Some(ref val) => <Self as $crate::storage::generator::StorageMap<$kty, $ty>>::insert(key, &val, storage),
						None => <Self as $crate::storage::generator::StorageMap<$kty, $ty>>::remove(key, storage),
					}
				});

				ret
			}
		}
	};
	// generator for lists.
	(($($vis:tt)*) $name:ident : $prefix:expr => list [$ty:ty]) => {
		$($vis)* struct $name;

		impl $name {
			fn clear_item<S: $crate::GenericStorage>(index: u32, storage: &S) {
				if index < <$name as $crate::storage::generator::StorageList<$ty>>::len(storage) {
					storage.kill(&<$name as $crate::storage::generator::StorageList<$ty>>::key_for(index));
				}
			}

			fn set_len<S: $crate::GenericStorage>(count: u32, storage: &S) {
				(count..<$name as $crate::storage::generator::StorageList<$ty>>::len(storage)).for_each(|i| $name::clear_item(i, storage));
				storage.put(&<$name as $crate::storage::generator::StorageList<$ty>>::len_key(), &count);
			}
		}

		impl $crate::storage::generator::StorageList<$ty> for $name {
			/// Get the prefix key in storage.
			fn prefix() -> &'static [u8] {
				$prefix
			}

			/// Get the key used to put the length field.
			// TODO: concat macro should accept byte literals.
			fn len_key() -> $crate::rstd::vec::Vec<u8> {
				let mut key = $prefix.to_vec();
				key.extend(b"len");
				key
			}

			/// Get the storage key used to fetch a value at a given index.
			fn key_for(index: u32) -> $crate::rstd::vec::Vec<u8> {
				let mut key = $prefix.to_vec();
				$crate::codec::Encode::encode_to(&index, &mut key);
				key
			}

			/// Read out all the items.
			fn items<S: $crate::GenericStorage>(storage: &S) -> $crate::rstd::vec::Vec<$ty> {
				(0..<$name as $crate::storage::generator::StorageList<$ty>>::len(storage))
					.map(|i| <$name as $crate::storage::generator::StorageList<$ty>>::get(i, storage).expect("all items within length are set; qed"))
					.collect()
			}

			/// Set the current set of items.
			fn set_items<S: $crate::GenericStorage>(items: &[$ty], storage: &S) {
				$name::set_len(items.len() as u32, storage);
				items.iter()
					.enumerate()
					.for_each(|(i, item)| <$name as $crate::storage::generator::StorageList<$ty>>::set_item(i as u32, item, storage));
			}

			fn set_item<S: $crate::GenericStorage>(index: u32, item: &$ty, storage: &S) {
				if index < <$name as $crate::storage::generator::StorageList<$ty>>::len(storage) {
					storage.put(&<$name as $crate::storage::generator::StorageList<$ty>>::key_for(index)[..], item);
				}
			}

			/// Load the value at given index. Returns `None` if the index is out-of-bounds.
			fn get<S: $crate::GenericStorage>(index: u32, storage: &S) -> Option<$ty> {
				storage.get(&<$name as $crate::storage::generator::StorageList<$ty>>::key_for(index)[..])
			}

			/// Load the length of the list.
			fn len<S: $crate::GenericStorage>(storage: &S) -> u32 {
				storage.get(&<$name as $crate::storage::generator::StorageList<$ty>>::len_key()).unwrap_or_default()
			}

			/// Clear the list.
			fn clear<S: $crate::GenericStorage>(storage: &S) {
				for i in 0..<$name as $crate::storage::generator::StorageList<$ty>>::len(storage) {
					$name::clear_item(i, storage);
				}

				storage.kill(&<$name as $crate::storage::generator::StorageList<$ty>>::len_key()[..])
			}
		}
	};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __handle_wrap_internal {
	(RAW_TYPE { $($raw:tt)* } { $($option:tt)* }) => {
		$($raw)*;
	};
	(OPTION_TYPE { $($raw:tt)* } { $($option:tt)* }) => {
		$($option)*;
	};
}

// TODO: revisit this idiom once we get `type`s in `impl`s.
/*impl<T: Trait> Module<T> {
	type Now = super::Now<T>;
}*/

#[macro_export]
macro_rules! __generate_genesis_config {
	(
		[$traittype:ident $traitinstance:ident]
		// normal getters
		[$( $fieldname:ident : $fieldtype:ty = $fielddefault:expr ;)*]
		// for normal builders
		[$( $normalclassname:ident ($normalbuild:expr) ;)*]
		// for map builders
		[$( $mapclassname:ident ($mapbuild:expr) ;)*]
		// extra genesis fields
		[$($curextras:tt)*]
		// final build storage call
		[$call:expr]

		$(#[$attr:meta])* $extrafieldname:ident : $extrafieldty:ty = $extrafielddefault:expr;
		$($extras:tt)*
	) => {
		__generate_genesis_config!(
			[$traittype $traitinstance]
			[$( $fieldname : $fieldtype = $fielddefault ;)*]
			[$( $normalclassname ($normalbuild) ;)*]
			[$( $mapclassname ($mapbuild) ;)*]
			[$($curextras)* $(#[$attr])* $extrafieldname : $extrafieldty = $extrafielddefault ;]
			[$call]
			$($extras)*
		);
	};
	(
		[$traittype:ident $traitinstance:ident]
		// normal getters
		[$( $fieldname:ident : $fieldtype:ty = $fielddefault:expr ;)*]
		// for normal builders
		[$( $normalclassname:ident ($normalbuild:expr) ;)*]
		// for map builders
		[$( $mapclassname:ident ($mapbuild:expr) ;)*]
		// extra genesis fields
		[$($curextras:tt)*]
		// final build storage call
		[$call:expr]

		$(#[$attr:meta])* $extrafieldname:ident : $extrafieldty:ty;
		$($extras:tt)*
	) => {
		__generate_genesis_config!(
				[$traittype $traitinstance]
				[$( $fieldname : $fieldtype = $fielddefault ;)*]
				[$( $normalclassname ($normalbuild) ;)*]
				[$( $mapclassname ($mapbuild) ;)*]
				[$($curextras)* $(#[$attr])* $extrafieldname : $extrafieldty = Default::default() ;]
				[$call]
				$($extras)*
		);
	};
	(
		[$traittype:ident $traitinstance:ident]
		// normal getters
		[$( $fieldname:ident : $fieldtype:ty = $fielddefault:expr ;)*]
		// for normal builders
		[$( $normalclassname:ident ($normalbuild:expr) ;)*]
		// for map builders
		[$( $mapclassname:ident ($mapbuild:expr) ;)*]
		// extra genesis fields
		[$($curextras:tt)*]
		// final build storage call
		[$call:expr]
	) => {
		__generate_genesis_config!(@GEN
				[$traittype $traitinstance]
				[$( $fieldname : $fieldtype = $fielddefault ;)*]
				[$( $normalclassname ($normalbuild) ;)*]
				[$( $mapclassname ($mapbuild) ;)*]
				[$($curextras)*]
				[$call]
		);
	};

	// Do not generate any `GenesisConfig`, if we not require it.
	(@GEN
		[$traittype:ident $traitinstance:ident]
		// normal getters
		[]
		// for normal builders
		[$( $normalclassname:ident ($normalbuild:expr) ;)*]
		// for map builders
		[$( $mapclassname:ident ($mapbuild:expr) ;)*]
		// extra genesis fields
		[]
		// final build storage call
		[$call:expr]
	) => {};

	(@GEN
		[$traittype:ident $traitinstance:ident]
		// normal getters
		[$( $fieldname:ident : $fieldtype:ty = $fielddefault:expr ;)*]
		// for normal builders
		[$( $normalclassname:ident ($normalbuild:expr) ;)*]
		// for map builders
		[$( $mapclassname:ident ($mapbuild:expr) ;)*]
		// extra genesis fields
		[$( $(#[$attr:meta])* $extrafieldname:ident : $extrafieldty:ty = $extrafielddefault:expr; )*]
		// final build storage call
		[$call:expr]
	) => {
		#[cfg(feature = "std")]
		#[derive(Serialize, Deserialize)]
		#[serde(rename_all = "camelCase")]
		#[serde(deny_unknown_fields)]
		pub struct GenesisConfig<$traitinstance: $traittype> {
			#[serde(skip)]
			pub _genesis_phantom_data: $crate::storage::generator::PhantomData<$traitinstance>,
			$(pub $fieldname : $fieldtype ,)*
			$( $(#[$attr])* pub $extrafieldname : $extrafieldty ,)*
		}

		#[cfg(feature = "std")]
		impl<$traitinstance: $traittype> Default for GenesisConfig<$traitinstance> {
			fn default() -> Self {
				GenesisConfig {
					_genesis_phantom_data: Default::default(),
					$($fieldname : $fielddefault ,)*
					$($extrafieldname : $extrafielddefault ,)*
				}
			}
		}

		#[cfg(feature = "std")]
		impl<$traitinstance: $traittype> $crate::runtime_primitives::BuildStorage for GenesisConfig<$traitinstance>
		{
			fn build_storage(self) -> ::std::result::Result<($crate::runtime_primitives::StorageMap, $crate::runtime_primitives::ChildrenStorageMap), String> {
				let mut r: $crate::runtime_primitives::StorageMap = Default::default();
				let mut c: $crate::runtime_primitives::ChildrenStorageMap = Default::default();

				// normal getters
				$({
					use $crate::codec::Encode;
					let v = ($normalbuild)(&self);
					r.insert(Self::hash(<$normalclassname<$traitinstance>>::key()).to_vec(), v.encode());
				})*

				// for maps
				$({
					use $crate::codec::Encode;
					let data = ($mapbuild)(&self);
					for (k, v) in data.into_iter() {
						r.insert(Self::hash(&<$mapclassname<$traitinstance>>::key_for(k)).to_vec(), v.encode());
					}
				})*

				// extra call
				$call(&mut r, &mut c, &self);

				Ok((r, c))
			}
		}
	};
}

/// Declares strongly-typed wrappers around codec-compatible types in storage.
///
/// For now we implement a convenience trait with pre-specialised associated types, one for each
/// storage item. This allows you to gain access to publicly visible storage items from a
/// module type. Currently you must disambiguate by using `<Module as Store>::Item` rather than
/// the simpler `Module::Item`. Hopefully the rust guys with fix this soon.
#[macro_export]
macro_rules! decl_storage {
	(
		$pub:vis trait $storetype:ident for $modulename:ident<$traitinstance:ident: $traittype:ident> as $cratename:ident {
			$($t:tt)*
		}
		add_extra_genesis {
			$( $(#[$attr:meta])* config($extrafield:ident) : $extraty:ty $(= $default:expr)* ;)*
			build($call:expr);
		}
	) => {
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
		$pub trait $storetype {
			__decl_store_items!($($t)*);
		}
		impl<$traitinstance: $traittype> $storetype for $modulename<$traitinstance> {
			__impl_store_items!($traitinstance $($t)*);
		}
		impl<$traitinstance: $traittype> $modulename<$traitinstance> {
			__impl_store_fns!($traitinstance $($t)*);
			__impl_store_metadata!($cratename; $($t)*);
		}
		__decl_genesis_config_items!([$traittype $traitinstance] [] [] [] [$( $(#[$attr])* $extrafield : $extraty $(= $default)* ; )* ] [$call] $($t)*);
	};
	(
		$pub:vis trait $storetype:ident for $modulename:ident<$traitinstance:ident: $traittype:ident> as $cratename:ident {
			$($t:tt)*
		}
	) => {
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
		$pub trait $storetype {
			__decl_store_items!($($t)*);
		}
		impl<$traitinstance: $traittype> $storetype for $modulename<$traitinstance> {
			__impl_store_items!($traitinstance $($t)*);
		}
		impl<$traitinstance: $traittype> $modulename<$traitinstance> {
			__impl_store_fns!($traitinstance $($t)*);
			__impl_store_metadata!($cratename; $($t)*);
		}
		__decl_genesis_config_items!([$traittype $traitinstance] [] [] [] [] [|_, _, _|{}] $($t)*);
	};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __decl_genesis_config_items {
	// maps without getters:
	//  - pub
	//  - $default
	// we don't allow any config() or build() on this pattern.
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!([$traittype $traitinstance] [$($cur)*] [$($nb)*] [$($mapcur)*] [$($extras)*] [$call] $($t)*);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!([$traittype $traitinstance] [$($cur)*] [$($nb)*] [$($mapcur)*] [$($extras)*] [$call] $($t)*);
	};

	// maps with getters:
	//  - pub
	//  - build()
	//  - $default
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) : map $kty:ty => $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) : map $kty:ty => $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) build($build:expr) : map $kty:ty => $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)* $name ($build);]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) build($build:expr) : map $kty:ty => $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)* $name ($build);]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};

	// simple values without getters:
	//  - pub
	//  - $default
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident : $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident : $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};

	// simple values with getters:
	//  - pub
	//  - (empty) / config() / config(myname)
	//  - build()
	//  - $default
	// Option<> types
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) : Option<$ty:ty>;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) : Option<$ty:ty> = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config() : Option<$ty:ty>;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $getfn : $ty = Default::default();]
			[$($nb)* $name (|config: &GenesisConfig<$traitinstance>| config.$getfn.clone());]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config() : Option<$ty:ty> = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $getfn : $ty = $default.unwrap_or_default();]
			[$($nb)* $name (|config: &GenesisConfig<$traitinstance>| config.$getfn.clone());]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config($myname:ident) : Option<$ty:ty>;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $myname : $ty = Default::default();]
			[$($nb)* $name (|config: &GenesisConfig<$traitinstance>| config.$myname.clone()); ]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config($myname:ident) : Option<$ty:ty> = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $myname : $ty = $default.unwrap_or_default();]
			[$($nb)* $name (|config: &GenesisConfig<$traitinstance>| config.$myname.clone()); ]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	// build
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) build($build:expr) : Option<$ty:ty>;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) build($build:expr) : Option<$ty:ty> = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config() build($build:expr) : Option<$ty:ty>;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $getfn : $ty = Default::default();]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config() build($build:expr) : Option<$ty:ty> = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $getfn : $ty = $default.unwrap_or_default();]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config($myname:ident) build($build:expr) : Option<$ty:ty>;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $myname : $ty = Default::default();]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config($myname:ident) build($build:expr) : Option<$ty:ty> = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $myname : $ty = $default.unwrap_or_default();]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};

	// raw types
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) : $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) : $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)*]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config() : $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $getfn : $ty = Default::default();]
			[$($nb)* $name (|config: &GenesisConfig<$traitinstance>| config.$getfn.clone());]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config() : $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $getfn : $ty = $default;]
			[$($nb)* $name (|config: &GenesisConfig<$traitinstance>| config.$getfn.clone());]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config($myname:ident) : $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $myname : $ty = Default::default();]
			[$($nb)* $name (|config: &GenesisConfig<$traitinstance>| config.$myname.clone()); ]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config($myname:ident) : $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $myname : $ty = $default;]
			[$($nb)* $name (|config: &GenesisConfig<$traitinstance>| config.$myname.clone()); ]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	// build
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) build($build:expr) : $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) build($build:expr) : $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)*]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config() build($build:expr) : $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $getfn : $ty = Default::default();]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config() build($build:expr) : $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $getfn : $ty = $default;]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config($myname:ident) build($build:expr) : $ty:ty;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $myname : $ty = Default::default();]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]
		$(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) config($myname:ident) build($build:expr) : $ty:ty = $default:expr;
		$($t:tt)*
	) => {
		__decl_genesis_config_items!(
			[$traittype $traitinstance]
			[$($cur)* $myname : $ty = $default;]
			[$($nb)* $name ($build);]
			[$($mapcur)*]
			[$($extras)*]
			[$call]
			$($t)*
		);
	};

	// exit
	([$traittype:ident $traitinstance:ident] [$($cur:tt)*] [$($nb:tt)*] [$($mapcur:tt)*] [$($extras:tt)*] [$call:expr]) => {
		// we pass the extra fields
		__generate_genesis_config!([$traittype $traitinstance] [$($cur)*] [$($nb)*] [$($mapcur)*] [] [$call] $($extras)* );
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! __decl_storage_items {
	// we have to put the map's pattern first to avoid the ambiguity.

	// factor out Option<> type first.
	// maps:
	//  - pub
	//  - $default
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => Option<$ty:ty>; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (OPTION_TYPE Option<$ty>) $cratename $name: map $kty => $ty = Default::default());
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => Option<$ty:ty> = $default:expr; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (OPTION_TYPE Option<$ty>) $cratename $name: map $kty => $ty = $default);
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};

	// maps with getters:
	//  - pub
	//  - $default
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($fn:expr))* : map $kty:ty => Option<$ty:ty>; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (OPTION_TYPE Option<$ty>) $cratename $name: map $kty => $ty = Default::default());
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($fn:expr))* : map $kty:ty => Option<$ty:ty> = $default:expr; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (OPTION_TYPE Option<$ty>) $cratename $name: map $kty => $ty = $default);
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};

	// raw types for map
	// maps:
	//  - pub
	//  - $default
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (RAW_TYPE $ty) $cratename $name: map $kty => $ty = Default::default());
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty = $default:expr; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (RAW_TYPE $ty) $cratename $name: map $kty => $ty = $default);
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};

	// maps:
	//  - pub
	//  - $default
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($fn:expr))* : map $kty:ty => $ty:ty; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (RAW_TYPE $ty) $cratename $name: map $kty => $ty = Default::default());
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($fn:expr))* : map $kty:ty => $ty:ty = $default:expr; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (RAW_TYPE $ty) $cratename $name: map $kty => $ty = $default);
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};

	// try to factor out Option<> to get the raw type.
	// simple values without getters:
	//  - pub
	//  - $default
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : Option<$ty:ty>; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (OPTION_TYPE Option<$ty>) $cratename $name: $ty = Default::default());
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : Option<$ty:ty> = $default:expr; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (OPTION_TYPE Option<$ty>) $cratename $name: $ty = $default);
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};

	// simple values with getters:
	//  - pub
	//  - config()
	//  - build()
	//  - $default
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($rename:ident)*))* $(build($fn:expr))* : Option<$ty:ty>; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (OPTION_TYPE Option<$ty>) $cratename $name: $ty = Default::default());
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($rename:ident)*))* $(build($fn:expr))* : Option<$ty:ty> = $default:expr; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (OPTION_TYPE Option<$ty>) $cratename $name: $ty = $default);
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};

	// simple values without getters:
	//  - pub
	//  - $default
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : $ty:ty; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (RAW_TYPE $ty) $cratename $name: $ty = Default::default());
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : $ty:ty = $default:expr; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (RAW_TYPE $ty) $cratename $name: $ty = $default);
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};

	// simple values with getters:
	//  - pub
	//  - config()
	//  - build()
	//  - $default
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($rename:ident)*))* $(build($fn:expr))* : $ty:ty; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (RAW_TYPE $ty) $cratename $name: $ty = Default::default());
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};
	($cratename:ident $traittype:ident $traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($rename:ident)*))* $(build($fn:expr))* : $ty:ty = $default:expr; $($t:tt)*) => {
		__decl_storage_item!(($pub) ($traittype as $traitinstance) (RAW_TYPE $ty) $cratename $name: $ty = $default);
		__decl_storage_items!($cratename $traittype $traitinstance $($t)*);
	};

	// exit
	($cratename:ident $traittype:ident $traitinstance:ident) => ()
}

#[macro_export]
#[doc(hidden)]
macro_rules! __decl_storage_item {
	// generator for maps.
	(($pub:vis) ($traittype:ident as $traitinstance:ident) ($wraptype:ident $gettype:ty) $cratename:ident $name:ident : map $kty:ty => $ty:ty = $default:expr) => {
		$pub struct $name<$traitinstance: $traittype>($crate::storage::generator::PhantomData<$traitinstance>);

		impl<$traitinstance: $traittype> $crate::storage::generator::StorageMap<$kty, $ty> for $name<$traitinstance> {
			type Query = $gettype;

			/// Get the prefix key in storage.
			fn prefix() -> &'static [u8] {
				stringify!($cratename $name).as_bytes()
			}

			/// Get the storage key used to fetch a value corresponding to a specific key.
			fn key_for(x: &$kty) -> $crate::rstd::vec::Vec<u8> {
				let mut key = <$name<$traitinstance> as $crate::storage::generator::StorageMap<$kty, $ty>>::prefix().to_vec();
				$crate::codec::Encode::encode_to(x, &mut key);
				key
			}

			/// Load the value associated with the given key from the map.
			fn get<S: $crate::GenericStorage>(key: &$kty, storage: &S) -> Self::Query {
				let key = <$name<$traitinstance> as $crate::storage::generator::StorageMap<$kty, $ty>>::key_for(key);

				__handle_wrap_internal!($wraptype {
					// raw type case
					storage.get(&key[..]).unwrap_or_else(|| $default)
				} {
					// Option<> type case
					storage.get(&key[..]).or_else(|| $default)
				})
			}

			/// Take the value, reading and removing it.
			fn take<S: $crate::GenericStorage>(key: &$kty, storage: &S) -> Self::Query {
				let key = <$name<$traitinstance> as $crate::storage::generator::StorageMap<$kty, $ty>>::key_for(key);

				__handle_wrap_internal!($wraptype {
					// raw type case
					storage.take(&key[..]).unwrap_or_else(|| $default)
				} {
					// Option<> type case
					storage.take(&key[..]).or_else(|| $default)
				})
			}

			/// Mutate the value under a key
			fn mutate<R, F: FnOnce(&mut Self::Query) -> R, S: $crate::GenericStorage>(key: &$kty, f: F, storage: &S) -> R {
				let mut val = <Self as $crate::storage::generator::StorageMap<$kty, $ty>>::take(key, storage);

				let ret = f(&mut val);

				__handle_wrap_internal!($wraptype {
					// raw type case
					<Self as $crate::storage::generator::StorageMap<$kty, $ty>>::insert(key, &val, storage)
				} {
					// Option<> type case
					match val {
						Some(ref val) => <Self as $crate::storage::generator::StorageMap<$kty, $ty>>::insert(key, &val, storage),
						None => <Self as $crate::storage::generator::StorageMap<$kty, $ty>>::remove(key, storage),
					}
				});

				ret
			}
		}
	};
	// generator for values.
	(($pub:vis) ($traittype:ident as $traitinstance:ident) ($wraptype:ident $gettype:ty) $cratename:ident $name:ident : $ty:ty = $default:expr) => {
		$pub struct $name<$traitinstance: $traittype>($crate::storage::generator::PhantomData<$traitinstance>);

		impl<$traitinstance: $traittype> $crate::storage::generator::StorageValue<$ty> for $name<$traitinstance> {
			type Query = $gettype;

			/// Get the storage key.
			fn key() -> &'static [u8] {
				stringify!($cratename $name).as_bytes()
			}

			/// Load the value from the provided storage instance.
			fn get<S: $crate::GenericStorage>(storage: &S) -> Self::Query {

				__handle_wrap_internal!($wraptype {
					// raw type case
					storage.get(<$name<$traitinstance> as $crate::storage::generator::StorageValue<$ty>>::key())
						.unwrap_or_else(|| $default)
				} {
					// Option<> type case
					storage.get(<$name<$traitinstance> as $crate::storage::generator::StorageValue<$ty>>::key())
						.or_else(|| $default)
				})
			}

			/// Take a value from storage, removing it afterwards.
			fn take<S: $crate::GenericStorage>(storage: &S) -> Self::Query {
				__handle_wrap_internal!($wraptype {
					// raw type case
					storage.take(<$name<$traitinstance> as $crate::storage::generator::StorageValue<$ty>>::key())
						.unwrap_or_else(|| $default)
				} {
					// Option<> type case
					storage.take(<$name<$traitinstance> as $crate::storage::generator::StorageValue<$ty>>::key())
						.or_else(|| $default)
				})

			}

			/// Mutate the value under a key.
			fn mutate<R, F: FnOnce(&mut Self::Query) -> R, S: $crate::GenericStorage>(f: F, storage: &S) -> R {
				let mut val = <Self as $crate::storage::generator::StorageValue<$ty>>::get(storage);

				let ret = f(&mut val);

				__handle_wrap_internal!($wraptype {
					// raw type case
					<Self as $crate::storage::generator::StorageValue<$ty>>::put(&val, storage)
				} {
					// Option<> type case
					match val {
						Some(ref val) => <Self as $crate::storage::generator::StorageValue<$ty>>::put(&val, storage),
						None => <Self as $crate::storage::generator::StorageValue<$ty>>::kill(storage),
					}
				});

				ret
			}
		}
	};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __decl_store_items {
	// maps:
	//  - pub
	//  - $default
	($(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty; $($t:tt)*) => {
		__decl_store_item!($name); __decl_store_items!($($t)*);
	};
	($(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty = $default:expr; $($t:tt)*) => {
		__decl_store_item!($name); __decl_store_items!($($t)*);
	};

	// maps:
	//  - pub
	//  - $default
	($(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($build:expr))* : map $kty:ty => $ty:ty; $($t:tt)*) => {
		__decl_store_item!($name); __decl_store_items!($($t)*);
	};
	($(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($build:expr))* : map $kty:ty => $ty:ty = $default:expr; $($t:tt)*) => {
		__decl_store_item!($name); __decl_store_items!($($t)*);
	};

	// simple values without getters:
	//  - pub
	//  - $default
	($(#[$doc:meta])* $pub:vis $name:ident : $ty:ty; $($t:tt)*) => {
		__decl_store_item!($name); __decl_store_items!($($t)*);
	};
	($(#[$doc:meta])* $pub:vis $name:ident : $ty:ty = $default:expr; $($t:tt)*) => {
		__decl_store_item!($name); __decl_store_items!($($t)*);
	};

	// simple values with getters:
	//  - pub
	//  - $default
	($(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* : $ty:ty; $($t:tt)*) => {
		__decl_store_item!($name); __decl_store_items!($($t)*);
	};
	($(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* : $ty:ty = $default:expr; $($t:tt)*) => {
		__decl_store_item!($name); __decl_store_items!($($t)*);
	};

	// exit
	() => ()
}

#[macro_export]
#[doc(hidden)]
macro_rules! __decl_store_item {
	($name:ident) => { type $name; }
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_store_fns {
	// with Option<>
	// maps:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($build:expr))* : map $kty:ty => Option<$ty:ty>; $($t:tt)*) => {
		__impl_store_fn!($traitinstance $name $getfn (Option<$ty>) map $kty => $ty);
		__impl_store_fns!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($build:expr))* : map $kty:ty => Option<$ty:ty> = $default:expr; $($t:tt)*) => {
		__impl_store_fn!($traitinstance $name $getfn (Option<$ty>) map $kty => $ty);
		__impl_store_fns!($traitinstance $($t)*);
	};

	// without Option<>
	// maps:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty; $($t:tt)*) => {
		__impl_store_fns!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty = $default:expr; $($t:tt)*) => {
		__impl_store_fns!($traitinstance $($t)*);
	};

	// maps:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($build:expr))* : map $kty:ty => $ty:ty; $($t:tt)*) => {
		__impl_store_fn!($traitinstance $name $getfn ($ty) map $kty => $ty);
		__impl_store_fns!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($build:expr))* : map $kty:ty => $ty:ty = $default:expr; $($t:tt)*) => {
		__impl_store_fn!($traitinstance $name $getfn ($ty) map $kty => $ty);
		__impl_store_fns!($traitinstance $($t)*);
	};

	// with Option<>
	// simple values with getters:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* : Option<$ty:ty>; $($t:tt)*) => {
		__impl_store_fn!($traitinstance $name $getfn (Option<$ty>) $ty);
		__impl_store_fns!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* : Option<$ty:ty> = $default:expr; $($t:tt)*) => {
		__impl_store_fn!($traitinstance $name $getfn (Option<$ty>) $ty);
		__impl_store_fns!($traitinstance $($t)*);
	};

	// without Option<>
	// simple values without getters:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : $ty:ty; $($t:tt)*) => {
		__impl_store_fns!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : $ty:ty = $default:expr; $($t:tt)*) => {
		__impl_store_fns!($traitinstance $($t)*);
	};

	// simple values with getters:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* : $ty:ty; $($t:tt)*) => {
		__impl_store_fn!($traitinstance $name $getfn ($ty) $ty);
		__impl_store_fns!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* : $ty:ty = $default:expr; $($t:tt)*) => {
		__impl_store_fn!($traitinstance $name $getfn ($ty) $ty);
		__impl_store_fns!($traitinstance $($t)*);
	};

	// exit
	($traitinstance:ident) => ()
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_store_fn {
	($traitinstance:ident $name:ident $get_fn:ident ($gettype:ty) map $kty:ty => $ty:ty) => {
		pub fn $get_fn<K: $crate::storage::generator::Borrow<$kty>>(key: K) -> $gettype {
			<$name<$traitinstance> as $crate::storage::generator::StorageMap<$kty, $ty>> :: get(key.borrow(), &$crate::storage::RuntimeStorage)
		}
	};
	($traitinstance:ident $name:ident $get_fn:ident ($gettype:ty) $ty:ty) => {
		pub fn $get_fn() -> $gettype {
			<$name<$traitinstance> as $crate::storage::generator::StorageValue<$ty>> :: get(&$crate::storage::RuntimeStorage)
		}
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_store_items {
	// maps:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty; $($t:tt)*) => {
		__impl_store_item!($name $traitinstance);
		__impl_store_items!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : map $kty:ty => $ty:ty = $default:expr; $($t:tt)*) => {
		__impl_store_item!($name $traitinstance);
		__impl_store_items!($traitinstance $($t)*);
	};

	// maps:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($build:expr))* : map $kty:ty => $ty:ty; $($t:tt)*) => {
		__impl_store_item!($name $traitinstance);
		__impl_store_items!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(build($build:expr))* : map $kty:ty => $ty:ty = $default:expr; $($t:tt)*) => {
		__impl_store_item!($name $traitinstance);
		__impl_store_items!($traitinstance $($t)*);
	};

	// simple values without getters:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : $ty:ty; $($t:tt)*) => {
		__impl_store_item!($name $traitinstance);
		__impl_store_items!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident : $ty:ty = $default:expr; $($t:tt)*) => {
		__impl_store_item!($name $traitinstance);
		__impl_store_items!($traitinstance $($t)*);
	};

	// simple values with getters:
	//  - pub
	//  - $default
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* : $ty:ty; $($t:tt)*) => {
		__impl_store_item!($name $traitinstance);
		__impl_store_items!($traitinstance $($t)*);
	};
	($traitinstance:ident $(#[$doc:meta])* $pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* : $ty:ty = $default:expr; $($t:tt)*) => {
		__impl_store_item!($name $traitinstance);
		__impl_store_items!($traitinstance $($t)*);
	};

	// exit
	($traitinstance:ident) => ()
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_store_item {
	($name:ident $traitinstance:ident) => { type $name = $name<$traitinstance>; }
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_store_metadata {
	(
		$cratename:ident;
		$($rest:tt)*
	) => {
		pub fn store_metadata() -> $crate::storage::generator::StorageMetadata {
			$crate::storage::generator::StorageMetadata {
				prefix: $crate::storage::generator::DecodeDifferent::Encode(stringify!($cratename)),
				functions: __store_functions_to_metadata!(; $( $rest )* ),
			}
		}
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! __store_functions_to_metadata {
	// maps: pub / $default
	// map Option<>
	(
		$( $metadata:expr ),*;
		$(#[doc = $doc_attr:tt])*
		$pub:vis $name:ident :
			map $kty:ty => Option<$ty:ty> $(= $default:expr)*;
		$($t:tt)*
	) => {
		__store_functions_to_metadata!(
			$( $metadata, )*
			__store_function_to_metadata!(
				$( $doc_attr ),*; $name; __store_type_to_metadata!($kty, $ty);
				$crate::storage::generator::StorageFunctionModifier::Optional
			);
			$( $t )*
		)
	};

	// map raw types
	(
		$( $metadata:expr ),*;
		$(#[doc = $doc_attr:tt])*
		$pub:vis $name:ident :
			map $kty:ty => $ty:ty $(= $default:expr)*;
		$($t:tt)*
	) => {
		__store_functions_to_metadata!(
			$( $metadata, )*
			__store_function_to_metadata!(
				$( $doc_attr ),*; $name; __store_type_to_metadata!($kty, $ty);
				$crate::storage::generator::StorageFunctionModifier::Default
			);
			$( $t )*
		)
	};

	// map getters: pub / $default
	// Option<>
	(
		$( $metadata:expr ),*;
		$(#[doc = $doc_attr:tt])*
		$pub:vis $name:ident get($getfn:ident) $(build($build:expr))* :
			map $kty:ty => Option<$ty:ty> $(= $default:expr)*;
		$($t:tt)*
	) => {
		__store_functions_to_metadata!(
			$( $metadata, )*
			__store_function_to_metadata!(
				$( $doc_attr ),*; $name; __store_type_to_metadata!($kty, $ty);
				$crate::storage::generator::StorageFunctionModifier::Optional
			);
			$( $t )*
		)
	};

	// map getters: pub / $default
	// raw types
	(
		$( $metadata:expr ),*;
		$(#[doc = $doc_attr:tt])*
		$pub:vis $name:ident get($getfn:ident) $(build($build:expr))* :
			map $kty:ty => $ty:ty $(= $default:expr)*;
		$($t:tt)*
	) => {
		__store_functions_to_metadata!(
			$( $metadata, )*
			__store_function_to_metadata!(
				$( $doc_attr ),*; $name; __store_type_to_metadata!($kty, $ty);
				$crate::storage::generator::StorageFunctionModifier::Default
			);
			$( $t )*
		)
	};

	// simple values: pub / $default
	(
		$( $metadata:expr ),*;
		$(#[doc = $doc_attr:tt])*
		$pub:vis $name:ident : Option<$ty:ty> $(= $default:expr)*;
		$($t:tt)*
	) => {
		__store_functions_to_metadata!(
			$( $metadata, )*
			__store_function_to_metadata!(
				$( $doc_attr ),*; $name; __store_type_to_metadata!($ty);
				$crate::storage::generator::StorageFunctionModifier::Optional
			);
			$( $t )*
		)
	};
	// raw types
	(
		$( $metadata:expr ),*;
		$(#[doc = $doc_attr:tt])*
		$pub:vis $name:ident : $ty:ty $(= $default:expr)*;
		$($t:tt)*
	) => {
		__store_functions_to_metadata!(
			$( $metadata, )*
			__store_function_to_metadata!(
				$( $doc_attr ),*; $name; __store_type_to_metadata!($ty);
				$crate::storage::generator::StorageFunctionModifier::Default
			);
			$( $t )*
		)
	};

	// simple values getters: pub / $default
	// Option<>
	(
		$( $metadata:expr ),*;
		$(#[doc = $doc_attr:tt])*
		$pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* :
			Option<$ty:ty> $(= $default:expr)*;
		$($t:tt)*
	) => {
		__store_functions_to_metadata!(
			$( $metadata, )*
			__store_function_to_metadata!(
				$( $doc_attr ),*; $name; __store_type_to_metadata!($ty);
				$crate::storage::generator::StorageFunctionModifier::Optional
			);
			$( $t )*
		)
	};
	// simple values getters: pub / $default
	// raw types
	(
		$( $metadata:expr ),*;
		$(#[doc = $doc_attr:tt])*
		$pub:vis $name:ident get($getfn:ident) $(config($($myname:ident)*))* $(build($build:expr))* :
			$ty:ty $(= $default:expr)*;
		$($t:tt)*
	) => {
		__store_functions_to_metadata!(
			$( $metadata, )*
			__store_function_to_metadata!(
				$( $doc_attr ),*; $name; __store_type_to_metadata!($ty);
				$crate::storage::generator::StorageFunctionModifier::Default
			);
			$( $t )*
		)
	};
	(
		$( $metadata:expr ),*;
	) => {
		$crate::storage::generator::DecodeDifferent::Encode(&[
			$( $metadata ),*
		])
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! __store_function_to_metadata {
	($( $fn_doc:expr ),*; $name:ident; $type:expr; $modifier:expr) => {
		$crate::storage::generator::StorageFunctionMetadata {
			name: $crate::storage::generator::DecodeDifferent::Encode(stringify!($name)),
			modifier: $modifier,
			ty: $type,
			documentation: $crate::storage::generator::DecodeDifferent::Encode(&[ $( $fn_doc ),* ]),
		}
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! __store_type_to_metadata {
	($name:ty) => {
		$crate::storage::generator::StorageFunctionType::Plain(
			$crate::storage::generator::DecodeDifferent::Encode(stringify!($name)),
		)
	};
	($key: ty, $value:ty) => {
		$crate::storage::generator::StorageFunctionType::Map {
			key: $crate::storage::generator::DecodeDifferent::Encode(stringify!($key)),
			value: $crate::storage::generator::DecodeDifferent::Encode(stringify!($value)),
		}
	}
}
