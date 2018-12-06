use super::storage::generator::{StorageValue, StorageMap, Storage};
use super::codec;

// ========================== test struct ==================
//
// =========================================================
pub struct A;

impl StorageValue<u32> for A {
    type Query = u32;

    fn key() -> &'static [u8] {
        b"A"
    }

    fn get<S: Storage>(storage: &S) -> <Self as StorageValue<u32>>::Query {
        storage.get(Self::key()).unwrap_or_else(|| Default::default())
    }

    fn take<S: Storage>(storage: &S) -> <Self as StorageValue<u32>>::Query {
        storage.take(Self::key()).unwrap_or_else(|| Default::default())
    }

    fn mutate<R, F: FnOnce(&mut Self::Query) -> R, S: Storage>(f: F, storage: &S) -> R {
        let mut val = Self::get(storage);
        let ret = f(&mut val);
        Self::put(&val, storage);
        ret
    }
}

pub struct B;

impl StorageMap<u32, u32> for B {
    type Query = u32;

    fn prefix() -> &'static [u8] {
        b"B"
    }

    fn key_for(x: &u32) -> Vec<u8> {
        let mut key = Self::prefix().to_vec();
        codec::Encode::encode_to(x, &mut key);
        key
    }

    fn get<S: Storage>(key: &u32, storage: &S) -> <Self as StorageMap<u32, u32>>::Query {
        let key = Self::key_for(key);
        storage.get(&key[..]).unwrap_or_else(|| Default::default())
    }

    fn take<S: Storage>(key: &u32, storage: &S) -> <Self as StorageMap<u32, u32>>::Query {
        let key = Self::key_for(key);
        storage.take(&key[..]).unwrap_or_else(|| Default::default())
    }

    fn mutate<R, F: FnOnce(&mut Self::Query) -> R, S: Storage>(key: &u32, f: F, storage: &S) -> R {
        let mut val = Self::take(key, storage);
        let r = f(&mut val);
        Self::insert(key, &val, storage);
        r
    }
}