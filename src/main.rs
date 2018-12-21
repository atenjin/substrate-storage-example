#[macro_use]
extern crate environmental;
extern crate parity_codec as codec;

#[cfg(any(feature = "msgbus-redis", feature = "cache-lru", ))]
#[macro_use]
extern crate lazy_static;

#[cfg(all(feature = "cache-lru"))]
extern crate lru;

mod traits;
mod storage;
mod datadef;

use std::collections::HashMap;
use std::marker::PhantomData;

use self::traits::MemDBT;
use self::storage::{StorageMap, StorageValue};
use self::codec::{Codec, Encode, Decode};

use self::datadef::*;

use self::storage::redis::redis_set_with_blocknumer;

struct MemDB {
    database: HashMap<Vec<u8>, Vec<u8>>
}

impl MemDB {
    fn new() -> MemDB {
        MemDB { database: HashMap::new() }
    }

    fn init(&mut self, v: Vec<(Vec<u8>, Vec<u8>)>) {
        for i in v {
            self.database.insert(i.0, i.1);
        }
    }
}

impl MemDBT for MemDB {
    fn storage(&self, key: &[u8]) -> Option<Vec<u8>> {
        self.database.get(key).map(|r| r.clone())
    }

    fn place_storage(&mut self, key: Vec<u8>, value: Option<Vec<u8>>) {
        match value {
            Some(v) => self.database.insert(key, v),
            None => self.database.remove(&key),
        };
    }
}

environmental!(ext: trait MemDBT);

/// Get `key` from storage and return a `Vec`, empty if there's a problem.
pub fn storage(key: &[u8]) -> Option<Vec<u8>> {
    ext::with(|ext| ext.storage(key).map(|s| s.to_vec()))
        .expect("storage cannot be called outside of an Externalities-provided environment.")
}

pub fn read_storage(key: &[u8], value_out: &mut [u8], value_offset: usize) -> Option<usize> {
    println!("read_storage {:?}, value_offset:{:}", key, value_offset);
    ext::with(|ext| ext.storage(key).map(|value| {
        let value = &value[value_offset..];
        let written = ::std::cmp::min(value.len(), value_out.len());
        value_out[..written].copy_from_slice(&value[..written]);
        value.len()
    })).expect("read_storage cannot be called outside of an Externalities-provided environment.")
}

pub fn set_storage(key: &[u8], value: &[u8]) {
    ext::with(|ext|
        ext.set_storage(key.to_vec(), value.to_vec())
    );
}

pub fn clear_storage(key: &[u8]) {
    ext::with(|ext|
        ext.clear_storage(key)
    );
}

pub fn exists_storage(key: &[u8]) -> bool {
    ext::with(|ext|
        ext.exists_storage(key)
    ).unwrap_or(false)
}

//pub fn clear_prefix(prefix: &[u8]) {
//    ext::with(|ext|
//        ext.clear_prefix(prefix)
//    );
//}

pub fn with_externalities<R, F: FnOnce() -> R>(ext: &mut MemDBT, f: F) -> R {
    ext::using(ext, f)
}


fn main() -> Result<(), String> {
    #[cfg(all(feature = "msgbus-redis"))] {
        storage::redis::init_redis("redis://127.0.0.1/")?;
    }
    println!("Hello, world!");

    let mut s = MemDB::new();

    genesis_init();
    with_externalities(&mut s, || {
        Number::put(0);
    });
    with_externalities(&mut s, || {
        A::put(1);
        let s = A::get();
        println!("{:?}", s);

        B::insert(1, 2);
        let r = B::get(1);
        println!("{:?}", r);

        C::put(Some(1));
    });
    with_externalities(&mut s, || {
        Number::put(1);
    });
//    println!("===============");
//    with_externalities(&mut s, || {
//        let r = C::get();
//        println!("{:?}", r);
//        B::insert(1, 3);
//        let r = B::get(1);
//        println!("{:?}", r);
//        let r = C::take();
//        println!("{:?}", r);
//    });
//
//    with_externalities(&mut s, || {
//        Number::put(2);
//    });
//    println!("===============");
//    with_externalities(&mut s, || {
//        let r = C::get();
//        println!("{:?}", r);
//        B::insert(1, 3);
//        let r = B::get(1);
//        println!("{:?}", r);
//        let r = C::take();
//        println!("{:?}", r);
//    });

    let a = [83, 121, 115, 116, 101, 109, 32, 80, 97, 114, 101, 110, 116, 72, 97, 115, 104];
    let b = [178, 85, 36, 9, 197, 161, 213, 45, 157, 104, 195, 169, 252, 253, 234, 249, 133, 17, 67, 247, 153, 29, 158, 115, 125, 44, 72, 47, 238, 224, 138, 111];
    redis_set_with_blocknumer(&a, 0,&b);
    Ok(())
}
