#[macro_use]
extern crate environmental;
extern crate parity_codec as codec;

mod traits;
mod storage;
mod datadef;

use std::collections::HashMap;
use std::marker::PhantomData;

use self::traits::MemDBT;
use self::storage::{StorageMap, StorageValue};
use self::codec::{Codec, Encode, Decode};

use self::datadef::*;

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


fn main() {
    println!("Hello, world!");

    let mut s = MemDB::new();

    with_externalities(&mut s, || {
//        A::put(1);
//        let s = A::get();
//        println!("{:?}", s);
//
//        B::insert(1, 2);
//        let r = B::get(1);
//        println!("{:?}", r);

        C::put(Some(1));
    });
    println!("===============");
    with_externalities(&mut s, || {
        let r = C::get();
        println!("{:?}", r);
    })
}
