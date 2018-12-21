use super::primitives::twox_128;

static mut BLOCKNUMBER_KEY: &'static [u8] = b"";
static mut BLOCKNUMBER_HASHED_KEY: [u8; 16] = [0; 16];

pub fn set_blocknumber_key(key: &'static [u8]) {
    let hash_key = twox_128(key);
    unsafe {
        BLOCKNUMBER_KEY = key;
        BLOCKNUMBER_HASHED_KEY = hash_key;
    }
}

pub fn blocknumber_key() -> &'static [u8] {
    unsafe {
        BLOCKNUMBER_KEY
    }
}

pub fn blocknumber_hashedkey() -> &'static [u8] {
    unsafe {
        &BLOCKNUMBER_HASHED_KEY
    }
}
