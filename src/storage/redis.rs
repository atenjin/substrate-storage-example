extern crate parking_lot;
extern crate redis;

use parking_lot::{Mutex, MutexGuard};

use std::error::Error;
use std::string::ToString;

use redis::{Connection, RedisError};
use redis::Commands;

struct RedisClient {
    conn: Option<Connection>
}

impl RedisClient {
    pub fn set_conn(&mut self, conn: Connection) {
        self.conn = Some(conn)
    }
    pub fn set_with_blocknumber(&self, key: &[u8], h: u64, value: &[u8]) {
        match self.conn {
            Some(ref conn) => {
                // todo  refactor with Struct redis::Pipeline
                // remove current score first
                let _: Result<usize, RedisError> = redis::cmd("zremrangebyscore").arg(key).arg(h).arg(h).query(conn);

//                let r= redis::cmd("zremrangebyscore").arg(key).arg(h).arg(h);
//                println!("{:?}", r)

                let num = h.to_string();
                let mut redis_key = key.to_vec();
                redis_key.extend(b"#".to_vec());
                redis_key.extend(num.as_bytes());

                let _: Result<usize, RedisError> = conn.zadd(key, redis_key.as_slice(), h).map_err(|e| {
                    // TODO log
                    println!("set blocknumber {:?}", e);
                    e
                });
                self.set(&redis_key, value)
            }
            None => { return; }
        };
    }
    pub fn set(&self, key: &[u8], value: &[u8]) {
        let _: Result<usize, RedisError> = match self.conn {
            Some(ref conn) => {
                conn.set(key, value)
            }
            None => { return; }
        }.map_err(|e| {
            // TODO log
            println!("{:?}", e);
            e
        });
    }
    pub fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
        match self.conn {
            Some(ref conn) => {
                conn.get(key)
            }
            None => { return None; }
        }.map_err(|e| {
            // TODO log
            println!("{:?}", e);
            e
        }).ok()
    }
}

lazy_static! {
    static ref REDIS: Mutex<RedisClient> = Mutex::new(RedisClient { conn: None });
}

pub fn init_redis(url: &str) -> Result<(), String> {
    let client = redis::Client::open(url).map_err(|e| e.description().to_string())?;
    let conn = client.get_connection().map_err(|e| e.description().to_string())?;
    REDIS.lock().set_conn(conn);
    Ok(())
}

pub fn redis_get(key: &[u8]) -> Option<Vec<u8>> {
    REDIS.lock().get(key)
}

pub fn redis_set(key: &[u8], value: &[u8]) {
    REDIS.lock().set(key, value)
}

pub fn redis_set_with_blocknumer(key: &[u8], h: u64, value: &[u8]) {
    REDIS.lock().set_with_blocknumber(key, h, value)
}
