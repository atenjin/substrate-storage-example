pub trait MemDBT {
    /// Read storage of current contract being called.
    fn storage(&self, key: &[u8]) -> Option<Vec<u8>>;
    /// Set storage entry `key` of current contract being called (effective immediately).
    fn set_storage(&mut self, key: Vec<u8>, value: Vec<u8>) {
        self.place_storage(key, Some(value));
    }

    /// Set or clear a storage entry (`key`) of current contract being called (effective immediately).
    fn place_storage(&mut self, key: Vec<u8>, value: Option<Vec<u8>>);

    /// Clear a storage entry (`key`) of current contract being called (effective immediately).
    fn clear_storage(&mut self, key: &[u8]) {
        self.place_storage(key.to_vec(), None);
    }

    /// Whether a storage entry exists.
    fn exists_storage(&self, key: &[u8]) -> bool {
        self.storage(key).is_some()
    }

//    /// Clear storage entries which keys are start with the given prefix.
//    fn clear_prefix(&mut self, prefix: &[u8]);
}
