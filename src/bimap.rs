use std::collections::HashMap;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct BiMap<T: Eq + std::hash::Hash> {
    left2right: HashMap<T, T>,
    right2left: HashMap<T, T>,
}
impl<T: Eq + std::hash::Hash> BiMap<T> {
    pub(crate) fn new() -> Self {
        Self {
            left2right: Default::default(),
            right2left: Default::default(),
        }
    }

    pub(crate) fn iter(&self) -> std::collections::hash_map::Iter<'_, T, T> {
        self.left2right.iter()
    }
}
impl<T: Eq + std::hash::Hash + Copy + std::fmt::Debug> BiMap<T> {
    pub(crate) fn get(&self, x: &T) -> Option<&T> {
        self.left2right.get(x).or_else(|| self.right2left.get(x))
    }
    pub(crate) fn insert_new(&mut self, a: T, b: T) {
        assert_ne!(a, b);
        assert_eq!(None, self.left2right.insert(a, b));
        assert_eq!(None, self.right2left.insert(b, a));
    }
}
