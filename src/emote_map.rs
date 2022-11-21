use std::collections::HashMap;

#[derive(Default)]
pub struct EmoteMap {
    map: HashMap<String, String>,
}

impl EmoteMap {
    pub fn get(&self, key: &str) -> Option<&str> {
        self.map.get(key).map(|c| &**c)
    }
}

impl<K, V> Extend<(K, V)> for EmoteMap
where
    K: ToString,
    V: ToString,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = (K, V)>,
    {
        self.map.extend(
            iter.into_iter()
                .map(|(k, v)| (k.to_string(), v.to_string())),
        )
    }
}
