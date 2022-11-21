use std::collections::HashMap;

#[derive(Default)]
pub struct BadgeMap {
    map: HashMap<String, HashMap<String, String>>,
}

impl BadgeMap {
    pub fn get(&self, id: &str, version: &str) -> Option<&str> {
        self.map.get(id)?.get(version).map(|v| &**v)
    }
}

impl<K, T, V> Extend<(K, (T, V))> for BadgeMap
where
    K: ToString,
    T: ToString,
    V: ToString,
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = (K, (T, V))>,
    {
        for (id, (k, v)) in iter {
            self.map
                .entry(id.to_string())
                .or_default()
                .insert(k.to_string(), v.to_string());
        }
    }
}
