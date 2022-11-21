use std::collections::HashMap;

use crate::{helix, UserFetcher};

#[derive(Default, Debug, serde::Serialize, serde::Deserialize)]
pub struct UserMap {
    pub map: HashMap<String, helix::User>,
}

impl UserMap {
    // TODO pre-populate this with the hidden user name endpoint
    pub fn get(&mut self, id: &str, fetcher: &UserFetcher) -> Option<&helix::User> {
        match self.map.get(id) {
            Some(user) => Some(user),
            None => {
                fetcher.request(id);
                None
            }
        }
    }
}
