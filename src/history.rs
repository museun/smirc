use std::{collections::HashMap, io::Write, path::Path};

use serde::{Deserialize, Serialize};

use crate::PreparedMessage;

#[derive(Default, Serialize, Deserialize)]
#[serde(transparent)]
pub struct History {
    pub map: HashMap<String, Vec<PreparedMessage>>,
}

impl History {
    pub fn add<'a>(&mut self, channel: &str, iter: impl IntoIterator<Item = &'a PreparedMessage>) {
        let list = self.map.entry(channel.to_string()).or_default();
        list.extend(iter.into_iter().cloned());
        list.sort_unstable_by_key(|msg| msg.pm.ts);
        list.dedup_by_key(|msg| msg.pm.ts)
    }

    // TODO std::io::Error
    pub fn save(&self, path: impl AsRef<Path>) {
        let _ = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(path)
            .ok()
            .map(std::io::BufWriter::new)
            .map(|mut fi| {
                let _ = serde_json::to_writer(&mut fi, self);
                let _ = fi.flush();
            });
    }

    pub fn load(path: impl AsRef<Path>) -> Self {
        std::fs::OpenOptions::new()
            .read(true)
            .open(path)
            .ok()
            .map(std::io::BufReader::new)
            .and_then(|mut fi| serde_json::from_reader(&mut fi).ok())
            .unwrap_or_default()
    }
}
