use crate::{MessageStats, PreparedMessage, Ring};

#[derive(Debug)]
pub struct Buffer {
    pub ring: Ring<PreparedMessage>,
    pub message_stats: MessageStats,
    pub text: String,
}

impl Buffer {
    const MAX_SIZE: usize = 100;

    pub fn new() -> Self {
        Self {
            ring: Ring::with_capacity(Self::MAX_SIZE),
            message_stats: MessageStats::default(),
            text: String::with_capacity(1024),
        }
    }

    pub fn clear(&mut self) {
        self.ring.buf.clear();
    }

    pub fn load_history<'a>(&mut self, iter: impl IntoIterator<Item = &'a PreparedMessage>) {
        for msg in iter.into_iter().cloned() {
            self.ring.push(msg)
        }
    }

    pub fn append(&mut self, message: PreparedMessage) {
        self.message_stats.unread = self
            .message_stats
            .active
            .then_some(0)
            .unwrap_or(self.message_stats.unread + 1);
        self.ring.push(message);
    }
}
