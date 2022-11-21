use std::collections::VecDeque;

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Ring<T> {
    max: usize,
    pub buf: VecDeque<T>,
}

impl<T> Ring<T> {
    pub fn with_capacity(max: usize) -> Self {
        assert!(max > 0, "max cannot be empty");
        Self {
            max,
            buf: VecDeque::with_capacity(max),
        }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn empty_available(&self) -> usize {
        self.max - self.len()
    }

    pub fn push(&mut self, item: T) {
        while self.buf.len() >= self.max {
            self.buf.pop_front();
        }
        self.buf.push_back(item);
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> + ExactSizeIterator {
        self.buf.iter()
    }
}
