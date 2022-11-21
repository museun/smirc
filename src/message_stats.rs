#[derive(Debug, Default)]
pub struct MessageStats {
    pub active: bool,
    pub unread: usize,
}

impl MessageStats {
    pub fn check_active(&mut self, active: bool) {
        self.active = active;
        if self.active {
            self.unread = 0
        }
    }
}
