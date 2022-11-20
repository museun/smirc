pub trait Repaint
where
    Self: Send + Sync + 'static + Clone,
{
    fn repaint(&self) {}
}

impl Repaint for egui::Context {
    fn repaint(&self) {
        self.request_repaint()
    }
}

impl Repaint for () {}
