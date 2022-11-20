use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
    io::Write,
    time::{Duration, Instant},
};

use anyhow::Context as _;
use egui::Vec2;
use egui_extras::RetainedImage;
use image::ImageFormat;
use tokio_stream::StreamExt as _;

use crate::Repaint;

pub enum Image {
    Static(RetainedImage),
    Animated(Animated),
}

impl Image {
    pub fn show_size(&self, ui: &mut egui::Ui, size: Vec2) -> egui::Response {
        match self {
            Self::Static(image) => image.show_size(ui, size),
            Self::Animated(image) => {
                let dt = ui.input().stable_dt.min(0.1);

                if let Some((img, delay)) = image.frame(dt) {
                    let resp = img.show_size(ui, size);
                    ui.ctx().request_repaint_after(delay);
                    return resp;
                }
                ui.allocate_response(size, egui::Sense::hover().union(egui::Sense::click()))
            }
        }
    }

    pub fn load_retained_image(name: &str, data: &[u8]) -> anyhow::Result<RetainedImage> {
        RetainedImage::from_image_bytes(name, data)
            .map_err(|err| anyhow::anyhow!("cannot load '{name}': {err}"))
    }
}

pub struct Animated {
    frames: Vec<RetainedImage>,
    intervals: Vec<Duration>,
    position: Cell<usize>,
    last: Cell<Option<Instant>>,
}

impl Animated {
    pub fn frame(&self, dt: f32) -> Option<(&RetainedImage, Duration)> {
        let pos = self.position.get();
        let delay = self.intervals.get(pos)?;

        match self.last.get() {
            Some(last) if last.elapsed().as_secs_f32() >= delay.as_secs_f32() - dt => {
                self.position.set((pos + 1) % self.frames.len());
                self.last.set(Some(Instant::now()));
            }
            Some(..) => {}
            None => {
                self.last.set(Some(Instant::now()));
            }
        }

        self.frames.get(pos).map(|frame| (frame, *delay))
    }

    pub fn load_apng(name: &str, data: &[u8]) -> anyhow::Result<Self> {
        use image::ImageDecoder as _;
        let dec = image::codecs::png::PngDecoder::new(data)?;
        anyhow::ensure!(dec.is_apng(), "expected an animated png");
        Self::load_frames(name, dec.total_bytes() as _, dec.apng())
    }

    pub fn load_gif(name: &str, data: &[u8]) -> anyhow::Result<Self> {
        use image::ImageDecoder as _;
        let dec = image::codecs::gif::GifDecoder::new(data)?;
        Self::load_frames(name, dec.total_bytes() as _, dec)
    }

    fn load_frames<'a>(
        name: &str,
        hint: usize,
        decoder: impl image::AnimationDecoder<'a>,
    ) -> anyhow::Result<Self> {
        let mut buf = std::io::Cursor::new(Vec::with_capacity(hint));

        let (mut frames, mut intervals) = (vec![], vec![]);

        for (i, frame) in decoder.into_frames().enumerate() {
            let frame = frame?;
            let delay = Duration::from(frame.delay());

            // TODO use DynamicImage instead
            frame.buffer().write_to(&mut buf, ImageFormat::Png)?;
            buf.flush().expect("flush image during transcode");

            let pos = buf.position();
            buf.set_position(0);

            let image =
                Image::load_retained_image(&format!("{name}_{i}"), &buf.get_ref()[..pos as usize])
                    .with_context(|| anyhow::anyhow!("cannot decode frame: {i}"))?;
            frames.push(image);
            intervals.push(delay);
        }

        Ok(Self {
            frames,
            intervals,
            position: Cell::default(),
            last: Cell::default(),
        })
    }
}

pub struct Cache {
    map: HashMap<String, Image>,
    loader: Loader,
}

impl Cache {
    pub fn new(loader: Loader) -> Self {
        Self {
            map: HashMap::default(),
            loader,
        }
    }

    pub fn get(&mut self, url: &str) -> Option<&Image> {
        match self.map.get(url) {
            Some(img) => Some(img),
            None => {
                self.loader.request(url);
                None
            }
        }
    }

    pub fn poll(&mut self) {
        for (k, v) in self.loader.produce.try_iter() {
            self.map.insert(k, v);
        }
    }
}

#[derive(Clone)]
pub struct Loader {
    submit: flume::Sender<String>,
    produce: flume::Receiver<(String, Image)>,
}

impl Loader {
    pub fn spawn(repaint: impl Repaint + 'static) -> Self {
        let (submit, submit_rx) = flume::unbounded::<String>();
        let (produce_tx, produce) = flume::unbounded();

        crate::runtime::spawn(async move {
            let mut seen = HashSet::new();
            let mut stream = submit_rx.into_stream();
            let client = reqwest::Client::new();

            while let Some(url) = stream.next().await {
                if !seen.insert(url.clone()) {
                    continue;
                }

                let client = client.clone();
                let tx = produce_tx.clone();
                let repaint = repaint.clone();

                tokio::spawn(async move {
                    let Some(data) = Self::fetch(client, &url).await else { return };

                    tokio::task::spawn_blocking(move || match Self::load(&url, data) {
                        Ok(img) => {
                            let _ = tx.send((url, img));
                            repaint.repaint();
                        }
                        Err(err) => eprintln!("cannot fetch: {url}: {err}"),
                    });
                });
            }
        });

        Self { submit, produce }
    }

    pub fn request(&self, url: &str) {
        let _ = self.submit.send(url.to_string());
    }

    // TODO cache this
    async fn fetch(client: reqwest::Client, url: &str) -> Option<Vec<u8>> {
        eprintln!("getting: {url}");
        let resp = client.get(url).send().await.ok()?;
        if resp.status().as_u16() == 404 {
            // TODO report this
            return None;
        }

        resp.bytes().await.ok().map(|d| d.to_vec())
    }

    fn load(name: &str, data: Vec<u8>) -> anyhow::Result<Image> {
        let img = match image::guess_format(&data[..data.len().min(128)])
            .map_err(|err| anyhow::anyhow!("cannot guess format for: '{name}': {err}"))?
        {
            ImageFormat::Png => {
                let dec = image::codecs::png::PngDecoder::new(&*data).map_err(|err| {
                    anyhow::anyhow!("expected png, got something else for '{name}': {err}")
                })?;

                if dec.is_apng() {
                    Animated::load_apng(name, &data).map(Image::Animated)?
                } else {
                    Image::load_retained_image(name, &data).map(Image::Static)?
                }
            }
            ImageFormat::Jpeg => Image::load_retained_image(name, &data).map(Image::Static)?,
            ImageFormat::Gif => Animated::load_gif(name, &data).map(Image::Animated)?,
            fmt => anyhow::bail!("unsupported format for '{name}': {fmt:?}"),
        };

        Ok(img)
    }
}
