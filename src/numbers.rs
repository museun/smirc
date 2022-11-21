use egui_extras::RetainedImage;

pub struct Numbers {
    pub images: [RetainedImage; 10],
    pub plus: RetainedImage,
}

impl Numbers {
    pub fn load() -> Self {
        macro_rules! load {
            ($id:expr) => {
                RetainedImage::from_image_bytes(
                    $id,
                    include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/numbers/", $id)),
                )
                .unwrap()
            };
        }

        Self {
            images: [
                load!("zero.png"),
                load!("one.png"),
                load!("two.png"),
                load!("three.png"),
                load!("four.png"),
                load!("five.png"),
                load!("six.png"),
                load!("seven.png"),
                load!("eight.png"),
                load!("nine.png"),
            ],
            plus: RetainedImage::from_image_bytes(
                "plus.png",
                include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/", "smorc.png")),
            )
            .unwrap(),
        }
    }

    pub fn digits(&self, mut input: usize) -> impl Iterator<Item = &RetainedImage> {
        let mut div = 1;
        while input >= div * 10 {
            div *= 10;
        }
        std::iter::from_fn(move || {
            if div == 0 {
                return None;
            }
            let v = input / div;
            input %= div;
            div /= 10;
            Some(&self.images[v])
        })
    }
}
