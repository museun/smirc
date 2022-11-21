#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum TextSpan {
    Text(String),
    Emote(EmoteSpan),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct EmoteSpan {
    urls: [String; 2],
}

impl EmoteSpan {
    // TODO be much smarter about this non-sense
    pub fn new(id: &str) -> Self {
        Self {
            urls: [
                format!(
                    "https://static-cdn.jtvnw.net/emoticons/v2/{id}/{format}/{theme_mode}/{scale}",
                    id = id,
                    format = "animated",
                    theme_mode = "dark",
                    scale = "2.0"
                ),
                format!(
                    "https://static-cdn.jtvnw.net/emoticons/v2/{id}/{format}/{theme_mode}/{scale}",
                    id = id,
                    format = "static",
                    theme_mode = "dark",
                    scale = "2.0"
                ),
            ],
        }
    }

    pub fn as_urls(&self) -> impl Iterator<Item = &str> + ExactSizeIterator {
        self.urls.iter().map(|c| &**c)
    }
}
