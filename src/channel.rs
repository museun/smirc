#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct Channel {
    pub id: String,
    pub profile_image_url: String,
    pub login: String,
    pub display_name: String,
}
