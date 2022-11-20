use std::sync::Arc;

use ::serde::{Deserialize, Serialize};
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use tokio::sync::Mutex;

use crate::ChannelExt;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Badge {
    pub set_id: String,
    pub versions: Vec<Versions>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Versions {
    pub id: String,
    pub image_url_1x: String,
    pub image_url_2x: String,
    pub image_url_4x: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EmoteImage {
    pub format: Vec<String>,
    pub id: String,
    pub images: EmoteImages,
    pub name: String,
    pub scale: Vec<String>,
    pub theme_mode: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EmoteImages {
    pub url_1x: String,
    pub url_2x: String,
    pub url_4x: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct User {
    pub id: String,
    pub login: String,

    pub description: String,
    pub display_name: String,

    pub profile_image_url: String,
}

#[derive(Clone, Debug, Default, Deserialize)]
struct OAuth {
    access_token: String,
    refresh_token: Option<String>,
    expires_in: u64,
    token_type: String,

    #[serde(default)]
    client_id: String,

    #[serde(default)]
    bearer_token: String,
}

impl OAuth {
    pub async fn refresh(&mut self) {}

    pub async fn create(client_id: &str, client_secret: &str) -> anyhow::Result<Self> {
        anyhow::ensure!(!client_id.is_empty(), "twitch client id was empty");
        anyhow::ensure!(!client_secret.is_empty(), "twitch client secret was empty");

        #[derive(Serialize)]
        struct Query<'a> {
            client_id: &'a str,
            client_secret: &'a str,
            grant_type: &'a str,
        }

        let query = Query {
            client_id,
            client_secret,
            grant_type: "client_credentials",
        };

        let mut resp: Self = reqwest::Client::new()
            .post("https://id.twitch.tv/oauth2/token")
            .header("user-agent", USER_AGENT)
            .query(&query)
            .send()
            .await?
            .json()
            .await?;

        resp.client_id = client_id.to_string();
        resp.bearer_token = format!("Bearer {}", resp.access_token);

        Ok(resp)
    }
}

#[derive(Copy, Clone)]
pub enum IdOrLogin<'a> {
    Id(&'a str),
    Login(&'a str),
}

#[derive(Clone)]
pub struct Client {
    client: reqwest::Client,
    oauth: Arc<Mutex<OAuth>>,
}

impl Client {
    pub async fn create(client_id: &str, client_secret: &str) -> anyhow::Result<Self> {
        let oauth = OAuth::create(client_id, client_secret).await?;
        let client = reqwest::ClientBuilder::new()
            .default_headers(
                [
                    ("client-id", &*oauth.client_id),
                    ("authorization", &oauth.bearer_token),
                ]
                .into_iter()
                .map(|(k, v)| Ok((HeaderName::from_static(k), HeaderValue::from_str(v)?)))
                .collect::<anyhow::Result<HeaderMap>>()?,
            )
            .build()?;

        Ok(Self {
            client,
            oauth: Arc::new(Mutex::new(oauth)),
        })
    }

    pub async fn get_global_emotes(&self) -> anyhow::Result<Vec<EmoteImage>> {
        self.get_response("chat/emotes/global", ()).await
    }

    pub async fn get_badges(&self, broadcaster_id: &str) -> anyhow::Result<Vec<Badge>> {
        #[derive(Serialize)]
        struct Query<'a> {
            broadcaster_id: &'a str,
        }

        self.get_response("chat/badges", Query { broadcaster_id })
            .await
    }

    pub async fn get_global_badges(&self) -> anyhow::Result<Vec<Badge>> {
        self.get_response("chat/badges/global", ()).await
    }

    pub async fn get_users<'a, const N: usize>(
        &self,
        logins: [IdOrLogin<'a>; N],
    ) -> anyhow::Result<Vec<User>> {
        let logins = logins.map(|item| match item {
            IdOrLogin::Id(id) => ("id", id),
            IdOrLogin::Login(login) => ("login", login.without_octo()),
        });

        let mut out = vec![];
        for login in logins.chunks(100) {
            let query = login.iter().collect::<Vec<_>>();
            // TODO do this in a separate task
            let users = self.get_response("users", query).await?;
            out.extend(users);
        }
        Ok(out)
    }

    // TODO refresh on token expiring
    async fn get_response<T>(
        &self,
        ep: &str,
        query: impl Serialize + Send,
    ) -> anyhow::Result<Vec<T>>
    where
        T: for<'de> Deserialize<'de> + Send,
    {
        #[derive(Deserialize)]
        struct Resp<T> {
            data: Vec<T>,
        }

        let req = self
            .client
            .get(&format!("https://api.twitch.tv/helix/{ep}"))
            .query(&query);

        let resp: Resp<T> = req.send().await?.json().await?;
        Ok(resp.data)
    }
}

pub const USER_AGENT: &str = concat!(env!("CARGO_PKG_NAME"), "/", env!("CARGO_PKG_VERSION"));
