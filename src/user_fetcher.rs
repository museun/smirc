use std::collections::HashSet;

use tokio_stream::StreamExt;

use crate::{helix, Repaint, UserMap};

pub struct UserFetcher {
    submit: flume::Sender<String>,
    produce: flume::Receiver<Vec<helix::User>>,
}

impl UserFetcher {
    pub fn spawn(client: helix::Client, repaint: impl Repaint + 'static) -> Self {
        let (submit, submit_rx) = flume::unbounded::<String>();
        let (produce_tx, produce) = flume::unbounded();

        let mut seen = HashSet::new();
        crate::runtime::spawn(async move {
            let mut stream = submit_rx.into_stream();

            while let Some(id) = stream.next().await {
                if !seen.insert(id.clone()) {
                    continue;
                }

                let client = client.clone();
                let tx = produce_tx.clone();
                let repaint = repaint.clone();

                tokio::spawn(async move {
                    let out = client.get_users([helix::IdOrLogin::Id(&id)]).await?;
                    if !out.is_empty() {
                        let _ = tx.send_async(out).await;
                        repaint.repaint();
                    }
                    anyhow::Result::<_, anyhow::Error>::Ok(())
                });
            }
        });

        Self { submit, produce }
    }

    pub fn request(&self, id: &str) {
        let _ = self.submit.send(id.to_string());
    }

    pub fn poll(&mut self, map: &mut UserMap) {
        for user in self.produce.try_iter().into_iter().flatten() {
            map.map.insert(user.id.clone(), user);
        }
    }
}
