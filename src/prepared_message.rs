use crate::{chat, BadgeMap};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PreparedMessage {
    pub pm: chat::Privmsg,
    pub ts: String,
    pub color: egui::Color32,
    pub badges: Vec<String>,
    pub message_spans: Vec<crate::TextSpan>,
}

impl PreparedMessage {
    pub fn new(pm: chat::Privmsg, map: &mut BadgeMap) -> Self {
        static FORMAT: &[time::format_description::FormatItem<'static>] =
            time::macros::format_description!("[hour]:[minute]:[second]");

        let ts = pm.ts.to_offset(
            time::UtcOffset::current_local_offset() //
                .expect("system should know when UTC is"),
        );

        Self {
            color: pm.tags.egui_color(),
            badges: pm
                .tags
                .badges()
                // BUG this flat_map should actually be a map
                // it should look up unknown badges (how?)
                .flat_map(|(k, v)| map.get(k, v))
                .map(ToString::to_string)
                .collect(),
            message_spans: pm.tags.emotes(&pm.data),
            ts: ts.format(&FORMAT).expect("valid timestamp"),
            pm,
        }
    }
}
