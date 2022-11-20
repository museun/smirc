use std::{collections::HashMap, time::Duration};

use tokio::{
    io::{AsyncBufReadExt, AsyncWriteExt, BufReader},
    net::TcpStream,
};

use crate::{BadgeMap, EmoteSpan, Repaint, TextSpan};

#[derive(Debug, Clone, Default)]
pub struct Tags(HashMap<String, String>);

impl Tags {
    pub(super) fn parse(input: &mut &str) -> Option<Self> {
        if !input.starts_with('@') {
            return None;
        }

        let (head, tail) = input.split_once(' ')?;
        *input = tail;

        let inner = head[1..]
            .split_terminator(';')
            .flat_map(|tag| tag.split_once('='))
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();

        Some(Self(inner))
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.0.get(key).map(|v| &**v)
    }

    pub fn get_parsed<T>(&self, key: &str) -> Option<Result<T, T::Err>>
    where
        T: std::str::FromStr,
        T::Err: std::fmt::Display,
    {
        self.get(key).map(<str>::parse)
    }

    pub fn insert(&mut self, key: impl ToString, val: impl ToString) {
        self.0.insert(key.to_string(), val.to_string());
    }

    pub fn badges(&self, map: &BadgeMap) -> Vec<String> {
        self.get("badges")
            .into_iter()
            .flat_map(|badges| badges.split(','))
            .flat_map(|badge| badge.split_once('/'))
            .flat_map(|(badge, version)| map.get(badge, version))
            .map(ToString::to_string)
            .collect()
    }

    pub fn emotes(&self, data: &str) -> Vec<TextSpan> {
        let chars = data.trim_end().chars().collect::<Vec<_>>();
        let mut emotes = self
            .get("emotes")
            .into_iter()
            .flat_map(|s| s.split('/'))
            .flat_map(|s| s.split_once(':'))
            .flat_map(|(emote, range)| {
                range
                    .split(',')
                    .flat_map(|c| c.split_once('-'))
                    .flat_map(|(start, end)| Some((start.parse().ok()?, end.parse().ok()?)))
                    .zip(std::iter::repeat(emote))
                    .map(|((start, end), kind): ((usize, usize), _)| {
                        (kind, (start, end - start + 1))
                    })
            })
            .collect::<Vec<(&str, (usize, usize))>>();

        emotes.sort_unstable_by_key(|(_, r)| *r);

        let trim = |data: &[char]| {
            let tail = data
                .iter()
                .rev()
                .take_while(|c| c.is_ascii_whitespace())
                .count();
            data.iter()
                .take(data.len() - tail)
                .skip_while(|c| c.is_ascii_whitespace())
                .collect()
        };

        let mut spans = vec![];
        let mut cursor = 0;

        for (emote, (start, end)) in emotes {
            if start != cursor {
                let text = trim(&chars[cursor..start]);
                spans.push(TextSpan::Text(text));
            }

            spans.push(TextSpan::Emote(EmoteSpan::new(emote)));
            cursor = start + end
        }

        if cursor != chars.len() {
            let text = trim(&chars[cursor..]);
            spans.push(TextSpan::Text(text));
        }

        spans
    }

    pub fn egui_color(&self) -> egui::Color32 {
        struct Color(u8, u8, u8);
        impl std::str::FromStr for Color {
            type Err = &'static str;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let s = match s.len() {
                    7 => &s[1..],
                    6 => s,
                    _ => return Err("invalid color"),
                };

                let color = u32::from_str_radix(s, 16).map_err(|_| "invalid hex digit")?;
                let (r, g, b) = (
                    ((color >> 16) & 0xFF) as _,
                    ((color >> 8) & 0xFF) as _,
                    (color & 0xFF) as _,
                );
                Ok(Self(r, g, b))
            }
        }

        impl Default for Color {
            fn default() -> Self {
                Self(0xFF, 0xFF, 0xFF)
            }
        }

        let Color(r, g, b) = self
            .get_parsed("color")
            .transpose()
            .ok()
            .flatten()
            .unwrap_or_default();
        egui::Color32::from_rgb(r, g, b)
    }
}

pub struct IrcWriter {
    sender: flume::Sender<String>,
}

impl IrcWriter {
    pub fn new() -> (Self, flume::Receiver<String>) {
        let (tx, rx) = flume::unbounded();
        (Self { sender: tx }, rx)
    }

    pub fn join(&self, channel: &str) {
        use std::borrow::Cow;

        let channel = if channel.starts_with('#') {
            Cow::Borrowed(channel)
        } else {
            Cow::Owned(format!("#{channel}"))
        };

        let _ = self.sender.send(format!("JOIN {channel}\r\n"));
    }

    pub fn part(&self, channel: &str) {
        use std::borrow::Cow;

        let channel = if channel.starts_with('#') {
            Cow::Borrowed(channel)
        } else {
            Cow::Owned(format!("#{channel}"))
        };

        let _ = self.sender.send(format!("PART {channel}\r\n"));
    }

    pub fn privmsg(&self, channel: &str, data: &str) {
        use std::borrow::Cow;

        let channel = if channel.starts_with('#') {
            Cow::Borrowed(channel)
        } else {
            Cow::Owned(format!("#{channel}"))
        };

        let _ = self.sender.send(format!("PRIVMSG {channel} :{data}\r\n"));
    }
}

pub struct Registration {
    pub name: String,
    pub oauth_token: String,
}

pub async fn run(
    addr: &str,
    reg: Registration,
    sink: flume::Sender<TwitchMessage>,
    recv: flume::Receiver<String>,
    repaint: impl Repaint + 'static,
) {
    enum MaybeDisconnect<T> {
        Ok(T),
        Break,
        Continue,
    }

    async fn check_error<T, E>(
        result: Result<T, E>,
        sink: &flume::Sender<TwitchMessage>,
    ) -> MaybeDisconnect<T>
    where
        T: Send,
        E: Send,
    {
        match result {
            Ok(val) => MaybeDisconnect::Ok(val),
            Err(_) => {
                if sink.send(TwitchMessage::Disconnected).is_err() {
                    return MaybeDisconnect::Break;
                }
                tokio::time::sleep(Duration::from_secs(5)).await;
                MaybeDisconnect::Continue
            }
        }
    }

    'main: loop {
        eprintln!("start of main loop");

        macro_rules! check {
            ($expr:expr) => {
                match check_error($expr, &sink).await {
                    MaybeDisconnect::Ok(val) => val,
                    MaybeDisconnect::Break => {
                        repaint.repaint();
                        break 'main;
                    }
                    MaybeDisconnect::Continue => {
                        repaint.repaint();
                        continue;
                    }
                }
            };
        }

        if sink.send(TwitchMessage::Connecting).is_err() {
            break;
        }
        repaint.repaint();

        let mut stream = check!(TcpStream::connect(addr).await);

        eprintln!("sending caps");
        for registration in [
            "CAP REQ :twitch.tv/membership\r\n",
            "CAP REQ :twitch.tv/tags\r\n",
            "CAP REQ :twitch.tv/commands\r\n",
        ] {
            check!(stream.write_all(registration.as_bytes()).await);
        }

        let Registration { name, oauth_token } = &reg;

        eprintln!("sending registration");
        for auth in [
            format!("PASS {oauth_token}\r\n"),
            format!("NICK {name}\r\n"),
        ] {
            check!(stream.write_all(auth.as_bytes()).await);
        }

        check!(stream.flush().await);

        let (read, mut write) = stream.into_split();
        let mut lines = BufReader::new(read).lines();

        let mut our_user_id = None;

        loop {
            let next = lines.next_line();
            let recv = recv.recv_async();

            tokio::select! {
                msg = next => {

                    let msg = check!(msg).as_deref().and_then(RawMessage::parse);
                    match msg {
                        Some(msg) => {
                            eprintln!("<- {}", msg.raw.escape_debug());
                            match msg.command {
                                Command::Ping => {
                                    check!(write.write_all(b"PONG tmi.twitch.tv\r\n").await);
                                    check!(write.flush().await);
                                }

                                Command::Ready => {
                                    if let Some(ready) = msg.as_ready() {
                                        our_user_id.replace(ready.user_id.to_string());

                                        let _ = sink.send(TwitchMessage::Ready(Identity {
                                            user_name: ready.user_name.unwrap_or(name).to_string(),
                                            user_id: ready.user_id.to_string(),
                                            color: ready.color,
                                        }));
                                    }
                                }
                                Command::Join => {
                                    if let Some(join) = msg.as_join() {
                                        if Some(join.user) == our_user_id.as_deref() {
                                            let msg = TwitchMessage::Join(join.channel.to_string());
                                            let _ = sink.send(msg);
                                        }
                                    }
                                }
                                Command::Part => {
                                    if let Some(part) = msg.as_part() {
                                        if Some(part.user) == our_user_id.as_deref() {
                                            let msg = TwitchMessage::Part(part.channel.to_string());
                                            let _ = sink.send(msg);
                                        }
                                    }
                                }

                                Command::Privmsg => {
                                    if let Some(privmsg) = msg.as_privmsg() {
                                        let msg = TwitchMessage::Privmsg(Privmsg {
                                            ts: msg.ts,
                                            tags: privmsg.tags.clone(),
                                            room_id: privmsg.target.to_string(),
                                            user_id: privmsg.sender.to_string(),
                                            data: privmsg.data.to_string(),
                                        });
                                        let _ = sink.send(msg);
                                    }
                                }

                                Command::Error => {
                                    if sink.send(TwitchMessage::Disconnected).is_err() {
                                        break 'main;
                                    }
                                    tokio::time::sleep(Duration::from_secs(5)).await;
                                    continue 'main;
                                }

                                Command::UserState => {
                                    let _ = sink.send(TwitchMessage::UserState(UserState {
                                        channel: msg.args.first().expect("channel attached to message").clone(),
                                        tags: msg.tags.clone(),
                                    }));
                                }

                                _ => {}
                            }

                            let _ = sink.send(TwitchMessage::Raw(msg));
                            repaint.repaint();
                        }
                        None => {
                            if sink.send(TwitchMessage::Disconnected).is_err() {
                                break 'main;
                            }
                            tokio::time::sleep(Duration::from_secs(5)).await;
                            continue 'main;
                        }
                    }
                }

                data = recv => {
                    let data = check!(data);
                    eprintln!("-> {}", data.escape_debug());
                    check!(write.write_all(data.as_bytes()).await);
                    check!(write.flush().await);
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum TwitchMessage {
    Connecting,
    Disconnected,

    Join(String),
    Part(String),

    Ready(Identity),
    Privmsg(Privmsg),

    UserState(UserState),

    Raw(RawMessage),
}

#[derive(Debug)]
pub struct Privmsg {
    pub ts: time::OffsetDateTime,
    pub tags: Tags,
    pub room_id: String,
    pub user_id: String,
    pub data: String,
}

#[derive(Debug)]
pub struct UserState {
    pub channel: String,
    pub tags: Tags,
}

#[derive(Debug)]
pub struct Identity {
    pub user_name: String,
    pub user_id: String,
    pub color: egui::Color32,
}

// TODO get rid of these
pub mod messages {
    use super::Tags;

    #[derive(Debug)]
    pub struct Ready<'a> {
        pub user_name: Option<&'a str>,
        pub user_id: &'a str,
        pub color: egui::Color32,
    }

    #[derive(Debug)]
    pub struct Join<'a> {
        pub channel: &'a str,
        pub user: &'a str,
    }

    #[derive(Debug)]
    pub struct Part<'a> {
        pub channel: &'a str,
        pub user: &'a str,
    }

    #[derive(Debug)]
    pub struct Privmsg<'a> {
        pub target: &'a str,
        pub sender: &'a str,
        pub data: &'a str,
        pub tags: &'a Tags,
    }
}

#[derive(Debug)]
pub enum Command {
    Ready,
    Ping,
    Join,
    Part,
    Privmsg,
    ClearMsg,
    ClearChat,
    UserState,
    Error,
    Other,
}

impl Command {
    fn parse(input: &mut &str) -> Self {
        let (head, tail) = input.split_at(input.find(' ').unwrap_or(input.len()));
        *input = tail;
        match head {
            "GLOBALUSERSTATE" => Self::Ready,
            "PING" => Self::Ping,
            "JOIN" => Self::Join,
            "PART" => Self::Part,
            "PRIVMSG" => Self::Privmsg,
            "CLEARMSG" => Self::ClearMsg,
            "CLEARCHAT" => Self::ClearChat,
            "USERSTATE" => Self::UserState,
            "ERROR" => Self::Error,
            _ => Self::Other,
        }
    }
}

#[derive(Debug)]
pub struct RawMessage {
    pub ts: time::OffsetDateTime,
    pub tags: Tags,
    pub command: Command,
    pub args: Vec<String>,
    pub data: Option<String>,
    pub raw: String,
}

impl RawMessage {
    // TODO get rid of this
    #[deprecated(note = "why is this a thing?")]
    fn as_ready(&self) -> Option<messages::Ready<'_>> {
        if !matches!(self.command, Command::Ready) {
            return None;
        }

        Some(messages::Ready {
            user_name: self.tags.get("display-name"),
            user_id: self.tags.get("user-id")?,
            color: self.tags.egui_color(),
        })
    }

    // TODO get rid of this
    #[deprecated(note = "why is this a thing?")]
    fn as_join(&self) -> Option<messages::Join<'_>> {
        if !matches!(self.command, Command::Join) {
            return None;
        }

        Some(messages::Join {
            channel: self.args.get(0)?,
            user: self.tags.get("user-id")?,
        })
    }

    // TODO get rid of this
    #[deprecated(note = "why is this a thing?")]
    fn as_part(&self) -> Option<messages::Part<'_>> {
        if !matches!(self.command, Command::Part) {
            return None;
        }

        Some(messages::Part {
            channel: self.args.get(0)?,
            user: self.tags.get("user-id")?,
        })
    }

    // TODO get rid of this
    #[deprecated(note = "why is this a thing?")]
    fn as_privmsg(&self) -> Option<messages::Privmsg<'_>> {
        if !matches!(self.command, Command::Privmsg) {
            return None;
        }

        Some(messages::Privmsg {
            target: self.tags.get("room-id")?,
            sender: self.tags.get("user-id")?,
            data: self.data.as_deref()?,
            tags: &self.tags,
        })
    }

    fn parse(input: &str) -> Option<Self> {
        let raw = input;

        let input = &mut input.trim();
        let tags = Tags::parse(input).unwrap_or_default();
        *input = input.trim();

        // XXX: why don't we save the user name?
        let (_head, tail) = input.split_at(input.find(' ').unwrap_or(input.len()));
        *input = tail.trim();

        let command = Command::parse(input);
        let (head, tail) = input.split_at(input.find(':').unwrap_or(input.len()));
        *input = tail;

        let args: Vec<_> = head
            .split_ascii_whitespace()
            .map(ToString::to_string)
            .collect();

        let data = input
            .trim_end()
            .strip_prefix(':')
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string());

        Some(Self {
            ts: time::OffsetDateTime::now_utc(),
            tags,
            command,
            args,
            data,
            raw: raw.to_string(),
        })
    }
}
