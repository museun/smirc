// #![cfg_attr(debug_assertions, allow(dead_code, unused_variables,))]
use std::collections::HashMap;

use chat::Tags;
use eframe::{epaint::Shadow, CreationContext, IconData, NativeOptions};
use egui::{
    style::Margin, vec2, Align, Align2, Area, CentralPanel, Color32, Frame, Grid, Label, Layout,
    Order, Pos2, Rect, RichText, Rounding, ScrollArea, Sense, SidePanel, Slider, Stroke, TextEdit,
    TextStyle, TopBottomPanel, Vec2, Window,
};

use egui_notify::Toasts;

use tokio::sync::oneshot::Receiver;

mod channel;
use channel::Channel;

mod state;
use state::State;

mod runtime;

mod image;
use crate::image as img;

mod repaint;
use repaint::Repaint;

mod helix;

mod action;
use action::Action;

mod keybind;
use keybind::KeyBind;

mod actions;
use actions::Actions;

mod history;
use history::History;

mod presence;
use presence::Presence;

mod util;
use util::{ChannelExt, WithIter};

mod numbers;
use numbers::Numbers;

mod ring;
use ring::Ring;

mod prepared_message;
use prepared_message::PreparedMessage;

mod buffer;
use buffer::Buffer;

mod message_stats;
use message_stats::MessageStats;

mod chat;

mod user_fetcher;
use user_fetcher::UserFetcher;

mod user_map;
use user_map::UserMap;

mod emote_map;
use emote_map::EmoteMap;

mod badge_map;
use badge_map::BadgeMap;

mod span;
use span::{EmoteSpan, TextSpan};

struct Application {
    state: State,
    emotes: EmoteMap,
    badges: BadgeMap,
    presences: HashMap<String, Presence>,
    cache: img::Cache,
    writer: chat::IrcWriter,
    helix: helix::Client,
    user_fetcher: UserFetcher,
    active: ActiveLayer,
    toasts: Toasts,
    history: History,
    twitch: flume::Receiver<chat::TwitchMessage>,
    identity: Option<chat::Identity>,
    numbers: Numbers,
}

impl Application {
    const SAVE_KEY: &'static str = concat!(env!("CARGO_PKG_NAME"), "_settings");
    const HISTORY_FILE: &'static str = concat!(env!("CARGO_MANIFEST_DIR"), "/", "history.json");
}

impl Application {
    fn new(
        mut state: State,
        helix: helix::Client,
        writer: chat::IrcWriter,
        emotes: EmoteMap,
        badges: BadgeMap,
        history: History,
        twitch: flume::Receiver<chat::TwitchMessage>,
        numbers: Numbers,
        repaint: impl Repaint + 'static,
    ) -> Self {
        for (i, id) in state.channels.iter().map(|ch| &ch.id).enumerate() {
            let Some(history) = history.map.get(id) else {continue};
            let buffer = &mut state.buffers[i];
            buffer.load_history(history);
        }

        Self {
            state,
            emotes,
            badges,
            cache: img::Cache::new(img::Loader::spawn(repaint.clone())),
            user_fetcher: UserFetcher::spawn(helix.clone(), repaint),
            writer,
            helix,
            presences: HashMap::new(),
            active: ActiveLayer::default(),
            toasts: Toasts::new(),
            history,
            twitch,
            identity: None,
            numbers,
        }
    }

    fn handle_keypress(&mut self, ctx: &egui::Context) {
        if ctx.input().key_pressed(egui::Key::Escape) {
            self.handle_cancel();
        }

        if ctx.input().key_pressed(egui::Key::Enter) {
            self.handle_accept();
        }

        // TODO handle these magic keys better
        if ctx.input().key_pressed(egui::Key::F12) {
            let val = ctx.debug_on_hover();
            ctx.set_debug_on_hover(!val)
        }

        if ctx.input().key_pressed(egui::Key::ArrowDown) {
            self.handle_select_next();
        }

        if ctx.input().key_pressed(egui::Key::ArrowUp) {
            self.handle_select_previous();
        }

        let Some(keybind) = Self::find_first_keybind(ctx) else { return };
        let Some(action) = self.state.actions.find_action(keybind) else { return };

        use Action::*;
        match action {
            ShowCommandPalette => self.show_command_palette(),
            ToggleJoinWindow => self.toggle_join_window(),
            ShowKeyBindings => self.show_key_bindings(),
            ShowSettings => self.show_settings(),
            SwitchChannel0 => self.switch_channel(0),
            SwitchChannel1 => self.switch_channel(1),
            SwitchChannel2 => self.switch_channel(2),
            SwitchChannel3 => self.switch_channel(3),
            SwitchChannel4 => self.switch_channel(4),
            SwitchChannel5 => self.switch_channel(5),
            SwitchChannel6 => self.switch_channel(6),
            SwitchChannel7 => self.switch_channel(7),
            SwitchChannel8 => self.switch_channel(8),
            SwitchChannel9 => self.switch_channel(9),
            NextChannel => self.next_channel(),
            PreviousChannel => self.previous_channel(),
        };
    }

    fn find_first_keybind(ctx: &egui::Context) -> Option<KeyBind> {
        let mut keybind = ctx.input().events.iter().find_map(|event| match event {
            egui::Event::Key {
                key,
                pressed,
                modifiers,
            } if !*pressed => Some(KeyBind {
                modifiers: *modifiers,
                key: *key,
            }),
            _ => None,
        })?;

        // NOTE: egui treats ctrl weirdly, so we'll pretend it doesn't exist and use `command` instead
        keybind.modifiers.ctrl = false;
        Some(keybind)
    }

    fn handle_select_next(&mut self) {
        self.active.handle_select_next(&mut self.state)
    }

    fn handle_select_previous(&mut self) {
        self.active.handle_select_previous(&mut self.state)
    }

    fn handle_cancel(&mut self) {
        self.active.handle_cancel();
    }

    fn handle_accept(&mut self) {
        self.active.handle_accept(&mut self.state);
    }

    fn switch_channel(&mut self, channel: usize) {
        if channel >= self.state.channels.len() {
            return;
        }

        self.state.selected = channel;
    }

    fn next_channel(&mut self) {
        self.state.selected = (self.state.selected + 1) % self.state.channels.len();
    }

    fn previous_channel(&mut self) {
        self.state.selected = self
            .state
            .selected
            .checked_sub(1)
            .unwrap_or(self.state.channels.len() - 1);
    }

    fn show_key_bindings(&mut self) {
        self.active.show_key_bindings();
    }

    fn show_settings(&mut self) {
        self.active.show_settings();
    }

    fn show_command_palette(&mut self) {
        self.active.show_command_palette(&mut self.state);
    }

    fn toggle_join_window(&mut self) {
        self.active.show_add_channel()
    }

    fn poll(&mut self) {
        self.cache.poll();
        self.user_fetcher.poll(&mut self.state.user_map);
        self.active.poll(&mut self.state, &mut self.toasts);
        self.poll_twitch();
    }

    fn poll_twitch(&mut self) {
        use chat::TwitchMessage::*;
        for msg in self.twitch.try_iter() {
            match msg {
                Connecting => {
                    eprintln!("Connecting")
                    // TODO display this in the UI
                }
                Disconnected => {
                    eprintln!("Disconnected")
                    // TODO display this in the UI
                }
                Join(id) => {
                    self.state.user_map.get(&id, &self.user_fetcher);
                    // TODO add buffer here, not on user lookup
                }
                Part(id) => {
                    self.state.user_map.get(&id, &self.user_fetcher);
                    // TODO remove the buffer
                }
                Ready(ready) => {
                    eprintln!("connected: {ready:#?}");
                    for channel in &self.state.channels {
                        self.writer.join(&channel.login);
                    }
                    self.identity.replace(ready);
                }

                UserState(userstate) => {
                    self.presences
                        .entry(userstate.channel)
                        .and_modify(|p| {
                            p.tags = userstate.tags.clone(); // unfortunate
                        })
                        .or_insert(Presence {
                            tags: userstate.tags,
                        });
                }

                Privmsg(privmsg) => {
                    // TODO make this get both at the same time
                    self.state
                        .user_map
                        .get(&privmsg.room_id, &self.user_fetcher);

                    self.state
                        .user_map
                        .get(&privmsg.user_id, &self.user_fetcher);

                    if let Some(index) = self
                        .state
                        .channels
                        .iter()
                        .position(|ch| ch.id == privmsg.room_id)
                    {
                        let buffer = &mut self.state.buffers[index];
                        buffer.append(PreparedMessage::new(privmsg, &mut self.badges));
                    }
                }

                Raw(..) => {}
            }
        }
    }
}

impl eframe::App for Application {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        self.poll();
        self.handle_keypress(ctx);

        self.toasts.show(ctx);

        self.active.display(
            &mut self.state,
            &mut self.cache,
            &self.identity,
            &self.presences,
            &mut self.emotes,
            &mut self.badges,
            &self.writer,
            &self.user_fetcher,
            &mut self.toasts,
            &self.helix,
            &self.numbers,
            ctx,
            frame,
        );
    }

    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        storage.set_string(
            Self::SAVE_KEY,
            serde_json::to_string(&self.state).expect("valid json"),
        );

        for (buffer, channel) in self.state.buffers.iter().zip(self.state.channels.iter()) {
            self.history.add(&channel.id, buffer.ring.iter())
        }

        self.history.save(Self::HISTORY_FILE);
    }
}

#[derive(Default, Debug)]
enum ActiveLayer {
    CommandPalette {
        selected: PaletteSelection,
    },
    AddChannel {
        state: AddChannelState,
    },
    Settings,
    KeyBindings,
    #[default]
    MainView,
}

#[derive(Debug, Default)]
enum AddChannelState {
    Waiting {
        name: String,
        waiting: Receiver<Vec<helix::User>>,
    },
    Produce {
        name: String,
        users: Vec<helix::User>,
    },
    Error {
        error: AddChannelError,
    },
    Editing {
        input: String,
    },
    #[default]
    Nothing,
}

impl AddChannelState {
    fn poll(&mut self, app_state: &mut State, toasts: &mut Toasts) {
        match self {
            Self::Waiting { name, waiting } => {
                if let Ok(users) = waiting.try_recv() {
                    *self = Self::Produce {
                        name: std::mem::take(name),
                        users,
                    }
                }
            }

            Self::Produce { name, users } if users.is_empty() => {
                *self = Self::Error {
                    error: AddChannelError::NotFound(std::mem::take(name)),
                };
            }

            Self::Produce { users, .. } => {
                let user = users.remove(0);
                app_state.channels.push(Channel {
                    id: user.id,
                    profile_image_url: user.profile_image_url,
                    login: user.login,
                    display_name: user.display_name,
                });
                app_state.buffers.push(Buffer::new());
                std::mem::take(self);
            }

            Self::Error { error } => {
                toasts.error(match error {
                    AddChannelError::NotFound(name) => {
                        let output = format!("Channel not found: {name}");
                        *self = Self::Editing {
                            input: std::mem::take(name),
                        };
                        output
                    }
                    AddChannelError::Invalid(name) => {
                        let output = format!("Channel name was invalid: {name}");
                        *self = Self::Editing {
                            input: std::mem::take(name),
                        };
                        output
                    }
                });
            }

            _ => {}
        }
    }
}

#[derive(Debug)]
enum AddChannelError {
    NotFound(String),
    Invalid(String),
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
enum PaletteSelection {
    Actions(usize),
    Channels(usize),
}

impl PaletteSelection {
    fn next(&mut self, state: &State) {
        match self {
            Self::Actions(index) if state.channels.is_empty() => {
                if let Some((next, ..)) = state
                    .actions
                    .list_actions()
                    .skip_while(|(i, ..)| i <= &*index)
                    .next()
                {
                    *index = next;
                    return;
                }

                *index = state.actions.first_visible_action_index();
            }

            Self::Actions(index) => {
                if *index == state.actions.last_visible_action_index() {
                    *self = Self::Channels(0);
                    return;
                }

                if let Some((next, ..)) = state
                    .actions
                    .list_actions()
                    .skip_while(|(i, ..)| i <= &*index)
                    .next()
                {
                    *index = next;
                    return;
                }

                *index = state.actions.first_visible_action_index();
            }

            Self::Channels(index) if state.actions.is_empty() => {
                *index = (*index + 1) % state.channels.len();
            }

            Self::Channels(index) => {
                if *index == state.channels.len() - 1 {
                    *self = Self::Actions(state.actions.first_visible_action_index());
                    return;
                }

                *index = (*index + 1) % state.channels.len();
            }
        }
    }

    fn previous(&mut self, state: &State) {
        match self {
            Self::Actions(index) if state.channels.is_empty() => {
                *index = index
                    .checked_sub(1)
                    .unwrap_or_else(|| state.actions.last_visible_action_index())
            }

            Self::Actions(index) => match index.checked_sub(1) {
                Some(i) => {
                    *index = i;
                    if *index < state.actions.first_visible_action_index() {
                        *self = Self::Channels(state.channels.len() - 1)
                    }
                }
                None => *self = Self::Channels(state.channels.len() - 1),
            },

            Self::Channels(index) if state.actions.is_empty() => {
                *index = index.checked_sub(1).unwrap_or(state.channels.len() - 1)
            }

            Self::Channels(index) => match index.checked_sub(1) {
                Some(i) => *index = i,
                None => *self = Self::Actions(state.actions.last_visible_action_index()),
            },
        }
    }
}

impl ActiveLayer {
    const fn is_main_view(&self) -> bool {
        matches!(self, Self::MainView)
    }

    const fn is_command_palette(&self) -> bool {
        matches!(self, Self::CommandPalette { .. })
    }

    const fn is_settings(&self) -> bool {
        matches!(self, Self::Settings)
    }

    const fn is_keybindings(&self) -> bool {
        matches!(self, Self::KeyBindings { .. })
    }

    // TODO why do we poll here?
    fn poll(&mut self, app_state: &mut State, toasts: &mut Toasts) {
        if let Self::AddChannel { state } = self {
            state.poll(app_state, toasts)
        }
    }

    // TODO really collate these into view structs
    fn display(
        &mut self,
        state: &mut State,
        cache: &mut image::Cache,
        identity: &Option<chat::Identity>,
        presences: &HashMap<String, Presence>,
        emotes: &mut EmoteMap,
        badges: &mut BadgeMap,
        writer: &chat::IrcWriter,
        fetcher: &UserFetcher,
        toasts: &mut Toasts,
        helix: &helix::Client,
        numbers: &Numbers,
        ctx: &egui::Context,
        frame: &eframe::Frame,
    ) {
        let active = self.is_main_view();

        CentralPanel::default()
            .frame(Frame::none().fill(ctx.style().visuals.faint_bg_color))
            .show(ctx, |ui| {
                Self::display_main_view(
                    active, state, cache, emotes, badges, presences, identity, writer, fetcher,
                    numbers, ui,
                );
                match self {
                    Self::CommandPalette { selected } => {
                        match Self::display_command_palette(state, cache, selected, ctx, frame) {
                            PaletteAction::Select => {
                                self.handle_accept(state);
                            }
                            PaletteAction::RemoveChannel(i) => {
                                state.channels.remove(i);
                                state.buffers.remove(i);
                                if state.selected == i {
                                    state.selected = i.saturating_sub(1);
                                }
                            }
                            PaletteAction::Nothing => {}
                        }
                    }

                    Self::KeyBindings => Self::display_key_bindings(state, ctx, frame),

                    // TODO why is this like this?
                    Self::AddChannel {
                        state: channel_state,
                    } => {
                        match Self::display_add_channel(
                            state,
                            toasts,
                            channel_state,
                            helix,
                            ctx,
                            frame,
                        ) {
                            AddChannelAction::Close => {
                                std::mem::take(channel_state);
                                self.show_add_channel();
                            }
                            AddChannelAction::Waiting { name, waiting } => {
                                *channel_state = AddChannelState::Waiting { name, waiting };
                            }
                            AddChannelAction::KeepOpen => {}
                        }
                    }

                    Self::Settings => Self::display_settings(state, ctx, frame),

                    Self::MainView => {}
                }
            });
    }

    fn show_add_channel(&mut self) {
        let next = match self {
            Self::AddChannel { .. } => Self::MainView,
            _ => Self::AddChannel {
                state: AddChannelState::Editing {
                    input: String::new(),
                },
            },
        };

        *self = next;
    }

    fn show_settings(&mut self) {
        if self.is_settings() {
            *self = Self::MainView;
            return;
        }
        eprintln!("switching to settings");
        *self = Self::Settings;
    }

    fn show_key_bindings(&mut self) {
        if self.is_keybindings() {
            *self = Self::MainView;
            return;
        }

        *self = Self::KeyBindings;
    }

    fn show_command_palette(&mut self, state: &mut State) {
        if self.is_command_palette() {
            self.handle_select_next(state);
            return;
        }

        *self = Self::CommandPalette {
            selected: PaletteSelection::Actions(state.actions.first_visible_action_index()),
        };
    }

    fn handle_cancel(&mut self) {
        std::mem::take(self);
    }

    fn handle_select_next(&mut self, state: &mut State) {
        if let Self::CommandPalette { selected, .. } = self {
            selected.next(state);
        }
    }

    fn handle_select_previous(&mut self, state: &mut State) {
        if let Self::CommandPalette { selected, .. } = self {
            selected.previous(state);
        }
    }

    fn handle_accept(&mut self, state: &mut State) {
        if let Self::CommandPalette { selected, .. } = self {
            match selected {
                PaletteSelection::Actions(index) => {
                    let action = state.actions[*index];
                    match action {
                        Action::ToggleJoinWindow => {
                            self.show_add_channel();
                            return;
                        }
                        Action::ShowKeyBindings => {
                            self.show_key_bindings();
                            return;
                        }
                        Action::ShowSettings => {
                            self.show_settings();
                        }
                        _ => {}
                    }
                }
                PaletteSelection::Channels(index) => {
                    state.selected = *index;
                }
            }
            self.handle_cancel();
        }
    }

    fn display_add_channel(
        state: &mut State,
        toasts: &mut Toasts,
        add_channel_state: &mut AddChannelState,
        helix: &helix::Client,
        ctx: &egui::Context,
        frame: &eframe::Frame,
    ) -> AddChannelAction {
        if let AddChannelState::Nothing = add_channel_state {
            return AddChannelAction::Close;
        }

        let mut action = AddChannelAction::KeepOpen;
        AddChannel {
            state,
            toasts,
            add_channel_state,
            helix,
            action: &mut action,
        }
        .display(ctx, frame);
        action
    }

    // TODO make this less bad
    fn display_key_bindings(state: &mut State, ctx: &egui::Context, frame: &eframe::Frame) {
        let Vec2 { y: h, .. } = frame.info().window_info.size;

        Window::new("show_key_bindings")
            .title_bar(false)
            .anchor(Align2::CENTER_CENTER, vec2(0.0, 0.0))
            .default_height(h - 100.0)
            .collapsible(false)
            .resizable(false)
            .frame(Frame::window(&ctx.style()).shadow(Shadow {
                extrusion: 0.0,
                color: Color32::TRANSPARENT,
            }))
            .show(ctx, |ui| {
                ScrollArea::vertical()
                    .auto_shrink([false, false])
                    .show(ui, |ui| {
                        Grid::new("key_bindings")
                            .striped(true)
                            .num_columns(2)
                            .show(ui, |ui| {
                                for (k, v) in &state.actions.map {
                                    ui.monospace(k.stringify());
                                    ui.horizontal(|ui| {
                                        ui.monospace(v.stringify());
                                        ui.add_space(ui.available_width());
                                    });
                                    ui.end_row();
                                }
                            });
                    });
            });
    }

    fn display_command_palette(
        state: &mut State,
        cache: &mut image::Cache,
        selected: &mut PaletteSelection,
        ctx: &egui::Context,
        frame: &eframe::Frame,
    ) -> PaletteAction {
        let actions = &state.actions;
        let channels = &state.channels;

        CommandPalette {
            actions,
            channels,
            cache,
            selected,
        }
        .display(ctx, frame)
    }

    fn display_settings(state: &mut State, ctx: &egui::Context, frame: &eframe::Frame) {
        let Vec2 { y: h, .. } = frame.info().window_info.size;

        Window::new("show_settings")
            .title_bar(false)
            .anchor(Align2::CENTER_CENTER, vec2(0.0, 0.0))
            .default_height(h - 100.0)
            .collapsible(false)
            .resizable(false)
            .frame(Frame::window(&ctx.style()).shadow(Shadow {
                extrusion: 0.0,
                color: Color32::TRANSPARENT,
            }))
            .show(ctx, |ui| {
                ui.add(
                    Slider::new(&mut state.icon_size, 16.0..=64.0)
                        .step_by(2.0)
                        .text("Icon size"),
                )
            });
    }

    // TODO collate these into view structs
    // TODo split this into multiple managable parts
    fn display_main_view(
        active: bool,
        state: &mut State,
        cache: &mut image::Cache,
        emote_map: &mut EmoteMap,
        badge_map: &mut BadgeMap,
        presences: &HashMap<String, Presence>,
        identity: &Option<chat::Identity>,
        writer: &chat::IrcWriter,
        fetcher: &UserFetcher,
        numbers: &Numbers,
        ui: &mut egui::Ui,
    ) {
        // TODO allow this to also be on the bottom
        SidePanel::new(egui::panel::Side::Left, "tab_bar")
            .frame(
                Frame::none()
                    .fill(ui.style().visuals.faint_bg_color)
                    .stroke(ui.style().visuals.window_stroke())
                    .inner_margin(Margin::same(4.0)),
            )
            .width_range(state.icon_size..=state.icon_size)
            .resizable(false)
            .show_inside(ui, |ui| {
                ScrollArea::vertical().show(ui, |ui| {
                    ui.vertical(|ui| {
                        TabBar {
                            state,
                            cache,
                            numbers,
                        }
                        .display(ui)
                    });
                });
            });

        TopBottomPanel::new(egui::panel::TopBottomSide::Bottom, "input_box")
            .frame(Frame::none().fill(ui.style().visuals.code_bg_color))
            .resizable(false)
            .show_inside(ui, |ui| {
                let buffer = &mut state.buffers[state.selected].text;

                let resp = ui.add_sized(
                    ui.available_size(),
                    TextEdit::singleline(buffer)
                        .text_color(ui.style().visuals.strong_text_color())
                        .lock_focus(true)
                        .hint_text(
                            RichText::new(format!(
                                "send a message to #{}",
                                &state.channels[state.selected].display_name
                            ))
                            .color(ui.style().visuals.text_color()),
                        )
                        .frame(false),
                );

                if !(ui.input().key_pressed(egui::Key::Enter) && active) {
                    if resp.lost_focus() {
                        resp.request_focus();
                    }
                    return;
                }

                let buf = std::mem::take(buffer);
                if buf.is_empty() {
                    return;
                }

                let Some(identity) = identity else {
                    // TODO report this
                    eprintln!("not connected");
                    return;
                };

                let channel = &state.channels[state.selected];
                writer.privmsg(&((&*channel.login).with_octo()), &buf);

                let Some(presence) = presences.get(&*((&*channel.login).with_octo())) else {
                    // TODO report this to the UI
                    eprintln!("not on: {}", channel.display_name);
                    return;
                };

                let emotes = Tags::build_emote_meta(&buf, emote_map);

                let mut tags = presence.tags.clone();
                tags.insert("emotes", emotes);

                let buffer = &mut state.buffers[state.selected];
                let pm = chat::Privmsg {
                    ts: time::OffsetDateTime::now_utc(),
                    tags,
                    room_id: channel.id.clone(),
                    user_id: identity.user_id.clone(),
                    data: buf,
                };
                buffer.append(PreparedMessage::new(pm, badge_map));
                resp.request_focus();
            });

        if state.buffers.is_empty() {
            return;
        }

        let selected = state.selected;
        let buffer = &state.buffers[selected];

        ScrollArea::vertical().stick_to_bottom(true).show(ui, |ui| {
            for msg in buffer.ring.iter() {
                ui.horizontal_wrapped(|ui| {
                    // TODO make this hideable
                    ui.small(&msg.ts);

                    ui.scope(|ui| {
                        let width = ui
                            .fonts()
                            .glyph_width(&TextStyle::Body.resolve(ui.style()), ' ');
                        ui.spacing_mut().item_spacing.x = width;

                        if let Some(badge) = msg.badges.first() {
                            if let Some(img) = cache.get(badge) {
                                img.show_size(ui, vec2(8.0, 8.0));
                            }
                        }

                        let name = state
                            .user_map
                            .get(&msg.pm.user_id, fetcher)
                            .map(|user| &user.display_name)
                            .unwrap_or(&msg.pm.user_id);

                        ui.colored_label(msg.color, name);
                    });

                    // TODO make this line selectable
                    ui.scope(|ui| {
                        let width = ui
                            .fonts()
                            .glyph_width(&TextStyle::Body.resolve(ui.style()), ' ');
                        ui.spacing_mut().item_spacing.x = width;

                        for span in &msg.message_spans {
                            match span {
                                TextSpan::Text(text) => {
                                    ui.add(Label::new(RichText::new(text).strong()));
                                }
                                TextSpan::Emote(emote) => {
                                    // TODO be smarter than this
                                    'lookup: for url in emote.as_urls() {
                                        if let Some(img) = cache.get(url) {
                                            img.show_size(ui, vec2(16.0, 16.0));
                                            break 'lookup;
                                        }
                                    }
                                }
                            }
                        }
                    });
                });
            }

            ui.allocate_space(ui.available_size());
        });
    }
}

pub enum AddChannelAction {
    Waiting {
        name: String,
        waiting: Receiver<Vec<helix::User>>,
    },
    KeepOpen,
    Close,
}

pub enum PaletteAction {
    RemoveChannel(usize),
    Select,
    Nothing,
}

struct CommandPalette<'a> {
    actions: &'a Actions,
    channels: &'a Vec<Channel>,
    cache: &'a mut image::Cache,
    selected: &'a mut PaletteSelection,
}

impl<'a> CommandPalette<'a> {
    fn display(mut self, ctx: &egui::Context, frame: &eframe::Frame) -> PaletteAction {
        let offset = vec2(frame.info().window_info.size.x * 0.6, 0.0);
        Area::new("show_command_palette")
            .interactable(true)
            .enabled(true)
            .order(Order::Foreground)
            .movable(false)
            .anchor(Align2::LEFT_TOP, offset)
            .show(ctx, |ui| self.display_frame(ui))
            .inner
    }

    fn display_frame(&mut self, ui: &mut egui::Ui) -> PaletteAction {
        let mut action = PaletteAction::Nothing;
        Frame::none()
            .fill(ui.visuals().window_fill())
            .inner_margin(Margin::same(10.0))
            .show(ui, |ui| {
                ScrollArea::vertical()
                    .auto_shrink([false, false])
                    .show(ui, |ui| {
                        self.display_actions(&mut action, ui);
                        ui.separator();
                        self.display_channels(&mut action, ui);
                    })
            });

        action
    }

    fn display_actions(&mut self, action: &mut PaletteAction, ui: &mut egui::Ui) {
        for (i, k, v) in self.actions.list_actions() {
            ui.push_id(egui::Id::new(v).with(i), |ui| {
                let sel = PaletteSelection::Actions(i);

                let resp = self
                    .maybe_selected_frame(sel, ui)
                    .show(ui, |ui| {
                        ui.horizontal(|ui| {
                            let widget = Label::new(
                                self.maybe_selected_text(sel, k.stringify(), ui).monospace(),
                            );
                            ui.add(widget);

                            ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                                let widget =
                                    Label::new(self.maybe_selected_text(sel, v.stringify(), ui));
                                ui.add(widget);
                            });
                        })
                        .response
                        .interact(Sense::hover().union(Sense::click()))
                    })
                    .inner;

                if resp.double_clicked() {
                    *action = PaletteAction::Select;
                }

                if resp.hovered() {
                    *self.selected = sel
                }
            });
        }
    }

    fn display_channels(&mut self, action: &mut PaletteAction, ui: &mut egui::Ui) {
        for (i, channel) in self.channels.iter().enumerate() {
            ui.push_id(egui::Id::new(&channel.id).with(i), |ui| {
                let sel = PaletteSelection::Channels(i);

                let resp = self
                    .maybe_selected_frame(sel, ui)
                    .show(ui, |ui| {
                        ui.horizontal(|ui| {
                            if let Some(img) = self.cache.get(&channel.profile_image_url) {
                                img.show_size(ui, vec2(16.0, 16.0));
                            }

                            let widget = Label::new(self.maybe_selected_text(
                                sel,
                                &channel.display_name,
                                ui,
                            ));
                            ui.add(widget);

                            ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                                if ui.small_button("❌").clicked() {
                                    *action = PaletteAction::RemoveChannel(i);
                                }
                                ui.add_space(ui.available_width())
                            });
                        });
                    })
                    .response
                    .interact(Sense::hover().union(Sense::click()));

                if resp.double_clicked() {
                    *action = PaletteAction::Select;
                }

                if resp.hovered() {
                    *self.selected = sel
                }
            });
        }
    }

    fn maybe_selected_text(
        &mut self,
        query: PaletteSelection,
        text: impl Into<String>,
        ui: &mut egui::Ui,
    ) -> RichText {
        let selected = *self.selected == query;

        RichText::new(text).color(
            selected
                .then_some(Color32::YELLOW)
                .unwrap_or_else(|| ui.visuals().text_color()),
        )
    }

    fn maybe_selected_frame(&mut self, query: PaletteSelection, ui: &mut egui::Ui) -> Frame {
        let selected = *self.selected == query;
        let color = selected
            .then_some(Color32::from_rgba_premultiplied(0x8d, 0x6b, 0xa9, 16))
            .unwrap_or_else(|| ui.visuals().window_fill());
        Frame::none().fill(color)
    }
}

struct AddChannel<'a> {
    state: &'a mut State,
    toasts: &'a mut Toasts,
    helix: &'a helix::Client,
    add_channel_state: &'a mut AddChannelState,
    action: &'a mut AddChannelAction,
}

impl<'a> AddChannel<'a> {
    fn display(mut self, ctx: &egui::Context, _frame: &eframe::Frame) {
        Window::new("Join a channel")
            .anchor(Align2::CENTER_CENTER, Vec2::ZERO)
            .resizable(false)
            .collapsible(false)
            .title_bar(false)
            .frame(
                Frame::none()
                    .stroke(ctx.style().visuals.window_stroke())
                    .fill(ctx.style().visuals.extreme_bg_color)
                    .inner_margin(Margin::same(2.0)),
            )
            .scroll2([false, false])
            .show(ctx, |ui| {
                ui.vertical_centered(|ui| {
                    ui.heading("Add a channel");
                    ui.separator();

                    ui.scope(|ui| {
                        ui.visuals_mut().selection.stroke = Stroke::new(0.0, Color32::BLACK);

                        let AddChannelState::Editing { input: buffer } = self.add_channel_state else {
                            return;
                        };

                        let resp = ui.add(
                            TextEdit::singleline(buffer)
                                .lock_focus(true)
                                .hint_text("enter a Twitch channel name"),
                        );
                        if resp.lost_focus() && ctx.input().key_pressed(egui::Key::Enter) {
                            let channel = std::mem::take(buffer);
                            self.try_add_channel(channel, ctx.clone());
                        } else {
                            resp.request_focus();
                        }
                    });
                });
            });
    }

    fn try_add_channel(&mut self, channel: String, repaint: impl Repaint) {
        if channel.contains(char::is_whitespace) {
            self.toasts.dismiss_all_toasts();
            *self.add_channel_state = AddChannelState::Error {
                error: AddChannelError::Invalid(channel),
            };
            return;
        }

        let fixed_channel = channel.clone().without_octo();

        if let Some(index) = self
            .state
            .channels
            .iter()
            .position(|ch| ch.login.eq_ignore_ascii_case(&fixed_channel))
        {
            self.toasts.dismiss_all_toasts();
            self.toasts
                .info(format!("Channel {channel} already exists"));

            self.state.selected = index;
            *self.action = AddChannelAction::Close;
            return;
        }

        let helix = self.helix.clone();
        let resp = crate::runtime::spawn({
            let channel = fixed_channel.clone();
            async move {
                let users = helix
                    .get_users([helix::IdOrLogin::Login(&channel)])
                    .await
                    .ok()
                    .unwrap_or_default();
                repaint.repaint();
                users
            }
        });

        *self.action = AddChannelAction::Waiting {
            name: fixed_channel,
            waiting: resp,
        };
    }
}

struct TabBar<'a> {
    state: &'a mut State,
    cache: &'a mut img::Cache,
    numbers: &'a Numbers,
}

impl<'a> TabBar<'a> {
    fn display(self, ui: &mut egui::Ui) {
        for (index, channel) in self.state.channels.iter().enumerate() {
            let stats = &mut self.state.buffers[index].message_stats;
            stats.check_active(index == self.state.selected);
            let Some(image) = self.cache.get(&channel.profile_image_url) else { continue };

            TabButton {
                image,
                name: &channel.display_name,
                selected: &mut self.state.selected,
                index,
                icon_size: self.state.icon_size,
                message_stats: stats,
                numbers: self.numbers,
            }
            .display(ui);
        }
    }
}

struct TabButton<'a> {
    image: &'a img::Image,
    name: &'a str,
    index: usize,
    selected: &'a mut usize,
    icon_size: f32,
    message_stats: &'a MessageStats,
    numbers: &'a Numbers,
}

impl<'a> TabButton<'a> {
    fn display(self, ui: &mut egui::Ui) {
        let resp = self
            .image
            .show_size(ui, vec2(self.icon_size, self.icon_size));

        if *self.selected != self.index {
            ui.painter_at(resp.rect).rect_filled(
                resp.rect,
                Rounding::none(),
                egui::Color32::from_rgba_premultiplied(48, 48, 48, 0xDD),
            )
        }

        if self.message_stats.unread != 0 {
            let scale = self.icon_size / 2.0;

            let n = self.message_stats.unread;
            ui.put(
                {
                    let rect = resp.rect;
                    match n {
                        1..=9 => {
                            let tl = rect.left_top();
                            let w = rect.width();
                            let x = tl.x + w / 2.0 - scale / 2.0;
                            Rect::from_min_max(Pos2::new(x, tl.y), rect.max)
                        }
                        _ => rect,
                    }
                },
                |ui: &mut egui::Ui| -> egui::Response {
                    ui.scope(|ui| {
                        ui.spacing_mut().item_spacing.x = 0.0;
                        let size = vec2(scale, scale);

                        ui.horizontal(|ui| {
                            if n > 99 {
                                let nine = &self.numbers.images[9];
                                nine.show_size(ui, size);
                                nine.show_size(ui, size);
                                let plus = &self.numbers.plus;
                                plus.show_size(ui, size);
                                return;
                            }

                            for digit in self.numbers.digits(n) {
                                digit.show_size(ui, size);
                            }
                        });
                    });

                    ui.allocate_response(
                        Vec2::ZERO,
                        Sense {
                            click: false,
                            drag: false,
                            focusable: false,
                        },
                    )
                },
            );
        }

        if resp.clicked() {
            *self.selected = self.index;
        }

        let resp = resp.on_hover_ui_at_pointer(|ui| {
            ui.label(self.name);
        });

        if resp.hovered() {
            ui.painter().rect_stroke(
                resp.rect,
                Rounding::none(),
                ui.style().visuals.selection.stroke,
            )
        }
    }
}

fn load_icon() -> IconData {
    let img = ::image::load_from_memory(include_bytes!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/smorc.png"
    )))
    .expect("valid smorc");

    IconData {
        width: img.width(),
        height: img.height(),
        rgba: img.into_bytes(),
    }
}

fn load_state(cc: &CreationContext) -> State {
    cc.storage
        .and_then(|storage| storage.get_string(Application::SAVE_KEY))
        .and_then(|data| serde_json::from_str(&data).ok())
        .unwrap_or_else(|| {
            eprintln!("WARNING: cannot load previous state, defaulting it");
            State::default()
        })
}

fn get_var(key: &str) -> anyhow::Result<String> {
    std::env::var(key).map_err(|_| anyhow::anyhow!("could not find key '{key}' in env"))
}

fn connect_to_twitch(
    registration: chat::Registration,
    repaint: impl Repaint,
) -> (chat::IrcWriter, flume::Receiver<chat::TwitchMessage>) {
    let (writer, recv) = chat::IrcWriter::new();
    let (tx, rx) = flume::unbounded();
    let _ = runtime::spawn({
        async move {
            eprintln!("connecting");
            crate::chat::run("irc.chat.twitch.tv:6667", registration, tx, recv, repaint).await;
            eprintln!("disconnected");
        }
    });
    (writer, rx)
}

// TODO redo the buffer thing so this isn't possible to get out of synchronization
fn validate_state(state: &mut State) {
    if let Some(to_add) = state
        .channels
        .len()
        .checked_sub(state.buffers.len())
        .filter(|&c| c != 0)
    {
        for _ in 0..to_add {
            state.buffers.push(Buffer::new());
        }
    }
}

fn main() -> anyhow::Result<()> {
    simple_env_load::load_env_from([".dev.env", ".secrets.env"]);

    let wait = runtime::start();

    // TODO get this from a configuration file as well
    let twitch_client_id = get_var("TWITCH_CLIENT_ID")?;
    let twitch_client_secret = get_var("TWITCH_CLIENT_SECRET")?;
    let twitch_oauth_token = get_var("TWITCH_OAUTH_TOKEN")?;
    let twitch_name = get_var("TWITCH_USER_NAME")?;

    let registration = chat::Registration {
        name: twitch_name,
        oauth_token: twitch_oauth_token,
    };

    // TODO get rid of this, a UI should be provided for it
    let reset = get_var("RESET_SETTINGS").is_ok();

    let now = std::time::Instant::now();
    let helix = runtime::spawn(async move {
        helix::Client::create(&twitch_client_id, &twitch_client_secret).await
    });

    let helix = helix.blocking_recv()??;
    eprintln!("helix took: {:.3?}", now.elapsed());

    let (global_badges, global_emotes) = runtime::spawn({
        let helix = helix.clone();
        async move {
            let badges = helix.get_global_badges();
            let emotes = helix.get_global_emotes();
            let (badges, emotes) = tokio::join!(badges, emotes);
            Result::<_, anyhow::Error>::Ok((badges?, emotes?))
        }
    })
    .blocking_recv()??;

    let badge_map = BadgeMap::default().with_iter(global_badges.iter().flat_map(|b| {
        std::iter::repeat(&b.set_id).zip(b.versions.iter().map(|v| (&v.id, &v.image_url_1x)))
    }));

    let emote_map = EmoteMap::default().with_iter(
        global_emotes
            .into_iter()
            .map(|emote| (emote.name, emote.id)),
    );

    let history = History::load(Application::HISTORY_FILE);

    eframe::run_native(
        "SMirc",
        NativeOptions {
            icon_data: Some(load_icon()),
            ..Default::default()
        },
        Box::new(move |cc| {
            let mut state = if reset {
                State::default()
            } else {
                load_state(cc)
            };

            validate_state(&mut state);

            let (writer, rx) = connect_to_twitch(registration, cc.egui_ctx.clone());

            cc.egui_ctx.set_pixels_per_point({
                // state.pixels_per_point;
                2.0
            });
            Box::new(Application::new(
                state,
                helix,
                writer,
                emote_map,
                badge_map,
                history,
                rx,
                Numbers::load(),
                cc.egui_ctx.clone(),
            ))
        }),
    );

    wait();
    Ok(())
}
