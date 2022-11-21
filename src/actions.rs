use std::ops::Index;

use serde::{Deserialize, Serialize};

use crate::{action::Action, keybind::KeyBind};

#[derive(Debug, Serialize, Deserialize)]
pub struct Actions {
    pub map: Vec<(KeyBind, Action)>,
}

impl Default for Actions {
    fn default() -> Self {
        Self::default_keybinds()
    }
}

impl Index<usize> for Actions {
    type Output = Action;

    fn index(&self, index: usize) -> &Self::Output {
        let (_, action) = &self.map[index];
        action
    }
}

impl Actions {
    pub fn default_keybinds() -> Self {
        use Action::*;

        macro_rules! key {
        (@inner $modifier:ident => $key:ident) => {
            KeyBind { modifiers: egui::Modifiers::$modifier, key: egui::Key::$key }
        };
        (@ident:ident) => {
            key!(@inner NONE => $ident)
        };
        (alt $ident:ident) => {
            key!(@inner ALT => $ident)
        };
        (ctrl $ident:ident) => {
            key!(@inner COMMAND => $ident)
        };
    }

        Self {
            map: vec![
                (key!(ctrl P), ShowCommandPalette),
                (key!(ctrl J), ToggleJoinWindow),
                (key!(ctrl H), ShowKeyBindings),
                (key!(ctrl S), ShowSettings),
                (key!(ctrl Num1), SwitchChannel0),
                (key!(ctrl Num2), SwitchChannel1),
                (key!(ctrl Num3), SwitchChannel2),
                (key!(ctrl Num4), SwitchChannel3),
                (key!(ctrl Num5), SwitchChannel4),
                (key!(ctrl Num6), SwitchChannel5),
                (key!(ctrl Num7), SwitchChannel6),
                (key!(ctrl Num8), SwitchChannel7),
                (key!(ctrl Num9), SwitchChannel8),
                (key!(ctrl Num0), SwitchChannel9),
                (key!(ctrl LBracket), PreviousChannel),
                (key!(ctrl RBracket), NextChannel),
            ],
        }
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn first_visible_action_index(&self) -> usize {
        let (index, ..) = self
            .list_actions()
            .next()
            .expect("atleast one visible action");
        index
    }

    pub fn last_visible_action_index(&self) -> usize {
        let (index, ..) = self
            .list_actions()
            .rev()
            .next()
            .expect("atleast one visible action");
        index
    }

    pub fn list_actions(
        &self,
    ) -> impl Iterator<Item = (usize, KeyBind, Action)> + DoubleEndedIterator + '_ {
        const VISIBLE_ACTIONS: &[Action] = &[
            Action::ToggleJoinWindow, //
            Action::ShowKeyBindings,
            Action::ShowSettings,
        ];

        self.map.iter().enumerate().filter_map(|(i, (k, v))| {
            if VISIBLE_ACTIONS.contains(v) {
                return Some((i, *k, *v));
            }
            None
        })
    }

    pub fn find_action(&self, keybind: KeyBind) -> Option<Action> {
        self.map
            .iter()
            .find_map(|(k, a)| (*k == keybind).then_some(a))
            .copied()
    }
}
