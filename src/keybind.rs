use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, Serialize, Deserialize)]
pub struct KeyBind {
    pub modifiers: egui::Modifiers,
    pub key: egui::Key,
}

impl PartialEq for KeyBind {
    fn eq(&self, other: &Self) -> bool {
        self.modifiers == other.modifiers && self.key == other.key
    }
}

impl std::fmt::Debug for KeyBind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.stringify())
    }
}

impl KeyBind {
    pub fn stringify(&self) -> String {
        format!(
            "{}{}",
            stringify_modifier(self.modifiers),
            stringify_key(self.key)
        )
    }
}

fn stringify_modifier(modifiers: egui::Modifiers) -> String {
    let mut buf = String::new();
    for repr in [
        (modifiers.command, "Ctrl"),
        (modifiers.alt, "Alt"),
        (modifiers.shift, "Shift"),
    ]
    .into_iter()
    .filter_map(|(c, key)| c.then_some(key))
    {
        if !buf.is_empty() {
            buf.push('+')
        }
        buf.push_str(repr)
    }

    if modifiers != egui::Modifiers::NONE && !buf.is_empty() {
        buf.push('+')
    }
    buf
}

fn stringify_key(key: egui::Key) -> &'static str {
    macro_rules! s {
    ($($key:ident)*) => {
        &[ $( (stringify!($key), $key), )* ]
    };
}

    use egui::Key::*;
    const KEYS: &[(&str, egui::Key)] = s! {
        ArrowDown ArrowLeft ArrowRight ArrowUp
        Escape    Tab       Backspace  Enter
        Space     Insert    Delete     Home
        End       PageUp    PageDown

        Num0 Num1 Num2 Num3 Num4 Num5 Num6 Num7 Num8 Num9

        Backslash Colon  Comma    Equals    Grave LBracket
        Minus     Period RBracket Semicolon Slash

        A B C D E F G H I J K L M
        N O P Q R S T U V W X Y Z

        F1  F2  F3  F4  F5  F6  F7  F8  F9  F10
        F11 F12 F13 F14 F15 F16 F17 F18 F19 F20
    };

    KEYS.iter()
        .find_map(|(name, k)| (*k == key).then_some(*name))
        .unwrap()
}

impl std::hash::Hash for KeyBind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        #[derive(Hash)]
        struct Modifiers {
            alt: bool,
            ctrl: bool,
            shift: bool,
            command: bool,
        }

        let egui::Modifiers {
            alt,
            ctrl,
            shift,
            command,
            ..
        } = self.modifiers;
        Modifiers {
            alt,
            ctrl,
            shift,
            command,
        }
        .hash(state);
        self.key.hash(state);
    }
}
