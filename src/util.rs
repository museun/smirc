use std::borrow::Cow;

pub trait WithIter<T>
where
    Self: Sized,
{
    fn with_iter(self, ex: impl IntoIterator<Item = T>) -> Self;
}

impl<C, T> WithIter<T> for C
where
    C: Extend<T>,
{
    fn with_iter(mut self, ex: impl IntoIterator<Item = T>) -> Self {
        self.extend(ex);
        self
    }
}

pub trait ChannelExt<'a> {
    fn with_octo(self) -> Cow<'a, str>
    where
        Self: Sized + 'a;

    fn without_octo(self) -> Self
    where
        Self: Sized;
}

impl<'a> ChannelExt<'a> for &'a str {
    fn with_octo(self) -> Cow<'a, str>
    where
        Self: Sized,
    {
        if self.starts_with('#') {
            return Cow::Borrowed(self);
        }

        Cow::from(format!("#{self}"))
    }

    fn without_octo(self) -> Self
    where
        Self: Sized,
    {
        self.strip_prefix('#').unwrap_or(self)
    }
}

impl<'a> ChannelExt<'a> for String {
    fn with_octo(self) -> Cow<'a, str>
    where
        Self: Sized,
    {
        if self.starts_with('#') {
            return Cow::Owned(self);
        }

        Cow::from(format!("#{self}"))
    }

    fn without_octo(self) -> Self
    where
        Self: Sized,
    {
        self.strip_prefix('#')
            .map(|s| s.to_string())
            .unwrap_or(self)
    }
}
