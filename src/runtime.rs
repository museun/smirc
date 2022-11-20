use std::future::Future;

use once_cell::sync::OnceCell;
use tokio::sync::oneshot::{self, Receiver};

static HANDLE: OnceCell<tokio::runtime::Handle> = OnceCell::new();

pub fn start() -> impl FnOnce() {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("build tokio runtime");

    let handle = rt.handle().clone();
    HANDLE.set(handle).expect("single initialization");

    let (tx, rx) = oneshot::channel::<()>();
    let thread = std::thread::spawn(move || {
        rt.block_on(async move {
            tokio::select! {
                _ = std::future::pending::<()>() => {}
                _ = rx => {}
            }
        });
    });

    move || {
        drop(tx);
        let _ = thread.join();
    }
}

pub fn enter(func: impl FnOnce()) {
    let _g = HANDLE.get().expect("runtime initialization").enter();
    func();
}

pub fn spawn<T>(fut: impl Future<Output = T> + Send + Sync + 'static) -> Receiver<T>
where
    T: Send + Sync + 'static,
{
    let (tx, rx) = oneshot::channel();
    enter(|| {
        tokio::task::spawn(async move {
            let res = fut.await;
            let _ = tx.send(res);
        });
    });
    rx
}

pub fn blocking<T>(func: impl FnOnce() -> T + Send + Sync + 'static) -> Receiver<T>
where
    T: Send + Sync + 'static,
{
    let (tx, rx) = oneshot::channel();
    enter(|| {
        tokio::task::spawn(async move {
            if let Ok(ok) = tokio::task::spawn_blocking(func).await {
                let _ = tx.send(ok);
            }
        });
    });
    rx
}
