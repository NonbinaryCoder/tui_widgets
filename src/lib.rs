use std::{
    io::{self, StdoutLock, Write},
    sync::atomic::{AtomicBool, Ordering},
};

/// Set to true while an app exists; set to false when it is dropped.
static APP_RUNNING: AtomicBool = AtomicBool::new(false);

/// A tui app.
///
/// Only one app may exist at a time.  To exit properly, call [`App::exit`]. The
/// app will still exit when in is dropped, but all errors will be ignored.
#[derive(Debug)]
#[must_use = "this `App` should be exited with the `exit` function to catch errors"]
pub struct App<O: Write = StdoutLock<'static>> {
    out: O,
    out_is_tty: bool,
}

impl App<StdoutLock<'static>> {
    /// Starts the app.
    ///
    /// # Panics
    ///
    /// Panics if the app is already running.
    pub fn start() -> Self {
        Self::start_with(io::stdout().lock(), true)
    }
}

impl<O: Write> App<O> {
    /// Starts the app, with it's output set to the provided stream.  If
    /// `out_is_tty` is unset, terminal functions not relying on ANSI codes will
    /// be skipped.
    ///
    /// # Panics
    ///
    /// Panics if the app is already running.
    pub fn start_with(output: O, out_is_tty: bool) -> Self {
        start_app();
        Self {
            out: output,
            out_is_tty,
        }
    }

    /// Exits the app.
    ///
    /// # Errors
    ///
    /// This function will return an error if writing to stdout fails.
    pub fn exit(self) -> io::Result<()> {
        Ok(())
    }
}

/// Records that the app has been started.
///
/// # Panics
///
/// Panics if the app is already running.
fn start_app() {
    // Probably doesn't need to be `SeqCst`, but I don't understand atomics
    // and don't want anything to go wrong.
    let was_running = APP_RUNNING.swap(true, Ordering::SeqCst);
    if was_running {
        panic!("Only one app can run at once!")
    }
}

impl<O: Write> Drop for App<O> {
    fn drop(&mut self) {
        // Probably doesn't need to be `SeqCst`, but I don't understand atomics
        // and don't want anything to go wrong.
        APP_RUNNING.store(false, Ordering::SeqCst);
    }
}

#[cfg(test)]
mod tests {
    use serial_test::serial;

    use super::*;

    #[test]
    #[serial]
    #[should_panic]
    fn cannot_double_start() {
        let _app = App::start();
        let _ = App::start();
    }

    #[test]
    fn can_restart() -> io::Result<()> {
        let app = App::start();
        app.exit()?;
        let _ = App::start();
        Ok(())
    }

    #[test]
    fn can_restart_after_drop() {
        let app = App::start();
        drop(app);
        let _ = App::start();
    }
}
