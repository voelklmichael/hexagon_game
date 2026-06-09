use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};

use jni::{jni_sig, jni_str, JavaVM};
use jni::objects::{Global, JObject, JValue};

use crate::music::TRACKS;

pub struct MusicPlayer {
    vm: JavaVM,
    media_player: Mutex<Option<Global<JObject<'static>>>>,
    volume: AtomicU32, // stored as f32 bits
    paused: AtomicBool,
    /// True once play_track succeeds; false after the track ends.
    active: AtomicBool,
}

impl MusicPlayer {
    pub fn new(volume: f32) -> Option<Self> {
        let ctx = ndk_context::android_context();
        // Safety: android-activity initialises ndk-context before android_main is called.
        let vm = unsafe { JavaVM::from_raw(ctx.vm().cast()) };
        Some(Self {
            vm,
            media_player: Mutex::new(None),
            volume: AtomicU32::new(volume.to_bits()),
            paused: AtomicBool::new(false),
            active: AtomicBool::new(false),
        })
    }

    pub fn play_track(&self, track: usize) {
        let _ = self.vm.attach_current_thread(|env| -> jni::errors::Result<()> {
            // Release the existing player (if any).
            {
                let mut guard = self.media_player.lock().unwrap();
                if let Some(old_mp) = guard.take() {
                    let _ = env.call_method(&old_mp, jni_str!("release"), jni_sig!("()V"), &[]);
                }
            }
            self.active.store(false, Ordering::Relaxed);

            // Get the Android Application context stored by android-activity.
            let ctx = ndk_context::android_context();
            // Safety: context() is the Android Application jobject.
            let context = unsafe { JObject::from_raw(env, ctx.context().cast()) };

            let am = env
                .call_method(
                    &context,
                    jni_str!("getAssets"),
                    jni_sig!("()Landroid/content/res/AssetManager;"),
                    &[],
                )?
                .l()?;

            let filename_jstr = env.new_string(TRACKS[track].0)?;
            let afd = env
                .call_method(
                    &am,
                    jni_str!("openFd"),
                    jni_sig!("(Ljava/lang/String;)Landroid/content/res/AssetFileDescriptor;"),
                    &[JValue::Object(&*filename_jstr)],
                )?
                .l()?;

            let mp = env.new_object(
                jni_str!("android/media/MediaPlayer"),
                jni_sig!("()V"),
                &[],
            )?;

            // MediaPlayer duplicates the fd internally during setDataSource.
            env.call_method(
                &mp,
                jni_str!("setDataSource"),
                jni_sig!("(Landroid/content/res/AssetFileDescriptor;)V"),
                &[JValue::Object(&afd)],
            )?;
            let _ = env.call_method(&afd, jni_str!("close"), jni_sig!("()V"), &[]);

            env.call_method(&mp, jni_str!("prepare"), jni_sig!("()V"), &[])?;

            let vol = f32::from_bits(self.volume.load(Ordering::Relaxed));
            env.call_method(
                &mp,
                jni_str!("setVolume"),
                jni_sig!("(FF)V"),
                &[JValue::Float(vol), JValue::Float(vol)],
            )?;

            env.call_method(&mp, jni_str!("start"), jni_sig!("()V"), &[])?;

            let mp_global = env.new_global_ref(mp)?;
            *self.media_player.lock().unwrap() = Some(mp_global);
            self.paused.store(false, Ordering::Relaxed);
            self.active.store(true, Ordering::Relaxed);

            Ok(())
        });
    }

    /// Returns true once when a track finishes (not when explicitly paused).
    pub fn check_and_reset_finished(&self) -> bool {
        if !self.active.load(Ordering::Relaxed) || self.paused.load(Ordering::Relaxed) {
            return false;
        }
        let is_playing = self
            .vm
            .attach_current_thread(|env| -> jni::errors::Result<bool> {
                let guard = self.media_player.lock().unwrap();
                let Some(mp) = guard.as_ref() else {
                    return Ok(false);
                };
                env.call_method(mp, jni_str!("isPlaying"), jni_sig!("()Z"), &[])?.z()
            })
            .unwrap_or(false);

        if !is_playing {
            self.active.store(false, Ordering::Relaxed);
            true
        } else {
            false
        }
    }

    pub fn set_volume(&self, volume: f32) {
        self.volume.store(volume.to_bits(), Ordering::Relaxed);
        let _ = self.vm.attach_current_thread(|env| -> jni::errors::Result<()> {
            let guard = self.media_player.lock().unwrap();
            if let Some(mp) = guard.as_ref() {
                env.call_method(
                    mp,
                    jni_str!("setVolume"),
                    jni_sig!("(FF)V"),
                    &[JValue::Float(volume), JValue::Float(volume)],
                )?;
            }
            Ok(())
        });
    }

    pub fn set_paused(&self, paused: bool) {
        self.paused.store(paused, Ordering::Relaxed);
        let _ = self.vm.attach_current_thread(|env| -> jni::errors::Result<()> {
            let guard = self.media_player.lock().unwrap();
            if let Some(mp) = guard.as_ref() {
                if paused {
                    env.call_method(mp, jni_str!("pause"), jni_sig!("()V"), &[])?;
                } else {
                    env.call_method(mp, jni_str!("start"), jni_sig!("()V"), &[])?;
                }
            }
            Ok(())
        });
    }
}

impl Drop for MusicPlayer {
    fn drop(&mut self) {
        let _ = self.vm.attach_current_thread(|env| -> jni::errors::Result<()> {
            let mut guard = self.media_player.lock().unwrap();
            if let Some(mp) = guard.take() {
                let _ = env.call_method(&mp, jni_str!("release"), jni_sig!("()V"), &[]);
            }
            Ok(())
        });
    }
}
