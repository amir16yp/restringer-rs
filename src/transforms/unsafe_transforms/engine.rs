#[cfg(not(any(
    feature = "unsafe-transform-quickjs",
    feature = "unsafe-transform-deno"
)))]
compile_error!(
    "One of the features `unsafe-transform-quickjs` or `unsafe-transform-deno` must be enabled"
);

use std::sync::atomic::{AtomicBool, AtomicU8, Ordering};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Engine {
    #[cfg(feature = "unsafe-transform-quickjs")]
    QuickJs,
    #[cfg(feature = "unsafe-transform-deno")]
    Deno,
}

impl Default for Engine {
    fn default() -> Self {
        #[cfg(feature = "unsafe-transform-deno")]
        {
            Self::Deno
        }
        #[cfg(all(
            feature = "unsafe-transform-quickjs",
            not(feature = "unsafe-transform-deno")
        ))]
        {
            Self::QuickJs
        }
    }
}

const ENGINE_UNSET: u8 = 0;
const ENGINE_QUICKJS: u8 = 1;
const ENGINE_DENO: u8 = 2;

static SELECTED_ENGINE: AtomicU8 = AtomicU8::new(ENGINE_UNSET);
static VERBOSE: AtomicBool = AtomicBool::new(false);

pub fn set_eval_verbose(verbose: bool) {
    VERBOSE.store(verbose, Ordering::Relaxed);
}

pub fn is_eval_verbose() -> bool {
    VERBOSE.load(Ordering::Relaxed)
}

impl Engine {
    fn to_u8(self) -> u8 {
        match self {
            #[cfg(feature = "unsafe-transform-quickjs")]
            Self::QuickJs => ENGINE_QUICKJS,
            #[cfg(feature = "unsafe-transform-deno")]
            Self::Deno => ENGINE_DENO,
        }
    }

    fn from_u8(value: u8) -> Self {
        match value {
            ENGINE_QUICKJS => {
                #[cfg(feature = "unsafe-transform-quickjs")]
                {
                    Self::QuickJs
                }
                #[cfg(not(feature = "unsafe-transform-quickjs"))]
                {
                    Self::default()
                }
            }
            ENGINE_DENO => {
                #[cfg(feature = "unsafe-transform-deno")]
                {
                    Self::Deno
                }
                #[cfg(not(feature = "unsafe-transform-deno"))]
                {
                    Self::default()
                }
            }
            _ => Self::default(),
        }
    }
}

pub fn set_default_engine(engine: Engine) {
    SELECTED_ENGINE.store(engine.to_u8(), Ordering::Relaxed);
}

fn selected_engine() -> Engine {
    let value = SELECTED_ENGINE.load(Ordering::Relaxed);
    if value == ENGINE_UNSET {
        Engine::default()
    } else {
        Engine::from_u8(value)
    }
}

pub enum JsEvaluator {
    #[cfg(feature = "unsafe-transform-quickjs")]
    QuickJs(super::quickjs_engine::QuickJsEngine),
    #[cfg(feature = "unsafe-transform-deno")]
    Deno(super::deno_engine::DenoEngine),
}

impl JsEvaluator {
    pub fn new() -> Self {
        default_engine()
    }

    pub fn eval_to_string(&self, code: &str) -> Result<String, String> {
        match self {
            #[cfg(feature = "unsafe-transform-quickjs")]
            Self::QuickJs(engine) => engine.eval_to_string(code),
            #[cfg(feature = "unsafe-transform-deno")]
            Self::Deno(engine) => engine.eval_to_string(code),
        }
    }

    pub fn eval_to_number(&self, code: &str) -> Result<f64, String> {
        match self {
            #[cfg(feature = "unsafe-transform-quickjs")]
            Self::QuickJs(engine) => engine.eval_to_number(code),
            #[cfg(feature = "unsafe-transform-deno")]
            Self::Deno(engine) => engine.eval_to_number(code),
        }
    }

    pub fn eval_to_bool(&self, code: &str) -> Result<bool, String> {
        match self {
            #[cfg(feature = "unsafe-transform-quickjs")]
            Self::QuickJs(engine) => engine.eval_to_bool(code),
            #[cfg(feature = "unsafe-transform-deno")]
            Self::Deno(engine) => engine.eval_to_bool(code),
        }
    }

    pub fn eval_to_json(&self, code: &str) -> Result<String, String> {
        match self {
            #[cfg(feature = "unsafe-transform-quickjs")]
            Self::QuickJs(engine) => engine.eval_to_json(code),
            #[cfg(feature = "unsafe-transform-deno")]
            Self::Deno(engine) => engine.eval_to_json(code),
        }
    }
}

impl Default for JsEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

fn default_engine() -> JsEvaluator {
    match selected_engine() {
        #[cfg(feature = "unsafe-transform-quickjs")]
        Engine::QuickJs => JsEvaluator::QuickJs(super::quickjs_engine::QuickJsEngine::new()),
        #[cfg(feature = "unsafe-transform-deno")]
        Engine::Deno => JsEvaluator::Deno(super::deno_engine::DenoEngine),
    }
}
