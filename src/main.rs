use anyhow::Result;
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher};
use rune::{
    load_sources,
    termcolor::{ColorChoice, StandardStream},
    EmitDiagnostics as _, Errors, LoadSourcesError, Options, Sources, Warnings,
};
use runestick::{Context, IntoTypeHash, Module, Source, Unit, Vm, VmExecution};
use std::{
    path::PathBuf,
    sync::{
        mpsc::{channel, Receiver},
        Arc,
    },
    time::Duration,
};

use tokio::runtime;

pub struct ScriptEngineBuilder {
    root: PathBuf,
    modules: Vec<Module>,
}

impl ScriptEngineBuilder {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            modules: vec![],
        }
    }

    pub fn add_module(mut self, module: Module) -> Self {
        self.modules.push(module);
        self
    }

    pub fn build(self) -> anyhow::Result<ScriptEngine> {
        let rt = runtime::Builder::new().enable_all().build().unwrap();
        let mut context = rune_modules::with_config(true)?;

        for module in self.modules {
            context.install(&module)?;
        }

        let context = Arc::new(context);
        let (tx, rx) = channel();

        let mut watcher: RecommendedWatcher = Watcher::new(tx, Duration::from_secs(2))?;
        watcher.watch(self.root.parent().unwrap(), RecursiveMode::Recursive)?;
        let unit: Arc<Unit> = Default::default();
        let mut engine = ScriptEngine {
            main_file: self.root,
            _watcher: watcher,
            rx,
            context,
            unit,
            sources: Sources::new(),
            rt,
            days: vec![],
        };

        let _ = engine.reload();
        Ok(engine)
    }
}

pub struct ScriptEngine {
    _watcher: RecommendedWatcher,
    rx: Receiver<DebouncedEvent>,
    context: Arc<Context>,
    unit: Arc<Unit>,
    main_file: PathBuf,
    sources: Sources,
    rt: runtime::Runtime,
    days: Vec<u32>,
}

impl ScriptEngine {
    fn reload(&mut self) -> Result<()> {
        let mut sources = Sources::new();
        sources.insert(Source::from_path(&self.main_file)?);

        let mut errors = Errors::new();
        let mut options = Options::default();
        options.debug_info(true);
        let mut warnings = Warnings::new();

        self.sources = sources;
        match load_sources(
            &self.context,
            &options,
            &mut self.sources,
            &mut errors,
            &mut warnings,
        ) {
            Ok(unit) => {
                self.unit = Arc::new(unit);
            }
            Err(e @ LoadSourcesError) => {
                let mut writer = StandardStream::stderr(ColorChoice::Always);
                errors.emit_diagnostics(&mut writer, &self.sources)?;
                return Err(e.into());
            }
        }

        let mut days = vec![];
        for day in 0..24 {
            if self
                .unit
                .lookup(
                    runestick::Item::with_item(&[&format!("day{}", day), "run"]).into_type_hash(),
                )
                .is_some()
            {
                days.push(day)
            }
        }

        self.days = days;
        Ok(())
    }

    pub fn call_all(&mut self) {
        self.call_days(self.days.clone())
    }

    pub fn call_days(&mut self, days: Vec<u32>) {
        for day in days {
            let vm = Vm::new(Arc::new(self.context.runtime()), self.unit.clone());
            let mut execution: VmExecution = vm
                .execute(&[&format!("day{}", day), "run"], ())
                .expect("failed call");
            if let Err(e) = self.rt.block_on(execution.async_complete()) {
                let mut writer = StandardStream::stderr(ColorChoice::Always);
                e.emit_diagnostics(&mut writer, &self.sources)
                    .expect("failed writing info");
            }
        }
    }

    pub fn update(&mut self) -> Vec<String> {
        let updates = self
            .rx
            .try_iter()
            .filter_map(|v| match v {
                DebouncedEvent::Write(v) => Some(v.to_str().unwrap().to_owned()),
                _ => None,
            })
            .collect::<Vec<_>>();
        if updates.len() > 0 {
            if let Err(_e) = self.reload() {
                eprintln!("Failed reloading");
            }
        }
        updates
    }
}

fn main() -> Result<()> {
    let mut engine = ScriptEngineBuilder::new("scripts/main.rn".into()).build()?;

    engine.call_all();
    loop {
        let updated_files = engine.update();

        if updated_files.len() == 0 {
            continue;
        }

        let days = updated_files
            .iter()
            .filter(|v| v.starts_with("day"))
            .map(|v| v[3..][2..].parse().expect("x"))
            .collect::<Vec<_>>();

        let non_days = updated_files
            .iter()
            .filter(|v| !v.starts_with("day"))
            .collect::<Vec<_>>();

        if non_days.len() > 0 {
            engine.call_all();
        } else {
            engine.call_days(days);
        }

        std::thread::sleep(Duration::from_millis(30));
    }
}
