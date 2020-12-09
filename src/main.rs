use anyhow::Result;
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher};
use rune::{
    load_sources,
    termcolor::{ColorChoice, StandardStream},
    EmitDiagnostics as _, Errors, LoadSourcesError, Options, Sources, Warnings,
};
use runestick::{
    CompileMeta, Context, FromValue, Hash, IntoTypeHash, Module, Source, Unit, UnitFn, Vm, VmError,
    VmErrorKind, VmExecution,
};
use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{
        mpsc::{channel, Receiver},
        Arc,
    },
    time::Duration,
    time::Instant,
};
use structopt::StructOpt;
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
            days: Default::default(),
            tests: Default::default(),
        };

        let _ = engine.reload();
        Ok(engine)
    }
}

#[derive(Default)]
struct TestVisitor {
    test_functions: Vec<(Hash, CompileMeta)>,
}

impl rune::CompileVisitor for TestVisitor {
    fn visit_meta(
        &mut self,
        _source_id: runestick::SourceId,
        meta: &runestick::CompileMeta,
        _span: runestick::Span,
    ) {
        let type_hash = match &meta.kind {
            runestick::CompileMetaKind::Function { is_test, type_hash } if *is_test => type_hash,
            _ => return,
        };

        self.test_functions.push((*type_hash, meta.clone()));
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
    days: HashMap<u32, f64>,
    tests: Vec<(Hash, CompileMeta)>,
}

impl ScriptEngine {
    fn reload(&mut self) -> Result<()> {
        let mut sources = Sources::new();
        sources.insert(Source::from_path(&self.main_file)?);

        let mut errors = Errors::new();
        let mut options = Options::default();
        options.debug_info(true);
        options.memoize_instance_fn(true);
        let mut warnings = Warnings::new();

        self.sources = sources;
        let mut test_finder = TestVisitor::default();
        let mut source_loader = rune::FileSourceLoader::new();
        match rune::load_sources_with_visitor(
            &self.context,
            &options,
            &mut self.sources,
            &mut errors,
            &mut warnings,
            &mut test_finder,
            &mut source_loader,
        ) {
            Ok(unit) => {
                self.unit = Arc::new(unit);
                self.tests = test_finder.test_functions;
            }
            Err(e @ LoadSourcesError) => {
                let mut writer = StandardStream::stderr(ColorChoice::Always);
                errors.emit_diagnostics(&mut writer, &self.sources)?;
                return Err(e.into());
            }
        }

        for day in 0..24 {
            if self
                .unit
                .lookup(
                    runestick::Item::with_item(&[&format!("day{}", day), "run"]).into_type_hash(),
                )
                .is_some()
            {
                self.days.entry(day).or_default();
            }
        }

        Ok(())
    }

    pub fn run_tests(&mut self) -> Result<bool> {
        // TODO: use rune-tests capture_output to stop prints from tests from showing
        let runtime = Arc::new(self.context.runtime());

        let start = Instant::now();
        let mut failures = HashMap::new();

        for test in &self.tests {
            print!("testing {:40} ", test.1.item.item);
            let mut vm = Vm::new(runtime.clone(), self.unit.clone());

            let info = self.unit.lookup(test.0).ok_or_else(|| {
                VmError::from(VmErrorKind::MissingEntry {
                    hash: test.0,
                    item: test.1.item.item.clone(),
                })
            })?;

            let offset = match info {
                // NB: we ignore the calling convention.
                // everything is just async when called externally.
                UnitFn::Offset { offset, .. } => offset,
                _ => {
                    return Err(VmError::from(VmErrorKind::MissingFunction { hash: test.0 }).into());
                }
            };

            vm.set_ip(offset);
            match self.rt.block_on(vm.async_complete()) {
                Err(e) => {
                    // TODO: store output here
                    failures.insert(test.1.item.item.clone(), e);
                    println!("failed");
                }
                Ok(_) => {
                    println!("ok.")
                }
            }
        }

        let elapsed = start.elapsed();

        for (item, error) in &failures {
            println!("----------------------------------------");
            println!("Test: {}\n", item);

            let mut writer = StandardStream::stderr(ColorChoice::Always);
            error
                .emit_diagnostics(&mut writer, &self.sources)
                .expect("failed writing info");
        }

        println!("====");
        println!(
            "Ran {} tests with {} failures in {:.3} seconds",
            self.tests.len(),
            failures.len(),
            elapsed.as_secs_f64()
        );

        Ok(failures.is_empty())
    }

    pub fn call_all(&mut self) -> Result<()> {
        self.call_days(self.days.keys().copied().collect())
    }

    pub fn call_days(&mut self, mut days: Vec<u32>) -> Result<()> {
        days.sort_unstable();
        for day in days {
            self.run_day(day, 10)?;
        }
        Ok(())
    }

    pub fn run_day(&mut self, day: u32, iterations: u32) -> Result<f64> {
        let start = Instant::now();
        let func = &[&format!("day{}", day), "run"];
        let mut idx = 0;
        let runtime = Arc::new(self.context.runtime());
        let result = loop {
            let vm = Vm::new(runtime.clone(), self.unit.clone());
            let mut execution: VmExecution = vm.execute(func, ()).expect("failed call");

            let result = match self.rt.block_on(execution.async_complete()) {
                Err(e) => {
                    let mut writer = StandardStream::stderr(ColorChoice::Always);
                    e.emit_diagnostics(&mut writer, &self.sources)
                        .expect("failed writing info");

                    println!("Day {:>2}     FAILED", day);
                    return Err(e.into());
                }
                Ok(v) => v,
            };
            idx += 1;
            if idx == iterations {
                break result;
            }
        };

        let result = <(u32, u32)>::from_value(result);
        let elapsed = start.elapsed();

        let previous_time = self.days[&day];
        let elapsed_ms = (elapsed.as_secs_f64() / iterations as f64) * 1000.0;
        let frac = 1.0 - elapsed_ms / previous_time;
        if frac.abs() > 0.05 && frac.is_finite() {
            let sign = if frac > 0.0 { '-' } else { '+' };
            println!(
                "Day {:>2}   {: >8.4}   {}{: >6.2}%     {:?}",
                day,
                elapsed_ms,
                sign,
                frac.abs() * 100.0,
                result,
            );
        } else {
            println!(
                "Day {:>2}   {: >8.5}   --------     {:?}",
                day, elapsed_ms, result,
            );
        }

        self.days.insert(day, elapsed_ms);
        Ok(elapsed_ms)
    }

    pub fn update(&mut self) -> Result<Vec<String>> {
        let updates = self
            .rx
            .try_iter()
            .filter_map(|v| match v {
                DebouncedEvent::Write(v) => {
                    Some(v.file_name().unwrap().to_str().unwrap().to_owned())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        if !updates.is_empty() {
            self.reload().map(|_| updates)
        } else {
            Ok(updates)
        }
    }

    pub fn days(&self) -> Vec<u32> {
        self.days.keys().copied().collect()
    }
}

#[derive(StructOpt)]
struct AOC {
    #[structopt(subcommand)]
    cmd: Option<Command>,
}

#[derive(StructOpt)]
enum Command {
    Bench {
        #[structopt(short)]
        iterations: u32,

        #[structopt(short)]
        day: Option<u32>,
    },
    RunAll,
}

fn run_reload(mut engine: ScriptEngine) -> Result<()> {
    loop {
        let updated_files = match engine.update() {
            Ok(v) => v,
            Err(_) => {
                eprintln!("failed reloading, not running");
                continue;
            }
        };

        if updated_files.is_empty() {
            continue;
        }

        eprintln!("Starting hot reload...");
        if !engine.run_tests().unwrap_or(false) {
            eprintln!("failed tests; not rerunning...");
            continue;
        }

        let days = updated_files
            .iter()
            .filter(|v| v.starts_with("day"))
            .map(|v| v[3..].strip_suffix(".rn").unwrap().parse().expect("x"))
            .collect::<Vec<_>>();

        let non_days = updated_files
            .iter()
            .filter(|v| !v.starts_with("day"))
            .collect::<Vec<_>>();

        if !non_days.is_empty() {
            let _ = engine.call_all();
        } else {
            let _ = engine.call_days(days);
        }

        std::thread::sleep(Duration::from_millis(30));
    }
}

fn run(run_once: bool) -> Result<()> {
    let mut engine = ScriptEngineBuilder::new("scripts/main.rn".into()).build()?;

    if !engine.run_tests().unwrap_or(false) {
        eprintln!("failed tests; not rerunning...");
    }

    if run_once {
        engine.call_all()
    } else {
        let _ = engine.call_all();
        run_reload(engine)
    }
}

fn bench(iterations: u32, day: Option<u32>) -> Result<()> {
    let mut engine = ScriptEngineBuilder::new("scripts/main.rn".into()).build()?;

    if let Some(day) = day {
        println!("Benchmarking day {}, iterations={}", day, iterations);
        engine.run_day(day, iterations)?;
    } else {
        println!("Benchmarking all days, iterations={}", iterations);
        let mut days = engine.days();
        days.sort_unstable();
        for day in days {
            engine.run_day(day, iterations)?;
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let args = AOC::from_args();

    if let Some(cmd) = args.cmd {
        match cmd {
            Command::Bench { iterations, day } => bench(iterations, day),
            Command::RunAll => run(true),
        }
    } else {
        run(false)
    }
}
