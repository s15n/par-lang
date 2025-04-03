use crate::interact::Handle;
use crate::par::language::{Definition, Internal, Name, Program};
use crate::par::process;
use crate::par::runtime::Context;
use crate::spawn::TokioSpawn;
use eframe::egui;
use std::sync::Arc;

type NameT = Internal<Name>;
type ExprT = Arc<process::Expression<NameT, ()>>;

fn run_def(
    interact: &mut Option<crate::playground::Interact>,
    ui: &mut egui::Ui,
    program: &Program<NameT, ExprT>,
    compiled_code: Arc<str>,
    definition: &Definition<NameT, ExprT>,
) {
    let name = definition.name.to_string();
    if ui.button(&name).clicked() {
        if let Some(int) = interact.take() {
            int.handle.lock().expect("lock failed").cancel();
        }
        *interact = Some(crate::playground::Interact {
            code: Arc::clone(&compiled_code),
            handle: Handle::start_expression(
                Arc::new({
                    let ctx = ui.ctx().clone();
                    move || ctx.request_repaint()
                }),
                Context::new(
                    Arc::new(TokioSpawn),
                    Arc::new(
                        program
                            .definitions
                            .iter()
                            .map(
                                |Definition {
                                     name, expression, ..
                                 }| {
                                    (name.clone(), expression.clone())
                                },
                            )
                            .collect(),
                    ),
                ),
                &definition.expression,
            ),
        });
    }
}

async fn run_def_playground_async(
    program: Program<NameT, ExprT>,
    compiled_code: Arc<str>,
    definition: Definition<NameT, ExprT>,
) {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([1000.0, 700.0]),
        //run_and_return: false,
        ..Default::default()
    };

    eframe::run_simple_native(
        "⅋layground",
        options,
        move |ctx, _frame| {
            egui::CentralPanel::default().show(ctx, |ui| {
                run_def(&mut None, ui, &program, compiled_code.clone(), &definition);
            });
        }, // 417, 339, 508
    )
    .expect("egui crashed");
}

pub fn run_def_playground(
    program: Program<NameT, ExprT>,
    compiled_code: Arc<str>,
    definition: Definition<NameT, ExprT>,
) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    runtime.block_on(async {
        run_def_playground_async(program, compiled_code, definition).await;
    })
}
/*
call:

        super::playground_run::run_def_playground(
            compiled.program.clone(),
            self.io.read(&self.uri).unwrap().as_str().into(),
            definition.clone(),
        );
 */
