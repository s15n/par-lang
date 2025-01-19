use std::future::IntoFuture;

use futures::task::Spawn;

pub struct TokioSpawn;

impl Spawn for TokioSpawn {
    fn spawn_obj(
        &self,
        future: futures::task::FutureObj<'static, ()>,
    ) -> Result<(), futures::task::SpawnError> {
        drop(tokio::task::spawn(future.into_future()));
        Ok(())
    }
}
