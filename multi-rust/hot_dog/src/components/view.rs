use dioxus::prelude::*;
use crate::backend::save_dog;

#[derive(serde::Deserialize, Default)]
struct DogApi {
    message: String,
}

#[component]
pub fn DogView() -> Element {
    let mut img_res = use_resource(|| async move {
        reqwest::get("https://dog.ceo/api/breeds/image/random")
            .await
            .unwrap()
            .json::<DogApi>()
            .await
            .unwrap()
            .message
    });

    rsx! {
        div { id: "dogview",
            img { src: img_res.cloned().unwrap_or_default() }
        }
        div { id: "buttons",
            button { onclick: move |_| img_res.restart(), id: "skip", "skip" }
            button { onclick: move |_| async move {
                let current = img_res.cloned().unwrap();
                img_res.restart();
                _ = save_dog(current).await;
            }, id: "save", "save!" }
        }
    }
}