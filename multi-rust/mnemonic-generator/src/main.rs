use std::{thread::sleep, time::Duration};

use anyhow::{anyhow, Result as AR};
use dioxus::prelude::*;
use dioxus_sdk::utils::timing::use_interval;
use serde_json::Value;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() -> AR<()> {
    dioxus::launch(App);
    Ok(())
}

#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS } document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        Hero {}
    }
}

#[component]
fn ProgressIndicator() -> Element {
    let mut prg = use_signal(|| 0.1);
    _ = use_interval(Duration::from_millis(50), move || {
        if prg.cloned() < 100.0 {
            prg.set(prg.cloned() + 0.1);
        }
    });

    rsx! {
        div { class: "w-full h-2 bg-gray-200 rounded-full",
            div { class: "h-full bg-blue-500 rounded-full", style: "width: {prg}%" }
        }
    }
}

#[component]
pub fn Hero() -> Element {
    let img_description = use_signal(|| String::new());
    let mut img_src: Signal<Option<String>> = use_signal(|| None);
    let mut active_request = use_signal(|| false);

    let generate_image = move |_: Event<MouseData>| async move {
        active_request.set(true);

        let task_req = std::env::var("BFL_API_KEY").map(|k| {
            reqwest::Client::new()
                .post("https://api.bfl.ai/v1/flux-kontext-max")
                .header("x-key", k)
                .header("Content-Type", "application/json")
                .body(
                    serde_json::json!({
                     "prompt": img_description.cloned(),
                     "output_format": "png",
                     "prompt_upsampling": true
                    })
                    .to_string(),
                )
        });

        let task_res = match task_req {
            Ok(req) => req.send().await.map_err(|e| anyhow!("{e}")),
            Err(e) => Err(anyhow!("{e}")),
        };

        let task_body = match task_res {
            Ok(res) => res.text().await.map_err(|e| anyhow!("{e}")),
            Err(e) => Err(anyhow!("{e}")),
        };

        #[derive(serde::Deserialize, Clone)]
        struct QueuedTask {
            id: String,
            polling_url: String,
        }

        #[derive(serde::Deserialize)]
        struct PollResponse {
            status: String,
            result: Option<PollResult>
        }

        #[derive(serde::Deserialize)]
        struct PollResult {
            sample: Option<String>,
        }

        let task =
            task_body.and_then(|v| serde_json::from_str::<QueuedTask>(&v).map_err(|e| anyhow!("{e}"))).ok();

        let mut result: Option<String> = None;
        let mut iteration_count = 30;
        while result.is_none() {
            if iteration_count <= 0 {
                break;
            }
            iteration_count -= 1;
            let poll_request = task
                .clone()
                .ok_or(anyhow!("no task"))
                .and_then(|v| {
                    std::env::var("BFL_API_KEY")
                        .map(|k| (k, v))
                        .map_err(|e| anyhow!("{e}"))
                })
                .map(|(key, value)| {
                    reqwest::Client::new()
                        .get(value.polling_url)
                        .header("accept", "application/json")
                        .header("x-key", key)
                });

            let poll_response = match poll_request {
                Ok(s) => s.send().await.map_err(|e| anyhow!("{e}")),
                Err(e) => Err(anyhow!("{e}"))
            };

            let poll_body = match poll_response {
                Ok(r) => r.text().await.map_err(|e| anyhow!("{e}")),
                Err(e) => Err(anyhow!("{e}"))
            };

            let value = poll_body.and_then(|b| serde_json::from_str::<PollResponse>(&b).map_err(|e| anyhow!("{e}")));

            if value.as_ref().is_ok_and(|v| v.status == "Ready") {
                result = value.ok().and_then(|v| v.result).and_then(|r| r.sample)
            }

            tokio::time::sleep(Duration::from_secs(3)).await
        }

        let retrieval_request = result.map(|url| reqwest::Client::new().get(url));

        let img_res = match retrieval_request {
            Some(r) => r.send().await.ok(),
            None => None,
        };

        let img_bytes = match img_res {
            Some(r) => r.bytes().await.ok(),
            None => None,
        };

        img_src.set(None);
        active_request.set(false);
    };

    rsx! {
        div { class: "min-h-screen bg-gray-100 flex items-center text-black justify-center",
            div { class: "w-full max-w-lg bg-white rounded-lg shadow-md p-8",
                h2 { class: "text-2xl font-bold mb-6 text-gray-800", "Mnemonic Generator" }
                form { method: "POST", action: "#", class: "space-y-4",
                    div {
                        label {
                            r#for: "lociAddress",
                            class: "block text-gray-700 font-medium mb-1",
                            "Loci Address"
                        }
                        input {
                            placeholder: "Enter Loci Address",
                            name: "lociAddress",
                            r#type: "text",
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            id: "lociAddress",
                        }
                    }
                    div {
                        label {
                            r#for: "parentLoci",
                            class: "block text-gray-700 font-medium mb-1",
                            "Parent Loci"
                        }
                        input {
                            name: "parentLoci",
                            placeholder: "Enter Parent Loci",
                            r#type: "text",
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            id: "parentLoci",
                        }
                    }
                    div {
                        label {
                            r#for: "childLoci",
                            class: "block text-gray-700 font-medium mb-1",
                            "Child Loci"
                        }
                        input {
                            r#type: "text",
                            placeholder: "Enter Child Loci",
                            name: "childLoci",
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            id: "childLoci",
                        }
                    }
                    div {
                        label {
                            r#for: "mnemonicText",
                            class: "block text-gray-700 font-medium mb-1",
                            "Mnemonic Text"
                        }
                        input {
                            placeholder: "Enter Mnemonic Text",
                            name: "mnemonicText",
                            r#type: "text",
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            id: "mnemonicText",
                        }
                    }
                    div {
                        label {
                            r#for: "imageDescription",
                            class: "block text-gray-700 font-medium mb-1",
                            "Image Description"
                        }
                        textarea {
                            rows: "4",
                            placeholder: "Describe the image",
                            name: "imageDescription",
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            id: "imageDescription",
                        }
                        if active_request() {
                            ProgressIndicator {}
                        }
                        button {
                            onclick: generate_image,
                            r#type: "button",
                            disabled: active_request(),
                            class: "mt-2 bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-500",
                            "Generate"
                        }
                        div { class: "mt-4",
                            img {
                                src: img_src,
                                alt: "Generated image placeholder",
                                class: "w-full h-auto border border-gray-300 rounded-md",
                                id: "imagePreview",
                            }
                        }
                    }
                    div {
                        label {
                            r#for: "answerPart1",
                            class: "block text-gray-700 font-medium mb-1",
                            "Answer Part 1"
                        }
                        input {
                            placeholder: "Enter Answer Part 1",
                            name: "answerPart1",
                            r#type: "text",
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            id: "answerPart1",
                        }
                    }
                    div {
                        label {
                            r#for: "answerPart2",
                            class: "block text-gray-700 font-medium mb-1",
                            "Answer Part 2"
                        }
                        input {
                            r#type: "text",
                            placeholder: "Enter Answer Part 2",
                            name: "answerPart2",
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            id: "answerPart2",
                        }
                    }
                    div {
                        label {
                            r#for: "tags",
                            class: "block text-gray-700 font-medium mb-1",
                            "Tags (comma-separated)"
                        }
                        input {
                            name: "tags",
                            placeholder: "Enter tags separated by commas",
                            r#type: "text",
                            class: "w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500",
                            id: "tags",
                        }
                    }
                    div { class: "flex items-center justify-between mt-6",
                        button {
                            r#type: "submit",
                            class: "bg-green-500 text-white px-5 py-2 rounded-md hover:bg-green-600 focus:outline-none focus:ring-2 focus:ring-green-500",
                            "Submit"
                        }
                        label { class: "inline-flex items-center",
                            input {
                                name: "clearOnSubmit",
                                r#type: "checkbox",
                                class: "mr-2 leading-tight",
                                id: "clearOnSubmit",
                            }
                            span { class: "text-gray-700", "Clear on submit" }
                        }
                    }
                }
            }
        }
    }
}
