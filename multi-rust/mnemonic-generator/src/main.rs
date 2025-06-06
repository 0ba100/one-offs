use dioxus::prelude::*;
use anyhow::{Result as AR, anyhow};
use openai_api_rust::{images::{ImagesApi, ImagesBody}, Auth, OpenAI};

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
pub fn Hero() -> Element {
    let img_description = use_signal(|| String::new());

    let generate_image = move |_: Event<MouseData>| async move {
        let body = ImagesBody { 
            prompt: img_description.cloned(),
            n: None, 
            size: None, 
            response_format: None, 
            user: Some(String::from("mnemonics-generator-dev")),
        };
        let auth = Auth::from_env()
            .map_err(|e| anyhow!("Failed to load auth: {}", e))
            .map(|auth| OpenAI::new(auth, "https://api.openai.com/v1"))
            .and_then(|c| c.image_create(&body).map_err(|e| anyhow!("{e}")))
            .ok()
            .and_then(|r| r.data)
            .and_then(|d| d.first().map(|f| f.url.to_owned()));
    };

    rsx! {
        div { class: "min-h-screen bg-gray-100 flex items-center justify-center",
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
                        button {
                            onclick: generate_image,
                            r#type: "button",
                            class: "mt-2 bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-500",
                            "Generate"
                        }
                        div { class: "mt-4",
                            img {
                                src: "",
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
