mod components;
mod backend;

use crate::components::*;

#[derive(Routable, Clone, PartialEq)]
enum Route {
    #[layout(NavBar)]
    #[route("/")]
    DogView,
    // #[route("/:..segments")]
    // PageNotFound { segments: Vec<String> },

    #[route("/favorites")]
    Favorites,
}



use dioxus::prelude::*;

static CSS: Asset = asset!("/assets/main.css");


fn main() {
    dioxus::launch(app);
}


// fn App() -> Element {
//     rsx! {
//         document::Stylesheet { href: CSS }
//         Title {}
//         DogView {}
//     }
// }

#[component]
fn app() -> Element {
    rsx! {
        document::Stylesheet { href: asset!("/assets/main.css") }

        Router::<Route> {}
    }
}


#[component]
fn Title() -> Element {
    rsx! {
        div { id: "title",
            h1 { "HotDog! 🌭" }
        }
    }
}

